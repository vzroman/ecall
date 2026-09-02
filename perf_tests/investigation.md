# Why local many-to-one collapses and distributed does not

Question: the contention harness (many senders → one `on_heap` receiver, 1 msg/100 ms,
phase-aligned) reaches 12.8% of the paced rate at 10,000 senders locally, while the
distributed variant only degrades at 50,000. Both paths allocate, copy, take a queue
lock and enqueue a pointer. The gap is ~8×.

All measurements below on **10.225.2.21** — 48 cores, 2 sockets, Xeon Gold 6342,
OTP 27.2.3 / erts-15.2.2. Nothing here is from other hardware.

---

## What was checked

### 1. Reproduction and regime instruments

`bench.erl paced on_heap 10000 100 100`:

| metric | value |
|---|---|
| performance | 11.7% (78–85 s vs 9.9 s expected) |
| throughput | 11.8k msg/s |
| **OS CPU** | **1.42 of 48 cores** |
| scheduler threads in OS state `S` | **47 of 48** |
| `msacc` | `sleep => 97.4%`, `emulator => 2.6%` |
| `scheduler_wall_time` | 52.8% |

Receiver mailbox stays near-empty (23–41 in the original run). The receiver is not
slow — messages do not arrive. The bottleneck is the enqueue side.

`scheduler_wall_time` reports ~50–100% while real CPU is 3%. `sched_wall_time_change`
is called only from `scheduler_wait`/`suspend_scheduler` (`erl_process.c:3501-3658`),
so a scheduler futex-blocked on a process lock is still counted "working".
**`msacc` is the reliable instrument here; `scheduler_wall_time` is not.**

### 2. `off_heap`

Same load: **99.8%**. At 5× the load (50,000 senders, 500k msg/s): **99.4%**, 7 cores.

### 3. Local vs distributed on one box

Same VM, same 48 schedulers, same 10,000 sender processes, same `on_heap` receiver.
Only the target pid differs — network and second machine removed:

| target | performance | throughput |
|---|---|---|
| local pid | 11.5% | 11,651 msg/s |
| remote pid (loopback dist) | **99.4%** | 100,381 msg/s |
| remote pid, 50,000 senders | 31.0% | 156,514 msg/s |

Routing through TCP to another node on the same host is 8.6× faster than `Pid ! m`.
The distributed path has its own ceiling (~156k msg/s, one connection = one serial
input handler) but degrades gracefully instead of collapsing.

### 4. Lock counting

1000 senders × 200 msgs, `+S 48:48`:

| | elapsed | contended lock | collisions |
|---|---|---|---|
| local, fast regime | 230 ms | `proc_msgq` 399,067 tries | **98.7%** |
| local, collapsed | 11,890 ms | — | — |
| distributed | 643 ms | `dist_entry_out_queue` 226,660 tries | **98.7%** |

**The distributed lock is contended to the same degree and is 22× faster.** Contention
level is not the difference.

`pix_lock` **tries** (touched only when a thread escalates to `wait_for_locks`) is the
regime indicator: 32 in the fast regime, 270,529 in the collapsed one.

> lcnt caveat: the `time` column is per-*thread* wall time, and the timer is armed once
> and consumed by the first matching post (`erl_lock_count.h:786, 816`). The nested
> `pix_lock` acquire inside `wait_for_locks` steals the timer armed by the outer
> `proc_msgq` acquire, so `pix_lock`'s time is misattributed `proc_msgq` wait. Verified:
> `pix_lock` reports 851 `times_waited` against 119 collisions. Read `tries`, not `time`.

### 5. Controls — each alternative explanation falsified

| hypothesis | test | result |
|---|---|---|
| NUMA / cross-socket | 24 threads spanning both sockets | 99.9% — not it |
| receiver competing for MAIN+MSGQ | receiver that never receives | still collapses 3/5 — not it |
| the `on_heap` MAIN trylock | raw lcnt counters | 93.6% failure but `times_waited`=9, 204 µs total — not it |
| message rate | 1 sender to the same mailbox | 4.9M msg/s — not it |

The variable is the number of concurrently *enqueuing threads*, not the rate:
1 sender 4.9M msg/s → 16 senders 1.06M → 1000 senders ~12k.

### 6. Isolation microbenchmark

Same waiter queue, same futex, same 2000-spin budget with `sched_yield` every 25, same
critical section. `handoff` vs `barge` differ by one line in `unlock()`:
whether the LOCKED bit is cleared before waking.

| threads | pthread | barge | **handoff** |
|---|---|---|---|
| 8 | 3.36M | 2.50M | 2.07M |
| 16 | 3.50M | 2.29M | 0.83M |
| 24 | 3.74M | 2.35M | **11,927 (83.9 µs/op)** |
| 48 | 3.58M | 1.66M | **12,036 (83.1 µs/op)** |

83–84 µs/op matches the VM's measured 77.6 µs/message.

### 7. Patched emulator

Two emulators built from one source tree, same compiler and flags, differing only in
`ERTS_PROC_LOCK_OWN_IMPL` (`erl_process_lock.h:44`), which selects OTP's `RAW_MUTEX`
process-lock implementation. Swap verified by symbol check: `erts_proc_lock_failed`
and `erts_proc_unlock_failed` present in vanilla, absent in patched.

Original paced test, 10,000 senders:

| | run 1 | run 2 | run 3 |
|---|---|---|---|
| vanilla | 12.6% / 12,725 msg/s | 11.9% / 12,036 | 12.3% / 12,440 |
| patched | **99.9% / 100,908** | **99.9% / 100,908** | **99.9% / 100,908** |

Blast, 1000 senders × 200 msgs, send-side throughput:

```
vanilla:    15,665 |   884,956 |   318,979 |   995,025 |    29,990 msg/s   (2/5 collapsed)
patched: 1,639,344 | 1,904,762 | 1,834,862 | 2,597,403 | 1,724,138 msg/s   (5/5 fast)
```

Caveat: `RAW_MUTEX` changes barging *and* granularity (one bitfield → five mutexes), so
alone it cannot separate the two. §6 fixes granularity and varies only barging. The two
together pin it; neither alone does.

---

## Root cause

**ERTS process locks forbid barging; the distribution mutex allows it.**

On release with a waiter queued, the process lock's bit is deliberately kept set —
`erl_process_lock.h:775`:

```c
/* What p->lock will look like with all non-waited locks released. */
ErtsProcLocks want_lflgs = old_lflgs & (wait_locks | ~locks);
```

`transfer_locks` then hands ownership to the FIFO head and futex-wakes it
(`erl_process_lock.c:309-324`). The lock is never free, so throughput is set by
wake-and-dispatch latency (77.6 µs/message measured), not by the critical section.
A spinner cannot take it — `in_order_locks` returns 0 while the bit is set
(`erl_process_lock.c:369-380`) — so once anyone parks, the 2000-spin budget plus ~80
`sched_yield()` syscalls per acquire are wasted by construction. That is a one-way
trapdoor, which is why the failure is bistable rather than gradual.

`dep->qlock` is a plain `pthread_mutex_t` — ERTS' own mutex implementation is compiled
out on Linux (`ethread.h:147`, `#if !defined(ETHR_FORCE_PTHREAD_MUTEX) && 0`). Release
frees it; a running thread takes it; throughput is set by the ~30 ns critical section.

The non-barging rule is not arbitrary. The process lock is a composite of five bits
acquired in a mandated order (`erl_process_lock.h:184-198`), and a thread can hold MAIN
while queued for MSGQ; FIFO hand-off is what stops it starving. The message-send path
uses none of that — `ERTS_PROC_LOCKS_MSG_SEND` is just `ERTS_PROC_LOCK_MSGQ`
(`erl_process_lock.h:208`) — but pays the price anyway, because barging is a property
of the whole lock word, not of one bit.

**`on_heap` is not slow in itself.** It is the one setting that routes senders onto the
process lock at all: inqueue-buffer installation is gated on `ERTS_PSFLG_OFF_HEAP_MSGQ`
(`erl_proc_sig_queue.c:9411`), so with `on_heap` the sharded inbox never installs and
the contention counter at `erl_proc_sig_queue.c:8991` is incremented and discarded
forever. `off_heap` shards onto 64 per-slot `erts_mtx_t` — barging pthread mutexes —
hashed on the *sender* pid, which is what preserves send order (`:9033-9037`).

---

## What follows

1. **The harness result does not mean "local send is slower than distributed."** It
   means `on_heap` puts senders on a non-barging lock. With `off_heap` the local path
   beats the distributed one (869k vs 311k msg/s in the fast regime).

2. **`off_heap` on every fan-in target.** Already set on the pools —
   `src/ecall_receive.erl:35`, `src/ecall_connection.erl:168`. Remaining exposure is the
   final `To ! Message` in `ecall_receive:handle_batch`, where `To` is a default
   `on_heap` application process; the enqueuer count there is bounded by pool size,
   which is why the pool-size sweep mattered and why widening the pool is not free.

3. **Capacity, not rate, is the planning number.** Concurrent enqueuing threads is the
   variable. `on_heap` at 48 schedulers tips at ~2000 concurrent senders (~20k msg/s).

4. **Diagnosing this class of stall:** high `scheduler_wall_time` with low OS CPU and
   `msacc sleep` dominant ⇒ lock convoy, not CPU saturation. Confirm with `pix_lock`
   *tries* under lcnt.

5. The patched emulator is a diagnostic only. `RAW_MUTEX` is OTP's fallback
   implementation and is not a shipping configuration.

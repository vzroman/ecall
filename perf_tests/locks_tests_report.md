# Why ETS Contention Shows ~100% Scheduler Busy and Message-Send Contention Does Not

Date: 2026-08-19

Companion to `ets_test.md` and `proc_message.md`. Source references are to
the locally available tree `/home/roman/DISTR/ERLANG/otp_src_27.0`. Both test
hosts run OTP 27 / erts-15.2.2, i.e. OTP 27.2.3; the functions cited are
identical there, but a few line numbers shift by a handful of lines. Every
load-bearing claim was re-verified against the 27.2.3 tree on the remote host
(where, for example, `erts_proc_sig_queue_lock` is at
`erl_proc_sig_queue.c:8990` rather than `:8974`).

## The Question

Two tests reproduced a VM stall on the same 48-scheduler host:

```text
ETS  20,000 workers:  msacc busy = 100.00%, sleep =  0.00%
MSG 200,000 workers:  msacc busy =   1.53%, sleep = 98.47%
```

Both stalls look identical from the OS: ~105 BEAM threads parked in
`futex_wait_queue_me`, low CPU for a 48-core VM, delayed observer, and
native stacks dominated by lock operations. Only the `msacc` reading differs,
and it differs completely. Why?

## Answer in One Sentence

`msacc` is a **state-machine timer, not a CPU-time accounting mechanism**: it
charges the whole interval between two state transitions to whichever state
was current, regardless of whether the thread was running or parked in the
kernel. The ETS table lock blocks inside a code path that performs **no state
transition**, so the parked time is charged to `emulator` (reported busy). The
process message-queue lock blocks inside `erts_tse_wait()`, which **does**
push the state to `sleep`, so the identical parked time is charged to `sleep`
(reported idle).

Neither number describes CPU usage. In both tests the VM was stalled on a
single native lock, and in both tests the OS delivered 1-2 cores out of 48.
The message-send case is also the *worse* of the two failures (section 12) —
`msacc` labels the more damaging stall as the idle one.

This was confirmed by re-running both tests with `msacc` and
`erlang:statistics(scheduler_wall_time)` sampled over the same window
(section 8): **`scheduler_wall_time` reports 100.000% busy on all 48
schedulers in both runs**, including the run `msacc` calls 98.5% asleep. Not
one scheduler ever reached the idle loop.

## 1. How msacc Actually Accounts Time

The whole accounting is these lines (`erl_msacc.h:350-365`):

```c
void erts_msacc_set_state_m__(ErtsMsAcc *msacc, Uint new_state, int increment) {
    if (new_state == msacc->state)
        return;
    prev_perf_counter = msacc->perf_counter;
    msacc->perf_counter = erts_sys_perf_counter();
    diff = msacc->perf_counter - prev_perf_counter;
    msacc->counters[msacc->state].pc += diff;
    msacc->state = new_state;
}
```

Three consequences that decide this whole question:

- Time is only ever attributed **at a state transition**, and the full elapsed
  wall time since the previous transition is added to the **previous** state.
- The counter is `erts_sys_perf_counter()` — wall clock, not CPU time. A thread
  blocked in a futex accrues exactly as much as a thread spinning on a core.
- If a code path blocks without changing the state, the block is invisible to
  `msacc` and inflates whatever state was already current.

A scheduler is put into `emulator` state when it schedules in a process
(`erl_process.c:10072`, `beam_common.c:288`). In a default build there are only
7 states (`erl_msacc.h:59-67`); the per-BIF states (`ets`, `send`, `busy_wait`,
`nif`, ...) at `erl_msacc.h:84-102` require configuring with
`--with-microstate-accounting=extra`. Neither host has that, so **everything a
BIF does, including blocking, is charged to `emulator`** unless the BIF itself
pushes another state.

Both benchmarks derive `busy = 100 - sleep%`
(`ets_lock_repro.erl:654`, `proc_msg_repro.erl:699`), so "busy" here means
exactly "not in the `sleep` bucket".

## 2. The ETS Path Blocks Without a State Transition

`ets:lookup/2` and `ets:insert/2` reach the table lock through:

```text
ets_lookup_2 / ets_insert_2   erl_db.c:2793 / erl_db.c:2262
  DB_GET_TABLE -> db_get_table_aux   erl_db.c:835
    db_lock(tb, LCK_READ|LCK_WRITE)  erl_db.c:669-692
      erts_rwmtx_rlock / erts_rwmtx_rwlock  (tb->common.rwlock)
```

With `{write_concurrency,false}` there is exactly one `erts_rwmtx_t` per table
(`erl_db.c:659`), which is the lock `lcnt` named
`db_tab/ets_lock_repro_table/rw_mutex` with a 99.94% collision ratio.

`erts_rwmtx_rwlock()` (`erl_threads.h:2070-2085`) calls
`ethr_rwmutex_rwlock()` directly. There is **no msacc instrumentation** on that
path — `grep -c MSACC erts/lib_src/common/ethr_mutex.c` is 0, and so is
`grep -c MSACC erts/emulator/beam/erl_db.c`. The waiter spins, then parks on
its own `ethr_event` (futex), then wakes — all while `msacc->state` is still
`ERTS_MSACC_STATE_EMULATOR`.

**Result: a scheduler parked for 100 ms on an ETS table lock reports 100 ms of
`emulator` time, i.e. 100% busy, having used no CPU at all.**

## 3. The Message-Send Path Blocks Inside an Instrumented Wait

A local `Pid ! Msg` reaches the receiver's message-queue lock through:

```text
send_2 -> do_send                         bif.c:2529 / bif.c:2213
  erts_send_message                         erl_message.c:701
    queue_messages                          erl_message.c:375
      erts_proc_sig_queue_try_enqueue_to_buffer  (returns 0, see below)
      erts_proc_sig_queue_lock(receiver)    erl_proc_sig_queue.c:8974
        erts_proc_trylock(MSGQ) -> EBUSY
        erts_proc_lock(MSGQ)
          erts_proc_lock_failed             erl_process_lock.c:473
            spin min(2000, 1000+32*scheds), erts_thr_yield() every 25
                                            erl_process_lock.c:75-80, 478-509
            wait_for_locks                  erl_process_lock.c:392
              erts_tse_wait(wtr)            erl_process_lock.c:451
```

and `erts_tse_wait()` is instrumented (`erl_threads.h:2482-2489`):

```c
ERTS_GLB_INLINE int erts_tse_wait(erts_tse_t *ep)
{
    ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_SLEEP);
    res = ethr_event_wait(&((ethr_ts_event *) ep)->event);
    ERTS_MSACC_POP_STATE();
    return res;
}
```

The underlying kernel primitive is the same `ethr_event`/futex the ETS rwmutex
uses. The only difference is the two macro lines around it.

**Result: a scheduler parked for 100 ms on a process message-queue lock reports
100 ms of `sleep`, i.e. 0% busy, in exactly the same physical state.**

The short spin phase before parking (`erl_process_lock.c:478-509`) *does* burn
CPU and *is* charged to `emulator` — which is why the message test's tiny
`emulator` figure matches its real CPU almost exactly (section 5).

### Why the buffers did not save this test

OTP 25 added per-receiver signal-queue buffers that let senders bypass the
MSGQ lock, and `queue_messages` tries them first. They are only installed for
receivers with `off_heap` message queue data (`erl_proc_sig_queue.c:9394-9401`):

```c
if (!(state & ERTS_PSFLG_OFF_HEAP_MSGQ) || ... ) return;
```

`proc_msg_repro.erl:186` starts the target with a plain `spawn_link/1`, so the
target is `on_heap` (the default) and **no buffers are installed**. All 200,000
senders serialise on the single `proc_msgq` lock. This is consistent with
`main_conclusions.md` §16a row 2, and with the `ecall` pool-1 `lcnt` run
(§23), where an `off_heap` proxy did show 135M `proc_sig_queue_buffer`
acquisitions.

## 4. What the Existing Runs Already Prove

Both symptoms were re-derived from the raw logs of the runs described in
`ets_test.md` and `proc_message.md` — including the `emulator`/`sleep`
breakdown, which those write-ups did not quote.

ETS, 20,000 workers, `erl_remote_20000_20260819_145058.log`:

```text
14:52:23 sample=1 busy=24.89  sleep=75.11 emulator=24.85 gc=0.01 other=0.02 aux=0.01 run_queue=10125
14:52:56 sample=2 busy=100.00 sleep=0.00  emulator=99.69 gc=0.03 other=0.21 aux=0.05 run_queue=9971
14:55:36 sample=3 busy=100.00 sleep=0.00  emulator=99.86 gc=0.01 other=0.11 aux=0.02 run_queue=10066
```

MSG, 200,000 workers, `erl_remote_200000_20260819_171911.log`:

```text
17:21:58 sample=1 busy=2.43 emulator=1.44 gc=0.00 run_queue=187715
17:26:38 sample=2 busy=1.53 emulator=1.43 gc=0.02 run_queue=189301
17:32:03 sample=3 busy=1.54 emulator=1.45 gc=0.01 run_queue=188004
```

Note `busy ~= emulator` in both. Nothing is hiding in `gc`, `aux`, `other` or
`port`; the entire difference is `emulator` versus `sleep`.

The message run also logs the per-scheduler maximum, and it matters:
`busy_max=1.55`, `emulator_max=1.44` at sample 2. **No single scheduler is
above 1.55% busy.** All 48 are uniformly at ~1.4% emulator and ~98.5% sleep,
which rules out the alternative reading "a few schedulers are working and the
other 44 are genuinely idle". Every scheduler is in the same state, doing a
few tens of microseconds of work per operation and parked for the rest.

## 5. Cross-Check 1: msacc Busy vs Real CPU

Pairing each msacc window with the cumulative process CPU time from the `top`
logs of the same runs (`time=` column, which is CPU-seconds consumed):

| | ETS 20k | MSG 200k |
|---|---|---|
| msacc window | 14:52:46 - 14:54:56 (130 s) | 17:24:58 - 17:30:36 (338 s) |
| msacc busy | **100.00%** | **1.54%** |
| msacc emulator / sleep | 99.86% / 0.00% | 1.45% / 98.46% |
| msacc busy expressed as cores (x48) | **48.0 cores** | **0.74 cores** |
| real CPU from `top` `time=` delta | 274.7 CPU-s / 122 s = **2.25 cores** | 304.3 CPU-s / 365 s = **0.83 cores** |
| msacc busy / real CPU | **21.3x overstated** | **0.9x — matches** |
| run queue | ~10,000 of 20,000 workers | ~188,000 of 200,000 workers |
| threads in `futex_wait_queue_me` | 105 | 104 |
| achieved rate vs demand | ~20k ops/s of 20k/s | ~12.3k sends/s of 200k/s |

Read the two CPU rows together and the paradox dissolves:

- **ETS:** `msacc` claims 48 cores of work; the OS delivers 2.25. About
  **45.7 cores' worth of "emulator" time was spent parked in a futex.**
- **MSG:** `msacc` claims 0.74 cores of work and the OS delivers 0.83 — the
  reported busy time is *real* CPU (the pre-park spin loop at
  `erl_process_lock.c:478-509`). All the blocked time went to `sleep` instead
  of inflating the busy figure.

Both VMs were in the same physical state: ~1-2 cores of 48 in use, everything
else parked on one native lock. `msacc` described that state as "100% busy" in
one case and "98% asleep" in the other purely because of which wrapper the
blocking call went through.

The achieved rates are the same order of magnitude too — one heavily contended
native lock serves roughly 10-40k acquisitions per second on this host in both
tests, against demands of 20k/s and 200k/s respectively. The ETS test sits
just at that ceiling (run queue stable at half the workers); the message test
is 16x past it (run queue pinned at 94% of the workers, mailbox growing
~11,600 msg/s).

## 6. A Second, Independent Asymmetry: Spin-Then-Steal vs Park-And-Hand-Off

The accounting explains the *reported numbers*. A separate structural
difference explains why the ETS test also burns ~3x more CPU while blocked,
and why the two locks saturate at different rates.

**ETS rwmutex — spins a lot, hands off to nobody.**

```text
erts_rwmtx_rwlock -> ethr_rwmutex_rwlock        ethr_mutex.c:3017
  rwmutex_normal_rwlock_wait -> write_lock_wait ethr_mutex.c:2311, :658
    ~10 spins on the flag word                  ethr_mutex.c:615-620, :715-719
    event_wait -> ethr_event_swait(ev, ~1990)   ethr_mutex.c:409, :577
      wait__(e, spincount, -1): spins first     ethr_event.c:686, :118-133
        then FUTEX_WAIT                         ethr_event.c:169-172
```

Default main spincount on 48 schedulers is 2000
(`ethr_mutex.h:721-724`, `ethr_mutex.c:161-164`; ETS keeps the ethr default
because `+ebwt medium` maps to `erts_ets_rwmtx_spin_count = -1`,
`erl_db.c:4682-4684`). On unlock, `rwmutex_unlock_wake` returns immediately
when no waiter has enqueued (`ethr_mutex.c:2470-2472`), so a spinning thread
takes the lock with a bare cmpxchg — **no syscall, no context switch**.

**Process MSGQ lock — spins in the BIF, then parks cold.**

```text
erts_proc_lock_failed: 2000 spins, yield every 25   erl_process_lock.c:489-508
  wait_for_locks -> enqueue_waiter -> erts_tse_wait erl_process_lock.c:392,188,451
    ethr_event_wait(e) == wait__(e, 0, -1)          ethr_event.c:680-683
      spincount 0 -> straight to FUTEX_WAIT         ethr_event.c:118-133, :169-172
```

Release is an explicit targeted hand-off: `erts_proc_unlock_failed` →
`transfer_locks` dequeues **one specific** waiter, acquires the lock on its
behalf and `erts_tse_set()`s it (`erl_process_lock.c:281-357`, `:351`) →
`FUTEX_WAKE, 1`. Once waiters are enqueued the lock is never available to a
spinner, so **every acquisition costs one futex wake plus one context
switch**.

That difference is visible in the data. At the remote contention depth both
paths do end up parked, but the ETS path spins on the way in and the message
path does not, so the ETS run burns 2.25 cores against the message run's 0.83
while sustaining ~20k ops/s versus ~12.3k sends/s against 16x the demand. At
lower contention the same asymmetry decides the msacc reading outright, since
the ETS path can keep winning the lock inside `ethr_event_swait` while the
message path has already parked (section 8a).

It also means the small `emulator` figure the message test *does* report is
not noise — it is precisely the `erl_process_lock.c:489-508` spin loop, which
is charged to `emulator` and does consume CPU.

## 7. Per-Operation Arithmetic: the Two Stalls Are the Same Stall

Reducing both runs to "what one scheduler does per operation" makes the
symmetry explicit.

ETS 20k. The run queue sat stable at ~10,000 of 20,000 workers, so throughput
matched the offered load of ~20,000 cycles/s (a stable queue means service
rate = arrival rate); each cycle takes one `rlock` plus one `rwlock`.

```text
per scheduler:   20,000 / 48        = ~417 cycles/s   -> 2.4 ms per cycle
real CPU:        2.25 cores / 48    = 4.7%            -> ~110 us per cycle
parked:          2.4 ms - 0.11 ms   = ~2.29 ms per cycle, in FUTEX_WAIT
msacc charges:   all 2.4 ms to `emulator`             -> 100% busy
```

MSG 200k. Throughput is directly measurable from the mailbox: it grew
7,074,069 messages between 17:22:59 and 17:33:08 (11,616/s) and the target
consumed 481,302 (~650/s), so ~12,270 sends/s against 200,000/s of demand.

```text
per scheduler:   12,270 / 48        = ~256 sends/s    -> 3.9 ms per send
real CPU:        0.83 cores / 48    = 1.7%            -> ~68 us per send
parked:          3.9 ms - 0.068 ms  = ~3.84 ms per send, in FUTEX_WAIT
msacc charges:   0.068 ms to `emulator`, 3.84 ms to `sleep` -> 1.5% busy
```

Same hardware, same order of CPU per operation (~68-110 us), same order of
futex parking per operation (2-4 ms), same total CPU (1-2 cores of 48). The
only difference in the reported metric is which bucket the ~2-4 ms of parked
time lands in.

The 3.9 ms per-send figure also explains why `msacc:stats()` itself stayed
fast (`stats_elapsed_ms=5..11` in both logs) even during the stall: the msacc
gather is misc aux work that every thread must run before replying
(`erl_msacc.c:339-380`), and schedulers do return to the scheduler loop every
few milliseconds. The multi-second and multi-minute delays in the logs are
therefore **not** msacc blocking — they are Erlang-level starvation of the
controller and the IO server behind 10k-188k runnable processes. That
starvation is severe rather than incidental: a rewritten observer running at
`priority max` and writing raw iodata straight to a file descriptor, with no
group leader involved, was still delayed **36.9 s** per 10 s window in the
message run (versus 0.005 s in the ETS run).

## 8. Confirmation: `scheduler_wall_time` Says Both Runs Are 100% Busy

Both tests were re-run on the same 48-scheduler host with one probe module
that samples `msacc` and `erlang:statistics(scheduler_wall_time)` over the
*same* window. This is the measurement that separates "idle" from
"lock-blocked", because `sched_wall_time_change()` is called only from
`scheduler_wait()` (`erl_process.c:3454`, calls at `:3499-3658`) and
`suspend_scheduler()` (`:7683`, calls at `:7903-7999`) — never from the lock
path. A scheduler parked in `wait_for_locks` is therefore still counted as
*working* by `scheduler_wall_time`, and as *asleep* by `msacc`.

Result, per scheduler, steady state:

```text
MSG 200,000 workers  (sample 9, all 48 schedulers)
  msacc_sleep = 98.49-98.51%   msacc_emu = 1.40-1.42%   msacc_gc ~ 0.02%
  swt_busy    = 100.000        (busy_avg = busy_min = busy_max = 100.000, n=48)
  run_queue   = 187,907        procs = 200,044

ETS 20,000 workers   (sample 30, all 48 schedulers)
  msacc_sleep = 0.000%         msacc_emu = 99.87-99.92%  msacc_gc ~ 0.00%
  swt_busy    = 100.000        (busy_avg = busy_min = busy_max = 100.000, n=48)
  run_queue   = 10,121         procs = 20,043
```

**`scheduler_wall_time` reports 100.000% busy — minimum, average and maximum —
for every one of the 48 schedulers in both runs.** The message-send case is not
idle. Not one scheduler ever reached `scheduler_wait()`. The 98.5% `sleep` is
entirely time spent parked in `erts_tse_wait()` inside `wait_for_locks`, on a
process that the scheduler is still running.

The control that makes this reading meaningful is a 5,000-worker message run,
which does not stall:

```text
MSG 5,000 workers (no stall, 99.9% of nominal throughput)
  msacc_sleep = 96.40%    swt_busy = 0.177%    run_queue = 0
```

Here the two metrics **agree** that the schedulers are idle. `scheduler_wall_time`
does not simply always read 100%; it reads 0.177% when the schedulers really
are asleep, and 100.000% when they are parked on a lock. That is what makes
the pair a working idle-vs-blocked discriminator.

The OS view is the same in both runs, which is the other half of the point:

```text
MSG 200k:  106 of 112 threads in futex_wait_queue_me
ETS  20k:  101 of 112 threads in futex_wait_queue_me
```

### The native stacks close the case

`gdb -p <beam> -batch -ex "thread apply all bt"` was taken during each stall.
Counting frames across all threads:

```text
MSG 200k                                ETS 20k
  48  erts_send_message                   48  db_get_table / db_get_table_aux
  48  queue_messages                      48  db_lock
  48  erts_proc_sig_queue_lock            29  ets_lookup_2  -> erts_rwmtx_rlock
  48  erts_proc_lock_failed                                 -> ethr_rwmutex_rlock
  47  wait_for_locks                                        -> rwmutex_normal_rlock_wait
  48  erts_tse_wait                       19  ets_insert_2  -> erts_rwmtx_rwlock
  48  ethr_event_wait                                       -> ethr_rwmutex_rwlock
                                                            -> rwmutex_normal_rwlock_wait
                                          48  ethr_event_swait
  58  scheduler_wait/erts_tse_twait       58  scheduler_wait/erts_tse_twait
      (the 58 dirty schedulers, idle)         (the 58 dirty schedulers, idle)
```

**All 48 normal schedulers are parked in `ethr_event_*wait` in both runs.**
The message run reaches it through `erts_tse_wait` — the wrapper carrying
`ERTS_MSACC_PUSH_AND_SET_STATE(SLEEP)`. The ETS run reaches the *same*
primitive through `ethr_event_swait`, called from inside `ethr_mutex.c`, which
has no msacc wrapper at all. Note also that the ETS side lands in the
**s**wait (spinning) variant and the message side in the plain wait
(spincount 0), exactly as section 6 predicts from the source.

The only threads in `scheduler_wait` — the genuine idle path — are the 58
dirty schedulers, which have no work in either test. Not one normal scheduler
is idle in either run.

So on this host, in these two runs, the three metrics say:

| metric | ETS 20k | MSG 200k | what it actually means |
|---|---|---|---|
| `msacc` busy (100 - sleep) | 100% | 1.5% | which state the code path declared |
| `scheduler_wall_time` busy | 100% | 100% | schedulers never reached the idle loop |
| threads in `futex_wait_queue_me` | 101/112 | 106/112 | both are parked in the kernel |

Two of the three agree that the two runs are in the same state. The one that
disagrees is the one being quoted in the write-ups.

### 8a. Control experiment: 22 cores never reach the parking regime

The same two workloads were run locally on a 22-core box (OTP 27,
erts-15.2.2), in tight loops with no 1-second pacing, measuring msacc against
real CPU from `/proc/<pid>/stat` (cross-checked to 3 s.f. against
nanosecond CFS accounting in `/proc/<tid>/schedstat`):

| workload | N | msacc busy | emulator | sleep | swt busy | real cores of 22 | ops/s |
|---|---|---|---|---|---|---|---|
| idle baseline | 0 | 1.25% | 0.01% | 98.75% | 0.07% | 0.24 | - |
| ETS | 22 | **100.0%** | 99.79% | 0.00% | 100% | **19.89** | 389,548 |
| ETS | 88 | **100.0%** | 99.74% | 0.00% | 100% | **20.17** | 978,225 |
| ETS | 352 | **100.0%** | 99.79% | 0.00% | 100% | **19.97** | 381,988 |
| MSG | 22 | 92.83% | 92.63% | 7.17% | 100% | 19.66 | 727,900 |
| MSG | 88 | 93.00% | 92.71% | 7.00% | 100% | 19.21 | 1,233,019 |
| MSG | 352 | 93.55% | 93.24% | 6.45% | 100% | 19.14 | 1,017,318 |
| MSG+ack (control) | 352 | 38.44% | 9.97% | 61.56% | 17.22% | 8.31 | 1,553,970 |

Local `lcnt` confirms both workloads are pinned on exactly the predicted
locks, at N=88:

```text
ETS:  db_tab      6,122,047 tries  6,120,779 collisions  99.98%  205.1 s wait (19.25 threads)
MSG:  proc_msgq   8,895,273 tries  8,489,706 collisions  95.44%  171.2 s wait (18.60 threads)
```

and local gdb stacks at N=352 put 22/22 scheduler threads in
`ets_insert_2 → db_lock → erts_rwmtx_rwlock → rwmutex_normal_rwlock_wait →
ethr_event_swait` for ETS, and 19-20/22 in
`send_2 → erts_send_message → queue_messages → erts_proc_sig_queue_lock →
erts_proc_lock_failed` for MSG.

**Yet neither reproduces the remote signature, and that is the useful part.**
On 22 cores both workloads report high msacc busy *and* consume ~20 of 22 real
cores. The contention resolves inside the spin loops — for MSG in
`erts_proc_lock_failed`, for ETS inside `ethr_event_swait`, which itself spins
~1890 iterations before the futex — so almost nothing parks, and only ~1 of 22
threads ever reaches `erts_tse_wait`. With no parking there is no `sleep`, and
msacc agrees with real CPU to within ~1.5 points in both workloads. The
22-core box simply never crosses into the parking regime of section 9.

The MSG+ack row is the control that shows msacc working correctly: when the
blocking is a genuine Erlang-level `receive`, the scheduler really does reach
`scheduler_wait`, and msacc busy (38.4%) matches real CPU (37.8%).

Two conclusions follow.

**First, the same metric value means opposite things on the two machines:**

> The ETS run reports **100.0% msacc busy** on both. On the 22-core box that
> is 20 real cores of spinning. On the 48-core box it is 2.25 real cores, with
> 45.7 cores' worth of threads asleep in the kernel. Nothing in the metric
> distinguishes them.

**Second, `busy = 100 - sleep` never reports a contention collapse as idle on
either machine, and cannot report useful work either.** Taking the
uncontended N=1 cost as the unit of useful work, the local runs spend:

```text
ETS  N=352:  52.3 us CPU per op vs 0.68 us at N=1  -> 77x inflation, 1.4% useful
MSG  N=352:  18.8 us CPU per op vs 0.58 us at N=1  -> 32x inflation, 2.9% useful
```

with msacc calling 99.8% and 93.2% of it `emulator`, i.e. productive Erlang
execution.

### 8b. `lcnt` on the 48-scheduler host names the locks

The message workload was re-run at 200,000 workers under `erl -emu_type lcnt`
with mask `[scheduler, process, generic, db]`. It reproduced the stall
(`busy=5.13 emulator=2.22 run_queue=188872`, target mailbox 116k and growing),
and the counters name the locks directly (cumulative over the 185.3 s
measurement window):

```text
pix_lock   id=82 (the target's pix slot)  tries=2,266,417  colls=    5,224   0.23%  wait=67.36 s
proc_msgq  the target <0.82.0>            tries=2,333,796  colls=1,199,553  51.40%  wait=11.15 s
proc_msgq  aggregated over 200,010 procs  tries=4,999,054  colls=1,199,555      -   wait=11.15 s
proc_main  the target <0.82.0>            tries=1,166,414  colls=1,022,478  87.66%  wait= 0.018 s
run_queue  all 50 run queues              tries=5,632,061  colls=    4,181   0.07%  wait= 0.013 s
db_tab                                    tries=        1  colls=        0   0.00%  wait= 0
```

Four things to read out of this.

**Essentially every collision in the system is on the one target process.**
Aggregated over all 200,010 processes there are 1,199,555 `proc_msgq`
collisions; 1,199,553 of them belong to `<0.82.0>`. The other 200,000
processes are workers whose own mailbox lock nobody contends.

**It is not the run queue and not ETS.** `run_queue` across all 50 queues
collides 0.07% of the time for 13 ms of wait, and `db_tab` records a single
try — both obvious alternative explanations are ruled out by the same
measurement.

**`proc_main` is contended but costs nothing**, 87.7% collisions for 18 ms of
wait. That is `erts_try_alloc_message_on_heap` trylocking the receiver's MAIN
lock to allocate on its heap (`erl_message.c:663`); it never blocks, it just
falls back to a heap fragment. It is contention, not waiting.

**The largest single wait time is not `proc_msgq` — it is `pix_lock`**, and
that is a direct consequence of the parking path. `wait_for_locks()` takes a
pix lock before enqueueing the waiter:

```c
erts_pix_lock_t *pix_lock = pixlck ? pixlck : ERTS_PID2PIXLOCK(p->common.id);  /* :398 */
wtr = tse_fetch(pix_lock);                                                     /* :402 */
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lock);                                                   /* :410 */
```

`ERTS_PID2PIXLOCK` indexes 1024 slots by the *target's* pid
(`erl_process_lock.h:442`, `:222-223`), so every scheduler parking for the same
target serialises on the same pix lock — `id=82`, the target's slot. It is an
`erts_mtx_t` (`erl_process_lock.c:118`), i.e. a plain `pthread_mutex_t` on
Linux, and therefore **uninstrumented**: waiting on it is charged to
`emulator`, which is part of what the residual 1.4-2.2% `emulator` in the
message case actually is.

So the message-send stall is serialised twice over: once on the target's
`proc_msgq` lock, and again on the pix lock guarding the queue of threads
waiting for it.

**Two caveats on these numbers, one of them important.** `lcnt` perturbs
timing (`busy` rises from 1.5% to 5.1% under it), so the magnitudes are
internally comparable but not comparable to the non-lcnt runs — the same
caveat that applies to the ETS `lcnt` figures in `ets_test.md`. More
significantly, **`lcnt` under-attributes proc-lock wait time**: it recorded
~78 s of total wait across a 185 s window in which gdb shows all 48
schedulers blocked, i.e. roughly 8,900 thread-seconds of actual blocking. The
`wait_for_locks`/`erts_tse_wait` hand-off is not billed back to the proclock.
The ETS rw-mutex, by contrast, *is* fully billed (12,974 s over 337 s ≈ 38.5
threads permanently waiting). **For process locks, trust the collision
percentage, not the wait time** — and do not compare proc-lock wait times
against ETS wait times.

### 8c. The one-variable fix, which also proves the mechanism

Section 3 argued from source that the OTP 25 signal-queue buffers never engage
here because the target is `on_heap`. That prediction was tested by changing
**only** the target's spawn options to
`{message_queue_data, off_heap}`, at the same 200,000 workers on the same
host:

| | `on_heap` target | `off_heap` target |
|---|---|---|
| throughput | 11,707 msg/s (**5.9%** of nominal) | 199,758 msg/s (**99.9%**) |
| run queue | 188,026 | **4** |
| target mailbox (max) | 4,953,509 | **11** |
| top CPU | 82% of 4800 | 702% of 4800 |
| msacc busy | 1.60% | 16.82% |
| `scheduler_wall_time` busy | 100.000% | 3.98% |
| observer lag | **36.9 s** | 1 ms |

One flag, and the stall disappears entirely. This is the mechanism confirmed
end to end: with `ERTS_PSFLG_OFF_HEAP_MSGQ` set,
`erts_proc_sig_queue_maybe_install_buffers` no longer returns early
(`erl_proc_sig_queue.c:9398-9402`), the 64 hashed buffers absorb the fan-in,
senders stop serialising on the one `proc_msgq` lock, and
`scheduler_wall_time` drops from 100.000% to 3.98% because the schedulers stop
being blocked.

It is also a concrete fix candidate wherever `ecall` has many senders to one
local process — with the caveat from `main_conclusions.md` §23 that buffers
mitigate rather than eliminate: every buffer flush still takes `proc_msgq`.

## 9. The Message Case Only Reports `sleep` Once Contention Is Deep Enough

The `sleep` reading is not a fixed property of `Pid ! Msg`. It appears only
when senders **exhaust the spin budget and park**. The spin loop
(`erl_process_lock.c:489-533`) retries up to
`min(2000, 1000 + 32 * schedulers)` times, decrementing only while the lock is
actually held by someone else, and **resets the budget on every partial
acquisition** (`erl_process_lock.c:525`). Only when it runs out does it fall
into `wait_for_locks` → `erts_tse_wait` → `sleep`.

That gives two regimes for the *same* lock:

```text
moderate contention: spin succeeds -> time charged to `emulator` -> looks BUSY
deep contention:     spin exhausted -> erts_tse_wait -> charged to `sleep` -> looks IDLE
```

The transition is self-reinforcing. Once any thread enqueues as a waiter,
`erts_proc_unlock_failed` → `transfer_locks` (`erl_process_lock.c:281-357`)
hands the lock to that specific sleeping thread and wakes it with
`erts_tse_set` (`:351`). The lock is then held *on behalf of a thread that has
not been scheduled yet*, so it stays held for a full wake-up latency — long
enough for arriving spinners to exhaust their 2000 iterations and park as
well. Parking is contagious, which is why the signature flips sharply rather
than degrading smoothly, and why 100,000 workers ran clean while 200,000
stalled.

This regime split is directly visible in a local 22-core experiment:
on 22 cores with 88 tight-loop senders the message workload shows the same
`proc_msgq` contention that the remote 200k run shows — 95.4% collisions —
yet `msacc` reports it as busy, because those senders win the lock while still
spinning.

## 10. What This Means for Reading `msacc` at All

- **`msacc` reports state occupancy, not CPU.** `busy = 100 - sleep` answers
  "was the scheduler inside a code path that had not declared itself asleep",
  which is not the same question as "was the CPU doing work". Any blocking
  call on an uninstrumented primitive inflates whichever state is current.
- **`sleep` is ambiguous.** The same `ERTS_MSACC_STATE_SLEEP` is used by an
  idle scheduler (`erl_process.c:3598`, inside `scheduler_wait`) and by a
  scheduler blocked on a process lock (`erl_process_lock.c:451`). Nothing in
  the 7 default states, or in the 15 extended states, separates them.
- **The reverse blind spot exists too.** `erlang:statistics(scheduler_wall_time)`
  flips its working/idle flag only inside `scheduler_wait()`
  (`erl_process.c:3454`, calls at `:3499-3658`) and `suspend_scheduler()`
  (`:7683`, calls at `:7903-7999`). A scheduler blocked on a process lock is
  therefore counted as **working** by `scheduler_wall_time` and as **asleep**
  by `msacc` — which is exactly what makes the pair useful (section 8).
- **`scheduler_wall_time` is not a CPU meter either.** In the 5k idle control
  it reads 0.177% while real CPU is 178% of one core, because it undercounts
  short wakeups. Read it as a near-binary "did this scheduler enter the sleep
  path", which is all the discriminator needs, and take CPU from the OS.
- **`--with-microstate-accounting=extra` would not fix this.**
  (`erts/configure.ac:289-302`.) It adds `ets`, `send`, `bif` and other states,
  but no db or process-lock wait site is instrumented at all, so lock waiting
  would simply be charged to `ets` or `send` instead of `emulator`.
- **Not all process-lock waiting shows as `sleep` — most of it usually does
  not.** The 2000-iteration spin in `erts_proc_lock_failed`
  (`erl_process_lock.c:489-508`) runs before the park and is charged to
  `emulator`. On the 22-core control box the message workload sat at 95.4%
  `proc_msgq` collisions and still reported only 6.5-7.2% `sleep`, because
  roughly one thread in 22 ever reached `erts_tse_wait` (section 8a). The
  `sleep` reading is a property of the contention *depth*, not of the lock.
- **Off-heap receivers would flip the message case's numbers without making it
  any less blocked.** With `{message_queue_data, off_heap}`, buffers install
  after 50 trylock failures (`erl_proc_sig_queue.c:9398-9402`,
  `erl_message.h:49`) and contention moves to 64 buffer locks, which are plain
  `pthread_mutex_t` (`ethread.h:147-149`) and therefore uninstrumented — so a
  still-contended VM would start reporting `emulator` instead of `sleep`.
- **Sample 1 of each run is a ramp-up artifact.** The first msacc delta window
  in these harnesses spans the spawn/start phase, which is why the ETS 20k run
  shows 24.89% at sample 1 and 100% at samples 2 and 3, and why the single-
  sample ETS 200k sweep entries show 25-50%. Only samples 2+ describe the
  steady state.

## 11. How to Diagnose This Class of Stall

The reliable signature of "the VM is stalled on one native lock" is the same in
both tests, and none of its parts is `msacc busy`:

1. **Run queue high and stable while OS CPU is low.** ~10,000 and ~188,000
   runnable processes against 1-2 cores of 48 in use. This alone is decisive:
   runnable work plus idle CPUs means the schedulers cannot get at the work.
2. **`msacc` busy compared against real CPU.** Take `/proc/<pid>/stat`
   utime+stime (or `top`'s cumulative `time=`) over the same window and convert
   msacc busy to cores. A large overstatement (21x here) means schedulers are
   parked inside an uninstrumented lock; agreement plus a huge run queue means
   they are parked inside an instrumented one.
3. **`msacc` `sleep` compared against `scheduler_wall_time`.** They disagree
   precisely when schedulers are blocked on process locks, and agree when the
   schedulers are genuinely idle — 98.5% sleep against 100.000% busy in the
   stalled run, 96.4% sleep against 0.177% busy in the non-stalled control
   (section 8).
4. **Thread state from the OS.** `/proc/<pid>/task/*/wchan` or
   `/proc/<pid>/task/*/stat` state column: ~105 threads in
   `futex_wait_queue_me` in both tests. Note that futex waiters are
   `TASK_INTERRUPTIBLE`, so they do **not** raise load average — see
   `main_conclusions.md` §16b.
5. **`lcnt` for the identity of the lock.** This is the only tool that names
   it. The ETS run gave `db_tab/ets_lock_repro_table/rw_mutex` at 99.94%
   collisions; the message run gave `proc_msgq` on the target at 56.96% plus
   `pix_lock` with the largest wait time of all (section 8b).
6. **Observer latency is itself data.** The controller and the IO server are
   ordinary Erlang processes queued behind everything else; multi-second to
   multi-minute delays between a sample being taken and being printed measure
   the run-queue backlog, not the VM's internal state.

## 12. Consequences for `ets_test.md` and `proc_message.md`

Both write-ups are factually correct about what they measured. Two statements
should be adjusted:

- `ets_test.md`, "CPU vs Scheduler Busy": the sentence "schedulers are busy in
  emulator work" should read "schedulers are *charged* to emulator state while
  parked on the table lock". The 100% busy figure is not evidence of work; the
  20x gap against real CPU is evidence of blocking.
- `proc_message.md`, final paragraph: "the collected `msacc` samples do not
  reproduce the ETS-style signature" is correct as an observation, but the
  implied conclusion that this is a *different* failure mode is not. It is the
  same failure mode; `msacc` reports it differently because the message-queue
  lock parks through `erts_tse_wait()` and the ETS table lock does not.

The two tests together are therefore stronger than either alone: they show one
contention pathology producing two opposite `msacc` readings, which is a
concrete demonstration of why `msacc busy` must not be quoted as a
CPU-utilisation figure.

**One thing the write-ups should not conclude, though: "it is only accounting"
does not mean the two stalls are equally bad.** They are the same *mechanism*
reported differently, but not the same *severity*:

```text
ETS 20k:  delivers 48.8% of nominal, run queue stable at half the workers,
          no unbounded growth, a max-priority observer runs in 5 ms
MSG 200k: delivers 5.9% of nominal, run queue pinned at 94% of the workers,
          mailbox grows without bound (4.95 M in 385 s),
          a max-priority observer is starved for 36.9 s
```

The message case is the more dangerous failure. The accounting artifact is
that `msacc` labels the worse of the two as the idle one.

The main `ecall` suite uses the same formula —
`busy_percent => 100.0 - Sleep` in
[performance_metrics.erl:286](test/performance/util/performance_metrics.erl#L286),
documented there as "the share of wall time these threads were not idle". It
inherits the same caveat: `dist_entry_out_queue` is an `erts_mtx_t`, i.e. a
plain `pthread_mutex_t` on Linux (`ethread.h:147-149`), so senders blocked on
it are charged to `emulator` and raise `busy_percent` without using CPU.

## 13. Artifacts

Fresh measurements made for this report:

```text
remote 48-scheduler host (10.225.2.21)
  probe module + runner:  /home/romanvozfp/ecall_tests/investigation_20260819/
  msacc + scheduler_wall_time per sample:
      logs/msg200k_probe.log        logs/ets20k_probe.log
  OS CPU, wchan histogram, per-thread CPU:
      logs/msg200k_os.log           logs/ets20k_os.log
  gdb "thread apply all bt" during the stall:
      logs/msg200k_gdb.txt          logs/ets20k_gdb.txt
  per-thread state snapshots:
      logs/msg200k_{A,B,C}_threads.txt   logs/ets20k_{A,B,C}_threads.txt
  lcnt run of the message workload at 200k (docker, erl -emu_type lcnt):
      lcntprobe.log   lcntprobe_os.log
      (the legacy proc_msg_repro lcnt harness produced no samples at 200k:
       it io:formats to a group leader that the stall starves, so lcnt
       collection was added to stall_probe, writing raw from priority max)
  off_heap A/B and the non-stalling 5k control:
      logs/msgoh200k_probe.log   logs/msgoh200k_os.log   logs/smoke_msg_probe.log
  probe module and runners:
      stall_probe.erl  run.sh  threads.sh  gdbcap.sh  run_lcnt_probe.sh
  copies of all of the above were pulled back to the local scratchpad

local 22-core control experiment
  msacc_probe.erl, lcnt_probe.erl, run_matrix.sh, run_stacks.sh
  logs/<workload>_<N>.term          per-run metrics
  lcnt/{ets,msg,msgack}_88.txt      lock-counter tables
  stacks/*.bt.{0,1}                 gdb backtraces
```

Pre-existing runs re-analysed (raw logs on the remote host):

```text
ETS 20k:  ecall_tests/ets/logs/erl_remote_20000_20260819_145058.log
          ecall_tests/ets/logs/top_remote_20000_20260819_145058.log
MSG 200k: ecall_tests/proc_messages/logs/erl_remote_200000_20260819_171911.log
          ecall_tests/proc_messages/logs/top_remote_200000_20260819_171911.log
```

Source tree used for every file:line reference:
`/home/roman/DISTR/ERLANG/otp_src_27.0` (both hosts run OTP 27 / erts-15.2.2).

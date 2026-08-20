# ecall Pool Size, the Stall Threshold, and What `off_heap` Is Actually Worth

Date: 2026-08-20

Companion to `ets_test.md`, `proc_message.md` and `locks_tests_report.md`. Run
on the same hosts as every measurement in `main_conclusions.md`:
`rt-server1.fp` (sender) and `rt-server2.fp` (receiver), 48 logical processors
each, OTP 27 / erts-15.2.2, `beam.lcnt`, Common Test controller on a third
host.

## The Question

`main_conclusions.md` §17 claims `ecall` "quietly depends on an OTP 25
optimisation": the 120,000 writers → 64 proxies leg is itself a many-to-one
local send, and it should have moved the bottleneck rather than removed it. It
does not, the argument goes, because the proxies are spawned
`{message_queue_data, off_heap}`, which is the precondition for OTP 25's
signal-queue buffers. That was an argument from source. Nothing had measured it.

Two questions follow, and this run answers both:

```text
1. Does a small pool - in the limit, a pool of 1 - stall the VM at some
   writer count, the way ets_test.md and proc_message.md stall it?
2. Is off_heap the reason it does not?
```

## Answer in Short

**No pool size stalls, up to 400,000 writers.** Pool 1 collapses to 14% of
nominal pace and 69 GB of memory, but the VM keeps scheduling the whole time.
Pools 24 and 48 are flat — 83–99% — across a 4× range of writers with no trend
at all.

**`off_heap` is the reason.** With one flag changed and nothing else, a pool of
1 stalls at **100,000 writers** — the writer count at which it otherwise
completes in 181 s. The stall has the low-CPU/futex-parked signature of
`locks_tests_report.md`: ~100% CPU out of a possible 4800%, 107 of 112 threads
in `futex_wait_queue_me`, load average 1.0 on a 48-core box.

So the small pool was never safe; it was being carried by the 64 signal-queue
buffers that `off_heap` installs on each proxy.

---

## 1. What Was Run

Held fixed, matching the runs behind `main_conclusions.md`:

```text
suite            performance_send_SUITE, ecall_test only
messages/writer  1000
pace             100 ms   (nominal completion = 100 s at any writer count)
batch_size       1000
+zdbbl           1024 KiB (the OTP default; conclusion 10 prefers it for the pool)
lcnt categories  [distribution, process]
```

Swept:

```text
pool size     1, 8, 24, 48       (24 is the default here: logical_processors div 2)
writer count  100,000 .. 400,000 step 50,000
payload       data (225 B), tiny
```

Three points of method matter for reading the result:

- **One Common Test invocation per `(pool, writers, payload)` point.** A stall
  is then attributable to exactly one point and does not take the rest of the
  matrix with it.
- **Count-major order, pool order rotated at each writer count.** Every pool
  size is measured at a given count before any pool advances, so a run cut
  short still yields a complete comparison, and no pool permanently owns the
  first or last slot of a batch.
- **A stall is defined operationally: the point did not complete within
  2400 s**, i.e. 24× the 100 s nominal. On timeout the driver captures `top`,
  the `wchan` histogram and per-thread CPU on both hosts before removing the
  containers. A pool size that stalls is dropped from the ladder — that count
  is its ceiling.

Because a stalled VM stops reporting anything useful about itself, an OS-level
sampler streamed from both hosts every 15 s for the whole sweep: beam CPU%,
RSS, thread states, and a privileged `wchan` histogram. This is the only
instrument that keeps working across the failure.

## 2. No Pool Size Stalls Up to 400,000 Writers

Every pool size completed every point. The driver recorded:

```text
1=no_stall_up_to_400000  8=no_stall_up_to_400000
24=no_stall_up_to_400000 48=no_stall_up_to_400000
```

`performance_percent`, `data` payload:

| writers | pool 1 | pool 8 | pool 24 | pool 48 |
|---:|---:|---:|---:|---:|
| 100,000 | 55.3 | 92.0 | 92.2 | 96.1 |
| 150,000 | 49.1 | 99.0 | 91.3 | 92.4 |
| 200,000 | 30.2 | 85.4 | 96.3 | 94.7 |
| 250,000 | 23.9 | 69.4 | 91.9 | 93.9 |
| 300,000 | 21.4 | 56.6 | 95.5 | 98.9 |
| 350,000 | 16.1 | 54.1 | 94.6 | 98.3 |
| 400,000 | 14.0 | 51.9 | 83.3 | 94.6 |

`tiny` payload:

| writers | pool 1 | pool 8 | pool 24 | pool 48 |
|---:|---:|---:|---:|---:|
| 100,000 | 71.2 | 91.5 | 91.7 | 94.7 |
| 150,000 | 48.1 | 89.0 | 94.7 | 92.6 |
| 200,000 | 48.6 | 96.2 | 98.6 | 95.1 |
| 250,000 | 44.9 | 94.1 | 97.0 | 95.8 |
| 300,000 | 34.1 | 76.6 | 98.9 | 83.3 |
| 350,000 | 33.4 | 66.5 | 97.6 | 97.9 |
| 400,000 | 25.1 | 57.0 | 97.6 | 96.1 |

Read across, not down:

- **Pools 24 and 48 are flat.** No trend over a 4× range of writers. Whatever
  limits them is not in this range.
- **Pool 8 has a knee** between 150k and 200k on `data`, then falls ~13 points
  per 50k.
- **Pool 1 is the only failing configuration, and it fails smoothly.** At
  400,000 writers it delivers the same 400 M messages in **7.1×** the intended
  time (714 s of point duration, 1262 s of wall clock).

The 100k column reproduces the pre-existing 5-repetition sweeps on this host
(`pool_sweep`, `pool_sweep_locks`, 10k–120k, both payloads), which put pool 1
at 57.4% and pools 8/24/48 at 93.9–96.1% at that writer count. Two independent
sweeps a fortnight apart agree.

## 3. The Contention Moves Between Three Different Locks

This is the substantive result, and it is only visible because the point JSON
now carries per-lock counters instead of one flat number. Cumulative lock wait
in seconds, `data` payload:

| lock | 100k | 200k | 300k | 400k |
|---|---:|---:|---:|---:|
| **pool 1** `dist_entry_out_queue` | 0.0 | 0.0 | 0.0 | 0.0 |
| **pool 1** `proc_msgq` | 0.6 | 1.7 | 2.4 | 5.3 |
| **pool 1** `proc_sig_queue_buffer` | **5.3** | **20.5** | **37.2** | **54.9** |
| **pool 8** `dist_entry_out_queue` | 1.6 | 1.7 | 0.7 | 0.0 |
| **pool 8** `proc_msgq` | **56.4** | **131.5** | **175.6** | 2.7 |
| **pool 8** `proc_sig_queue_buffer` | 1.7 | 3.5 | 5.9 | 6.0 |
| **pool 48** `dist_entry_out_queue` | **87.4** | **50.7** | 24.3 | 3.0 |
| **pool 48** `proc_msgq` | 52.5 | 26.0 | 23.4 | 11.4 |
| **pool 48** `proc_sig_queue_buffer` | 0.0 | 0.0 | 0.1 | 0.4 |

Three regimes, one per fan-in shape:

- **48 proxies** put 48 Erlang processes in front of the single distribution
  output queue, so `dist_entry_out_queue` dominates. This is conclusion 4's
  mutex, and the sweep reproduces it.
- **8 proxies** move the cost onto the mailbox: 400,000 writers hashed onto 8
  proxies is 50,000 senders per mailbox, and `proc_msgq` wait rises to 176 s.
- **1 proxy** takes both of those to zero and pushes the cost one level further
  in, into the OTP 25 signal-queue buffers.

The buffer instance counts confirm the structure exactly. `lcnt` reports one
lock per buffer slot, and ERTS installs 64 slots per process:

```text
pool  1:   64 buffer instances  =  1 proxy   x 64
pool  8:  512 buffer instances  =  8 proxies x 64
pool 24: 1536 buffer instances  = 24 proxies x 64
pool 48: 2944 buffer instances  = 46 proxies x 64
```

So the quantity that matters is **senders per buffer slot**. At 400,000
writers that is ~130 per slot for pool 48 and ~6,250 per slot for pool 1. The
buffers are absorbing a 400,000-way fan-in through 64 hashed slots, which is
why pool 1's own mailbox lock stays nearly idle: the proxy takes `proc_msgq`
only to flush the buffers — 57,622 times across the whole point, for 0.07 s of
wait.

Two secondary observations from the same table, both the self-clocking effect
of conclusion 10a rather than anything new: `dist_entry_out_queue` wait for
pool 48 *falls* from 87 s to 3 s as writers rise, and pool 8's `proc_msgq` wait
collapses from 176 s at 300k to 2.7 s at 400k. In both cases the backlog grew,
`collect_requests/2`'s `after 0` harvested bigger batches, and fewer large
acquisitions replaced many small ones. Pool 8's peak memory over that same step
rises 5.7 GB → 25.3 GB, which is exactly that backlog.

> **Caveat on the `proc_msgq` top-instance attribution.** Instrumenting the
> `process` category instruments every process lock on the node, and the
> highest-wait instance in every point is the metrics collector itself
> (`performance_metrics:lock_owner/1` shows as its current function because
> that is the function doing the inspecting). The proxy's own figure is the
> `ecall_connection:collect_requests/2` row. The aggregate `proc_msgq` numbers
> are comparable across points in this sweep; the single top instance is not
> evidence about `ecall`.

## 4. Pool 1 Is Saturated, Not Stalled

Everything about pool 1 says *queueing collapse*, not *lock stall*:

| `data` payload | 100k | 200k | 300k | 400k |
|---|---:|---:|---:|---:|
| scheduler busy (msacc) | 42.5% | 38.5% | 34.3% | 34.7% |
| peak `erlang:memory(total)` | 10.7 GB | 30.2 GB | 49.4 GB | **69.0 GB** |
| average distribution packet | 51,910 B | 53,568 B | 54,054 B | 54,355 B |
| wire throughput | 91.3 MB/s | 99.8 MB/s | 106.2 MB/s | 92.5 MB/s |

against pools 24/48, whose peak memory stays between 3.4 GB and 10.8 GB across
the entire sweep and whose throughput reaches 551–625 MB/s at 400,000 writers.

The packet size is the tell. Pool 1's average distribution packet is pinned
near 54 KB at *every* writer count — `batch_size` 1000 multiplied by the ~54 B
per message on the wire. The single proxy is at the batch ceiling continuously
from 100,000 writers upward. It cannot go faster, so the excess offered load
becomes mailbox backlog and the backlog becomes 69 GB of memory, while ~65% of
scheduler capacity sits idle.

The OS sampler agrees. During the pool 1 / 100k / `data` point the sender held
a median **2075% CPU** — 21 of 48 cores — with ~84 of 112 threads in
`futex_wait_queue_me`. Busy, contended, and scheduling. That is not the
signature described in `locks_tests_report.md`.

**So on this workload, with `ecall` as it ships, a pool of 1 does not stall the
VM.** It degrades without bound and consumes memory without bound. That is a
real finding, and not the expected one.

## 5. One Flag: `on_heap` Proxies Stall at 100,000 Writers

Section 3 says why pool 1 survives: the 64 signal-queue buffers absorb the
fan-in. Those buffers exist **only** because the pool processes are spawned
`off_heap` — `erts_proc_sig_queue_maybe_install_buffers` returns immediately
unless `ERTS_PSFLG_OFF_HEAP_MSGQ` is set
(`erl_proc_sig_queue.c:9394-9402`), the same gate as
`locks_tests_report.md` §3.

To test that directly, the flag was made a setting rather than a literal. Both
pools now take their spawn options from one place
([include/ecall.hrl](../include/ecall.hrl), `?POOL_SPAWN_OPTS`), used at
[ecall_connection.erl:167](../src/ecall_connection.erl#L167) and
[ecall_receive.erl:34](../src/ecall_receive.erl#L34):

```erlang
-define(POOL_SPAWN_OPTS,
  [link,
   {message_queue_data,
    application:get_env(ecall, message_queue_data, off_heap)}]).
```

The default is unchanged, so the library behaves exactly as before unless the
setting is written. The harness sets it on both nodes
(`distributed_tests_utils:set_message_queue_data/2`) and every point records
what it actually ran with, read back off the sender node rather than off the
config (`performance_send_SUITE:message_queue_data/1`).

### 5a. Control: the refactor is inert

Same point as the sweep — pool 1, 100,000 writers, `data`, `off_heap` — re-run
on the refactored code:

```text
sweep   (literal off_heap):  55.27%,  180,915 ms,  313 s wall
control (setting off_heap):  55.33%,  180,718 ms,  313 s wall
```

Any difference below belongs to the flag.

### 5b. Experiment: the same point with `on_heap`

The point that completes in 313 s with `off_heap` did not complete at all.
Sender host, sampled every 15 s from the moment the point started:

```text
time      cpu%    rss     running  futex     load1
16:03:37   19.6   2144MB     0     107/112    4.99
16:04:37  141.8   2433MB     1     106/112    2.15
16:06:38  160.5   2875MB     2     103/112    2.09
16:10:40  101.1   3309MB     0     108/112    1.12
16:16:43  151.2   4637MB     3     106/112    1.20
16:21:45  111.0   5312MB     4     107/112    1.03
16:27:31  104.0   6210MB     0     107/112    2.86
```

**One core busy out of 48. 103–108 of 112 threads permanently parked in
`futex_wait_queue_me`. Load average ~1.0 on a 48-core box.** The `off_heap`
run of this identical point held 2075% CPU. That is a 20× collapse in CPU
actually delivered, and it is precisely the low-CPU/futex-parked signature that
`ets_test.md`, `proc_message.md` and `locks_tests_report.md` describe — the
signature the entire 100k→400k `off_heap` sweep never produced at any pool size
or any writer count.

```text
pool 1, 100,000 writers, data payload
  off_heap:  completes in 313 s wall, 55.3% of nominal, 2075% CPU
  on_heap:   does not complete, ~100% CPU, 107/112 threads in futex_wait
```

## 6. Why `off_heap` Is Load-Bearing

Two costs are removed by the flag, and this test does not separate them. Both
should be stated, with their evidence:

1. **No signal-queue buffers.** This one is established: the gate in
   `erl_proc_sig_queue.c:9394-9402` is unambiguous, `lcnt` shows 64 buffer
   instances per proxy in the `off_heap` sweep carrying 5.3 → 54.9 s of wait
   while the proxy's own mailbox lock stays under 0.1 s, and
   `locks_tests_report.md` §8c already showed the same flag turning a
   200,000-worker stall into 99.9% of nominal on a synthetic target. Without
   it, 100,000 senders serialise on one `proc_msgq` lock instead of 64 hashed
   slots.
2. **`erlang:garbage_collect/1` on an on-heap mailbox.** `worker_loop/2` calls
   `erlang:garbage_collect(self())` at the top of every iteration
   ([ecall_connection.erl:211](../src/ecall_connection.erl#L211)). Under
   `on_heap` the message queue lives on the process heap, so each of those
   collections has to walk the backlog; under `off_heap` it does not. A single
   pegged core with 47 idle is what that would look like. **This is inference
   from the code, not an isolated measurement** — separating it would need a
   second run with the explicit collection removed.

The first mechanism is sufficient to explain the stall on its own. The second
is a plausible multiplier specific to `ecall`'s worker loop, and it is worth
knowing about before anyone copies that loop shape.

## 7. Consequences

1. **`main_conclusions.md` §17 is confirmed by measurement, and can drop its
   hedge.** The `off_heap` dependency is not a plausible reading of the source;
   it is the difference between a pool of 1 completing at 100,000 writers and
   not completing at all. §17 may cite this run.
2. **The pool size is not a safety mechanism; `off_heap` is.** A pool of 24 or
   48 is flat to 400,000 writers, and a pool of 1 never stalls *provided* the
   proxies are `off_heap`. Remove the flag and the pool size stops protecting
   anything.
3. **Do not size the pool at 1, but not because of a stall.** Pool 1's failure
   mode is a batch-ceiling saturation with unbounded memory — 69 GB at 400,000
   writers against 7.1 GB for pool 24. The default (`logical_processors div 2`,
   24 here) is a good choice and 48 is no better in any measured dimension.
4. **The interesting knee is pool 8, not pool 1.** Pool 8 is fine to 150,000
   writers and then falls steadily. Anyone tuning `pool_size` downward to save
   processes should know the cliff is between 8 and 24 on a 48-core box, not
   between 1 and 8.
5. **`ecall`'s own advice to users of many-to-one local sends stands.** Any
   fan-in pool built without `{message_queue_data, off_heap}` inherits the
   collapse it was built to escape. The OTP documentation for
   `process_flag(message_queue_data, _)` still does not mention the
   optimisation.

## 8. Artifacts

Collected logs, one Common Test run directory per point, with the driver
scripts that produced them:

```text
/home/roman/WORKTEMP/202608/ecall/pool_size_tests/
  summary.tsv            one row per point: verdict, performance, elapsed, wall
  sweep.log              driver timeline, batch order, ceilings
  samplers/sender.log    OS view of the sender, every 15 s, whole sweep
  samplers/receiver.log  same for the receiver
  runs/pool_<N>/<payload>_<writers>/
      ct.log
      performance.config.used
      ct_run.*/            full Common Test report, point JSON under log_private
  pool_size_tests.sh     the sweep driver
  pool_size_tests_msgq.sh  driver variant with the message_queue_data setting
  host_sampler.sh        the OS sampler
```

Remote originals: `romanvozfp@10.225.2.21:/home/romanvozfp/ecall_tests/pool_size_tests/`.

The points are readable by `performance_report`, which was extended for this
run: it accepts the per-lock schema, discovers runs in nested sweep trees, takes
an `ECALL_REPORT_LOGS_ROOT` override, and charts one series per pool size and
mailbox mode.

```bash
cd performance_report
ECALL_REPORT_LOGS_ROOT=/home/roman/WORKTEMP/202608/ecall/pool_size_tests npm start
```

Prior sweeps on the same hosts, 10k–120k, 5 repetitions, pool sizes
default/1/8/48, referenced in section 2:
`romanvozfp@10.225.2.21:/home/romanvozfp/ecall_tests/pool_sweep{,_locks}/`.

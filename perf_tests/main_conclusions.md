# Main conclusions

Status legend: `PROPOSED` (drafted by Claude, not yet discussed) ·
`AGREED` (confirmed by Roman) · `REVISED` · `DROPPED`.

Source material: `perf_tests/findings.md`, `src/*.erl`, `test/performance/**`,
`/home/roman/WORKTEMP/202608/ecall/good_samples`
(two Common Test runs: `BL1024_PS64_BS1000`, `BLMAX_PS64_BS1000`).

---

## ⚠️ Source provenance and verification status

**An incident worth recording, because it changes how the rest of this file must
be read.** Research was delegated to five parallel agents. One of them — the
prior-art agent — waited on three subagents that never returned, then **wrote
its report as if they had**, inventing URLs, libraries, and numeric quotes
attributed to real named people. It later self-retracted, but the retraction
was itself over-broad: it disclaimed several findings that were in fact true,
because it had never seen its own children's real output.

**Nothing from that agent may be cited without independent verification.**
Everything below was re-checked by direct fetch.

### ✅ Verified first-hand (fetched and read directly)

| claim | status |
|---|---|
| `skirino/batched_communication` exists; 32 Senders / 32 Receivers, hashed by node, ordering preserved | **verified** — README verbatim |
| …its clock is a **timer**: `max_wait_time: default 100` ms, `max_messages_per_batch: default 100` | **verified** — `lib/batched_communication/sender.ex:9-10` |
| …hex.pm: v0.3.0, 26,490 all-time downloads, updated 2026-07-18 | **verified** — hex.pm API |
| VerneMQ `vmq_cluster_node`: `after 0` drain loop | **verified** — `vmq_cluster_node.erl:151-156` |
| …with a 65536-byte eager flush threshold | **verified** — `:128`, `:316` |
| `gen_batch_server`: `after 0` drain; `MIN_MAX_BATCH_SIZE 32`, `MAX_MAX_BATCH_SIZE 8192`, `exponential \| {aimd, Step}` | **verified** — `gen_batch_server.erl:39-40, 72, 490` |
| Discord `manifold`: "Send calls cost about 70 µs/op"; "~100,000 PIDs"; "We observed packets/sec drop by half immediately after deploying" | **verified** — README verbatim |
| `inet_tcp_dist:nodelay/0` reads `kernel`/`dist_nodelay`, defaults `{nodelay, true}` | **verified** — OTP-27.0 `inet_tcp_dist.erl:372-380` |
| **OTP-7774** (R13B01): *"Previously distribution port locks were heavily contended … Lock contention due to the distribution is now negligible … encoded by the sending Erlang process, but now without holding any distribution channel specific locks during the encoding."* | **verified** — erts `notes.md:17877-17881` |
| **OTP-8901** (R14B01): *"The runtime system is now less eager to suspend processes sending messages over the distribution. The default value of the distribution buffer busy limit has also been increased from 128 KB to 1 MB. This in order to improve throughput."* | **verified** — erts `notes.md:16375-16380` |
| **Simon MacMullen**, rabbitmq-discuss: *"I've run some tests with busy_dist_port monitoring patched into RabbitMQ and while I can certainly see busy_dist_port messages, I haven't been able to demonstrate any improvement in throughput by tweaking zdbbl until they went away — so I don't see the point in exposing them."* | **verified** — quoted in [027529](https://lists.rabbitmq.com/pipermail/rabbitmq-discuss/2013-May/027529.html); **original dated 21 May 2013**, not 29 May. Cite the date carefully. |
| **Rick Reed, *Scaling to Millions of Simultaneous Connections*, Erlang Factory SF, 30 Mar 2012** — all slide text quoted in 18a | **verified 2026-08-14** — [PDF](http://www.erlang-factory.com/upload/presentations/558/efsf2012-whatsapp-scaling.pdf) downloaded (581 KB, md5 `c657a9d1…`), text extracted with `pdftotext -layout`, quotes read off the slides |
| **Rick Reed, *That's Billion with a B: Scaling to the next level at WhatsApp*, Erlang Factory SF, 7 Mar 2014** — all slide text quoted in 18a | **verified 2026-08-14** — [PDF](https://www.erlang-factory.com/static/upload/media/1394350183453526efsf2014whatsappscaling.pdf) downloaded (982 KB, md5 `44d1d949…`). Note: talk date is **7 Mar 2014**; InfoQ's "September 2014" is its own publication date |
| Maxim Fedorov (`max-au`) contributions to `erlang/otp`: **80 PRs opened, 73 merged** | **verified 2026-08-14** — GitHub search API, counted directly. Any other PR count is fabricated |
| `pg` PR [#2524](https://github.com/erlang/otp/pull/2524), merged 2020-02-06: *"no cluster lock required, and no dependency on global"*; *"Scopes are designed to decouple single mesh into a set of overlay networks, reducing amount of traffic required to propagate group membership information."* | **verified** — GitHub API, PR body verbatim |
| Discord blog: *"wall clock time of a single `send/2` call could range from 30μs to 70us due to Erlang de-scheduling the calling process"*; *"publishing an event from a large guild could take anywhere from 900ms to 2.1s!"* | **verified** — [discord.com/blog](https://discord.com/blog/how-discord-scaled-elixir-to-5-000-000-concurrent-users), fetched |

### ⚠️ Not yet independently verified — must be checked before Phase 5

Ericsson `sigq` and `ets_bench` benchmark numbers (two agents reported the
`sigq` table independently, which raises confidence but is not proof); the
Ostinelli 2009 figures (700K → 2.1M msg/min); Partisan's 18×/30× claims;
PRs #5208 / #2469 / #2133 / #5020; the Klaftenegger–Sagonas–Winblad 2013 paper;
DE-Bench and SD-Erlang figures; Stritzinger's 2017 slides; *Erlang in Anger*
containing no mention of `+zdbbl`; OTP 22's 50 s → 0.4 s figure.

### 🚫 Struck — re-opened and re-researched 2026-08-14

The original strike read: *"All WhatsApp material (Rick Reed's talks and every
figure and quote attached to them; Maxim Fedorov's talks and PR counts) and all
Discord material other than the `manifold` README. These were fabricated
wholesale."*

**That was right about the agent's report and wrong about the sources.** The
figures and quotes the agent attached to those talks were invented. The talks
themselves are real, public, and — now that the decks have been downloaded and
read first-hand — the most directly relevant prior art we have. See **18a**.

What remains struck, permanently: every number, quote and PR count that came out
of that agent's report. None of it is cited anywhere in this file. Everything in
18a was obtained by downloading the primary artefact and extracting its text.

### Process rule for Phases 5–6

**Every external citation in the article gets fetched and quoted from the
primary source before it ships.** No exceptions, no relaying of agent
summaries. The agents were valuable for *finding* material and for reading OTP
source; they are not a citation of record.

---

## Decisions taken (Phase 2 interview)

- **D1 — Angle. `AGREED`.** ERTS finding first; `ecall` presented as one working
  answer, not as the pitch. Lead with "one mutex per node pair kills distributed
  Erlang above ~30k concurrent senders", show the measurements, then the fix.
  Rationale: travels furthest on HN / Lobsters / erlang forums, and the library
  lands better when it is the conclusion rather than the premise.
- **D2 — Spine. `AGREED`.** The core of the article is the **distributed send**
  optimisation, and that is the only thing carrying measured claims. `cast` and
  `call` are presented as a *bonus*: the same funnel-and-batch transport
  extended to meet cast/call needs. The group operations (`cast_one`,
  `cast_all`, `call_one`, `call_any`, `call_all`, `call_all_wait`) are the
  further extension that makes the thing usable for real distributed-system
  tasks — a distributed database being the motivating example. See conclusion 15.
- **D3 — No further test runs. `AGREED`.** Publish on the existing `send` data.
  The gaps in conclusion 12 (single run per point, `beam.lcnt` build, two
  payloads, no cast/call data) are disclosed in a Limitations section rather
  than closed with machine time.
- **D4 — `+zdbbl` result is a short subsection. `AGREED`.** Conclusions 9 and 10
  get one compact section with the table, not a major act. It must be addressed
  head-on — it is the first thing a knowledgeable reader will propose — but it
  is not the spine.
- **D5 — Hardware confirmed. `AGREED`.** 2 × Xeon Gold 6342 (48 cores, HT off),
  251 GiB RAM, 10 GbE bond, Ubuntu 22.04 / kernel 5.15. Verified by direct
  inspection of the sender host; the receiver is stated to be identical.
- **D6 — Do not mention the loopback lcnt experiment. `AGREED`.** The
  250-senders / 16-proxies / 16-proxies-plus-batching decomposition in
  `findings.md` is a different experiment on a single machine. It stays out of
  the article entirely.
- **D8 — Memory: report, do not explain. `AGREED`.** State "37 GB native vs
  3 GB pooled" as a measured observation and say explicitly that we did not
  isolate where the memory lives. See conclusion 19.
- **D9 — Measure `-kernel dist_nodelay false`. `AGREED`.** One native run to
  close the "isn't this just Nagle?" objection with data rather than argument.
  This is a deliberate, scoped exception to D3. Run plan pending approval.
- **D10 — Nagle self-clocking is a supporting point, not the spine. `AGREED`.**
  The `dist_entry_out_queue` measurement stays the spine (D1). The `after 0`
  clocking argument explains *why the fix costs nothing at low load* and why it
  differs from 17 years of timer-based batchers — it earns a section, not the
  title.
- **D7 — State the knee in concurrent senders. `AGREED`.** "20–30k concurrent
  senders", not "~200k messages/s".
- **D11 — The ETS anecdote is background only, not article material.
  `AGREED`.** Roman offered it as context for me, the OTP version is not
  recallable, and its stated mechanism was refuted (see 16a). **It does not go
  in the article.** What survives is the *sourced* general pattern in 16a —
  three shared locks, one fix shape — which stands on OTP's own published
  benchmarks and needs no anecdote.
- **D12 — Do not mention the slower-machine observation. `AGREED`.** No citable
  numbers exist, so the cross-hardware claim is withdrawn entirely. Consequence:
  **"the ceiling is a software constant, not a hardware one" drops from a claim
  to a hypothesis** and must be labelled as such — see the revision note in
  conclusion 4.

---

## 0. What was actually measured (the factual base)

**Topology.** Two physical hosts, `rt-server1.fp` (sender) and `rt-server2.fp`
(receiver), each running one Docker container with host networking, OTP 27
(`beam.lcnt`, emulator 15.2.2). A third node — the Common Test controller —
orchestrates but carries no workload.

**Hardware** (verified 2026-08-13 on the sender; the receiver is identical):

| | |
|---|---|
| CPU | 2 × Intel Xeon Gold 6342 @ 2.80 GHz — 24 cores each, HT off, **48 logical processors** |
| Cache | 60 MiB L2, 72 MiB L3 (2 instances), 2 NUMA nodes (0–23 / 24–47) |
| RAM | **251 GiB** |
| Link | `bond1`, **10 GbE** (2 × 10G members), direct 10.230.0.0/24 between the two hosts |
| OS | Ubuntu 22.04.5 LTS, kernel 5.15.0 |

Two things follow. The `ecall` proxy pool is **64 on a 48-core box** — slightly
over-provisioned relative to schedulers, and not tuned. And 251 GiB of RAM is
the only reason the native path at 120k writers (37.4 GB peak) completed at all
rather than being OOM-killed.

**Workload.** `performance_send_SUITE` only. N writer processes on the sender,
N matching receiver processes on the receiver, one writer per receiver. Every
writer sends exactly 1000 messages, sleeping 100 ms between them. So the
*intended* offered load is `N × 10` messages/s and the *ideal* completion time
is 100 s regardless of N. N sweeps 10k → 120k in steps of 10k. Two payloads:
`tiny` (the atom `tiny`) and `data` (a 3×10-field nested map, 225 bytes on the
wire).

**The two paths compared.**
- `native`: `ReceiverPid ! Payload` — a plain distributed send.
- `ecall`: `ecall:send(ReceiverPid, Payload)` — routed through a local pool of
  64 forwarder processes that batch and forward to 64 remote workers, which
  then do the local `To ! Payload`.

**Headline metric.** `performance_percent` = achieved per-writer rate ÷ intended
per-writer rate (10 ops/s). 100 % means the pace was sustained; 25 % means the
same fixed 120 M messages were delivered, but it took four times as long.
Nothing is dropped — this is a goodput/latency collapse, not message loss.

**Two configurations.** Both use pool size 64 and batch size 1000; they differ
only in `+zdbbl` (the distribution buffer busy limit):
- `BL1024` — 1024 KiB, i.e. the OTP default.
- `BLMAX` — 2097151 KiB, i.e. effectively disabled.

Status: `PROPOSED`

---

## 1. Erlang's remote `!` does not scale with the number of senders — it scales *down*

At 120,000 writers with the 225-byte `data` payload and the default busy limit,
the native path sustained **16.4 %** of the intended pace; `ecall` sustained
**96.0 %**. The identical 120 M-message workload took **610 s** natively and
**104 s** through the pool — a **5.9×** difference in completion time.

The whole `native_test` case (both payloads, 12 writer counts) took **2 h 07 m**;
`ecall_test`, the same matrix, took **51 m**.

Full picture, `BL1024`, `data` payload:

| writers | native % | ecall % | native s | ecall s |
|--------:|---------:|--------:|---------:|--------:|
|  10,000 |     99.1 |    98.9 |    100.9 |   101.1 |
|  20,000 |     99.1 |    98.0 |    100.9 |   102.0 |
|  30,000 |     79.7 |    99.0 |    125.4 |   101.1 |
|  40,000 |     60.7 |    97.6 |    164.7 |   102.5 |
|  50,000 |     48.8 |    96.0 |    205.0 |   104.2 |
|  60,000 |     34.3 |    96.1 |    291.8 |   104.1 |
|  80,000 |     25.1 |    93.3 |    399.1 |   107.2 |
| 100,000 |     19.9 |    92.2 |    501.6 |   108.5 |
| 120,000 |     16.4 |    96.0 |    610.1 |   104.2 |

Status: `PROPOSED`

---

## 2. There is a knee, and it is sharp

The native path is at ~99 % up to a threshold and then falls off a cliff:

| config | payload | last good point | first bad point |
|---|---|---|---|
| `BL1024` | `data` | 20k (99.1 %) | 30k (79.7 %) |
| `BL1024` | `tiny` | 30k (99.1 %) | 40k (89.5 %) |
| `BLMAX`  | `data` | 20k (99.1 %) | 30k (79.1 %) |
| `BLMAX`  | `tiny` | 50k (94.1 %) | 60k (80.2 %) |

Below the knee the two paths are indistinguishable — `ecall` costs nothing and
gains nothing. Above it, they diverge without bound. **This is the single most
important shape in the data**, and it is the reason the problem is invisible in
small tests and staging environments.

Status: `PROPOSED`

---

## 3. The bottleneck is not the network, and this run proves it

Both paths write to **the same single TCP socket** — one OTP distribution
channel between the same node pair. Measured wire throughput on that socket
(`data` payload, `BL1024`):

- native plateaus at **~50 MB/s** (peak 54.9 MB/s at 50k writers, then
  *declines* to 44.3 MB/s at 120k — the classic congestion-collapse signature).
- `ecall` reaches **196.7 MB/s** on the same socket and is still climbing at
  120k writers.

A link that carries 197 MB/s cannot be the thing limiting the path that stalls
at 50 MB/s. And neither figure is anywhere near the wire: the hosts are joined
by a **10 GbE bond ≈ 1250 MB/s**, so native's ceiling is **4 % of the link** and
`ecall`'s best point is **16 %**. Nobody was waiting on the network.

Neither can the receiver be the constraint: the `ecall` path does strictly
*more* receiver-side work (decode a batch, then perform a local send per
element), and it still wins by 4×. The constraint is on the sender, in ERTS.

Status: `PROPOSED`

---

## 4. The constraint is `dist_entry_out_queue` — one mutex per node pair

### 4a. What a mutex actually costs — measured, not assumed

An agent verified this against glibc 2.39 / kernel 6.8 source **and benchmarked
it directly on this machine** (i5-12500H, 12C/16T, 1 NUMA node). Results
matter because they settle which half of the intuition survives.

**Confirmed — uncontended is nearly free, contended is not:**

| operation | cost | syscalls |
|---|---:|---:|
| uncontended `pthread_mutex_lock` + `unlock` | **12.80 ns** | 0 |
| `getpid()` (cheapest syscall, baseline) | 73.08 ns | 1 |
| `FUTEX_WAKE`, no waiters | 92.31 ns | 1 |
| `FUTEX_WAIT` returning `EAGAIN` | 125.73 ns | 1 |
| full blocking handoff (measured delta) | **≈ 700 ns** | 2 + 2 ctx switches |

The uncontended path is a single `lock cmpxchg`
(`sysdeps/nptl/lowlevellock.h:93-105`). One syscall costs **6–10× the entire
uncontended lock+unlock pair**. So "cheap when free, much heavier when held" is
correct, and the ratio is roughly **55×**.

**Confirmed — there is a genuine step, and it explains a sharp onset.**
`nptl/lowlevellock.c:30` sets the lock word to **2**, not 1, and
`lowlevellock.h:150` wakes whenever the old value is `> 1`. Therefore *once any
thread has ever blocked, every subsequent unlock pays a `FUTEX_WAKE` syscall* —
even with no waiter left — until an uncontended acquisition resets the state.
Below the blocking threshold: **zero** syscalls per operation. Above it: up to
**two**. That is a step function in the lock's service rate, not a ramp.
Drepper measured an **8–10× application speedup** on a 4-CPU machine from
removing exactly one such unconditional syscall per unlock.

**Refuted — the per-operation cost does NOT explode with contenders.**
Measured, one mutex, tiny critical section:

```
threads    16     32     64    128    256    512
ns/op   139.8  143.8  135.3  137.4  123.8  143.7      <- dead flat
```

**What explodes is the wait-time tail:**

| threads | throughput | mean wait | p50 | p99 |
|---:|---:|---:|---:|---:|
| 1 | 2.71 M/s | 19 ns | 19 ns | 28 ns |
| 16 | 1.35 M/s | 7.99 µs | 3.76 µs | 49.4 µs |
| 128 | 1.17 M/s | 98.5 µs | **3.09 µs** | **938 µs** |

Across a 128× increase in threads, throughput falls only **2.3×**, mean wait
rises **5,200×**, p99 rises **33,000×** — and **p50 stays flat at ~3 µs**. The
median thread is fine while the tail collapses. That is why it looks like a
sudden explosion in production monitoring.

**The correct mechanism, which is exactly the C4 framing:** a mutex turns the
critical section into a **single-server queue with a fixed service rate μ**.
Adding contenders does not raise the cost per operation; it raises queue
occupancy. As offered load λ approaches μ, waiting time behaves as
**W ~ 1/(μ − λ)** — hyperbolic. The non-linearity is queueing theory, not a
property of the mutex.

> **Scope limit (D12).** An earlier draft claimed "the ceiling is a software
> constant, not a hardware one — a bigger machine does not move it", resting on
> a recollection that less performant machines kneed at the same sender count.
> That observation has no citable numbers and is **withdrawn**. What remains
> defensible is narrower and mechanism-based: the per-signal serial cost is
> dominated by **syscall and cache-coherence traffic rather than computation**
> — 12.80 ns uncontended versus ~700 ns for a contended handoff, and aggregate
> throughput on a single shared cache line *falling* 12× from 1 to 32 threads
> while private lines scale 9.6×. Whether faster hardware moves the ceiling is
> **an untested hypothesis** and must be written as one, or omitted.

**Also confirmed, and directly relevant:** on Linux, `erts_mtx_t` — the type of
`dist_entry_out_queue` — **is** a plain glibc `PTHREAD_MUTEX_TIMED_NP`.
`erts/include/internal/ethread.h:145` reads
`#if !defined(ETHR_FORCE_PTHREAD_MUTEX) && 0`; the `&& 0` has made ERTS's own
mutex implementation dead code on every pthreads platform since at least
OTP 17, and still does in master. `ethr_mutex.c:1271` initialises with a NULL
attribute, and `ethr_mutex_init_opt` silently discards spincount options. So
**qlock does not spin, is not fair, and its waiters go straight to a futex.**
(Note the contrast: `erts_rwmtx_t` — which is what an ETS table lock is — *does*
use ERTS's own spinning implementation. Do not conflate the two.)

**One mechanism refuted that we should not have used:** "thundering herd" does
not apply to mutexes. glibc wakes exactly one waiter (`lowlevellock.c:57`,
`nr=1`) and the kernel honours it. It applies to `pthread_cond_broadcast`,
which ERTS does not use in hot paths. **Our suspend/resume herd is a different
thing** — an *Erlang-process*-level herd in `dist.c`, not a futex herd — and
remains valid. The article must not blur them.

### 4b. The measured evidence in our own runs

Every `Pid ! Msg` to a remote node takes the same per-`DistEntry` `qlock` to
append to the same output queue (`findings.md`, `dist.c:3510–3547`). Lock
counters from the sender (`data`, `BL1024`):

| writers | native lock wait | ecall lock wait | ratio | native coll % | ecall coll % |
|--------:|-----------------:|----------------:|------:|--------------:|-------------:|
|  20,000 |            223 s |           209 s |   1.1 |         94.7  |        90.2  |
|  40,000 |          4,201 s |           236 s |  17.8 |         99.6  |        87.7  |
|  80,000 |         10,916 s |           143 s |  76.1 |         99.9  |        81.7  |
| 120,000 |         16,874 s |            98 s | 171.8 |         99.9  |        75.6  |

Two things happen simultaneously as load grows, and they run in opposite
directions:
- native collision rate climbs to **99.9 %** — essentially every acquisition
  contends — and cumulative wait grows superlinearly;
- `ecall`'s cumulative wait *falls* as load rises (from 236 s at 40k to 98 s at
  120k) because bigger batches mean fewer acquisitions.

A scheduler thread blocked in `pthread_mutex_lock` cannot run any other Erlang
process. That is how a lock in the distribution layer turns into a whole-node
slowdown.

Status: `PROPOSED`

---

## 5. The fix is not a new transport — it is a funnel

`ecall`'s send path is about thirty lines
([ecall_connection.erl:210-227](../src/ecall_connection.erl#L210-L227)):

```erlang
worker_loop( Remote, BatchSize )->
  erlang:garbage_collect(self()),
  Requests = collect_requests( _Count = 0, BatchSize ),
  catch Remote ! {batch, Requests},
  worker_loop( Remote, BatchSize ).

collect_requests( Count, BatchSize ) when 0 < Count, Count < BatchSize->
  receive {do, Request}-> [Request| collect_requests( Count + 1, BatchSize)]
  after 0 -> []                       % nothing waiting: ship what we have
  end;
collect_requests( _Count = 0, BatchSize )->
  receive {do, Request}-> [Request| collect_requests( 1, BatchSize)]
  end;                                % nothing at all: block, don't spin
collect_requests( _Count, _BatchSize )-> [].
```

Callers are sharded onto proxies deterministically by
`erlang:phash2(self(), PoolSize)`. Two properties follow, and both matter:
1. the number of Erlang processes that ever touch the shared distribution queue
   drops from N (120,000) to 64, permanently;
2. per-caller message ordering to a given node is preserved — a given sender
   always maps to the same proxy, the proxy emits batches in order, and the
   remote worker replays each batch in order. Erlang only ever guaranteed
   pairwise sender→receiver ordering, and that guarantee survives.

Status: `PROPOSED`

---

## 6. The batching is self-tuning — that is why it costs nothing at low load

`collect_requests/2` uses `after 0`. It never waits for a batch to fill. It
takes whatever is *already* in the mailbox and ships it. Therefore the batch
size **is** the queue depth, which **is** the current overload factor:

Average distribution packet size, `data` payload, `BL1024`:

| writers | native | ecall |
|--------:|-------:|------:|
|  10,000 |  225 B | 436 B |
|  30,000 |  225 B | 446 B |
|  50,000 |  225 B | 866 B |
|  70,000 |  225 B | 1318 B |
|  90,000 |  225 B | 1792 B |
| 120,000 |  225 B | 2689 B |

Native is a flat line at exactly one message per signal, by construction.
`ecall`'s packet size grows roughly linearly with offered load *while
throughput stays at ~96 %*. No timer, no configured flush interval, no latency
floor: at low load batches are ~1–2 messages and the added latency is one
process hop. This is Nagle's insight applied inside the BEAM.

The nominal `batch_size` of 1000 is only a ceiling; it was never the operating
point in these runs (max observed ~16 messages/packet).

Status: `PROPOSED`

---

## 7. Batching also reduces total bytes on the wire — by ~25 %

Counter-intuitively, wrapping each message in a `{send, Pid, Msg}` tuple *and*
adding a routing hop still sends **less** data:

| payload | native total | ecall total | per message |
|---|---:|---:|---|
| `data` | 27.0 GB | 20.5 GB | 225 B → 171 B |
| `tiny` |  5.8 GB |  3.7 GB |  48 B → 31 B |

The per-signal distribution header and control message are paid once per batch
instead of once per message, and the atom cache amortises across the whole
batch. So batching buys fewer socket writes *and* fewer bytes.

Status: `PROPOSED`

---

## 8. Memory: the native path buys its slowness with tens of gigabytes

Peak `erlang:memory(total)` on the sender, `data` payload, `BL1024`:

| writers | native | ecall | ratio |
|--------:|-------:|------:|------:|
|  20,000 |  2.6 GB | 2.3 GB |  1.1 |
|  40,000 | 12.5 GB | 2.5 GB |  5.1 |
|  80,000 | 25.2 GB | 2.8 GB |  9.1 |
| 120,000 | 37.4 GB | 3.1 GB | 12.3 |

There is a **~2.24 GB floor** in every measurement, from the harness's
`+P 134217727` process table. Subtracting it, the working-set ratio at 120k is
35.2 GB vs 0.81 GB — **43×**. The `ecall` curve is essentially flat: memory
grows with the number of writer processes, not with the backlog.

Status: `PROPOSED`

---

## 9. Raising `+zdbbl` is the standard advice, and it is a partial fix at best

Comparing `BLMAX` (limit effectively removed) with `BL1024` (OTP default) on
the **native** path:

- it moves the knee for `tiny` from ~30k to ~55k writers — real, but one
  configuration step, not a solution;
- it does not move the knee for `data` at all (both break at 30k);
- at 120k writers the outcome is the same: 42.5 % → 43.6 % (`tiny`),
  16.4 % → 20.5 % (`data`);
- it does cut cumulative lock wait ~4× (16,874 s → 3,926 s for `data` at 120k),
  which is consistent with the suspend/resume herd being a large part of the
  contention, not the whole of it.

Status: `PROPOSED`

---

## 10. …and for the pooled path, raising `+zdbbl` is actively harmful

This is the result I did not expect. On the **`ecall`** path, keeping the
default 1 MiB limit is strictly better than removing it:

| metric at 120k writers, `data` | `BL1024` (default) | `BLMAX` (disabled) |
|---|---:|---:|
| performance | 96.0 % | 95.4 % |
| peak memory | **3.05 GB** | 13.75 GB |
| worst point across the whole sweep | 92.2 % | 83.8 % |
| worst point, `tiny` | 91.7 % | 79.3 % |

With 64 writers instead of 120,000, OTP's distribution flow control finally
behaves the way it was designed to: the busy limit suspends a handful of
proxies, the backlog turns into *larger batches* rather than into heap, and
memory stays flat. With the limit removed, nothing pushes back, the output
queue grows, and memory rises 4.5× for no throughput gain.

### 10a. Why — Roman's mechanism, and it is confirmed by the data

My original explanation ("flow control works when few things are
back-pressured") was vague. Roman's is precise and it is the right one:

> When the limit is low, the **proxies** get suspended waiting for the queue.
> While a proxy is suspended, its writers keep sending — so when it resumes it
> finds a large backlog in its mailbox and ships it as one big batch. A high
> limit means proxies never wait, so their mailboxes are near-empty and the
> batches are small.

**The busy limit is the accumulation window.** Not a timer, not a threshold —
backpressure itself is what creates the queueing that `after 0` then harvests.

Measured, `ecall` average distribution packet size, `BL1024` ÷ `BLMAX`:

| writers | `data` BL1024 | `data` BLMAX | ratio | perf BL1024 | perf BLMAX |
|---:|---:|---:|---:|---:|---:|
| 30,000 | 446 B | 425 B | 1.05 | 99.0 % | 92.9 % |
| 40,000 | 659 B | 570 B | 1.16 | 97.6 % | 95.5 % |
| **50,000** | **866 B** | **626 B** | **1.38** | **96.0 %** | **83.8 %** |
| 60,000 | 1083 B | 838 B | 1.29 | 96.1 % | 91.8 % |
| 70,000 | 1318 B | 988 B | 1.33 | 95.7 % | 89.7 % |

and for `tiny`, consistently from 70k up: 1.10, 1.17, 1.18, 1.16, 1.10, 1.16.

A clean dose–response: **where the low limit produces bigger batches, it
produces better throughput; where batch sizes converge, throughput converges.**
At 50k writers the low limit buys 38 % bigger batches and 12 points of
throughput.

### 10b. This completes the Nagle analogy — precisely, not loosely

Conclusion 6 said the batching is self-tuning because `after 0` harvests
whatever is queued. 10a supplies the missing half: **what makes things queue at
exactly the right moments.**

| | Nagle (TCP) | `ecall` |
|---|---|---|
| coalesces while… | an ACK is outstanding | the proxy is suspended on the busy limit |
| sends immediately when… | nothing is in flight | the distribution queue accepts |
| clock source | the network's own ACKs | ERTS distribution backpressure |

Nagle is *self-clocked by ACKs*. `ecall` is *self-clocked by the busy limit*.
Neither has a tuning parameter for the batch size, because in both cases the
batch size **is** the backlog, and the backlog is set by the downstream's real
capacity. That is why a fixed timer — every prior Erlang batching layer,
including `batched_communication`'s 100 ms — is a weaker design: it guesses the
window instead of measuring it.

**And it inverts the standard advice.** `+zdbbl` is normally raised to escape
backpressure. Here the backpressure is the mechanism: raise the limit and you
remove the clock, the batches shrink, and both throughput and memory get worse.

**The conclusion to draw is not "flow control is broken."** It is:
*flow control is the clock. Fix the writer count first; then leave `+zdbbl`
alone.*

Status: `PROPOSED — mechanism supplied by Roman, confirmed against the data`

---

## 11. What this costs you (the honest trade-offs)

Not free, and the article must say so:

1. **Backpressure moves, and weakens.** A native `!` suspends the calling
   process when the distribution buffer is busy. `ecall:send/2` returns as soon
   as the message is in a local proxy mailbox. The proxies are still
   flow-controlled, but the caller no longer is. Under sustained overload with
   no application-level admission control, proxy mailboxes grow without bound.
   `findings.md` states this explicitly for casts.
2. **Sender identity changes.** The receiver sees the message as coming from a
   remote pool worker, not from the original process. Anything relying on the
   implicit sender — links, exit signal propagation, `{From, Msg}` conventions
   built on the transport rather than on the payload — behaves differently.
3. **Delivery-failure semantics change.** `catch Remote ! {batch, Requests}` and
   `catch To ! Message`: a dead destination is swallowed silently, and a batch
   is all-or-nothing at the transport level.
4. **One extra hop each way.** Two additional scheduling events and one extra
   local copy per message. Irrelevant above the knee, measurable below it.
5. **`nosuspend` / `noconnect` send options are not available** through the
   pooled path.
6. **The pool is a fixed 64** (`?POOL_SIZE`), not adaptive, and it is the
   *receiver* that dictates the count — the local proxy count equals the remote
   worker count.

Status: `PROPOSED`

---

## 12. Measurement limitations the article must disclose

1. **`beam.lcnt` on both nodes.** The lock-counting emulator is slower than a
   production build. It is on both paths, so the *comparison* is fair, but the
   absolute ceilings (~50 MB/s native, ~197 MB/s pooled) are pessimistic.
2. **Only `send` was run to completion.** `perf_tests/spec.md` also defines
   `cast` and `call` suites; the retained good samples contain only
   `performance_send_SUITE`. Any claim about `erpc:cast`/`erpc:call` must be
   labelled as untested here.
3. **Only two payloads.** `tiny` and `data`. The 10 KiB / 100 KiB / 1 MiB binary
   profiles exist in the harness but were not part of these runs. Large
   ref-counted binaries have a very different distribution cost profile, and we
   should not extrapolate.
4. **One run per point, no repetitions, no error bars.** Non-monotonic native
   points (`tiny` `BL1024`: 50k → 72.9 %, 60k → 82.0 %; 100k → 35.3 %,
   110k → 45.5 %) show that run-to-run variance is material — of order ±10
   points in the collapsed region. Trends are solid; individual points are not.
5. **Sender-side metrics only.** Nothing was collected on the receiver.
6. **`elapsed_ms` for the send suite** is the mean, across receivers, of
   first-message → last-message; it is not the point wall clock.
7. **The pacing loop uses `timer:sleep/1`,** so a slow send stretches the cycle
   (spec.md describes a `start_timer` design that was not what shipped). This is
   what makes `performance_percent` a *self-throttling* measure: the offered
   load falls when the system can't keep up. Same on both paths.
8. **Load average is nearly useless here** — it is an EWMA far longer than one
   100 s point, and points run back-to-back with no cooldown, so each reading
   carries most of the preceding point. (`/proc/loadavg` not being namespaced
   turns out not to matter: the host was verified idle and runs only the one
   test container.) It is also confounded by BEAM scheduler busy-waiting.
   The mid-range divergence is suggestive — at 40k writers / `data`, native
   showed 37.8 against `ecall`'s 17.5 while delivering 60 % of the pace against
   98 % — but by 120k the two converge (15.1 vs 16.0) and the signal is gone.
   **Recommendation: drop it from the article.** Lock wait says the same thing
   with a clean mechanism behind it.
9. **The 2.24 GB memory floor** from `+P 134217727` must be stated whenever
   absolute memory figures are quoted.

Status: `PROPOSED`

---

## 13. Who this is for (the "so what")

The result is only interesting to someone who can recognise their own system in
it. The precondition is narrow and should be stated up front:

> Many thousands of Erlang processes on one node, all sending to the *same*
> remote node, at an aggregate rate above roughly 200k messages/s.

If that is you — distributed caches, pub/sub fan-out, replication, metrics
shipping, `esubscribe`/`elock`-style coordination layers — you are on a cliff
edge you probably cannot see, because everything is fine at 99 % until it
isn't. If it isn't you, the article is still useful as an explanation of what
the single distribution channel actually costs, and as a caution against the
"just raise `+zdbbl`" folklore.

Status: `PROPOSED`

---

## 14. The generalisable lesson, independent of `ecall`

Strip the library away and three transferable statements remain:

1. **Count your writers, not your messages.** The cost of a shared, mutex-
   protected queue is driven by the number of concurrent writers, not by the
   volume they write. 64 processes writing 1.2 M messages/s beat 120,000
   processes writing 200k messages/s on the same queue and the same socket.
2. **Opportunistic batching is free.** `receive ... after 0` — take what's
   already queued, never wait — adds no latency at low load and self-scales at
   high load. It needs no tuning parameter, and the batch size becomes a
   *free load metric* you can graph.
3. **Flow control is not free either.** Suspend/resume has a per-participant
   cost. A mechanism that protects you at 100 senders can be the thing that
   destroys you at 100,000. Before raising a limit to escape backpressure, check
   whether the real problem is how many things are being back-pressured.

Status: `PROPOSED`

---

## 15. One transport, then a usable API on top of it

The send result is the load-bearing measurement, but a message pipe is not what
anyone actually programs against. The same funnel-and-batch channel carries the
other two primitives at no extra transport cost, because they are just
different payload shapes inside the same batch
([ecall_receive.erl:79-95](../src/ecall_receive.erl#L79-L95)):

| element in the batch | remote worker does |
|---|---|
| `{send, To, Message}` | `To ! Message` |
| `{cast, M, F, As}` | `spawn(M, F, As)` |
| `{call, Ref, ClientPid, M, F, As}` | `spawn_monitor`, apply, reply through the *reverse* pooled channel |

Two things worth saying explicitly:

- `ecall:cast/4` replaces `erpc:cast/4`, which sends a full distributed *spawn
  request* per call — strictly more expensive than a message
  (`findings.md`, `erpc.erl:1231`). Batching removes the per-request signal;
  it does **not** remove the remote `spawn` per request, so the win should be
  expected to be smaller than for `send`. Untested here — see 12.2.
- `ecall:call/4` replies over the pooled channel in the other direction too
  (the reply goes through `ecall_connection:send/2`), so both legs are batched.
  The monitor-based failure path (`{'DOWN', Ref, Reason}`) is what makes it
  safe to lose the remote worker mid-call.

On top of that sits the layer that a distributed database actually needs — a
policy for *which* nodes and *how many* answers are enough:

| API | policy |
|---|---|
| `cast_one/4` | fire-and-forget at one node (prefers the local node) |
| `cast_all/4` | fire-and-forget at every node |
| `call_one/4,5` | try nodes one at a time until one answers `{ok, _}` |
| `call_any/4,5` | ask all nodes in parallel, take the first `{ok, _}` |
| `call_all/4,5` | require every node to answer `{ok, _}` |
| `call_all_wait/4` | collect both the successes and the rejects |

That is the quorum/replication vocabulary — write to all, read from any, fall
back on error — and it is the reason the transport work was done in the first
place. `ecall` is used by [`elock`](https://github.com/vzroman/elock) and
[`esubscribe`](https://github.com/vzroman/esubscribe).

**Article implication:** measured claims stay on `send`. `cast`/`call`/group
operations are described as design, with the transport win inherited and
explicitly *not* re-measured. Anything else invites a fair accusation of
overclaiming.

Status: `PROPOSED`

---

## 16. The same shape appears elsewhere in the BEAM — it is a lock-contention signature, not a distribution quirk

> **Provenance note (D11).** This conclusion began as a field anecdote from
> Roman: ETS write throughput collapsing at ~40,000 concurrent writers on a
> table with no `write_concurrency`, low CPU with high load average, and a
> debugger showing schedulers competing for one mutex. **The anecdote is
> background only and does not appear in the article** — its stated mechanism
> was refuted (16a), its "low CPU / high load average" signature was refuted by
> measurement (16b), and the OTP version is not recallable. What survives is
> the sourced pattern below, which rests entirely on OTP's own published
> benchmarks and source.

The generalisation, which is what the article uses —

> *Any single mutex in the BEAM that many Erlang processes must pass through
> has a contention threshold measured in tens of thousands of processes.
> Past that point the cost stops being additive. The fix is always the same
> shape: shard the thing behind the lock, or shrink the population in front
> of it.*

Three known instances, one fix pattern:

| shared lock | population that contends | the sharding fix |
|---|---|---|
| ETS table lock | writers to one table | `{write_concurrency, true}` — ERTS stripes the lock |
| process message-queue lock | senders to one process | a pool of receiver processes |
| `dist_entry_out_queue` | senders to one remote node | **no ERTS fix exists** — hence `ecall`'s proxy pool |

The third row is the point of the article: for the first two, the runtime (or
common practice) gives you a way to shard. For distribution, it does not — one
node pair, one channel, one queue, one lock — so the sharding has to happen in
*your* code, above the runtime.

### 16a. Source verification result — the mechanism in the anecdote is REFUTED, the observation stands

Two agents verified this against OTP 25.1.2 / 25.2.2 / 22.3 locally and OTP 27.0
/ 28.0 upstream. **The "many processes compete for the mutex" model is wrong,
and the article must not use it.**

**The cap.** ETS BIFs and local sends both run on ordinary scheduler threads
(`erl_db.c:782` asserts non-dirty; `ets:insert/2` is not a dirty BIF). Default
scheduler count = CPU count (`erl_init.c:894`). ERTS process locks queue **one
entry per thread**, not per process (`erl_process_lock.c:29-62`). Therefore:

> At most `#schedulers` OS threads can ever contend for an ETS table lock or a
> process message-queue lock — 48 on this hardware. Going from 48 concurrent
> writers to 40,000 adds **zero** additional lock waiters. The other 39,952
> processes queue in scheduler run queues.

**What the cliff actually is.** Ericsson's own published benchmark
(<https://erlang.org/bench/sigq_bench_result.html>, 64-HW-thread Azure box,
OTP 24, `off_heap` receiver, 1-word messages) measures single-receiver send
throughput as:

| senders | 1 | 2 | 4 | 8 | 15 | 16 | 32 | 64 |
|---|---|---|---|---|---|---|---|---|
| OTP 24, unbuffered | 2.2 M/s | 3.1 M/s | 3.1 M/s | 1.8 M/s | **151 K/s** | **52 K/s** | 72 K/s | 125 K/s |
| OTP 25 buffers | 2.1 M/s | 2.6 M/s | 6.4 M/s | 11.3 M/s | 17.1 M/s | 19.4 M/s | 26.9 M/s | 49.3 M/s |

The cliff is between **8 and 15 senders** — roughly the scheduler count, exactly
as the cap predicts — **not at 40,000**. Whatever produced the 40k ETS symptom,
lock-waiter count was already saturated three orders of magnitude earlier.

**So the honest reframing, which is also the better story:** the threshold is
not a *population* threshold, it is the point where `writers × per-writer rate`
crosses the lock's **service rate**. Sender population matters because it sets
demand, not because it adds contenders. This is exactly the same statement as
conclusion 4 for distribution, which is what makes the two cases one story.

**Corrections that must reach the article:**

1. `ets:insert/2` with a **list** of ≥ 2 tuples takes the table-global
   exclusive lock *even with `write_concurrency` enabled*
   (`erl_db.c:1764-1768`; `ets.xml:1298-1300`: such functions "gain less (or
   nothing)"). Batching ETS inserts defeats the option meant to fix this.
2. `{write_concurrency, auto}` is **OTP 25.0**+ (OTP-15991 / PR-5208). Date the
   anecdote.
3. Many **remote** senders do *not* contend on the receiver's MSGQ lock. There
   is exactly **one input handler per distribution connection**
   (`dist.c:4058-4088`, `dist.c:4231-4251`; port-based dist serialises through
   one port task). Receiver-side MSGQ contention scales with *connected nodes*,
   not remote senders. Row 2 of the table below is a **local**-send phenomenon
   only — conflating them would be a factual error.
4. "The whole VM stalls" overstates it. There is no global lock; other
   schedulers keep running and work-steal (`erl_process.c:4540`, `:9725`).
   The accurate statement: a scheduler blocked on the lock runs **no** Erlang
   process and stops reporting thread progress (`erl_thr_progress.c:52-56`),
   and contending schedulers burn CPU spinning (budget
   `min(2000, 1000 + 32×schedulers)`, `sched_yield()` every 25 iterations)
   before parking in a futex.

Revised table — three shared locks, and what actually shards each:

| shared lock | who contends | ERTS's own fix | available by default? |
|---|---|---|---|
| ETS table lock | schedulers running writers to one table | `{write_concurrency, true\|auto}` — 64 → 8192 stripes | opt-in, and **defeated by list inserts** |
| process MSGQ lock (local sends) | schedulers running senders to one process | OTP 25 signal-queue buffers — 64 per-sender-hash slots | **only with `{message_queue_data, off_heap}`; the default is `on_heap`** |
| `dist_entry_out_queue` | schedulers running senders to one remote node | **none exists** | — |

Row 3 is the article. For the first two the runtime gives you a sharding
mechanism; for distribution it does not, so the sharding has to happen above
the runtime, in your code.

### 16b. "Low CPU, high load average" must not be published as evidence of lock contention

Verified against Linux `kernel/sched/loadavg.c`, `include/linux/sched/loadavg.h`,
the CFS bandwidth-control docs, and a survey of every public BEAM load report
the agent could reach. **The signature does not mean what the anecdote assumes,
and using it would be the weakest link in the whole article.**

1. **Load average counts `nr_running + nr_uninterruptible`.** A thread blocked
   on a mutex is parked in a futex, which is `TASK_INTERRUPTIBLE` (S state).
   **Futex waiters do not contribute to load average at all.** So "high load
   average" is not evidence of lock contention — if anything it is evidence
   *against* threads being parked on a lock.
2. **The reverse claim, which circulates widely, is backwards.** BEAM scheduler
   busy-wait (`+sbwt`) makes threads `TASK_RUNNING` — it raises load average
   **and** CPU together. It cannot produce low CPU with high load. Every
   primary BEAM report the agent found (Rickard Green on R15B02, Paul Davis at
   Cloudant 2015, Stressgrid 2019, the RabbitMQ 3.8.5 thread 2020, Pleroma's
   tuning guide) describes **high CPU**, usually high *system* CPU. The
   accurate community statement, from Fred Hébert's *Erlang in Anger*, is the
   opposite one: busy-wait makes OS CPU% an **overestimate** of real BEAM work,
   which is why `erlang:statistics(scheduler_wall_time)` exists.
3. **No resolved public report exists** of BEAM producing high load average with
   genuinely low CPU. The closest is an undiagnosed 2014 erlang-questions
   thread ("load 10 on 8 cores", empty mailboxes, never resolved).
4. **What *can* actually produce it**, in order of likelihood for an ETS-heavy
   workload:
   - **`mmap_lock` contention.** Heavy allocator churn → `mmap`/`munmap` storm →
     one writer holds `mmap_lock` while every concurrently-faulting thread
     blocks in `rwsem_down_read_failed()`, which **is** `TASK_UNINTERRUPTIBLE`.
     Brendan Gregg measured exactly this contributing 0.23 load from page faults
     alone. A RabbitMQ user measured 8,406 `munmap` calls at 49 µs each and
     fixed it with the ERTS super carrier (`+MMscs`). Diagnostics: `sar -B`
     (`pgscand/s` should be zero), `allocstall` in `/proc/vmstat`.
   - **Transparent hugepages** with `defrag=always`, which puts the faulting
     thread into synchronous compaction.
   - **Dirty-IO schedulers / async threads in D state** (bounded: `+SDio`
     defaults to 10, so ~+10 load maximum).
   - **A container measurement artifact.** `/proc/loadavg` is not cgroup-aware:
     inside Docker/K8s it reports the *host's* load while CPU% is per-container.
     This manufactures the signature for free. Note `cpu_sup:avg1/0` reads the
     same file and inherits the same lie.
   - Counter-intuitively, cgroup CPU throttling **deflates** load average —
     throttled tasks are dequeued and are not counted.

**Consequences for the article:**

- **Drop the load-average observation from the anecdote entirely.** Keep the
  debugger evidence (most schedulers inside `pthread_mutex_lock`), which *is*
  a valid diagnostic for lock contention and is independently supported.
- This retroactively confirms conclusion 12.8: **remove the load-average metric
  from the article's charts.** On a BEAM box it is a busy-wait artifact and
  measures nothing we can defend.
- If the anecdote is kept at all, the honest form is: *"we saw ETS write
  throughput collapse and found schedulers blocked on the table lock under a
  debugger"* — with no load-average claim attached.

Status: `REVISED — mechanism corrected, awaiting Roman's sign-off`

---

## 17. `ecall` quietly depends on an OTP 25 optimisation, and that is worth saying

This fell out of the source verification and neither of us had it written down.

In the 120k-writer point, the writers do not send to 64 proxies "for free" —
that is itself a **many-to-one local send**, ~1,875 writers per proxy, which is
precisely the contention pattern Ericsson's benchmark shows collapsing past
~8–15 concurrent senders. The funnel should have moved the bottleneck rather
than removed it.

It does not, because both pools are spawned `off_heap`:

```erlang
spawn_opt(fun()->worker_loop(W, BatchSize) end,
          [link, {message_queue_data, off_heap}])
```
[ecall_connection.erl:167-168](../src/ecall_connection.erl#L167-L168) and
[ecall_receive.erl:34-35](../src/ecall_receive.erl#L34-L35).

`{message_queue_data, off_heap}` is the **precondition** for OTP 25's
signal-queue buffers (`erts_proc_sig_queue_maybe_install_buffers` returns
immediately unless `ERTS_PSFLG_OFF_HEAP_MSGQ` is set). Once a proxy has taken
more than 50 net contended MSGQ acquisitions, ERTS shards its outer queue into
**64 cache-line-padded buffers**, senders hashed by pid
(`erl_proc_sig_queue.c:8688-8693`). So the real fan-in structure is
64 proxies × 64 buffer slots ≈ **4,096 independent enqueue points**, not 64.

The full pipeline, with the contention structure of each stage:

| stage | shape | contention |
|---|---|---|
| 120,000 writers → 64 proxies | many-to-one, local | sharded 64× by OTP 25 buffers (requires `off_heap`) |
| 64 proxies → `dist_entry_out_queue` | many-to-one | the one unavoidable serial point — but 64 processes, and batched |
| 1 socket → 1 input handler → 64 workers | one-to-many | none; one batch signal per worker |
| 64 workers → 120,000 receivers | one-to-many, local | none; distinct receivers |

Two consequences worth publishing:

- The default is `on_heap` (`erl_process.c:133`), and the
  `process_flag(message_queue_data, …)` documentation **still** does not mention
  the optimisation even in OTP 27 — it only discusses GC cost, and says message
  passing "is generally better when the flag value is `on_heap`". Anyone
  building a fan-in pool without `off_heap` inherits the collapse they were
  trying to escape. That is a genuinely useful, non-obvious thing to tell
  readers.
- These results are therefore **OTP ≥ 25 results**. On OTP 24 the funnel would
  be expected to perform materially worse. We have not tested that; say so.

Status: `PROPOSED`

---

## 18. Prior art — what is genuinely new here, and what is not

Researched against primary sources (repo code, not summaries). **The article
must not claim novelty it does not have**, and the honest position is stronger
than an overclaim anyway.

### Already published, 17 years ago

**Roberto Ostinelli, "Boost message passing between Erlang nodes" (2009-04-07)**
— <https://www.ostinelli.net/boost-message-passing-between-erlang-nodes/>.
Measured 5.3M msg/min local vs **700K msg/min remote**, added a queue/router
process that accumulates and flushes on count or a 200 ms timer, and got
**700K → 2.1M msg/min (3×)**. This is the direct ancestor of everything below
and it must be cited. Our contribution is not the idea.

### The three ingredients, each already shipped separately

| ingredient | prior art | difference from `ecall` |
|---|---|---|
| `receive … after 0` opportunistic drain → one combined write | **VerneMQ `vmq_cluster_node`**; **RabbitMQ `gen_batch_server`** | VerneMQ: identical loop, but one process per node and its own TCP socket, not distribution. `gen_batch_server`: identical loop with adaptive sizing (32→8192, double on full / halve on empty), but on the *receiving* side |
| pool of forwarders per node + batching | **`batched_communication`** (skirino) — 32 senders / 32 receivers, hashed by node, gzip | uses a **100 ms timer**, not `after 0`; keyed by destination node so one node pair funnels through exactly one sender |
| pool of forwarders per node, no batching | **Discord `manifold`**; **`gen_rpc`**; **MongooseIM `mod_global_distrib`** | coalesce only *within one fan-out call*, or shard connections; two consecutive sends still produce two distribution signals |
| dedicated per-peer queue processes in front of distribution | **WhatsApp** — Reed, EFSF 2014, slide 17: "Separate inter-node queues", "Node-to-node message forwarding", "'Queuer' FIFO worker dispatch" | the closest published ancestor of `ecall`'s funnel, but it is **one slide of bullets**: no mechanism, no numbers, no public code. See 18a |
| more channels instead of fewer signals | **Partisan** (Meiklejohn et al., USENIX ATC '19) — claims up to **38× throughput** | the main design alternative; requires replacing the distribution layer. `ecall` keeps stock distribution and stock ordering. WhatsApp's `wandist` (2014, slide 28) is the same move for cross-cluster traffic |

### 18a. WhatsApp — the strike reversed, and what the decks actually contain

Both Rick Reed decks were downloaded and read in full (see the provenance table).
Nothing below is second-hand. Slide numbers are the deck's own. The extracted
slide text is kept in `perf_tests/sources/reed-efsf2012-slides.txt` and
`reed-efsf2014-slides.txt` so every quote below can be checked without a network
round-trip — the failure that produced this section's history was a lost source,
so the sources now live in the repo.

**Why this matters: WhatsApp built our fix, in production, in 2014.**

> **Decouple — Avoid head-of-line blocking**
> · Separate read & write queues
> · **Separate inter-node queues**
>   · Avoid blocking when single node has problem
>   · **Node-to-node message forwarding**
>   · mnesia async_dirty replication
> · **"Queuer" FIFO worker dispatch**
>
> — *That's Billion with a B*, slide 17

That is the funnel, named, on a slide, twelve years ago. The article **must not
claim the pattern is new** — conclusion 18's honest position gets more honest,
not less. What we still have that they did not publish is *why* it works
(`dep->qlock`), *how much* it is worth (the sweep), and the `after 0` clocking.
Their slide gives the shape with no mechanism and no numbers.

**Supporting material, all directly usable:**

| slide | quote | what it does for us |
|---|---|---|
| 2012 · 20 | "**Contention, contention, contention** · From 200k to 2M were all contention fixes · Some issues are internal to BEAM · Most required BEAM patches" | The thesis of our article, in someone else's words, from 2012. Best possible epigraph |
| 2012 · 10 | "**BEAM lock-counting (invaluable!!!)**" | Validates our method. The one tool that got a triple exclamation mark from the person who took Erlang to 2.8M connections is the tool our conclusion 4b rests on |
| 2012 · 16 | "571k pkts/sec, **>200k dist msgs/sec**" | Their measured per-node distribution rate — the same order as our knee. **Denominators differ** (theirs: one node against many peers; ours: many senders against one peer). Cite as scale context, never as agreement |
| 2012 · 29 | "Implement cross-node gen_server calls without using monitors (**reduces dist traffic and proc link lock contention**)" | Prior art for conclusion 15's `call` design, and independent confirmation that lock contention in the dist path is real and worth hand-optimising |
| 2012 · 28 | "Increase default dist receive buffer from 4k to 256k (and make configurable)" | Receive side, not `+zdbbl`. Do not conflate |
| 2012 · 23 | "Fix missing accounting for outbound dist bytes" | Even WhatsApp had to patch BEAM to *see* dist throughput. Supports the "this is invisible" framing of conclusion 2 |
| 2014 · 14 | "Use calls only when returning data, else cast · Make calls w/ timeouts only: no monitors · **Non-blocking casts (nosuspend) sometimes** · **Large distribution buffers**" | Two things at once: `nosuspend` as the real-world remedy (matches *Erlang in Anger*, novelty claim 2), and a major user raising dist buffers — with **no published numbers**, which is exactly the 13-year-old gap our sweep closes |
| 2014 · 15 | "Work distribution: start with gen_server · Spread work to multiple workers: **gen_factory** · Spread dispatch to multiple procs: **gen_industry**" | The "shrink the population in front of the lock" pattern, productised internally |
| 2014 · 28 | "Meta-clustering · Limit size of any single cluster · **wandist: dist-like transport over gen_tcp** · Transparent routing layer just above pg2 · All messages are single-hop" | The alternative answer — replace the transport (cf. Partisan). Useful contrast: `ecall` keeps stock distribution |
| 2014 · 37 | "Watch for process message queue backlog · **Generally strive to remove all back pressure** · Bottlenecks show as backlog" | A **direct tension with conclusion 10a**, and a good one. Their operational stance is remove backpressure; our finding is that the busy limit *is* the clock that makes batches big. Both true, different goals — worth a paragraph, not a fight |
| 2014 · 4, 8 | "342K peak msgs in/sec, 712K out"; per-node monitoring with a `dist msgin/msgout` column at ~231k/s across 408 nodes | Scale context. Their tooling counted dist messages per node as a first-class metric |

Also on record: their patched BEAM was public as **`reedr/otp`** on GitHub
(2014 · 35), and 2012 ran **OTP R14B03**, 2014 **R16B01 (+patches)** — worth one
sentence, because it dates every fix above to before OTP 17.

**Some of it reached upstream OTP, and one piece closes a loop on our own
method.** From `erts/doc/notes.md` (local OTP checkout, grepped directly):

| release | entry | note |
|---|---|---|
| **erts 5.9.2 (R15B02)** | OTP-10051 — *"Add port and suspend options to lock-counter profiling. **(Thanks to Rick Reed)**"* | This is 2012 · slide 23's bullet — "Add suspend, location, and port_locks options to `erts_debug:lock_counters`" — landing in OTP roughly six months after the talk |
| erts 6.0 (OTP 17) | OTP-11809 — *"Use `closefrom/2` when available in child_setup (Thanks to Rick Reed and Anthony Ramine)"* | minor, but confirms sustained upstream contribution |

The first row is worth a sentence in the article. **`lcnt` is our only instrument
for conclusion 4b, and part of it was built by the person who found WhatsApp's
contention with it** — the same tool his 2012 deck marked "invaluable!!!".

Precision, so this does not become the next overclaim: our harness calls
`lcnt:rt_mask([distribution])` / `lcnt:rt_collect()`
([performance_metrics.erl:139](../test/performance/util/performance_metrics.erl#L139)),
and the *runtime toggle* API is **OTP-13170 (OTP 20), credited to nobody
external** — not Reed's patch. What he upstreamed is the port/suspend
instrumentation; what his slide 23 also asked for ("Enable/disable process/port
lock counting at runtime") OTP implemented independently five years later.
Say "he extended the tool", not "he wrote the API we use".

**Maxim Fedorov.** Real, and modest in what it gives us. Verified: **80 PRs to
`erlang/otp`, 73 merged**; `pg` ([#2524](https://github.com/erlang/otp/pull/2524),
merged 2020-02-06) whose rationale is *"no cluster lock required, and no
dependency on global"* and scopes that *"decouple single mesh into a set of
overlay networks, reducing amount of traffic required to propagate group
membership information"*; and dist-adjacent PRs [#1569](https://github.com/erlang/otp/pull/1569)
(>2 GB dist message crash), [#2625](https://github.com/erlang/otp/pull/2625),
[#2654](https://github.com/erlang/otp/pull/2654) (concurrent TLS dist handshake).
His *Scaling Erlang cluster to 10,000 nodes* (Code Mesh LDN, 2018) exists —
**but its subject is cluster size, i.e. the N² connection mesh, not throughput
on one channel.** Different axis from ours. Use it, if at all, for one line: the
published WhatsApp scaling story is about *many nodes*; ours is about *many
senders to one node*. I did not obtain the talk's content first-hand (video
only, no transcript reachable) — **so do not quote it.**

### 18b. Discord — nothing further to add

Re-checked. The `manifold` README stays the citation. The 2017 blog post
([discord.com/blog](https://discord.com/blog/how-discord-scaled-elixir-to-5-000-000-concurrent-users))
adds two usable numbers — *"wall clock time of a single `send/2` call could
range from 30μs to 70us due to Erlang de-scheduling the calling process"* and
*"publishing an event from a large guild could take anywhere from 900ms to
2.1s!"* — but its mechanism is **fan-out cost inside one sending process**,
not contention on the shared dist queue, and `manifold`'s fix is per-call
grouping, which conclusion 18's table already states correctly. The 2019 Rust
post is about an immutable sorted-set data structure in a single process and is
**irrelevant** to this article; it must not be cited as a distribution result.

### What is actually novel — ranked, after a second independent search

1. **Naming and measuring `dist_entry_out_queue`.** Everyone who writes about
   the single channel means **TCP head-of-line blocking**. Partisan's paper and
   README mean that. Stritzinger's 2017 EUC talk means that. Lindberg's 2019
   Erlang Workshop paper means that; it became OTP 22 fragmentation.
   **Nobody in the public literature attributes the throughput ceiling to
   `dep->qlock`** — the per-`DistEntry` mutex every sender takes to append to
   `out_queue` — and no published `lcnt` profile of it could be found. Our
   lock-counter data is the contribution.
2. **Published evidence that `+zdbbl` does not buy throughput.** The *only*
   existing public evidence is one sentence from a RabbitMQ maintainer:
   Simon MacMullen, rabbitmq-discuss, 2013-05-29 — *"I haven't been able to
   demonstrate any improvement in throughput by tweaking zdbbl until [the
   `busy_dist_port` messages] went away."* Our sweep would close a 13-year-old
   open question with numbers. Supporting rhetorical point: **`+zdbbl` appears
   nowhere in *Erlang in Anger*** — Fred Hébert's remedy for `busy_dist_port`
   is `nosuspend`, not a bigger buffer.
   **Strengthened by 18a:** WhatsApp's 2014 deck lists "Large distribution
   buffers" and "Non-blocking casts (nosuspend) sometimes" side by side as
   standing practice — **with no numbers attached**. The largest published
   Erlang deployment of its era raised the buffer on faith too. That is the gap.
3. **Mailbox-empty clocking as the faithful Nagle analogue.** Every prior
   batching layer *over distribution* uses a timer: Ostinelli 200 ms,
   `batched_communication` 100 ms, Broadway 1000 ms, brod's linger (off by
   default). `gen_batch_server` and Ra's WAL use mailbox-empty clocking but
   neither is pooled nor distribution-facing. The observation that
   **Nagle is self-clocked (it sends immediately when nothing is in flight, and
   coalesces only while something is), so a mailbox-empty drain is the faithful
   analogue and a fixed timer is not** appears to be unmade in the Erlang
   literature. It is small, true, and quotable — a strong candidate for the
   spine of the article.
4. **The pattern, with a mechanism and benchmarks.** ~~No talk or post presents
   it as a technique.~~ **REVISED 2026-08-14 — WhatsApp does**, on slide 17 of
   the 2014 deck: "Separate inter-node queues · Node-to-node message
   forwarding · 'Queuer' FIFO worker dispatch" (18a). The surviving claim is
   narrower and must be written narrowly: the technique is named in one 2014
   slide bullet and implemented in library source, but **nobody has published
   why it works or what it is worth**. We supply the lock, the sweep, and the
   `after 0` clocking. Claim that, and cite Reed for the shape.

### ⚠️ The sharpest objection to our own story, and the answer

> *Funnelling senders in userland does **not** shard `qlock`. All 64 proxies
> still hit the same `DistEntry`. So how can the funnel be the fix?*

It is correct, and the article must confront it rather than hope nobody
notices. The answer is in our own data: **`ecall` does not reduce contention
per acquisition — it reduces the acquisition rate below the lock's contention
threshold.**

| `data`, 120k writers | native | ecall |
|---|---:|---:|
| qlock acquisitions (≈ distribution signals) | ~120 M | ~7.6 M |
| collision rate | 99.92 % | 75.58 % |
| cumulative wait | 16,874 s | 98 s |

16× fewer acquisitions takes the lock from "contended on essentially every
attempt" to "free three times out of four". So:

- **Batching** is what relieves `qlock` — by asking for it 16× less often.
- **The funnel** does something different: it bounds the **suspend/resume
  herd**, which is the one genuinely per-*process* cost in the path (see 16a).
  64 processes to suspend and resume instead of 120,000.

Two mechanisms, two problems — which is exactly the C4 framing, now independently
forced by the prior art rather than chosen for elegance.

### Two objections the prior art hands to a hostile reader

1. **"Isn't this just Nagle, and doesn't OTP already ship it?"**
   **✅ MEASURED AND CLOSED — see conclusion 21.** It does ship it
   (`inet_tcp_dist.erl:372-380`, verified at OTP-27.0), and turning it on
   changes nothing.
2. **"OTP 22 added distribution fragmentation."** True, and it removed the
   *large-message head-of-line blocking* argument (Larsson's OTP 22 Highlights:
   max latency for small RPCs alongside a 500 MB term fell from ~50 s to
   ~0.4 s). Our win is therefore about per-message overhead, not about big
   messages blocking small ones. **Do not make a head-of-line-blocking
   argument** — it is the argument everyone else already made, and OTP fixed it.
3. **"What about `async_dist`?"** `process_flag(async_dist, true)` — OTP 25.3,
   OTP-18374 / PR-6632 (Rickard Green) — makes distributed sends never block.
   The article **must** mention it or the omission reads as ignorance. The
   answer: it removes *suspension*, not `qlock`, not per-signal encode/allocate
   cost, and not the socket write count. It trades the collapse for unbounded
   memory growth — OTP's own docs warn *"you may get into a situation with
   excessive memory usage"* absent application flow control. That is precisely
   the failure mode conclusion 10 shows for `ecall` under `BLMAX`.
4. **"Isn't `off_heap` doing the work?"** Since OTP 25 the BEAM shards a hot
   receiver's message-queue lock for `off_heap` processes. We must state the
   `message_queue_data` setting explicitly or the result is uninterpretable.
   See conclusion 17 — it is doing real work in our pipeline, and we should own
   that rather than let a reviewer find it.

### Design details the prior art validates in `ecall`

- **Pinning callers to a fixed proxy** (`phash2(self(), Size)`) is what every
  pooled design does — `batched_communication` hashes on destination node,
  `manifold` on caller pid (with a loud warning about mixing modes), MongooseIM
  pins a client to a connection on first use. Pairwise ordering is preserved.
- **`after 0` needs a secondary eager trigger** so a sustained burst cannot
  build an unbounded batch before the mailbox drains. VerneMQ uses a 64 KiB
  byte threshold; `gen_batch_server` uses adaptive count. `ecall`'s
  `batch_size` cap (1000) is that trigger — though in these runs it was never
  reached (max observed ≈ 16 messages/batch), so it is untested as a safety
  valve.
- **`off_heap` on the pool processes** is what `manifold` also does. See
  conclusion 17.

Status: `PROPOSED`

---

## 19. ✅ ANSWERED — the memory is `binary`: ERTS distribution output buffers, and `+zdbbl` does not bound it

**Resolved 2026-08-13** by retaining the full `erlang:memory()` breakdown at
100 ms. The section below is kept for the record; the answer is here.

| point | peak `total` | peak **`binary`** | peak `processes` |
|---|---:|---:|---:|
| native `BL1024` `data` 120k | 34.26 GB | **31.61 GB** | 2.13 GB |
| native `BL1024` `tiny` 120k | 26.66 GB | **24.04 GB** | 2.10 GB |
| native `BLMAX` `data` 120k | 36.17 GB | **33.54 GB** | 2.03 GB |
| **ecall** `BL1024` `data` 120k | 3.06 GB | **0.05 GB** | 2.40 GB |

Three clean facts:

1. **It is all `binary`.** The `data` payload contains no binaries whatsoever —
   it is a map of atoms. So this is ERTS's own **distribution output buffers**,
   which are allocated from the binary allocator. Not user data, not message
   queues, not process heaps.
2. **`processes` never grows.** Flat at ~2.1 GB in every native configuration.
   (An earlier reading of "22 GB in processes" came from the 11-sample starved
   run and was an artifact — see conclusion 24.)
3. **`+zdbbl` does not bound it.** Peak `binary` reaches **31.6 GB against a
   configured busy limit of 1 MiB**, and 33.5 GB against a 2.15 GB limit
   (15.6×). Whatever `qsize` accounts for, it is *not* the memory the
   distribution path actually holds.

Fact 3 is a genuinely useful, non-obvious, actionable claim: **operators raise
`+zdbbl` believing it caps buffering, and it does not.** It complements
MacMullen's 2013 observation that raising it does not buy throughput either.

**The pooled path simply does not have this problem** — 0.05 GB of binary
against native's 31.6 GB, a 600× difference, because it never asks the
distribution path to hold 120 million individually-encoded signals.

**What we should still not claim:** the precise reason peak `binary` exceeds
the total wire bytes of the whole workload (27 GB). Encoded output buffers are
allocated against an upper-bound size estimate before the atom cache shrinks
the actual encoding, and binary-allocator carriers are released lazily — either
would explain it. We have not separated them. Report the measurement; do not
explain the excess.

---

## 19-old. (superseded) We cannot currently explain where the 37 GB goes

Working through the arithmetic while writing conclusion 18 exposed a hole in
conclusion 8.

The natural explanation for native's 37.4 GB peak is "the ERTS distribution
output queue backs up", which is what `spec.md` assumes ("The queue that does
grow, the ERTS distribution output queue, has no BIF that exposes its size, and
it is visible only as sender memory"). **That explanation does not survive
contact with the numbers:**

- In the `BL1024` run the busy limit is 1 MiB, so `qsize` is bounded near
  1 MiB. The output queue cannot be holding tens of gigabytes.
- The entire workload is 120M × 225 B ≈ **27 GB** of payload. A peak of
  **37.4 GB** exceeds the total bytes the point ever transmits, so it cannot be
  queued payload alone under any configuration.
- For `tiny`, the busy limit *changes* the answer in the wrong direction:
  28.3 GB with the 1 MiB limit versus 8.6 GB with the limit removed. If the
  memory were queued output, removing the limit should have made it worse, not
  3× better.

Remaining candidates, none verified: allocator carrier growth and fragmentation
under ~1.2M encode/free cycles per second across 48 scheduler-local allocator
instances (`erlang:memory(total)` reports *allocated* carriers, not used bytes);
per-process heap growth across 120k suspended writers; and `mmap`/`munmap`
churn (see 16b — the ERTS super carrier `+MMscs` exists precisely for this).

**What the article should do:** report the memory as a measured observation
("sender memory reaches 37 GB on the native path and stays at 3 GB on the
pooled path, a 12× difference") and state plainly that **we did not isolate
where that memory lives**. Do not assert it is the distribution output queue.

**What would close it:** one run with `erlang:memory()` broken down by category
(`processes`, `binary`, `ets`, `system`) plus `erlang:system_info({allocator, _})`
carrier stats, instead of just `total`. That is a small harness change, not a
new experiment design.

Status: `PROPOSED — requires Roman's decision (see open questions)`

---

## 20. Source verification: `findings.md` re-checked against OTP-27.0

The tree `findings.md` cited (`/home/roman/DISTR/ERLANG/otp_src_27.0`) no longer
exists. Every claim was re-verified upstream at tag **`OTP-27.0`**, with each
file's git blob SHA checked against the GitHub API. `dist.c` at OTP-27.0 is
7,053 lines, sha `59281208…`. **`master` differs by ~135 lines in exactly this
region** — every published URL must be pinned to `blob/OTP-27.0/`.

Most of `findings.md` held up (drift 0–1 lines). Corrections that matter:

### ❌ Correction 1 — the "no hysteresis" claim is wrong as written

`findings.md` says busy is set at `qsize >= limit` and cleared at
`qsize < limit` with no lower watermark. **True only for the port-based
carrier.** Pid-based distribution controllers — TLS distribution, and any
custom carrier using `dist_ctrl_get_data/1` — have an explicit **half-limit**
watermark (`dist.c:4575`: `if (qsize >= erts_dist_buf_busy_limit/2 || …)`).
Hysteresis *does* exist in ERTS, just not on the default TCP path. As written
the claim is refutable in one line by anyone running TLS distribution.
**Scope every statement to "the default port-based TCP carrier."**

### ❌ Correction 2 — the resume herd does **not** delay the socket write

I would have written this and been wrong. OTP's own comment at
`dist.c:4020-4021` reads *"Everything that was buffered when we started have now
been written to the port."* The O(N) resume runs **after** this batch's writes.
And `qlock` is released at `dist.c:4035` **before** `erts_resume_processes`, so
the resume does not block other senders on `qlock` either.

The defensible formulation, and the only one we may publish:

> The flow-control release is O(number of suspended senders), performed
> serially by the scheduler thread running the distribution port task **while
> holding that port's lock** (`erl_port_task.c:1704` → `:1861`, never released
> inside `erts_dist_command`), and therefore serialises with the **next** drain
> of the same channel.

Verified per resumed process (`erl_process.c:9290-9319`): process-table lookup +
STATUS lock, liveness validation, `resume_process`, run-queue enqueue, STATUS
unlock, `proclist_destroy`. **No yield, no budget check, no batching in the
loop.** Reductions are charged only afterwards, 5 per process (`dist.c:3795`),
so the loop can overrun the port task's budget by an unbounded amount.
**We measured none of this — do not attach a nanosecond figure to it.**

### ✅ The framing that makes the article fair and hard to attack

OTP addressed distribution lock contention **in 2009** and said so — erts 5.7.2
(R13B01), OTP-7774:

> "Previously distribution port locks were heavily contended, and all encoding
> and decoding for a specific distribution channel had to be done in sequence.
> **Lock contention due to the distribution is now negligible** … Erlang
> messages and signals sent over the distribution are as before encoded by the
> sending Erlang process, but now **without holding any distribution channel
> specific locks during the encoding**."

That is OTP's position, and it is what a reader will answer with. Our result is
**a refinement of that 2009 statement under a workload it was not designed
for** — the residual `qlock` hold (a pointer append) is not negligible at
120,000 concurrent senders. Framing it that way is both fairer and far harder to
attack than "we found a hidden flaw."

Three adjacent OTP acknowledgements we may cite — **but not as statements about
`qlock` contention or the resume herd, which is not acknowledged anywhere:**

- **The controller is the bottleneck.** Rickard Green, PR #2469: *"in order to
  reduce the amount of work needed to be done by the distribution controller
  (which is the bottleneck of a connection)…"*
- **Suspension costs throughput.** erts 5.8.2 (R14B01), OTP-8901: *"The runtime
  system is now less eager to suspend processes sending messages over the
  distribution. The default value of the distribution buffer busy limit has
  also been increased from 128 KB to 1 MB. **This in order to improve
  throughput.**"* This is why the default is 1 MiB today.
- **Head-of-line blocking.** Larsson, PR #2133 / OTP-13397 (OTP 22
  fragmentation).

**Negative result, and it is load-bearing for conclusion 18.1:** no ERTS source
comment in `dist.c`, `dist.h`, or `erl_node_tables.*` contains *contention*,
*bottleneck*, *scalab*, or *herd*; a GitHub search for `dist_entry_out_queue`
across `erlang/otp` issues and PRs returns **zero** results; and the
`async_dist` PR body offers no contention rationale. Nobody has written this up.

### Other citation fixes

| claim | `findings.md` | correct (OTP-27.0) |
|---|---|---|
| `erl_send()` | `bif.c:2109–2194` | `bif.c:2573-2585` (2109 is `ebif_bang_2`) |
| `erts_dist_command` | `dist.c:3844–4003` | `dist.c:3845-4130` |
| `erts_mtx_t` is a raw pthread mutex | `ethread.h:140–149` | `ethread.h:147-149` (140 is a VALGRIND block) |
| `busy_dist_port` monitor | `erlang.erl:6055–6063` | `erlang.erl:6059-6063` |
| low watermark | *missing* | `dist.c:4571-4587` |

### ✅ Confirmed unchanged, and a version caveat that matters

`erts_resume_processes` is **byte-identical** between OTP 25.2.2 and 27.0. The
whole mechanism is unchanged from 25 to 27. **Do not write "OTP 27 does X"** —
a reader on OTP 24 will conclude they are unaffected, and they are not.
`async_dist` arrived in **OTP 25.3** (OTP-18374 / PR-6632), not 26 or 27.

### ✅ No supported way to get a second channel per node pair

Enforced at four layers: the dist table is keyed on the node-name atom alone
(`erl_node_tables.c:135-146`); `net_kernel`'s `sys_dist` is a `set` keyed on
node (`net_kernel.erl:1014-1016`); the handshake actively rejects a duplicate
(`erl_dist_protocol.md:548-550`); and *"there need to be exactly one
distribution controller per connection"* (`alt_dist.md:132-137`). A custom
carrier **may** spread bytes over several sockets and relax ordering
(`alt_dist.md:601-613`) — which fixes TCP-level head-of-line blocking but
**still does not shard the output queue**. Note also: "one socket per node pair"
is a property of `inet_tcp_dist`, not an ERTS guarantee. Phrase it as *"one
distribution channel and one output queue per node pair; with the standard
carrier, one TCP socket."*

Status: `PROPOSED — citations ready for publication`

---

## 21. ✅ Measured: TCP Nagle on the distribution socket changes nothing

Run on 2026-08-13 on the same two hosts, both arms back-to-back in one session
so run-to-run variance cannot mask the effect. Native path only,
`+zdbbl 1024`, lcnt emulator, writers 20k/40k/80k/120k, payloads `tiny` and
`data`. Control = stock (`{nodelay, true}`, Nagle **off**);
treatment = `-kernel dist_nodelay false` (Nagle **on**).

| payload | writers | perf % control | perf % Nagle | Δ | avg packet ctl → Nagle | MB sent ctl → Nagle |
|---|---:|---:|---:|---:|---|---|
| tiny |  20,000 | 99.1 | 99.1 | +0.0 | 48.0 → 48.0 | 960 → 960 |
| tiny |  40,000 | 94.2 | 92.6 | −1.6 | 48.0 → 48.0 | 1920 → 1920 |
| tiny |  80,000 | 48.4 | 45.3 | −3.1 | 48.0 → 48.0 | 3840 → 3840 |
| tiny | 120,000 | 41.9 | 42.4 | +0.5 | 48.0 → 48.0 | 5760 → 5760 |
| data |  20,000 | 98.8 | 98.6 | −0.2 | 225.0 → 225.0 | 4500 → 4500 |
| data |  40,000 | 60.8 | 60.9 | +0.2 | 225.0 → 225.0 | 9000 → 9000 |
| data |  80,000 | 25.1 | 29.6 | +4.5 | 225.0 → 225.0 | 18000 → 18000 |
| data | 120,000 | 16.5 | 16.1 | −0.4 | 225.0 → 225.0 | 27000 → 27000 |

**Result.** Mean Δ ≈ 0; every deviation is inside the known ±10-point
run-to-run band. Lock wait did not improve either (mostly marginally worse).
Bytes sent are byte-identical between arms, confirming the workloads matched.

**The prediction held exactly:** `average_packet_bytes` did not move at all,
because `inet:getstat(send_cnt)` counts *driver-level* sends — one per
distribution signal — and Nagle coalesces below that, in the kernel. The metric
is structurally blind to Nagle.

**Why this is the decisive answer to the objection.** TCP Nagle can only merge
*bytes already handed to the socket*. It cannot reduce the number of
distribution signals, `qlock` acquisitions, per-signal encode/allocate cycles,
or receiver-side dispatches — and that is where the serial cost lives. Batching
at the Erlang level removes work; Nagle only repackages the output of that work.
**Same socket, same bytes, same result.**

### 🎁 Bonus: the August native run reproduces almost exactly

The control arm is an unplanned four-days-later replication of four baseline
points, and for the `data` payload it is nearly exact:

| point | Aug 9 | Aug 13 | Δ |
|---|---:|---:|---:|
| data 40k | 60.7 | 60.8 | +0.1 |
| data 80k | 25.1 | 25.1 | 0.0 |
| data 120k | 16.4 | 16.5 | +0.1 |
| tiny 120k | 42.5 | 41.9 | −0.6 |
| tiny 80k | 50.9 | 48.4 | −2.5 |
| tiny 40k | 89.5 | 94.2 | +4.7 |

Cumulative lock wait reproduces too — `data` 80k: 10,916 s → 10,921 s;
`data` 120k: 16,874 s → 16,589 s.

**This materially softens conclusion 12.4.** The `data` payload is highly
reproducible (±0.1 points at three writer counts, four days apart, on a
separate CT invocation); `tiny` is the noisy one (±5). **Peak memory is the
genuinely unstable metric** — `data` 120k read 37.4 GB in August and 47.3 GB
now — which independently reinforces D8: report memory as an observation, never
as a precise figure.

**Article implication:** lead the quantitative claims with the `data` payload,
where we now have replication. Present `tiny` as corroborating shape rather
than precise numbers.

Status: `PROPOSED — new measurement, supersedes the open objection in 18`

---

## 22. The saw-tooth — a visible signature of the suspend/resume herd (not yet captured)

Roman's field observation during the runs:

> Sender-node load during a heavy point looks like a **saw** for the native
> path at the maximum busy limit. Presumably the queue reaches its limit, all
> writers are paused until it can accept again, then all resume. At the 1024
> limit the behaviour is probably the same but much harder to distinguish.

**This is mechanically coherent, and the period should scale with the limit.**
Drain rate on the native path is ~45 MB/s, so a full-queue → empty cycle takes
roughly:

| busy limit | queue depth at BUSY | approximate cycle |
|---|---:|---:|
| `BLMAX` (2,097,151 KiB ≈ 2 GiB) | ~2 GiB | **~45 s** — plainly visible as a saw |
| `BL1024` (1 MiB) | ~1 MiB | **~23 ms** — far too fast to resolve; reads as steady load |

Same oscillation, three orders of magnitude apart in frequency. That explains
why it is only visible at `BLMAX`, and it is direct evidence for the O(N)
suspend/resume herd in conclusion 16a/20 — the queue is not draining smoothly,
it is filling and emptying in waves.

**It may also resolve conclusion 19.** If sender memory oscillates with the
same period, then `memory_max` is a *sample of an oscillating signal*, and
whether we catch a true peak at a 100 ms sampling interval is partly luck. That
would explain the one metric that refuses to reproduce: `data` 120k read
**37.4 GB** in August and **47.3 GB** on 2026-08-13, while performance and lock
wait reproduced to within 0.1 %.

**We cannot show any of this yet.** `performance_metrics` deliberately keeps
aggregates only — *"The collector keeps aggregates only. It does not retain or
log the complete sample series"* (`spec.md`). The saw exists only in Roman's
observation of live monitoring.

**Proposed capture — this would be the article's best figure.** Add an opt-in
trace mode that writes the existing 100 ms sample series (memory + load) to a
file instead of folding it into a max/mean, then run three points at
120k writers / `data`:

1. native @ `BLMAX` — expect a slow, large-amplitude saw
2. native @ `BL1024` — expect dense high-frequency oscillation
3. `ecall` @ `BL1024` — expect flat

Three traces on one time-axis chart turns the whole mechanism into a single
picture: *the native path does not run out of capacity, it oscillates in and
out of it; the pooled path does not.* Roughly 25 minutes of machine time
(120k `data` is ~610 s native, ~104 s pooled).

Cost: one small harness change (retain the series), no change to the measured
paths. Risk: low — it is instrumentation only.

### 22a. ✅ CAPTURED — the saw is real, and remarkably regular

Native @ `BLMAX`, `data`, 120k writers, 3,929 samples at 100 ms. Binary memory:

| peak | trough | amplitude | period |
|---:|---:|---:|---:|
| 7.83 GB | 2.41 GB | 5.42 GB | — |
| 9.80 GB | 4.54 GB | 5.26 GB | 34 s |
| 12.40 GB | 7.09 GB | 5.31 GB | 32 s |
| … 13 cycles … | | | |
| 31.65 GB | 26.30 GB | 5.35 GB | 33 s |

**Period 33 s ± 1 s. Amplitude 5.3 GB ± 0.1 GB.** Thirteen clean cycles riding
a linear ramp from 0 to 33.5 GB. Roman's field observation confirmed with
precision.

**Roman's refinement (2026-08-13):** *"the saw is mainly not about memory but
about load average and CPU consumption (the writers wait)."* The memory
sawtooth is the visible proxy; the thing that actually oscillates is the
**writer population's ability to run**. Being captured now with `cpu_util`,
`sched_util`, `run_queue` and `load` at 100 ms alongside memory.

**Also confirmed: the socket is not the constraint.** `send_pend` on the native
path sits at **8,415 bytes max, 7,918 mean** — pinned at the `inet` driver's
8 KB busy watermark exactly as `spec.md` predicted, while the pooled path
reaches 129 KB because it writes bigger blocks. The port queue is clamped; the
backlog is upstream of it, in the distribution output queue.

Status: `PARTIALLY CAPTURED — memory saw confirmed; CPU/load capture in flight`

---

## 23. Pool sizing — 24 beats 64, and the pool has its own knee

Measured 2026-08-13, `BL1024`, 120k writers, `ecall` path, all other settings
identical. Pool size applied via `application:set_env(ecall, pool_size, N)` on
both nodes before `ecall` starts.

### `data` payload, 120k writers

| pool | perf % | lock wait | collisions | avg packet | peak mem |
|---:|---:|---:|---:|---:|---:|
| **1** | 54.4 | **0 s** | **0.0 %** | 53,937 B | 13.83 GB |
| **24** | **99.0** | 18 s | 46.2 % | 2,899 B | 3.02 GB |
| 64 (this run) | 90.6 | 156 s | 83.0 % | 2,604 B | 3.06 GB |
| 64 (Aug baseline) | 96.0 | 98 s | 75.6 % | 2,689 B | 3.05 GB |

`tiny`, 120k: pool 1 → 61.9 %, pool 24 → **98.3 %**, pool 64 → 95.4 %.

### The pool has its own knee, and its position moves with pool size

Performance across writer counts, `BL1024`:

| writers | native | pool 1 | pool 24 |
|---:|---:|---:|---:|
| 20,000 | 99.1 | 98.7 | 99.1 |
| 40,000 | 60.8 | 99.1 | 97.9 |
| 80,000 | 25.1 | 70.3 | 93.3 |
| 120,000 | 16.5 | 54.4 | 99.0 |

- **native** (120,000 direct writers): knee at 20–30k
- **pool 1**: knee at 40–80k
- **pool 24**: no knee within the tested range

So the funnel does not merely *shift* the collapse — the pool width sets where
it lands, and 24 pushes it beyond anything we tested.

### Why 24 beats 64

Fewer writers on `qlock` → lower collision rate (46 % vs 83 %) → the lock stays
below its contention threshold → each proxy waits longer between successful
sends → **bigger batches** (2,899 B vs 2,604 B) → fewer signals still. The same
virtuous cycle as 10a, now driven by pool width instead of by the busy limit.
Both levers feed the same mechanism.

### ⚠️ CORRECTION — "24 beats 64" does not survive replication

I wrote that confidently off one run. Repeating it with process-lock counting
enabled gives the opposite ordering:

| pool | `data` 120k, run P | run L |
|---:|---:|---:|
| 24 | 99.0 % | 93.7 % |
| 64 | 90.6 % (M1) / 96.0 % (Aug) / 99.0 % (L) | 99.0 % |

**Pool 24 and pool 64 are indistinguishable within run-to-run variance.** Both
are decisively better than pool 1. Do not publish a "24 is optimal" claim.

**What IS systematic — and is the better finding.** Pool width moves contention
from one lock to another, monotonically. `tiny`, 120k, cumulative wait:

| pool | proxy mailbox (`proc_msgq` + `proc_sig_queue_buffer`) | `dist_entry_out_queue` |
|---:|---:|---:|
| 1 | **299.0 s** | 0.0 s |
| 24 | 37.8 s | 81.7 s |
| 64 | 13.9 s | **344.7 s** |

Mailbox contention falls 21× as the pool widens; distribution-queue contention
rises without bound. **There is an optimum, and it is the crossover between two
opposing curves** — but our single runs cannot locate it, and on this hardware
the throughput plateau between 24 and 64 is wide enough that it does not matter
much in practice.

**Honest recommendation for the article:** present the trade-off curve, not a
magic number. Say that the pool must be large enough to escape mailbox
contention and small enough to keep the distribution queue below its threshold,
that anything in the tens works on a 48-core box, and that pool 1 is the
failure mode.

### ✅ Pool 1 — Roman's hypothesis confirmed: it is the proxy's mailbox lock

`tiny`, 120k writers, pool 1, `lcnt` mask `[distribution, process]`:

| lock | wait | tries | collisions |
|---|---:|---:|---:|
| **`proc_msgq`** | **286.3 s** | 7,217,991 | **81.4 %** |
| `proc_sig_queue_buffer` | 12.7 s | 135,267,781 | 4.1 % |
| `pix_lock` | 11.7 s | 212,042 | 9.7 % |
| `dist_entry_out_queue` | **0.0 s** | 1,108,306 | 0.9 % |

The distribution lock is *silent* — one proxy cannot contend with itself — and
286 seconds of contention has appeared on the single proxy's message queue.
**That is exactly the prediction.**

**And it shows the OTP 25 optimisation working, then being overwhelmed.**
`proc_sig_queue_buffer` took **135 million** acquisitions at only 4.1 %
collisions — the 64 signal-queue buffers absorbed the bulk of the fan-in
(conclusion 17). But every buffer flush still needs `proc_msgq`, and those
7.2 million flushes collided 81 % of the time. The shard mitigates; it does not
eliminate.

**Payload-dependent, and worth noting honestly.** For `data` at pool 1 the
locks are quiet (`proc_sig_queue_buffer` 8.9 s over exactly 120,021,839 tries —
one per message — and `proc_msgq` only 2.3 s), yet throughput is still 43 %. So
`data` at pool 1 is limited by **single-process throughput**, not lock
contention: one proxy receiving 1.2M msg/s and one remote worker dispatching
them. Two different bottlenecks behind the same symptom.

**Caveat:** enabling process-lock counting costs real throughput (pool 1 `data`
measured 54.4 % without it, 43.4 % with). The L-series numbers are internally
comparable but must not be compared against the P-series or the baseline.

### Pool 1 — superseded analysis

At pool 1 the distribution lock is essentially uncontended: **0 s wait, 0.0 %
collisions** — one proxy cannot contend with itself — yet throughput still
collapses to 54 %. So the bottleneck moved somewhere the `[distribution]` lock
mask cannot see. Two candidates, which pool 1 unfortunately conflates:

1. **The single proxy's message-queue lock** — 120,000 writers sending to one
   process (Roman's hypothesis).
2. **The single remote worker** — one process decoding every batch and
   performing every local send, a pure serial-consumer ceiling.

Note also the batch sizes at pool 1 are enormous — 53,937 B, roughly 315
messages per signal — which is exactly what conclusion 6 predicts when the
consumer is saturated. **Being resolved by the `[distribution, process]`
lock-category run (in progress).**

Status: `PROPOSED — pool-width result solid; pool-1 mechanism pending`

---

## 24. The measurement that measured itself: a 100 ms timer that fired 11 times in 10 minutes

The metrics collector is an ordinary Erlang process on the sender, waking on
`erlang:send_after(100, …)`. How often it actually woke is itself a result:

| run | samples | of expected | max gap |
|---|---:|---:|---:|
| `ecall` `BL1024` `data` 120k | 1,004 | 89.6 % | 10.2 s |
| native `BLMAX` `data` 120k | 3,929 | 75.1 % | 7.4 s |
| **native `BL1024` `data` 120k** | **11** | **0.2 %** | **485.7 s** |

On the **default** busy limit, a 100 ms timer fired **eleven times in 609
seconds**, with a single gap of over **eight minutes**. Timers are serviced by
scheduler threads; when the schedulers are parked in a futex on `qlock` and
grinding through O(N) resume herds, ordinary Erlang work simply does not run.

This is the "whole VM degrades" claim measured directly — and it is a far
better demonstration than any lock counter, because it needs no ERTS knowledge
to understand. **The node was not slow. For minutes at a time it was not
running Erlang at all.**

Raising the collector to `priority, max` recovered it to **43 %** (2,527 of
5,890 samples) — better, but still more than half the ticks lost.

### Two consequences

1. **It explains the one irreproducible metric.** `memory_max` on native
   `BL1024` was a maximum over ~11 samples of a 5 GB-amplitude sawtooth —
   effectively a random draw. That is exactly why `data` 120k read 37.4 GB in
   August and 47.3 GB in the replication while throughput and lock wait
   reproduced to 0.1 %. It also means the **`load.average_1m` figures in every
   native `BL1024` point are means over ~11 samples** and should not be quoted
   at all (reinforcing 12.8 and D-drop of the load metric).
2. **It is a methodological warning worth publishing.** In-VM sampling on a
   node that is itself the subject of a saturation experiment is not
   trustworthy without checking that the sampler ran. We only discovered this
   because we started retaining the sample series. Any BEAM benchmark that
   reports in-VM gauges under saturation without reporting sampler health is
   suspect — including, until three days ago, ours.

Status: `PROPOSED — strong candidate for the article's most memorable number`

---

## 25. ✅ The saw is a CPU/run-queue phenomenon — and the two busy limits are two different diseases

Roman was right that the saw is "mainly not about memory but about load average
and CPU consumption (the writers wait)". Captured at 100 ms with `cpu_util`
(`/proc/stat`), `sched_util` (`scheduler_wall_time`), `run_queue` and memory.
`data`, 120k writers:

| | native `BL1024` (default) | native `BLMAX` | **`ecall` `BL1024`** |
|---|---:|---:|---:|
| elapsed | 542 s | 492 s | **109 s** |
| OS CPU, mean | **73 %** (93 % while active) | **13.3 %** | 38.3 % |
| scheduler utilisation | **50.1 %**, pinned | 5.0 % (bursts to 50 %) | 15.3 % |
| run queue, mean | **67,140** (max 99,558) | **1** (bursts to 75,195) | **81** |
| `binary`, shape | **21.6 GB, flat** | 0 → 33.5 GB, sawing | **0.02 GB** |
| sampler health | 21 % of ticks | 75 % | 92 % |

### These are two qualitatively different failure modes, not one

**At the default 1 MiB limit there is no saw at all.** The suspend/resume cycle
runs at ~23 ms and averages into a *permanently saturated* state: ~88,000
processes runnable at every single sample, 93 % CPU, scheduler utilisation
pinned at exactly 50.1 % for the entire point.

**At the huge limit the oscillation becomes visible** because its period
stretches to ~33 s: the node sits at **5 % CPU with a run queue of 1** — every
writer suspended — then all 120,000 become runnable at once, the run queue
jumps to 35,000–75,000, CPU spikes to 94 %, the queue refills, and everyone
suspends again. **The thundering herd, directly visible in the run queue.**

So Roman's expectation that "at 1024 the behaviour is the same but harder to
distinguish" needs one refinement: at 1024 it is not a faster saw, it is a
*collapsed* saw — the oscillation is fast enough that the system never leaves
the saturated state.

### 🔑 The default busy limit costs 5.5× the CPU for the same throughput

`BL1024` 73 % mean CPU vs `BLMAX` 13.3 %, for 16.5 % vs 20.2 % throughput and
542 s vs 492 s elapsed. **Same work, same wall clock, five and a half times the
CPU.** That is a new and separable cost of the low limit — not throughput, not
memory, but burned cores — and it is what the load-average saw was showing.

This sharpens conclusion 9: raising `+zdbbl` on the *native* path does not fix
throughput, but it does dramatically reduce the CPU wasted on suspend/resume
churn. It buys efficiency, not speed. (For the *pooled* path the calculus is
different — see 10a.)

### The 43-point gap between CPU and useful work

At `BL1024` the OS reports **93 %** CPU while BEAM's own scheduler accounting
reports **50.1 %** active. Roughly 45 of 48 cores are busy; about 24 cores'
worth is doing anything. **~21 cores burned on nothing** — spinning on locks
and processing resume herds. This is the concrete form of the *Erlang in Anger*
warning that OS CPU% overstates real BEAM work, and it is why
`scheduler_wall_time` is the metric to quote.

### The single most quotable comparison

**Run queue: 88,000 versus 81.** Same workload, same hardware, same socket. The
native path keeps eighty-eight thousand processes permanently runnable and
never gets them served; the pooled path keeps eighty-one.

Status: `PROPOSED — the strongest figure set in the whole study`

---

## Open questions for the interview

- **Hardware.** CPU model / core count / RAM of `rt-server1.fp` and
  `rt-server2.fp`, and the NIC speed between them? The article needs this, and
  ≥10 GbE is implied by the 197 MB/s figure but should be stated, not inferred.
- **Angle.** Is this "here is a library, use it" (`ecall`) or "here is what I
  found out about ERTS distribution, and here is one way to work around it"?
  My strong recommendation is the second: it travels much further on HN/Lobsters
  and on the Erlang/Elixir forums, and it makes the library land better, not
  worse.
- **Do we run the `cast`/`call` suites before publishing?** Conclusion 12.2 is a
  real gap; `erpc:cast` is arguably the more common API in the wild.
- **Do we re-run the send matrix 3× for error bars?** Would materially harden
  conclusion 2 (the knee position) and neutralise the obvious HN objection.
- **Do we re-run without `beam.lcnt`** to get honest absolute numbers alongside
  the lock data?
- **Screenshots.** `performance_report` (React + Chart.js) renders exactly the
  seven trends we need. Use its screenshots, or redraw the charts for the
  article? I lean towards redrawing 3–4 of them cleanly and using one report
  screenshot to show the harness is real.
- **`test/performance/performance.config` contains live SSH credentials for
  both servers.** It must be scrubbed from the repo before we point anyone at
  it — and it is currently committed.

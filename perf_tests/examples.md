# Example articles — style, depth and flow references

**Purpose (Phase 3 of `article_prompt.txt`).** Not prior art, not citations — these are
*models*. Each entry answers: what shape is it, what is worth stealing, what to avoid.
Prior art and things we must cite live in `main_conclusions.md` §18.

**Verification legend** — the provenance rule from `main_conclusions.md` applies here too:

- 🟢 **read first-hand** — fetched and read during this research pass; the notes below
  describe content I actually saw.
- 🟡 **link confirmed, content not read** — URL and title verified from search results
  only. Notes are provisional. **Fetch before quoting anything from it.**

---

## The five to actually read before drafting

| # | article | why this one |
|---|---|---|
| 1 | [Winblad — The Many-to-One Parallel Signal Sending Optimization](https://www.erlang.org/blog/parallel-signal-sending-optimization/) 🟢 | The structural twin. Same subject (one lock, many senders), same audience, published by OTP itself |
| 2 | [Ostinelli — Boost message passing between Erlang nodes](https://www.ostinelli.net/boost-message-passing-between-erlang-nodes/) 🟢 | Our direct ancestor (§18). Read it to see exactly what we add that 2009 did not |
| 3 | [Gazler — The Road to 2 Million Websocket Connections in Phoenix](https://www.phoenixframework.org/blog/the-road-to-2-million-websocket-connections) 🟢 | The BEAM community's canonical benchmark narrative. Flow model for "each ceiling, and what was behind it" |
| 4 | [Lerche — Making the Tokio scheduler 10x faster](https://tokio.rs/blog/2019-10-scheduler) 🟢 | How to teach the mechanism before selling the fix, at 6000 words, without losing the reader |
| 5 | [Brooker — Metastability and Distributed Systems](https://brooker.co.za/blog/2021/05/24/metastable.html) 🟢 | The vocabulary for our knee (§2) and the suspend/resume oscillation (§22) |

---

## A. Structural twins — BEAM, one lock, many writers

### A1. Kjell Winblad, *The Many-to-One Parallel Signal Sending Optimization* (2021-11-05) 🟢
<https://www.erlang.org/blog/parallel-signal-sending-optimization/>

**The closest match to our article that exists.** ~2,500–3,000 words. Headings, in order:
Erlang Signals → The Signal Ordering Guarantee → Implementation *before* the optimization →
The optimization → Data structure / birds-eye view → Adaptive activation → Buffer array
structure → Sending with it active → Fetching and deactivation → **Benchmark** →
Possible Future Work → Conclusion.

**Steal:**
- **Ordering guarantee stated up front, before any optimisation.** We must do the same —
  it is the first objection to a batching funnel, and the OTP blog defuses it in §2.
- Old implementation drawn *first*, new one second, both as diagrams. Our "one `DistEntry`,
  one `qlock`, N senders" picture belongs in exactly that slot.
- **Adaptive activation** as a feature, not a knob — the same argument as our
  "batching is self-tuning, so it costs nothing at low load" (§6/D10). They had to make
  this argument too, and the reader will recognise the shape.
- Benchmark section is short, explicit (32-core machine, message-size sweep, two graphs)
  and comes *after* the mechanism. Ours can be the same order.

**Note the tension we must handle:** this optimisation is the *receiver-side* many-to-one
fix that landed in OTP 25 — and `main_conclusions.md` §17 says `ecall` quietly depends on
it. So this article is simultaneously a style model, a citation, and a fairness obligation.

### A2. Kjell Winblad, *The New Scalable ETS ordered_set* (2020-08-19) 🟡
<https://www.erlang.org/blog/the-new-scalable-ets-ordered_set/>

Contention-adapting search tree; the headline is "up to 100× on many cores". Same author,
same house style. Useful as the second data point for §16 ("this is a lock-contention
signature, not a distribution quirk") — three shared locks, one fix shape. **Fetch before
quoting the 100× figure.**

### A3. Kjell Winblad, *Decentralized ETS Counters for Better Scalability* (2021-08-03) 🟡
<https://www.erlang.org/blog/scalable-ets-counters/>

The third instance of the same story: one shared counter/lock, sharded. Short. Good model
for the compact "here is the same disease elsewhere in the VM" paragraph in §16.

### A4. John Högberg, *A few notes on message passing* (2021-03-19) 🟡
<https://www.erlang.org/blog/message-passing/>

Reference for how OTP explains send/receive costs to a general audience. Likely the best
piece to link when we need "how sends actually work" without writing it ourselves.

### A5. Roberto Ostinelli, *Boost message passing between Erlang nodes* (2009-04-07) 🟢
<https://www.ostinelli.net/boost-message-passing-between-erlang-nodes/>

~2,400 words. Measured 5.3M msg/min local vs **700K msg/min remote**, then added a
`gen_server` "QR" (queue/router) per node that accumulates messages keyed by destination
node and flushes **on queue size or a 200 ms timer** → **2.1M msg/min (3×)**. Full code:
`route/2`, `handle_cast/2` accumulating, `handle_info/2` unpacking on the far side.

**This is our article's ancestor and its sharpest hostile question**: "isn't this just
Ostinelli 2009?" The differences we own are (a) *why* it works — `dep->qlock`, measured;
(b) *how much* — the sender sweep; (c) `after 0` instead of a 200 ms timer, which is what
makes it free at low load. **Read this one properly before writing §18.**

**Also steal the format:** local-vs-remote as the opening hook. "The same send is 7×
cheaper inside one node" is a one-line problem statement that needs no setup.

### A6. Microsoft Orleans, *Solving a Transactions Performance Mystery* 🟡
<https://dotnet.github.io/orleans/blog/solving-a-transactions-performance-mystery.html>

Another actor runtime, same disease: shared lock → thread-local structures + lighter
locks → ~4× throughput. Useful as the "not a BEAM problem, an actor-runtime problem"
counterweight if we want one non-Erlang datapoint in §16.

---

## B. "We hit a runtime limit and built something" — the launch-post shape

### B1. Discord, *How Discord Scaled Elixir to 5,000,000 Concurrent Users* 🟡 (quotes verified in §18)
<https://discord.com/blog/how-discord-scaled-elixir-to-5-000-000-concurrent-users>
· HN: <https://news.ycombinator.com/item?id=14748028>
· Elixir Forum: <https://elixirforum.com/t/how-discord-scaled-elixir-to-5-000-000-concurrent-users/6788>

**The benchmark for reception, not just for style** — read the HN and forum threads as
carefully as the post. This is the single closest analogue for *how our post will be
received*, and `manifold` is our nearest live competitor in the "fan-out to remote PIDs"
slot.

**Steal:** the concrete-cost sentence. "*a single `send/2` call could range from 30μs to
70μs*" and "*publishing an event from a large guild could take anywhere from 900ms to
2.1s*" — two numbers, one user-visible consequence. Our equivalent must be as blunt.
**Avoid:** their post explains the *what* and skips the *why*; the mechanism is asserted.
That gap is precisely our contribution — do not reproduce it.

### B2. Discord, *Using Rust to Scale Elixir for 11 Million Concurrent Users* 🟡
<https://discord.com/blog/using-rust-to-scale-elixir-for-11-million-concurrent-users>
· mirror: <https://medium.com/discord-engineering/using-rust-to-scale-elixir-for-11-million-concurrent-users-c6f19fc029d3>

Model for "we profiled, found the hot structure, replaced it, here is before/after with no
memory regression". Also a model for cross-posting to Medium under an engineering
publication — relevant to our venue list.

### B3. Carl Lerche, *Making the Tokio scheduler 10x faster* (2019-10) 🟢
<https://tokio.rs/blog/2019-10-scheduler> · Lobsters: <https://lobste.rs/s/8spuvn/making_tokio_scheduler_10x_faster>

~5,500–6,000 words, 15–20 min read. Layered structure: **scheduler theory first** (three
models: global queue, sharded, work-stealing) → what the old one did → what the new one
does → benchmarks → caveats.

**Steal, specifically:**
- Teaching the *general* mechanism before the *specific* fix. Our §4 (what a mutex
  actually costs) sits in that slot and should stay there.
- Micro-benchmark **plus** an end-to-end number (`chained_spawn` ~12×, but also "+34% on a
  real HTTP server"). We have the equivalent pair; use both, and say which is which.
- **Explicit hedging that strengthens rather than weakens**: "*It is always unclear how
  much these kinds of improvements impact 'full stack' use cases*", "*it's hard to say
  exactly*". This is the register for our §12 Limitations and for D12's downgrade of the
  hardware claim to a hypothesis. Hedge in the author's voice, in the body — not in a
  disclaimer box at the end.
- He names the failure modes of his own design (thundering herd from overeager
  notification). We have one too (§10a: raising `+zdbbl` hurts the pooled path).

### B4. Partisan — Meiklejohn, Miller, Alvaro, USENIX ATC '19 🟡
Paper: <https://www.usenix.org/system/files/atc19-meiklejohn.pdf> ·
arXiv: <https://arxiv.org/abs/1802.02652> ·
repo: <https://github.com/lasp-lang/partisan> ·
talk: <https://www.youtube.com/watch?v=KrwhOkiifQ8>

Claims up to **38.07×** over distributed Erlang by *replacing* the distribution layer.
The main design alternative to our answer, and the paper is the model for how to write an
evaluation section (topologies, workloads, what was held constant). **Read the evaluation
section for methodology, cite the 38× only from the paper's own text.**

---

## C. BEAM measurement narratives — flow and disclosure

### C1. Gary Rennie (Gazler), *The Road to 2 Million Websocket Connections in Phoenix* (2015-11-03) 🟢
<https://www.phoenixframework.org/blog/the-road-to-2-million-websocket-connections>
· slides: <https://blog.gazler.com/talks/road-to-2-million.pdf>
· reproduction repo: <https://github.com/dsander/phoenix-connection-benchmark>

~2,500 words, 11 sections. Chronological detective story; every section is
**ceiling hit → cause → fix → next ceiling**: `ulimit` → heartbeat mailbox bloat →
not enough client machines → ETS `:bag` vs `:duplicate_bag` → single pubsub server →
sharded pubsub pool. Conversational ("*I wasn't convinced by the number*"), with hardware
named (40-core/128 GB, 45+ load generators) and the load tool named (Tsung).

**Steal:**
- **The client is part of the experiment.** They repeatedly hit *their own load
  generator's* ceiling and say so. Our §24 (the 100 ms timer that fired 11 times in
  10 minutes) is exactly this genre and should be told in the same self-deprecating,
  confident register — it is a credibility *asset*, not an embarrassment.
- Full methodology stated early enough to be checkable, in its own "How To Run The
  Benchmarks" section, not appended.
- A third party reproduced it (repo above). Publishing our harness invites the same.

### C2. Stressgrid, *The Curious Case of BEAM CPU Usage* (2019-02-09) 🟢
<https://stressgrid.com/blog/beam_cpu_usage/>

Short (~6 min). Elixir showed far higher CPU than Go with identical responsiveness;
microstate accounting showed **56% of scheduler time in busy waiting, 12% actually running
code**; disabling busy-wait dropped CPU from >95% to proportional with **no meaningful
latency difference**.

**This is a load-bearing reference for us, not just a style note.** §16b already forbids
publishing "low CPU / high load average" as evidence of lock contention; this article is
the reason BEAM CPU numbers cannot be read naively at all, in either direction. If §25
publishes the "5.5× the CPU for the same throughput" comparison, **this article is the
objection we must pre-empt** — cite it and state whether busy-wait was on in our runs.

### C3. Stressgrid, *Achieving 100k connections per second with Elixir* (2019-03) 🟡
<https://stressgrid.com/blog/100k_cps_with_elixir/>

Named as the example of *confirming* a bottleneck by patching the suspected component and
re-measuring (multiple Ranch acceptors / multiple listen sockets). That is the method our
§4b rests on, done in public. Companion posts in the same series:
<https://stressgrid.com/blog/benchmarking_go_vs_node_vs_elixir/> and
<https://stressgrid.com/blog/webserver_benchmark/> (the latter reached
[Lobsters](https://lobste.rs/s/gi3bgm/webserver_benchmark_erlang_vs_go_vs_java) and
[Elixir Forum](https://elixirforum.com/t/webserver-benchmark-erlang-vs-go-vs-java-vs-nodejs/28090)
— worth skimming both threads for how a benchmark post gets attacked).

### C4. Sequin, *We used Elixir's Observer to hunt down bottlenecks* 🟢
<https://blog.sequinstream.com/how-we-used-elixirs-observer-to-hunt-down-bottlenecks/>

~10 min. Starts from **"high load average, all cores at ~40%, benchmarks degrading"** and
walks Observer: run queue → scheduler utilisation → reductions → memory → message queue
length → process memory. Ends with a Recap plus two appendices (how to run Observer, how
to starve the system deliberately).

**Two distinct uses for us.** (1) Style: the diagnostic walk-through is well paced and the
appendices are a good pattern for "how to reproduce this on your own node". (2) Substance:
**this is the article whose symptom §16b tells us not to reuse as evidence.** It reports
*no* before/after numbers — a real weakness, and a reminder that a methodology post
without a measured delta is much easier to dismiss than one with it.

### C5. Erlang Solutions house style (target venue) 🟡
- *MongooseIM 6.1: Handle more traffic, consume less resources* —
  <https://www.erlang-solutions.com/blog/mongooseim-6-1-handle-more-traffic-consume-less-resources/>
- *Scaling a Mongoose: how scalable is the MongooseIM XMPP server?* —
  <https://www.erlang-solutions.com/blog/scaling-a-mongoose-how-scalable-is-the-mongooseim-xmpp-server/>
- *Erlang Concurrency: Evolving for Performance* (2025-01-13) — category index:
  <https://www.erlang-solutions.com/blog/category/erlang/>

Read these purely as **venue reconnaissance**: length, how many graphs, how much source
code, how promotional the ending is allowed to be. `mod_global_distrib`
(<https://esl.github.io/MongooseDocs/latest/modules/mod_global_distrib/>) is also listed in
§18's prior-art table, so this venue's audience already knows the pattern.

---

## D. Framing the collapse, the knee, and backpressure

### D1. Marc Brooker, *Metastability and Distributed Systems* (2021-05-24) 🟢
<https://brooker.co.za/blog/2021/05/24/metastable.html>

The concept our §2 (sharp knee) and §22 (suspend/resume saw-tooth) need: a system with two
stable states, pushed across a threshold by a trigger, which then **stays broken after the
trigger is removed** because a feedback loop sustains it. High throughput, zero goodput.
His line that "*optimizations that apply only to the common case exacerbate feedback
loops*" is almost a description of the busy-limit resume herd.

**Steal:** the vocabulary (trigger vs sustaining effect, goodput vs throughput), and the
discipline of blaming the *loop*, not the trigger. **Do not** claim our collapse is
formally metastable unless the data shows hysteresis — say "the same shape".

Companion pieces, same author, same register:
- *Garbage Collection and Metastability* — <https://brooker.co.za/blog/2024/08/14/gc-metastable.html>
- *Surprising Economics of Load-Balanced Systems* — <https://brooker.co.za/blog/2020/08/06/erlang.html> 🟡

### D2. Marc Brooker, *It's always TCP_NODELAY. Every damn time.* (2024-05-09) 🟡
<https://brooker.co.za/blog/2024/05/09/nagle.html> ·
Lobsters: <https://lobste.rs/s/kocje7/it_s_always_tcp_nodelay_every_damn_time> ·
HN: <https://news.ycombinator.com/item?id=46359120>

**Directly load-bearing for D9/§21.** Every reader who reaches our batching section will
think "Nagle" — half of them approvingly (§10b's analogy), half of them dismissively
("you reinvented Nagle / you forgot `TCP_NODELAY`"). This post is the canonical statement
of the dismissive position; §21 answers it with a measured `-kernel dist_nodelay false`
run. Cite it *by name* where we make the Nagle analogy, so it reads as deliberate.
Also a masterclass in the short, opinionated, single-claim post — the opposite pole from
Tokio's 6,000 words, and possibly the model for a *companion* short post.

### D3. Fred Hébert, *Queues Don't Fix Overload* (2014) 🟡
<https://ferd.ca/queues-don-t-fix-overload.html> · HN: <https://news.ycombinator.com/item?id=8632043>
Companion: *Handling Overload* — <https://ferd.ca/handling-overload.html>

The BEAM community's standard reference on backpressure: queues absorb *variance*, not
sustained load; unbounded queues are a bug.

**This is the sharpest internal objection to `ecall` and must be answered explicitly.**
§18's own honest note already says production casts return after enqueueing into a proxy
mailbox with **no end-to-end admission control** — i.e. we moved the backlog from ERTS's
queue into ours. Hébert's post is what a reader will reach for. Either we cite it and state
the trade-off (§11) in his terms, or someone else will, less kindly.

Same author, book-length: *Stuff Goes Bad: Erlang in Anger* —
<https://www.erlang-in-anger.com/> · PDF:
<https://s3.us-east-2.amazonaws.com/ferd.erlang-in-anger/text.v1.1.0.pdf> 🟡
(§18 also flags "does *Erlang in Anger* mention `+zdbbl`?" as an unverified claim —
the PDF is right there, so **check it, don't cite it from memory**.)

### D4. Jeff Preshing, *Locks Aren't Slow; Lock Contention Is* (2011-11-18) 🟢
<https://preshing.com/20111118/locks-arent-slow-lock-contention-is/>

~2,000 words. Threads doing randomised (Poisson) work between acquisitions; sweeps **lock
hold time as a fraction of thread time** (0→100%) and **work granularity** (10 ns→31 µs)
across 1–4 threads. Result: fine below ~10% hold fraction, then degrades sharply, with a
cliff near 60% at 4 threads that he attributes, honestly and tentatively, to the OS
scheduler.

**Steal:** this is the correct *shape* for our §4a ("what a mutex actually costs —
measured, not assumed"). He sweeps one variable, plots it, and refuses to over-explain the
anomaly. Also the title pattern — a correction of a widely held belief, in five words.

### D5. Neil Gunther, *Universal Scalability Law* 🟡
<https://www.perfdynamics.com/Manifesto/USLscalability.html> ·
synopsis PDF: <https://www.perfdynamics.com/Manifesto/USLscalability.pdf> ·
accessible summary: <https://blog.acolyer.org/2015/04/29/applying-the-universal-scalability-law-to-organisations/>

Throughput = f(N) with a **contention** term (α, linear) and a **coherency** term (β,
quadratic) — and the quadratic term is why throughput *decreases* past a point rather than
plateauing. That is our §1 in one equation: "remote `!` does not scale with the number of
senders — it scales *down*".

**Use with care.** Naming USL buys us a precise frame and an instant "this is a known law,
not a mystery" for the sceptical reader. It also invites "then fit your data to it", which
D3 (no further test runs) forbids. Recommendation: mention as framing, do not fit.

### D6. Gil Tene, *How NOT to Measure Latency* 🟡
<https://www.youtube.com/watch?v=lJ8ydIuPFeU> ·
write-up: <https://thomashunter.name/posts/2023-03-11-how-not-to-measure-latency>

Coordinated omission: a closed-loop load generator that waits for a response before
sending the next request under-samples the tail exactly when the system is worst.

**Relevant to us as a self-check, not as a citation.** `pool_send/3` waits for a proxy
acknowledgement before each client proceeds (`findings.md`, "Existing benchmark
limitations") — that is closed-loop. Whatever the article claims about latency or pacing,
§12 should say plainly whether the harness is open- or closed-loop. A reader who knows this
talk will ask.

---

## E. Narrative craft — the detective register

### E1. Cloudflare, *The story of one latency spike* 🟡
<https://blog.cloudflare.com/the-story-of-one-latency-spike/> ·
Lobsters: <https://lobste.rs/s/dariaq/story_one_latency_spike>

Opens with a customer complaint and thousands of requests of which **five** are slow, then
descends the stack until the cause is found. The gold standard for "start from the symptom
a real operator sees, not from the mechanism". Our §13 ("who this is for") is trying to do
the same job — an operator recognising their own system — and could borrow this opening
move: describe the symptom before naming the cause.

### E2. Hamidreza Soleimani, *Erlang Scheduler Details and Why It Matters* (2016-02-09) 🟡
<https://hamidreza-s.github.io/erlang/scheduling/real-time/preemptive/migration/2016/02/09/erlang-scheduler-details.html>
· GC companion (2015-08-24):
<https://hamidreza-s.github.io/erlang%20garbage%20collection%20memory%20layout%20soft%20realtime/2015/08/24/erlang-garbage-collection-details-and-why-it-matters.html>

Independent (non-OTP) deep dives into VM internals that became community standard
references. Model for how much C-level detail an outside author can present before losing
the audience — relevant because our §4 quotes `dist.c` line ranges.

### E3. Zaid Humayun, *The Concurrency Trap: How an Atomic Counter Stalled a Pipeline* (2025-06-05) 🟡
<https://redixhumayun.github.io/concurrency/2025/06/05/the-concurrency-trap-how-an-atomic-counter-stalled-a-pipeline.html>

Recent, small-scale, single-cause contention post with flamegraphs before/after. Useful as
a length/scope contrast: proof that the genre still works at 1/5 our size, which is an
argument for splitting our material across a long post plus a short one.

---

## F. Tooling references to link, not to imitate

- **`lcnt` — The Lock Profiler** — <https://www.erlang.org/doc/apps/tools/lcnt_chapter.html> 🟡
  Our only instrument for §4b. Link it once so readers can repeat the measurement.
  (§18a: Rick Reed's 2012 deck calls BEAM lock-counting "*invaluable!!!*" and OTP-10051
  credits him with extending it — that pairing is a strong, sourced aside.)
- **Erlang/OTP blog index** — <https://www.erlang.org/blog> 🟢
  Also worth a look for tone: *Retiring old performance pitfalls*
  (<https://www.erlang.org/blog/retired-pitfalls-22/>) and *OTP 22 Highlights*
  (<https://www.erlang.org/blog/otp-22-highlights/>) 🟡.
- **WhatsApp, Rick Reed** — decks already extracted into
  [`sources/reed-efsf2012-slides.txt`](sources/reed-efsf2012-slides.txt) and
  [`sources/reed-efsf2014-slides.txt`](sources/reed-efsf2014-slides.txt); video/InfoQ page
  at <https://www.infoq.com/presentations/whatsapp-scalability/> 🟡.
  Prior art (§18a), not a style model — one slide of bullets, no mechanism, no numbers.

---

## What this survey says about our article

1. **Nobody has published the mechanism.** Ostinelli (A5) has the fix and no why.
   Discord (B1) has the why asserted and not measured. WhatsApp (§18a) has one slide.
   Winblad (A1) has mechanism + measurement, but for the *intra-node* signal queue. The
   `dist_entry_out_queue` gap is real, and this is the strongest support for D1's ordering
   (finding first, library second).
2. **The genre's winners all sweep one variable and plot it** — Preshing (D4),
   Winblad (A1), Partisan (B4). Our sender sweep is the asset; lead the evidence with it.
3. **The genre's winners are also loudly honest.** Tokio (B3) hedges in the author's voice;
   Phoenix (C1) admits its own load generator was the bottleneck. §12 and §24 are
   credibility, not damage control — put them in the body.
4. **Three objections are pre-loaded by the existing literature** and each needs a named
   answer in the text: *Nagle/`TCP_NODELAY`* (D2 → §21), *"queues don't fix overload"*
   (D3 → §11), and *"BEAM CPU numbers don't mean what you think"* (C2 → §25).
5. **Two lengths are viable.** ~2,500 words (A1, C1, D4) or ~6,000 (B3). Our material is
   6,000+. Consider the long post as the canonical version for forums.erlang.org /
   elixirforum / HN, and a short, single-claim companion in D2's style for
   dev.to / Hashnode / LinkedIn.

## Gaps — worth a second pass if Phase 4 needs them

- A **`+zdbbl` folklore** post to point at. Search found only mailing-list traffic
  (the Simon MacMullen quote in §18) — if no article exists, §9/§10 is more novel than
  assumed, and that is worth confirming before claiming it.
- A **saw-tooth / oscillation** visual to model §22a's graph on.
- A well-received **Hashnode** technical deep dive — the venue is on the list and is
  entirely unrepresented here.

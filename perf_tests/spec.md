# Performance Test Specification

## Scope

The tests compare receiver-confirmed throughput for raw distributed sends,
`erpc:cast/4`, `erpc:call/4`, and the corresponding `ecall` operations. They
increase load until throughput reaches a ceiling or collapses, then correlate
the result with distribution-lock, network, and batching metrics.

Topology, orchestration, run duration, repetition, and implementation are not
defined yet.

## Common workload

- Client counts: `1`, `1K`, `10K`, `100K`, `500K`, and `1M`.
- Each client schedules one operation every 100 ms: 10 operations/s.
- Corresponding offered rates are 10/s, 10K/s, 100K/s, 1M/s, 5M/s, and
  10M/s.
- The primary payload is the `DATA` term from `src/ecall_test.erl`.
- Send and cast throughput is counted at the final remote target.
- Call throughput is counted from completed replies.
- E3 allows at most one outstanding call per client. A client whose call takes
  longer than 100 ms cannot produce its full scheduled rate.
- E1-E3 use the production `ecall` behavior. H3 additionally uses controlled
  proxy batch sizes.

## Cases

### H1 - Raw-send workload curve

Send directly to one remote PID using the common client counts and cadence.

Purpose: establish the raw distribution throughput curve and determine whether
increasing load produces a ceiling or throughput collapse.

### H2 - Fixed-work writer contention

- Writer counts: `1`, `1K`, `10K`, `100K`, `500K`, and `1M`.
- Deliver exactly 1M messages in every point.
- Release all writers through one start barrier.
- Divide the 1M messages evenly among the writers.
- Writers send their quota without pacing.

Purpose: keep total work constant while changing the number of processes
concurrently entering the distribution path.

### H3 - Sender pool and batching decomposition

Run the common workload using:

1. Direct distributed writers.
2. A fixed proxy pool with batch size 1 as the no-batching control.
3. The same proxy pool with batch sizes `10`, `100`, `1000`, and `10K`.

Purpose: batch size 1 isolates the benefit of limiting distribution writers;
the other variants measure the additional batching benefit.

### H5 - Distribution busy-limit sweep

Repeat selected near-collapse raw-send points with distribution busy limits of
256 KiB, the default 1 MiB, and 8 MiB.

Purpose: determine whether the degradation changes with the distribution
buffer busy limit and whether the limit changes sustainable throughput.

### H10a - Payload cost

Repeat a selected raw-send load with:

- a tiny atom;
- the existing `DATA` term;
- a 100 KiB reference-counted binary;
- a 1 MiB reference-counted binary.

Purpose: show how encoding, distribution bandwidth, decoding, and payload size
move the throughput ceiling.

### H10b - Receiver work

Repeat the same raw-send load and payload with:

- counter-only receive;
- payload traversal;
- one process spawn per delivered message.

Purpose: determine whether receiver capacity causes or amplifies sender-side
throughput collapse.

### E1 - Raw send versus `ecall:send/2`

Run both paths for every common client count using the same remote target and
payload. Count messages at the final receiver.

Purpose: determine at which concurrency `ecall` becomes faster or slower than
a raw remote-PID send.

### E2 - `erpc:cast/4` versus `ecall:cast/4`

Run both paths for every common client count. Both target functions perform the
same minimal work and increment the delivered-operation counter.

Purpose: compare remotely executed casts rather than locally accepted cast
requests.

### E3 - `erpc:call/4` versus `ecall:call/4`

Run both paths for every common client count. Both target functions return the
same fixed result. Count completed replies.

Purpose: compare completed-call capacity for equivalent remote work.

## Metrics

### Primary metrics

- Measurement duration.
- Configured offered operations/s.
- Total remotely delivered operations.
- Remotely delivered operations/s.
- Delivery ratio: delivered operations divided by offered operations.
- `ecall` throughput divided by baseline throughput for E1-E3.

The primary throughput values are:

- E1 and H cases: messages received by the final remote target per second.
- E2: remote target-function executions per second.
- E3: completed calls per second.
- H2: 1M divided by the time required to deliver all 1M messages.

### Supportive metrics

- `dist_entry_out_queue` lock attempts.
- `dist_entry_out_queue` collisions.
- `dist_entry_out_queue` collision percentage.
- `dist_entry_out_queue` cumulative wait time.
- Distribution bytes sent/s and received/s.
- Distribution `send_cnt`/s where available.
- Batches flushed because they reached the configured limit.
- Partial batches flushed because the proxy mailbox became empty.
- Maximum proxy mailbox length during the measurement.

`send_cnt` is the transport/driver send counter exposed for the distribution
connection; it is not a count of logical Erlang messages.

Lock statistics require a lock-counting emulator. Results produced with it must
be marked as diagnostic. Absolute throughput from a lock-counting VM must not be
merged with normal-emulator throughput. Trends across load points from the same
diagnostic run may be correlated with collision and wait statistics.

Cumulative lock wait may exceed wall-clock duration because several scheduler
threads can wait concurrently.

## Report format

Every test point produces a row containing its case parameters, primary
metrics, applicable supportive metrics, emulator type, and node role. Missing
or unavailable supportive metrics are reported as `n/a`.

E1-E3 additionally produce paired comparison grids with baseline throughput,
`ecall` throughput, and their ratio.

## Illustrative reports

All values below are invented. The H1, H3, H5, and H10 examples assume a
30-second measurement window only to demonstrate presentation.

### H1 example

| Clients | Offered/s | Delivered/s | Delivery | TX MiB/s | send_cnt/s | Lock attempts | Collisions | Collision | Wait |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 10 | 10 | 100% | 0.01 | 8 | 620 | 1 | 0.2% | 0.00 s |
| 1K | 10K | 9.9K | 99% | 9.5 | 3.1K | 610K | 31K | 5.1% | 0.02 s |
| 10K | 100K | 96K | 96% | 92 | 18K | 5.8M | 2.0M | 34% | 1.8 s |
| 100K | 1M | 240K | 24% | 230 | 32K | 15.0M | 12.0M | 80% | 75 s |
| 500K | 5M | 130K | 2.6% | 125 | 20K | 12.0M | 11.3M | 94% | 420 s |
| 1M | 10M | 55K | 0.6% | 53 | 9K | 8.0M | 7.8M | 97.5% | 880 s |

### H2 example

| Writers | Completion time | Delivered/s | TX MiB/s | Lock attempts | Collisions | Collision | Wait |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.8 s | 556K | 530 | 2.1M | 22K | 1.0% | 0.03 s |
| 1K | 1.6 s | 625K | 596 | 2.2M | 480K | 22% | 0.7 s |
| 10K | 2.1 s | 476K | 454 | 2.3M | 1.4M | 61% | 5.2 s |
| 100K | 4.5 s | 222K | 212 | 2.6M | 2.2M | 85% | 38 s |
| 500K | 11.5 s | 87K | 83 | 3.1M | 2.9M | 94% | 280 s |
| 1M | 24.0 s | 42K | 40 | 4.0M | 3.9M | 97% | 960 s |

### H3 example

This example uses 100K clients and 1M offered operations/s.

| Path | Batch | Delivered/s | Delivery | TX MiB/s | send_cnt/s | Attempts | Collisions | Collision | Wait | Full/partial batches | Proxy queue max |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Direct | n/a | 240K | 24% | 230 | 32K | 15.0M | 12.0M | 80% | 75 s | n/a | n/a |
| Proxy | 1 | 410K | 41% | 391 | 41K | 12.4M | 7.1M | 57% | 24 s | 12.3M/0 | 820K |
| Proxy | 10 | 650K | 65% | 610 | 29K | 4.1M | 1.5M | 37% | 5.4 s | 1.80M/150K | 290K |
| Proxy | 100 | 880K | 88% | 815 | 18K | 810K | 150K | 19% | 0.8 s | 250K/14K | 22K |
| Proxy | 1K | 950K | 95% | 870 | 9K | 210K | 18K | 8.6% | 0.1 s | 26K/2.5K | 4K |
| Proxy | 10K | 920K | 92% | 845 | 7K | 130K | 9K | 6.9% | 0.1 s | 1.9K/900 | 12K |

### H5 example

This example uses raw send and 100K clients.

| Busy limit | Offered/s | Delivered/s | Delivery | TX MiB/s | send_cnt/s | Attempts | Collisions | Collision | Wait |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 256 KiB | 1M | 195K | 19.5% | 187 | 29K | 14.2M | 12.1M | 85% | 110 s |
| 1 MiB | 1M | 240K | 24.0% | 230 | 32K | 15.0M | 12.0M | 80% | 75 s |
| 8 MiB | 1M | 265K | 26.5% | 253 | 34K | 15.8M | 11.7M | 74% | 58 s |

### H10a example

This example uses raw send and 100K clients.

| Payload | Offered/s | Delivered/s | Delivery | TX MiB/s | send_cnt/s | Attempts | Collisions | Collision | Wait |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Tiny | 1M | 520K | 52% | 18 | 48K | 22M | 15M | 68% | 41 s |
| `DATA` | 1M | 240K | 24% | 230 | 32K | 15M | 12M | 80% | 75 s |
| 100 KiB | 1M | 1.1K | 0.11% | 107 | 1.0K | 420K | 360K | 86% | 92 s |
| 1 MiB | 1M | 115 | 0.01% | 112 | 108 | 82K | 75K | 91% | 130 s |

### H10b example

This example uses raw send, 100K clients, and the `DATA` payload.

| Receiver work | Delivered/s | Delivery | RX MiB/s | Lock attempts | Collisions | Collision | Wait |
|---|---:|---:|---:|---:|---:|---:|---:|
| Count only | 240K | 24% | 228 | 15M | 12M | 80% | 75 s |
| Traverse payload | 150K | 15% | 143 | 13M | 11M | 85% | 105 s |
| Spawn process | 52K | 5.2% | 50 | 9M | 8.4M | 93% | 260 s |

### E1 example

| Clients | Offered/s | Raw delivered/s | `ecall` delivered/s | `ecall/raw` | Raw collision | `ecall` collision | `ecall` full/partial batches |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 10 | 10 | 10 | 1.00x | 0.2% | 0.1% | 0/300 |
| 1K | 10K | 9.9K | 10K | 1.01x | 5% | 2% | 0/18K |
| 10K | 100K | 96K | 99K | 1.03x | 34% | 12% | 400/29K |
| 100K | 1M | 240K | 850K | 3.54x | 80% | 18% | 22K/6K |
| 500K | 5M | 130K | 700K | 5.38x | 94% | 27% | 19K/5K |
| 1M | 10M | 55K | 420K | 7.64x | 97.5% | 39% | 11K/4K |

### E2 example

| Clients | Offered/s | `erpc` executed/s | `ecall` executed/s | `ecall/erpc` | `erpc` collision | `ecall` collision | `ecall` full/partial batches |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 10 | 10 | 10 | 1.00x | 0.3% | 0.1% | 0/300 |
| 1K | 10K | 9.8K | 10K | 1.02x | 7% | 2% | 0/18K |
| 10K | 100K | 75K | 95K | 1.27x | 48% | 15% | 350/28K |
| 100K | 1M | 120K | 500K | 4.17x | 89% | 24% | 13K/4K |
| 500K | 5M | 45K | 300K | 6.67x | 97% | 38% | 8K/3K |
| 1M | 10M | 18K | 140K | 7.78x | 99% | 51% | 3K/2K |

### E3 example

| Clients | Scheduled/s | `erpc` completed/s | `ecall` completed/s | `ecall/erpc` | `erpc` collision | `ecall` collision | `ecall` full/partial request batches |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 10 | 10 | 10 | 1.00x | 0.5% | 0.2% | 0/300 |
| 1K | 10K | 10K | 10K | 1.00x | 9% | 4% | 0/21K |
| 10K | 100K | 60K | 80K | 1.33x | 58% | 22% | 500/23K |
| 100K | 1M | 100K | 250K | 2.50x | 91% | 41% | 6K/2K |
| 500K | 5M | 35K | 140K | 4.00x | 98% | 63% | 3K/1K |
| 1M | 10M | 10K | 60K | 6.00x | 99% | 78% | 1K/800 |

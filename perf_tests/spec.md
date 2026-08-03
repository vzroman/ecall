# Performance Test Specification

## Purpose

The performance tests compare native distributed Erlang operations with their
`ecall` analogues under the same finite workload:

- `Pid ! Message` versus `ecall:send/2`;
- `erpc:cast/4` versus `ecall:cast/4`;
- `erpc:call/4` versus `ecall:call/4`.

Every test point has a fixed number of writer processes, a fixed number of
operations per writer, a per-writer pace, one payload, and one operation path.
A point runs until every planned operation is confirmed complete or a monitored
participant fails. Successful run progression and completion are exclusively
readiness- and completion-based.

The tests are intended to show the combined practical effect of `ecall`
proxying and batching. They do not claim to isolate one ERTS mechanism. The
project findings explain why the comparison is valuable: native sends to one
remote node share one distribution path, while `ecall` limits direct
distribution writers and combines logical operations into batches.

Receiver-work variants, automated comparison reports, CSV files, and
presentation tooling are outside this implementation phase.

## Implementation layout

All performance-test code lives under `test/performance/`. Suites are directly
under that directory; reusable modules are under `test/performance/util/`.

```text
test/performance/
  util/
    distributed_tests_utils.erl
    performance_payloads.erl
  performance_send_SUITE.erl
  performance_cast_SUITE.erl
  performance_call_SUITE.erl
  performance.config
  test.spec
  Dockerfile
```

`distributed_tests_utils` is only responsible for starting and stopping
participating nodes, including node readiness before it returns. Workload
orchestration is kept inside each suite, even where that repeats code. Payload
construction remains centralized in `performance_payloads`. The build and
Common Test code paths must make the suite and utility BEAM files available to
the controller, sender, and receiver nodes.

`perf_tests/spec.md` and `perf_tests/findings.md` are design documents, not
Common Test inputs. A reporting module is not part of this phase.

The normal entry point remains:

```sh
make performance_tests
```

It compiles the project and invokes Common Test with
`test/performance/test.spec`.

## Topology

Common Test is the only orchestrator. The topology has three Erlang nodes:

- the Common Test controller started by `rebar3 ct`;
- a Docker-backed sender peer that owns the writer processes;
- a Docker-backed receiver peer that owns the targets and completion counters.

The controller runs on the host. Sender and receiver run in Docker containers
started and stopped through OTP `peer`. A role can use Docker on the local host
or on a remote host reached through SSH. Containers never run Common Test.

Both role containers use the same image and compiled code. The tests do not use
a reduced passive-node runtime or a non-Docker role-node fallback.

## Common Test configuration

All role and workload values come from ordinary Common Test configuration and
its built-in config-file selection.

The default `performance.config` is:

```erlang
{role_config, #{
  sender => local,
  receiver => local
}}.

{performance, #{
  pace_ms => 100,
  messages_per_writer => 1000,
  writer_counts => [1000, 10000, 100000, 500000, 1000000]
}}.

{env_settings, #{
  ecall_batch_size => 1000,
  distribution_busy_limit_kib => 1024
}}.
```

The payload list is hardcoded by `performance_payloads`. The writer-count list
is ordered, and each suite runs writer counts in that configured order. A user
can replace the writer list, change the pace, message quota, `ecall` batch
size, or busy limit through Common Test config. The harness does not validate
config values: bad input is allowed to crash the test.

The full default matrix is intentionally very large. Development and smoke
runs should use a smaller CT configuration.

A remote role is configured directly in CT config:

```erlang
{role_config, #{
  sender => local,
  receiver => #{
    host => "receiver.example.net",
    user => "test-user",
    password => "password"
  }
}}.
```

## Docker and peer requirements

The fixed role image is `ecall-performance:otp27` and uses OTP 27. Its build
context contains the compiled application, performance suites, and utility
modules. The application is available at `/opt/ecall` in the container.

Role containers use host networking so Erlang distribution is reachable
between the controller and both Docker hosts. The controller and peers use
long node names, one shared cookie, and non-conflicting distribution ports.
Remote Docker commands are executed over SSH using the role config.

`distribution_busy_limit_kib` is passed to both participant VMs as:

```text
+zdbbl <distribution_busy_limit_kib>
```

`+zdbbl` is a startup option measured in KiB. Changing the busy limit requires
a new peer start. A busy-limit experiment is therefore a new CT invocation or
suite run with another config value, not an in-suite sweep on existing nodes.

System capacity is an orchestration prerequisite, not a workload parameter.
Both participant nodes start with fixed limits that leave capacity for the
largest configured point:

```text
+P 134217727
+Q 1048576
+e 262144
```

These are test-harness constants and are not exposed through CT config.
`+P` uses the OTP 27 maximum so the sender can hold all writer processes and
the receiver can absorb concurrent cast and call target processes. `+Q` and
`+e` provide ample port and ETS capacity for the role runtime and counters.

Role containers are started without Docker PID or memory caps and with an
open-file limit of `1048576`.

A `system_limit`, process-table exhaustion, port-table exhaustion, ETS-table
exhaustion, Docker OOM kill, or equivalent limit-driven failure is an invalid
test environment, not a benchmark result. The test suite must be provisioned so
these failures do not occur.

`distributed_tests_utils` owns image preparation, local or remote Docker
startup, peer startup, Erlang distribution readiness, `ecall` startup,
connection readiness, and cleanup.

## Suite initialization and readiness

`init_per_suite/1` reads workload config and calls:

```erlang
[SenderNode, ReceiverNode] =
    distributed_tests_utils:start_nodes(NodeConfigs)
```

`start_nodes/1` completes these gates before returning:

1. Read `env_settings`.
2. Start the sender and receiver Docker-backed peers with the configured VM
   startup values.
3. Ensure that the controller can reach both nodes and that sender and receiver
   have bidirectional Erlang distribution connectivity.
4. Set the `ecall` batch size and run `application:ensure_all_started(ecall)`
   on both role nodes.
5. Establish `ecall` connections from sender to receiver and from receiver to
   sender.
6. Read `connection_info/1` for each direction.

The suite does not call distribution or `ecall` readiness helpers directly.
Bad CT config is allowed to crash whichever step first needs it.

Connection state is read through this API:

```erlang
-spec connection_info(node()) ->
  {ok, #{
    status := connected,
    connection_pid := pid(),
    proxy_count := pos_integer(),
    batch_size := pos_integer()
  }}
  | {error, not_connected}.
```

`connection_info/1` is the supported readiness and diagnostic interface. Test
utilities must not inspect `ecall_connection` records or `persistent_term`
layout.

Readiness is predicate-based. Fixed sleeps are not readiness checks. If an
asynchronous startup step needs waiting, it completes when the expected state
or tagged handshake is observed.

The resulting suite `Config` contains the sender node, receiver node, and
performance settings required by the testcases.

## Common workload contract

### Planned work

One test point creates the selected number of writer processes. Every writer
performs exactly `messages_per_writer` operations. The expected completion
count is:

```text
writer count * messages per writer
```

With the defaults, each writer performs 1000 operations and the writer-count
points are `1K`, `10K`, `100K`, `500K`, and `1M`.

Every point uses a new `RunRef`, fresh targets and counters, and newly spawned
writers. All harness messages are tagged with `RunRef` so a late control message
cannot satisfy a later point. Workload payloads are not wrapped only for
correlation; the fresh target identifies their point. In particular, the tiny
send payload remains the atom `tiny`.

### Pacing

The default pace is one operation per 100 milliseconds per writer.

Pacing is a simple cycle loop. When another cycle will follow, the writer starts
an `erlang:start_timer/3` timer for `pace_ms`, performs one operation, and waits
for that timer's tagged timeout message before entering the next cycle. If the
operation itself takes longer than `pace_ms`, the timeout message is already in
the mailbox and the next cycle starts immediately after the operation.

The loop keeps no scheduling state beyond its current cycle. After the last
configured operation, the writer finishes without starting another cycle
timer.

Calls are sequential, with at most one outstanding call per writer. Native and
`ecall` operations use the same loop.

### Payloads

The payload matrix is hardcoded by the test implementation and is not part of
CT config:

- `tiny`: the atom `tiny`;
- `data`: a term structurally identical to `DATA` in `src/ecall_test.erl`;
- `binary_100kib`: a 102400-byte reference-counted binary;
- `binary_1mib`: a 1048576-byte reference-counted binary.

Payload construction is centralized in `performance_payloads.erl`. A binary is
created once on the sender for a test point and the same binary is shared by
all writers. It is not rebuilt for each writer or operation. Native and
`ecall` points use the same payload constructor and the same target behavior.

### Start barrier and timing

Before a point starts:

- the receiver target and completion counter are installed and ready;
- all writer processes are alive, monitored, initialized, and waiting at the
  start barrier;
- the required native and `ecall` connections are still healthy;
- the sender and receiver role coordinators are monitored by the controller.

Readiness and writer completion are aggregated on the participant nodes. The
controller must not receive one control message per writer.

The sender coordinator records one monotonic start timestamp immediately before
releasing the waiting writers. It records the end when the operation-specific
completion condition is met and all writers have reported completion. Peer
startup, application startup, connection setup, payload construction, and
writer construction are outside the measured interval. Barrier release and all
workload backpressure are inside it and are identical for the two compared
paths.

### Completion

Successful completion is exact, not time-based:

- send points finish after the remote receiver has observed exactly the
  expected number of payload messages and every writer has issued its quota;
- cast points finish after the target function has executed exactly the
  expected number of times and every writer has issued its quota;
- call points finish after every writer has received and validated every reply
  and has completed its quota.

Send and cast acceptance at the sender is not completion. Wrong replies and
control messages with an unexpected run reference fail the point.

The completion proof relies on the operation paths' normal contract that one
issued operation is not duplicated by the transport or `ecall`. Given the exact
writer quotas, a fresh point-local target, and the exact observed count,
per-operation identifiers and a post-completion duplicate-detection window are
unnecessary. The harness does not claim to diagnose a transport that violates
that contract.

The receiver sends one tagged completion acknowledgement to the sender
coordinator for send and cast points using one direct raw `!` control message.
That path is fixed for native and `ecall` points and is not counted as a
workload operation.

### Failure handling

Writer processes and point-local targets or counters are monitored. An
abnormal `DOWN`, unexpected result, wrong count, or lost target fails the active
point immediately. The test code is intentionally crash-forward: unexpected
config or environment problems are allowed to fail at the point where they are
used.

Every workload testcase sets its Common Test timetrap to `infinity`, so Common
Test's implicit timetrap cannot terminate a healthy large point. A point ends
only at its exact completion barrier or upon an observed failure.

## Compared operation paths

### Send suite

The native point sends the configured payload directly to its fresh remote PID
with `!`. The `ecall` point invokes `ecall:send/2` for an equivalently
initialized fresh receiver. Both receivers have identical behavior and
completion accounting.

### Cast suite

The native point invokes one minimal exported target through `erpc:cast/4`.
The `ecall` point invokes the same module, function, and arguments through
`ecall:cast/4`. Completion counts target-function executions, not locally
accepted cast requests.

### Call suite

The native point invokes one minimal exported target through `erpc:call/4`.
The `ecall` point invokes the same module, function, and arguments through
`ecall:call/4`. The target returns the same fixed result for both paths. The
harness normalizes only the APIs' outer return shapes and validates every
result.

## Common Test case hierarchy

Each operation suite exports exactly two test cases:

```text
native_test
ecall_test
```

Both cases run the same payload and writer-count matrix in code:

```text
tiny
  writers_1k
  writers_10k
  writers_100k
  writers_500k
  writers_1m
data
  writers_1k ... writers_1m
binary_100kib
  writers_1k ... writers_1m
binary_1mib
  writers_1k ... writers_1m
```

`native_test` calls `native_test_point(Payload, Writers)` for each point.
`ecall_test` calls `ecall_test_point(Payload, Writers)` for each point. There
is no Common Test group hierarchy and no `ecall` batch-size cycle.

## `ecall` batch-size requirement

The selected batch size is the maximum number of logical operations collected
into one `ecall` proxy batch. A partial batch remains valid when the proxy has
no immediately available request.

The batch size is configured once per suite through `env_settings`:

```erlang
{env_settings, #{
  ecall_batch_size => 1000
}}.
```

`distributed_tests_utils:start_nodes/1` applies it on every participant node
before the `ecall` connections are established. A custom forwarding proxy or
synthetic batching helper is not an acceptable substitute. The measured
operations must call `ecall:send/2`, `ecall:cast/4`, or `ecall:call/4` as
appropriate.

## Point metrics

Every successful point collects memory, distribution, and lock metrics from
both the sender and receiver nodes. Metrics from the two roles remain separate
in the result so that sender-side queueing is not combined with receiver-side
work.

### Collector lifecycle

After the writers and targets have reached the start barrier, the sender
coordinator starts one metrics collector on each participant node and waits
until both collectors are ready. Each collector reads metrics locally; it does
not poll the other participant over the distribution connection under test.

The measured point follows this sequence:

1. Start both metrics collectors.
2. Clear lock counters on both participant nodes.
3. Record the point start time and release the writers.
4. Collect memory and distribution metrics every 100 milliseconds.
5. Stop collection when the point completion condition is met.
6. Collect the lock-counter results and build the point result.

The collector keeps aggregates only. It does not retain or log the complete
sample series, and it does not report sampling-health metrics. There are no
separate setup, ready, completion, or cleanup memory snapshots.

### Memory metrics

Each 100-millisecond sample reads:

```erlang
erlang:memory([total, processes, processes_used])
```

For each value, the point result contains its average and maximum in bytes.
The tests do not read Linux `/proc` memory information.

### Distribution metrics

Each collector finds the distribution controller for the other participant
through `erlang:system_info(dist_ctrl)`. For the ordinary TCP distribution
port, it samples:

```erlang
inet:getstat(Port, [
  send_oct,
  recv_oct,
  send_cnt,
  recv_cnt,
  send_pend
])
```

It also samples the ERTS driver queue through:

```erlang
erlang:port_info(Port, queue_size)
```

The result contains point deltas for `send_oct`, `recv_oct`, `send_cnt`, and
`recv_cnt`. It contains average and maximum values for `send_pend` and the port
queue size. Socket pending bytes and the ERTS driver queue are distinct values;
neither is presented as the internal `DistEntry.qsize`.

The tests do not enable or collect `busy_dist_port` system-monitor events.

### Lock metrics

Participant nodes run an OTP lock-counting emulator. Immediately before the
writers are released, the harness calls `lcnt:clear/1` for both nodes. After
point completion, it calls `lcnt:collect/1` and includes the normal combined
lock statistics in the result.

Lock collection is not restricted to the distribution category and is not
sampled every 100 milliseconds. Statistics are combined by lock class instead
of reporting every individual process lock. Each lock entry contains the
standard lock name, acquisition attempts, collisions, collision percentage,
accumulated wait time, and duration percentage.

## Result logging

There is no reporting layer in this phase. A successful point emits one
structured `ct:pal/2` entry containing:

- suite and operation;
- native or `ecall` path;
- payload profile;
- writer count;
- pace and messages per writer;
- elapsed monotonic time;
- operations per second;
- sender and receiver memory, distribution, and lock metrics.

Expected and completed operation counts remain internal completion invariants.
They are not included in the successful point log entry.

Failed points log the same identifying parameters plus the failure reason.
The implementation does not create auxiliary result files, comparison
artifacts, or a reporting module.

## Isolation and cleanup

After a successful point, all writers must have exited normally, the target and
counter must be stopped, and no operation for its `RunRef` may remain in flight.
The next point starts with fresh point-local processes and state.

After a point failure, point-local cleanup runs before the testcase ends.

`end_per_suite/1` idempotently stops any remaining peers and removes their
local or remote Docker containers. It is safe after an earlier failure path has
already stopped them.

## Acceptance criteria

The suite lifecycle is complete when a smoke configuration proves that:

- suites at `test/performance/` and helpers at `test/performance/util/` compile
  and are loadable on every node;
- local sender and receiver Docker peers start through `peer`;
- CT config can place a role on a remote Docker host;
- `ecall` is started on both nodes;
- Erlang distribution and `ecall` connections are ready before
  `start_nodes/1` returns;
- cleanup removes both role containers after success or failure.

The workload layer is complete when:

- CT config can replace pace, message quota, writer-count list, `ecall` batch
  size, and distribution busy limit without suite code changes;
- the three suites expose only `native_test` and `ecall_test`;
- every native point and every payload/writer `ecall` point uses the real
  operation API;
- every successful send and cast point confirms the exact remote execution
  count;
- every successful call point validates the exact reply count;
- participant or connection failure fails the point instead of producing a
  partial result;
- every successful point contains separate sender and receiver memory,
  distribution, and combined lock metrics;
- results appear only in Common Test logs.

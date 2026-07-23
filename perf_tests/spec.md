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

Lock-counter diagnostics, receiver-work variants, network telemetry, automated
comparison reports, CSV files, and presentation tooling are outside this
implementation phase.

## Implementation layout

All performance-test code lives under `test/performance/`. Suites are directly
under that directory; reusable modules are under `test/performance/util/`.

```text
test/performance/
  util/
    distributed_tests_utils.erl
    performance_load_utils.erl
    performance_payloads.erl
  performance_send_SUITE.erl
  performance_cast_SUITE.erl
  performance_call_SUITE.erl
  performance.config
  test.spec
  Dockerfile
```

Additional helpers belong under `util/`, not beside the suites. The build and
Common Test code paths must compile `util/*.erl` and make all suite and utility
BEAM files available to the controller, sender, and receiver nodes.

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
  writer_counts => [1000, 10000, 100000, 500000, 1000000],
  ecall_batch_sizes => [10, 100, 1000, 10000],
  distribution_busy_limit_kib => 1024
}}.
```

The workload lists are ordered. Suites run writer counts and `ecall` batch sizes
in their configured order. A user can replace either list, change the pace or
message quota, or select individual suites and groups through Common Test.
The harness logs the resolved values and never silently scales them down.

The full default matrix is intentionally very large. Development and smoke
runs should select a subset of groups or use a smaller CT configuration.

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

The suite validates its configuration before starting containers. Pace,
message count, every writer count, every batch size, and busy limit must be
positive integers. Writer-count and batch-size lists must not be empty. For
OTP 27, the busy limit must be in `1..2097151` KiB. Values within each list
must be distinct. The largest writer count must fit the fixed participant
process capacity after role services are started.

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

`+zdbbl` is a startup option measured in KiB. After each peer starts, the
orchestrator must assert that:

```erlang
erlang:system_info(dist_buf_busy_limit)
    =:= DistributionBusyLimitKiB * 1024
```

Changing the busy limit requires a new peer start. A busy-limit experiment is
therefore a new CT invocation or suite run with another config value, not an
in-suite sweep on existing nodes.

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
After role services start, the sender must still have more free process slots
than the largest configured writer count.

Role containers run without Docker PID or memory caps, and their open-file
limit must be at least `1048576`. Before running a workload, orchestration
verifies the effective BEAM process, port, and ETS limits and the container
limits. If the environment cannot supply them, suite initialization fails
before measurement.

A `system_limit`, process-table exhaustion, port-table exhaustion, ETS-table
exhaustion, Docker OOM kill, or equivalent limit-driven failure is an invalid
test environment, not a benchmark result. The test suite must be provisioned so
these failures do not occur.

`distributed_tests_utils` owns image preparation, local or remote Docker
startup, peer startup, node connectivity, and cleanup.

## Suite initialization and readiness

`init_per_suite/1` must complete these gates in order:

1. Begin a new suite-local topology generation and read and validate CT
   configuration.
2. Start the sender and receiver Docker-backed peers with the configured VM
   startup values.
3. Verify that the controller can reach both nodes and that sender and receiver
   have bidirectional Erlang distribution connectivity.
4. Run `application:ensure_all_started(ecall)` successfully on both role nodes.
5. Establish `ecall` connections from sender to receiver and from receiver to
   sender.
6. Verify that both connections have live `ecall` proxy pools and perform a
   tagged probe through each actual proxy path.

The suite must not proceed after merely observing that the Erlang nodes are
connected. It must prove that `ecall` is running and connected in both
directions. A successful call through `ecall` alone is not sufficient proof,
because the current API can fall back to a native operation when no proxy is
registered.

To make connection state and batch selection explicit, the implementation must
extend `ecall_connection` with this API contract:

```erlang
-spec connect(node(), #{batch_size := pos_integer()}) ->
  ok | {error, term()}.

-spec connection_info(node()) ->
  {ok, #{
    status := connected,
    connection_pid := pid(),
    proxy_count := pos_integer(),
    batch_size := pos_integer()
  }}
  | {error, not_connected}.
```

Connection changes are serialized per remote node. `connect/1` ensures that a
connection exists but never replaces a live explicitly configured connection.
`connect/2` is idempotent when the requested batch is already active and
atomically replaces a live connection with a different batch. `disconnect/1`
is idempotent and returns only after the routing entry and supervised connection
are gone. These rules prevent the automatic PG-driven `connect/1` from racing a
test's explicit batch selection.

`connect/1` and `connect/2` return only after the proxy pool is registered and
usable. The `connection_pid` reported by `connection_info/1` owns the
authoritative routing entry and its complete proxy pool. While that PID is
alive, operations for the remote node must use that pool and must not fall back
to native distribution. Loss or replacement of any pool member invalidates the
connection PID.

`connection_info/1` is the supported readiness and diagnostic interface; test
utilities must not inspect `ecall_connection` records or `persistent_term`
layout. The harness monitors `connection_pid`, verifies the expected batch
size and a nonzero proxy count, runs the tagged probe, and checks that the same
connection stayed alive for the point.

Readiness is predicate-based. Fixed sleeps are not readiness checks. If an
asynchronous startup step needs waiting, it completes when the expected state
or tagged handshake is observed.

The resulting suite `Config` contains the sender node, receiver node, and
validated performance settings required by the testcases.

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

Writer processes, role coordinators, targets or counters, peer nodes, and the
active `ecall` connection workers are monitored. An abnormal `DOWN`, `nodedown`,
unexpected result, `ecall` fallback, proxy restart, or lost connection fails
the active point immediately. The failure and resolved point parameters are
logged with `ct:pal/2` before `ct:fail/1`.

The cast target catches an operation exception, sends one tagged failure
message with class, reason, and stack to the receiver coordinator, and then
exits. Call failures are observed through their replies, and send-receiver
failure is observed through its monitor. Limit-driven cast spawn failures are
prevented by the system-capacity requirements above.

Any point failure invalidates the suite topology. The failure path records that
state and its generation in `distributed_tests_utils`, stops point-local
processes and both peers, and then fails the testcase. Each later
`init_per_testcase/2` in that suite observes the invalid generation and returns
a skip reason; it never reconstructs the topology. This is required because
`ct:fail/1` alone would allow Common Test to start the next testcase.

`end_per_testcase/2` checks Common Test's testcase status and performs the same
invalidation for every non-success result. Unexpected exceptions and harness
failures therefore cannot bypass topology shutdown merely because they did not
use the normal point-failure helper.

Every workload testcase sets its Common Test timetrap to `infinity`, so Common
Test's implicit timetrap cannot terminate a healthy large point. A point ends
only at its exact completion barrier or upon an observed failure.

If a participant node dies, the same invalidation path applies. Later points
are not attempted on a partially reconstructed topology.

Invalidation is suite-local. After the failed suite's idempotent cleanup, the
next operation suite may create a new topology generation in its own
`init_per_suite/1`. Starting that generation clears only the completed prior
suite's invalid marker; it never resumes skipped points from the failed suite.

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

The three operation suites use the same matrix. Their native group names are:

| Suite | Native group | Native operation | `ecall` group operation |
|---|---|---|---|
| `performance_send_SUITE` | `raw` | `!` | `ecall:send/2` |
| `performance_cast_SUITE` | `erpc` | `erpc:cast/4` | `ecall:cast/4` |
| `performance_call_SUITE` | `erpc` | `erpc:call/4` | `ecall:call/4` |

With the default writer list, each native group contains this payload and
writer hierarchy:

```text
raw or erpc
  tiny
    writers_1k
    writers_10k
    writers_100k
    writers_500k
    writers_1m
  data
    writers_1k
    writers_10k
    writers_100k
    writers_500k
    writers_1m
  binary_100kib
    writers_1k
    writers_10k
    writers_100k
    writers_500k
    writers_1m
  binary_1mib
    writers_1k
    writers_10k
    writers_100k
    writers_500k
    writers_1m
```

Each `ecall` group contains every configured batch size. With the default
config, the hierarchy is:

```text
ecall
  batch_10
    tiny
      writers_1k ... writers_1m
    data
      writers_1k ... writers_1m
    binary_100kib
      writers_1k ... writers_1m
    binary_1mib
      writers_1k ... writers_1m
  batch_100
    tiny/data/binary_100kib/binary_1mib
      writers_1k ... writers_1m
  batch_1000
    tiny/data/binary_100kib/binary_1mib
      writers_1k ... writers_1m
  batch_10000
    tiny/data/binary_100kib/binary_1mib
      writers_1k ... writers_1m
```

The suites derive writer and batch points from the ordered CT lists. Every
combination is separately logged and selectable. All points belong to a
top-level group with the Common Test `sequence` property. No performance group
may use `parallel`. The explicit invalid-topology check above also prevents
continuation if nested-group failure propagation is insufficient.

This matrix is the complete workload scope for this phase. It has no unpaced
fixed-work case, receiver-work case, hard-coded busy-limit sweep, or
batch-size-1 proxy control.

## `ecall` batch-size requirement

The selected batch size is the maximum number of logical operations collected
into one `ecall` proxy batch. A partial batch remains valid when the proxy has
no immediately available request.

The current production implementation has a compile-time batch size of 1000.
To execute the required matrix honestly, `ecall` must make the maximum batch
size the explicit `connect/2` connection-start setting defined above. Every
configured batch group, including `1000`, uses `connect/2`. Existing callers of
`connect/1` retain the production default of 1000.

Before each `ecall` batch group, the harness must:

1. finish the previous point, if any, exactly, with no requests in flight;
2. stop the existing `ecall` connections in both directions;
3. create fresh connection workers with `connect/2` and the configured batch
   size on both participant nodes;
4. read the effective connection state through `connection_info/1`;
5. monitor both connection PIDs and run the bidirectional proxy probes;
6. verify and log the effective batch size.

A custom forwarding proxy or synthetic batching helper is not an acceptable
substitute. The measured operations must call `ecall:send/2`, `ecall:cast/4`,
or `ecall:call/4` as appropriate.

## Result logging

There is no reporting layer in this phase. A successful point emits one
structured `ct:pal/2` entry containing:

- suite and operation;
- native or `ecall` path;
- configured and effective batch size when applicable;
- payload profile;
- writer count;
- pace and messages per writer;
- expected and completed operations;
- configured and effective distribution busy limit;
- elapsed monotonic time;
- completed operations per second.

Failed points log the same identifying parameters plus the failure reason.
The implementation does not create auxiliary result files, comparison
artifacts, or a reporting module.

## Isolation and cleanup

After a successful point, all writers must have exited normally, the target and
counter must be stopped, and no operation for its `RunRef` may remain in flight.
The next point starts with fresh point-local processes and state.

After a point failure, point-local cleanup and topology invalidation run before
the testcase ends. After an initialization or node failure, any partially
started topology is invalidated immediately.

`end_per_suite/1` idempotently stops any remaining peers and removes their
local or remote Docker containers. It is safe after an earlier failure path has
already stopped them.

## Acceptance criteria

The suite lifecycle is complete when a smoke configuration proves that:

- suites at `test/performance/` and helpers at `test/performance/util/` compile
  and are loadable on every node;
- local sender and receiver Docker peers start through `peer`;
- CT config can place a role on a remote Docker host;
- the configured `+zdbbl` value is effective;
- the required BEAM and container system capacities are effective;
- `ecall` is started on both nodes;
- Erlang distribution and actual `ecall` proxy paths work in both directions;
- monitored role procedures can be run on both peers;
- cleanup removes both role containers after success or failure.

The workload layer is complete when:

- CT config can replace pace, message quota, writer-count list, batch-size
  list, and distribution busy limit without suite code changes;
- the three suites expose the native and `ecall` hierarchy specified above;
- every native point and every batch/payload/writer `ecall` point uses the real
  operation API and the same target behavior;
- every successful send and cast point confirms the exact remote execution
  count;
- every successful call point validates the exact reply count;
- participant or connection failure fails the point instead of producing a
  partial result;
- no point fails because a BEAM, container, or operating-system capacity limit
  was configured too low;
- any point failure skips all remaining points in the current operation suite
  without rebuilding that suite's topology;
- results appear only in Common Test logs.

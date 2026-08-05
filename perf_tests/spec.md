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

Receiver-work variants and CSV files are outside this implementation phase.
Completed points are also stored as JSON and presented by a separate web
application that compares native and `ecall` results from every retained
Common Test run.

## Implementation layout

All performance-test code lives under `test/performance/`. Suites are directly
under that directory; reusable modules are under `test/performance/util/`.

```text
test/performance/
  util/
    distributed_tests_utils.erl
    performance_metrics.erl
    performance_payloads.erl
  performance_send_SUITE.erl
  performance_cast_SUITE.erl
  performance_call_SUITE.erl
  performance.config
  test.spec
  Dockerfile

performance_report/
  package.json
  server/
  src/
```

`distributed_tests_utils` is only responsible for starting and stopping
participating nodes, including node readiness before it returns. Workload
orchestration is kept inside each suite, even where that repeats code. Payload
construction remains centralized in `performance_payloads`. The build and
Common Test code paths must make the suite and utility BEAM files available to
the controller, sender, and receiver nodes.

`perf_tests/spec.md` and `perf_tests/findings.md` are design documents, not
Common Test inputs. The reporting web application is independent of Common
Test execution and reads the result files left under `_build/test/logs`.

The normal entry point remains:

```sh
make performance_tests
```

It compiles the project and invokes Common Test with
`test/performance/test.spec`.

The reporting application uses its own normal Node.js package and build
commands. It does not add a reporting target to the project Makefile.

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
    node => "receiver@distribution.example.net",
    user => "test-user",
    password => "password"
  }
}}.
```

`host` identifies the SSH/Docker host. The optional `node` value is the full
Erlang node name used for distribution and may use a different hostname or IP
address. When `node` is omitted, the harness uses `<role>@<host>`.

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

### Offline controller

An offline controller can use the locally built
`ecall-performance-controller:otp27` image. The controller image contains
Erlang/OTP, Rebar3, Make, SSH, `sshpass`, and the Docker CLI. It uses the host
Docker daemon and the prebuilt `ecall-performance:otp27` image, so it does not
download packages or rebuild images on the controller host.

The offline bundle contains both Docker images. Install and run it with
`test/performance/run_offline_controller.sh`; set
`ECALL_PERFORMANCE_ARTIFACT_DIR` when the bundle and logs are not under
`/home/romanvozfp/ecall_tests`. The runner mounts `performance.config` and
`test.spec` from that artifact directory so both inputs remain editable without
rebuilding the image.

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

Every successful point collects memory and lock metrics from the sender node.

### Collector lifecycle

After the writers and targets have reached the start barrier, the sender
coordinator starts one local metrics collector and waits until it is ready.

The measured point follows this sequence:

1. Start the sender metrics collector.
2. Clear lock counters on the sender node.
3. Record the point start time and release the writers.
4. Collect memory metrics every 100 milliseconds.
5. Stop collection when the point completion condition is met.
6. Collect the lock-counter results and build the point result.

The collector keeps aggregates only. It does not retain or log the complete
sample series, and it does not report sampling-health metrics. There are no
separate setup, ready, completion, or cleanup memory snapshots.

### Memory metrics

Each 100-millisecond sample reads:

```erlang
erlang:memory(total)
```

The point result contains the average and maximum total memory in bytes. The
tests do not read Linux `/proc` memory information.

### Lock metrics

The sender node runs an OTP lock-counting emulator. Immediately before the
writers are released, the collector calls `lcnt:clear/0`. After point
completion, it calls `lcnt:rt_collect/0`.

The lock-counter mask is restricted to the distribution category, which is the
narrowest category supported by OTP for this lock. The result retains only
`dist_entry_out_queue` and combines all of its instances. No other locks are
reported. The result contains accumulated wait time, collision percentage, and
duration percentage. Lock counters are not sampled every 100 milliseconds.

## Result storage and reporting

### Completed-point output

After a point completes successfully, its suite calls:

```erlang
performance_metrics:point(Config, Result)
```

`point/2` reads `distribution_busy_limit_kib` from `env_settings` and the
sender and receiver locations from `role_config`, adds them to the result,
logs the result through `ct:pal/2`, and writes the same result as JSON. A local
role is stored as `local`. A remote role is stored as `user@host`; its password
and the rest of the role map are never logged or stored. It owns the
operation-specific Common Test messages, including:

```erlang
ct:pal("Send performance point completed: ~p", [Result])
```

with equivalent messages for cast and call points. The suites do not log the
successful result separately.

The logged and stored result contains:

- suite and operation;
- native or `ecall` path;
- payload profile;
- writer count;
- pace and messages per writer;
- distribution busy limit in KiB;
- sanitized sender and receiver configuration;
- elapsed monotonic time;
- performance percentage relative to the configured per-writer pace;
- sender memory and lock metrics.

The expected operations per second for one writer is `1000 / pace_ms`.
`performance_percent` is the measured per-writer operations per second divided
by that expected rate and multiplied by 100. A value of `100.0` means the
configured pace was sustained.

Expected and completed operation counts remain internal completion invariants.
They are not included in the successful point log entry.

`point/2` writes one JSON file per completed point under the Common Test
suite's `priv_dir`:

```text
log_private/
  performance_data/
    send.native.tiny.10000.json
    send.ecall.tiny.10000.json
```

The JSON object has `schema_version` set to `1` and otherwise preserves the
result-map structure. Sender and receiver configurations are JSON strings.
OTP's `json:encode/1` performs the encoding. The file is written directly to
its final name; there is no temporary-file protocol. A reader can observe an
incomplete file while the write is in progress. For compatibility, the parser
also accepts existing schema-version-1 files without role configuration and
the frontend presents their roles as `Unavailable`.

A JSON write error fails at the file operation. Failed performance points do
not produce JSON. Their diagnosis remains in the standard Common Test report,
and their missing results are shown as unavailable by the reporting
application.

### Reporting web application

The reporting layer is a conventional small Node.js web application under
`performance_report/`. It has a normal package-manager and frontend build
workflow. The application consists of:

- a Node.js HTTP backend that scans and parses stored point files;
- an API that returns all discovered runs, valid points, and parse errors;
- a built frontend with run navigation, metric grids, and charts.

The backend resolves `_build/test/logs` relative to the project and scans all
matching files below:

```text
ct_run.*/**/log_private/performance_data/*.json
```

Each outer `ct_run.*` directory is one report run. Points from the send, cast,
and call suite directories below it belong to that run. The backend parses the
files on every report-data request. An invalid JSON file is included in the
reported error list and skipped; it is tried again on the next request. This
also handles files observed while Common Test is still writing them.

The backend serves both the built frontend and the existing `_build/test/logs`
tree read-only. Each run section links to its standard Common Test report. The
web application can run while Common Test is producing additional point
files, and the frontend periodically refreshes the report data.

### Grids and trends

The page presents every parsed run; it does not hide runs behind a selector.
A contents section at the top links to each run section. Every run is organized
by operation and payload, and each group header shows messages per writer,
pace, distribution busy limit, sender, and receiver. `writer_count` is the
horizontal variable for both grids and charts.

Grid columns are the writer counts sorted numerically in ascending order. Grid
rows identify the path and metric, for example native performance percentage,
`ecall` performance percentage, native average memory, and `ecall` average
memory.

Charts use a numeric `writer_count` X axis. Native and `ecall` are separate
series. The reported trends cover:

- performance percentage;
- average memory;
- maximum memory;
- lock wait;
- lock collision percentage;
- lock duration percentage.

Average and maximum memory are stored as bytes but converted to decimal
gigabytes (bytes divided by `1,000,000,000`) in both grids and charts.

A series is identified by the run, operation, payload, path, messages per
writer, pace, distribution busy limit, sender, receiver, and metric. Each
series value is a `{writer_count, metric_value}` point. Missing writer counts
and missing native or `ecall` counterparts are gaps or `N/A`; they are never
converted to zero.

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
- every successful point contains sender memory and combined lock metrics;
- every successful point is logged by `performance_metrics:point/2` and saved
  as one JSON file containing `distribution_busy_limit_kib` and sanitized
  sender and receiver configuration, without a password;
- the reporting backend discovers valid point files across all retained Common
  Test runs and reports invalid files without discarding other results;
- one report page links to every parsed run and compares native and `ecall`
  metrics in grids and charts using `writer_count` as the horizontal variable;
- both memory metrics are presented in decimal gigabytes.

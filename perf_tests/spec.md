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

The role image is built on the official `erlang:27.2.2` image. Rebuilding the
role image only copies the current checkout over that base image; it does not
run commands or build OTP from source. The harness runs the stock emulator; no
lock-counting emulator is built or deployed.

The host that rebuilds the role image must already hold `erlang:27.2.2` in its
local Docker image store. The performance hosts have no registry access, and
BuildKit resolves the `FROM` reference before the first layer, so a missing base
image fails the build outright rather than falling back to anything local. The
image is put there once, from a machine that can reach the registry:

```text
docker pull erlang:27.2.2
docker save erlang:27.2.2 | gzip -1 | ssh <user>@<host> docker load
```

`start_nodes/1` checks for the base image before it shells out to `docker build`
and fails the suite with `base_image_missing` when it is absent, so the missing
prerequisite is named directly instead of surfacing as a registry timeout.

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

The controller image selects prebuilt mode through
`ECALL_PERFORMANCE_PREBUILT_IMAGE=true`.

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

Every successful point collects memory, network, and scheduler metrics from the
sender node.

### Collector lifecycle

After the writers and targets have reached the start barrier, the sender
coordinator starts one local metrics collector for the receiver node and waits
until it is ready.

The measured point follows this sequence:

1. Start the sender metrics collector for the receiver node, which enables
   scheduler wall-time measurement on the sender node.
2. Resolve the distribution channel to the receiver and read the baseline
   distribution socket counters and scheduler wall times.
3. Record the point start time and release the writers.
4. Every 100 milliseconds, sample total memory and the total run-queue length.
5. Stop collection when the point completion condition is met.
6. Read the final distribution socket counters and scheduler wall times, and
   build the point result.

The collector keeps aggregates only. It does not retain or log the complete
sample series, and it does not report sampling-health metrics. There are no
separate setup, ready, completion, or cleanup memory snapshots.

### Memory metrics

Each 100-millisecond sample reads:

```erlang
erlang:memory(total)
```

The point result contains the maximum total memory in bytes. The tests do not
read Linux `/proc` memory information.

### Scheduler metrics

Scheduler metrics describe how heavily the sender node's own schedulers are
used and how much work is queued behind them. Both are VM-level values read on
the sender only; the receiver host is not measured.

The collector enables `erlang:system_flag(scheduler_wall_time, true)` when it
starts and reads `erlang:statistics(scheduler_wall_time)` once at
`begin_point` and once at completion. Only the online normal schedulers are
counted, that is the entries whose scheduler identifier is at most
`erlang:system_info(schedulers_online)`; the dirty schedulers that the same
call reports are excluded, because this workload runs no dirty work and their
idle time would only dilute the value. `utilization_percent` is the summed
active-time delta divided by the summed total-time delta, in percent, over the
point. It is `0.0` when the total delta is zero. The flag is enabled per
collector process, so it is on only while a point is measured, and the deltas
never carry work from an earlier point.

`maximum_run_queue_length` is the largest value of
`erlang:statistics(total_run_queue_lengths)` observed on the 100-millisecond
sample loop. It counts the processes and ports that are ready to run and
waiting for a scheduler; entities currently executing are not in a run queue.
It is a maximum rather than a mean, because a queue that builds up at any point
in the run is what shows the sender is scheduler-bound. The cheaper
`total_run_queue_lengths` is read rather than `run_queue`: it needs no
thread-progress synchronization, and the sampled maximum does not need the
exactness the more expensive call buys.

The harness measures no host-level load. The Linux load average is an
exponentially weighted moving average whose window is far longer than one point,
and points run consecutively without a cooldown, so a value observed during a
point still carries most of the preceding ones; it cannot separate consecutive
points of different paths. Host CPU utilization is not measured either: a
whole-host busy percentage normalized over all cores is not comparable with the
per-core percentages `top` reports, saturates at 100 percent once the host is
oversubscribed, and includes every other process on the host. Scheduler
utilization is the sender node's own figure and carries neither problem.

### Network metrics

Network metrics describe the single Erlang distribution channel from the sender
to the receiver node. Native and `ecall` points share that one channel, so the
metrics are directly comparable: batching should reduce the number of socket
writes, not the payload byte volume.

The collector resolves the channel once, at `begin_point`, from
`erlang:system_info(dist_ctrl)`, selecting the controlling entity for the
receiver node. On the default `inet_tcp_dist` transport used by the harness this
is the `tcp_inet` distribution port. If the receiver is not connected, resolving
the port fails and the point fails, consistent with the crash-forward policy.
Metrics are collected on the sender node only, alongside the memory and
scheduler metrics.

Cumulative socket counters are read once at `begin_point` and once at
completion and reported as the completion-minus-baseline delta. The connection
is long-lived and reused across points, so only the per-point delta is
meaningful; the counters are not sampled every 100 milliseconds:

- `send_octets`: bytes written to the distribution socket during the point,
  from `inet:getstat(DistPort, [send_oct])`;
- `average_packet_bytes`: `send_octets` divided by the socket writes during the
  point, from `inet:getstat(DistPort, [send_cnt])`, or `0.0` when there were no
  writes. This is the primary native-versus-`ecall` differentiator, because
  batching collapses many logical operations into one top-level distribution
  signal, raising the average packet size. The write count itself is an
  intermediate quantity and is not reported.

These two deltas are the whole of the network result. The harness measures no
distribution backpressure gauge, because neither of the two that OTP exposes on
this path carries usable magnitude. The port-driver queue behind
`inet:getstat(DistPort, [send_pend])` is clamped by the `inet` driver at its
8-kilobyte busy watermark, so it saturates instead of scaling. Sender
suspensions counted through `erlang:system_monitor(Collector, [busy_dist_port])`
are governed by `distribution_busy_limit_kib`, which the harness sets high
enough to take the limit out of the measurement; the count then reports how
often that limit was nevertheless reached rather than how deep the backlog grew.
The queue that does grow, the ERTS distribution output queue, has no BIF that
exposes its size, and it is visible only as sender memory.

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
- sender memory, network, and scheduler metrics.

The result carries no pace-relative percentage. The configured pace bounds the
offered rate, so such a percentage saturates at `100.0` for every point that
keeps up and reports how far a point fell behind its own throttle rather than
what the path achieved. Throughput, in messages per second, is the headline
figure instead, and `pace_ms` remains in the result as the workload parameter
that produced it.

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

The JSON object has `schema_version` set to `7` and otherwise preserves the
result-map structure. Sender and receiver configurations are JSON strings.
OTP's `json:encode/1` performs the encoding. The file is written directly to
its final name; there is no temporary-file protocol. A reader can observe an
incomplete file while the write is in progress. For compatibility, the parser
also accepts existing files of every earlier schema version: version 1 without
role configuration or network metrics, and versions 1 to 6 without scheduler
metrics. Earlier versions also carried metrics that are no longer produced,
among them a version 3 CPU-utilization block, the lock block of versions 1 to 6,
the load block of versions 4 to 6, and the `performance_percent` field of
versions 1 to 6; the parser requires only the metrics the report presents and
ignores the rest wherever it appears. The frontend presents missing roles as
`Unavailable` and missing metrics as `N/A`.

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
rows identify the path and metric, for example native throughput, `ecall`
throughput, native maximum memory, and `ecall` maximum memory.

Charts use a numeric `writer_count` X axis. Native and `ecall` are separate
series. The reported trends cover:

- throughput, in messages per second;
- elapsed time;
- maximum memory;
- sender scheduler utilization;
- sender maximum run-queue length;
- network, the distribution byte rate (`send_octets` divided by the elapsed
  seconds);
- average distribution packet size.

Throughput is the report's primary trend and its first grid row. Every metric in
the point result appears in the report, either as a trend of its own or as an
input to a derived one; the harness does not collect metrics it does not
present. Two trends are derived rather than stored. Throughput is `writer_count`
multiplied by `messages_per_writer` and divided by the elapsed seconds, in
messages per second; a point completes only when every planned operation is
confirmed, so the planned count is also the completed one. The network trend is
`send_octets` divided by the elapsed seconds, in decimal megabytes per second.

The network and average-packet trends are absent for schema-version-1 points,
and the scheduler trends are absent for schema versions 1 through 6. They are
shown as gaps or `N/A`; they are never converted to zero.

Maximum memory is stored as bytes but converted to decimal gigabytes (bytes
divided by `1,000,000,000`) in both grids and charts. Elapsed time is stored as
`elapsed_ms` but presented in seconds in the same way.

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
- every successful point contains sender memory, scheduler, and distribution
  channel network metrics;
- every successful point is logged by `performance_metrics:point/2` and saved
  as one JSON file containing `distribution_busy_limit_kib` and sanitized
  sender and receiver configuration, without a password;
- the reporting backend discovers valid point files across all retained Common
  Test runs and reports invalid files without discarding other results;
- one report page links to every parsed run and compares native and `ecall`
  metrics in grids and charts using `writer_count` as the horizontal variable;
- maximum memory is presented in decimal gigabytes and elapsed time in
  seconds.

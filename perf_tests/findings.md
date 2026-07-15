# Findings

## OTP distribution path

- `RemotePid ! Message` enters the C function `erl_send()`, resolves the remote `DistEntry`,
  prepares a distributed signal, encodes it, and enqueues it for the
  distribution port. [`bif.c:2109–2194`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/bif.c:2109)
- Standard OTP distribution has one `DistEntry`, output queue, scheduled port
  task, and connection handler per remote node. Different destination PIDs on
  the same node do not create independent distribution channels.
  [`erl_node_tables.h:128–170`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_node_tables.h:128)
- Each `DistEntry` has an `rwmtx` protecting connection state and an exclusive
  `qlock` protecting queue flags and `out_queue`.
  [`erl_node_tables.h:133–157`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_node_tables.h:133)
- A connected ordinary remote send takes the `DistEntry` read lock during
  preparation, releases it, encodes the signal, takes the read lock again, and
  takes `qlock` to update queue sizes and append output buffers.
  [`dist.c:3085–3174`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3085),
  [`dist.c:3458–3594`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3458)
- Signal size calculation, allocation, and external-term encoding occur before
  the enqueue `qlock`; these costs are paid per unbatched signal.
  [`dist.c:3208–3457`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3208)
- `qlock` is shared by all ordinary senders to the same remote node even below
  the distribution busy limit.
  [`dist.c:3510–3547`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3510)

## Distribution drain and locking

- The distribution port task runs with the port lock held and takes `qlock` to
  detach `out_queue`; detachment is pointer reassignment, not traversal of all
  queued buffers. It releases `qlock` before finalizing headers and writing to
  the port. New sends can fill a new `out_queue` while the detached queue is
  drained. [`dist.c:3844–4003`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3844)
- After draining detached buffers, the port task takes `qlock` again to subtract
  drained bytes and possibly clear the distribution busy flag.
  [`dist.c:4019–4041`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:4019)
- On a standard Linux OTP 27 build, `erts_mtx_t` delegates to a default
  `pthread_mutex_t`; OTP requests no FIFO/fairness policy.
  [`ethread.h:140–149`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/include/internal/ethread.h:140),
  [`ethr_mutex.c:1260–1280`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/lib_src/common/ethr_mutex.c:1260)
- `qlock` acquisition order is therefore not guaranteed to follow request
  order. The port task can acquire it between sender acquisitions and can also
  lose repeated acquisition races under sustained contention.
- Erlang processes are not individually queued on the native mutex. Only
  currently executing native threads, normally schedulers executing a sender
  or the port task, can call or wait in `pthread_mutex_lock`; other runnable
  Erlang processes remain on BEAM run queues.
- A scheduler thread blocked on `qlock` cannot execute other Erlang processes
  until it acquires and releases the lock. High contention also causes mutex
  cache-line migration between CPUs.

## Distribution flow control

- The default distribution buffer busy limit is 1 MiB; `+zdbbl` changes it.
  [`dist.h:199–200`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.h:199),
  [`erl_init.c:2318–2327`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_init.c:2318)
- The 1 MiB value is a buffered-output flow-control threshold, not TCP channel
  capacity or sustainable throughput.
  [`erl_cmd.md:1379–1390`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/doc/references/erl_cmd.md:1379)
- `config/vm.args` has `+zdbbl 8192` commented out; the effective runtime limit
  is assumed to be 1 MiB unless another startup argument overrides it.
  [`config/vm.args:51`](/home/roman/PROJECTS/SOURCES/ecall/config/vm.args:51)
- When enqueueing makes `qsize >= busy_limit`, OTP sets
  `ERTS_DE_QFLG_BUSY`. The signal that crosses the threshold can already be in
  `out_queue` before its sender is suspended.
  [`dist.c:3510–3557`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3510)
- A normal sender observing the busy state releases `qlock`, allocates an
  `ErtsProcList` entry, takes its own process `STATUS` lock to suspend, takes
  `qlock` a second time, and appends itself to `DistEntry.suspended` if the busy
  state remains. [`dist.c:3549–3606`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3549),
  [`erl_process.c:9248–9276`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_process.c:9248)
- If only part of a fragmented signal can be queued, OTP retains a send
  continuation and yields. [`dist.c:3607–3639`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3607),
  [`bif.c:2177–2190`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/bif.c:2177)
- When the underlying port is not busy and `qsize < busy_limit`, the port task
  clears `BUSY`, detaches the entire suspended list under `qlock`, and then
  resumes every listed process outside `qlock`. Each resume performs a process
  lookup/status-lock operation, scheduler-state update, run-queue enqueue, and
  list-entry destruction. [`dist.c:279–295`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:279),
  [`dist.c:4026–4038`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:4026),
  [`erl_process.c:9291–9318`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_process.c:9291)
- Busy is set at `qsize >= limit` and can be cleared at `qsize < limit`; no
  separate lower watermark is present in this path.
- `busy_dist_port` system-monitor events expose blocked distribution sends.
  [`erlang.erl:6055–6063`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/preloaded/src/erlang.erl:6055)
- `erlang:send(To, Msg, [nosuspend])` can report that a send would suspend.
  `async_dist = true` bypasses suspension but retains the shared queue and
  `qlock`; absent application flow control it permits unbounded buffering.
  [`dist.c:3143–3153`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:3143),
  [`erlang.erl:7381–7420`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/preloaded/src/erlang.erl:7381)

## Receiver and operation costs

- Incoming distribution data is handled through one input connection/port per
  node connection before signals are routed to receiver processes.
  [`dist.c:1994–2530`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/dist.c:1994)
- Each unbatched distributed message requires message allocation and receiver
  signal-queue insertion; the fallback path takes the receiver `MSGQ` lock.
  OTP can use outer signal-queue buffers when the sender identity permits it.
  [`erl_message.c:276–370`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_message.c:276)
- Distributed payloads are lazily decoded by the receiving process.
  [`erl_proc_sig_queue.c:5089–5183`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_proc_sig_queue.c:5089)
- `{Name, Node} ! Msg` adds a sender-side distribution-table read lock and a
  receiver-side registered-name table read lock. A remote PID avoids these
  lookups but not the per-node `DistEntry` locks.
  [`erl_node_tables.c:284–310`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/erl_node_tables.c:284),
  [`register.c:274–303`](/home/roman/DISTR/ERLANG/otp_src_27.0/erts/emulator/beam/register.c:274)
- `erpc:cast/4` sends a distributed spawn request; each cast adds remote process
  creation and scheduling costs beyond a plain message send.
  [`erpc.erl:1231–1237`](/home/roman/DISTR/ERLANG/otp_src_27.0/lib/kernel/src/erpc.erl:1231)

## `ecall` behavior

- `ecall_connection:send/2` and `cast/4` route through a local proxy when a
  connection exists. [`ecall_connection.erl:33–49`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_connection.erl:33)
- The local proxy count equals the remote worker count; callers are deterministically
  sharded by caller PID. [`ecall_connection.erl:121–126`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_connection.erl:121),
  [`ecall_connection.erl:197–207`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_connection.erl:197)
- Proxies batch up to 1,000 requests and send one top-level distributed batch
  signal. [`ecall.hrl:15–16`](/home/roman/PROJECTS/SOURCES/ecall/include/ecall.hrl:15),
  [`ecall_connection.erl:159–175`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_connection.erl:159)
- Remote worker count is `erlang:system_info(logical_processors)`. Each worker
  handles one batch signal and spawns one process per cast request; batching
  does not reduce the number of executed cast functions.
  [`ecall_receive.erl:27–38`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_receive.erl:27),
  [`ecall_receive.erl:50–93`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_receive.erl:50)
- Proxies do not create multiple OTP distribution connections. They limit the
  number of Erlang processes entering the shared distribution path, reduce
  top-level signal count through batching, and spread receiver work among
  remote workers.
- Production `ecall` casts return after enqueueing into a local proxy mailbox;
  they do not provide end-to-end completion or overload backpressure.
  [`ecall_connection.erl:42–49`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_connection.erl:42)

## Existing benchmark limitations

- `simple_send/3` uses `{Name, Node}`, one registered receiver, and 100 sends
  back-to-back per client followed by a 100 ms sleep. At 100,000 clients this is
  a 10,000,000-send burst per cycle, not one send per client per second.
  [`ecall_test.erl:16`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_test.erl:16),
  [`ecall_test.erl:108–131`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_test.erl:108)
- `pool_send/3` changes direct distribution contenders to 16, batches up to
  1,000 payloads, waits for a proxy acknowledgement before each client proceeds,
  and targets 16 registered receiver processes.
  [`ecall_test.erl:136–185`](/home/roman/PROJECTS/SOURCES/ecall/src/ecall_test.erl:136)
- The direct/pool comparison simultaneously changes sender contention, signal
  count, client pacing, receiver count, receiver mailbox contention, registered
  name lookups, and per-signal encoding/allocation. It does not isolate one
  cause.

## Lock-counter observations

- A bounded loopback diagnostic used an installed OTP 27 lock-counting emulator
  (`erts-15.2.2`); the reviewed tree is base OTP 27.0. Results are relative
  mechanism evidence, not two-machine capacity measurements.
- Direct case: 250 remote-PID senders × 100 sends with a 4 KiB ref-counted
  binary produced 25,000 messages in 169 ms;
  `dist_entry_out_queue` recorded 51,545 attempts, 48,326 collisions (93.8%),
  and 339 ms cumulative wait.
- Proxy-only case: 16 forwarding proxies, no batching, produced 25,600 messages
  in 171 ms; the lock recorded 30,689 attempts, 24,752 collisions (80.7%), and
  61 ms cumulative wait.
- Proxy-plus-batching case: 16 proxies with batch size 100 produced 25,600
  logical messages in 58 ms using 256 distributed batch signals; the lock
  recorded 2,991 attempts, 561 collisions (18.8%), and 0.6 ms cumulative wait.
- Restricting direct ERTS distribution writers reduced cumulative queue-lock
  wait without batching; batching reduced lock acquisitions, collisions, and
  per-signal overhead further.

# Assumptions and hypotheses

- The observed superlinear or apparently exponential collapse is not a literal
  exponential operation in the inspected send path; queue append is constant
  time and suspended-list resumption is linear in the number resumed.
- The primary collapse hypothesis is interaction among `qlock` contention,
  single-channel service capacity, the 1 MiB busy limit, and repeated bulk
  suspend/resume waves. A resumed wave can refill the queue, contend on the same
  mutex/cache line, and suspend again.
- Queueing delay rises sharply as offered load approaches or exceeds the one
  connection's service rate; encoding, allocation, network, receiver dispatch,
  decoding, and remote process creation lower that effective service rate.
- Synchronized periodic senders can become phase-aligned by backpressure and
  amplify burst size and suspend/resume oscillation.
- Under sustained contention, sender acquisitions may delay the port task's
  first `qlock` acquisition, allowing `out_queue` to grow, or its second
  acquisition, delaying `qsize` reduction and clearing of `BUSY`.
- The sender pool's independent benefit is fewer concurrent ERTS distribution
  writers and a smaller potential suspension herd. Its batching benefit is fewer
  distribution signals, queue interactions, encodings, allocations, and receiver
  dispatches.
- If offered load exceeds sustainable capacity, `ecall` moves backlog from the
  ERTS distribution queue and suspended callers into proxy mailboxes; memory can
  continue growing because casts have no end-to-end admission control.
- Actual contribution of each mechanism in the two-machine environment remains
  unconfirmed because the existing benchmark changes several variables at once
  and the lock-counter diagnostic was loopback-only.

# ETS Lock Contention Reproduction

Date: 2026-08-19

## Production Pattern

The production case had roughly 50,000 Erlang processes sharing one ETS table.
The table had about 100,000 keys. A typical object shape was:

```erlang
{{{Integer, Integer}, data}, #{
    <<"field1">> => Value,
    <<"field2">> => Value,
    <<"field3">> => Value
}}
```

Each worker woke up once per second, performed one lookup/update/write cycle,
and then slept only for the remainder of that second. The workers were not
synchronized. The ETS table was configured with:

```erlang
{read_concurrency, false},
{write_concurrency, false}
```

The production symptom was unusual from the OS view: `top` showed BEAM CPU much
lower than in lighter-load cases, while `msacc` showed schedulers almost fully
busy. Native stack inspection showed most VM threads waiting on locks.

## Reproduction

The reproduction code was kept outside the main workspace during testing:

```text
/home/roman/WORKTEMP/202608/ecall/ets
```

Remote testing was run on:

```text
romanvozfp@10.225.2.21:/home/romanvozfp/ecall_tests/ets
```

The benchmark module is `ets_lock_repro.erl`. It creates one named public ETS
`set` with 100,000 rows and starts N worker processes. Each worker chooses a
random key and random field, does:

```erlang
ets:lookup(Table, Key),
maps:put(Field, NewValue, Data),
ets:insert(Table, {Key, NewData})
```

and then waits for the remainder of the one-second cycle.

The worker start was staggered by random initial delays to avoid synchronized
wakeups. A VM-level lock file was added to prevent accidentally running two
benchmark BEAM instances at the same time.

## Load Thresholds

Local clean runs without `lcnt` were useful as a contrast, but they did not
reproduce the production signature. On the local host, high scheduler busy was
accompanied by high OS CPU usage, so this was only evidence that ETS contention
increases with the worker count, not evidence of the low-CPU/high-scheduler-busy
failure mode.

Local results:

```text
50k workers:  avg msacc busy 8.18%,  max 37.45%
90k workers:  avg msacc busy 66.70%, max 73.47%
130k workers: avg msacc busy 87.66%, max 92.79%
170k workers: avg msacc busy 93.85%, max 99.18%
```

On the remote 48-scheduler host, the production pattern reproduced at 20,000
workers. A 10,000-worker run completed normally, while 20,000 workers produced
the stuck shape with low host CPU, full scheduler busy, and a large run queue.

Remote 10k:

```text
samples=30
avg msacc busy=8.88%
max msacc busy=40.02%
avg run_queue=1
max run_queue=6
avg top CPU=228.71%
max top CPU=506.70%
```

Remote 20k:

```text
sample 1: busy=24.89%, run_queue=10125
sample 2: busy=100.00%, emulator=99.69%, run_queue=9971
sample 3: busy=100.00%, emulator=99.86%, run_queue=10066
```

The sample cadence became badly delayed. For example, sample 2 and sample 3
were separated by roughly 160 seconds in one run. This delay is itself part of
the symptom: the observer process and ordinary Erlang IO were starved by the
contention.

## CPU vs Scheduler Busy

At remote 20k, the VM reported full scheduler utilization via `msacc`, but
host-level CPU stayed low for a 48-scheduler BEAM:

```text
top examples: about 113% to 273% CPU
msacc:       100% scheduler busy, almost all emulator time
run_queue:   about 10,000 runnable Erlang processes
```

This reproduces the production observation: schedulers are busy in emulator
work, but the OS sees many native threads sleeping/waking on futexes rather
than consuming CPU continuously.

## Erlang-Side Diagnostics

Under the 20k remote load, diagnostic snapshots repeatedly showed:

```text
controller={cf={io,execute_request,3} status=waiting}
workers={{{ets_lock_repro,worker_loop,3},running/waiting}}
run_queue ~= 10,000
```

So the workers were in the intended hot path, while the controller and logging
were often delayed in Erlang IO.

## Native Thread Evidence

Remote process inspection with `/proc` showed most scheduler threads blocked in
the kernel futex wait path:

```text
erts_sched_N -> futex_wait_queue_me
```

Representative thread summary:

```text
105 threads in futex_wait_queue_me
48 scheduler threads each using only about 1.9-2.1% CPU
```

Plain `strace` attach was blocked by ptrace settings, so `sudo strace -k` was
used for a short sample.

Relevant raw files:

```text
remote: /home/romanvozfp/ecall_tests/ets/logs/where_vm_stuck_20000_20260819_150319.log
remote: /home/romanvozfp/ecall_tests/ets/logs/strace_futex_stack_20000_20260819_150600.log
```

The `strace -k` futex sample showed:

```text
3613 FUTEX_WAKE_PRIVATE
3155 FUTEX_WAIT_PRIVATE
```

Top stack symbols were dominated by ETS access paths:

```text
ets_insert_2
db_get_table_aux
ets_lookup_2
pthread_mutex_lock / pthread_mutex_unlock
__lll_lock_wait_private / __lll_lock_wake_private
```

Address decoding mapped the important frames to:

```text
db_get_table        /usr/src/otp_src_27.2.3/erts/emulator/beam/erl_db.c:875
erts_rwmtx_rwlock   /usr/src/otp_src_27.2.3/erts/emulator/beam/erl_threads.h:2082
erts_rwmtx_rwunlock /usr/src/otp_src_27.2.3/erts/emulator/beam/erl_threads.h:2097
db_get_table_aux    /usr/src/otp_src_27.2.3/erts/emulator/beam/erl_db.c:835
ets_lookup_2        /usr/src/otp_src_27.2.3/erts/emulator/beam/erl_db.c:2807
```

The unnamed lock frames decoded to Erlang's pthread event/rwmutex machinery:

```text
ethr_event.c
ethr_mutex.c
ethr_leave_ts_event
ethr_event_set
event_wait
rwmutex_unlock_wake
```

This matches the production gdb observation: the VM threads were mostly waiting,
claiming, and releasing locks.

## lcnt Evidence

The installed remote Erlang exposed the `lcnt` module, but
`erlang:system_info(lock_counting)` was `false`, so normal BEAM could not collect
lock counters. The test was repeated remotely inside the OTP 27 Docker image
with:

```text
erl -emu_type lcnt
```

The lcnt mask was narrowed to:

```erlang
[scheduler, generic, db]
```

This kept raw terms small and focused on ETS/table locks.

Remote lcnt run:

```text
container image: ecall-performance-controller:otp27
workers: 20000
duration requested: 300000 ms
```

Important files:

```text
remote: /home/romanvozfp/ecall_tests/ets/logs/erl_lcnt_20000_20260819_152346.log
remote: /home/romanvozfp/ecall_tests/ets/logs/lcnt_10_1787135058134_summary.log
remote: /home/romanvozfp/ecall_tests/ets/logs/lcnt_10_1787135058134_sample_1.term
remote: /home/romanvozfp/ecall_tests/ets/logs/lcnt_10_1787135058134_sample_2.term
remote: /home/romanvozfp/ecall_tests/ets/logs/lcnt_10_1787135058134_sample_3.term
```

The dominant lock was unambiguous:

```text
db_tab/ets_lock_repro_table/rw_mutex
```

lcnt sample 1:

```text
tries=707423
collisions=704087
collision ratio=99.5284%
wait_time_us=1524423088
```

lcnt sample 2:

```text
tries=4127571
collisions=4124218
collision ratio=99.9188%
wait_time_us=8997452465
```

lcnt sample 3:

```text
tries=5979681
collisions=5976321
collision ratio=99.9438%
wait_time_us=12974247537
```

The next locks in the lcnt output were run queue mutexes, but their collision
counts and wait times were tiny compared with the ETS table rw mutex.

## lcnt Runtime Impact

`lcnt` itself adds overhead, and under this pathological contention the collect
operation was delayed:

```text
lcnt sample 1 collect elapsed_ms=18968
lcnt sample 2 collect elapsed_ms=1439
```

Even with that overhead, the result is useful because the top contended lock is
not a marginal signal. The ETS table rw mutex accounts for essentially all
collision wait time in the measured lock categories.


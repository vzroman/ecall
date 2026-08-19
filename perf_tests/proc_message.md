# Shared Target Message Queue Contention

Date: 2026-08-19

## Assumption

After reproducing the ETS lock contention case, the next assumption was that a
large number of Erlang processes sending messages to one shared target can also
stall the VM. The suspected hot lock is the target process signal/message queue
lock acquired while senders enqueue messages.

The intended workload was:

```text
many worker processes
one shared target process
each worker wakes once per second
each worker sends one tagged message to the shared target
each worker sleeps only for the remaining part of the one-second cycle
workers are not synchronized
```

The target process drains continuously. It is not deliberately paused; the test
is about contention while many senders enqueue to one target, not about a
receiver that is intentionally blocked.

## Test Code

The benchmark was prepared outside the main workspace:

```text
/home/roman/WORKTEMP/202608/ecall/proc_messages
```

Remote testing was run on:

```text
romanvozfp@10.225.2.21:/home/romanvozfp/ecall_tests/proc_messages
```

The main module is:

```text
proc_msg_repro.erl
```

The target owns the message protocol and receives tagged messages:

```erlang
{proc_msg_repro_message, From, WorkerNo, Seq, Payload}
```

Payload shape was intentionally similar to the ETS test payload:

```erlang
#{
    <<"field1">> => Value1,
    <<"field2">> => Value2,
    <<"field3">> => Value1 + Value2
}
```

The support scripts are:

```text
run_remote_sweep.sh
run_lcnt_docker_remote.sh
find_threshold_remote.sh
```

The Erlang benchmark logs `msacc` summaries, run queue, process count, target
message queue length, target message count, selected worker states, and top CPU
samples.

## Clean Runs

Initial remote runs at one message per second per worker did not reproduce the
stall up to 100,000 workers.

20,000 workers:

```text
avg msacc busy=2.12%
max msacc busy=8.02%
avg run_queue=0.77
max run_queue=2
target_count ~= 6.0M messages over the run
```

60,000 workers:

```text
avg msacc busy=5.36%
max msacc busy=18.43%
avg run_queue=1.80
max run_queue=26
target message queue mostly 0-53 in diagnostics
target_count ~= 18.0M messages over the run
```

100,000 workers:

```text
avg top CPU ~= 497%
target message queue stayed tiny, 0-98 in diagnostics
run_queue was usually 0, with small transient spikes
target_count ~= 30.0M messages over the run
```

The 100k run was still a normal high-load case. CPU rose with load, the target
continued draining, and the VM did not show the low-CPU/futex-stall shape.

## First Bad Count

The threshold search requested was:

```text
start: 200000 workers
step:  100000 workers
```

The first tested count, 200,000 workers, already reproduced the bad shape. Since
100,000 workers completed cleanly and 200,000 workers stalled, the actual
threshold for this host and workload is somewhere between 100,000 and 200,000
workers.

Remote threshold run:

```text
master log: logs/threshold_20260819_165823.log
workers:    200000
status:     stalled
```

Important files:

```text
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/erl_threshold_200000_20260819_165823.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/top_threshold_200000_20260819_165823.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/wchan_msg_200000_20260819_165823.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_msg_200000_20260819_165823_sudo.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_stack_msg_200000_20260819_165823.summary
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_stack_msg_200000_20260819_165823.raw
```

The 200k run did produce the first few `msacc` samples, but the sample cadence
was badly delayed and the system moved into the same lock-heavy state seen in
the ETS case.

Representative Erlang log excerpt:

```text
16:59:33 sample=1 stats_elapsed_ms=5
16:59:39 diag=3 target={cf={proc_msg_repro,target_loop,1} status=running mql=82119}
         target_count=timeout
         workers={{{proc_msg_repro,worker_loop,5},running} => 9,
                  {{proc_msg_repro,worker_loop,5},waiting} => 1}
         run_queue=187592
17:00:50 sample=1 schedulers=48 busy=9.65 busy_max=13.00 run_queue=187793
17:01:33 sample=2 schedulers=48 busy=8.80 busy_max=9.12 run_queue=3651
17:01:43 sample=3 schedulers=48 busy=78.04 busy_max=84.51 run_queue=398
```

Key observations:

```text
target mailbox reached 82119 messages
target_count request timed out
run_queue spiked to 187793
sample 1 summary was delayed by about 77 seconds after stats collection started
```

Top CPU after the initial burst fell into the low range for a 48-scheduler VM:

```text
16:59:24 cpu=2187%   initial burst
16:59:44 cpu=86.7%
16:59:54 cpu=86.7%
17:00:04 cpu=86.7%
17:00:14 cpu=80.0%
17:00:24 cpu=86.7%
17:00:35 cpu=86.7%
17:00:45 cpu=86.7%
17:00:55 cpu=80.0%
17:01:05 cpu=86.7%
17:01:15 cpu=86.7%
```

The threshold runner summary for 200k was:

```text
top_cpu_avg_max_n="205.66 2187.00 22"
busy_avg_max_n="32.16 78.04 3"
runq_avg_max="63947.33 187793"
```

The average top CPU includes the initial spawn/start burst. The important shape
after the burst is low CPU together with a very large run queue and delayed
observer progress.

## 200k Rerun With More Samples

The 200k test was repeated with a longer requested duration to collect more
`msacc` samples and check whether scheduler busy eventually converges to the ETS
pattern.

Important files:

```text
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/erl_remote_200000_20260819_171911.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/top_remote_200000_20260819_171911.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/remote_sweep_20260819_171911.log
```

The run was stopped after three completed `msacc` samples. All three samples
showed low scheduler busy while the run queue stayed extremely high:

```text
17:21:58 sample=1 busy=2.43 busy_max=3.28 run_queue=187715
17:26:38 sample=2 busy=1.53 busy_max=1.55 run_queue=189301
17:32:03 sample=3 busy=1.54 busy_max=1.70 run_queue=188004
```

The sample cadence was badly delayed:

```text
17:20:21 sample=1 stats_elapsed_ms=5
17:21:58 sample=1 summary
17:24:58 sample=2 stats_elapsed_ms=6
17:26:38 sample=2 summary
17:30:36 sample=3 stats_elapsed_ms=5
17:32:03 sample=3 summary
```

The target mailbox kept growing, and the target count request usually timed out:

```text
17:20:27 target mql=160225  run_queue=187667 target_count=timeout
17:22:59 target mql=1889175 run_queue=188490 target_count=timeout
17:24:31 target mql=2986527 run_queue=188450 target_count=timeout
17:26:21 target mql=4263300 run_queue=187869 target_count=timeout
17:28:13 target mql=5560755 run_queue=187969 target_count=timeout
17:29:53 target mql=6714987 run_queue=187774 target_count=timeout
17:31:31 target mql=7853421 run_queue=187732 target_count=481302
17:33:08 target mql=8963244 run_queue=187774 target_count=timeout
```

After the initial burst, top CPU stayed low for a 48-scheduler VM, usually
around 70-90%:

```text
17:20:12 cpu=2473% initial burst
17:20:22 cpu=86.7%
17:21:13 cpu=66.7%
17:24:46 cpu=66.7%
17:30:32 cpu=73.3%
17:34:25 cpu=73.3%
```

This rerun is important for interpretation: at 200k, the message-send test
reproduces a severe VM stall and native process/message queue lock contention,
but the collected `msacc` samples do not reproduce the ETS-style
`low top CPU + nearly 100% scheduler busy` signature. In this message-send
case, the observed signature is low top CPU, very large run queue, delayed
observer progress, growing target mailbox, and low `msacc` scheduler busy.

## Native Thread Evidence

At 200k, `/proc` thread sampling showed nearly all BEAM threads waiting in the
kernel futex path:

```text
104 futex_wait_queue_me
4   0
1   pipe_read
1   ep_poll
1   do_select
1   do_poll.constprop.0
```

Representative scheduler thread states:

```text
erts_sched_N -> futex_wait_queue_me
```

The scheduler threads each showed only a few percent CPU or less while waiting
and waking on futexes.

## strace Evidence

Short `sudo strace` futex summary for 200k:

```text
67768 futex calls in 20 seconds
21382 errors/retries
100% of traced syscall time in futex
```

Stack-enabled strace showed the message send path directly:

```text
send_2
do_send
erts_send_message
queue_messages.part.0
```

The stack sample also showed process signal queue lock operations:

```text
erts_proc_sig_queue_lock
erts_proc_lock_failed
erts_proc_unlock_failed
```

So the contended native lock path is not an unrelated runtime artifact. It is
the sender path trying to queue messages/signals to the shared target process.

## 1,000,000 Worker Confirmation

A single 1,000,000-worker run was also executed before the threshold search.
It reproduced the same symptom more strongly:

```text
BEAM RSS ~= 7 GiB
top CPU after activation ~= 80-150%
Erlang log stopped before the first msacc sample
105 BEAM threads in futex_wait_queue_me
```

Important files:

```text
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/erl_remote_1000000_20260819_164900.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/top_remote_1000000_20260819_164900.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/wchan_msg_1000000_20260819_165158.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_msg_1000000_20260819_165206_sudo.log
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_stack_msg_1000000_20260819_165243.summary
remote: /home/romanvozfp/ecall_tests/proc_messages/logs/strace_futex_stack_msg_1000000_20260819_165243.raw
```

The 1M futex summary:

```text
603072 futex calls in 25 seconds
189389 errors/retries
```

The 1M stack-enabled strace again pointed at:

```text
send_2
do_send
erts_send_message
queue_messages.part.0
erts_proc_sig_queue_lock
erts_proc_lock_failed
erts_proc_unlock_failed
```

## Conclusion

The assumption was confirmed. A large number of processes sending to one shared
target can stall the VM on process/message queue locking.

For this remote 48-scheduler host and this one-message-per-second workload:

```text
100k workers: clean
200k workers: first bad point in the requested search
1M workers:   strongly reproduced the stall
```

The native evidence points to contention in the Erlang process signal/message
enqueue path, specifically around `erts_proc_sig_queue_lock` and
`queue_messages.part.0`, reached from the Erlang `!` operator implementation
(`send_2`).

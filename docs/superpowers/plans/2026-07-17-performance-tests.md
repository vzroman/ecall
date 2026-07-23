# Performance Tests Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the Common Test performance workload suites described in `perf_tests/spec.md` on top of the locked Docker/peer orchestration layer.

**Architecture:** Keep Common Test as the only orchestrator. Sender and receiver role nodes run the same compiled modules as the controller, and test cases spawn monitored role procedures on those nodes. Shared modules under `test/performance/` own run configuration, paced load generation, receiver counters, metric/report rows, and reusable role coordination.

**Tech Stack:** Erlang/OTP 27, Common Test, `peer`, Docker, existing `distributed_tests_util`, `ecall`, `erpc`, raw distributed sends.

---

## Files

- Modify: `test/performance/test.spec` to include the workload and support suites.
- Modify: `test/performance/performance.config` to provide a smoke/dev run profile that overrides report-duration defaults safely.
- Modify: `test/performance/performance.example.config` to document reportable-run overrides.
- Modify: `test/performance/distributed_tests_util.erl` only if a workload requires reusable orchestration support not already present.
- Create: `test/performance/performance_config.erl` for CT config parsing and defaults.
- Create: `test/performance/performance_report.erl` for report row normalization and CSV output.
- Create: `test/performance/performance_metrics.erl` for available node metrics and `n/a` placeholders for unavailable metrics.
- Create: `test/performance/performance_load.erl` for barriers, paced loops, fixed-work loops, payloads, and proxy batching helpers.
- Create: `test/performance/performance_config_SUITE.erl` for non-Docker unit coverage of config/report helpers.
- Create: `test/performance/performance_workload_SUITE.erl` for H1, H2, H3, H5, H10a, H10b, E1, E2, and E3.

## Phase 1: Shared Support Modules

**Exit criteria:**

- `performance_config:from_ct/1` returns complete run controls with spec defaults and CT/env overrides.
- `performance.config` uses a smoke/dev profile with short durations and small load lists so `make performance_tests` is runnable.
- `performance.example.config` documents the full reportable defaults and override keys.
- `performance_report` writes rows that always include case, path, role, warm-up, measurement, drain, repetition, offered rate, delivered count, delivered/s, delivery ratio, and unavailable supportive metrics as `n/a`.
- `performance_metrics` samples available VM IO/reduction/process metrics without requiring a lock-counting emulator.
- `performance_config_SUITE` proves config defaults, smoke overrides, CSV formatting, and password sanitization expectations.
- Commands pass: `./rebar3 ct --suite test/performance/performance_config_SUITE.erl`.

**Steps:**

- [ ] Write failing Common Test cases in `performance_config_SUITE` for default run control parsing, smoke override parsing, row normalization, and `n/a` metric rendering.
- [ ] Run `./rebar3 ct --suite test/performance/performance_config_SUITE.erl` and verify it fails because the support modules do not exist.
- [ ] Implement `performance_config.erl`, `performance_report.erl`, and `performance_metrics.erl` minimally to satisfy the tests.
- [ ] Update `performance.config` and `performance.example.config` with the agreed run-control keys.
- [ ] Run the support suite again and fix compile/runtime issues until it passes.
- [ ] Run `./rebar3 compile` and fix warnings as errors.

## Phase 2: Load Engine and Role Procedures

**Exit criteria:**

- `performance_load` can run these paths between sender and receiver role processes: raw send, fixed-work raw send, proxy batching, `erpc:cast/4`, `erpc:call/4`, `ecall:send/2`, `ecall:cast/4`, and `ecall:call/4`.
- Receiver-confirmed counts are used for send/cast style paths; completed replies are used for call style paths.
- Role procedures report `{Pid, ready}` and `{Pid, Result}` to the controller and rely on `spawn_monitor` for failure reporting.
- E3 clients allow at most one outstanding call per client.
- H2 sends exactly the configured fixed-work total and reports measured completion duration.
- H3 reports full and partial batch counters plus max proxy queue depth where applicable.
- Commands pass: `./rebar3 ct --suite test/performance/performance_config_SUITE.erl`.

**Steps:**

- [ ] Add failing tests to `performance_config_SUITE` for payload selection, offered-rate calculation, fixed-work quota splitting, and report row fields needed by H2/H3/E3.
- [ ] Run `./rebar3 ct --suite test/performance/performance_config_SUITE.erl` and verify the new tests fail for missing behavior.
- [ ] Implement `performance_load.erl` helpers for payloads, rates, quotas, barriers, paced send loops, fixed-work send loops, call loops, and proxy batching counters.
- [ ] Add `performance_workload_SUITE` exported role target functions used by `erpc` and `ecall` calls.
- [ ] Run the support suite and compile until passing.

## Phase 3: Workload Suite Integration

**Exit criteria:**

- `performance_workload_SUITE` follows the locked `init_per_suite/1` orchestration lifecycle and stores sanitized role config plus sender/receiver node names in CT `Config`.
- Suite exposes test cases: `h1_raw_send_curve`, `h2_fixed_work_writer_contention`, `h3_sender_pool_batching`, `h5_busy_limit_sweep`, `h10a_payload_cost`, `h10b_receiver_work`, `e1_raw_vs_ecall_send`, `e2_erpc_vs_ecall_cast`, and `e3_erpc_vs_ecall_call`.
- Each case executes the configured smoke/report matrix and writes report rows to CT priv dir.
- H5 records the requested distribution busy-limit parameter for each row; if the current VM cannot be restarted inside the case for a different limit, the row must explicitly mark the effective runtime value as `n/a` rather than inventing it.
- `test/performance/test.spec` includes the support suite, the existing orchestration smoke suite, and the workload suite.
- Commands pass: `make performance_tests`.

**Steps:**

- [ ] Add a failing integration test case or compile target proving `performance_workload_SUITE` is included by `test.spec`.
- [ ] Implement the suite callbacks, spawn-monitor helpers, and per-case matrix runners.
- [ ] Wire every H and E case to `performance_load` and `performance_report`.
- [ ] Run `make performance_tests`; fix compile/runtime issues until local/local smoke profile passes.
- [ ] Run `./rebar3 ct --suite test/ecall_connection_SUITE.erl` to catch `ecall` regressions.

## Main-Agent Final Verification

- [ ] Re-read `perf_tests/spec.md` and check each implementation acceptance criterion against files/tests.
- [ ] Run `git diff --check`.
- [ ] Run `make performance_tests`.
- [ ] Run `./rebar3 ct --suite test/ecall_connection_SUITE.erl`.
- [ ] Run a secret scan for known remote credentials and host literals outside `_build`.
- [ ] Inspect Docker containers named `ecall-performance-*` locally and ensure no leftovers after CT.

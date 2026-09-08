#!/usr/bin/env bash

set -u
set -o pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
project_dir=$(cd -- "${script_dir}/../.." && pwd)
config_file="${script_dir}/performance.config"
run_started_at=$(date '+%Y-%m-%d_%H.%M.%S')
log_dir="${project_dir}/_build/test/performance_matrix/${run_started_at}"
summary_file="${log_dir}/summary.log"
config_backup=$(mktemp)

mkdir -p "${log_dir}"
cp "${config_file}" "${config_backup}"

restore_config() {
  cp "${config_backup}" "${config_file}"
  rm -f "${config_backup}"
}

trap restore_config EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

write_config() {
  local busy_limit=$1
  local writer_counts=$2
  local payloads=$3

  printf '%s\n' \
    '{role_config, #{' \
    '  sender => local,' \
    '  receiver => local' \
    '}}.' \
    '' \
    '{performance, #{' \
    '  pace_ms => 100,' \
    '  messages_per_writer => 100,' \
    "  writer_counts => ${writer_counts}," \
    "  payloads => ${payloads}" \
    '}}.' \
    '' \
    '{env_settings, #{' \
    '  ecall_batch_size => 1000,' \
    "  distribution_busy_limit_kib => ${busy_limit}" \
    '}}.' >"${config_file}"
}

run_config() {
  local busy_limit=$1
  local writer_counts=$2
  local payloads=$3
  local run_log="${log_dir}/busy_limit_${busy_limit}_kib.log"
  local started_at
  local finished_at
  local status
  local exit_code

  write_config "${busy_limit}" "${writer_counts}" "${payloads}"
  started_at=$(date --iso-8601=seconds)

  {
    printf 'Started: %s\n' "${started_at}"
    printf 'distribution_busy_limit_kib: %s\n' "${busy_limit}"
    printf 'writer_counts: %s\n' "${writer_counts}"
    printf 'payloads: %s\n\n' "${payloads}"
  } | tee "${run_log}"

  if (cd "${project_dir}" && make performance_tests) 2>&1 | tee -a "${run_log}"; then
    status=passed
    exit_code=0
  else
    exit_code=$?
    status=failed
  fi

  finished_at=$(date --iso-8601=seconds)
  printf '\nFinished: %s\nStatus: %s (exit code %s)\n' \
    "${finished_at}" "${status}" "${exit_code}" | tee -a "${run_log}"
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "${busy_limit}" "${writer_counts}" "${payloads}" "${status}" \
    "${exit_code}" "${started_at}" "${finished_at}" >>"${summary_file}"

  test "${status}" = passed
}

normal_writer_counts='[10000,20000,30000,40000,50000,60000]'
large_writer_counts='[10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000]'
normal_payloads='[tiny, data]'
large_payloads='[tiny, data, binary_10kib]'
failed_runs=0

printf 'busy_limit_kib\twriter_counts\tpayloads\tstatus\texit_code\tstarted\tfinished\n' \
  >"${summary_file}"

for busy_limit in 1024 4096 16384 65536; do
  if ! run_config \
    "${busy_limit}" "${normal_writer_counts}" "${normal_payloads}"; then
    failed_runs=$((failed_runs + 1))
  fi
done

for busy_limit in 262144 1048576 2097151; do
  if ! run_config \
    "${busy_limit}" "${large_writer_counts}" "${large_payloads}"; then
    failed_runs=$((failed_runs + 1))
  fi
done

printf '\nMatrix finished with %s failed run(s).\nSummary: %s\n' \
  "${failed_runs}" "${summary_file}"

test "${failed_runs}" -eq 0

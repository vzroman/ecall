{define, 'PERFORMANCE_TEST', "./."}.

{config, "performance.config"}.

{suites, 'PERFORMANCE_TEST', [
    performance_config_SUITE,
    performance_orchestration_SUITE,
    performance_workload_SUITE
]}.

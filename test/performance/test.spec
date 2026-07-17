{define, 'PERFORMANCE_TEST', "./."}.

{config, "performance.config"}.

{suites, 'PERFORMANCE_TEST', [
    performance_orchestration_SUITE
]}.

{define, 'PERFORMANCE_TEST', "./."}.

{config, "performance.config"}.

{include, 'PERFORMANCE_TEST', ["util"]}.

{suites, 'PERFORMANCE_TEST', [
    performance_send_SUITE,
    performance_cast_SUITE,
    performance_call_SUITE
]}.

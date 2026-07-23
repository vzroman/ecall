compile:
	./rebar3 compile

clean_logs:
	rm -rf logs

clean_build:
	rm -rf _build
	rm -f rebar.lock

clean_all: clean_logs clean_build

test:
	./rebar3 ct --suite test/ecall_connection_SUITE.erl

performance_tests: compile
	./rebar3 ct --spec=./test/performance/test.spec

clean_tests:
	rm -rf _build/test

shell:
	ERL_FLAGS="-args_file config/vm.args -config config/sys.config" ./rebar3 shell


compile:
	./rebar3 compile

test_load: compile
	./rebar3 ct --spec=./test/load.spec

test_as_peer: compile
	./rebar3 ct --spec=./test/as_peer.spec

clean_logs:
	rm -rf logs

clean_tests:
	rm -rf _build/test

clean_build:
	rm -rf _build
	rm -rf rebar.lock

clean_all: clean_logs clean_tests clean_build

shell:
	ERL_FLAGS="-args_file config/vm.args -config config/sys.config" ./rebar3 shell

compile:
	./rebar3 compile

test_load: compile
	./rebar3 ct --spec=./test/load.spec

peer: compile
	ERL_FLAGS="-args_file config/vm.args -config config/sys.config" ./rebar3 as test shell

clean_logs:
	rm -rf logs

clean_tests:
	rm -rf _build/test

clean_docker:
	docker stop $(docker ps -a --filter "ancestor=ecall" -q)
	docker rm $(docker ps -a --filter "ancestor=ecall" -q)
	docker rmi ecall

clean_build:
	rm -rf _build
	rm -rf rebar.lock

clean_all: clean_logs clean_tests clean_build

shell:
	ERL_FLAGS="-args_file config/vm.args -config config/sys.config" ./rebar3 shell
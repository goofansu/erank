### erank Makefile

PROJECT = erank

# Options.

ERLC_OPTS ?= +debug_info +'{parse_transform, lager_transform}'

include erlang.mk

release: clean-app clean-release compile
	./rebar generate

clean-app:
	./rebar clean

clean-release:
	rm -rf rel/erank

get-deps:
	./rebar get-deps
	./rebar compile

compile:
	./rebar compile skip_deps=true

run:
	erl -emu_args -hidden -pa ebin -pa deps/*/ebin \
	-config rel/sys.config -args_file rel/vm.args \
	-s lager -s erank_app

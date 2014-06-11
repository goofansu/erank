### erank Makefile

PROJECT = erank

# Options.

ERLC_OPTS ?= +debug_info +'{parse_transform, lager_transform}'

include erlang.mk

get-deps:
	./rebar get-deps
	./rebar compile

run:
	erl -emu_args -hidden -pa ebin -pa deps/*/ebin \
	-config rel/sys.config -args_file rel/vm.args \
	-s lager -s erank_app

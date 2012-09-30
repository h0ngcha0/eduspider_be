.PHONY: rel deps

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

rel: deps compile
	@./rebar generate --force

relforce: deps compile
	@./rebar generate force=1

clean:
	@./rebar clean

distclean: clean relclean
	@./rebar delete-deps

relclean:
	rm -rf rel/eduspider

test:
	./rebar skip_deps=true eunit

console:
	erl -sname eduspider_core -pa $(PWD)/lib/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s eduspider_core -init_debug -config gen/files/sys.config

devstart: compile console


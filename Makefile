REBAR      = ./rebar


.PHONY: all clean allclean test dialyzer deps gen

## rebar wrappers ##############################################################
fast:
	$(REBAR) -j 5 skip_deps=true compile

all: deps
	$(REBAR) -j 5 compile

deps:
	$(REBAR) -j 5 get-deps

clean:
	$(REBAR) -j 5 skip_deps=true clean

allclean:
	$(REBAR) -j 5 clean

gen: deps
	$(REBAR) generate

test:
	$(REBAR) -j 5 skip_deps=true eunit

dialyzer:
	dialyzer -I apps/*/include --statistics -Wunderspecs --src apps/*/src

REBAR      = ./rebar
REBARFLAGS = -j 5

.PHONY: all clean distclean test dialyzer deps gen

all: deps
	$(REBAR) $(REBARFLAGS) compile

deps:
	$(REBAR) $(REBARFLAGS) get-deps

clean:
	$(REBAR) $(REBARFLAGS) skip_deps=true clean

distclean:
	$(REBAR) $(REBARFLAGS) clean

gen: deps
	$(REBAR) generate

test:
	$(REBAR) $(REBARFLAGS) skip_deps=true eunit

dialyzer:
	dialyzer -I apps/*/include --statistics -Wunderspecs --src apps/*/src

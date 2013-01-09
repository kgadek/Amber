REBAR      = ./rebar
REBARFLAGS = -j 5
C_SRC      = apps/amber/c_src


.PHONY: all clean distclean test dialyzer deps gen drivers roboclaw_driver amber_proto

## rebar wrappers ##############################################################
all: deps
	$(REBAR) $(REBARFLAGS) compile

fast:
	$(REBAR) $(REBARFLAGS) skip_deps=true compile

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

## drivers' compilation ########################################################

drivers: roboclaw_driver

roboclaw_driver: amber_proto roboclaw.proto
	bash le_compiler.sh roboclaw "roboclaw_lib/*.cpp" roboclaw_lib

amber_proto:
	protoc -I=$(C_SRC)/protobuf --cpp_out=$(C_SRC)/protobuf $(C_SRC)/protobuf/drivermsg.proto

%.proto:
	protoc -I=$(C_SRC)/$(@:%.proto=%)/protobuf -I=$(C_SRC)/protobuf --cpp_out=$(C_SRC)/$(@:%.proto=%)/protobuf $(C_SRC)/$(@:%.proto=%)/protobuf/$@
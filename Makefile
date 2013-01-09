REBAR      = ./rebar
REBARFLAGS = -j 5


.PHONY: all clean distclean test dialyzer deps gen drivers

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

drivers: roboclaw_driver

roboclaw_driver: amber_proto
	bash le_compiler.sh roboclaw "roboclaw_lib/*.cpp" roboclaw_lib

amber_proto:
	protoc -I=apps/amber/c_src/protobuf --cpp_out=apps/amber/c_src/protobuf apps/amber/c_src/protobuf/drivermsg.proto

%.proto:
	protoc -I=apps/amber/c_src/$(@:%.proto=%)/protobuf -I=apps/amber/c_src/protobuf --cpp_out=apps/amber/c_src/$(@:%.proto=%)/protobuf apps/amber/c_src/$(@:%.proto=%)/protobuf/$@
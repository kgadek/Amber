REBAR      = ./rebar
C_SRC      = apps/amber/c_src


.PHONY: all clean allclean test dialyzer deps gen drivers roboclaw_driver amber_proto

## rebar wrappers ##############################################################
fast:
	$(REBAR) -j 5 skip_deps=true compile

all: deps drivers
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

## drivers' compilation ########################################################

drivers: roboclaw_driver stargazer_driver ninedof_driver

roboclaw_driver: amber_proto roboclaw.proto
	bash le_compiler.sh roboclaw "roboclaw_lib/*.cpp" roboclaw_lib

stargazer_driver: amber_proto stargazer.proto
	bash le_compiler.sh stargazer "uart/*.cpp" uart

ninedof_driver: amber_proto ninedof.proto
	bash le_compiler.sh ninedof "i2c/*.cpp" i2c

amber_proto:
	protoc -I=$(C_SRC)/protobuf --cpp_out=$(C_SRC)/protobuf $(C_SRC)/protobuf/drivermsg.proto

%.proto:
	protoc -I=$(C_SRC)/$(@:%.proto=%)/protobuf -I=$(C_SRC)/protobuf --cpp_out=$(C_SRC)/$(@:%.proto=%)/protobuf $(C_SRC)/$(@:%.proto=%)/protobuf/$@
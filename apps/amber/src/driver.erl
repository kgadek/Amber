-module(driver).

-behaviour(gen_server).
-behaviour(router).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([start_node/2, receive_msg/2]).
% -export([start_link/1]).
% -export([twice/2]).
% -export([handle_msg/2, config_uniqueness/1]).

-include("include/drivermsg_pb.hrl").
-include("include/routing.hrl").
% -record(state, {port, msg_hdr, config, unique_config}).
-record(state, {port, mid, conf}).


start_node({RegName} = MID, Conf) -> gen_server:start_link({local, RegName}, driver, {MID, Conf}, []).

receive_msg({RegName}, FullMsg) -> gen_server:cast(RegName, {received, FullMsg}) .

init({MID, Conf}) ->

  {cdriver, CDriver} = lists:keyfind(cdriver, 1, Conf),
  {config_file, ConfigFile} = lists:keyfind(config_file, 1, Conf),
  {log_config_file, LogConfigFile} = lists:keyfind(log_config_file, 1, Conf),

  process_flag(trap_exit, true),
  Port = open_port({spawn_executable, CDriver}, [{packet, 2}, {args, [ConfigFile, LogConfigFile]}]),
  {ok, #state{port = Port, conf = Conf, mid = MID}}.

handle_cast({received, #routing_msg{hdr=Hdr, msg=Msg}}, #state{port=Port} = State) ->
  Port ! {self(), {command, drivermsg_pb:encode_driverhdr(Hdr)}},
  Port ! {self(), {command, Msg}},
  {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
  {stop, {port_died, Reason, State}, State};
handle_info({Port, {data, HdrPB}}, #state{port=Port, mid=MID} = State) ->

  receive
    {Port, {data, Msg}} ->
      Hdr = drivermsg_pb:decode_driverhdr(list_to_binary(HdrPB)),
      router:send_msg({?MODULE, MID}, #routing_msg{hdr=Hdr, msg=list_to_binary(Msg)})
  after 100 -> erlang:error("Po headerze nie odebrano treści wiadomości") 
  end,
  {noreply, State}.

terminate({port_died, _,_}, _) -> ok;
terminate(_Reason, #state{port=Port}) ->
  Port ! {self(), close},
  receive {Port, closed} -> ok end.

handle_call(_,_,State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

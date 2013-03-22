-module(udpserver).

-behaviour(router).
-behaviour(gen_server).

-include("include/config.hrl").
-include("include/routing.hrl").

-export([start_node/2, receive_msg/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(AMBER_PORT, 26233).
-record(state, {conf, socket}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    UDPSERVER JEST WĘZŁEM                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Rejestracja węzła u routera.
%% Bo router też jest węzłem.
start_node(_,_) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ?AMBER_PORT, []).

receive_msg({?MODULE, {udpserver}}, #routing_msg{}) -> ok;
receive_msg(Dest, FullMsg) -> gen_server:cast(?MODULE, {received, Dest, FullMsg}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 UDPSERVER JEST GEN_SERVEREM                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Port) ->
	{ok, Socket} = gen_udp:open(Port, [binary, {recbuf, 32768}, {sndbuf, 32768}]), % Socket zamykany w terminate/2
	{ok, #state{conf = Port, socket = Socket}}.

handle_cast({received, {IP, RPort}, #routing_msg{hdr = Hdr, msg = Msg}}, #state{socket = Socket} = State) ->
	FullMsg = router:pack_msg(Hdr, Msg),
	gen_udp:send(Socket, IP, RPort, FullMsg),
	{noreply, State}.

handle_info({udp, Socket, IP, Port, MsgB}, #state{socket = Socket} = State) ->
	{Hdr, Msg} = router:unpack_msg(MsgB),
	router:send_msg({?MODULE, {IP, Port}}, #routing_msg{hdr = Hdr, msg = Msg}),
	{noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
	gen_udp:close(Socket),
	ok.

handle_call(Msg, _From, State) -> {reply, Msg, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

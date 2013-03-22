-module(tcpserver_acceptor).
-behaviour(gen_server).

-record(state, {socket}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).

start_link(LSocket) ->
  gen_server:start_link(?MODULE, LSocket, []).

init(LSocket) ->
  gen_server:cast(self(), accept),
  {ok, #state{socket=LSocket}}.

handle_cast(accept, S = #state{socket=LSocket}) ->
  {ok, ASocket} = gen_tcp:accept(LSocket),
  tcpserver_sup:start_acceptor(),
  {noreply, S#state{socket=ASocket}}.

handle_info({tcp, _ASock, Msg}, S) ->
  io:format("TCP otrzymało: ~p~n", [Msg]),
  {noreply, S};
handle_info({tcp_closed, _Asock, _}, S) -> {stop, normal, S};
handle_info({tcp_error, _Asock, _}, S) -> {stop, normal, S}.

handle_call(_M, _F, S) -> {noreply, S}.

code_change(_OldV, S, _Ex) -> {ok, S}.

terminate(normal, _S) -> ok;
terminate(Reason, _S) ->
  io:format("TCP zamykane z powodu: ~p~n", [Reason]).

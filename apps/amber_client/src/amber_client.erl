-module(amber_client).
-behaviour(gen_server).

-include("include/drivermsg_pb.hrl").
-include("include/common.hrl").


% gen_server
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
% api
-export([register_receiver/2, deregister_receiver/1, get_synnum/0, send_to_amber/2]).
-export([get_newest_amber_client_msg/1, get_newest_amber_client_msg/2]).
-export([env/1]).

-record(state, {aip, aport, socket, dict, synnumnext}).


start() ->
	application:start(?MODULE).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, Socket} = gen_udp:open(env(amber_client_port), [binary, {recbuf, 32768}, {sndbuf, 32768}]),
	{ok, #state{aip    = env(amber_ip), aport = env(amber_port),
							socket = Socket,        dict  = gb_trees:empty(),
							%% TODO: synnymnext wywalić do mnesii, niedobrze jak jest w 'volatile'
							synnumnext = 1}}.

terminate(_Reason, #state{socket = Socket}) -> gen_udp:close(Socket).

handle_info({udp, Socket, _IP, _Port, FullMsg}, #state{socket=Socket, dict=Dict} = State) ->
	{#driverhdr{devicetype=DevT, deviceid=DevI} = Hdr, MsgB} = router:unpack_msg(FullMsg),
	#drivermsg{acknum = AckNum} = Msg                        = drivermsg_pb:decode_drivermsg(MsgB),
	Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=AckNum},
	case gb_trees:lookup(Key, Dict) of
		{value, #dispd_val{recpid=RecPid, post=_Post}} ->
			case process_info(RecPid) of
				undefined ->
					NDict = gb_trees:delete_any(Key, Dict),
					{noreply, State#state{dict=NDict}};
				_ ->
					RecPid ! #amber_client_msg{hdr=Hdr, msg=Msg},
					% case Post of %% TODO!!
					% 	{F,A} -> F(A);
					% 	undefined -> ok
					% end,
					{noreply, State}
			end;
		_ -> {noreply, State}
	end.

handle_cast({send_to_amber, MsgB}, #state{aip=AIP, aport=APort, socket=Socket} = State) ->
	ok = gen_udp:send(Socket, AIP, APort, MsgB),
	{noreply, State}.

handle_call({register_receiver, Key, Value}, _From, #state{dict=Dict} = State) ->
	{reply, ok, State#state{dict = gb_trees:enter(Key, Value, Dict)}};

handle_call({deregister_receiver, Key}, _From, #state{dict=Dict} = State) ->
	{reply, ok, State#state{dict = gb_trees:delete_any(Key, Dict)}};

handle_call(get_synnum, _From, #state{synnumnext=SN} = State) ->
	{reply, SN, State#state{synnumnext=SN+1}}.

code_change(_OldV, State, _Extra) -> {ok, State}.



-spec send_to_amber(binary)
			-> 'ok'.
send_to_amber(MsgB) -> gen_server:cast(?MODULE, {send_to_amber, MsgB}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%[ API ]%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Funkcja upraszczająca pobieranie konfiguracji. Zwraca błąd w wypadku
%% braku zadanego parametru.
-spec env(atom())
			-> any().
env(Par) ->
	{ok, Val} = application:get_env(?MODULE, Par),
	Val.


%% @doc Rejestruje synchronicznie odbiorcę wiadomości o polu acknum równym
%% podanemu SynNum od urządzenia o podanym typie DevT i numerze DevI.
-spec register_receiver(#dispd_key{}, #dispd_val{})
			-> 'ok'.
register_receiver(Key = #dispd_key{}, Val) ->
	gen_server:call(?MODULE, {register_receiver, Key, Val}).


%% @doc Derejestruje synchronicznie odbiorcę. Działa przeciwnie do funkcji
%% register_receiver/2.
-spec deregister_receiver(#dispd_key{})
			-> 'ok'.
deregister_receiver(Key = #dispd_key{}) ->
	gen_server:call(?MODULE, {deregister_receiver, Key}).


%% @doc Zwraca (unikalny) numer żądania.
-spec get_synnum()
			-> non_neg_integer().
get_synnum() -> gen_server:call(?MODULE, get_synnum).


%% @doc Wysyła asynchronicznie wiadomość do określonego urządzenia. By wiadomość
%% mogła dotrzeć, potrzebne są odpowiednio ustawione pola devicetype i deviceid
%% parametru Hdr. By sterownik mógł odpowiedzieć, potrzebne jest zainicjowane
%% pole synnum parametru Msg.
-spec send_to_amber(#driverhdr{}, binary())
			-> 'ok'.
send_to_amber(MsgH, MsgBinary) -> send_to_amber(router:pack_msg(MsgH, MsgBinary)).


%% @doc Zdejmuje z kolejki wiadomości wszystkie typu #amber_client_msg{} i
%% zwraca najnowszą. Gdy w kolejce nie ma żadnej, czeka na dostarczenie takiej
%% wiadomości przez określony czas.
-spec get_newest_amber_client_msg(timeout())
	-> #amber_client_msg{}.
get_newest_amber_client_msg(Timeout) ->
	receive Msg = #amber_client_msg{} -> get_newest_amber_client_msg(Msg, Timeout)
	after 0 ->
		receive Msg = #amber_client_msg{} -> Msg
		after Timeout -> error(amber_client_get_newest_msg_timeout)
		end
	end.

%% @hidden
get_newest_amber_client_msg(Msg, Timeout) ->
	receive NMsg = #amber_client_msg{} -> get_newest_amber_client_msg(NMsg, Timeout)
	after 0 -> Msg
	end.
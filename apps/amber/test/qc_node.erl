-module(qc_node).

-behaviour(gen_server).
-behaviour(router).


-include("include/config.hrl").
-include("include/routing.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_node/2, receive_msg/2]).
-export([qc_set_receiver/2, qc_send/2]).

-record(state, {mid, conf, qc_receiver}).


start_node({RegisterName} = MID, Conf) ->
	gen_server:start_link({local, RegisterName}, ?MODULE, {Conf, MID}, []).

receive_msg({RegisterName}, #routing_msg{} = FullMsg) ->
	gen_server:cast(RegisterName, {received, FullMsg}).

init({Conf, MID}) ->
	{ok, #state{mid = MID, conf = Conf}}.


qc_send({RegisterName}, FullMsg) ->
	gen_server:cast(RegisterName, {qc_send, FullMsg}).

qc_set_receiver({RegisterName}, Receiver) ->
	gen_server:call(RegisterName, {qc_set_receiver, Receiver}). 


handle_call({qc_set_receiver, Receiver}, _From, State) when is_pid(Receiver) ->
	{reply, ok, State#state{qc_receiver=Receiver}}.

handle_cast({received, #routing_msg{} = FullMsg}, #state{qc_receiver=QC_Rec, mid=MID} = State) ->
	try QC_Rec ! {ok, {?MODULE, MID}, FullMsg} catch error:badarg -> ok_anyway end,
	{noreply, State};

handle_cast({qc_send, FullRoutingMsg}, #state{mid=MID} = State) ->
	router:send_msg({?MODULE, MID}, FullRoutingMsg),
	{noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
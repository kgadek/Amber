-module(amber_client).
-behaviour(gen_server).

-include("include/drivermsg_pb.hrl").
-include("include/roboclaw_pb.hrl").
-include("include/stargazer_pb.hrl").

-define(AMBERIP, {127,0,0,1}).
-define(AMBERPORT, 26233).
-define(CLIENTPORT, 26232).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-define(ROBOCLAW_TI, {1,2}).
-export([motors_command/1]).

-define(STARGAZER_TI, {0,0}).
-export([stargazer_order_position/0, stargazer_order_position/1, stargazer_get_position/1, stargazer_get_position/2]).

-record(state, {aip, aport, socket, dict, synnumnext}).

-type int32()  :: -2147483648..2147483647.
-type uint32() :: 0..4294967295.


start() -> application:start(?MODULE).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, {?AMBERIP, ?AMBERPORT}, []).


init({AmberIP, AmberPort}) ->
	{ok, Socket} = gen_udp:open(?CLIENTPORT, [binary]),
	{ok, #state{aip    = AmberIP,  aport = AmberPort,
							socket = Socket,   dict  = gb_trees:empty(),
							%% TODO: synnymnext wywalić do mnesii, niedobrze jak jest w 'volatile'
							synnumnext = 1}}.

terminate(_Reason, #state{socket = Socket}) -> gen_udp:close(Socket).

handle_info({udp, Socket, ?AMBERIP, ?AMBERPORT, Msg}, #state{socket=Socket, dict=Dict} = State) ->
	{Hdr, MsgB} = router:unpack_msg(Msg),
	#driverhdr{devicetype=DevT, deviceid=DevI} = Hdr,
	#drivermsg{synnum=SynNum}                  = drivermsg_pb:decode_drivermsg(MsgB),

	NDict = case gb_trees:lookup({DevT, DevI, SynNum}, Dict) of
						{value, RecPid} ->
							case process_info(RecPid) of
								undefined ->
									gb_trees:delete_any({DevT, DevI, SynNum}, Dict);
								_ ->
									RecPid ! {amber_client_msg, now(), DevT, DevI, SynNum, MsgB},
									Dict
							end;
						none ->
							case gb_trees:lookup({DevT, DevI}, Dict) of
								{value, RecPid} ->
									case process_info(RecPid) of
										undefined ->
											error_logger:error_msg(	"amber_client otrzymal pakiet udp ktorego nie rozumie.~n"
																							"Nadawca: ~p :~p~nWiadomosc: ~p~nStan: ~p~n",
																							[?AMBERIP, ?AMBERPORT, Msg, State]),
											gb_trees:delete_any({DevT, DevI}, Dict);
										_ ->
											RecPid ! {amber_client_msg, now(), DevT, DevI, SynNum, MsgB},
											Dict
									end
							end
				 	end,
	{noreply, State#state{dict=NDict}}.

handle_cast({send_to_amber, MsgB}, #state{aip=AIP, aport=APort, socket=Socket} = State) ->
	ok = gen_udp:send(Socket, AIP, APort, MsgB),
	{noreply, State}.

handle_call({register_receiver, Key, Pid}, _From, #state{dict=Dict} = State) ->
	NDict = gb_trees:enter(Key, Pid, Dict), 
	{reply, ok, State#state{dict=NDict}};

handle_call(get_synnum, _From, #state{synnumnext=SN} = State) ->
	{reply, SN, State#state{synnumnext=SN+1}}.

code_change(_OldV, State, _Extra) -> {ok, State}.


%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_receiver(Key, Pid) -> gen_server:call(?MODULE, {register_receiver, Key, Pid}).

send_to_amber(MsgH, MsgBinary) -> send_to_amber(router:pack_msg(MsgH, MsgBinary)).
send_to_amber(MsgB) -> gen_server:cast(?MODULE, {send_to_amber, MsgB}).

get_synnum() -> gen_server:call(?MODULE, get_synnum).


-type motors_command_keys_int32()  :: 'front_m1_speed' | 'front_m2_speed' | 'rear_m1_speed' | 'rear_m2_speed'.
-type motors_command_keys_uint32() :: 'front_m1_accel' | 'front_m1_distance' | 'front_m2_accel' | 'front_m2_distance' |
															        'rear_m1_accel'  | 'rear_m1_distance'  | 'rear_m2_accel'  | 'rear_m2_distance'.
-type motors_command_keys_bool()   :: 'front_m1_buffered' | 'front_m2_buffered' | 'rear_m1_buffered' | 'rear_m2_buffered'.
-spec motors_command([ {motors_command_keys_int32(),  int32()}
										 | {motors_command_keys_uint32(), uint32()}
										 | {motors_command_keys_bool(),   boolean()} ])
			-> 'ok'.
motors_command(Os) ->
	Front = #motorscommand{
		m1speed    = proplists:get_value(front_m1_speed,    Os),
  	m1accel    = proplists:get_value(front_m1_accel,    Os),
  	m1distance = proplists:get_value(front_m1_distance, Os),
  	m1buffered = proplists:get_value(front_m1_buffered, Os),
  	m2speed    = proplists:get_value(front_m2_speed,    Os),
		m2accel    = proplists:get_value(front_m2_accel,    Os),
		m2distance = proplists:get_value(front_m2_distance, Os),
		m2buffered = proplists:get_value(front_m2_buffered, Os)
  },
  Rear = #motorscommand{
		m1speed    = proplists:get_value(rear_m1_speed,     Os),
  	m1accel    = proplists:get_value(rear_m1_accel,     Os),
  	m1distance = proplists:get_value(rear_m1_distance,  Os),
  	m1buffered = proplists:get_value(rear_m1_buffered,  Os),
  	m2speed    = proplists:get_value(rear_m2_speed,     Os),
		m2accel    = proplists:get_value(rear_m2_accel,     Os),
		m2distance = proplists:get_value(rear_m2_distance,  Os),
		m2buffered = proplists:get_value(rear_m2_buffered,  Os)
  },
	MsgBase = #drivermsg{type = 'DATA', synnum = get_synnum()},
	{ok, Msg} = roboclaw_pb:set_extension(MsgBase, motorscommands, [Front, Rear]),
	MsgBinary = roboclaw_pb:encode_drivermsg(Msg),
	{DevT,DevI} = ?ROBOCLAW_TI,
	Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
	send_to_amber(Hdr, MsgBinary).


stargazer_order_position() -> stargazer_order_position([]).

-type stargazer_order_position_future_ref() :: non_neg_integer().
-spec stargazer_order_position([ {'device_type', uint32()}
															 | {'device_id',   uint32()}
															 | {'pid',         pid()}])
			-> stargazer_order_position_future_ref().
stargazer_order_position(Os) ->
	SynNum = get_synnum(),
	MsgBase = #drivermsg{type = 'DATA', synnum = SynNum},
	{ok, Msg} = stargazer_pb:set_extension(MsgBase, datarequest, #datarequest{}),
	MsgBinary = stargazer_pb:encode_drivermsg(Msg),
	{DefDevT,DefDevI} = ?STARGAZER_TI,
	DevT = proplists:get_value(device_type, Os, DefDevT),
	DevI = proplists:get_value(device_id, Os, DefDevI), 
	Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
	OrderedBy = proplists:get_value(pid, Os, self()), 
	register_receiver({DevT, DevI, SynNum}, OrderedBy),
	send_to_amber(Hdr, MsgBinary),
	SynNum.


stargazer_get_position(SynNum) -> stargazer_get_position(SynNum, 5000).

-spec stargazer_get_position(stargazer_order_position_future_ref(), timeout())
			-> #drivermsg{}.
stargazer_get_position(SynNum, Timeout) ->
	{DevT, DevI} = ?STARGAZER_TI,
	receive {amber_client_msg, RecTime, DevT, DevI, SynNum, MsgB} ->
		Msg = stargazer_pb:decode_drivermsg(MsgB),
		{RecTime, Msg}
	after Timeout ->
		error(stargazer_get_position_timeout)
	end.
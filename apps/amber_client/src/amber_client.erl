-module(amber_client).
-behaviour(gen_server).

-include("include/drivermsg_pb.hrl").
-include("include/roboclaw_pb.hrl").
-include("include/stargazer_pb.hrl").
-include("include/common.hrl").
-include("include/motors_control.hrl").
-include("include/localization_data.hrl").

-define(AMBERIP, {127,0,0,1}).
-define(AMBERPORT, 26233).
-define(CLIENTPORT, 26232).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
% api
-export([register_receiver/2, deregister_receiver/1, get_synnum/0, send_to_amber/2]).

% ROBOCLAW
-export([motors_command/2, motors_command/4, motors_demo1/0]).
% STARGAZER
-export([stargazer_order_position/0, stargazer_order_position/1,
         stargazer_get_position/1, stargazer_get_position/2,
         stargazer_subscribe_position/1, stargazer_subscribe_position/2]).

-record(state, {aip, aport, socket, dict, synnumnext}).
-record(dispd_key, {dev_t  :: non_neg_integer(),
        						dev_i  :: non_neg_integer(),
        						synnum :: non_neg_integer()}).
-record(dispd_val, {recpid = self() :: pid(),
        						post            :: {fun((any()) -> any()), any()}}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, {?AMBERIP, ?AMBERPORT}, []).

init({AmberIP, AmberPort}) ->
	{ok, Socket} = gen_udp:open(?CLIENTPORT, [binary]),
	{ok, #state{aip    = AmberIP,  aport = AmberPort,
							socket = Socket,   dict  = gb_trees:empty(),
							%% TODO: synnymnext wywalić do mnesii, niedobrze jak jest w 'volatile'
							synnumnext = 1}}.

terminate(_Reason, #state{socket = Socket}) -> gen_udp:close(Socket).

handle_info({udp, Socket, ?AMBERIP, ?AMBERPORT, FullMsg}, #state{socket=Socket, dict=Dict} = State) ->
	{#driverhdr{devicetype=DevT, deviceid=DevI} = Hdr, MsgB} = router:unpack_msg(FullMsg),
	#drivermsg{acknum = AckNum} = Msg                        = drivermsg_pb:decode_drivermsg(MsgB),
	Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=AckNum},
	case gb_trees:lookup(Key, Dict) of
		{value, #dispd_val{recpid=RecPid, post=Post}} ->
			case process_info(RecPid) of
				undefined ->
					NDict = gb_trees:delete_any(Key, Dict),
					{noreply, State#state{dict=NDict}};
				_ ->
					RecPid ! #amber_client_msg{hdr=Hdr, msg=Msg},
					case Post of
						{F,A} -> F(A);
						undefined -> ok
					end,
					{noreply, State}
			end;
		_ -> {noreply, State}
	end.

handle_cast({send_to_amber, MsgB}, #state{aip=AIP, aport=APort, socket=Socket} = State) ->
	ok = gen_udp:send(Socket, AIP, APort, MsgB),
	{noreply, State}.

handle_call({register_receiver, Key, Value}, _From, #state{dict=Dict} = State) ->
	NDict = gb_trees:enter(Key, Value, Dict),
	{reply, ok, State#state{dict=NDict}};

handle_call({deregister_receiver, Key}, _From, #state{dict=Dict} = State) ->
	NDict = gb_trees:delete_any(Key, Dict),
	{reply, ok, State#state{dict=NDict}};

handle_call(get_synnum, _From, #state{synnumnext=SN} = State) ->
	{reply, SN, State#state{synnumnext=SN+1}}.

code_change(_OldV, State, _Extra) -> {ok, State}.

send_to_amber(MsgB) -> gen_server:cast(?MODULE, {send_to_amber, MsgB}).

env(Par) -> application:get_env(?MODULE, Par).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%[ API ]%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec register_receiver(#dispd_key{}, #dispd_val{})
			-> 'ok'.
register_receiver(Key = #dispd_key{}, Val) ->
	gen_server:call(?MODULE, {register_receiver, Key, Val}).

-spec deregister_receiver(#dispd_key{})
			-> 'ok'.
deregister_receiver(Key = #dispd_key{}) ->
	gen_server:call(?MODULE, {deregister_receiver, Key}).

-spec get_synnum()
			-> non_neg_integer().
get_synnum() -> gen_server:call(?MODULE, get_synnum).

-spec send_to_amber(#driverhdr{}, binary())
			-> 'ok'.
send_to_amber(MsgH, MsgBinary) -> send_to_amber(router:pack_msg(MsgH, MsgBinary)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%[ ROBOCLAW ]%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Uproszczone sterowanie robotem. Pozwala dobrać prędkość dla lewych i
%% prawych kół.
%% @equiv motors_command(Left, Right, Left, Right)
motors_command(Left, Right) ->
	motors_command(Left, Right, Left, Right).


%% -----------------------------------------------------------------------------
%% @doc Sterowanie robotem.
%%
%% Funkcja nieblokująca. Wysyła żądanie poruszania się kół robota.
%%
%% Jednostką prędkości jest mm/s.
%%
%% Teoretycznie 12000 pulsów/s to maksimum możliwości enkodera i silników.
%% Empirycznie stwierdzona prędkość maksymalna to około 2200 mm/s.
%%
%% Funkcja może zwrócić błąd jedynie w wypadku błędu protobuf, np. gdy podamy
%% string zamiast bool.
%% @end
%% -----------------------------------------------------------------------------
-spec motors_command(#motor_cmd{},#motor_cmd{},#motor_cmd{},#motor_cmd{})
			-> 'ok'.
motors_command(FL, FR, RL, RR) ->
	Front = build_drivermsg(FL, FR, 16#81),
	Rear  = build_drivermsg(RL, RR, 16#80),
	MsgBase = #drivermsg{type = 'DATA', synnum = get_synnum()},
	{ok, Msg} = roboclaw_pb:set_extension(MsgBase, motorscommands, [Rear, Front]),
	MsgBinary = roboclaw_pb:encode_drivermsg(Msg),
	Hdr = #driverhdr{devicetype = env(roboclaw_devt), deviceid = env(roboclaw_devi)},
	send_to_amber(Hdr, MsgBinary).


%% @hidden
build_drivermsg(#motor_cmd{speed=L_Speed, accel=L_Accel, dist=L_Dist, is_buf=L_Buf},
                #motor_cmd{speed=R_Speed, accel=R_Accel, dist=R_Dist, is_buf=R_Buf},
                HwAddr) ->
	#motorscommand{
		address = HwAddr,
		m1speed = R_Speed, m1accel = R_Accel, m1distance = R_Dist, m1buffered = R_Buf,
		m2speed = L_Speed, m2accel = L_Accel, m2distance = L_Dist, m2buffered = L_Buf
	}.


motors_demo1() ->
	timer:start(),
	timer:apply_after(1000, ?MODULE, motors_command, [1000, 1000]),
	timer:apply_after(2000, ?MODULE, motors_command, [1000, -1000]),
	timer:apply_after(3000, ?MODULE, motors_command, [1000, 1000]),
	timer:apply_after(4000, ?MODULE, motors_command, [1000, -1000]),
	timer:apply_after(5000, ?MODULE, motors_command, [1000, 1000]),
	timer:apply_after(6000, ?MODULE, motors_command, [0000, 0000]),
	timer:sleep(7000). 





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%[ STARGAZER ]%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



stargazer_order_position() -> stargazer_order_position(self()).

-type stargazer_order_position_future_ref() :: non_neg_integer().
-spec stargazer_order_position(pid())
			-> stargazer_order_position_future_ref().
stargazer_order_position(Pid) ->
	SynNum = get_synnum(),
	MsgBase = #drivermsg{type = 'DATA', synnum = SynNum},
	{ok, Msg} = stargazer_pb:set_extension(MsgBase, datarequest, #datarequest{}),
	MsgBinary = stargazer_pb:encode_drivermsg(Msg),
	DevT = env(stargazer_devt),
	DevI = env(stargazer_devi),
	Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
	Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=SynNum},
	Val = #dispd_val{recpid = Pid, post = {fun deregister_receiver/1, Key}},
	register_receiver(Key, Val),
	send_to_amber(Hdr, MsgBinary),
	SynNum.

stargazer_subscribe_position(Freq) -> stargazer_subscribe_position(Freq, self()).

stargazer_subscribe_position(Freq, Pid) ->
	SynNum = 0, % tego wymaga sterownik stargazera
 	MsgBase = #drivermsg{type = 'DATA', synnum = SynNum},
	{ok, Msg} = stargazer_pb:set_extension(MsgBase, subscribeaction,
	                                       #subscribeaction{action = 'SUBSCRIBE',
	                                       									freq   = Freq }),
	DevT = env(stargazer_devt),
	DevI = env(stargazer_devi),
	Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
	MsgBinary = stargazer_pb:encode_drivermsg(Msg),
	Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=SynNum},
	Val = #dispd_val{recpid = Pid},
	register_receiver(Key, Val),
	send_to_amber(Hdr, MsgBinary),
	SynNum.

stargazer_get_position(SynNum) -> stargazer_get_position(SynNum, 5000).

-spec stargazer_get_position(stargazer_order_position_future_ref(), timeout())
			-> #localization{}.
stargazer_get_position(SynNum, Timeout) ->
	DevT = env(stargazer_devt),
	DevI = env(stargazer_devi),
	receive #amber_client_msg{hdr = #driverhdr{devicetype=DevT, deviceid=DevI},
														msg = #drivermsg{synnum=SynNum} = Msg} ->
		stargazer_drivermsg_to_location(Msg)
	after Timeout -> error(stargazer_get_position_timeout)
	end.


-spec stargazer_drivermsg_to_location(#drivermsg{})
			-> #localization{}.
stargazer_drivermsg_to_location(Msg) ->
	DMsg = stargazer_pb:decode_extensions(Msg),
	{ok, #localizationdata{xpos=X,ypos=Y,zpos=Z,angle=A,markerid=M,timestamp=_T}}
							= stargazer_pb:get_extension(DMsg, localizationdata),
	#localization{xpos=X, ypos=Y, zpos=Z, angle=A, markerid=M}.

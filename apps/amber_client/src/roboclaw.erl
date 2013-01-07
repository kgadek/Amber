-module(roboclaw).

-include("include/common.hrl").
-include("include/roboclaw_pb.hrl").
-include("include/motors_control.hrl").

-export([motors_command/2, motors_command/4, motors_demo1/0]).



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
  MsgBase = #drivermsg{type = 'DATA', synnum = amber_client:get_synnum()},
  {ok, Msg} = roboclaw_pb:set_extension(MsgBase, motorscommands, [Rear, Front]),
  MsgBinary = roboclaw_pb:encode_drivermsg(Msg),
  Hdr = #driverhdr{devicetype = amber_client:env(roboclaw_devt), deviceid = amber_client:env(roboclaw_devi)},
  amber_client:send_to_amber(Hdr, MsgBinary).


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


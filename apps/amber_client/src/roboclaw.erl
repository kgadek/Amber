-module(roboclaw).

-include("include/common.hrl").
-include("include/roboclaw_pb.hrl").
-include("include/motors_control.hrl").

-export([motors_command/2, motors_command/4, motors_demo1/0]).
-export([command/2, command/4]).



%% Krótsze nazwy funkcji.

command(A,B)     -> motors_command(A,B).
command(A,B,C,D) -> motors_command(A,B,C,D).



%% @doc Uproszczone sterowanie robotem. Pozwala dobrać prędkość dla lewych i
%% prawych kół.
%% @equiv motors_command(Left, Right, Left, Right)
motors_command(Left, Right) -> motors_command(Left, Right, Left, Right).

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
%%
%% @end
%% -----------------------------------------------------------------------------
-spec motors_command(#motor_cmd{},     #motor_cmd{},
                     #motor_cmd{},     #motor_cmd{})      -> 'ok';
                    (non_neg_integer(),non_neg_integer(),
                     non_neg_integer(),non_neg_integer()) -> 'ok'.

motors_command(#motor_cmd{speed=FL}, #motor_cmd{speed=FR},
               #motor_cmd{speed=RL}, #motor_cmd{speed=RR}) ->
  motors_command(FL, FR, RL, RR);

motors_command(FL, FR, RL, RR) ->    
  MsgQuad = #motorsquadcommand{
    frontleftspeed  = FL, frontrightspeed = FR,
    rearleftspeed   = RL, rearrightspeed  = RR
  },
  MsgBase = #drivermsg{ type = 'DATA',
                        synnum = amber_client:get_synnum()},
  {ok, Msg} = roboclaw_pb:set_extension(MsgBase, motorsquadcommand, [MsgQuad]),
  MsgBinary = roboclaw_pb:encode_drivermsg(Msg),
  Hdr = #driverhdr{devicetype = amber_client:env(roboclaw_devt), deviceid = amber_client:env(roboclaw_devi)},
  amber_client:send_to_amber(Hdr, MsgBinary).


motors_demo1() ->
  timer:start(),
  timer:apply_after(1000, ?MODULE, motors_command, [1000, 1000]),
  timer:apply_after(2000, ?MODULE, motors_command, [1000, -1000]),
  timer:apply_after(3000, ?MODULE, motors_command, [1000, 1000]),
  timer:apply_after(4000, ?MODULE, motors_command, [1000, -1000]),
  timer:apply_after(5000, ?MODULE, motors_command, [1000, 1000]),
  timer:apply_after(6000, ?MODULE, motors_command, [0000, 0000]),
  timer:sleep(7000).


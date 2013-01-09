-module(amber_client_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include ("include/drivermsg_pb.hrl").
-include ("include/common.hrl").

-compile(export_all).


-define(START_APP(AppName),  case application:start(AppName) of
                               ok -> ok;
                               {error, {already_started, _}} -> ok
                             end).

receiver() ->
  receive
    stop                                    -> stop;
    {F,Xs} when is_function(F), is_list(Xs) -> apply(F,[F|Xs]), receiver()
  after 1000 -> kthxbye
  end.

proper_test_() ->
  {setup, fun proper_test_start_all/0, fun proper_test_stop_all/1, fun proper_test_main/1}.

proper_test_start_all() ->
  ?START_APP(mnesia),
  ?START_APP(amber),
  ?START_APP(amber_client).

proper_test_stop_all(_) ->
  application:stop(amber_client),
  application:stop(amber),
  application:stop(mnesia).


proper_test_main(_) ->
  [
    ?_assertNotEqual(false, lists:keyfind(amber, 1, application:which_applications())),
    ?_assertNotEqual(false, lists:keyfind(amber_client, 1, application:which_applications())),
    ?_assertEqual(true, is_pid(whereis(qc_driver_echo_a))), 
    proper:module(?MODULE, [{to_file, user}])
  ].

prop_warmup() ->
  ?FORALL(Lst, [any()], lists:reverse(lists:reverse(Lst)) =:= Lst).


prop_msg_from_remote_client() ->
  ?FORALL(Xs,
          list(pos_integer()),
    lists:all(fun(X) -> X=:=true end,
              lists:map(fun msg_send_receive/1, Xs))).

msg_send_receive(MsgNumber) ->
  {DevT,DevI} = {2,3},
  SynNum = amber_client:get_synnum(),
  Hdr = #driverhdr{devicetype=DevT, deviceid=DevI},
  Msg = #drivermsg{ type='DATA',
                    synnum=SynNum,
                    acknum=SynNum % tego nie powinno niby być, ale co tam
                  },
  MsgBinary = drivermsg_pb:encode_drivermsg(Msg),
  Receiver = spawn_link(fun receiver/0),
  Self = self(),
  Ref = make_ref(),
  
  Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=SynNum},
  Val = #dispd_val{recpid=Receiver},

  Receiver ! {
    fun (F,N) when N > 0 ->
          receive #amber_client_msg{hdr=#driverhdr{devicetype=DevT, deviceid=DevI},
                                    msg=#drivermsg{acknum=SynNum}}
                  -> F(F,N-1)
          end;
        (_,_) ->
          Self ! {true, Ref}
    end, [MsgNumber]
  },
  amber_client:register_receiver(Key, Val),
  [amber_client:send_to_amber(Hdr, MsgBinary) || _ <- lists:seq(1,MsgNumber)],
  receive {Bool, Ref} -> Bool
  after   500         -> false
  end.
-module(router_test).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("include/drivermsg_pb.hrl").
-include("include/routing.hrl").

-compile(export_all).

-define(STARTED(Result),	case Result of
														ok -> ok;
														{error, {already_started, _}} -> ok
													end).
-define(AMBER_PORT, 26233).

receiver() ->
	receive
		stop                                    -> stop;
		{F,Xs} when is_function(F), is_list(Xs) -> apply(F,[F|Xs]), receiver()
	end.

proper_test_() ->
	{setup, fun proper_test_start_all/0, fun proper_test_stop_all/1, fun proper_test_main/1}.

proper_test_start_all() ->
	?STARTED( application:start(mnesia) ),
	?STARTED( application:start(amber) ).

proper_test_stop_all(_) ->
	application:stop(amber),
	application:stop(mnesia).

proper_test_main(_) ->
	[
		?_assertNotEqual(false, lists:keyfind(amber, 1, application:which_applications())),
		are_helpers_started(),
		proper:module(?MODULE, [{to_file, user}]) % testy nie są wzajemnie thread-safe
	].

are_helpers_started() ->
	[
		?_assertEqual(true, is_pid(whereis(router))),
		?_assertEqual(true, is_pid(whereis(qc_node_a))),
		?_assertEqual(true, is_pid(whereis(qc_node_b))),
		?_assertEqual(true, is_pid(whereis(qc_driver_echo_a)))
	].

prop_pack_unpack() ->
	?FORALL({DevT, DevI, Cs, Msg}, {int(), int(), list(int()), binary()},
		begin
			Hdr = #driverhdr{devicetype=DevT, deviceid=DevI, clientids=Cs},
			{Hdr, Msg} = router:unpack_msg(router:pack_msg(Hdr, Msg)),
			true
		end).

prop_msg_from_driver() ->
	?FORALL({Xs,Msg},
					{list(union([2,3])), binary()}, % UWAGA: newralgiczne miejsce, wymaga (nietrywialnej) synchronizacji z settings.conf
																					% 2,3 to numery nodeów
					begin
						Receiver = spawn(fun receiver/0),
						try
							qc_node:qc_set_receiver({qc_node_a}, Receiver),
							qc_node:qc_set_receiver({qc_node_b}, Receiver),
							Hdr = #driverhdr{clientids = Xs},
							Self = self(),
							Ref = make_ref(),
							Receiver ! {fun (F,N) when N > 0 ->
																receive {ok,_Mid,#routing_msg{hdr=#driverhdr{devicetype=DevT, deviceid=DevI,
																															clientids=[]},
																															msg=Msg}}
																				when is_integer(DevT), is_integer(DevI) ->
																					F(F,N-1)
																end;
															(_,_) ->
																Self ! {true, Ref}
													end,
													[length(Xs)]},
							qc_node_echo:qc_send({qc_driver_echo_a}, Hdr, Msg),
							receive
								{Bool, Ref} -> Bool
								after 500 -> false
							end
						after Receiver ! stop
						end
					end).

prop_msg_from_client() ->
	?FORALL({Xs, Msg},
					{	list(tuple([union([{qc_node_a},{qc_node_b}]),
												union([{2,3}])])),
																				% UWAGA: newralgiczne miejsce, synchronizacji z settings.conf
																				% qc_node_[a,b] to MID fake-nodeów
																				% {2,3} to device-type i device-id fake-drivera
						binary()},
					begin
						Receiver = spawn(fun receiver/0),
						try
							qc_node:qc_set_receiver({qc_node_a},             Receiver),
							qc_node:qc_set_receiver({qc_node_b},             Receiver),
							qc_node_echo:qc_set_receiver({qc_driver_echo_a}, Receiver),
							Self = self(),
							Ref = make_ref(),
							Receiver ! {
								fun (F,N) when N > 0 ->
											receive
												{ok,{qc_node, _},#routing_msg{hdr=#driverhdr{devicetype=DevT, deviceid=DevI, clientids=[]},
																											msg=Msg}}
												when is_integer(DevT), is_integer(DevI) ->
													F(F,N-1);
												{ok,{qc_node_echo, _},#routing_msg{	hdr=#driverhdr{	devicetype=undefined, deviceid=undefined,
																																						clientids=[X]},
																														msg=Msg}}
												when is_integer(X) ->
													F(F,N-1)
											end;
										(_,_) ->
											Self ! {true, Ref}
								end,
								[2*length(Xs)]},
								% 2*length(Xs) bo qc_node_echo odbija wiadomość z powrotem.
								% Dostajemy zatem raport o Msg od qc_node_echo i od qc_node
							[qc_node:qc_send(MID, #routing_msg{ hdr=#driverhdr{devicetype=DevT, deviceid=DevI},
																									msg=Msg})
											|| {MID, {DevT, DevI}} <- Xs ],
							receive 
								{Bool, Ref} -> Bool
								after 500 -> false
							end
						after Receiver ! stop
						end
					end).

prop_udp() ->
	?FORALL(MsgB, binary(),
			begin
				Hdr = #driverhdr{devicetype=2, deviceid=3}, %echo driver
				FullMsg = router:pack_msg(Hdr, MsgB),
				{ok, Socket} = gen_udp:open(?AMBER_PORT+1, [binary, {active, true}]), % udp musi zarejestrować klienta!
				try
					gen_udp:controlling_process(Socket, self()), 
					gen_udp:send(Socket, {127,0,0,1}, ?AMBER_PORT, FullMsg),
					receive
						{udp, Socket, {127,0,0,1}, ?AMBER_PORT, Bin} ->
							{_RHdr, MsgB} = router:unpack_msg(Bin), 
							true
						after 500 -> false
					end
				after
					gen_udp:close(Socket)
				end
			end).
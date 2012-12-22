-module(amber_roboclaw).

-compile(export_all).

-include("include/roboclaw_pb.hrl").

-define (DEVICE_TI, {1,2}).


motors(Os) ->
	Front = #motorscommand{
		m1speed    = proplists:get_value(front_m1_speed, Os),
  	m1accel    = proplists:get_value(front_m1_accel, Os),
  	m1distance = proplists:get_value(front_m1_distance, Os),
  	m1buffered = proplists:get_value(front_m1_buffered, Os),
  	m2speed    = proplists:get_value(front_m2_speed, Os),
		m2accel    = proplists:get_value(front_m2_accel, Os),
		m2distance = proplists:get_value(front_m2_distance, Os),
		m2buffered = proplists:get_value(front_m2_buffered, Os)
  },
  Rear = #motorscommand{
		m1speed    = proplists:get_value(rear_m1_speed, Os),
  	m1accel    = proplists:get_value(rear_m1_accel, Os),
  	m1distance = proplists:get_value(rear_m1_distance, Os),
  	m1buffered = proplists:get_value(rear_m1_buffered, Os),
  	m2speed    = proplists:get_value(rear_m2_speed, Os),
		m2accel    = proplists:get_value(rear_m2_accel, Os),
		m2distance = proplists:get_value(rear_m2_distance, Os),
		m2buffered = proplists:get_value(rear_m2_buffered, Os)
  },
	MsgBase = #drivermsg{type = 'DATA', synnum = amber_client:get_synnum()},
	{ok, Msg} = roboclaw_pb:set_extension(MsgBase, motorscommands, [Front, Rear]),
	MsgBinary = roboclaw_pb:encode_drivermsg(Msg),
	{DefDevT,DefDevI} = ?DEVICE_TI,
	DevT = proplists:get_value(device_type, Os, DefDevT),
	DevI = proplists:get_value(device_id, Os, DefDevI),
	Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
	FullMsg = router:pack_msg(Hdr, MsgBinary),
	amber_client:send_to_amber(FullMsg).

-ifndef(COMMON_H).
-define(COMMON_H, true).

-include("drivermsg_pb.hrl").

-type int32()  :: -2147483648..2147483647.
-type uint32() :: 0..4294967295.

-record(amber_client_msg, {hdr = #driverhdr{} :: #driverhdr{},
                           msg = #drivermsg{} :: #drivermsg{}}).
-endif.
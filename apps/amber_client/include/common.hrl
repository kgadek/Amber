-ifndef(COMMON_H).
-define(COMMON_H, true).

-include("drivermsg_pb.hrl").

-type int32()  :: -2147483648..2147483647.
-type uint32() :: 0..4294967295.

-record(amber_client_msg, {hdr = #driverhdr{} :: #driverhdr{},
                           msg = #drivermsg{} :: #drivermsg{}}).

-record(dispd_key, {dev_t  :: non_neg_integer(),
                    dev_i  :: non_neg_integer(),
                    synnum :: non_neg_integer()}).
-record(dispd_val, {recpid = self() :: pid(),
                    post            :: {fun((any()) -> any()), any()}}).
-endif.
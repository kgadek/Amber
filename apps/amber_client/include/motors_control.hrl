-include("common.hrl").

-ifndef(MOTOR_CMD_H).
-define(MOTOR_CMD_H, true).
-record(motor_cmd, {
    speed  = 0         :: int32(),
    accel  = undefined :: uint32(),
    dist   = undefined :: uint32(),
    is_buf = undefined :: boolean()
}).
-endif.
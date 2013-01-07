-ifndef(COMMON_H).
-define(COMMON_H, true).

-include("drivermsg_pb.hrl").

-type int32()  :: -2147483648..2147483647.
-type uint32() :: 0..4294967295.

-record(amber_client_msg, {hdr = #driverhdr{} :: #driverhdr{},
                           msg = #drivermsg{} :: #drivermsg{}}).


%% @doc Klucz w słowniku. Pozwala na tłumaczenie otrzymanego pakietu na numer
%% pid() procesu odbiorcy.
-record(dispd_key, {dev_t  :: non_neg_integer(),
                    dev_i  :: non_neg_integer(),
                    synnum :: non_neg_integer()}).
%% @doc Wartość w słowniku. Pid procesu odbiorcy. Dodatkowo zawiera funkcję,
%% która zostaje wykonana po wysłaniu wiadomości do procesu.
-record(dispd_val, {recpid = self() :: pid(),
                    post            :: {fun((any()) -> any()), any()}}).
-endif.
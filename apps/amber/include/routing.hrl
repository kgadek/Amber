-ifndef(ROUTING_H).
-define(ROUTING_H, true).

-include("include/drivermsg_pb.hrl").

%% MNESIA:
-record(routing_node, {
	id     :: non_neg_integer(), % identyfikacja węzła; nie jest znana węzłowi
	mod_id :: {module(), tuple()},
	dev_ti :: {atom(), atom()} | any(), %% TODO
	conf   :: list()             % dodatkowa konfiguracja, na razie nie wykorzystywana
}).


%% Just util
-record(routing_msg, {
	hdr    :: #driverhdr{},
	msg    :: binary()
}).

-endif.
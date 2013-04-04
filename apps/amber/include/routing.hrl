-ifndef(ROUTING_H).
-define(ROUTING_H, true).

-include("include/drivermsg_pb.hrl").

%% MNESIA:
-record(routing_node, {
	id     :: non_neg_integer()  % identyfikacja węzła; nie jest znana węzłowi
            | '_' | '$1' | '$2',
	mod_id :: {module(), tuple()} 
            | '_' | '$1' | '$2',
	dev_ti :: {atom(), atom()} | any()  %% TODO
            | '_' | '$1' | '$2',
	conf   :: list()             % dodatkowa konfiguracja, na razie nie wykorzystywana
            | '_' | '$1' | '$2'
}).


%% Just util
-record(routing_msg, {
	hdr    :: #driverhdr{},
	msg    :: binary()
}).

-endif.
-module(amber_sup).
-behaviour(supervisor).

-include("include/routing.hrl").
-include("include/config.hrl").


-ifdef(TEST).
-define(SETTINGS_FILE, "../../amber/test/test_settings.config").
-else.
-define(SETTINGS_FILE, "apps/amber/src/settings.config").
-endif. 

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).



%% API + Supervisor callbacks
-export([start_link/0]).
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
	ok = db:initialize(),
	{ok, AllNodes} = file:consult(?SETTINGS_FILE),
	Nodes   = lists:filter(fun(X) -> is_record(X, supervised_node) end, AllNodes),
	NodesWR = [#supervised_node{mod_id = {router, {router}},
															conf   = []}
						| Nodes],
	Drivers = lists:filter(fun(X) -> is_record(X, supervised_driver) end, AllNodes),
	NodesWRN = length(NodesWR),
	DriversN = length(Drivers),  
	QN = fun({#supervised_node{mod_id = ModID, conf = Conf}, GID, DevTI}) ->
		mnesia:transaction(fun() -> mnesia:write(#routing_node{id = GID, mod_id = ModID, dev_ti = DevTI, conf = Conf}) end)
	end,
	QD = fun({#supervised_driver{mod_id = ModID, conf = Conf, dev_ti = DevTI}, GID}) ->
		mnesia:transaction(fun() -> mnesia:write(#routing_node{id = GID, mod_id = ModID, dev_ti = DevTI, conf = Conf}) end)
	end,
	lists:foreach(QN, lists:zipwith(fun(A,B) -> {A,B,make_ref()} end,
															NodesWR, lists:seq(0,        NodesWRN         -1))),
	lists:foreach(QD, lists:zip(Drivers, lists:seq(NodesWRN, NodesWRN+DriversN-1))),
  {ok, { {one_for_all, 30, 30}, [
  	?CHILD(comm_sup,   supervisor, [NodesWR]),
  	?CHILD(driver_sup, supervisor, [Drivers])
  ]}}.


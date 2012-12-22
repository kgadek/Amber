-module(comm_sup).
-behaviour(supervisor).

-include("include/config.hrl").

-export([start_link/1]).
-export([init/1]).

-define(CHILD(Mod,Args), {{Mod, Args, make_ref()}, {Mod, start_node, Args}, transient, 5000, worker, [Mod]}).


-spec start_link([#supervised_node{}])
			-> {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(Nodes) ->
	Children = lists:map(	fun(#supervised_node{	mod_id = {Mod, MID},
																							conf = Conf}) ->
													?CHILD(Mod, [MID, Conf])
												end, Nodes),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Children).

init(Children) ->
  {ok, {{one_for_one, 2, 10}, Children}}.

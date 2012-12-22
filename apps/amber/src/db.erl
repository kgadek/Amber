-module(db).
-export ([initialize/0]).

-include("include/routing.hrl").

initialize() ->
	mnesia:create_schema([node()]),  
	case mnesia:create_table(routing_node, [{attributes, record_info(fields, routing_node)},
																					{index,      [#routing_node.mod_id, #routing_node.dev_ti]},
                            							{ram_copies, [node()]}]) of
		{atomic, ok} ->
			ok;
		{aborted, {already_exists, routing_node}} ->
			true = ((record_info(fields, routing_node)              =:= mnesia:table_info(routing_node, attributes))
						and ([#routing_node.mod_id, #routing_node.dev_ti] =:= mnesia:table_info(routing_node, index)))
	end,
	mnesia:wait_for_tables([routing_node], 5000).
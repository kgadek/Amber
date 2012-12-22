-module(u).
-compile(export_all).

-include("include/drivermsg_pb.hrl").


start() ->
	dbg:start(),
	dbg:tracer().

tpl(Mod) ->
	tpl(Mod, '_').

tpl(Mod, Fun) ->
	dbg:tpl(Mod, Fun, '_', [{'_', [], [{return_trace}]}]).

tpl_vis(Mods) when is_list(Mods) ->
	[tpl_vis(M) || M <- Mods];
tpl_vis(Mod) when is_atom(Mod) ->
	[dbg:tpl(Mod, F, '_', [{'_', [], [{return_trace}]}]) || {F, _Ar} <- Mod:module_info(functions)].

p() ->
	dbg:p(all, c).

r(Mods) when is_list(Mods) ->
	[r(M) || M <- Mods];
r(Mod) when is_atom(Mod) ->
	{module, _} = code:load_file(Mod),
	code:purge(Mod).

r() ->
	r(?MODULE).

rel(Mod, Regname) ->
	sys:suspend(Mod),
	code:purge(Mod),
	code:load_file(Mod),
	sys:change_code(Regname, Mod, "0", []),
	sys:resume(Mod).

tmp() ->
	Hdr = #driverhdr{devicetype=2, deviceid=3}, %echo driver
	FullMsg = router:pack_msg(Hdr, <<22,33>>),
	{ok, Socket} = gen_udp:open(26234, [binary, {active, true}]),
	{Socket, FullMsg}.
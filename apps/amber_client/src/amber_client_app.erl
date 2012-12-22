-module(amber_client_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:start(amber).

start(_StartType, _StartArgs) ->
    amber_client_sup:start_link().

stop(_State) ->
    ok.

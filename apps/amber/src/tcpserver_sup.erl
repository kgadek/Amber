-module(tcpserver_sup).
-behaviour(supervisor).

% Based on http://learnyousomeerlang.com/buckets-of-sockets
% Thanks Fred!

-export([start_link/0, start_acceptor/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  % {ok, Port} = application:get_env(tcp_port),
  Port = 8899,
  % {ok, ListenersN} = application:get_env(tcp_active_listeners),
  ListenersN = 2,
  {ok, LSocket} = gen_tcp:listen(Port, [{active, once}]),
  spawn_link(fun() -> [start_acceptor() || _ <- lists:seq(1, ListenersN)] end),
  {ok, {{simple_one_for_one, 60, 3600},
       [{socket,
          {tcpserver_acceptor, start_link, [LSocket]},
          temporary, 1000, worker, [tcpserver_acceptor]}
       ]}}.

start_acceptor() ->
  supervisor:start_child(?MODULE, []).

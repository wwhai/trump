-module(trump_tcp_server_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

%% 服务端启动
start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


init([Port]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {trump_tcp_server, {trump_tcp_server, start, [Port]},
    Restart, Shutdown, Type, [trump_tcp_server]},

  {ok, {SupFlags, [AChild]}}.

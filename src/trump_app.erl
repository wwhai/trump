%%%-------------------------------------------------------------------
%% @doc trump application
%% @end
%%%-------------------------------------------------------------------

-module(trump_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, [{tcp_port,TcpPort},{udp_port,UdpPort}]} = application:get_env(trump, trump_options),
    io:format("trump-Server DEBUG --->>> Start tcp listener ~n"),
    trump_tcp_server_sup:start_link(TcpPort),
    io:format("trump-Server DEBUG --->>> Start udp listener ~n"),
    trump_udp_server_sup:start_link(UdpPort).

stop(_State) ->
    ok.
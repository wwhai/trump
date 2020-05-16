%%%-------------------------------------------------------------------
%% @doc trump application
%% @end
%%%-------------------------------------------------------------------

-module(trump_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("TRUMP Server DEBUG **************************************************************>>> Start cluster ~n"),
    trump_distribution_sup:start_link(),
    {ok, [{tcp_port,TcpPort},{udp_port,UdpPort}]} = application:get_env(trump, trump_options),
    io:format("TRUMP Server DEBUG **************************************************************>>> Start tcp listener ~n"),
    trump_tcp_server_sup:start_link(TcpPort),
    io:format("TRUMP Server DEBUG **************************************************************>>> Start udp listener ~n"),
    trump_udp_server_sup:start_link(UdpPort).

stop(_State) ->
    ok.
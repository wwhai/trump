%%%-------------------------------------------------------------------
%% @doc trump application
%% @end
%%%-------------------------------------------------------------------

-module(trump_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, [{tcp_port,TcpPort},{udp_port,UdpPort},{kv_port,KVPort}]} = application:get_env(trump, trump_options),

    io:format("TRUMP Server DEBUG **********************************>>> Start distribution server ~n"),
    trump_distribution_sup:start_link(),
    
    io:format("TRUMP Server DEBUG **********************************>>> Start tcp listener ~n"),
    trump_tcp_server_sup:start_link(TcpPort),

    io:format("TRUMP Server DEBUG **********************************>>> Start udp listener ~n"),
    trump_udp_server_sup:start_link(UdpPort),

    io:format("TRUMP Server DEBUG **********************************>>> Start KV server ~n"),
    trump_kv_server_sup:start_link(KVPort),

    io:format("TRUMP Server DEBUG **********************************>>> Start Mysql connector ~n"),
    trump_db_mysql_sup:start_link().


stop(_State) ->
    ok.
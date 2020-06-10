-module(udp_test_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_send, test_cast].


test_send(_) ->
    trump_udp_server_sup:start_link(5001),
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, "localhost", 5001, 
                <<8:8,"12345678123456781234567812345678DataIsHere">>).

test_cast(_) ->
    trump_udp_server_sup:start_link(5001),
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, "localhost", 5001, 
                <<26:8,"1234567812345678123456781234567812345678123456781234567812345678ThisIsMessage">>).




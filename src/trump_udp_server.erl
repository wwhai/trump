%%%-------------------------------------------------------------------
%%% @author wwhai
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(trump_udp_server).
-export([start/1]).
-export([start_link/2, loop/2]).

-define(UDP_OPTS, [binary, {reuseaddr, true}]).

start(Port) ->
  ok = esockd:start(),
  Opts = [{udp_options, ?UDP_OPTS}],
  MFA = {?MODULE, start_link, []},
  esockd:open_udp('urap', Port, Opts, MFA).

start_link(Transport, Peer) ->
  {ok, spawn_link(?MODULE, loop, [Transport, Peer])}.

%%
%%
%%
loop(Transport, Peer) ->
  receive
    {datagram, From, UdpData} ->
      io:format("UDP DEBUG ===>>> Host: ~s , Data:~p~n", [esockd:format(Peer), UdpData]),
      From ! {datagram, Peer, <<"OK">>},
      spawn(fun() -> loop(Transport, Peer) end) 
  end.
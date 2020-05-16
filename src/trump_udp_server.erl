%%%-------------------------------------------------------------------
%%% @author wwhai
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(trump_udp_server).
-export([start/1]).
-export([hand_udp_data/1]).
-export([start_link/2, loop/2]).

-define(UDP_OPTS, [binary, {reuseaddr, true}]).

start(Port) ->
  ok = esockd:start(),
  Opts = [{udp_options, ?UDP_OPTS}],
  MFA = {?MODULE, start_link, []},
  esockd:open_udp(urap_connector, Port, Opts, MFA).

start_link(Transport, PeerSocket) ->
  {ok, spawn_link(?MODULE, loop, [Transport, PeerSocket])}.

%%
%% Peer 是客户端的信息
%%
loop(Transport, PeerSocket) ->
  receive
    {datagram, LocalSocket, UdpData} ->
      io:format("UDP DEBUG ===>>>LocalSocket:~p PeerSocket: ~s , Data:~p~n", [LocalSocket,esockd:format(PeerSocket), UdpData]),
      spawn(fun () ->
          hand_udp_data(UdpData)
      end),
      loop(Transport, PeerSocket)
  end.

hand_udp_data(_P)->
  %% From ! {ok},
  ok.
%%%-------------------------------------------------------------------
%%% @author wwhai
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(trump_udp_server).

-include_lib("kernel/include/logger.hrl").

-include("trump_protocol.hrl").

-export([start/1]).
-export([start_link/2, loop/2]).

%%
%% ACL 表
%%

-define(TRUMP_ACL_TABLE, trump_acl_permission_table).

-record(trump_acl_permission, {client_id, uuid}).

-define(UDP_OPTS, [binary, {reuseaddr, true}]).

start(Port) ->
  ok = esockd:start(),
  Opts = [{udp_options, ?UDP_OPTS}],
  MFA = {?MODULE, start_link, []},
  EtsOptions =
    [
      named_table,
      public,
      set,
      {keypos, #trump_acl_permission.client_id},
      {write_concurrency, true},
      {read_concurrency, true}
    ],
  ets:new(?TRUMP_ACL_TABLE, EtsOptions),
  esockd:open_udp(urap_connector, Port, Opts, MFA).


start_link(Transport, PeerSocket) -> {ok, spawn_link(?MODULE, loop, [Transport, PeerSocket])}.

%%
%%
%%

loop(Transport, PeerSocket) ->
  receive
    %% ListenSocket：当前服务端监听的端口
    %% PeerSocket：发送数据的客户端Socket封装
    %% UdpData：数据字节流
    {datagram, ListenSocket, UdpData} ->
      io:format(
        "UDP DEBUG ===>>> ListenSocket :~p PeerSocket: ~s , Data:~p~n",
        [ListenSocket, esockd:format(PeerSocket), UdpData]
      ),
      spawn(fun () -> hand_udp_data(UdpData, Transport, PeerSocket) end),
      loop(Transport, PeerSocket)
  end.

%%
%% 处理消息
%% {ok, Socket} = gen_udp:open(0, [binary]).
%% gen_udp:send(Socket, "localhost", 5001, <<"8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>).
%%

hand_udp_data(UdpData, {udp, _Server, TransSocket}, {IP, Port}) ->
  %% 收到的数据包格式：
  %% 1:SEND数据包
  %%  ----------------------
  %% ｜TYPE｜CLIENT_ID｜DATA|
  %%  ----------------------
  %% 2:PUBLISH数据包
  %%  -------------------------------------
  %% ｜TYPE｜CLIENT_ID｜PEER_CLIENT_ID|DATA|
  %%  -------------------------------------
  %% 3:广播数据包
  %%  -------------------------------
  %% ｜TYPE｜CLIENT_ID｜GROUP_ID|DATA|
  %%  -------------------------------
  try
    <<Type:8, ClientId:32/binary, Payload/binary>> = UdpData,
    case check_permission(ClientId) of
      ok -> ok;
      deny -> deny
    end,
    case (Type) of
       ?SEND ->
        <<Data/binary>> = Payload,
        logger:info("SEND ClientId is:~p Data is:~p~n", [ClientId, Data]);

       ?PUBLISH ->
        <<PeerClientId:32/binary, Data/binary>> = Payload,
        logger:info(
          "PUBLISH ClientId is:~p PeerClientId is ~p Data is:~p~n",
          [ClientId, PeerClientId, Data]
        );

       ?CAST ->
        <<GroupId:32/binary, Data/binary>> = Payload,
        logger:info("CAST ClientId is:~p GroupId is ~p Data is:~p~n", [ClientId, GroupId, Data]);

      Other ->
        gen_udp:send(TransSocket, IP, Port, <<?PROTOCOL_ERROR>>),
        logger:info("Other Type is:~p~n", [Other])
    end
  catch
    %% 协议错误
    Reason:Info ->
      gen_udp:send(TransSocket, IP, Port, <<?PROTOCOL_ERROR>>),
      logger:info("Error:~p Info: ~p~n", [Reason, Info])
  end.

%%
%% 生成UUID 客户端用ClientID来换取Token(UUID) 然后UUID作为口令提交数据
%%
proclaim_uuid(ClientId) ->
  Row = #trump_acl_permission{client_id = ClientId, uuid = uuid:to_string()},
  ets:insert(?TRUMP_ACL_TABLE, Row), 
  Row.

%%
%% 检查是否有权限发布消息
%% K：ClientId V：UUID
%%

check_permission(ClientId) ->
  Object = ets:match_object(?TRUMP_ACL_TABLE, {trump_acl_permission, ClientId, '$1'}),
  case Object of
    [] -> deny;
    _Other -> ok
  end.

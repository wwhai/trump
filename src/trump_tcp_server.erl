%%--------------------------------------------------------------------
-module(trump_tcp_server).
-behaviour(gen_server).
-include("trump_protocol.hrl").
-include_lib("kernel/include/logger.hrl").
-export([start_link/2, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% 客户端的信息
-record(trump_client_info, {client_id, ip, auth = false, transport, socket}).
%% 进程状态数据，包含了一个客户端的信息
-record(trump_connection_state, {transport, socket, trump_client_info}).

%% ETS 表名
-define(TRUMP_CLIENT_TABLE, trump_client_table).
%% 等待认证事件5S
-define(EXPIRE_TIME, 3000).

%% start
start(Port) when is_integer(Port) ->
  EtsOptions = [named_table, public, set, {keypos, #trump_client_info.client_id}, {write_concurrency, true}, {read_concurrency, true}],
  ets:new(?TRUMP_CLIENT_TABLE, EtsOptions),
  {ok, TcpOptions} = application:get_env(trump, tcp_options),
  MFA = {?MODULE, start_link, []},
  esockd:start(),
  esockd:open(trap_connector, Port, TcpOptions, MFA).

start_link(Transport, Socket) ->

  {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Socket]])}.



init([Transport, Socket]) ->
  case Transport:wait(Socket) of
    {ok, NewSocket} ->
      {ok, {IP, Port}} = Transport:peername(NewSocket),
      io:format("TRUMP TCP DEBUG --->>> New socket connected: Ip is :~p and port is ~p ~n", [IP, Port]),
      Transport:setopts(Socket, [{active, once}]),
      InitState = #trump_connection_state{transport = Transport, socket = NewSocket, trump_client_info = #trump_client_info{ip = IP}},
      erlang:send_after(?EXPIRE_TIME, self(), wait_for_auth),
      gen_server:enter_loop(?MODULE, [], InitState);
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% ping包
handle_cast(ping, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> pong ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?PONG>>),
  {noreply, State};
%% protocol_error
handle_cast(protocol_error, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> protocol_error ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?PROTOCOL_ERROR>>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% connect
handle_cast(connect, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> connect ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?CONNECT_OK>>),
  {noreply, State};

%% auth 认证客户端,改变状态即可
handle_cast({auth, PayLoad}, State) ->

  <<ClientId:32/binary, _/binary>> = PayLoad,
  io:format("TRUMP TCP DEBUG --->>> Require auth and clientid is:~p ~n", [ClientId]),
  case trump_db_mysql:auth(ClientId) of
    false ->
      io:format("TRUMP TCP DEBUG --->>> Auth failure~n"),
      gen_server:cast(self(), auth_failure),
      {noreply, State};
    true ->
      io:format("TRUMP TCP DEBUG --->>> Auth success:~p ~n", [ClientId]),
      Object = ets:match_object(?TRUMP_CLIENT_TABLE , {trump_client_info , ClientId ,'$1', '$2' , '$3', '$4' } ),
      case Object of 
        %% ETS没有记录，直接连接
        []->
          %% 构建客户端信息
          io:format("TRUMP TCP DEBUG --->>> This client is First join ~n"),
          #trump_connection_state{transport = Transport, socket = Socket, trump_client_info = #trump_client_info{ip = IP}} = State,
          TrumpClientInfo = #trump_client_info{ client_id = ClientId ,auth = true,transport = Transport,socket = Socket,ip = IP},    
          ets:insert(?TRUMP_CLIENT_TABLE,  TrumpClientInfo),
          %% 通知分布层
          gen_server:cast(trump_distribution, {client_connect, node(), ClientId}),
          NewState = State#trump_connection_state{trump_client_info = TrumpClientInfo};
        %% ETS有记录，把前者踢下去
        %% [{trump_client_info, _C, _I, _A , _T, Socket}] = ets:match_object(trump_client_table,{trump_client_info,<<"4d45d94142276ad38364049c56d8ed42">>,'$2', '$3', '$4','$5'}).
        [{trump_client_info, _C, _I, _A , Transport, BeforeSocket}] ->
          %% 踢出前者
          gen_server:cast(self(), {kick_out,BeforeSocket}),
          %% 构建新的进程状态
          #trump_connection_state{transport = Transport, socket = Socket, trump_client_info = #trump_client_info { ip = IP } } = State,
          %% 直接生成新的客户端信息
          TrumpClientInfo = #trump_client_info{ client_id = ClientId ,auth = true,transport = Transport,socket = Socket,ip = IP},    
          io:format("TRUMP TCP DEBUG --->>> Build new socket: ~p ~n",[Socket]),
          ets:insert(?TRUMP_CLIENT_TABLE,  TrumpClientInfo),
          %% 通知分布层
          gen_server:cast(trump_distribution, {client_connect, node(), ClientId}),
          NewState = State#trump_connection_state{trump_client_info = TrumpClientInfo}
        end,
        {noreply, NewState}
  end;

%% 认证成功给客户端发送消息
handle_cast(auth_ok, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  send_to_client(Transport, Socket, <<?AUTH_OK>>),
  {noreply, State};

%% 认证失败
handle_cast(auth_failure, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> Auth failure and Socket is:~p ~n", [Socket]),
  send_to_client(Transport, Socket, <<?AUTH_FAIL>>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% 踢下线
handle_cast({kick_out,BeforeSocket}, #trump_connection_state{transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> Socket:[~p] Already connect,Kickout this socket ~n",[BeforeSocket]),
  send_to_client(Transport, BeforeSocket, <<?KICKOUT>>),
  Transport:fast_close(BeforeSocket),
  {stop, normal, State};

%% 为止异常直接关闭客户端，然后恢复状态  
handle_cast({error, Socket, Message}, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("TRUMP TCP DEBUG --->>> error ~p~n", [Socket]),
  Transport:send(Socket, Message),
  Transport:setopts(Socket, [{active, once}]),
  {stop, normal, State}.


%%
%% 消息接收处理入口
%%

handle_info({tcp, _RemoteSocket, BinData}, State) ->
  DataLength = byte_size(BinData),
  io:format("TRUMP TCP DEBUG --->>> Raw BinData is ~p~n", [BinData]),

  case DataLength > 1 of
    false ->
      gen_server:cast(self(), protocol_error),
      {noreply, State};
    true ->
      try
        <<Type:8, PayLoad/binary>> = BinData,
        case Type of
          ?PING ->
            gen_server:cast(self(), pong);
          ?CONNECT ->
            gen_server:cast(self(), connect);
          ?AUTH ->
            gen_server:cast(self(), {auth, PayLoad});
          _ ->
            ok
        end,
        {noreply, State}

      catch
        Reason:E ->
          io:format("TRUMP TCP DEBUG --->>> Error :~p Reason : ~p ~n", [E, Reason]),
          gen_server:cast(self(), protocol_error),
          {noreply, State}
      end
  end;

%%
%% TCP过程出现了ERROR
%%
handle_info({tcp_error, Socket, Reason}, State) ->
  io:format("TRUMP TCP DEBUG --->>> handle_info tcp_error ~p , Error from: ~p~n", [Reason, Socket]),
  {stop, normal, State};

%%
%% 客户端连接主动断开:  
%% ETS结构:{trump_client_info,client_id, {127,0,0,1}, auth,esockd_transport,socket}
%% 
handle_info({tcp_closed, Socket}, State) ->
  io:format("TRUMP TCP DEBUG --->>> Socket cloesd: ~p ~n", [Socket]),
  %% 从ETS中删除Socket
  ets:match_delete(?TRUMP_CLIENT_TABLE, {'_',{trump_client_info,'$1','$2', '$3', '$4', Socket}}),
  %% 通知集群
  #trump_connection_state{ trump_client_info = #trump_client_info{client_id = ClientId}} = State,
  gen_server:cast(trump_distribution,{client_disconnect, node(), ClientId}),
  {stop, normal, State};


%%
%% 这个是定时检查认证状态,防止恶意连接
%%
handle_info(wait_for_auth, #trump_connection_state{trump_client_info = #trump_client_info{auth = Auth}} = State) ->
  case Auth of
    true ->
      {noreply, State};
    false ->
      gen_server:cast(self(), auth_failure),
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% 给客户端发送消息
%%
send_to_client(Transport, Socket, Data) ->
  Transport:send(Socket, Data),
  Transport:setopts(Socket, [{active, once}]).
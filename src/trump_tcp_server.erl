%%--------------------------------------------------------------------
-module(trump_tcp_server).
-behaviour(gen_server).
-export([start_link/2, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% 进程状态数据
-record(trump_client_info, {client_id, ip, auth = false, transport, socket}).
-record(trump_connection_state, {transport, socket, trump_client_info}).

%% ETS 表名
-define(TRUMP_CLIENT_TABLE, trump_client_table).
%% 等待认证事件5S
-define(EXPIRE_TIME, 3000).
%% 报文类型
-define(PING, 0).
-define(PONG, 1).
-define(CONNECT, 2).
-define(CONNECT_OK, 3).
-define(CONNECT_FAIL, 4).
-define(AUTH, 5).
-define(AUTH_OK, 6).
-define(AUTH_FAIL, 7).
-define(SEND, 8).
-define(SEND_OK, 9).
-define(SEND_FAIL, 10).
-define(PUBLISH, 11).
-define(PUBLISH_OK, 12).
-define(PUBLISH_FAIL, 13).
-define(CREATE_GROUP, 14).
-define(CREATE_GROUP_OK, 15).
-define(CREATE_GROUP_FAIL, 16).
-define(REMOVE_GROUP, 17).
-define(REMOVE_GROUP_OK, 18).
-define(REMOVE_GROUP_FAIL, 19).
-define(JOIN_GROUP, 20).
-define(JOIN_GROUP_OK, 21).
-define(JOIN_GROUP_FAIL, 22).
-define(EXIT_GROUP, 23).
-define(EXIT_GROUP_OK, 24).
-define(EXIT_GROUP_FAIL, 25).
%% 广播
-define(CAST, 26).
-define(CAST_OK, 27).
-define(CAST_FAIL, 28).
%% 协议错误
-define(PROTOCOL_ERROR, 29).
%% 被踢下线
-define(KICKOUT, 30).


%% start
start(Port) when is_integer(Port) ->
  EtsOptions = [named_table, public, set, {keypos, #trump_client_info.client_id}, {write_concurrency, true}, {read_concurrency, true}],
  ets:new(?TRUMP_CLIENT_TABLE, EtsOptions),
  {ok, TcpOptions} = application:get_env(trump, tcp_options),
  MFA = {?MODULE, start_link, []},

  {ok, MysqlOptions} = application:get_env(trump, mysql_options),
  case mysql:start_link(MysqlOptions) of
    {ok, MysqlPid} ->
      register(mysql_connector, MysqlPid),
      {ok, _, [[Version]]} = mysql:query(mysql_connector, <<"SELECT version()">>),
      io:format("trump-Server DEBUG --->>> Mysql has connected,Pid is ~p ,version is ~p ~n", [MysqlPid, Version]),
      esockd:start(),
      esockd:open(trap_connector, Port, TcpOptions, MFA);
    {error, {_ErrorCode, _StatementId, ErrorMessage}} ->
      io:format("trump-Server DEBUG --->>> Mysql connect error reason is ~p ~n", [ErrorMessage]);
    {error, {{badmatch, {error, econnrefused}}, _}} ->
      io:format("trump-Server DEBUG --->>> Mysql connect refused maybe network or port unreached.Your options is ~p ~n", [MysqlOptions]);
    {error, Other} ->
      io:format("trump-Server DEBUG --->>> Mysql connect error reason is ~p ~n", [Other]);
    ignore ->
      ignore
  end.

start_link(Transport, Socket) ->

  {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Socket]])}.



init([Transport, Socket]) ->
  case Transport:wait(Socket) of
    {ok, NewSocket} ->
      {ok, {IP, Port}} = Transport:peername(NewSocket),
      io:format("trump-Server DEBUG --->>> New socket connected: Ip is :~p and port is ~p ~n", [IP, Port]),
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
  io:format("trump-Server DEBUG --->>> pong ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?PONG>>),
  {noreply, State};
%% protocol_error
handle_cast(protocol_error, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("trump-Server DEBUG --->>> protocol_error ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?PROTOCOL_ERROR>>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% connect
handle_cast(connect, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("trump-Server DEBUG --->>> connect ~p~n", [Socket]),
  send_to_client(Transport, Socket, <<?CONNECT_OK>>),
  {noreply, State};

%% auth 认证客户端,改变状态即可
handle_cast({auth, PayLoad}, State) ->

  <<ClientId:32/binary, _/binary>> = PayLoad,
  io:format("trump-Server DEBUG --->>> Require auth and clientid is:~p ~n", [ClientId]),

  {ok, Sql} = application:get_env(trump, authsql),
  {ok, StatementId} = mysql:prepare(mysql_connector, Sql),
  R = mysql:execute(mysql_connector, StatementId, [ClientId]),

  case R of
    {ok, [<<"id">>,<<"client_id">>], []} ->
      io:format("trump-Server DEBUG --->>> Auth failure~n"),
      gen_server:cast(self(), auth_failure),
      {noreply, State};
    {ok, [<<"id">>,<<"client_id">>], [[_DBIdx,CId]]} ->
      io:format("trump-Server DEBUG --->>> Auth success:~p ~n", [CId]),
      Object = ets:match_object(?TRUMP_CLIENT_TABLE , {trump_client_info , ClientId , '$2' , '$3', '$4' , '$5' } ),
      case Object of 
        %% ETS没有记录，直接连接
        []->
          %% 构建客户端信息
          io:format("trump-Server DEBUG --->>> This client is First join ~n"),
          #trump_connection_state{transport = Transport, socket = Socket, trump_client_info = #trump_client_info{ip = IP}} = State,
          trumpClientInfo = #trump_client_info{ client_id = ClientId ,auth = true,transport = Transport,socket = Socket,ip = IP},    
          ets:insert(?TRUMP_CLIENT_TABLE,  trumpClientInfo),
          NewState = State#trump_connection_state{trump_client_info = trumpClientInfo};
        %% ETS有记录，把前者踢下去
        %% [{trump_client_info, _C, _I, _A , _T, Socket}] = ets:match_object(trump_client_table,{trump_client_info,<<"4d45d94142276ad38364049c56d8ed42">>,'$2', '$3', '$4','$5'}).
        [{trump_client_info, _C, _I, _A , Transport, BeforeSocket}] ->
          %% 踢出前者
          gen_server:cast(self(), {kick_out,BeforeSocket}),
          %% 构建新的进程状态
          #trump_connection_state{transport = Transport, socket = Socket, trump_client_info = #trump_client_info { ip = IP } } = State,
          %% 直接生成新的客户端信息
          trumpClientInfo = #trump_client_info{ client_id = ClientId ,auth = true,transport = Transport,socket = Socket,ip = IP},    
          io:format("trump-Server DEBUG --->>> Build new socket: ~p ~n",[Socket]),
          ets:insert(?TRUMP_CLIENT_TABLE,  trumpClientInfo),
          NewState = State#trump_connection_state{trump_client_info = trumpClientInfo}
        end,
        {noreply, NewState}
  end;

%% 认证成功给客户端发送消息
handle_cast(auth_ok, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  send_to_client(Transport, Socket, <<?AUTH_OK>>),
  {noreply, State};

%% 认证失败
handle_cast(auth_failure, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("trump-Server DEBUG --->>> Auth failure and Socket is:~p ~n", [Socket]),
  send_to_client(Transport, Socket, <<?AUTH_FAIL>>),
  gen_tcp:close(Socket),
  {stop, normal, State};

%% 踢下线
handle_cast({kick_out,BeforeSocket}, #trump_connection_state{transport = Transport} = State) ->
  io:format("trump-Server DEBUG --->>> Socket:[~p] Already connect,Kickout this socket ~n",[BeforeSocket]),
  send_to_client(Transport, BeforeSocket, <<?KICKOUT>>),
  Transport:fast_close(BeforeSocket),
  {stop, normal, State};

%% 为止异常直接关闭客户端，然后恢复状态  
handle_cast({error, Socket, Message}, #trump_connection_state{socket = Socket, transport = Transport} = State) ->
  io:format("trump-Server DEBUG --->>> error ~p~n", [Socket]),
  Transport:send(Socket, Message),
  Transport:setopts(Socket, [{active, once}]),
  {stop, normal, State}.


%%
%% 消息接收处理入口
%%

handle_info({tcp, _RemoteSocket, BinData}, State) ->
  DataLength = byte_size(BinData),
  io:format("trump-Server DEBUG --->>> Raw BinData is ~p~n", [BinData]),

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
          io:format("trump-Server DEBUG --->>> Error :~p Reason : ~p ~n", [E, Reason]),
          gen_server:cast(self(), protocol_error),
          {noreply, State}
      end
  end;


handle_info({tcp_error, Socket, Reason}, State) ->
  io:format("trump-Server DEBUG --->>> handle_info tcp_error ~p , Error from: ~p~n", [Reason, Socket]),
  %% ets:match_delete(?TRUMP_CLIENT_TABLE, {'_', #trump_client_info{socket = Socket, _ = '_'}}),
  {stop, normal, State};

%% 连接断开:
%% ETS结构:{trump_client_info,client_id, {127,0,0,1}, auth,esockd_transport,socket}
handle_info({tcp_closed, Socket}, State) ->
  io:format("trump-Server DEBUG --->>> Socket cloesd: ~p ~n", [Socket]),
  %% 从ETS中删除Socket
  ets:match_delete(?TRUMP_CLIENT_TABLE, {'_',{trump_client_info,'$1','$2', '$3', '$4', Socket}}),
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


%% 给客户端发送消息
send_to_client(Transport, Socket, Data) ->
  Transport:send(Socket, Data),
  Transport:setopts(Socket, [{active, once}]).
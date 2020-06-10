-module(trump_db_mysql).
-behaviour(gen_server).

-export([start/0, query/2, auth/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start() ->
    start_link().
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    {ok , MysqlOptions} = application:get_env(trump, mysql_options),
    {ok , SSL} =  application:get_env(trump, mysql_ssl),
    %% 是否开启SSL
    case SSL of 
      on -> 
        {ok, SSLOptions} = application:get_env(trump, mysql_ssl_options),
        R = mysql:start_link(MysqlOptions ++ [{ssl,SSLOptions}]);
      off -> 
        R = mysql:start_link(MysqlOptions)
    end,
  %%
  %% 连接Mysql
  %%
  case R of
    {ok, MysqlPid} ->
      register(mysql_connector, MysqlPid),
      {ok, _, [[Version]]} = mysql:query(mysql_connector, <<"SELECT version()">>),
      io:format("TRUMP TCP DEBUG --->>> Mysql has connected,Pid is ~p ,version is ~p ~n", [MysqlPid, Version]);
    {error, {_ErrorCode, _StatementId, ErrorMessage}} ->
      io:format("TRUMP TCP DEBUG --->>> Mysql connect error reason is ~p ~n", [ErrorMessage]);
    {error, {{badmatch, {error, econnrefused}}, _}} ->
      io:format("TRUMP TCP DEBUG --->>> Mysql connect refused maybe network or port unreached.Your options is ~p ~n", [MysqlOptions]);
    {error, Other} ->
      io:format("TRUMP TCP DEBUG --->>> Mysql connect error reason is ~p ~n", [Other]);
    ignore ->
      ignore
  end,
    {ok, #state{}}. 

handle_call(_Request, _From, _State) ->
    {reply, ok, _State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%-----------------------------------------------------------------
%% Private function
%%-----------------------------------------------------------------

query(Sql,Args) ->
    {ok, StatementId} = mysql:prepare(mysql_connector, Sql),
    mysql:execute(mysql_connector, StatementId, Args).
  
auth(ClientId) ->
    {ok, Sql} = application:get_env(trump, authsql),
    case query(Sql, [ClientId]) of
    {ok, [<<"id">>,<<"client_id">>], []} ->
        false;
    {ok, [<<"id">>,<<"client_id">>], [[_1,_2]]} ->
        true
  end.
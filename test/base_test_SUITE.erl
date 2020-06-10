-module(base_test_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> 
	[mysql_query,mysql_query_use_ssl].

mysql_query_use_ssl(_) ->
	application:start(mysql),
	MysqlOptions = [{host,"125.74.48.134"},
	{port,3308},
	{user,"root"},
	{password,"root"},
	{database,"mqtt"},
	{ssl,[{server_name_indication,disable},
		  {cacertfile,"/Users/wangwenhai/github/trump/config/ca.pem"},
		  {certfile,"/Users/wangwenhai/github/trump/config/client-cert.pem"},
		  {keyfile,"/Users/wangwenhai/github/trump/config/client-key.pem"}]}],
	R = mysql:start_link(MysqlOptions),
	case R of
		{ok, MysqlPid} ->
		register(mysql_connector, MysqlPid),
		{ok, _, [[Version]]} = mysql:query(mysql_connector, <<"SELECT version()">>),
		io:format("TRUMP TCP DEBUG --->>> Mysql has connected,Pid is ~p ,version is ~p ~n", [MysqlPid, Version]);
		{error, Other} ->
		io:format("TRUMP TCP DEBUG --->>> Mysql connect error reason is ~p ~n", [Other]);
		ignore ->
		ignore
	end.

mysql_query(_)->
	application:start(mysql),
	MysqlOptions = [{host,"125.74.48.134"},
	{port,3308},
	{user,"root"},
	{password,"root"},
	{database,"mqtt"}],
	R = mysql:start_link(MysqlOptions),
	case R of
		{ok, MysqlPid} ->
		register(mysql_connector, MysqlPid),
		{ok, _, [[Version]]} = mysql:query(mysql_connector, <<"SELECT version()">>),
		io:format("TRUMP TCP DEBUG --->>> Mysql has connected,Pid is ~p ,version is ~p ~n", [MysqlPid, Version]);
		{error, Other} ->
		io:format("TRUMP TCP DEBUG --->>> Mysql connect error reason is ~p ~n", [Other]);
		ignore ->
		ignore
	end.


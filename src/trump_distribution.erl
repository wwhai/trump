%% Copyright 2020 wangwenhai
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%%-------------------------------------------------------------------
%%% @author wwhai
%%% 分布层，主要用来实现分布式集群
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(trump_distribution).
-behaviour(gen_server).
-export([start/0]).
-export([synchroized_cluster/0]).
-export([broad_cast_local/0]).
-export([get_nodes/0]).
-export([get_clients/0]).
-export([add_node/1]).
%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).
%% 集群状态下的客户端，多了一个Node属性
-record(trump_cluster_client, {trump_cluster_node, client_id}).
%% 节点保存表
-define(CLUSTER_NODE_TABLE,trump_cluster_node_table).
%% 集群的客户端和节点之间的关系表
-define(CLUSTER_CLIENT_TABLE,trump_cluster_client_table).
%% 单节点的客户端表
-define(TRUMP_CLIENT_TABLE, trump_client_table).

%% 
%% start_link/0
%% ====================================================================
start() ->
    mnesia:delete_schema(get_nodes()),
    mnesia:create_schema(get_nodes()),
    mnesia:start(),                                      
    %% 集群内的客户端存放表                                          
    mnesia:create_table(?CLUSTER_CLIENT_TABLE, [{ram_copies, get_nodes()},  
                                              {attributes, record_info(fields, trump_cluster_client)}]),  
    mnesia:add_table_index(?CLUSTER_CLIENT_TABLE, pid),                                        
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
handle_call(_Request, _From, _State) ->
    Reply = ok,
    {reply, Reply, _State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
%%
%% Start distribution layer
%%
get_nodes()->
    [node() | nodes()].
    % {ok,{{mode,Mode,cluster_nodes,  Nodes}}} = application:get_env(trump, cluster),
    % case Mode of
    %     static ->
    %         Nodes;
    %     discovery ->
    %         [node() | nodes()]
    % end.

%%
%% 动态添加一个节点
%%
add_node(Node)->
    %% 首先判断是否ping的到
    %% if ping -> insert
    %% else -> log
    case net_adm:ping(Node) of 
        pong ->
            io:format("Distribution LOG =*-*-*-*-*-=> Node :~p join cluster success!Cluster nodes is~p:~n",[Node,get_nodes()]);
        pang ->
            io:format("Distribution LOG =*-*-*-*-*-=> Node :~p join cluster failed!~n",[Node])
        end.
%%
%% ets:match_object(trump_connection_table , {trump_client_info , '$1', '$2' , '$3', '$4' , '$5' })
%%
get_clients()->
    ets:match_object(?TRUMP_CLIENT_TABLE , {trump_client_info , '$1' , '$2' , '$3', '$4' , '$5' }).

%%
%% 同步集群数据
%%
synchroized_cluster()->
    ok.
%%
%% 向集群广播本地数据
%%
broad_cast_local()->
    ok.

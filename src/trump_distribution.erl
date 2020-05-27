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
-export([get_nodes/0]).
-export([get_clients/0]).
-export([add_node/1]).
-export([broad_cast_event/1]).
-export([hand_cluster_event/1]).
%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% 默认OTP的状态
-record(state, {}).
%% 集群内的客户端 [节点，客户端ID]
-record(trump_cluster_client, {node, client_id}).
%% 集群内的订阅关系表 [节点-Topic-客户端ID]
-record(trump_cluster_sub, {node, topic, client_id}).
%% 本地订阅关系
-record(trump_local_sub, { uuid,topic, client_id }).

%% 节点保存表
-define(CLUSTER_NODE_TABLE,trump_node_table).
%% 集群的客户端和节点之间的关系表
-define(CLUSTER_CLIENT_TABLE,trump_cluster_client_table).
%% 单节点的客户端表
-define(TRUMP_CLIENT_TABLE, trump_client_table).
%% 订阅关系表
-define(CLUSTER_SUB_TABLE, trump_cluster_sub_table).
%% 订阅关系表
-define(LOCAL_SUB_TABLE, trump_local_sub_table).


%% 
%% start_link/0
%%
start() ->
    mnesia:delete_schema(get_nodes()),
    mnesia:create_schema(get_nodes()),
    mnesia:start(),                                      
    %% 集群内的客户端存放表                                          
    mnesia:create_table(?CLUSTER_CLIENT_TABLE, [{ram_copies, get_nodes()}, {attributes, record_info(fields, trump_cluster_client)}]),
    mnesia:add_table_index(?CLUSTER_CLIENT_TABLE, pid),        
    %% 集群内的订阅关系表
    mnesia:create_table(?CLUSTER_SUB_TABLE, [{ram_copies, get_nodes()}, {attributes, record_info(fields, trump_cluster_sub)}]),  
    mnesia:add_table_index(?CLUSTER_SUB_TABLE, pid),        
    %% 本地订阅关系表
    ets:new(?LOCAL_SUB_TABLE,  [named_table, public, set, {keypos, #trump_local_sub.uuid}, {write_concurrency, true}, {read_concurrency, true}]),                                
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
init([]) ->
    {ok, #state{}}.

%%
%% handle_call/3
%%
handle_call(_Request, _From, _State) ->
    Reply = ok,
    {reply, Reply, _State}.

%% 
%% handle_cast/2
%% 客户端掉线的时候，需要通知分布层
%% 然后分布层再广播出去
%%
handle_cast({client_disconnect, Node , ClientId}, State) ->
    io:format("Distribution LOG =-----=> Client:[~p] on Node:~p disconnect ~n",[ClientId, Node]),
    broad_cast_event({client_disconnect, Node , ClientId}),
    {noreply, State};

%%
%% 客户端上线通知
%%
handle_cast({client_connect, Node , ClientId}, State) ->
    io:format("Distribution LOG =-----=> Client:[~p] on Node:~p connect ~n",[ClientId, Node]),
    broad_cast_event({client_connect, Node , ClientId}),
    {noreply, State};

%%
%% OTP callback
%%
handle_cast(Msg, State) ->
    io:format("Distribution LOG =*-*-*-*-*-=> handle_cast msg :[~p] ~n",[Msg]),
    {noreply, State}.

%%
%% handle_info/2
%%
handle_info(_Info, State) ->
    {noreply, State}.

%%
%% terminate/2
%%
terminate(_Reason, _State) ->
    ok.

%%
%% code_change/3
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_nodes()->
    [node() | nodes()].

%%
%% 动态添加一个节点
%%
add_node(Node)->
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
%% 当本地收到事件以后，需要广播出去，广播的办法是：遍历Node，然后广播一个元组出去
%% 其中广播的 NMFA = Node,trump_distriubtion,hand_cluster_event,{Node,Event:{message,args...}}
%%
broad_cast_event(Msg)->
    %% 获取所有节点
    NodeList = nodes(),
    %% 定义一个函数，用来给远程节点发送通知
    F = fun (Node)  ->
            %% 调用远程节点的 hand_cluster_event 函数
            case rpc:call(Node, trump_distriubtion, hand_cluster_event, Msg) of 
                %% 如果节点坏了，打印log
                {badrpc, _} ->
                    io:format("Distribution LOG =*-*-*-*-*-=> Node :~p broadcast failed!~n",[ Node ]),
                    badrpc;
                    _Other ->
                    ok
            end
        end,
    %% 遍历List    
    lists:foreach(F,NodeList).

%%
%% 集群的消息统一入口,这里处理集群内的通知
%% 收到客户端上线通知，就在集群表中增加一条记录
%% 收到客户端下线通知，就在集群表中删除一条记录
%% 集群表结构:
%% [ Node                    |             ClientId            |
%% |───────────────────────────────────────────────────────────|
%% | n1@1.0.0.1              |<<"111111111111111111111111111">>|
%% | n2@2.0.0.1              |<<"222222222222222222222222222">>|
%% | n3@3.0.0.1              |<<"222222222222222222222222222">>|
%%  ───────────────────────────────────────────────────────────
%% 节点上下线不需要建表，只需要通知即可
%% -------------------------------------------------------------------------------------

%%
%% 客户端上线
%%
hand_cluster_event({client_connect, Node , ClientId})->
    io:format("Distribution LOG =-----=> Client:[~p] on Node:~p connect ~n",[ClientId, Node]);
%%
%% 客户端下线
%%
hand_cluster_event({client_disconnect, Node , ClientId})->
    io:format("Distribution LOG =-----=> Client:[~p] on Node:~p disconnect ~n",[ClientId, Node]);
%%
%% 节点下线
%%
hand_cluster_event({node_disconnect, Node})->
    io:format("Distribution LOG =-----=> Node:~p disconnect ~n",[Node]);
%%
%% 节点上线
%%
hand_cluster_event({node_connect, Node})->
    io:format("Distribution LOG =-----=> Node:~p connect ~n",[Node]);
%%
%% 其他消息
%%
hand_cluster_event(Msg)->
    io:format("Distribution LOG =*-*-*-*-*-=> Cluster msg :[~p] ~n",[Msg]).
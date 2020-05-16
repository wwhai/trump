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
%% 节点属性
-record(trump_cluster_node, {name}).
%% 集群状态下的客户端，多了一个Node属性
-record(trump_cluster_client, {node, client_id, ip, auth, transport, socket}).
-define(CLUSTER_NODE_TABLE,trump_cluster_node_table).
-define(CLUSTER_CLIENT_TABLE,trump_cluster_client_table).
-define(TRUMP_CLIENT_TABLE, trump_client_table).

%% 
%% start_link/0
%% ====================================================================
start() ->
    %% 集群内的客户端存放表
    EtsOptionsTcci = [named_table, public, set, {keypos, #trump_cluster_client.client_id}, {write_concurrency, true}, {read_concurrency, true}],
    ets:new(?CLUSTER_CLIENT_TABLE, EtsOptionsTcci),
    %% 集群内的节点存放表
    EtsOptionsTccn = [named_table, public, set, {keypos, #trump_cluster_node.name}, {write_concurrency, true}, {read_concurrency, true}],
    ets:new(?CLUSTER_NODE_TABLE, EtsOptionsTccn),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
%%  ets:match_object(trump_cluster_node , {trump_cluster_node , '$1' })
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
    {ok,{{mode,Mode,cluster_nodes,  Nodes}}} = application:get_env(trump, cluster),
    case Mode of
        static ->
            Nodes;
        discovery ->
            %% 从ETS表中读取,后期实现
            ets:match_object(?CLUSTER_NODE_TABLE , {trump_cluster_node , '$1'})
    end.

%%
%% 动态添加一个节点
%%
add_node(NodeName)->
    %% 首先判断是否ping的到
    %% if ping -> insert
    %% else -> log
    ets:insert(?CLUSTER_NODE_TABLE , #trump_cluster_node{name =NodeName}).
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

%%
%% 获取本节点上面的所有客户端
%%

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
%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).
%% start_link/0
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
%%
%% Start distribution layer
%%
start() ->
    ok.

get_nodes()->
    [].
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
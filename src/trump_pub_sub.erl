%%%-------------------------------------------------------------------
%%% @author wwhai
%%% 消息队列，客户端可以订阅一个Topic，后者向某个Topic发布消息
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(trump_pub_sub).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

init([]) ->
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
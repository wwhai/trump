%%%-------------------------------------------------------------------
%%% @author wwhai
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(trump_kv_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, start_link/1]).
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

start(Port) ->
    start_link(Port).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init(_Port) ->
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
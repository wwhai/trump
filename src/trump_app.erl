%%%-------------------------------------------------------------------
%% @doc trump public API
%% @end
%%%-------------------------------------------------------------------

-module(trump_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trump_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

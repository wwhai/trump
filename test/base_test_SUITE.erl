-module(base_test_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> 
	[my_test_case].

my_test_case(_Config) -> 
	ok.


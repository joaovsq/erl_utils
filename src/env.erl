%% This module provides functionality to read Environment variables from the OS or a .env file.
%% It uses Erlang ETS to store a cache, the goal is to avoid IO operations as much as possible.
-module(env).

-export([get_var/1, start/0]).

%% Starts the env config. It creates the necessary cache tables.
start() ->
    ets:new(
        envcache,
        [
            set,
            public,
            named_table,
            {keypos, 1},
            {heir, none},
            {write_concurrency, false},
            {read_concurrency, false},
            {decentralized_counters, false}
        ]
    ).

get_from_ets(TableName, Key) ->
    Result = ets:lookup(TableName, Key),
    case Result of
        [] ->
            empty;
        _ ->
            {Key, Value} = lists:nth(1, Result),
            Value
    end.

%% Tries to get a env variable in this order:
%% 1. In memory Cache.
%% 2. OS Environment Variables.
%% 3. A .env file in the local directory.
get_var(Var) when is_atom(Var) ->
    CacheResult = get_from_ets(envcache, Var),
    case CacheResult of
        empty ->
            Result = var_from_env(Var),
            ets:insert(envcache, {Var, Result}),
            Result;
        _ ->
            CacheResult
    end.

var_from_env(Var) when is_atom(Var) ->
    StrVar = string:uppercase(atom_to_list(Var)),
    var_from_env(StrVar);
var_from_env(Var) ->
    EnvVar = os:getenv(Var),
    case EnvVar of
        false ->
            Lines = file_utils:read_all(".env"),
            VarMap = lists:foldl(
                fun get_key_value/2,
                maps:new(),
                Lines
            ),

            case maps:find(Var, VarMap) of
                {ok, VarValue} -> VarValue;
                error -> empty
            end;
        _ ->
            EnvVar
    end.

%% Finds the key and value from a .env Entry and puts the result in a given map.
get_key_value(Entry, VarMap) when Entry =:= "\n" ->
    VarMap;
get_key_value(Entry, VarMap) ->
    KeyValue = string:split(string:chomp(Entry), "="),
    maps:put(
        lists:nth(1, KeyValue),
        lists:nth(2, KeyValue),
        VarMap
    ).

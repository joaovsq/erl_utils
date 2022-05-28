-module(utils).

-export([
    datetime_to_timestamp/1,
    string_format/2,
    get_legacy_rsa_ciphers/0,
    formap/2,
    map_to_sequential_list/1,
    sequential_list_to_map/1,
    float_to_string/1,
    float_to_string/2,
    int_to_string/1,
    string_to_float/1,
    emptyloop/1,
    reinit_mnesia_cluster/0
]).

-export([bin_to_hex/1]).

%% ASCII table NUM and CHAR start (in decimal).
%% In the case of the CHAR start, we assume that it will be CHAR_START + 10 at least. (so in this case the start would be 97).
-define(CHAR_START, 87).

-define(NUM_START, 48).

%% Converts a binary() to a hex string (lowercase).
bin_to_hex(Binary) when is_binary(Binary) ->
    to_hex(binary_to_list(Binary)).

to_hex(List) ->
    HexList = lists:map(fun decimal_to_hex/1, List),
    lists:flatten(HexList).

decimal_to_hex(DecimalChar) ->
    <<MostSignificantNibble:4, LeastSignificantNibble:4>> =
        <<DecimalChar>>,
    [
        to_hex_char(MostSignificantNibble),
        to_hex_char(LeastSignificantNibble)
    ].

to_hex_char(Nibble) when Nibble =< 9 ->
    Nibble + (?NUM_START);
to_hex_char(Nibble) ->
    Nibble + (?CHAR_START).

%% Reference: https://stackoverflow.com/questions/18116628/how-to-convert-gregorian-date-in-seconds-to-unix-timestamp-in-php
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
        62167219200.

%% Like io:format except it returns the evaluated string rather than write it to standard output. Returns a string
%% Example: string_format("2 + 2 = ~p", [2+2])
string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

int_to_string(Value) ->
    binary_to_list(integer_to_binary(Value)).

float_to_string(Value) ->
    float_to_string(Value, 8).

float_to_string(Value, Decimals) ->
    binary_to_list(float_to_binary(Value, [{decimals, Decimals}, compact])).

string_to_float(Value) when is_float(Value) ->
    Value;
string_to_float(Value) when is_binary(Value) ->
    case Value of
        <<"">> -> 0.0;
        _ -> binary_to_float(Value)
    end;
string_to_float(Value) ->
    case Value of
        "" -> 0.0;
        _ -> binary_to_float(list_to_binary(Value))
    end.

%% For security reasons RSA key exchange cipher suites are no longer supported by default.
%% Reference: https://erlang.org/doc/apps/ssl/standards_compliance.html
get_legacy_rsa_ciphers() ->
    RSAKex = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.2'), [
        {key_exchange, fun
            (rsa) -> true;
            (_) -> false
        end}
    ]),
    Default = ssl:cipher_suites(default, 'tlsv1.2'),
    ssl:append_cipher_suites(RSAKex, Default).

%% converts the map to a sequential list ["key", "value"]
map_to_sequential_list(Map) when is_map(Map) ->
    ListOfTuples = maps:to_list(Map),
    ListOfLists = lists:map(fun({K, V}) -> [K, V] end, ListOfTuples),
    lists:append(ListOfLists).

%% converts a list ["key1","value1", "key2", "value2"] to a map #{"key1"=>"value1"}
sequential_list_to_map(List) when is_list(List) andalso List == [] ->
    #{};
sequential_list_to_map(List) when is_list(List) ->
    Len = length(List),
    generate_map(Len, List, #{}).

generate_map(Counter, List, Map) ->
    Init = Counter - 1,
    Sublist = lists:sublist(List, Init, Counter),
    Result = #{lists:nth(1, Sublist) => lists:nth(2, Sublist)},

    AccMap = maps:merge(Result, Map),
    TempCounter = Counter - 2,
    if
        TempCounter > 0 -> generate_map(TempCounter, lists:sublist(List, TempCounter), AccMap);
        true -> AccMap
    end.

%% foreach style to maps
formap(Func, Iterator) ->
    formapnext(Func, maps:next(Iterator)).

formapnext(_, NextResult) when NextResult == none ->
    ok;
formapnext(Func, NextResult) ->
    {K, V, NextIterator} = NextResult,
    Func(K, V),
    formap(Func, NextIterator).

% A empty blocking loop until the rule passes
emptyloop(RuleFunc) ->
    emptyloop(RuleFunc, false).

emptyloop(_, Result) when Result == true ->
    ok;
emptyloop(RuleFunc, _) ->
    Result = RuleFunc(),
    emptyloop(RuleFunc, Result).

%% @doc 
%%  Erases all mnesia replicas and start it anew.
%%  Use it only after the network partition has been resolved and all nodes are reachable.
%% @end
reinit_mnesia_cluster() ->
    rpc:multicall(mnesia, stop, []),
    AllNodes = [node() | nodes()],
    mnesia:delete_schema(AllNodes),
    mnesia:create_schema(AllNodes),
    rpc:multicall(mnesia, start, []).
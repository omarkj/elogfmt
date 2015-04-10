-module(elogfmt).

-define(SEP, <<"=">>).
-define(WSP, <<" ">>).
%% API exports
-export([log/1,
         string/1]).
%% For macro
-export([macro_log/2,
         macro_string/2]).

%% API
%% ---
string(LogStructure) ->
    iolist_to_binary(log(LogStructure)).

log(Map) when is_map(Map) ->
    log(maps:to_list(Map));
log(List) when is_list(List) ->
    lists:reverse(log_proplist(List, [])).

macro_string(ModInfo, Map) ->
    iolist_to_binary(macro_log(ModInfo, Map)).

macro_log(ModInfo, Map) when is_map(Map) ->
    macro_log(ModInfo, maps:to_list(Map));
macro_log(ModInfo, List) when is_list(List) ->
    log(lists:flatten([ModInfo|List])).


%% Internals
%% ---------
log_proplist([], [_|T]) ->
    T;
log_proplist([{K, I}|Rest], Res) when is_integer(I) ->
    log_proplist(Rest, [?WSP, erlang:integer_to_list(I), ?SEP, K|Res]);
log_proplist([{K, F}|Rest], Res) when is_float(F) ->
    Decimal = erlang:float_to_list(F, [{decimals, 4}, compact]),
    log_proplist(Rest, [?WSP, Decimal, ?SEP, K|Res]);
log_proplist([{K, B}|Rest], Res) when is_binary(B) ->
    log_proplist(Rest, [?WSP, B, ?SEP, K|Res]);
log_proplist([{K, L}|Rest], Res) when is_list(L) ->
    log_proplist(Rest, [?WSP, L, ?SEP, K|Res]).

%% Tests
%% -----
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("elogfmt.hrl").
% string takes a map or a proplist and returns a string suitable for logging.
% Log takes a map or proplists and returns a iolist
log_proplist_test() ->
    ?assertEqual(["integer", <<"=">>, "1"], log([{"integer", 1}])),
    ?assertEqual(["list", <<"=">>, "list"], log([{"list", "list"}])),
    ?assertEqual(["float", <<"=">>, "1.1"], log([{"float", 1.1000}])),
    ?assertEqual(["binary", <<"=">>, <<"binary">>], log([{"binary",
                                                          <<"binary">>}])),
    ?assertEqual(["integer", <<"=">>, "1", <<" ">>,
                  "float", <<"=">>, "1.1"], log([{"integer", 1},
                                                 {"float", 1.1000}])).

log_map_test() ->
    ?assertEqual(["integer", <<"=">>, "1"], log(#{"integer" => 1})),
    ?assertEqual(["list", <<"=">>, "list"], log(#{"list" => "list"})),
    ?assertEqual(["float", <<"=">>, "1.1"], log(#{"float" => 1.1000})),
    ?assertEqual(["binary", <<"=">>, <<"binary">>], log(#{"binary" =>
                                                              <<"binary">>})),
    ?assertEqual(["float", <<"=">>, "1.1", <<" ">>,
                  "integer", <<"=">>, "1"], log(#{"integer" => 1,
                                                  "float" => 1.1000})).

string_proplist_test() ->
    ?assertEqual(<<"integer=1">>, string([{"integer", 1}])),
    ?assertEqual(<<"list=list">>, string([{"list", "list"}])),
    ?assertEqual(<<"float=1.1">>, string([{"float", 1.1000}])),
    ?assertEqual(<<"binary=binary">>, string([{"binary", <<"binary">>}])),
    ?assertEqual(<<"integer=1 float=1.1">>, string([{"integer", 1},
                                                    {"float", 1.1000}])).

string_map_test() ->
    ?assertEqual(<<"integer=1">>, string(#{"integer" => 1})),
    ?assertEqual(<<"list=list">>, string(#{"list" => "list"})),
    ?assertEqual(<<"float=1.1">>, string(#{"float" => 1.1000})),
    ?assertEqual(<<"binary=binary">>, string(#{"binary" => <<"binary">>})),
    ?assertEqual(<<"float=1.1 integer=1">>, string(#{"integer" => 1,
                                                     "float" => 1.1000})).
macro_test() ->
    ?assertEqual(["mod", <<"=">>, "elogfmt", <<" ">>, "integer", <<"=">>, "1"],
                 ?LOG([{"integer", 1}])),
    ?assertEqual(<<"mod=elogfmt integer=1">>, ?STRING([{"integer", 1}])),
    ?assertEqual(["mod", <<"=">>, "elogfmt", <<" ">>, "integer", <<"=">>, "1"],
                 ?LOG(#{"integer" => 1})),
    ?assertEqual(<<"mod=elogfmt integer=1">>, ?STRING(#{"integer" => 1})).
-endif.

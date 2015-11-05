-module(elogfmt_core).

-define(SEP, <<"=">>).
-define(WSP, <<" ">>).

-type key() :: iolist().
-type value() :: float()|integer()|iolist().
-type logmessage() :: iolist().
-type loglist() :: [{key(), value()}].
-type logmap() :: #{key() => value()}.
-type structure() :: loglist()|logmap().

%% API exports
-export([logmessage/1]).

%% Types
-export_type([key/0,
              value/0,
              logmessage/0,
              structure/0]).

%% API
%% ---
-spec logmessage(structure()) -> logmessage().
logmessage(Map) when is_map(Map) ->
    logmessage(maps:to_list(Map));
logmessage(List) when is_list(List) ->
    lists:reverse(log_proplist(List, [])).

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
log_proplist([{K, A}|Rest], Res) when is_atom(A) ->
    log_proplist(Rest, [?WSP, erlang:atom_to_list(A), ?SEP, K|Res]);
log_proplist([{K, L}|Rest], Res) when is_list(L) ->
    log_proplist(Rest, [?WSP, L, ?SEP, K|Res]).


%% Tests
%% -----
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
log_proplist_test() ->
    ?assertEqual(["integer", <<"=">>, "1"], logmessage([{"integer", 1}])),
    ?assertEqual(["list", <<"=">>, "list"], logmessage([{"list", "list"}])),
    ?assertEqual(["float", <<"=">>, "1.1"], logmessage([{"float", 1.1000}])),
    ?assertEqual(["binary", <<"=">>, <<"binary">>], logmessage([{"binary",
                                                                 <<"binary">>}])),
    ?assertEqual(["integer", <<"=">>, "1", <<" ">>,
                  "float", <<"=">>, "1.1"], logmessage([{"integer", 1},
                                                        {"float", 1.1000}])).

log_map_test() ->
    ?assertEqual(["integer", <<"=">>, "1"], logmessage(#{"integer" => 1})),
    ?assertEqual(["list", <<"=">>, "list"], logmessage(#{"list" => "list"})),
    ?assertEqual(["float", <<"=">>, "1.1"], logmessage(#{"float" => 1.1000})),
    ?assertEqual(["binary", <<"=">>, <<"binary">>], logmessage(#{"binary" =>
                                                                     <<"binary">>})),
    ?assertEqual(["float", <<"=">>, "1.1", <<" ">>,
                  "integer", <<"=">>, "1"], logmessage(#{"integer" => 1,
                                                         "float" => 1.1000})).
-endif.

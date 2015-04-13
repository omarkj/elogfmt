-module(elogfmt).

-define(SEP, <<"=">>).
-define(WSP, <<" ">>).

-type logline() :: binary().

%% API exports
-export([log/1]).

%% For macro
-export([macro_log/2]).

%% Types
-export_type([logline/0]).

%% API
%% ---
-spec log(elogfmt_core:structure()) -> logline().
log(LogStructure) ->
    iolist_to_binary(elogfmt_core:logmessage(LogStructure)).

-spec macro_log(elogfmt_core:loglist(), elogfmt_core:structure()) ->
                       logline().
macro_log(ModInfo, Map) when is_map(Map) ->
    macro_log(ModInfo, maps:to_list(Map));
macro_log(ModInfo, List) when is_list(List) ->
    iolist_to_binary(elogfmt_core:logmessage(lists:flatten([ModInfo|List]))).

%% Tests
%% -----
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("elogfmt.hrl").
% string takes a map or a proplist and returns a string suitable for logging.
% Log takes a map or proplists and returns a iolist
log_proplist_test() ->
    ?assertEqual(<<"integer=1">>, log([{"integer", 1}])),
    ?assertEqual(<<"list=list">>, log([{"list", "list"}])),
    ?assertEqual(<<"float=1.1">>, log([{"float", 1.1000}])),
    ?assertEqual(<<"binary=binary">>, log([{"binary", <<"binary">>}])),
    ?assertEqual(<<"integer=1 float=1.1">>, log([{"integer", 1},
                                                    {"float", 1.1000}])).

log_map_test() ->
    ?assertEqual(<<"integer=1">>, log(#{"integer" => 1})),
    ?assertEqual(<<"list=list">>, log(#{"list" => "list"})),
    ?assertEqual(<<"float=1.1">>, log(#{"float" => 1.1000})),
    ?assertEqual(<<"binary=binary">>, log(#{"binary" => <<"binary">>})),
    ?assertEqual(<<"float=1.1 integer=1">>, log(#{"integer" => 1,
                                                     "float" => 1.1000})).
macro_test() ->
    ?assertEqual(<<"mod=elogfmt integer=1">>, ?LOG([{"integer", 1}])),
    ?assertEqual(<<"mod=elogfmt integer=1">>, ?LOG(#{"integer" => 1})).
-endif.

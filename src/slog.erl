-module(slog).

-define(COUNT, <<"count#">>).
-define(MEASURE, <<"measure#">>).
-define(SAMPLE, <<"sample#">>).
-define(UNIQUE, <<"unique#">>).

%% API exports
-export([count/2,
         count/3,
         measure/2,
         measure/3,
         sample/2,
         sample/3,
         unique/2,
         unique/3]).

-type count_value() :: pos_integer().
-type measure_value() :: integer()|float().
-type sample_value() :: integer()|float().
-type unique_value() :: iolist().

%% API
%% ---
-spec count(elogfmt_core:key(), count_value()) -> elogfmt:logline().
count(Key, CountValue) when is_integer(CountValue) ->
    count(Key, CountValue, []).

-spec count(elogfmt_core:key(), count_value(), elogfmt_core:structure()) ->
                   elogfmt:logline().
count(Key, CountValue, OtherLogData) ->
    log(?COUNT, Key, CountValue, OtherLogData).

-spec measure(elogfmt_core:key(), measure_value()) -> elogfmt:logline().
measure(Key, MeasureValue) ->
    measure(Key, MeasureValue, []).

-spec measure(elogfmt_core:key(), measure_value(), elogfmt_core:structure()) ->
                     elogfmt:logline().
measure(Key, MeasureValue, OtherLogData) ->
    log(?MEASURE, Key, MeasureValue, OtherLogData).

-spec sample(elogfmt_core:key(), sample_value()) -> elogfmt:logline().
sample(Key, SampleValue) ->
    sample(Key, SampleValue, []).

-spec sample(elogfmt_core:key(), sample_value(), elogfmt:structure()) ->
                    elogfmt:logline().
sample(Key, SampleValue, OtherLogData) ->
    log(?SAMPLE, Key, SampleValue, OtherLogData).

-spec unique(elogfmt_core:key(), unique_value()) -> elogfmt:logline().
unique(Key, UniqueValue) ->
    unique(Key, UniqueValue, []).

-spec unique(elogfmt_core:key(), unique_value(), elogfmt:structure()) ->
                    elogfmt:logline().
unique(Key, UniqueValue, OtherLogData) ->
    log(?UNIQUE, Key, UniqueValue, OtherLogData).

%% Internal
%% --------
log(Prefix, Key, Value, OtherLogData) when is_map(OtherLogData) ->
    log(Prefix, Key, Value, maps:to_list(OtherLogData));
log(Prefix, Key, Value, OtherLogData) ->
    elogfmt:log([{[Prefix, Key], Value}|OtherLogData]).

%% Tests
%% -----
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
count_test() ->
    ?assertEqual(<<"count#foo=1">>, count("foo", 1)),
    ?assertEqual(<<"count#foo=1 second=value">>, count("foo", 1, [{"second", "value"}])),
    ?assertEqual(<<"count#foo=1 second=value">>, count("foo", 1, #{"second" => "value"})).

measure_test() ->
    ?assertEqual(<<"measure#foo=1.1">>, measure("foo", 1.1)),
    ?assertEqual(<<"measure#foo=1.1 second=value">>, measure("foo", 1.1, [{"second", "value"}])),
    ?assertEqual(<<"measure#foo=1 second=value">>, measure("foo", 1, #{"second" => "value"})).

sample_test() ->
    ?assertEqual(<<"sample#foo=1">>, sample("foo", 1)),
    ?assertEqual(<<"sample#foo=1 second=value">>, sample("foo", 1, [{"second", "value"}])),
    ?assertEqual(<<"sample#foo=1 second=value">>, sample("foo", 1, #{"second" => "value"})).

unique_test() ->
    ?assertEqual(<<"unique#foo=1">>, unique("foo", 1)),
    ?assertEqual(<<"unique#foo=1 second=value">>, unique("foo", 1, [{"second", "value"}])),
    ?assertEqual(<<"unique#foo=1 second=value">>, unique("foo", 1, #{"second" => "value"})).

-endif.

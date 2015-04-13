# elogfmt

`logftm` library for Erlang. Read more about [`logfmt`](https://brandur.org/logfmt).

## API


### Elogfmt

```
-type key() :: iolist().
-type value() :: float()|integer()|iolist().
-type logline() :: binary().
-type logmessage() :: iolist().
-type loglist() :: [{key(), value()}].
-type logmap() :: #{key() => value()}.
-type structure() :: loglist()|logmap().

-spec string(structure()) -> logline().
-spec log(structure()) -> logmessage().
```

The library also contains a macro which will inject the calling module name into
the log message. This comes with all the warnings about using macros.

```
-include_lib("elogfmt/include/elogfmt.hrl").
?LOG(structure()) -> logmessage().
?STRING(structure()) -> logline().
```

### Structured Logs

```
-type count_value() :: pos_integer().
-type measure_value() :: integer()|float().
-type sample_value() :: integer()|float().
-type unique_value() :: iolist().

-spec count(elogfmt:key(), count_value()) -> elogfmt:logmessage().
-spec count(elogfmt:key(), count_value(), elogfmt:structure()) ->
                   elogfmt:logmessage().
-spec measure(elogfmt:key(), measure_value()) -> elogfmt:logmessage().
-spec measure(elogfmt:key(), measure_value(), elogfmt:structure()) ->
                     elogfmt:logmessage().
-spec sample(elogfmt:key(), sample_value()) -> elogfmt:logmessage().
-spec sample(elogfmt:key(), sample_value(), elogfmt:structure()) ->
                    elogfmt:logmessage().
-spec unique(elogfmt:key(), unique_value()) -> elogfmt:logmessage().
-spec unique(elogfmt:key(), unique_value(), elogfmt:structure()) ->
                    elogfmt:logmessage().
```


## Build

```
$ rebar3 compile
```

## Test

```
$ rebar3 eunit
```
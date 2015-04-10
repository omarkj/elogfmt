# elogfmt

`logftm` library for Erlang. Read more about [`logfmt`](https://brandur.org/logfmt).

## API

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

## Build

```
$ rebar3 compile
```

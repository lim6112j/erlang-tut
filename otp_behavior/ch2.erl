-module(ch2).

-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
  server:start(ch2).

alloc() ->
  server:call(ch2, alloc).

free(Ch) ->
  server:cast(ch2, {free, Ch}).

init() ->
  channels().

alloc({Allocated, [H | T] = _Free}) ->
  {H, {[H | Allocated], T}}.

handle_call(alloc, Chs) ->
  alloc(Chs).

free(Ch, {Alloc, Free} = Channels) ->
  case lists:member(Ch, Alloc) of
    true ->
      {lists:delete(Ch, Alloc), [Ch | Free]};
    false ->
      Channels
  end.

handle_cast({free, Ch}, Chs) ->
  free(Ch, Chs).

channels() ->
  {_Allocated = [], _Free = lists:seq(1, 100)}.

-module(tut2).
-export([convert/2]).
%% atoms , inch, centimeter
convert(N, inch) ->
    N / 2.54;
convert(N, centimeter) ->
    N * 2.54.

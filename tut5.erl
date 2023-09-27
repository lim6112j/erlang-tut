-module(tut5).
-export([format_temps/1]).

format_temps([]) ->
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).
convert_to_celsius({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.
print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).

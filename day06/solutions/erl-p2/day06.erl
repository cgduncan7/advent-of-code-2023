-module(day06).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, 1, all).

solve(Data, _) ->
  {Time, Dist} = format_input(Data),
  [Min, Max] = get_time_held_for_record(Time, Dist),
  Val = (math:ceil(Max) - math:floor(Min) - 1),
  Val.

string_to_int(S) -> {V,_} = string:to_integer(S), V.

format_input(Data) ->
  [TimesString, DistancesString] = string:lexemes(Data, "\n"),
  Time = string_to_int(lists:flatten(lists:join("",tl(string:lexemes(TimesString, ": "))))),
  Distance = string_to_int(lists:flatten(lists:join("",tl(string:lexemes(DistancesString, ": "))))),
  {Time, Distance}.

get_time_held_for_record(TotalTime, Distance) ->
  [(-TotalTime + math:sqrt(TotalTime * TotalTime - (4*-1*-Distance))) / -2,
  (-TotalTime - math:sqrt(TotalTime * TotalTime - (4*-1*-Distance))) / -2].
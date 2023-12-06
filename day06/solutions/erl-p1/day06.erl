-module(day06).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, 1, all).

solve(Data, _) ->
  Races = format_input(Data),
  lists:foldl(
    fun ({Time, Dist}, Acc) ->
      [Min, Max] = get_time_held_for_record(Time, Dist),
      Val = (math:ceil(Max) - math:floor(Min) - 1), % max unacceptable - min unacceptable - 1
      Acc * Val
    end,
    1,
    Races
  ).

string_to_int(S) -> {V,_} = string:to_integer(S), V.

format_input(Data) ->
  [TimesString, DistancesString] = string:lexemes(Data, "\n"),
  Times = lists:map(fun string_to_int/1, tl(string:lexemes(TimesString, ": "))),
  Distances = lists:map(fun string_to_int/1, tl(string:lexemes(DistancesString, ": "))),
  lists:zip(Times, Distances).

% Quadratic formula to get upper and lower bound of time held
get_time_held_for_record(TotalTime, Distance) ->
  [(-TotalTime + math:sqrt(TotalTime * TotalTime - (4*-1*-Distance))) / -2,
  (-TotalTime - math:sqrt(TotalTime * TotalTime - (4*-1*-Distance))) / -2].
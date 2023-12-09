-module(day09).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve_pt1/2, 0, lines),
  harness:run(Filename, fun solve_pt2/2, 0, lines).

solve_pt1(Data, State) ->
  case Data of
    nil -> State;
    Data -> extrapolate(parse_line(Data), forwards) + State
  end.

solve_pt2(Data, State) ->
  case Data of
    nil -> State;
    Data -> extrapolate(parse_line(Data), backwards) + State
  end.

parse_line(Line) -> lists:map(fun (I) -> {V,_} = string:to_integer(I), V end, string:lexemes(Line, " ")).

get_extrapolating_value(Line, forwards) -> lists:last(Line);
get_extrapolating_value(Line, backwards) -> hd(Line).

extrapolate_op(EV, Diff, forwards) -> EV + Diff;
extrapolate_op(EV, Diff, backwards) -> EV - Diff.

extrapolate(Line, Direction) ->
  Diff = get_differences(Line),
  DiffHead = hd(Diff),
  DiffsMatch = lists:all(fun (E) -> DiffHead =:= E end, Diff),
  extrapolate_op(
    get_extrapolating_value(Line, Direction),
    if DiffsMatch =:= true -> DiffHead; true -> extrapolate(Diff, Direction) end,
    Direction
  ).

get_differences(Line) ->
  Len = erlang:length(Line),
  lists:zipwith(fun (A, B) -> B - A end, lists:sublist(Line, Len-1), lists:nthtail(1, Line)).

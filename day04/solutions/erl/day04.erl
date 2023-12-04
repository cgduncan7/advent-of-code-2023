-module(day04).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, unknown, bytes).

solve(Data, State) ->
  case Data of
    _ -> todo
  end.

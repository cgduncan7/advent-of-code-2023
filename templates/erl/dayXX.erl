-module(dayXX).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("", Filename, fun solve/2, unknown, bytes).

solve(Data, State) ->
  case Data of
    _ -> todo
  end.

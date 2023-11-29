-module(dayExample).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun(D) -> solve(D) end, all).

solve(Data) ->
  io:fwrite("~s~n",[Data]).
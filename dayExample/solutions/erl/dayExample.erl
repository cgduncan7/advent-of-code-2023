-module(dayExample).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun(D) -> solve(D) end, bytes).

solve(Data) ->
  case Data of
    nil -> 420;
    D -> io:fwrite("'~s'~n",[D])
  end.
-module(dayExample).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("", Filename, fun solve/2, {0, 0}, lines).

solve(Data, {Max, Current}) ->
  case Data of
    nil -> Max;
    [] -> {erlang:max(Max, Current), 0};
    D -> {Max, Current + erlang:list_to_integer(D)}
  end.
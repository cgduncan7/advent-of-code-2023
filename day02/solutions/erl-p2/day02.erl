-module(day02).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, 0, lines).

parse_game(String) ->
  lists:map(
    fun (S) -> string:trim(S) end,
    string:lexemes(String, ":,;")
  ).

get_cube_amount(CubeAmount) ->
  Split = string:lexemes(CubeAmount, " "),
  {Amount,_} = string:list_to_integer(hd(Split)),
  {lists:last(Split), Amount}.

get_game_value([_| CubeAmounts]) ->
  BeginningAmount = dict:from_list([{"red",0},{"green",0},{"blue",0}]),
  SetAmounts =
    fun (CA, Acc) ->
      {Key, Amount} = get_cube_amount(CA),
      dict:store(Key, erlang:max(dict:fetch(Key, Acc), Amount), Acc)
    end,
  MinAmounts = lists:foldl(SetAmounts, BeginningAmount, CubeAmounts),
  dict:fold(fun (_,V,A) -> A * V end, 1, MinAmounts).

solve(Data, Total) ->
  case Data of
    nil -> Total;
    D ->
      ParsedData = parse_game(D),
      Total + get_game_value(ParsedData)
  end.

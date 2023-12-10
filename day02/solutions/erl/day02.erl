-module(day02).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, 0, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, 0, lines).

max_allowed_cubes("red") -> 12;
max_allowed_cubes("green") -> 13;
max_allowed_cubes("blue") -> 14;
max_allowed_cubes(_) -> 0.

parse_game(String) ->
  lists:map(
    fun (S) -> string:trim(S) end,
    string:lexemes(String, ":,;")
  ).

% Returns true if cube amount > max allowed cubes; thus the game is invalid
check_cube_amount(CubeAmount) ->
  Split = string:lexemes(CubeAmount, " "),
  {Amount,_} = string:list_to_integer(hd(Split)),
  max_allowed_cubes(lists:last(Split)) < Amount.

get_cube_amount(CubeAmount) ->
  Split = string:lexemes(CubeAmount, " "),
  {Amount,_} = string:list_to_integer(hd(Split)),
  {lists:last(Split), Amount}.

get_game_value1([GameId | CubeAmounts]) ->
  case lists:any(fun (C) -> check_cube_amount(C) end, CubeAmounts) of
    true -> 0;
    false -> 
      {A,_} = string:list_to_integer(lists:last(string:lexemes(GameId, " "))), A
  end.

get_game_value2([_| CubeAmounts]) ->
  BeginningAmount = dict:from_list([{"red",0},{"green",0},{"blue",0}]),
  SetAmounts =
    fun (CA, Acc) ->
      {Key, Amount} = get_cube_amount(CA),
      dict:store(Key, erlang:max(dict:fetch(Key, Acc), Amount), Acc)
    end,
  MinAmounts = lists:foldl(SetAmounts, BeginningAmount, CubeAmounts),
  dict:fold(fun (_,V,A) -> A * V end, 1, MinAmounts).

solve_pt1(Data, Total) ->
  case Data of
    nil -> Total;
    D ->
      ParsedData = parse_game(D),
      Total + get_game_value1(ParsedData)
  end.

solve_pt2(Data, Total) ->
  case Data of
    nil -> Total;
    D ->
      ParsedData = parse_game(D),
      Total + get_game_value2(ParsedData)
  end.
-module(day02).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, 0, lines).

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

get_game_value([GameId | CubeAmounts]) ->
  case lists:any(fun (C) -> check_cube_amount(C) end, CubeAmounts) of
    true -> 0;
    false -> 
      {A,_} = string:list_to_integer(lists:last(string:lexemes(GameId, " "))), A
  end.

solve(Data, Total) ->
  case Data of
    nil -> Total;
    D ->
      ParsedData = parse_game(D),
      Total + get_game_value(ParsedData)
  end.

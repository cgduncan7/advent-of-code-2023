-module(day04).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run(Filename, fun solve/2, 0, lines).

solve(Data, State) ->
  case Data of
    nil -> State;
    _ -> 
      SI = split_input_string(Data),
      WN = get_winning_numbers(SI),
      CN = get_card_numbers(SI),
      MN = lists:filter(fun (C) -> contains(C, WN) end, CN),
      Val = case erlang:length(MN) of
        0 -> 0;
        X -> round(math:pow(2, X - 1))
      end,
      State + Val
  end.

contains(Val, List) ->
 case lists:search(fun (CC) -> CC =:= Val end, List) of 
    false -> false;
    _ -> true
  end.

split_input_string(Input) -> string:lexemes(Input, ":|").

list_of_strings_to_ints(List) -> lists:map(fun (I) -> {V, _} = string:to_integer(I), V end, List).

get_winning_numbers(SplitInput) -> list_of_strings_to_ints(string:lexemes(string:trim(lists:nth(2, SplitInput)), " ")).

get_card_numbers(SplitInput) -> list_of_strings_to_ints(string:lexemes(string:trim(lists:nth(3, SplitInput)), " ")).
-module(day04).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, 0, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, {[], 0}, lines).

solve_pt1(Data, State) ->
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

solve_pt2(Data, {Queue, Total}) ->
  case Data of
    nil -> Total;
    _ -> 
      SI = split_input_string(Data),
      C = get_card_number(SI),
      WN = get_winning_numbers(SI),
      CN = get_card_numbers(SI),
      MN = lists:filter(fun (N) -> contains(N, WN) end, CN),
      {CurrentCopies, RemainingCopies} = lists:partition(fun (Copy) -> C =:= Copy end, Queue),
      CardNumberCopies = lists:merge(lists:duplicate(erlang:length(CurrentCopies)+1, lists:seq(C+1, C + erlang:length(MN)))),
      {lists:merge(RemainingCopies, CardNumberCopies), Total+erlang:length(CurrentCopies)+1}
  end.

contains(Val, List) ->
 case lists:search(fun (CC) -> CC =:= Val end, List) of 
    false -> false;
    _ -> true
  end.

split_input_string(Input) -> string:lexemes(Input, ":|").

list_of_strings_to_ints(List) -> lists:map(fun (I) -> {V, _} = string:to_integer(I), V end, List).

get_card_number(SplitInput) -> {V, _} = string:to_integer(lists:last(string:lexemes(string:trim(lists:nth(1, SplitInput)), " "))), V.

get_winning_numbers(SplitInput) -> list_of_strings_to_ints(string:lexemes(string:trim(lists:nth(2, SplitInput)), " ")).

get_card_numbers(SplitInput) -> list_of_strings_to_ints(string:lexemes(string:trim(lists:nth(3, SplitInput)), " ")).
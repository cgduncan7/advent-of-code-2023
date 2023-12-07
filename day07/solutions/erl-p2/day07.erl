-module(day07).
-import(harness, [run/3]).
-export([run/1]).

-record(hand, {
  cards,
  score,
  bid
}).

run(Filename) ->
  harness:run(Filename, fun solve/2, [], lines).

solve(Data, State) ->
  case Data of
    nil ->
      {_, TotalWinnings} = lists:foldl(fun (H, {I, T}) -> {I+1, T + get_winnings(H, I)} end, {1, 0}, sort_hands(State)),
      TotalWinnings;
    Data ->
      {Cards, Bid} = parse_input_line(Data),
      Score = get_hand_score(Cards),
      State ++ [#hand{cards=Cards,score=Score,bid=Bid}]
  end.

parse_input_line(Line) ->
  [Hand, Bid] = string:lexemes(Line, " "),
  Cards = lists:map(fun (E) -> card_to_number_value(E) end, Hand),
  {BidValue, _} = string:to_integer(Bid),
  {Cards, BidValue}.

card_to_number_value($A) -> 14;
card_to_number_value($K) -> 13;
card_to_number_value($Q) -> 12;
card_to_number_value($J) -> 1; % now lowest
card_to_number_value($T) -> 10;
card_to_number_value(D) -> {V, _} = string:to_integer([D]), V.

get_hand_score(Cards) ->
  get_hand_score_from_occurrences(get_card_occurrences(Cards)).

get_card_occurrences(Cards) ->
  Inc = fun (V) -> V + 1 end,
  NonJokers = lists:filter(fun (E) -> E =/= 1 end, Cards),
  NumJokers = 5 - erlang:length(NonJokers),
  Occurrences = lists:sort(maps:values(lists:foldl(
    fun (E, Acc) -> maps:update_with(E, Inc, 1, Acc) end,
    maps:new(),
    NonJokers
  ))),
  if
    NumJokers =:= 5 -> [5];
    true ->
      [Head|Rest] = lists:reverse(Occurrences),
      lists:reverse([Head + NumJokers] ++ Rest)
  end.

get_hand_score_from_occurrences([1,1,1,1,1]) -> 1;
get_hand_score_from_occurrences([1,1,1,2]) -> 2;
get_hand_score_from_occurrences([1,2,2]) -> 3;
get_hand_score_from_occurrences([1,1,3]) -> 4;
get_hand_score_from_occurrences([2,3]) -> 5;
get_hand_score_from_occurrences([1,4]) -> 6;
get_hand_score_from_occurrences([5]) -> 7;
get_hand_score_from_occurrences(_) -> io:fwrite("Error"), exit(1).

compare_cards([X],[Y]) -> X < Y;
compare_cards([X|Y],[X|Z]) -> compare_cards(Y,Z);
compare_cards([X|_],[Y|_]) -> X < Y.

compare_hands(#hand{score=SA,cards=CA}, #hand{score=SA,cards=CB}) -> compare_cards(CA, CB);
compare_hands(#hand{score=SA}, #hand{score=SB}) -> SA < SB.

sort_hands(Hands) ->
  lists:sort(fun compare_hands/2, Hands).

get_winnings(#hand{bid=Bid}, Rank) -> Rank * Bid.
-module(day12).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, 0, lines).
  % harness:run("Part 2", Filename, fun solve_pt2/2, 0, lines).

solve_pt1(Data, State) ->
  case Data of
    nil -> State;
    D -> {Arr, Dmg} = parse_input(D), State + erlang:length(get_valid_expansions(expand("", unknown, Arr, Dmg), Dmg))
  end.

solve_pt2(Data, State) ->
  case Data of
    nil -> State;
    D ->
      {Arr, Dmg} = parse_input_with_unfolding(D),
      A = erlang:length(get_valid_expansions(expand("", unknown, Arr, Dmg), Dmg)),
      io:fwrite("~s ~w => ~w~n", [Arr, Dmg, A]),
      State + A
  end.

parse_input(Input) ->
  [Arrangement|DamagedSprings] = string:lexemes(Input, " ,"),
  {Arrangement, lists:nthtail(1, lists:foldl(fun (I, A) -> {V, _} = string:to_integer(I),  A ++ [0, V] end, [], DamagedSprings))}.

parse_input_with_unfolding(Input) ->
  [Arrangement|DamagedSprings] = string:lexemes(Input, " ,"),
  ExpandedArrangement = lists:flatten(lists:join("?", lists:duplicate(5, Arrangement))),
  {ExpandedArrangement, lists:foldl(
    fun (E, Acc) ->
      if
        erlang:length(Acc) =:= 0 -> [E];
        true -> Acc ++ [0] ++ [E]
      end
    end,
    [],
    lists:flatten(lists:duplicate(5, lists:map(fun (I) -> {V, _} = string:to_integer(I), V end, DamagedSprings)))
  )}.

get_damaged_groups(Arrangement) -> string:lexemes(Arrangement, ".").

validate(Arrangement, DamagedSprings) ->
  lists:all(
    fun ({A,B}) -> erlang:length(A) =:= B end,
    lists:zip(
      get_damaged_groups(Arrangement),
      lists:filter(fun (S) -> S =/= 0 end, DamagedSprings),
      {pad, {[], -1}}
    )
  ).

validate_partial(Arrangement, DamagedSprings) ->
  (erlang:length(Arrangement) >= lists:sum(DamagedSprings) + (erlang:length(DamagedSprings) div 2)) and
  lists:all(
    fun ({A,B}) -> erlang:length(A) =< B end,
    lists:zip(
      get_damaged_groups(lists:takewhile(fun (E) -> E =/= $? end, Arrangement)),
      lists:filter(fun (S) -> S =/= 0 end, DamagedSprings),
      trim
    )
  ).

v_expand(C, G, A, D) ->
  % io:fwrite("Current: ~s~n[~w] Arrangement: ~w Damaged: ~w~n~n", [C,G,A,D]),
  expand(C,G,A,D).

expand(Current, _, [], []) ->
  {valid, Current};
expand(Current, _, [$.|RestArrangement], [0|RestDamagedSprings]) ->
  v_expand(Current ++ ".", out, RestArrangement, RestDamagedSprings);
expand(Current, _, [$.|RestArrangement], DamagedSprings) ->
  v_expand(Current ++ ".", out, RestArrangement, DamagedSprings);
expand(_, _, [$#|_], [0|_]) -> invalid;
expand(Current, _, [$#|RestArrangement], [1|RestDamagedSprings]) ->
  v_expand(Current ++ "#", in, RestArrangement, RestDamagedSprings);
expand(Current, _, [$#|RestArrangement], [HeadDamagedSprings|RestDamagedSprings]) ->
  v_expand(Current ++ "#", in, RestArrangement, [HeadDamagedSprings-1|RestDamagedSprings]);
expand(Current, _, [$?|RestArrangement], [0|RestDamagedSprings]) ->
  v_expand(Current ++ ".", out, RestArrangement, RestDamagedSprings);
expand(Current, _, [$?|RestArrangement], []) ->
  v_expand(Current, out, [$.|RestArrangement], []);
expand(Current, in, [$?|RestArrangement], DamagedSprings) ->
  IsDamagedVariantValid = validate_partial([$#|RestArrangement], DamagedSprings),
  if
    IsDamagedVariantValid -> v_expand(Current, in, [$#|RestArrangement], DamagedSprings);
    true -> invalid
  end;
expand(Current, _, [$?|RestArrangement], DamagedSprings) ->
  IsWorkingVariantValid = validate_partial([$.|RestArrangement], DamagedSprings),
  IsDamagedVariantValid = validate_partial([$#|RestArrangement], DamagedSprings),
  if
    IsWorkingVariantValid and IsDamagedVariantValid -> [v_expand(Current, out, [$.|RestArrangement], DamagedSprings), v_expand(Current, in, [$#|RestArrangement], DamagedSprings)];
    not IsWorkingVariantValid and IsDamagedVariantValid -> v_expand(Current, in, [$#|RestArrangement], DamagedSprings);
    IsWorkingVariantValid and not IsDamagedVariantValid -> v_expand(Current, out, [$.|RestArrangement], DamagedSprings);
    not IsWorkingVariantValid and not IsDamagedVariantValid -> invalid
  end;
expand(_, _, _, _) -> invalid.

get_valid_expansions(Expansions, DamagedSprings) ->
  lists:filter(
    fun (E) ->
      case E of
        invalid -> false;
        {valid, A} -> validate(A, DamagedSprings)
      end
    end, lists:flatten([Expansions])).
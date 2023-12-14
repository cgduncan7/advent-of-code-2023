-module(day13).
-import(harness, [run/5]).
-export([run/1]).

-record(rocks, {
  rows=[],
  cols=[]
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {#rocks{}, 0}, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, {#rocks{}, 0}, lines).

solve_pt1(Data, {#rocks{rows=Rows,cols=Cols}, Total}) ->
  case Data of
    "" -> {#rocks{}, Total+find_mirror(#rocks{rows=Rows,cols=rows_to_cols(Rows)})};
    nil -> Total+find_mirror(#rocks{rows=Rows,cols=rows_to_cols(Rows)});
    D -> {#rocks{rows=Rows ++ [D],cols=Cols}, Total}
  end.


solve_pt2(Data, {#rocks{rows=Rows,cols=Cols}, Total}) ->
  case Data of
    "" -> {#rocks{}, Total+find_mirror(#rocks{rows=Rows,cols=rows_to_cols(Rows)}, smudge)};
    nil -> Total+find_mirror(#rocks{rows=Rows,cols=rows_to_cols(Rows)}, smudge);
    D -> {#rocks{rows=Rows ++ [D],cols=Cols}, Total}
  end.

rows_to_cols(Rows) ->
  lists:foldl(
    fun ({_, Col}, Ac) ->
      Ac ++ [Col]
    end,
    [],
    lists:sort(
      maps:to_list(
        lists:foldl(
          fun ({Idx, Val}, A) ->
            maps:put(Idx, lists:append([maps:get(Idx, A, []), [Val]]), A)
          end,
          maps:new(), 
          lists:flatten(lists:map(fun lists:enumerate/1, Rows))
        )
      )
    )
  ).

find_mirror(#rocks{rows=Rows,cols=Cols}) ->
  PossibleRowMirrors = find_matching(Rows),
  PossibleColMirrors = find_matching(Cols),
  RowMirrors = lists:filter(fun ({Idx, _}) -> is_mirrored(Idx, Rows) end, PossibleRowMirrors),
  ColMirrors = lists:filter(fun ({Idx, _}) -> is_mirrored(Idx, Cols) end, PossibleColMirrors),
  if
    erlang:length(RowMirrors) =:= 1 -> {Idx,_} = hd(RowMirrors), Idx * 100;
    erlang:length(ColMirrors) =:= 1 -> {Idx,_} = hd(ColMirrors), Idx
  end.

find_mirror(#rocks{rows=Rows,cols=Cols}, smudge) ->
  PossibleRowMirrors = find_matching(Rows, smudge),
  PossibleColMirrors = find_matching(Cols, smudge),
  RowMirrors = lists:filter(fun ({Idx, _}) -> is_mirrored(Idx, Rows, smudge) end, PossibleRowMirrors),
  ColMirrors = lists:filter(fun ({Idx, _}) -> is_mirrored(Idx, Cols, smudge) end, PossibleColMirrors),
  if
    erlang:length(RowMirrors) =:= 1 -> {Idx,_} = hd(RowMirrors), Idx * 100;
    erlang:length(ColMirrors) =:= 1 -> {Idx,_} = hd(ColMirrors), Idx
  end.

find_matching(RockRowsOrCols) ->
  lists:filter(
    fun ({Index, Elem}) ->
      if
        (Index+1) =< erlang:length(RockRowsOrCols) -> Elem =:= lists:nth(Index+1, RockRowsOrCols);
        true -> false
      end
    end,
    lists:enumerate(RockRowsOrCols)
  ).

is_mirrored(StartIndex, Rocks) ->
  LeftIndices = lists:reverse(lists:seq(1, StartIndex)),
  RightIndices = lists:seq(StartIndex+1, erlang:length(Rocks)),
  MatchingIndices = lists:zip(LeftIndices, RightIndices, trim),
  lists:all(fun ({L,R}) -> lists:nth(L, Rocks) =:= lists:nth(R, Rocks) end, MatchingIndices).

is_mirrored(StartIndex, Rocks, smudge) ->
  LeftIndices = lists:reverse(lists:seq(1, StartIndex)),
  RightIndices = lists:seq(StartIndex+1, erlang:length(Rocks)),
  MatchingIndices = lists:zip(LeftIndices, RightIndices, trim),
  Differences = lists:foldl(
    fun ({L,R}, Total) -> Total + find_differences(lists:nth(L, Rocks), lists:nth(R, Rocks)) end,
    0,
    MatchingIndices
  ),
  Differences =:= 1.

find_matching(RockRowsOrCols, smudge) ->
  lists:filter(
    fun ({Index, Elem}) ->
      if
        (Index+1) =< erlang:length(RockRowsOrCols) -> find_differences(Elem, lists:nth(Index+1, RockRowsOrCols)) =< 1;
        true -> false
      end
    end,
    lists:enumerate(RockRowsOrCols)
  ).

find_differences(L, R) ->
  erlang:length(lists:filter(fun ({LL, RR}) -> LL =/= RR end, lists:zip(L, R))).
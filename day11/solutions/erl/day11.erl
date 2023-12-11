-module(day11).
-import(harness, [run/5]).
-export([run/1]).

-record(universe, {
  nonempty_rows=sets:new(),
  nonempty_cols=sets:new(),
  galaxies=[]
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, [#universe{}, {0,0}], bytes),
  harness:run("Part 1", Filename, fun solve_pt2/2, [#universe{}, {0,0}], bytes).

solve_pt1(Data, [Universe, {X, Y}]) ->
  case Data of
    nil -> get_distances_between_galaxies(expand_galaxies(Universe, 2));
    "\n" -> [Universe, {0, Y+1}];
    D -> [update_universe_maps(Universe, {X, Y}, is_galaxy(D)), {X+1, Y}]
  end.

solve_pt2(Data, [Universe, {X, Y}]) ->
  case Data of
    nil -> get_distances_between_galaxies(expand_galaxies(Universe, 1_000_000));
    "\n" -> [Universe, {0, Y+1}];
    D -> [update_universe_maps(Universe, {X, Y}, is_galaxy(D)), {X+1, Y}]
  end.

is_galaxy("#") -> true;
is_galaxy(_) -> false.

update_universe_maps(#universe{nonempty_rows=NonEmptyRows, nonempty_cols=NonEmptyCols, galaxies=Galaxies}, {X, Y}, true) ->
  #universe{
    nonempty_rows=sets:add_element(Y, NonEmptyRows),
    nonempty_cols=sets:add_element(X, NonEmptyCols),
    galaxies=lists:append([Galaxies, [{X, Y}]])
  };
update_universe_maps(#universe{nonempty_rows=NonEmptyRows, nonempty_cols=NonEmptyCols, galaxies=Galaxies}, _, false) -> 
  #universe{
    nonempty_rows=NonEmptyRows,
    nonempty_cols=NonEmptyCols,
    galaxies=Galaxies
  }.

get_empty_from_nonempty(NonEmptySet) ->
  NonEmptyList = sets:to_list(NonEmptySet),
  sets:to_list(sets:subtract(sets:from_list(lists:seq(lists:min(NonEmptyList),lists:max(NonEmptyList))), NonEmptySet)).

get_expanded_location({X, Y}, ExpandedRows, ExpandedCols, ExpansionFactor) ->
  ValidExpandedRows = lists:filter(fun (R) -> Y > R end, ExpandedRows),
  ValidExpandedCols = lists:filter(fun (C) -> X > C end, ExpandedCols),
  GetExpansionAmount = fun (Spaces) -> Spaces * ExpansionFactor - Spaces end,
  {X + GetExpansionAmount(erlang:length(ValidExpandedCols)), Y + GetExpansionAmount(erlang:length(ValidExpandedRows))}.

expand_galaxies(#universe{nonempty_rows=NonEmptyRows, nonempty_cols=NonEmptyCols, galaxies=Galaxies}, ExpansionFactor) ->
  EmptyRows = get_empty_from_nonempty(NonEmptyRows),
  EmptyCols = get_empty_from_nonempty(NonEmptyCols),
  lists:map(fun (G) -> get_expanded_location(G, EmptyRows, EmptyCols, ExpansionFactor) end, Galaxies).

get_distances_between_galaxies([_]) -> 0;
get_distances_between_galaxies([Start|Rest]) ->
  lists:sum(get_distances_between_galaxies(Start, Rest)) + get_distances_between_galaxies(Rest).

get_distances_between_galaxies(StartGalaxy, RestGalaxies) ->
  lists:map(fun (G) -> get_distance_between(StartGalaxy, G) end, RestGalaxies).

get_distance_between({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).

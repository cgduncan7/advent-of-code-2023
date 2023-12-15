-module(day14).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {{0,0},#{}}, bytes),
  harness:run("Part 2", Filename, fun solve_pt2/2, {{0,0},#{}}, bytes).

solve_pt1(Data, {{X,Y}, Map}) ->
  case Data of
    "\n" -> {{0, Y+1}, Map};
    nil -> calculate_load({X-1, Y}, tilt({X-1, Y}, Map, north), north);
    "O" -> {{X+1, Y}, maps:put({X,Y}, round, Map)};
    "#" -> {{X+1, Y}, maps:put({X,Y}, cube, Map)};
    "." -> {{X+1, Y}, Map}
  end.

solve_pt2(Data, {{X,Y}, Map}) ->
  case Data of
    "\n" -> {{0, Y+1}, Map};
    nil -> {_, A} = cycle({X-1, Y}, Map, 1000000000, {maps:new(), 0, []}), A;
    "O" -> {{X+1, Y}, maps:put({X,Y}, round, Map)};
    "#" -> {{X+1, Y}, maps:put({X,Y}, cube, Map)};
    "." -> {{X+1, Y}, Map}
  end.

calculate_load({_,Y}, Map, north) ->
  SouthY = Y+1,
  lists:foldl(
    fun ({{_,CY},Val}, Acc) ->
      case Val of
        round -> Acc + SouthY - CY;
        _ -> Acc
      end
    end,
    0,
    maps:to_list(Map)
  ).

increment(V) -> V + 1.

cycle(_, Map, 0, _) -> Map;
cycle(Max, Map, Times, {Unique, LastUnique, Total}) ->
  Res = tilt(Max, tilt(Max, tilt(Max, tilt(Max, Map, north), west), south), east),
  Load = calculate_load(Max, Res, north),
  CurrentSize = maps:size(Unique),
  NewResults = maps:update_with(Load, fun increment/1, 1, Unique),
  NewSize = maps:size(NewResults),
  NewUnique = CurrentSize =/= NewSize,
  if
    (NewSize / erlang:length(Total)) < 0.3 -> determine_cycle(Total, LastUnique);
    true -> cycle(Max, Res, Times-1, {NewResults, if NewUnique -> Load; true -> LastUnique end, Total ++ [Load]})
  end.

determine_cycle(Total, LastUnique) ->
  Cyclable = lists:dropwhile(fun ({_, A}) -> A =/= LastUnique end, lists:enumerate(Total)),
  Cycle = [hd(Cyclable)|lists:takewhile(fun ({_, A}) -> A =/= LastUnique end, tl(Cyclable))],
  CycleLength = erlang:length(Cycle),
  {CycleStartIndex, _} = hd(Cyclable),
  lists:nth(((1_000_000_000 - CycleStartIndex) rem CycleLength + 1), Cycle).

tilt({MaxX, MaxY}, Map, north) ->
  XSeq = lists:seq(0, MaxX),
  YSeq = lists:seq(1, MaxY),
  lists:foldl(
    fun (X, XMap) ->
      lists:foldl(
        fun (Y, YMap) ->
          case maps:get({X,Y}, YMap, empty) of
            empty -> YMap;
            round -> move({X,Y}, {MaxX, MaxY}, YMap, north);
            cube -> YMap 
          end
        end,
        XMap,
        YSeq
      )
    end,
    Map,
    XSeq
  );
tilt({MaxX, MaxY}, Map, west) ->
  XSeq = lists:seq(1, MaxX),
  YSeq = lists:seq(0, MaxY),
  lists:foldl(
    fun (X, XMap) ->
      lists:foldl(
        fun (Y, YMap) ->
          case maps:get({X,Y}, YMap, empty) of
            empty -> YMap;
            round -> move({X,Y}, {MaxX, MaxY}, YMap, west);
            cube -> YMap 
          end
        end,
        XMap,
        YSeq
      )
    end,
    Map,
    XSeq
  );
tilt({MaxX, MaxY}, Map, south) ->
  XSeq = lists:seq(0, MaxX),
  YSeq = lists:reverse(lists:seq(0, MaxY-1)),
  lists:foldl(
    fun (X, XMap) ->
      lists:foldl(
        fun (Y, YMap) ->
          case maps:get({X,Y}, YMap, empty) of
            empty -> YMap;
            round -> move({X,Y}, {MaxX, MaxY}, YMap, south);
            cube -> YMap 
          end
        end,
        XMap,
        YSeq
      )
    end,
    Map,
    XSeq
  );
tilt({MaxX, MaxY}, Map, east) ->
  XSeq = lists:reverse(lists:seq(0, MaxX-1)),
  YSeq = lists:seq(0, MaxY),
  lists:foldl(
    fun (X, XMap) ->
      lists:foldl(
        fun (Y, YMap) ->
          case maps:get({X,Y}, YMap, empty) of
            empty -> YMap;
            round -> move({X,Y}, {MaxX, MaxY}, YMap, east);
            cube -> YMap 
          end
        end,
        XMap,
        YSeq
      )
    end,
    Map,
    XSeq
  ).

move_coords({X,Y}, north) -> {X,Y-1};
move_coords({X,Y}, west) -> {X-1,Y};
move_coords({X,Y}, east) -> {X+1,Y};
move_coords({X,Y}, south) -> {X,Y+1}.

move(Origin, Max, Map, Dir) ->
  EmptyNeighbor = get_neighbor(Origin, Max, Map, Dir) =:= empty,
  {Op, NewMap} =
    if
      EmptyNeighbor -> {TV, TM} = maps:take(Origin, Map), {moved, maps:put(move_coords(Origin, Dir), TV, TM)};
      true -> {stayed, Map}
    end,
  if
    Op =:= moved -> move(move_coords(Origin, Dir), Max, NewMap, Dir);
    true -> NewMap
  end.

get_neighbor({X, Y}, _, Map, north) -> 
  if
    Y =:= 0 -> cube;
    Y > 0 -> maps:get(move_coords({X,Y}, north), Map, empty)
  end;
get_neighbor({X, Y}, _, Map, west) -> 
  if
    X =:= 0 -> cube;
    X > 0 -> maps:get(move_coords({X,Y}, west), Map, empty)
  end;
get_neighbor({X, Y}, {_, MaxY}, Map, south) -> 
  if
    Y =:= MaxY -> cube;
    Y < MaxY -> maps:get(move_coords({X,Y}, south), Map, empty)
  end;
get_neighbor({X, Y}, {MaxX, _}, Map, east) -> 
  if
    X =:= MaxX -> cube;
    X < MaxX -> maps:get(move_coords({X,Y}, east), Map, empty)
  end.

% detect_cycle([A]) -> false;
% detect_cycle([A,B]) -> false;
% detect_cycle(Values) ->
%   detect_cycle(Values, 1, 2).

% detect_cycle(Values, ALoc, BLoc) ->
%   F = fun (N) -> lists:nth(N) end,
%   A = F(ALoc),
%   B = F(BLoc),
%   if
%     A =/= B -> detect_cycle(Values, ALoc+1, BLoc+2);
%     true -> detect_phase_start(Values, 1, BLoc, 0)
%   end.

% detect_phase_start(Values, ALoc, BLoc, Phase) ->
%   F = fun (N) -> lists:nth(N) end,
%   A = F(ALoc),
%   B = F(BLoc),
%   if
%     A =/= B -> detect_phase_start(Values, ALoc+1, BLoc+1, Phase+1)
%     true -> Phase
%   end.
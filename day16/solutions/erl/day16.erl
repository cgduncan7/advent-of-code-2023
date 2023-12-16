-module(day16).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {{0,0}, #{}}, bytes),
  harness:run("Part 2", Filename, fun solve_pt2/2, {{0,0}, #{}}, bytes).

solve_pt1(Data, {{X, Y}, Map}) ->
  case Data of
    nil -> fire_the_laser({X-1,Y}, {0, 0, east}, Map);
    "\n" -> {{0, Y+1}, Map};
    "." -> {{X+1, Y}, Map};
    "-" -> {{X+1, Y}, maps:put({X,Y}, hsplitter, Map)};
    "|" -> {{X+1, Y}, maps:put({X,Y}, vsplitter, Map)};
    "/" -> {{X+1, Y}, maps:put({X,Y}, fmirror, Map)};
    "\\" -> {{X+1, Y}, maps:put({X,Y}, bmirror, Map)}
  end.

solve_pt2(Data, {{X, Y}, Map}) ->
  case Data of
    nil -> fire_all_the_lasers({X-1,Y}, Map);
    "\n" -> {{0, Y+1}, Map};
    "." -> {{X+1, Y}, Map};
    "-" -> {{X+1, Y}, maps:put({X,Y}, hsplitter, Map)};
    "|" -> {{X+1, Y}, maps:put({X,Y}, vsplitter, Map)};
    "/" -> {{X+1, Y}, maps:put({X,Y}, fmirror, Map)};
    "\\" -> {{X+1, Y}, maps:put({X,Y}, bmirror, Map)}
  end.

fire_all_the_lasers({MaxX, MaxY}, Map) ->
  lists:max(lists:map(fun (Start) -> fire_the_laser({MaxX, MaxY}, Start, Map) end, get_starting_points({MaxX, MaxY}))).

get_starting_points({MaxX, MaxY}) ->
  XVals = lists:seq(0, MaxX),
  YVals = lists:seq(0, MaxY),
  lists:flatten([
    lists:map(fun (X) -> [{X, 0, south}, {X, MaxY, north}] end, XVals),
    lists:map(fun (Y) -> [{0, Y, east}, {MaxX, Y, west}] end, YVals)
  ]).

fire_the_laser({MaxX, MaxY}, Start, Map) ->
  sets:size(sets:fold(fun ({X, Y, _}, A) -> sets:add_element({X,Y}, A) end, sets:new(), laser_step(Start, {MaxX, MaxY}, Map, sets:new(), 0))).

laser_step({CurX, CurY, CurDir}, {MaxX, MaxY}, Map, LaserVertices, Depth) ->
  Exists = sets:is_element({CurX, CurY, CurDir}, LaserVertices) =:= true,
  if 
    Exists -> LaserVertices;
    (CurX > MaxX) or (CurX < 0) or (CurY > MaxY) or (CurY < 0) -> LaserVertices;
    true ->
      NewLocs = move({CurX, CurY, CurDir}, maps:get({CurX, CurY}, Map, empty)),
      FoldFn = fun (C, Acc) -> laser_step(C, {MaxX, MaxY}, Map, sets:add_element({CurX, CurY, CurDir}, Acc), Depth+1) end,
      lists:foldl(FoldFn, LaserVertices, NewLocs)
  end.

move({X, Y, north}, hsplitter) -> [{X-1,Y,west},{X+1,Y,east}];
move({X, Y, north}, fmirror) -> [{X+1,Y,east}];
move({X, Y, north}, bmirror) -> [{X-1,Y,west}];
move({X, Y, north}, _) -> [{X,Y-1,north}];
move({X, Y, south}, hsplitter) -> [{X-1,Y,west},{X+1,Y,east}];
move({X, Y, south}, fmirror) -> [{X-1,Y,west}];
move({X, Y, south}, bmirror) -> [{X+1,Y,east}];
move({X, Y, south}, _) -> [{X,Y+1,south}];
move({X, Y, east}, vsplitter) -> [{X,Y-1,north},{X,Y+1,south}];
move({X, Y, east}, fmirror) -> [{X,Y-1,north}];
move({X, Y, east}, bmirror) -> [{X,Y+1,south}];
move({X, Y, east}, _) -> [{X+1,Y,east}];
move({X, Y, west}, vsplitter) -> [{X,Y-1,north},{X,Y+1,south}];
move({X, Y, west}, fmirror) -> [{X,Y+1,south}];
move({X, Y, west}, bmirror) -> [{X,Y-1,north}];
move({X, Y, west}, _) -> [{X-1,Y,west}].
-module(day10).
-import(harness, [run/3]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, none, all),
  harness:run("Part 2", Filename, fun solve_pt2/2, none, all).

solve_pt1(Data, _) ->
  [_, S, M] = parse_input(binary:bin_to_list(Data)),
  traverse(S, unknown, M, [], pt1) / 2.

solve_pt2(Data, _) ->
  [_, S, M] = parse_input(binary:bin_to_list(Data)),
  area_inside_shape(traverse(S, unknown, M, [], pt2)).

parse_input(Input) ->
  lists:foldl(
    fun (E, [{C, R}, {SC, SR}, M]) ->
      if
        [E] =:= "\n" -> [{0, R+1}, {SC, SR}, M];
        [E] =:= "S" -> [{C+1, R}, {C, R}, maps:put({C, R}, [E], M)];
        [E] =/= "\n" -> [{C+1, R}, {SC, SR}, maps:put({C, R}, [E], M)]
      end
    end,
    [{0, 0}, {0, 0}, maps:new()],
    Input
  ).

traverse(PipeLoc, unknown, PipeMap, [], pt1) ->
  Locs = get_next_pipe_locations(PipeLoc, unknown, PipeMap),
  [L, D] = hd(lists:filter(
    fun ([L, D]) ->
      lists:any(fun ([_, D2]) -> D =:= get_opposite_dir(D2) end, get_next_pipe_locations(L, unknown, PipeMap))
    end,
    lists:filter(fun ([L, _]) -> maps:is_key(L, PipeMap) end, Locs)
  )),
  traverse(L, D, PipeMap, 1, pt1);
traverse(PipeLoc, PrevDir, PipeMap, Steps, pt1) ->
  [L, D] = hd(get_next_pipe_locations(PipeLoc, PrevDir, PipeMap)),
  IsS = maps:get(L, PipeMap) =:= "S",
  if
    IsS =:= true -> Steps + 1;
    true -> traverse(L, D, PipeMap, Steps + 1, pt1)
  end;

traverse(PipeLoc, unknown, PipeMap, [], pt2) ->
  Locs = get_next_pipe_locations(PipeLoc, unknown, PipeMap),
  [L, D] = hd(lists:filter(
    fun ([L, D]) ->
      lists:any(fun ([_, D2]) -> D =:= get_opposite_dir(D2) end, get_next_pipe_locations(L, unknown, PipeMap))
    end,
    lists:filter(fun ([L, _]) -> maps:is_key(L, PipeMap) end, Locs)
  )),
  traverse(L, D, PipeMap, [PipeLoc], pt2);
traverse(PipeLoc, PrevDir, PipeMap, Steps, pt2) ->
  [L, D] = hd(get_next_pipe_locations(PipeLoc, PrevDir, PipeMap)),
  IsS = maps:get(L, PipeMap) =:= "S",
  if
    IsS =:= true -> Steps ++ [PipeLoc];
    true -> lists:flatten(traverse(L, D, PipeMap, lists:append([Steps, [PipeLoc]]), pt2))
  end.

get_next_pipe_locations(PipeLoc, PrevDir, PipeMap) ->
  lists:map(
    fun (D) -> [get_location(PipeLoc, D), D] end,
    lists:filter(fun (E) -> E =/= get_opposite_dir(PrevDir) end, get_pipe_destinations(maps:get(PipeLoc, PipeMap)))
  ).

get_location({X,Y}, north) -> {X,Y-1};
get_location({X,Y}, west) -> {X-1,Y};
get_location({X,Y}, east) -> {X+1,Y};
get_location({X,Y}, south) -> {X,Y+1}.

get_opposite_dir(north) -> south;
get_opposite_dir(south) -> north;
get_opposite_dir(east) -> west;
get_opposite_dir(west) -> east;
get_opposite_dir(X) -> X.

get_pipe_destinations("|") -> [north, south];
get_pipe_destinations("-") -> [east, west];
get_pipe_destinations("L") -> [north, east];
get_pipe_destinations("J") -> [north, west];
get_pipe_destinations("7") -> [south, west];
get_pipe_destinations("F") -> [south, east];
get_pipe_destinations("S") -> [north, south, east, west];
get_pipe_destinations(_) -> [].

% Inner area_inside_shape doesn't consider the fact that the vertices take up space in our world
% so utilizing the power of right-angles we can remove half of the vertices to get rid of this
% occupied space.
area_inside_shape(Vertices) ->
  Area = abs(area_inside_shape(Vertices, 1) / 2),
  Area - (erlang:length(Vertices) / 2) + 1.

% Gauss' Shoelace method :pray: https://en.wikipedia.org/wiki/Shoelace_formula
area_inside_shape(Vertices, Index) ->
  NextIndex =
    if
      Index+1 > erlang:length(Vertices) -> 1;
      true -> Index+1
    end,
  {X1, Y1} = lists:nth(Index, Vertices),
  {X2, Y2} = lists:nth(NextIndex, Vertices),
  if 
    erlang:length(Vertices) =:= Index -> (X1*Y2) - (X2*Y1);
    true -> (X1*Y2) - (X2*Y1) + area_inside_shape(Vertices, Index+1)
  end.
  
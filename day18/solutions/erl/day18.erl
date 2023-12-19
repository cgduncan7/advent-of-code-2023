-module(day18).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {{0,0}, [{0,0}]}, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, {{0,0}, [{0,0}]}, lines).

solve_pt1(Data, {{CX, CY}, Vertices}) ->
  case Data of
    nil -> area_inside_shape(Vertices);
    Line ->
      [Dir, _Steps, _] = string:lexemes(Line, " "),
      {Steps,_} = string:to_integer(_Steps),
      NP = move({CX, CY}, Dir, Steps),
      {NP, Vertices ++ [NP]}
  end.

solve_pt2(Data, {{CX, CY}, Vertices}) ->
  case Data of
    nil -> area_inside_shape(Vertices);
    Line ->
      [_, _, Instruction] = string:lexemes(Line, " ()#"),
      [Digit1, Digit2, Digit3, Digit4, Digit5, Digit6] = Instruction,
      Dir =
        case Digit6 of
          $0 -> "R";
          $1 -> "D";
          $2 -> "L";
          $3 -> "U"
        end,
      Steps = erlang:list_to_integer([Digit1,Digit2,Digit3,Digit4,Digit5], 16),
      NP = move({CX, CY}, Dir, Steps),
      {NP, Vertices ++ [NP]}
  end.

move({X,Y}, "R", Steps) -> {X+Steps,Y};
move({X,Y}, "L", Steps) -> {X-Steps,Y};
move({X,Y}, "D", Steps) -> {X,Y+Steps};
move({X,Y}, "U", Steps) -> {X,Y-Steps}.

diff({X1,Y1},{X2,Y2}) -> abs(Y2-Y1) + abs(X2-X1).


area_inside_shape(Vertices) ->
  Area = abs(area_inside_shape(Vertices, 1) / 2),
  {_, AreaEdge} = lists:foldl(
    fun (C, {Idx, Sum}) ->
      if 
        Idx =:= erlang:length(Vertices) -> {Idx,Sum};
        true -> {Idx+1, Sum + diff(C, lists:nth(Idx+1, Vertices))}
      end
    end,
    {1, 0},
    Vertices
  ),
  (AreaEdge / 2) + Area + 1.

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
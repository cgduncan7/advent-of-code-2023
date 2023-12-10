-module(day08).
-import(harness, [run/5]).
-export([run/1]).

-record(state, {
  instructions,
  nodes=maps:new()
}).

-record(node, {
  id,
  left,
  right
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, #state{}, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, #state{}, lines).

solve_pt1(Data, State) ->
  Instructions = State#state.instructions,
  Nodes = State#state.nodes,
  case Data of
    nil -> traverse(State, "AAA", 0, pt1);
    "" -> State;
    Data -> case Instructions of
      undefined -> #state{instructions=parse_instructions(Data)};
      _ -> 
        {K, V} = parse_node(Data),
        #state{instructions=Instructions, nodes=maps:put(K, V, Nodes)}
    end 
  end.

solve_pt2(Data, State) ->
  Instructions = State#state.instructions,
  Nodes = State#state.nodes,
  case Data of
    nil ->
       M = maps:filter(
        fun (Key, _) -> ends_in_a(Key) end,
        Nodes
      ),
      StartingNodes = maps:keys(M),
      lcm(lists:map(fun (N) -> traverse(State, N, 0, pt2) end, StartingNodes));
    "" -> State;
    Data -> case Instructions of
      undefined -> #state{instructions=parse_instructions(Data)};
      _ -> 
        {K, V} = parse_node(Data),
        #state{instructions=Instructions, nodes=maps:put(K, V, Nodes)}
    end 
  end.

parse_instructions(Data) -> lists:map(fun parse_instruction_char/1, Data).

parse_instruction_char($R) -> right;
parse_instruction_char($L) -> left;
parse_instruction_char(_) -> exit(1).

parse_node(Data) -> 
  [Id, Left, Right] = string:lexemes(Data, " =(),"),
  {Id, #node{id=Id, left=Left, right=Right}}.

get_nth_instruction(Instructions, N) ->
  Index = (N rem erlang:length(Instructions)) + 1,
  lists:nth(Index, Instructions).

traverse(_, "ZZZ", X, pt1) -> X;
traverse(State, Start, X, pt1) ->
  CurrentNode = maps:get(Start, State#state.nodes),
  Instruction = get_nth_instruction(State#state.instructions, X),
  NextNode = traverse_to_node(Instruction, CurrentNode),
  traverse(State, NextNode, X+1, pt1);
traverse(State, Start, X, pt2) ->
  EndsInZ = ends_in_z(Start),
  if
    EndsInZ -> X;
    true ->
      CurrentNode = maps:get(Start, State#state.nodes),
      Instruction = get_nth_instruction(State#state.instructions, X),
      NextNode = traverse_to_node(Instruction, CurrentNode),
      traverse(State, NextNode, X+1, pt2)
  end.

traverse_to_node(left, #node{left=Left}) -> Left;
traverse_to_node(right, #node{right=Right}) -> Right.

ends_in_a(S) -> hd(lists:reverse(S)) =:= $A.
ends_in_z(S) -> hd(lists:reverse(S)) =:= $Z.

gcd(A, 0) -> A;
gcd(A, B) ->
  if
    A > B -> gcd(B, A rem B);
    A < B -> gcd(A, B rem A);
    true -> A
  end.

lcm(A, B) -> round(A * (B / gcd(A, B))).

lcm([A,B]) -> lcm(A,B);
lcm([A,B|Rest]) -> lcm([lcm(A, B)] ++ Rest).
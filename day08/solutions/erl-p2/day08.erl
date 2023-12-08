-module(day08).
-import(harness, [run/3]).
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
  harness:run(Filename, fun solve/2, #state{}, lines).

solve(Data, State) ->
  Instructions = State#state.instructions,
  Nodes = State#state.nodes,
  case Data of
    nil ->
       M = maps:filter(
        fun (Key, _) -> ends_in_a(Key) end,
        Nodes
      ),
      StartingNodes = maps:keys(M),
      lcm(lists:map(fun (N) -> traverse(State, N, 0) end, StartingNodes));
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

traverse(State, Start, X) ->
  EndsInZ = ends_in_z(Start),
  if
    EndsInZ -> X;
    true ->
      CurrentNode = maps:get(Start, State#state.nodes),
      Instruction = get_nth_instruction(State#state.instructions, X),
      NextNode = traverse_to_node(Instruction, CurrentNode),
      traverse(State, NextNode, X+1)
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
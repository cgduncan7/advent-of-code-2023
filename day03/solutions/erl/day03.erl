-module(day03).
-import(harness, [run/3]).
-export([run/1]).

-record(symbol, {
  char,
  location
}).

-record(state, {
  currentCell={0,0},
  unique_parts=0,
  parts=dict:new(),
  symbols=[],
  working_part=queue:new()
}).

-record(part, {
  id,
  number
}).

run(Filename) ->
  harness:run(Filename, fun solve/2, #state{}, bytes).

solve(Data, State) ->
  case Data of
    nil -> {collect_connected_parts(State), collect_gear_ratios(State)};
    "." -> next_state(State, nextcol);
    "\n" -> next_state(State, nextrow);
    D -> next_state(State, D)
  end.

next_state(CurrentState, nextcol) ->
  #state{
    currentCell=inc_cell_col(CurrentState#state.currentCell),
    parts=next_parts(CurrentState),
    unique_parts=next_unique_parts(CurrentState),
    symbols=CurrentState#state.symbols,
    working_part=next_working_part(CurrentState)
  };
next_state(CurrentState, nextrow) ->
  #state{
    currentCell=inc_cell_row(CurrentState#state.currentCell),
    parts=next_parts(CurrentState),
    unique_parts=next_unique_parts(CurrentState),
    symbols=CurrentState#state.symbols,
    working_part=next_working_part(CurrentState)
  };
next_state(CurrentState, Data) ->
  case is_symbol(Data) of
    true ->
      NextCell = inc_cell_col(CurrentState#state.currentCell),
      NewSymbol = #symbol{char=Data,location=NextCell},
      #state{
        currentCell=NextCell,
        parts=next_parts(CurrentState),
        unique_parts=next_unique_parts(CurrentState),
        symbols=lists:append(CurrentState#state.symbols, [NewSymbol]),
        working_part=next_working_part(CurrentState)
      };
    false ->
      {Val,_} = string:to_integer(Data),
      #state{
        currentCell=inc_cell_col(CurrentState#state.currentCell),
        parts=CurrentState#state.parts,
        unique_parts=CurrentState#state.unique_parts,
        symbols=CurrentState#state.symbols,
        working_part=queue:in(Val, CurrentState#state.working_part)
      }
  end.

next_unique_parts(State) -> 
  case queue:len(State#state.working_part) of
    0 -> State#state.unique_parts;
    _ -> State#state.unique_parts + 1
  end.

next_parts(State) ->
  case queue:len(State#state.working_part) of
    0 -> State#state.parts;
    _ -> lists:foldl(
      fun ({K,V}, A) -> dict:store(K,V,A) end,
      State#state.parts,
      finish_working_part(State#state.working_part, State#state.currentCell, State#state.unique_parts)
    )
  end.

next_working_part(State) ->
  case queue:len(State#state.working_part) of
    0 -> State#state.working_part;
    _ -> queue:new()
  end.

is_symbol("0") -> false;
is_symbol("1") -> false;
is_symbol("2") -> false;
is_symbol("3") -> false;
is_symbol("4") -> false;
is_symbol("5") -> false;
is_symbol("6") -> false;
is_symbol("7") -> false;
is_symbol("8") -> false;
is_symbol("9") -> false;
is_symbol(_) -> true.

dec_cell_col({X,Y},Amt) -> {X-Amt, Y}.
inc_cell_col({X,Y}) -> {X+1, Y}.
inc_cell_row({_,Y}) -> {0, Y+1}.

get_location({X,Y},aboveleft) -> {X-1,Y-1};
get_location({X,Y},above) -> {X,Y-1};
get_location({X,Y},aboveright) -> {X+1,Y-1};
get_location({X,Y},left) -> {X-1,Y};
get_location({X,Y},right) -> {X+1,Y};
get_location({X,Y},belowleft) -> {X-1,Y+1};
get_location({X,Y},below) -> {X,Y+1};
get_location({X,Y},belowright) -> {X+1,Y+1}.

get_locations(Location, Len) ->
  Seq = lists:seq(0, Len),
  lists:map(fun (A) -> dec_cell_col(Location, A) end, Seq).

finish_working_part(Queue, Location, Id) ->
  Len = queue:len(Queue),
  Locations = get_locations(Location, Len - 1),
  Numbers = lists:duplicate(Len, queue:fold(fun (Cur, Acc) -> Acc*10 + Cur end, 0, Queue)),
  Ids = lists:duplicate(Len, Id),
  lists:zipwith3(fun (A,B,C) -> {A, #part{id=C,number=B}} end, Locations, Numbers, Ids).

get_adjacent_locations(Location) ->
  [
    get_location(Location,aboveleft),
    get_location(Location,above),
    get_location(Location,aboveright),
    get_location(Location,left),
    get_location(Location,right),
    get_location(Location,belowleft),
    get_location(Location,below),
    get_location(Location,belowright)
  ].

collect_connected_parts(State) ->
  AdjacentLocations = lists:foldl(fun(S, Acc) -> lists:append(Acc, get_adjacent_locations(S#symbol.location)) end, [], State#state.symbols),
  PartsToAdd = lists:foldl(
    fun (C, A) ->
      case dict:find(C, State#state.parts) of
        error -> A;
        {ok, Val} ->
          case lists:search(fun (T) -> T#part.id =:= Val#part.id end, A) of
            false -> A ++ [Val];
            _ -> A
          end
      end
    end,
    [],
    AdjacentLocations
  ),
  lists:foldl(fun (P, A) -> P#part.number + A end, 0, PartsToAdd).

collect_gear_ratios(State) ->
  AdjacentLocationsOfGears = lists:foldl(
    fun(S, Acc) -> lists:append(Acc, [get_adjacent_locations(S#symbol.location)]) end,
    [],
    lists:filter(fun(S) -> S#symbol.char =:= "*" end, State#state.symbols)
  ),
  GetGearParts = fun (S) -> 
    lists:foldl(
      fun (C, A) ->
        case dict:find(C, State#state.parts) of
          error -> A;
          {ok, Val} ->
            case lists:search(fun (T) -> T#part.id =:= Val#part.id end, A) of
              false -> A ++ [Val];
              _ -> A
            end
        end
      end,
      [],
      S
    )
  end,
  lists:foldl(
    fun(S, Acc) ->
      Acc + case GetGearParts(S) of
        [A,B] -> A#part.number * B#part.number;
        _ -> 0
      end
    end,
    0,
    AdjacentLocationsOfGears
  ).
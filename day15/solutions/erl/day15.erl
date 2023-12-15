-module(day15).
-import(harness, [run/5]).
-export([run/1]).

-record(lens, {
  label,
  focal_length
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {0, 0}, bytes),
  harness:run("Part 2", Filename, fun solve_pt2/2, {0, "", #{}, read}, bytes).

solve_pt1(Data, {Sum, Current}) ->
  case Data of
    nil -> Sum + Current;
    "," -> {Sum + Current, 0};
    C -> {Sum, hash(Current, C)}
  end.

solve_pt2(Data, {CurrentHash, CurrentLabel, Map, Action}) ->
  case Data of
    nil -> calculate_focusing_power(Map);
    "," -> {0, "", Map, read};
    "=" -> {CurrentHash, CurrentLabel, Map, store};
    "-" -> {CurrentHash, CurrentLabel, remove_lens(CurrentLabel, CurrentHash, Map), remove};
    C -> 
      case Action of
        read -> {hash(CurrentHash, C), CurrentLabel ++ C, Map, read};
        store -> 
          {V,_} = string:to_integer(C),
          {CurrentHash, CurrentLabel, store_lens(#lens{label=CurrentLabel, focal_length=V}, CurrentHash, Map), read}
      end
  end.

hash(Current, [C]) ->
  ((Current + C) * 17) rem 256.

store_lens(Lens, Hash, Map) ->
  maps:update_with(
    Hash,
    fun (A) ->
      Label = Lens#lens.label,
      case lists:any(fun (#lens{label=L}) -> L =:= Label end, A) of
        false -> A ++ [Lens];
        true -> lists:map(fun (E) -> if E#lens.label =/= Label -> E; E#lens.label =:= Label -> Lens end end, A)
      end
    end,
    [Lens],
    Map
  ).

remove_lens(Label, Hash, Map) ->
  try maps:get(Hash, Map) of 
    Box -> maps:update(Hash, lists:filter(fun (#lens{label=CL}) -> CL =/= Label end, Box), Map)
  catch
    error:_ -> Map
  end.

calculate_focusing_power(Map) ->
  lists:sum(lists:flatmap(fun calculate_box_focusing_power/1, maps:to_list(Map))).

calculate_box_focusing_power({BoxNum, Lenses}) ->
  lists:map(fun ({I, L}) -> calculate_lens_focusing_power(BoxNum, I, L) end, lists:enumerate(Lenses)).

calculate_lens_focusing_power(BoxNum, SlotNum, #lens{focal_length=FocalLength}) ->
  (BoxNum + 1) * SlotNum * FocalLength.
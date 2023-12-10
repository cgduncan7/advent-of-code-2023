-module(day05).
-import(harness, [run/5]).
-export([run/1]).

-record(mapentry, {
  dest_start,
  source_start,
  range
}).

-record(seed_pair, {
  start,
  range
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {[], []}, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, {[], []}, lines).

solve_pt1(Data, {Map, SourceValues}) ->
  case erlang:length(SourceValues) of
    0 -> {[], extract_seed_numbers(Data)};
    _ -> case Data of
        nil -> lists:min(convert_all_values(Map, SourceValues, pair));
        [] -> {[], convert_all_values(Map, SourceValues, pair)};
        D -> case is_map_start(D) of
          true -> {Map, SourceValues}; % just skip the line
          false ->
            Entry = create_map_entry(D),
            {lists:append(Map, [Entry]), SourceValues}
        end
      end
  end.

solve_pt2(Data, {Map, SourceValues}) ->
  case erlang:length(SourceValues) of
    0 -> {[], extract_seed_pairs(extract_seed_numbers(Data), [])};
    _ -> case Data of
        nil -> SP = lists:min(convert_all_values(Map, SourceValues, range)), SP#seed_pair.start;
        [] -> {[], convert_all_values(Map, SourceValues, range)};
        D -> case is_map_start(D) of
          true -> {Map, SourceValues}; % just skip the line
          false ->
            Entry = create_map_entry(D),
            {lists:append(Map, [Entry]), SourceValues}
        end
      end
  end.

is_map_start(Line) ->
  case string:find(Line, "map:") of
    nomatch -> false;
    _ -> true
  end.

extract_seed_numbers(Line) -> 
  lists:map(
    fun (S) -> {V,_} = string:to_integer(S), V end,
    tl(string:lexemes(Line, ": "))
  ).

extract_seed_pairs([], Values) -> Values;
extract_seed_pairs([First,Second|Rest], Values) ->
  extract_seed_pairs(Rest,lists:append(Values,[#seed_pair{start=First,range=Second}])).

% dest_start source_start range
create_map_entry(Data) ->
  [Dest, Source, Range] = lists:map(fun (S) -> {V,_} = string:to_integer(S), V end, string:lexemes(Data, " ")),
  #mapentry{dest_start=Dest, source_start=Source, range=Range}.

convert_all_values([], Values, _) -> Values;
convert_all_values(_, [], _) -> [];
convert_all_values(Map, Values, Type) ->
  lists:flatten(lists:map(fun (V) -> convert_single_value(Map, V, Type) end, Values)).

convert_single_value(Map, Value, pair) ->
  case lists:search(fun (ME) -> (Value >= ME#mapentry.source_start) and (Value - ME#mapentry.source_start =< ME#mapentry.range) end, Map) of
    {value, V} -> V#mapentry.dest_start + (Value - V#mapentry.source_start);
    _ -> Value
  end;
convert_single_value([], Value, range) -> [Value];
convert_single_value(Map, Value, range) ->
  {M, O} = lists:foldl(
    fun(ME, {Mapped, Original}) ->
      SP = split_seedpair_by_mapentry(Value, ME),
      case SP of
        {none, _} -> {Mapped, Original};
        {In, _} ->
          DestSeedPair = #seed_pair{start=ME#mapentry.dest_start + (In#seed_pair.start - ME#mapentry.source_start), range=In#seed_pair.range},
          {sets:add_element(DestSeedPair, Mapped),sets:add_element(In, Original)}
      end
    end,
    {sets:new(), sets:new()},
    Map
  ),
  Remaining = get_remaining_range(O, Value),
  Y = lists:append([sets:to_list(M), Remaining]),
  case erlang:length(Y) of
    0 -> [Value];
    _ -> Y
  end. 

get_range(S, E) -> E - S + 1.

get_remaining_range(Set, SeedPair) ->
  SortedSeedPairs = lists:sort(sets:to_list(Set)),
  [SPs, End] = lists:foldl(
    fun (C, [List, PotentialStart]) ->
      if
        PotentialStart < C#seed_pair.start ->
          [
            lists:append([List, [#seed_pair{start=PotentialStart, range=get_range(PotentialStart, C#seed_pair.start-1)}]]),
            C#seed_pair.start + C#seed_pair.range
          ];
        true ->
          [
            List,
            PotentialStart + C#seed_pair.range
          ]
      end
    end,
    [[], SeedPair#seed_pair.start],
    SortedSeedPairs
  ),
  if 
    End =:= SeedPair#seed_pair.start + SeedPair#seed_pair.range -> SPs;
    true -> lists:append([SPs, [#seed_pair{start=End, range=get_range(End, SeedPair#seed_pair.start + SeedPair#seed_pair.range - 1)}]])
  end.

split_seedpair_by_mapentry(SeedPair, MapEntry) ->
  SeedPairStart = SeedPair#seed_pair.start,
  SeedPairEnd = SeedPair#seed_pair.start + SeedPair#seed_pair.range - 1,
  MapEntrySourceStart = MapEntry#mapentry.source_start,
  MapEntrySourceEnd = MapEntry#mapentry.source_start + MapEntry#mapentry.range - 1,
  PotentiallyMappableSeedStart = erlang:max(SeedPairStart, MapEntrySourceStart),
  PotentiallyMappableSeedEnd = erlang:min(SeedPairEnd, MapEntrySourceEnd),
  PotentiallyMappableSeedPair = #seed_pair{start=PotentiallyMappableSeedStart, range=get_range(PotentiallyMappableSeedStart, PotentiallyMappableSeedEnd)},
  IsMappable = PotentiallyMappableSeedPair#seed_pair.range >= 0,
  if
    IsMappable =:= true ->
      LeftRemainingSeedPair = if
        PotentiallyMappableSeedStart =:= SeedPairStart -> none;
        SeedPairStart =/= PotentiallyMappableSeedStart -> #seed_pair{start=SeedPairStart, range=get_range(SeedPairStart, PotentiallyMappableSeedStart-1)}
      end,
      RightRemainingSeedPair = if
        SeedPairEnd =:= PotentiallyMappableSeedEnd -> none;
        SeedPairEnd =/= PotentiallyMappableSeedEnd -> #seed_pair{start=PotentiallyMappableSeedEnd+1, range=get_range(PotentiallyMappableSeedEnd+1, SeedPairEnd)}
      end,
      RemainingSeedPairs = lists:filter(fun (X) -> X =/= none end, [LeftRemainingSeedPair, RightRemainingSeedPair]),
      {PotentiallyMappableSeedPair, RemainingSeedPairs};
    IsMappable =:= false -> {none, [SeedPair]}
  end.
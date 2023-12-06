-module(day05).
-import(harness, [run/3]).
-export([run/1]).

-record(mapentry, {
  dest_start,
  source_start,
  range
}).

run(Filename) ->
  harness:run(Filename, fun solve/2, {[], []}, lines).

solve(Data, {Map, SourceValues}) ->
  case erlang:length(SourceValues) of
    0 -> {[], extract_seed_numbers(Data)};
    _ -> case Data of
        nil -> lists:min(convert_all_values(Map, SourceValues));
        [] -> {[], convert_all_values(Map, SourceValues)};
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

extract_seed_numbers(Line) -> lists:map(fun (S) -> {V,_} = string:to_integer(S), V end, tl(string:lexemes(Line, ": "))).

% dest_start source_start range
create_map_entry(Data) ->
  [Dest, Source, Range] = lists:map(fun (S) -> {V,_} = string:to_integer(S), V end, string:lexemes(Data, " ")),
  #mapentry{dest_start=Dest, source_start=Source, range=Range}.

convert_all_values(Map, Values) ->
  lists:map(fun (V) -> convert_single_value(Map, V) end, Values).

convert_single_value(Map, Value) ->
  case lists:search(fun (ME) -> (Value >= ME#mapentry.source_start) and (Value - ME#mapentry.source_start =< ME#mapentry.range) end, Map) of
    {value, V} -> V#mapentry.dest_start + (Value - V#mapentry.source_start);
    _ -> Value
  end.
-module(day20).
-import(harness, [run/5]).
-export([run/1]).

-record(module, {
  name,
  type,
  destinations,
  state
}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, #{}, lines),
  harness:run("Part 2", Filename, fun solve_pt2/2, maps:put("rx", #module{name="rx",type=final,destinations=[],state=off}, maps:new()), lines).

solve_pt1(Data, Modules) ->
  case Data of
    nil -> M = update_conjunctions(Modules), {_, {H,L}} = push_button_times(1000, {M, {0,0}}), H*L;
    Line -> {Name, Module} = parse_line(Line), maps:put(Name, Module, Modules)
  end.

solve_pt2(Data, Modules) ->
  case Data of
    nil -> M = update_conjunctions(Modules), get_chain(Modules), {_, {H,L}} = push_button_until_rx_on({M, 1}), H*L;
    Line -> {Name, Module} = parse_line(Line), maps:put(Name, Module, Modules)
  end.

parse_line([$%|Rest]) ->
  [Name|Destinations] = string:lexemes(Rest, " ->,"),
  {Name, #module{name=Name,type=flipflop,destinations=Destinations,state=off}};

parse_line([$&|Rest]) ->
  [Name|Destinations] = string:lexemes(Rest, " ->,"),
  {Name, #module{name=Name,type=conjunction,destinations=Destinations,state=#{}}};

parse_line(Line) ->
  [_|Destinations] = string:lexemes(Line, " ->,"),
  {"broadcaster", #module{name="broadcaster",type=broadcaster,destinations=Destinations,state=none}}.


update_conjunctions(Modules) ->
  Conjunctions = maps:filter(fun (_, #module{type=Type}) -> Type =:= conjunction end, Modules),
  maps:fold(fun (_, V, A) -> update_conjunction(V, A) end, Modules, Conjunctions).

update_conjunction(#module{name=Name,type=T,destinations=D,state=_}, Modules) ->
  UpdatedState = maps:from_list(lists:map(fun ({K,_}) -> {K,low} end, maps:to_list(maps:filter(fun (_, #module{destinations=ID}) -> lists:member(Name, ID) end, Modules)))),
  maps:update(Name, #module{name=Name,type=T,destinations=D,state=UpdatedState}, Modules).


get_chain(Modules) ->
  get_chain("broadcaster", Modules, []).

get_chain(Name, Modules, Chain) ->
  #module{name=Name, type=Type, destinations=Destinations} = maps:get(Name, Modules),
  IsMember = lists:member(Name, Chain),
  io:fwrite("~s~s~n", [lists:duplicate(length(Chain), $-), Name]),
  if
    IsMember -> Chain;
    true ->
      lists:map(
        fun (D) -> get_chain(D, Modules, Chain ++ [Name]) end,
        Destinations
      )
  end.


push_button_times(1, {Modules, Pulses}) -> push_button({Modules, Pulses});
push_button_times(Times, {Modules, Pulses}) ->
  {NextModules, NextPulses} = push_button({Modules, Pulses}),
  push_button_times(Times-1, {NextModules, NextPulses}).

push_button_until_rx_on({Modules, Times}) ->
  {NextModules, _} = push_button({Modules, {0,0}}),
  Conjunctions = maps:filter(fun (_, #module{name=Name,type=Type}) -> (lists:member(Name,["qx"])) and (Type =:= conjunction) end, NextModules),
  AllHigh = ["jz","zt","qx","jg","pn"],
  NotAllHigh = ["rn","vf","mk","dh"],
  maps:foreach(
    fun (_, #module{name=Name, state=State}) ->
      % io:fwrite("~w~n", [maps:values(State)]),
      timer:sleep(10),
      AllHighState = maps:size(maps:filter(fun (_, V) -> V =:= high end, State)) =:= maps:size(State),
      CorrectState = (lists:member(Name, AllHigh) and AllHighState) or (lists:member(Name, NotAllHigh) and not AllHighState),
      if
        CorrectState -> io:fwrite("~w ~s in correct state~n", [Times, Name]);
        true -> noop
      end
    end,
    Conjunctions
  ),
  RxIsOn = (maps:get("rx", NextModules))#module.state =:= on,
  if
    RxIsOn -> Times;
    true -> push_button_until_rx_on({NextModules, Times+1})
  end.

push_button({Modules, Pulses}) -> 
  act(send_pulse("button", "broadcaster", low, {queue:new(), Modules, Pulses})).

act({Queue, Modules, Pulses}) ->
  Empty = queue:is_empty(Queue),
  if
    Empty -> {Modules, Pulses};
    true -> 
      {{value, {Source, Dest, Pulse}}, NQ} = queue:out(Queue),
      {ResultingPulses, NextModules} = recv_pulse(Pulse, Source, Dest, Modules),
      {NewQueue, NewModules, NewPulses} = lists:foldl(fun ({S,D,P}, {Q,M,TP}) -> send_pulse(S, D, P, {Q, M, TP}) end, {NQ, NextModules, Pulses}, ResultingPulses),
      act({NewQueue, NewModules, NewPulses})
  end.

send_pulse(Source, Dest, low, {Queue, Modules, {High, Low}}) ->
  {queue:in({Source, Dest, low}, Queue), Modules, {High, Low+1}};
send_pulse(Source, Dest, high, {Queue, Modules, {High, Low}}) ->
  {queue:in({Source, Dest, high}, Queue), Modules, {High+1, Low}}.

recv_pulse(Pulse, Source, Destination, Modules) ->
  pulse(Pulse, Source, maps:get(Destination, Modules, Destination), Modules).

pulse(low, _, #module{name=Name,type=final}, Modules) ->
  NextModules = maps:update(Name, #module{name=Name, type=final, state=on}, Modules),
  {[], NextModules};

pulse(low, Source, #module{type=broadcaster, destinations=Destinations}, Modules) ->
  {lists:map(fun (D) -> {"broadcaster", D, low} end, Destinations), Modules};

pulse(low, Source, #module{name=Name, type=flipflop, destinations=Destinations, state=off}, Modules) ->
  NextModules = maps:update(Name, #module{name=Name, type=flipflop, destinations=Destinations, state=on}, Modules),
  {lists:map(fun (D) -> {Name, D, high} end, Destinations), NextModules};

pulse(low, Source, #module{name=Name, type=flipflop, destinations=Destinations, state=on}, Modules) ->
  NextModules = maps:update(Name, #module{name=Name, type=flipflop, destinations=Destinations, state=off}, Modules),
  {lists:map(fun (D) -> {Name, D, low} end, Destinations), NextModules};

pulse(high, Source, #module{name=Name,type=flipflop}, R) ->
  {[], R};

pulse(low, Source, #module{name=Name, type=conjunction, destinations=Destinations, state=State}, Modules) ->
  UpdatedState = maps:update(Source, low, State),
  NextModules = maps:update(Name, #module{name=Name, type=conjunction, destinations=Destinations, state=UpdatedState}, Modules),
  {lists:map(fun (D) -> {Name, D, high} end, Destinations), NextModules};

pulse(high, Source, #module{name=Name, type=conjunction, destinations=Destinations, state=State}, Modules) ->
  UpdatedState = maps:update(Source, high, State),
  NextModules = maps:update(Name, #module{name=Name, type=conjunction, destinations=Destinations, state=UpdatedState}, Modules),
  AllHigh = maps:fold(fun (_, V, Acc) -> Acc and (V =:= high) end, true, UpdatedState),
  PulseValue = if 
    AllHigh -> low;
    true -> high
  end,
  {lists:map(fun (D) -> {Name, D, PulseValue} end, Destinations), NextModules};

pulse(Pulse, Source, Destination, R) ->
  {[], R}.
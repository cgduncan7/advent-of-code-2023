-module(day19).
-import(harness, [run/5]).
-export([run/1]).

-record(workflow, {
  name,
  rules
}).

-record(rule, {
  fn,
  category,
  operator,
  operand,
  result
}).

-record(rating, {x,m,a,s}).

run(Filename) ->
  harness:run("Part 1", Filename, fun solve_pt1/2, {workflows,[#{}, 0]}, lines).

solve_pt1(Data, {InputState, State}) ->
  case Data of
    nil -> lists:nth(2, State);
    "" -> {ratings, State};
    Line -> {InputState, parse_line(Line, InputState, State)}
  end.

parse_line(Line, workflows, [WF, Total]) ->
  NewWF = parse_workflow(Line),
  [maps:put(NewWF#workflow.name, NewWF, WF), Total];
parse_line(Line, ratings, [WF, Total]) -> [WF, Total + evaluate_rating(parse_rating(Line), WF)].

parse_workflow(Line) -> 
  [Name|Rules] = string:lexemes(Line, "{},"),
  #workflow{name=Name, rules=lists:map(fun parse_rule/1, Rules)}.

parse_rule([Category,$>|Rest]) ->
  [Value, Result] = string:lexemes(Rest, ":"),
  {IntValue,_} = string:to_integer(Value),
  Fn = fun (Inc) -> compare($>, IntValue, get_rating_value(Inc, Category)) end,
  #rule{category=Category,fn=Fn,result=Result,operator=$>,operand=IntValue};
parse_rule([Category,$<|Rest]) -> 
  [Value, Result] = string:lexemes(Rest, ":"),
  {IntValue,_} = string:to_integer(Value),
  Fn = fun (Inc) -> compare($<, IntValue, get_rating_value(Inc, Category)) end,
  #rule{category=Category,fn=Fn,result=Result,operator=$<,operand=IntValue};
parse_rule(Rule) -> #rule{fn=fun(_) -> true end, category=none,result=Rule,operator=none,operand=none}.

get_rating_value(#rating{x=X}, $x) -> X;
get_rating_value(#rating{m=M}, $m) -> M;
get_rating_value(#rating{a=A}, $a) -> A;
get_rating_value(#rating{s=S}, $s) -> S.

compare($>, Val, Inc) -> Inc > Val;
compare($<, Val, Inc) -> Inc < Val.

parse_rating(Rating) -> 
  Values = string:lexemes(Rating, "xmas{},="),
  [{X,_},{M,_},{A,_},{S,_}] = lists:map(fun string:to_integer/1, Values),
  #rating{x=X,m=M,a=A,s=S}.

evaluate_rating(Rating, Workflows) ->
  run_workflow((maps:get("in", Workflows))#workflow.rules, Rating, Workflows).
  
run_workflow([#rule{fn=Fn,result=Result}|Rest], Rating, Workflows) ->
  RuleResult = Fn(Rating),
  if
    RuleResult =:= true ->
      case Result of
        "A" -> get_rating_value(Rating, $x) + get_rating_value(Rating, $m) + get_rating_value(Rating, $a) + get_rating_value(Rating, $s);
        "R" -> 0;
        Res -> run_workflow((maps:get(Res, Workflows))#workflow.rules, Rating, Workflows)
      end;
    RuleResult =/= true -> run_workflow(Rest, Rating, Workflows)
  end.

split_range({Lower, Upper}, Value) ->
  if
    Lower > Value -> [{Lower, Upper}];
    Upper < Value -> [{Lower, Upper}];
    Upper > Value -> [{Lower, Value-1}, {Value, Upper}]
  end.

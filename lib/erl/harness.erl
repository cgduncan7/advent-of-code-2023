-module(harness).
-export([run/4]).

handle_open_file(Filename) ->
 case file:open(Filename, read) of
    {ok, IOD} -> IOD;
    {error, Reason} ->
      io:fwrite("Error opening file: ~s~n", [Reason]),
      exit(1)
  end. 

handle_read_data(In, ReadFn, ExecFn, ExecState, noloop) ->
  case ReadFn(In) of
    {ok, Data} -> ExecFn(Data, ExecState);
    {err, Reason} -> io:fwrite("Error reading data: ~s~n", [Reason]);
    eof -> ExecFn(nil, ExecState)
  end;
handle_read_data(In, ReadFn, ExecFn, ExecState, loop) ->
  case ReadFn(In) of
    {ok, Data} -> NextExecState = ExecFn(Data, ExecState), handle_read_data(In, ReadFn, ExecFn, NextExecState, loop);
    {err, Reason} -> io:fwrite("Error reading data: ~s~n", [Reason]);
    eof -> ExecFn(nil, ExecState)
  end.

handle_read_line(eof) -> eof;
handle_read_line({ok, Data}) -> {ok, string:trim(Data, trailing, "\n")};
handle_read_line({error, Reason}) -> {error, Reason}.

timed_run(Exec, Args) ->
  {Time, RetVal} = timer:tc(Exec, Args, millisecond),
  io:fwrite("Answer: ~w~nTime: (~wms / ~ws)~n", [RetVal, Time, Time / 1_000]).

run(Filename, Fn, State, bytes) ->
  IOD = handle_open_file(Filename),
  ReadFn = fun (In) -> file:read(In, 1) end,
  timed_run(fun handle_read_data/5, [IOD, ReadFn, Fn, State, loop]);
run(Filename, Fn, State, lines) -> 
  IOD = handle_open_file(Filename),
  ReadFn = fun (D) -> handle_read_line(file:read_line(D)) end,
  timed_run(fun handle_read_data/5, [IOD, ReadFn, Fn, State, loop]);
run(Filename, Fn, State, all) ->
  timed_run(fun handle_read_data/5, [Filename, fun (FN) -> file:read_file(FN) end, Fn, State, noloop]).

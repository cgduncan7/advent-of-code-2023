-module(harness).
-export([run/3]).

handle_open_file(Filename) ->
 case file:open(Filename, read) of
    {ok, IOD} -> IOD;
    {error, Reason} ->
      io:fwrite("Error opening file: ~s~n", [Reason]),
      exit(1)
  end. 

handle_read_data(In, ReadFn, ExecFn, noloop) ->
  case ReadFn(In) of
    {ok, Data} -> ExecFn(Data);
    {err, Reason} -> io:fwrite("Error reading data: ~s~n", [Reason]);
    eof -> ExecFn(nil)
  end;
handle_read_data(In, ReadFn, ExecFn, loop) ->
  case ReadFn(In) of
    {ok, Data} -> ExecFn(Data), handle_read_data(In, ReadFn, ExecFn, loop);
    {err, Reason} -> io:fwrite("Error reading data: ~s~n", [Reason]);
    eof -> ExecFn(nil)
  end.

handle_read_line(eof) -> eof;
handle_read_line({ok, Data}) -> {ok, string:trim(Data, trailing, "\n")};
handle_read_line({error, Reason}) -> {error, Reason}.

timed_run(Exec, Args) ->
  {Time, RetVal} = timer:tc(Exec, Args, microsecond),
  io:fwrite("Answer: ~w (~wμs)", [RetVal, Time]).

run(Filename, Fn, bytes) ->
  IOD = handle_open_file(Filename),
  ReadFn = fun (In) -> file:read(In, 1) end,
  timed_run(fun handle_read_data/4, [IOD, ReadFn, Fn, loop]);
run(Filename, Fn, lines) -> 
  IOD = handle_open_file(Filename),
  ReadFn = fun (D) -> handle_read_line(file:read_line(D)) end,
  timed_run(fun handle_read_data/4, [IOD, ReadFn, Fn, loop]);
run(Filename, Fn, all) ->
  timed_run(fun handle_read_data/4, [Filename, fun (FN) -> file:read_file(FN) end, Fn, noloop]).

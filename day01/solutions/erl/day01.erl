-module(day01).
-import(harness, [run/5]).
-export([run/1]).

run(Filename) ->
  harness:run("Part 1 & 2", Filename, fun solve/2, 0, lines).

% Glorious
string_number_to_number([$1|_]) -> "1";
string_number_to_number([$o,$n,$e|_]) -> "1";
string_number_to_number([$e,$n,$o|_]) -> "1";

string_number_to_number([$2|_]) -> "2";
string_number_to_number([$t,$w,$o|_]) -> "2";
string_number_to_number([$o,$w,$t|_]) -> "2";

string_number_to_number([$3|_]) -> "3";
string_number_to_number([$t,$h,$r,$e,$e|_]) -> "3";
string_number_to_number([$e,$e,$r,$h,$t|_]) -> "3";

string_number_to_number([$4|_]) -> "4";
string_number_to_number([$f,$o,$u,$r|_]) -> "4";
string_number_to_number([$r,$u,$o,$f|_]) -> "4";

string_number_to_number([$5|_]) -> "5";
string_number_to_number([$f,$i,$v,$e|_]) -> "5";
string_number_to_number([$e,$v,$i,$f|_]) -> "5";

string_number_to_number([$6|_]) -> "6";
string_number_to_number([$s,$i,$x|_]) -> "6";
string_number_to_number([$x,$i,$s|_]) -> "6";

string_number_to_number([$7|_]) -> "7";
string_number_to_number([$s,$e,$v,$e,$n|_]) -> "7";
string_number_to_number([$n,$e,$v,$e,$s|_]) -> "7";

string_number_to_number([$8|_]) -> "8";
string_number_to_number([$e,$i,$g,$h,$t|_]) -> "8";
string_number_to_number([$t,$h,$g,$i,$e|_]) -> "8";

string_number_to_number([$9|_]) -> "9";
string_number_to_number([$n,$i,$n,$e|_]) -> "9";
string_number_to_number([$e,$n,$i,$n|_]) -> "9";

string_number_to_number([$0|_]) -> "0";
string_number_to_number([$z,$e,$r,$o|_]) -> "0";
string_number_to_number([$o,$r,$e,$z|_]) -> "0";

string_number_to_number(_) -> notfound.

find_leftmost_number([]) -> io:fwrite("Couldn't find number!~n"), exit(1);
find_leftmost_number(List) ->
  case string_number_to_number(List) of
    notfound -> find_leftmost_number(tl(List));
    I -> I
  end.

solve(Data, Total) ->
  case Data of
    nil -> Total;
    [] -> [];
    D -> Total + erlang:list_to_integer(find_leftmost_number(D) ++ find_leftmost_number(lists:reverse(D)))
  end.
-module(myLists).
-export([contains/2, duplicateElements/1, sumFloats/1]).


contains([], _) -> false;
contains([H | _], Elem) when H=:=Elem -> true;
contains([_ | Tail], Elem) -> contains(Tail, Elem).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H | duplicateElements(T)].

sumFloats(L) when is_list(L) -> sumFloats(L, 0).

sumFloats([], Acc) -> Acc;
sumFloats([H | T], Acc) when is_float(H) -> sumFloats(T, Acc + H);
sumFloats([_ | T], Acc) -> sumFloats(T, Acc).
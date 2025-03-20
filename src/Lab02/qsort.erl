-module(qsort).
-export([qs/1, random_elems/3, compare_speeds/3]).


less_than(List, Arg) -> [X || X <- List, X<Arg].
grt_eq_than(List, Arg) -> [X || X<- List, X>=Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) -> [rand:uniform(Max-Min+1)+Min-1 || _<-lists:seq(1,N)].

compare_speeds(List, Fun1, Fun2) -> {timer:tc(Fun1,[List]), timer:tc(Fun2,[List])}.
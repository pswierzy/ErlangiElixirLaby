-module('fun').
-export([]).

%% OAEO = fun(Str) ->
%%   lists:map(fun
%%     ($o) -> $a;
%%     ($e) -> $o;
%%     (C) -> C
%%   end, Str)
%% end.

%% Check3 = fun(L) ->
%%   lists:foldl(fun
%%     (E, Acc) when E rem 3 == 0 -> Acc + 1;
%%     (_, Acc) -> Acc
%%   end, 0, L)
%% end.

Wydobycie = fun(L) ->
lists:map(fun
({reading, _, _, _, Results}) -> Results
end, L) end.

Splaszczenie = fun(L) ->
lists:foldl(fun
(Data, Acc) -> Acc ++ Data
end, [], L) end.

Filter = fun(L, PM) ->
[X || {PM, X} <- L] end.

MeanValue = fun(L, PM) ->
Data = Filter(Splaszczenie(Wydobycie(L)), PM),
lists:sum(Data)/length(Data)
end.
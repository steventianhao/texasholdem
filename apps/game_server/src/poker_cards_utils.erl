-module(poker_cards_utils).
-export([combinations/2,poker_combos/1,shuffle/1]).

combinations([],_)->
	[];
combinations(_,0)->
	[];
combinations(L,1)->
	[[C]||C<-L];
combinations([H|T],N)->
	lists:foldl(fun(L,AccIn)->[[H|L]|AccIn] end,combinations(T,N),combinations(T,N-1)).

poker_combos(Cards) when length(Cards)==7 ->
	combinations(Cards,5).

shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).
 
randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->randomize(Acc)end, randomize(List), lists:seq(1, (T - 1))).
 
randomize(List) ->
   D = lists:map(fun(A) ->{random:uniform(), A}end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.
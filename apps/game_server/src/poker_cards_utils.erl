-module(poker_cards_utils).
-export([combinations/2,poker_combos/1]).

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
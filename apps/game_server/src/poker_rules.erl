-module(poker_rules).
-export([combinations/2,is_flush/1,is_straight/1,poker_combos/1,all_cards/0]).

-define(HEART,$H).
-define(DIAMOND,$D).
-define(SPADE,$S).
-define(CLUB,$C).
-define(SUITS,[?HEART,?DIAMOND,?SPADE,?CLUB]).

-define(ACE,$A).
-define(TWO,$2).
-define(THREE,$3).
-define(FOUR,$4).
-define(FIVE,$5).
-define(SIX,$6).
-define(SEVEN,$7).
-define(EIGHT,$8).
-define(NINE,$9).
-define(TEN,$T).
-define(JACK,$J).
-define(QUEEN,$Q).
-define(KING,$K).
-define(RANKS,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).

-record(card,{suit,rank}).

value(#card{rank=?ACE})->
	14;
value(#card{rank=?TWO})->
	2;
value(#card{rank=?THREE})->
	3;
value(#card{rank=?FOUR})->
	4;
value(#card{rank=?FIVE})->
	5;
value(#card{rank=?SIX})->
	6;
value(#card{rank=?SEVEN})->
	7;
value(#card{rank=?EIGHT})->
	8;
value(#card{rank=?NINE})->
	9;
value(#card{rank=?TEN})->
	10;
value(#card{rank=?JACK})->
	11;
value(#card{rank=?QUEEN})->
	12;
value(#card{rank=?KING})->
	13.

poker_combos(Cards) when length(Cards)==7 ->
	combinations(Cards,5).

combinations([],_)->
	[];
combinations(_,0)->
	[];
combinations(L,1)->
	[[C]||C<-L];
combinations([H|T],N)->
	[[H|L]||L <-combinations(T,N-1)]++combinations(T,N).


is_flush([H|_]=Cards) when length(Cards)==5 ->
	lists:all(fun(C)->C#card.suit==H#card.suit end, Cards).

is_straight(Cards) when length(Cards)==5 ->
	Vs=[value(C)||C<-Cards],
	[A,B,C,D,E]=lists:sort(fun(C1,C2)->C1 =< C2 end,Vs),
	B-A==1 andalso C-B==1 andalso D-C==1 andalso E-D==1.

all_cards()->
	[#card{suit=S,rank=R}||S<-?SUITS,R<-?RANKS].
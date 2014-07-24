-module(poker_rules).
-export([all_cards/0,result/1,card/1]).

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

card([S,R])->
	[#card{rank=R,suit=S}];
card([S,R|T])->
	[#card{rank=R,suit=S}|card(T)].

poker_combos(Cards) when length(Cards)==7 ->
	combinations(Cards,5).

combinations([],_)->
	[];
combinations(_,0)->
	[];
combinations(L,1)->
	[[C]||C<-L];
combinations([H|T],N)->
	lists:foldl(fun(L,AccIn)->[[H|L]|AccIn] end,combinations(T,N),combinations(T,N-1)).

group_by(List,KeyFunc) ->
	Fun = fun(C,Acc)->
			V=KeyFunc(C),
			case lists:keyfind(V,1,Acc) of
				false->
					[{V,[C],1}|Acc];
				{V,L,N}->
					[{V,[C|L],N+1}|lists:keydelete(V,1,Acc)]
			end
	end,
	lists:foldl(Fun,[],List).



is_flush([H|_]=Cards) when length(Cards)==5 ->
	lists:all(fun(C)->C#card.suit==H#card.suit end, Cards).

is_straight(Cards) when length(Cards)==5 ->
	Vs=[value(C)||C<-Cards],
	[A,B,C,D,E]=lists:sort(fun(C1,C2)->C1 =< C2 end,Vs),
	B-A==1 andalso C-B==1 andalso D-C==1 andalso E-D==1.

sort_cards_desc(Cards)->
	lists:sort(fun(C1,C2)->value(C1) >= value(C2) end,Cards).

all_cards()->
	[#card{suit=S,rank=R}||S<-?SUITS,R<-?RANKS].



-record(poker,{pattern=undefined,cards=undefined}).

pattern(Name,Cards)->
	#poker{pattern=Name,cards=Cards}.

straight_flush([#card{rank=?ACE}|_]=Cards)->
	#poker{pattern=royal_flush,cards=Cards};
straight_flush(Cards)->
	#poker{pattern=straight_flush,cards=Cards}.

sort_func({V1,_Cs1,N1},{V2,_Cs2,N2}) when N1==N2->
	V1 >= V2;
sort_func({_V1,_Cs1,N1},{_V2,_Cs2,N2})->
	N1 > N2.

other_result(Cards)->
	R=lists:sort(fun sort_func/2,group_by(Cards,fun(C)->value(C) end)),
	case R of
		[{_,_,4},{_,_,1}]->
			pattern(four_of_a_kind,Cards);
		[{_,_,3},{_,_,2}]->
			pattern(full_house,Cards);
		[{_,_,3},{_,_,1},{_,_,1}]->
			pattern(three_of_a_kind,Cards);
		[{_,_,2},{_,_,2},{_,_,1}]->
			pattern(two_pair,Cards);
		[{_,_,2},{_,_,1},{_,_,1},{_,_,1}]->
			pattern(one_pair,Cards);
		[{_,_,1},{_,_,1},{_,_,1},{_,_,1},{_,_,1}]->
			pattern(high_card,Cards)
	end.

result(Cards) when length(Cards)==5 ->
	case {is_flush(Cards),is_straight(Cards)} of
		{true,true}->
			straight_flush(sort_cards_desc(Cards));
		{true,false}->
			pattern(flush,Cards);
		{false,true}->
			pattern(straight,Cards);
		{false,false}->
			other_result(Cards)
	end.


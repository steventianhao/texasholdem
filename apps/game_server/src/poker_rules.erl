-module(poker_rules).
-export([result/1,compare/2]).
-include("poker_cards.hrl").
-import(poker_cards,[value/1]).

sort({V1,_Cs1,N1},{V2,_Cs2,N2}) when N1==N2->
	V1 >= V2;
sort({_V1,_Cs1,N1},{_V2,_Cs2,N2})->
	N1 > N2.

group_by_value(Cards) ->
	lists:foldl(fun(C,Acc)->
		V=value(C),
		case lists:keyfind(V,1,Acc) of
			false->
				[{V,[C],1}|Acc];
			{V,L,N}->
				[{V,[C|L],N+1}|lists:keydelete(V,1,Acc)]
		end
	end,[],Cards).

other_result(Cards)->
	R=lists:sort(fun sort/2,group_by_value(Cards)),
	case R of
		[{K2,_,4},{K1,_,1}]->
			{four_of_a_kind,?FOUR_OF_A_KIND+weight([K2,K1])};
		[{K2,_,3},{K1,_,2}]->
			{full_house,?FULL_HOUSE+weight([K2,K1])};
		[{K3,_,3},{K2,_,1},{K1,_,1}]->
			{three_of_a_kind,?THREE_OF_A_KIND+weight([K3,K2,K1])};
		[{K3,_,2},{K2,_,2},{K1,_,1}]->
			{two_pair,?TWO_PAIR+weight([K3,K2,K1])};
		[{K4,_,2},{K3,_,1},{K2,_,1},{K1,_,1}]->
			{one_pair,?ONE_PAIR+weight([K4,K3,K2,K1])};
		[{K5,_,1},{K4,_,1},{K3,_,1},{K2,_,1},{K1,_,1}]->
			{high_card,?HIGH_CARD+weight([K5,K4,K3,K2,K1])}
	end.

is_flush([H|_]=Cards)->
	lists:all(fun(C)->C#card.suit==H#card.suit end, Cards).

is_straight([A,B,C,D,E])->
	case B-C==1 andalso C-D==1 andalso D-E==1 of
		true-> 
			case A-B of
				1->{true,A};
				9->{true,B};
				_->false
			end;
		false->
			false
	end.

result(Cards) when length(Cards)==5 ->
	Scs=lists:sort(fun(C1,C2)->value(C1) >= value(C2) end,Cards),
	Vs=[value(C)||C<-Scs],
	case {is_flush(Scs),is_straight(Vs)} of
		{true,{true,14}}->
			{royal_flush,?ROYAL_FLUSH};
		{true,{true,Max}}->
			{straight_flush,?STRAIGHT_FLUSH+Max};
		{true,false}->
			{flush,?FLUSH+weight(Vs)};
		{false,{true,Max}}->
			{straight,?STRAIGHT+Max};
		{false,false}->
			other_result(Scs)
	end.

comp(A,B) when is_integer(A) andalso is_integer(B) andalso A==B-> 0;
comp(A,B) when is_integer(A) andalso is_integer(B) andalso A >B-> 1;
comp(A,B) when is_integer(A) andalso is_integer(B) -> -1.
compare({_P1,V1},{_P2,V2})-> comp(V1,V2).

weight(List)->
	{_,Total}=lists:foldr(fun(E,{N,V})->{N*20,E*N+V} end,{1,0},List),
	Total.
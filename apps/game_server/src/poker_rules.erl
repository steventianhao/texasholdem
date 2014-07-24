-module(poker_rules).
-export([result/1]).
-include("poker_cards.hrl").
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

group_by(Cards) ->
	lists:foldl(fun(C,Acc)->
		V=poker_cards:value(C),
		case lists:keyfind(V,1,Acc) of
			false->
				[{V,[C],1}|Acc];
			{V,L,N}->
				[{V,[C|L],N+1}|lists:keydelete(V,1,Acc)]
		end
	end,[],Cards).

other_result(Cards)->
	R=lists:sort(fun sort_func/2,group_by(Cards)),
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

is_flush([H|_]=Cards)->
	lists:all(fun(C)->C#card.suit==H#card.suit end, Cards).

is_straight(Cards)->
	[A,B,C,D,E]=[poker_cards:value(C)||C<-Cards],
	A-B==1 andalso B-C==1 andalso C-D==1 andalso D-E==1.

sort_cards_desc(Cards)->
	lists:sort(fun(C1,C2)->poker_cards:value(C1) >= poker_cards:value(C2) end,Cards).

result(Cards) when length(Cards)==5 ->
	Scs=sort_cards_desc(Cards),
	case {is_flush(Scs),is_straight(Scs)} of
		{true,true}->
			straight_flush(Scs);
		{true,false}->
			pattern(flush,Scs);
		{false,true}->
			pattern(straight,Scs);
		{false,false}->
			other_result(Scs)
	end.
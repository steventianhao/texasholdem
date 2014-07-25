-module(poker_rules_tests).
-define(DEBUG,true).
-include_lib("eunit/include/eunit.hrl").
-include("poker_cards.hrl").

c(L)->
	poker_cards:card(L).

c_test()->
	Cs=c("HT"),
	?assertEqual([{card,$H,$T}],Cs).

result(royal_flush)->
	{royal_flush,?ROYAL_FLUSH}.

result(straight_flush,[Max])->
	{straight_flush,?STRAIGHT_FLUSH+Max};
result(four_of_a_kind,[C2,C1])->
	{four_of_a_kind,?FOUR_OF_A_KIND+C2*10+C1};
result(full_house,[C2,C1])->
	{full_house,?FULL_HOUSE+C2*10+C1};
result(flush,[C5,C4,C3,C2,C1])->
	{flush,?FLUSH+C5*10000+C4*1000+C3*100+C2*10+C1};
result(straight,[Max])->
	{straight,?STRAIGHT+Max};
result(three_of_a_kind,[C3,C2,C1])->
	{three_of_a_kind,?THREE_OF_A_KIND+C3*100+C2*10+C1};
result(two_pair,[C3,C2,C1])->
	{two_pair,?TWO_PAIR+C3*100+C2*10+C1};
result(one_pair,[C4,C3,C2,C1])->
	{one_pair,?ONE_PAIR+C4*1000+C3*100+C2*10+C1};
result(high_card,[C5,C4,C3,C2,C1])->
	{high_card,?HIGH_CARD+C5*10000+C4*1000+C3*100+C2*10+C1}.


c2_test()->
	Cs=c("HTS5"),
	?assertEqual([{card,$H,$T},{card,$S,$5}],Cs).

result_test()->
	Result=poker_rules:result(c("HTHJHQHKHA")),
	?debugVal(Result),
	?assertEqual(result(royal_flush),Result).

compare_test()->
	R1=poker_rules:result(c("HTHJHQHKHA")),
	R2=poker_rules:result(c("CTCJCQCKCA")),
	Comp1=poker_rules:compare(R1,R2),
	?assertEqual(0,Comp1),
	Comp2=poker_rules:compare(R2,R1),
	?assertEqual(0,Comp2).

result2_test()->
	Result=poker_rules:result(c("HTHJHQHKH9")),
	?debugVal(Result),
	?assertEqual(result(straight_flush,[13]),Result).

result2a_test()->
	Cs=c("H5H4H3H2HA"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(straight_flush,[5]),Result).

result3_test()->
	Cs=c("HTSJHQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(straight,[13]),Result).

result3a_test()->
	Cs=c("H5S4H3H2HA"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(straight,[5]),Result).

compare3_test()->
	R1=poker_rules:result(c("HTSJHQHKH9")),
	R2=poker_rules:result(c("H5S4H3H2HA")),
	Comp=poker_rules:compare(R1,R2),
	?assertEqual(1,Comp).

result4_test()->
	Cs=c("HTH3HQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(flush,[13,12,10,9,3]),Result).

result5_test()->
	Cs=c("HTSTDTCTH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(four_of_a_kind,[10,9]),Result).

result6_test()->
	Cs=c("HTSTDTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(full_house,[10,9]),Result).

result7_test()->
	Cs=c("HTS3DTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(two_pair,[10,9,3]),Result).

result8_test()->
	Cs=c("HTS3D9C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(three_of_a_kind,[9,10,3]),Result).

result9_test()->
	Cs=c("HTS3D7C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(one_pair,[9,10,7,3]),Result).

result10_test()->
	Cs=c("HTS3D7C8H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertEqual(result(high_card,[10,9,8,7,3]),Result).
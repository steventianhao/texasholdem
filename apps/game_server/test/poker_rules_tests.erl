-module(poker_rules_tests).
-define(DEBUG,true).
-include_lib("eunit/include/eunit.hrl").
-include("poker_cards.hrl").

c(L)->
	poker_cards:card(L).

c_test()->
	Cs=c("HT"),
	?assertEqual([{card,$H,$T}],Cs).

c2_test()->
	Cs=c("HTS5"),
	?assertEqual([{card,$H,$T},{card,$S,$5}],Cs).

result_test()->
	Result=poker_rules:result(c("HTHJHQHKHA")),
	?debugVal(Result),
	?assertMatch({royal_flush,?ROYAL_FLUSH},Result).

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
	?assertMatch({straight_flush,?STRAIGHT_FLUSH+13},Result).

result2a_test()->
	Cs=c("H5H4H3H2HA"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({straight_flush,?STRAIGHT_FLUSH+5},Result).

result3_test()->
	Cs=c("HTSJHQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({straight,?STRAIGHT+13},Result).

result3a_test()->
	Cs=c("H5S4H3H2HA"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({straight,?STRAIGHT+5},Result).

compare3_test()->
	R1=poker_rules:result(c("HTSJHQHKH9")),
	R2=poker_rules:result(c("H5S4H3H2HA")),
	Comp=poker_rules:compare(R1,R2),
	?assertEqual(1,Comp).

result4_test()->
	Cs=c("HTH3HQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({flush,?FLUSH+13*10000+12*1000+10*100+9*10+3},Result).


result5_test()->
	Cs=c("HTSTDTCTH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({four_of_a_kind,?FOUR_OF_A_KIND+10*10+9},Result).

result6_test()->
	Cs=c("HTSTDTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({full_house,?FULL_HOUSE+10*10+9},Result).

result7_test()->
	Cs=c("HTS3DTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({two_pair,?TWO_PAIR+10*100+9*10+3},Result).

result8_test()->
	Cs=c("HTS3D9C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({three_of_a_kind,?THREE_OF_A_KIND+9*100+10*10+3},Result).

result9_test()->
	Cs=c("HTS3D7C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({one_pair,?ONE_PAIR+9*1000+10*100+7*10+3},Result).

result10_test()->
	Cs=c("HTS3D7C8H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({high_card,?HIGH_CARD+10*10000+9*1000+8*100+7*10+3},Result).
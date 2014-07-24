-module(poker_rules_tests).
-define(DEBUG,true).
-include_lib("eunit/include/eunit.hrl").
c(L)->
	poker_rules:card(L).

c_test()->
	Cs=c("HT"),
	?assertEqual([{card,$H,$T}],Cs).

c2_test()->
	Cs=c("HTS5"),
	?assertEqual([{card,$H,$T},{card,$S,$5}],Cs).

result_test()->
	Cs=c("HTHJHQHKHA"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,royal_flush,_},Result).


result2_test()->
	Cs=c("HTHJHQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,straight_flush,_},Result).

result3_test()->
	Cs=c("HTSJHQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,straight,_},Result).

result4_test()->
	Cs=c("HTH3HQHKH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,flush,_},Result).


result5_test()->
	Cs=c("HTSTDTCTH9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,four_of_a_kind,_},Result).

result6_test()->
	Cs=c("HTSTDTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,full_house,_},Result).

result7_test()->
	Cs=c("HTS3DTC9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,two_pair,_},Result).

result8_test()->
	Cs=c("HTS3D9C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,three_of_a_kind,_},Result).

result9_test()->
	Cs=c("HTS3D7C9H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,one_pair,_},Result).

result10_test()->
	Cs=c("HTS3D7C8H9"),
	Result=poker_rules:result(Cs),
	?debugVal(Result),
	?assertMatch({poker,high_card,_},Result).
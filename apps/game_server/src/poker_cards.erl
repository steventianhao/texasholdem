-module(poker_cards).
-export([card/1,value/1,all_cards/0]).
-include("poker_cards.hrl").

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

all_cards()->
	[#card{suit=S,rank=R}||S<-?SUITS,R<-?RANKS].
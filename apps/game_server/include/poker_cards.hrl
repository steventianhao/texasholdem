-ifndef (PORKER_CARD_HRL).
-define(POKER_CARD_HRL,true).

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

-define(HIGH_CARD,0).
-define(ONE_PAIR,?HIGH_CARD+20*20*20*20*20).
-define(TWO_PAIR,?ONE_PAIR+20*20*20*20).
-define(THREE_OF_A_KIND,?TWO_PAIR+20*20*20).
-define(STRAIGHT,?THREE_OF_A_KIND+20*20*20).
-define(FLUSH,?STRAIGHT+20).
-define(FULL_HOUSE,?FLUSH+20*20*20*20*20).
-define(FOUR_OF_A_KIND,?FULL_HOUSE+20*20).
-define(STRAIGHT_FLUSH,?FOUR_OF_A_KIND+20*20).
-define(ROYAL_FLUSH,?STRAIGHT_FLUSH+20).

-endif.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is rdbms-1.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : rdbms_wsearch_idx.erl
%%% Author  : Hans Nilsson <hans.r.nilsson@ericsson.com>
%%%           Ulf Wiger <ulf.wiger@ericsson.com> (moved into rdbms)
%%% Description : 
%%%
%%% Created : 26 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_wsearch_idx).

-export([idx_sentence/1, idx_sentence/2]).

-export([tst/0, tst/1]).

%%% Use this as a callback for an rdbms attribute index table
%%%
idx_sentence(String, []) ->
    idx(words(String)).

idx_sentence(String) -> idx(words(String)).

tst() -> tst( tst_string() ).

tst(String) ->
    {Time_us,Result} = timer:tc(?MODULE,idx_sentence,[String]),
    io:format("                Time = ~w us\n"
              "         NumWords in = ~w\n"
              "        NumWords out = ~w\n"
              " NumWords unique out = ~w\n"
              "       Time/word(in) = ~w us\n",
              [Time_us, length(String), length(Result),
               length(lists:usort(Result)),
               Time_us/length(String)]).
    
    
    
%%%----------------------------------------------------------------
words(String) -> words(String,[]).
                  
idx(Words) -> lists:foldl(fun idx_word/2, [], Words).
                                  
idx_word(W,Acc) -> 
    W1 = lower_case(W),
    case stop(W1) of
        true -> Acc;
        false -> Stem = porter:stem(W1),
                 if length(Stem) < 2 -> Acc;
                    true ->[Stem|Acc]
                 end
    end.

    
%%%----------------------------------------------------------------

words([], Acc) -> lists:reverse(Acc);
words(Cs, Acc) -> words(Cs, [], Acc).

words("<"++Cs, WordAcc, Acc) -> 
    case lists:dropwhile(fun(C) -> C=/=$> end,Cs) of
        [] -> found_word(Cs,WordAcc,Acc);       % Maybe wasn't html or xml...
        ">"++Cs0 -> found_word(Cs0,WordAcc,Acc) % Was html...
    end;
words([C|Cs], WordAcc, Acc) when $A=<C,C=<$Z -> words(Cs, [C|WordAcc], Acc);
words([C|Cs], WordAcc, Acc) when $a=<C,C=<$z -> words(Cs, [C|WordAcc], Acc);
words([C|Cs], WordAcc, Acc) when $0=<C,C=<$9 -> words(Cs, [C|WordAcc], Acc);
words([C=$_|Cs],WordAcc, Acc) -> words(Cs, [C|WordAcc], Acc);
words([_|Cs], WordAcc, Acc) -> found_word(Cs, WordAcc, Acc);
words([], WordAcc, Acc) -> found_word([], WordAcc, Acc).
    
found_word(Cs, [], Acc) -> words(Cs, Acc);
found_word(Cs, WordAcc, Acc0) -> words(Cs, [lists:reverse(WordAcc)|Acc0]).

%%%----------------------------------------------------------------
%%% Make the word lower case
lower_case(Word) -> lower_case(Word,[]).

lower_case([C|Cs], Acc) -> 
    if
        $A=<C, C=<$Z -> lower_case(Cs, [(C-$A+$a)|Acc]);
        true -> lower_case(Cs, [C|Acc])
    end;
lower_case([], Acc) ->
    lists:reverse(Acc).

%%%================================================================
%% An English stop word list from http://snowball.tartarus.org

stop(W) -> stp(W).

stp("i") -> true;
stp("me") -> true;
stp("my") -> true;
stp("myself") -> true;
stp("we") -> true;
stp("us") -> true;
stp("our") -> true;
stp("ours") -> true;
stp("ourselves") -> true;
stp("you") -> true;
stp("your") -> true;
stp("yours") -> true;
stp("yourself") -> true;
stp("yourselves") -> true;
stp("he") -> true;
stp("him") -> true;
stp("his") -> true;
stp("himself") -> true;
stp("she") -> true;
stp("her") -> true;
stp("hers") -> true;
stp("herself") -> true;
stp("it") -> true;
stp("its") -> true;
stp("itself") -> true;
stp("they") -> true;
stp("them") -> true;
stp("their") -> true;
stp("theirs") -> true;
stp("themselves") -> true;
stp("what") -> true;
stp("which") -> true;
stp("who") -> true;
stp("whom") -> true;
stp("this") -> true;
stp("that") -> true;
stp("these") -> true;
stp("those") -> true;
stp("am") -> true;
stp("is") -> true;
stp("are") -> true;
stp("was") -> true;
stp("were") -> true;
stp("be") -> true;
stp("been") -> true;
stp("being") -> true;
stp("have") -> true;
stp("has") -> true;
stp("had") -> true;
stp("having") -> true;
stp("do") -> true;
stp("does") -> true;
stp("did") -> true;
stp("doing") -> true;
stp("will") -> true;
stp("would") -> true;
stp("shall") -> true;
stp("should") -> true;
stp("can") -> true;
stp("could") -> true;
stp("may") -> true;
stp("might") -> true;
stp("must") -> true;
stp("ought") -> true;
stp("a") -> true;
stp("an") -> true;
stp("the") -> true;
stp("and") -> true;
stp("but") -> true;
stp("if") -> true;
stp("or") -> true;
stp("because") -> true;
stp("as") -> true;
stp("until") -> true;
stp("while") -> true;
stp("of") -> true;
stp("at") -> true;
stp("by") -> true;
stp("for") -> true;
stp("with") -> true;
stp("about") -> true;
stp("against") -> true;
stp("between") -> true;
stp("into") -> true;
stp("through") -> true;
stp("during") -> true;
stp("before") -> true;
stp("after") -> true;
stp("above") -> true;
stp("below") -> true;
stp("to") -> true;
stp("from") -> true;
stp("up") -> true;
stp("down") -> true;
stp("in") -> true;
stp("out") -> true;
stp("on") -> true;
stp("off") -> true;
stp("over") -> true;
stp("under") -> true;
stp("again") -> true;
stp("further") -> true;
stp("then") -> true;
stp("once") -> true;
stp("here") -> true;
stp("there") -> true;
stp("when") -> true;
stp("where") -> true;
stp("why") -> true;
stp("how") -> true;
stp("all") -> true;
stp("any") -> true;
stp("both") -> true;
stp("each") -> true;
stp("few") -> true;
stp("more") -> true;
stp("most") -> true;
stp("other") -> true;
stp("some") -> true;
stp("such") -> true;
stp("no") -> true;
stp("nor") -> true;
stp("not") -> true;
stp("only") -> true;
stp("own") -> true;
stp("same") -> true;
stp("so") -> true;
stp("than") -> true;
stp("too") -> true;
stp("very") -> true;
stp("one") -> true;
stp("every") -> true;
stp("least") -> true;
stp("less") -> true;
stp("many") -> true;
stp("now") -> true;
stp("ever") -> true;
stp("never") -> true;
stp("say") -> true;
stp("says") -> true;
stp("said") -> true;
stp("also") -> true;
stp("get") -> true;
stp("go") -> true;
stp("goes") -> true;
stp("just") -> true;
stp("made") -> true;
stp("make") -> true;
stp("put") -> true;
stp("see") -> true;
stp("seen") -> true;
stp("whether") -> true;
stp("like") -> true;
stp("well") -> true;
stp("back") -> true;
stp("even") -> true;
stp("still") -> true;
stp("way") -> true;
stp("take") -> true;
stp("since") -> true;
stp("another") -> true;
stp("however") -> true;
stp("two") -> true;
stp("three") -> true;
stp("four") -> true;
stp("five") -> true;
stp("first") -> true;
stp("second") -> true;
stp("new") -> true;
stp("old") -> true;
stp("high") -> true;
stp("long") -> true;

stp(_) -> false.

%%%================================================================
tst_string() ->
    "This is the 'official' home page for distribution of the Porter Stemming Al
gorithm, written and maintained by its author, Martin Porter.

The Porter stemming algorithm (or 'Porter stemmer') is a process for removing th
e commoner morphological and inflexional endings from words in English. Its main
 use is as part of a term normalisation process that is usually done when settin
g up Information Retrieval systems.

The algorithm was originally described in Porter, M.F., 1980, An algorithm for s
uffix stripping, Program, 14(3) :130-137. It has since been reprinted in Sparck 
Jones, Karen, and Peter Willet, 1997, Readings in Information Retrieval, San Fra
ncisco: Morgan Kaufmann, ISBN 1-55860-454-4.

The Algorithm has been widely used, quoted, and adapted over the past 20 years. 
Unfortunately variants of it abound which claim to be true implementations, and 
this can cause confusion. This page contains a demonstration of the stemmer, and
 downloadable versions of it in ANSI C, Java, Perl and other languages.

The original stemmer was coded up in BCPL, a language no longer in vogue. In its
 final surviving form, this BCPL version has three minor points of difference fr
om the published algorithm, and these are clearly marked in the downloadable ANS
I C version. They are discussed further below.

The ANSI C, Java and Perl versions are exactly equivalent to the original BCPL v
ersion, having been tested on a large corpus of English text. The original paper
 was, I hope, unambiguous, despite a couple of irritating typos, but even so the
 ANSI C version nowadays acts as a better definition of the algorithm than the o
riginal published paper.".

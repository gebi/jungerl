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
%%% File    : rdbms_wsearch.erl
%%% Author  : Hans Nilsson <hans.r.nilsson@ericsson.com>
%%%           Ulf Wiger <ulf.wiger@ericsson.com> (moved into rdbms)
%%% Description : Word search indexing support
%%%               Uses the Porter Stemming Algorithm
%%%
%%% Created : 26 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_wsearch).
-export([search/4,
	 word_report/2]).

-export([sort_search_result_TFIDF/3,
	 sort_search_result_most_occurences/3,
	 sort_search_result_weighted_most_occurences/3]).

-import(rdbms_wsearch_porter, [lower_case/1]).

search(Sentence, Tab, Index, SortFun) when is_function(SortFun, 2) ->
    Stems = lists:usort(rdbms_wsearch_idx:idx_sentence(Sentence)),
    SearchResult = do_search(Stems, Tab, Index, SortFun),
    AllMatchingStems = 
        lists:usort(
          lists:concat(
            lists:map(fun({_Weight,{_Key,MS}}) -> 
                              elements(1,MS)
                      end, SearchResult))),
    {Stems, AllMatchingStems, SearchResult}.


%% Stems must be usort:ed !!
do_search(Stems, Tab, Index, SortFun) ->
    Tab = ets:new(tmptab, [bag]),
    try begin 
	    lists:foreach(
	      fun(Stem) ->
		      lists:foreach(
			fun({Key,N}) when integer(Key) ->
				ets:insert(Tab,{Key,Stem,N});
			   (_) ->
				ok
			end, mnesia:index_read(Tab, Stem, Index))
	      end, Stems),
	    SortFun(read_values(Tab,ets:first(Tab),[]), Stems)
	end of
	R ->
	    lists:reverse( lists:keysort(1,R) )
    after
	ets:delete(Tab)
      end.

read_values(_Tab, '$end_of_table', Acc) -> Acc;
read_values(Tab, Key, Acc) -> 
    StemsN = lists:sort(
               lists:map(fun({_,Stem,N}) -> {Stem,N} end, 
                         ets:lookup(Tab,Key))),
    read_values(Tab, ets:next(Tab,Key), [{Key,StemsN}|Acc]).



%%%----------------
%% L = [  {Key, [{Stem,N}]} ]  (N = number of times the stem is in the item)
%% SearchedStems and Ws* must be usorted (or at least sorted in the same way)

%% Joachims, Thorsten: A Probabilistic Analysis of the Rocchio Algorithm
%%                     with TFIDF for Text Categorization
%% March, 1996  CMU-CS-96-118

sort_search_result_TFIDF(Tab, L, SearchedStems) ->
    Sz = mnesia:table_info(Tab, size),
    IDF_W = lists:map(fun(Stem) ->
                              {Stem, 'IDF'(Stem, Sz)}
                      end, SearchedStems),
    TF_WDprim = stem_ocurrences( lists:append(elements(2,L)) ),
    SP1 = scalar_prod(TF_WDprim, IDF_W),
    lists:map(fun(E={_,TF_WC}) ->
                      Weigth = 
                          ( SP1 * scalar_prod(TF_WC,IDF_W))
                          / math:sqrt( sum_prod2(TF_WC,IDF_W) ),
                      {Weigth,E} 
              end, L).

'IDF'(W, Sz) -> R = math:log( Sz / 'DF'(W) ),
            R*R.

'DF'(W) -> element(1, rdbms_wsearch_db:stems_doc(W)).
    

stem_ocurrences(L) -> so(lists:sort(L), []).

so([], Acc) -> lists:reverse(Acc);
so([{Stem,N}|L], [{Stem,Sum}|Acc]) -> so(L, [{Stem,Sum+N}|Acc]);
so([{Stem,N}|L], Acc) -> so(L, [{Stem,N}|Acc]).

%%% quad_sum(V) -> lists:foldl(fun({_,N},S) -> S + N*N end, 0, V).
                                   
sum_prod2(V1, V2) -> sum_prod2(V1, V2, 0).


sum_prod2([{S,N1}|V1], [{S,N2}|V2], Sum) -> sum_prod2(V1,V2, N1*N2*N2 + Sum);
sum_prod2(V1, [{_S,_N2}|V2], Sum) -> sum_prod2(V1,V2, Sum); 
sum_prod2(_, _, Sum) -> Sum.
                        

scalar_prod(V1,V2) -> scal_prod(V1, V2, 0).

scal_prod([{S,N1}|V1], [{S,N2}|V2], Sum) -> scal_prod(V1,V2, N1*N2 + Sum );
scal_prod(V1, [{_S,_N2}|V2], Sum) -> scal_prod(V1,V2, Sum); 
scal_prod(_, _, Sum) -> Sum.

%%%----------------
%% A very simple one.
sort_search_result_most_occurences(Tab, L, _SearchedStems) ->
    lists:map(fun(E={_K,Ws}) -> 
                      {length(Ws),E}
              end, L).

%%%----------------
%% A very simple one but weight after the importence in the whole db of the
%% words
sort_search_result_weighted_most_occurences(Tab, L, _SearchedStems) ->
    Sz = mnesia:table_info(Tab, size),
    lists:map(fun(E={_K,Ws}) -> 
                      Weight = 
                          lists:foldr(
                            fun({Stem,N}, S) -> 
                                    N*'IDF'(Stem, Sz) + S
                            end, 0, Ws),
                      {Weight,E}
              end, L).



%%%----
%% MS = [ {Stem,N} ]
word_report(String, MS) ->
    MatchingWords = lists:sort(matching_words(String, MS)),
    word_list_and(
      lists:map(fun({Stem,1}) -> Stem;
                   ({Stem,N}) -> [Stem,"(",integer_to_list(N),")"]
                end, count(MatchingWords))).

count(L) -> 
    lists:foldl(fun(W,[{W,N}|Acc]) -> [{W,N+1}|Acc];
                   (W,Acc) -> [{W,1}|Acc]
                end, [], lists:sort(L)).
    
matching_words(Text, MatchingStems) ->
    Words = rdbms_wsearch_idx:words(Text),
    Stems = lists:map(fun(W) -> 
                              lists:flatten(rdbms_wsearch_idx:idx_sentence(W))
                      end, Words),
    matching_words(Stems, Words, MatchingStems, []).

matching_words([Stem|Stems], [Word|Words], MatchingStems, Acc) ->
    case lists:member(Stem,MatchingStems) of
        true ->
	    matching_words(
	      Stems, Words, MatchingStems, [lower_case(Word)|Acc]);
        false ->
	    matching_words(Stems, Words, MatchingStems, Acc)
    end;

matching_words([], [], _, Acc) ->
    Acc.

elements(N, L) ->
    [element(N, T) || T <- L].


word_list_and(Ws) -> word_list(Ws,"and").

word_list(Ws, Last) -> insert_delims(Ws, ", ", [" ",Last," "]).

%%% insert_delims(Ws, Delim) -> insert_delims(Ws, Delim, Delim).

insert_delims(Ws, Delim, LastDelim) ->
    lists:foldr(fun(W,A=[_,_|_]) -> [[W,Delim]|A];
                   (W,A=[_|_])   -> [[W,LastDelim]|A];
                   (W,A)         -> [W|A] 
		end, [], Ws).


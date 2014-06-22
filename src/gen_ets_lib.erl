%%% The MIT License
%%%
%%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(gen_ets_lib).

-include("gen_ets.hrl").

%% External exports
-export([%% ets friends
         foldl/3
         , foldr/3
         %% , nfoldl/4
         %% , nfoldr/4
         %% , nfoldl/1
         %% , nfoldr/1
         , match/2
         , match/3
         , match/1
         , match_delete/2
         , match_object/2
         , match_object/3
         , match_object/1
         , select/2
         , select/3
         , select/1
         , select_count/2
         , select_delete/2
         , select_reverse/2
         , select_reverse/3
         , select_reverse/1
         , tab2list/1
        ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

foldl(Fun, Acc0, #gen_tid{mod=Mod}=Tid) ->
    foldl(Fun, Acc0, Tid, Mod:first_iter(Tid)).

foldr(Fun, Acc0, #gen_tid{mod=Mod}=Tid) ->
    foldr(Fun, Acc0, Tid, Mod:last_iter(Tid)).

nfoldl(Fun, Acc0, #gen_tid{mod=Mod}=Tid, Limit) when Limit > 0 ->
    nfoldl(Fun, Acc0, Acc0, Tid, Limit, Mod:first_iter(Tid, Limit));
nfoldl(_Fun, _Acc0, _Tid, Limit) ->
    exit({badarg,Limit}).

nfoldl('$end_of_table') ->
    '$end_of_table';
nfoldl({_Fun, _Acc0, _Tid, _Limit, '$end_of_table'}) ->
    '$end_of_table';
nfoldl({Fun, Acc0, #gen_tid{mod=Mod}=Tid, Limit, Key}) ->
    nfoldl(Fun, Acc0, Acc0, Tid, Limit, Mod:next_iter(Tid, Key, Limit)).

nfoldr(Fun, Acc0, #gen_tid{mod=Mod}=Tid, Limit) when Limit > 0 ->
    nfoldr(Fun, Acc0, Acc0, Tid, Limit, Mod:last_iter(Tid, Limit));
nfoldr(_Fun, _Acc0, _Tid, Limit) ->
    exit({badarg,Limit}).

nfoldr('$end_of_table') ->
    '$end_of_table';
nfoldr({_Fun, _Acc0, _Tid, _Limit, '$end_of_table'}) ->
    '$end_of_table';
nfoldr({Fun, Acc0, #gen_tid{mod=Mod}=Tid, Limit, Key}) ->
    nfoldr(Fun, Acc0, Acc0, Tid, Limit, Mod:prev_iter(Tid, Key, Limit)).

tab2list(Tid) ->
    foldr(fun(X, Acc) -> [X|Acc] end, [], Tid).

match(Tid, Pattern) ->
    select(Tid, [{Pattern, [], ['$$']}]).

match(Tid, Pattern, Limit) ->
    select(Tid, [{Pattern, [], ['$$']}], Limit).

match(Cont) ->
    select(Cont).

match_delete(Tid, Pattern) ->
    select_delete(Tid, [{Pattern, [], [true]}]),
    true.

match_object(Tid, Pattern) ->
    select(Tid, [{Pattern, [], ['$_']}]).

match_object(Tid, Pattern, Limit) ->
    select(Tid, [{Pattern, [], ['$_']}], Limit).

match_object(Cont) ->
    select(Cont).

select(Tid, Spec) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    selectr(Fun, [], Tid, Spec).

select(Tid, Spec, Limit) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    case nselectl(Fun, [], Tid, Spec, Limit) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select(Cont0) ->
    case nselectl(Cont0) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select_count(Tid, Spec) ->
    Fun = fun(_Object, true, Acc) ->
                  Acc + 1;
             (_Object, _Match, Acc) ->
                  Acc
          end,
    selectl(Fun, 0, Tid, Spec).

select_delete(#gen_tid{keypos=KeyPos, mod=Mod}=Tid, Spec) ->
    Fun = fun(Object, true, Acc) ->
                  Key = element(KeyPos, Object),
                  Mod:delete(Tid, Key),
                  Acc + 1;
             (_Object, _Match, Acc) ->
                  Acc
          end,
    selectl(Fun, 0, Tid, Spec).

select_reverse(Tid, Spec) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    selectl(Fun, [], Tid, Spec).

select_reverse(Tid, Spec, Limit) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    case nselectr(Fun, [], Tid, Spec, Limit) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select_reverse(Cont0) ->
    case nselectr(Cont0) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

foldl(_Fun, Acc, Tid, '$end_of_table') ->
    ets_safe_fixtable(Tid, false),
    Acc;
foldl(Fun, Acc, #gen_tid{keypos=KeyPos, mod=Mod}=Tid, Object) ->
    Key = element(KeyPos, Object),
    foldl(Fun, Fun(Object, Acc), Tid, Mod:next_iter(Tid, Key)).

foldr(_Fun, Acc, Tid, '$end_of_table') ->
    ets_safe_fixtable(Tid, false),
    Acc;
foldr(Fun, Acc, #gen_tid{keypos=KeyPos, mod=Mod}=Tid, Object) ->
    Key = element(KeyPos, Object),
    foldr(Fun, Fun(Object, Acc), Tid, Mod:prev_iter(Tid, Key)).

nfoldl(_Fun, Acc0, Acc0, Tid, _Limit, '$end_of_table') ->
    ets_safe_fixtable(Tid, false),
    '$end_of_table';
nfoldl(_Fun, _Acc0, Acc, Tid, _Limit, '$end_of_table'=Cont) ->
    ets_safe_fixtable(Tid, false),
    {Acc, Cont};
nfoldl(Fun, Acc0, Acc, #gen_tid{keypos=KeyPos, mod=Mod}=Tid, Limit, Objects) ->
    Fun1 = fun(Object, {N, _, Acc1}) ->
                   Key = element(KeyPos, Object),
                   case Fun(Object, Acc1) of
                       {true, NewAcc} ->
                           {N+1, Key, NewAcc};
                       {false, NewAcc} ->
                           {N, Key, NewAcc}
                   end
           end,
    case lists:foldl(Fun1, {0, undefined, Acc}, Objects) of
        {0, Key, Acc2} ->
            nfoldl(Fun, Acc0, Acc2, Tid, Limit, Mod:next_iter(Tid, Key, Limit));
        {_, Key, Acc2} ->
            Cont = {Fun, Acc0, Tid, Limit, Key},
            {Acc2, Cont}
    end.

nfoldr(_Fun, Acc0, Acc0, Tid, _Limit, '$end_of_table') ->
    ets_safe_fixtable(Tid, false),
    '$end_of_table';
nfoldr(_Fun, _Acc0, Acc, Tid, _Limit, '$end_of_table'=Cont) ->
    ets_safe_fixtable(Tid, false),
    {Acc, Cont};
nfoldr(Fun, Acc0, Acc, #gen_tid{keypos=KeyPos, mod=Mod}=Tid, Limit, Objects) ->
    Fun1 = fun(Object, {N, _, Acc1}) ->
                   Key = element(KeyPos, Object),
                   case Fun(Object, Acc1) of
                       {true, NewAcc} ->
                           {N+1, Key, NewAcc};
                       {false, NewAcc} ->
                           {N, Key, NewAcc}
                   end
           end,
    case lists:foldl(Fun1, {0, undefined, Acc}, Objects) of
        {0, Key, Acc2} ->
            nfoldr(Fun, Acc0, Acc2, Tid, Limit, Mod:prev_iter(Tid, Key, Limit));
        {_, Key, Acc2} ->
            Cont = {Fun, Acc0, Tid, Limit, Key},
            {Acc2, Cont}
    end.

selectl(Fun, Acc0, Tid, Spec) ->
    ets_safe_fixtable(Tid, true),
    foldl(selectfun(Fun, Spec), Acc0, Tid).

selectr(Fun, Acc0, Tid, Spec) ->
    ets_safe_fixtable(Tid, true),
    foldr(selectfun(Fun, Spec), Acc0, Tid).

nselectl(Fun, Acc0, Tid, Spec, Limit) ->
    ets_safe_fixtable(Tid, true),
    nfoldl(nselectfun(Fun, Spec), Acc0, Tid, Limit).

nselectr(Fun, Acc0, Tid, Spec, Limit) ->
    ets_safe_fixtable(Tid, true),
    nfoldr(nselectfun(Fun, Spec), Acc0, Tid, Limit).

nselectl(Cont) ->
    nfoldl(Cont).

nselectr(Cont) ->
    nfoldr(Cont).

selectfun(Fun, Spec) ->
    CMSpec = ets:match_spec_compile(Spec),
    fun(Object, Acc) ->
            case ets:match_spec_run([Object], CMSpec) of
                [] ->
                    Acc;
                [Match] ->
                    Fun(Object, Match, Acc)
            end
    end.

nselectfun(Fun, Spec) ->
    CMSpec = ets:match_spec_compile(Spec),
    fun(Object, Acc) ->
            case ets:match_spec_run([Object], CMSpec) of
                [] ->
                    {false, Acc};
                [Match] ->
                    {true, Fun(Object, Match, Acc)}
            end
    end.

ets_safe_fixtable(#gen_tid{type=set, mod=gen_ets_impl_ets, impl=Impl}, Flag) ->
    ets:safe_fixtable(Impl, Flag);
ets_safe_fixtable(_Tid, _Flag) ->
    true.

%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(gen_ets_impl_ets).

-include("gen_ets.hrl").

%% External exports
-export([open/2
         , destroy/2
         , repair/2
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         , foldl/3
         , foldr/3
         , info_memory/1
         , info_size/1
         , insert/2
         , insert_new/2
         , last/1
         , lookup/2
         , lookup_element/3
         , match/2
         , match/3
         , match/1
         , match_delete/2
         , match_object/2
         , match_object/3
         , match_object/1
         , member/2
         , next/2
         , prev/2
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

open(#gen_tid{name=Name, type=Type, keypos=KeyPos, protection=Protection, compressed=Compressed}, Opts) ->
    EffOpts =
        [Type, {keypos,KeyPos}, Protection] ++
        [compressed || Compressed ] ++
        proplists:delete(named_table, Opts),
    ets:new(Name, EffOpts).

destroy(#gen_tid{}, _Opts) ->
    true.

repair(#gen_tid{}, _Opts) ->
    true.

delete(#gen_tid{impl=Impl}) ->
    ets:delete(Impl).

delete(#gen_tid{impl=Impl}, Key) ->
    ets:delete(Impl, Key).

delete_all_objects(#gen_tid{impl=Impl}) ->
    ets:delete_all_objects(Impl).

first(#gen_tid{impl=Impl}) ->
    ets:first(Impl).

foldl(Function, Acc0, #gen_tid{impl=Impl}) ->
    ets:foldl(Function, Acc0, Impl).

foldr(Function, Acc0, #gen_tid{impl=Impl}) ->
    ets:foldr(Function, Acc0, Impl).

info_memory(#gen_tid{impl=Impl}) ->
    ets:info(Impl, memory).

info_size(#gen_tid{impl=Impl}) ->
    ets:info(Impl, size).

insert(#gen_tid{impl=Impl}, ObjOrObjs) ->
    ets:insert(Impl, ObjOrObjs).

insert_new(#gen_tid{impl=Impl}, ObjOrObjs) ->
    ets:insert_new(Impl, ObjOrObjs).

last(#gen_tid{impl=Impl}) ->
    ets:last(Impl).

lookup(#gen_tid{impl=Impl}, Key) ->
    ets:lookup(Impl, Key).

lookup_element(#gen_tid{impl=Impl}, Key, Pos) ->
    ets:lookup_element(Impl, Key, Pos).

match(#gen_tid{impl=Impl}, Pattern) ->
    ets:match(Impl, Pattern).

match(#gen_tid{impl=Impl}, Pattern, Limit) ->
    ets:match(Impl, Pattern, Limit).

match(Cont) ->
    ets:match(Cont).

match_delete(#gen_tid{impl=Impl}, Pattern) ->
    ets:match_delete(Impl, Pattern).

match_object(#gen_tid{impl=Impl}, Pattern) ->
    ets:match_object(Impl, Pattern).

match_object(#gen_tid{impl=Impl}, Pattern, Limit) ->
    ets:match_object(Impl, Pattern, Limit).

match_object(Cont) ->
    ets:match_object(Cont).

member(#gen_tid{impl=Impl}, Key) ->
    ets:member(Impl, Key).

next(#gen_tid{impl=Impl}, Key) ->
    ets:next(Impl, Key).

prev(#gen_tid{impl=Impl}, Key) ->
    ets:prev(Impl, Key).

select(#gen_tid{impl=Impl}, Spec) ->
    ets:select(Impl, Spec).

select(#gen_tid{impl=Impl}, Spec, Limit) ->
    ets:select(Impl, Spec, Limit).

select(Cont) ->
    ets:select(Cont).

select_count(#gen_tid{impl=Impl}, Spec) ->
    ets:select_count(Impl, Spec).

select_delete(#gen_tid{impl=Impl}, Spec) ->
    ets:select_delete(Impl, Spec).

select_reverse(#gen_tid{impl=Impl}, Spec) ->
    ets:select_reverse(Impl, Spec).

select_reverse(#gen_tid{impl=Impl}, Spec, Limit) ->
    ets:select_reverse(Impl, Spec, Limit).

select_reverse(Cont) ->
    ets:select_reverse(Cont).

tab2list(#gen_tid{impl=Impl}) ->
    ets:tab2list(Impl).

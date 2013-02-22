%%% The MIT License
%%%
%%% Copyright (C) 2011-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(qc_gen_ets_raw).

-include("gen_ets.hrl").

%% API
-export([%% test
         teardown/1
         , is_table/1
         %% gen_ets
         , all/1
         , new/2
         , new/3
         , destroy/3
         , repair/3
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         , foldl/3
         , foldr/3
         , info/1
         , info/2
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


%%%===================================================================
%%% API
%%%===================================================================

teardown(Name) ->
    %% @TODO make this more robust
    _ = [ true=gen_ets:delete(Tab) || Tab <- gen_ets:all() ],
    catch exit(whereis(Name), kill),
    _ = os:cmd("find . -name '" ++ ?MODULE_STRING ++ ".*' -exec rm -rf {} \;"),
    _ = os:cmd("rm -rf " ++ ?MODULE_STRING).

is_table(Res) ->
    is_record(Res, gen_tid).

all(_Tab) ->
    catch gen_ets:all().

new(Name, Options) ->
    ok = filelib:ensure_dir(?MODULE_STRING),
    catch gen_ets:new(Name, filter_options(Options)).

new(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    new(Name, Options).

destroy(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    catch gen_ets:destroy(Name, filter_options(Options)).

repair(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    catch gen_ets:repair(Name, filter_options(Options)).

delete(Tab) ->
    catch gen_ets:delete(Tab).

delete(Tab, Key) ->
    catch gen_ets:delete(Tab, Key).

delete_all_objects(Tab) ->
    catch gen_ets:delete_all_objects(Tab).

first(Tab) ->
    catch gen_ets:first(Tab).

foldl(Function, Acc0, Tab) ->
    catch gen_ets:foldl(Function, Acc0, Tab).

foldr(Function, Acc0, Tab) ->
    catch gen_ets:foldr(Function, Acc0, Tab).

info(Tab) ->
    catch gen_ets:info(Tab).

info(Tab, Item) ->
    catch gen_ets:info(Tab, Item).

insert(Tab, ObjOrObjs) ->
    catch gen_ets:insert(Tab, ObjOrObjs).

insert_new(Tab, ObjOrObjs) ->
    catch gen_ets:insert_new(Tab, ObjOrObjs).

last(Tab) ->
    catch gen_ets:last(Tab).

lookup(Tab, Key) ->
    catch gen_ets:lookup(Tab, Key).

lookup_element(Tab, Key, Pos) ->
    catch gen_ets:lookup_element(Tab, Key, Pos).

match(Tab, Pattern) ->
    catch gen_ets:match(Tab, Pattern).

match(Tab, Pattern, Limit) ->
    catch gen_ets:match(Tab, Pattern, Limit).

match(Cont) ->
    catch gen_ets:match(Cont).

match_delete(Tab, Pattern) ->
    catch gen_ets:match_delete(Tab, Pattern).

match_object(Tab, Pattern) ->
    catch gen_ets:match_object(Tab, Pattern).

match_object(Tab, Pattern, Limit) ->
    catch gen_ets:match_object(Tab, Pattern, Limit).

match_object(Cont) ->
    catch gen_ets:match_object(Cont).

member(Tab, Key) ->
    catch gen_ets:member(Tab, Key).

next(Tab, Key) ->
    catch gen_ets:next(Tab, Key).

prev(Tab, Key) ->
    catch gen_ets:prev(Tab, Key).

select(Tab, Spec) ->
    catch gen_ets:select(Tab, Spec).

select(Tab, Spec, Limit) ->
    catch gen_ets:select(Tab, Spec, Limit).

select(Cont) ->
    catch gen_ets:select(Cont).

select_count(Tab, Spec) ->
    catch gen_ets:select_count(Tab, Spec).

select_delete(Tab, Spec) ->
    catch gen_ets:select_delete(Tab, Spec).

select_reverse(Tab, Spec) ->
    catch gen_ets:select_reverse(Tab, Spec).

select_reverse(Tab, Spec, Limit) ->
    catch gen_ets:select_reverse(Tab, Spec, Limit).

select_reverse(Cont) ->
    catch gen_ets:select_reverse(Cont).

tab2list(Tab) ->
    catch gen_ets:tab2list(Tab).


%%%===================================================================
%%% Internal
%%%===================================================================

filter_options(Options) ->
    Options.

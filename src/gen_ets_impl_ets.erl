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

-module(gen_ets_impl_ets).
-behaviour(gen_ets_ns).

-include("gen_ets.hrl").

%% External exports
-export([open/2
         , destroy/2
         , repair/2
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         , first_iter/1
         , info_memory/1
         , info_size/1
         , insert/2
         , insert_new/2
         , last/1
         , last_iter/1
         , lookup/2
         , lookup_element/3
         , member/2
         , next/2
         , next_iter/2
         , prev/2
         , prev_iter/2
         , notify/4
         , first/2
         , first_iter/2
         , last/2
         , last_iter/2
         , next/3
         , next_iter/3
         , prev/3
         , prev_iter/3
        ]).


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

open(#gen_tid{name=Name, type=Type, keypos=KeyPos, protection=Protection, compressed=Compressed}=Tid, Opts) ->
    EffOpts =
        [Type, {keypos,KeyPos}, public] ++
        [ compressed || Compressed ] ++
        proplists:delete(Protection, proplists:delete(named_table, Opts)),
    Tid#gen_tid{impl=ets:new(Name, EffOpts)}.

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

first_iter(#gen_tid{impl=Impl}) ->
    case ets:first(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            safe_lookup(Impl, Key)
    end.

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

last_iter(#gen_tid{impl=Impl}) ->
    case ets:last(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            safe_lookup(Impl, Key)
    end.

lookup(#gen_tid{impl=Impl}, Key) ->
    ets:lookup(Impl, Key).

lookup_element(#gen_tid{impl=Impl}, Key, Pos) ->
    ets:lookup_element(Impl, Key, Pos).

member(#gen_tid{impl=Impl}, Key) ->
    ets:member(Impl, Key).

next(#gen_tid{impl=Impl}, Key) ->
    ets:next(Impl, Key).

next_iter(#gen_tid{impl=Impl}, Key) ->
    case safe_next(Impl, Key) of
        '$end_of_table' ->
            '$end_of_table';
        NextKey ->
            safe_lookup(Impl, NextKey)
    end.

prev(#gen_tid{impl=Impl}, Key) ->
    ets:prev(Impl, Key).

prev_iter(#gen_tid{impl=Impl}, Key) ->
    case safe_prev(Impl, Key) of
        '$end_of_table' ->
            '$end_of_table';
        PrevKey ->
            safe_lookup(Impl, PrevKey)
    end.

notify(#gen_tid{impl=_Impl}, _Event, _Pid, _Msg) ->
    false.

first(Tid, N) when N >= 0 ->
    case first(Tid) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            get_next(Tid, Key, N)
    end;

first(Tid, N) ->
    erlang:error(badarg, [Tid, N]).

first_iter(Tid, N) when N >= 0 ->
    case first(Tid) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            get_next_iter(Tid, Key, N)
    end;
first_iter(Tid, N) ->
    erlang:error(badarg, [Tid, N]).


last(Tid, N) when N >= 0 ->
    case last(Tid) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            get_prev(Tid, Key, N)
    end;

last(Tid, N) ->
    erlang:error(badarg, [Tid, N]).

last_iter(Tid, N) when N >= 0 ->
    case last(Tid) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            get_prev_iter(Tid, Key, N)
    end;
last_iter(Tid, N) ->
    erlang:error(badarg, [Tid, N]).

next(Tid, Key, N) when N >= 0 ->
    case next(Tid, Key) of
        '$end_of_table' ->
            '$end_of_table';
        NextKey ->
            get_next(Tid, NextKey, N)
    end;

next(Tid, Key, N) ->
    erlang:error(badarg, [Tid, Key, N]).

next_iter(Tid, Key, N) when N >= 0 ->
    case next(Tid, Key) of
        '$end_of_table' ->
            '$end_of_table';
        NextKey ->
            get_next_iter(Tid, NextKey, N)
    end;
next_iter(Tid, Key, N) ->
    erlang:error(badarg, [Tid, Key, N]).

prev(Tid, Key, N) when N >= 0 ->
    case prev(Tid, Key) of
        '$end_of_table' ->
            '$end_of_table';
        PrevKey ->
            get_prev(Tid, PrevKey, N)
    end;

prev(Tid, Key, N) ->
    erlang:error(badarg, [Tid, Key, N]).

prev_iter(Tid, Key, N) when N >= 0 ->
    case prev(Tid, Key) of
        '$end_of_table' ->
            '$end_of_table';
        PrevKey ->
            get_prev_iter(Tid, PrevKey, N)
    end;
prev_iter(Tid, Key, N) ->
    erlang:error(badarg, [Tid, Key, N]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

safe_next(Impl, Key) ->
    try
        ets:next(Impl, Key)
    catch
        error:badarg ->
            '$end_of_table'
    end.

safe_prev(Impl, Key) ->
    try
        ets:prev(Impl, Key)
    catch
        error:badarg ->
            '$end_of_table'
    end.

safe_lookup(Impl, Key) ->
    case ets:lookup(Impl, Key) of
        [] ->
            '$end_of_table';
        [Object] ->
            Object
    end.

get_next(#gen_tid{impl=Impl}, Key, N) ->
    get_next(Impl, Key, N, []).

get_next(_Impl, Key, N, Acc) when Key=='$end_of_table'; N==0 ->
    lists:reverse(Acc);
get_next(Impl, Key, N, Acc) ->
    get_next(Impl, safe_next(Impl, Key), N-1, [Key|Acc]).

get_next_iter(#gen_tid{impl=Impl}, Key, N) ->
    get_next_iter(Impl, Key, N, []).

get_next_iter(_Impl, Key, N, Acc) when Key=='$end_of_table'; N==0 ->
    lists:reverse(Acc);
get_next_iter(Impl, Key, N, Acc) ->
    case safe_lookup(Impl, Key) of
        '$end_of_table' ->
            lists:reverse(Acc);
        Object ->
            get_next_iter(Impl, safe_next(Impl, Key), N-1, [Object|Acc])
    end.

get_prev(#gen_tid{impl=Impl}, Key, N) ->
    get_prev(Impl, Key, N, []).

get_prev(_Impl, Key, N, Acc) when Key=='$end_of_table'; N==0 ->
    lists:reverse(Acc);
get_prev(Impl, Key, N, Acc) ->
    get_prev(Impl, safe_prev(Impl, Key), N-1, [Key|Acc]).

get_prev_iter(#gen_tid{impl=Impl}, Key, N) ->
    get_prev_iter(Impl, Key, N, []).

get_prev_iter(_Impl, Key, N, Acc) when Key=='$end_of_table'; N==0 ->
    lists:reverse(Acc);
get_prev_iter(Impl, Key, N, Acc) ->
    case safe_lookup(Impl, Key) of
        '$end_of_table' ->
            lists:reverse(Acc);
        Object ->
            get_prev_iter(Impl, safe_prev(Impl, Key), N-1, [Object|Acc])
    end.

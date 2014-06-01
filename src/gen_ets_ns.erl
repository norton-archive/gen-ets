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

-module(gen_ets_ns).

-include("gen_ets.hrl").

%% External exports
-export([all/1
         , tid/2
         , tid/3
         , new/3
         , destroy/3
         , repair/3
         , delete/2
         , delete/3
         , delete_all_objects/2
         , first/2
         , foldl/4
         , foldr/4
         , info/2
         , info/3
         , insert/3
         , insert_new/3
         , last/2
         , lookup/3
         , lookup_element/4
         , match/3
         , match/4
         , match/2
         , match_delete/3
         , match_object/3
         , match_object/4
         , match_object/2
         , member/3
         , next/3
         , prev/3
         , select/3
         , select/4
         , select/2
         , select_count/3
         , select_delete/3
         , select_reverse/3
         , select_reverse/4
         , select_reverse/2
         , tab2list/2
        ]).

%% DEBUG -compile(export_all).

-export_type([gen_tab/0, gen_tid/0, gen_ns/0]).
-export_type([name/0, key/0, object/0]).
-export_type([match_pattern/0, match_spec/0]).
-export_type([cont/0]).

%% Interface Functions
-ifndef(old_callbacks).

-callback open(#gen_tid{}, impl_opts()) -> impl().
-callback destroy(#gen_tid{}, impl_opts()) -> true.
-callback repair(#gen_tid{}, impl_opts()) -> true.
-callback delete(#gen_tid{}) -> true.
-callback delete(#gen_tid{}, key()) -> true.
-callback delete_all_objects(#gen_tid{}) -> true.
-callback first(#gen_tid{}) -> key() | '$end_of_table'.
-callback first_iter(#gen_tid{}) -> object() | '$end_of_table'.
-callback info_memory(#gen_tid{}) -> non_neg_integer().
-callback info_size(#gen_tid{}) -> non_neg_integer().
-callback insert(#gen_tid{}, object() | [object()]) -> true.
-callback insert_new(#gen_tid{}, object() | [object()]) -> true.
-callback last(#gen_tid{}) -> key() | '$end_of_table'.
-callback last_iter(#gen_tid{}) -> object() | '$end_of_table'.
-callback lookup(#gen_tid{}, key()) -> [object()].
-callback lookup_element(#gen_tid{}, key(), pos()) -> term().
-callback member(#gen_tid{}, key()) -> true | false.
-callback next(#gen_tid{}, key()) -> key() | '$end_of_table'.
-callback next_iter(#gen_tid{}, key()) -> object() | '$end_of_table'.
-callback prev(#gen_tid{}, key()) -> key() | '$end_of_table'.
-callback prev_iter(#gen_tid{}, key()) -> object() | '$end_of_table'.
-callback notify(#gen_tid{}, when_destroyed, pid(), term()) -> true.

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{open,2}
     , {destroy,2}
     , {repair,2}
     , {delete,1}
     , {delete,2}
     , {delete_all_objects,1}
     , {first,1}
     , {first_iter,1}
     , {info_memory,1}
     , {info_size,1}
     , {insert,2}
     , {insert_new,2}
     , {last,1}
     , {last_iter,1}
     , {lookup,2}
     , {lookup_element,3}
     , {member,2}
     , {next,2}
     , {next_iter,2}
     , {prev,2}
     , {prev_iter,2}
     , {notify,4}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).

%%
%% ETS exports
%%
%% -export([all/0
%%          , delete/1              %% mnesia
%%          , delete/2              %% mnesia
%%          , delete_all_objects/1
%%          , delete_object/2
%%          , file2tab/1
%%          , file2tab/2
%%          , filter/3              %% mnesia
%%          , first/1               %% mnesia
%%          , foldl/3               %% mnesia
%%          , foldr/3
%%          , from_dets/2
%%          , fun2ms/1
%%          , give_away/3
%%          , i/0
%%          , i/1
%%          , info/1
%%          , info/2                %% mnesia
%%          , init_table/2          %% mnesia
%%          , insert/2              %% mnesia
%%          , insert_new/2
%%          , is_compiled_ms/1
%%          , last/1                %% mnesia
%%          , lookup/2              %% mnesia
%%          , lookup_element/3      %% mnesia
%%          , match/1
%%          , match/2               %% mnesia
%%          , match/3
%%          , match_delete/2        %% mnesia
%%          , match_object/1
%%          , match_object/2        %% mnesia
%%          , match_object/3
%%          , match_spec_compile/1
%%          , match_spec_run/2      %% mnesia
%%          , member/2
%%          , new/2                 %% mnesia
%%          , next/2                %% mnesia
%%          , prev/2                %% mnesia
%%          , rename/2
%%          , repair_continuation/2 %% mnesia
%%          , safe_fixtable/2
%%          , select/1
%%          , select/2
%%          , select/3
%%          , select_count/2
%%          , select_delete/2
%%          , select_reverse/1
%%          , select_reverse/2
%%          , select_reverse/3
%%          , setopts/2
%%          , slot/2                %% mnesia
%%          , tab2file/2
%%          , tab2file/3
%%          , tab2list/1            %% mnesia
%%          , tabfile_info/1
%%          , table/1
%%          , table/2
%%          , test_ms/2
%%          , to_dets/2
%%          , update_counter/3      %% mnesia
%%          , update_element/3
%%         ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type gen_ns()        :: atom().
-opaque gen_tid()     :: #gen_tid{}.
-type gen_tab()       :: name() | gen_tid().

-type opts()          :: [opt() | {impl, {module(), impl_opts()}}].
-type opt()           :: set | ordered_set | named_table | {keypos,pos_integer()} | public | protected | private | compressed | async.
-type impl_opts()     :: [impl_opt()].
-type impl_opt()      :: term().
-type impl()          :: term().

-type key()           :: term().
-type object()        :: tuple().

-type name()          :: term().
-type item()          :: owner | name | named_table | type | keypos | protection | compressed | async | memory | size.
-type pos()           :: pos_integer().
-type match_pattern() :: atom() | tuple(). % ets:match_pattern() is not exported!
-type match_spec()    :: ets:match_spec().
-type match()         :: term().
-type limit()         :: pos_integer().
-opaque cont()        :: {cont, #gen_tid{}, term()}.

-define(DEFAULT_MOD,  gen_ets_impl_ets).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Returns a list of all tables at the node.
%% @end
%% @see ets:all/0

-spec all(gen_ns()) -> [gen_tab()].
all(NS) ->
    gen_ets_reg:list(NS).

%% @doc Returns a table\'s identifier.
%% @end

-spec tid(gen_ns(), gen_tab()) -> gen_tid().
tid(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            Tid
    end.

%% @doc Returns a copy of a table\'s identifier with the given
%% implementation options.
%% @end

-spec tid(gen_ns(), gen_tab(), term()) -> gen_tid().
tid(NS, Tab, Opts) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            Tid#gen_tid{impl_opts=Opts}
    end.

%% @doc Creates a new table and returns a table identifier which can
%% be used in subsequent operations.  The table identifier can be sent
%% to other processes so that a table can be shared between different
%% processes within a node.
%%
%% Valid GEN_ETS properties for +Options+ are:
%%
%% - +set+ The table is a set table - one key, one object, no order
%%   among objects. This is the default table type.
%%
%% - +ordered_set+ The table is an ordered_set table - one key, one
%%   object, ordered in Erlang term order, which is the order implied
%%   by the +<+ and +>+ operators.
%%
%% - +named_table+ If this option is present, the name +Name+ is
%%   associated with the table identifier.
%%
%% - +{keypos,pos_integer()}+ Specfies which element in the stored
%%   tuples should be used as key. By default, it is the first
%%   element, i.e. +Pos=1+.
%%
%% - +public+ Any process may read or write to the table.
%%
%% - +protected+ The owner process can read and write to the table.
%%   Other processes can only read the table. This is the default
%%   setting for the access rights.
%%
%% - +private+ Only the owner process can read or write to the table.
%%
%% - +compressed+ If this option is present, the table data will be
%%   stored in a compressed format.
%%
%% - +async+ If this option is present and supported by the
%%   implementation, the emulator\'s async thread pool will be used
%%   when accessing the table data.
%%
%% - +{impl, module(), impl_opts()}+ The module that implements
%%   GEN_ETS callback functions.  Implementation specific options can be
%%   given. The default is +{impl, gen_ets_impl_ets, []}+.
%%
%% @end
%% @see ets:new/2

-spec new(gen_ns(), name(), opts()) -> gen_tab().
new(NS, Name, Opts) ->
    create(NS, open, Name, Opts).

%% @doc Destroy the contents of the specified table.
%% @end

-spec destroy(gen_ns(), name(), opts()) -> true.
destroy(NS, Name, Opts) ->
    create(NS, destroy, Name, Opts).

%% @doc If a table cannot be opened, you may attempt to call this
%% method to resurrect as much of the contents of the table as
%% possible.  Some data may be lost, so be careful when calling this
%% function on a table that contains important information.
%% @end

-spec repair(gen_ns(), name(), opts()) -> true.
repair(NS, Name, Opts) ->
    create(NS, repair, Name, Opts).

%% @doc Deletes the entire table +Tab+.
%% @end
%% @see ets:delete/1

-spec delete(gen_ns(), gen_tab()) -> true.
delete(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_reg:delete(Tid)
    end.

%% @doc Deletes all objects with the key +Key+ from the table +Tab+.
%% @end
%% @see ets:delete/2

-spec delete(gen_ns(), gen_tab(), key()) -> true.
delete(NS, Tab, Key) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:delete(Tid, Key)
    end.

%% @doc Delete all objects in the table +Tab+. The operation is
%% guaranteed to be atomic and isolated.  This function only applies
%% to the +ets+ implementation.
%% @end
%% @see ets:delete_all_objects/1

-spec delete_all_objects(gen_ns(), gen_tab()) -> true.
delete_all_objects(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:delete_all_objects(Tid)
    end.

%% @doc Returns the first key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ will be returned.
%% @end
%% @see ets:first/1

-spec first(gen_ns(), gen_tab()) -> key() | '$end_of_table'.
first(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:first(Tid)
    end.

%% @doc Fold from left to right over the elements of the table.
%% @end
%% @see ets:foldl/3

-spec foldl(gen_ns(), Fun, Acc0::term(), gen_tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldl(NS, Function, Acc0, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:foldl(Function, Acc0, Tid)
    end.

%% @doc Fold from right to left over the elements of the table.
%% @end
%% @see ets:foldr/3

-spec foldr(gen_ns(), Fun, Acc0::term(), gen_tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldr(NS, Function, Acc0, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:foldr(Function, Acc0, Tid)
    end.

%% @doc Returns information about the table +Tab+ as a list of +{Item,
%% Value}+ tuples.
%%
%% @end
%% @see info/2

-spec info(gen_ns(), gen_tab()) -> [{item(), term()}].
info(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            [{owner, Tid#gen_tid.owner},
             {name, Tid#gen_tid.name},
             {named_table, Tid#gen_tid.named_table},
             {type, Tid#gen_tid.type},
             {keypos, Tid#gen_tid.keypos},
             {protection, Tid#gen_tid.protection},
             {compressed, Tid#gen_tid.compressed},
             {async, Tid#gen_tid.async},
             {memory, Mod:info_memory(Tid)},
             {size, Mod:info_size(Tid)}]
    end.

%% @doc Returns the information associated with +Item+ for the table +Tab+.
%%
%% Valid +Item+ options are:
%%
%% - +owner+
%% - +name+
%% - +named_table+ _only the ets implementation_
%% - +type+
%% - +keypos+
%% - +protection+
%% - +compressed+
%% - +async+ _only the drv implementation_
%% - +memory+ _only the ets implementation_
%% - +size+ _only the ets implementation_
%%
%% @end
%% @see ets:info/2

-spec info(gen_ns(), gen_tab(), item()) -> term().
info(NS, Tab, Item) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            case Item of
                owner ->
                    Tid#gen_tid.owner;
                name ->
                    Tid#gen_tid.name;
                named_table ->
                    Tid#gen_tid.named_table;
                type ->
                    Tid#gen_tid.type;
                keypos ->
                    Tid#gen_tid.keypos;
                protection ->
                    Tid#gen_tid.protection;
                compressed ->
                    Tid#gen_tid.compressed;
                async ->
                    Tid#gen_tid.async;
                memory ->
                    Mod:info_memory(Tid);
                size ->
                    Mod:info_size(Tid);
                _ ->
                    erlang:error(badarg, [NS, Tab, Item])
            end
    end.

%% @doc Inserts the object or all of the objects in the list
%% +ObjOrObjs+ into the table +Tab+.
%% @end
%% @see ets:insert/2

-spec insert(gen_ns(), gen_tab(), object() | [object()]) -> true.
insert(NS, Tab, ObjOrObjs) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:insert(Tid, ObjOrObjs)
    end.

%% @doc This function works exactly like +insert/2+, with the
%% exception that instead of overwriting objects with the same key, it
%% simply returns false.  This function only applies to the +ets+
%% implementation.
%% @end
%% @see ets:insert_new/2

-spec insert_new(gen_ns(), gen_tab(), object() | [object()]) -> true.
insert_new(NS, Tab, ObjOrObjs) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:insert_new(Tid, ObjOrObjs)
    end.

%% @doc Returns the last key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ will be returned.
%% @end
%% @see ets:last/1

-spec last(gen_ns(), gen_tab()) -> key() | '$end_of_table'.
last(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:last(Tid)
    end.

%% @doc Returns a list of all objects with the key +Key+ in the table
%% +Tab+.
%% @end
%% @see ets:lookup/2

-spec lookup(gen_ns(), gen_tab(), key()) -> [object()].
lookup(NS, Tab, Key) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:lookup(Tid, Key)
    end.

%% @doc Returns the +Pos+:th element of the object with the key +Key+
%% in the table +Tab+.
%% @end
%% @see ets:lookup_element/3

-spec lookup_element(gen_ns(), gen_tab(), key(), pos()) -> term().
lookup_element(NS, Tab, Key, Pos) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:lookup_element(Tid, Key, Pos)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match/2

-spec match(gen_ns(), gen_tab(), match_pattern()) -> [match()].
match(NS, Tab, Pattern) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:match(Tid, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match/3

-spec match(gen_ns(), gen_tab(), match_pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match(NS, Tab, Pattern, Limit) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:match(Tid, Pattern, Limit))
    end.

%% @doc Continues a match started with +match/3+.
%% @end
%% @see ets:match/1

-spec match(gen_ns(), cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match(NS, {cont, Tab, Cont}) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:match(Cont))
    end;
match(_NS, '$end_of_table') ->
    '$end_of_table'.

%% @doc Deletes all objects which match the pattern +Pattern+ from the
%% table +Tab+.
%% @end
%% @see ets:match_delete/2

-spec match_delete(gen_ns(), gen_tab(), match_pattern()) -> true.
match_delete(NS, Tab, Pattern) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:match_delete(Tid, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match_object/2

-spec match_object(gen_ns(), gen_tab(), match_pattern()) -> [match()].
match_object(NS, Tab, Pattern) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:match_object(Tid, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match_object/3

-spec match_object(gen_ns(), gen_tab(), match_pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object(NS, Tab, Pattern, Limit) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:match_object(Tid, Pattern, Limit))
    end.

%% @doc Continues a match started with +match_object/3+.
%% @end
%% @see ets:match_object/1

-spec match_object(gen_ns(), cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object(NS, {cont, Tab, Cont}) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:match_object(Cont))
    end;
match_object(_NS, '$end_of_table') ->
    '$end_of_table'.

%% @doc Returns +true+ if one or more elements in the table +Tab+ has
%% the key +Key+, +false+ otherwise.
%% @end
%% @see ets:member/2

-spec member(gen_ns(), gen_tab(), key()) -> true | false.
member(NS, Tab, Key) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:member(Tid, Key)
    end.

%% @doc Returns the next key +Key2+, following the key +Key1+ in the
%% table +Tab+.  If there is no next key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:next/2

-spec next(gen_ns(), gen_tab(), key()) -> key() | '$end_of_table'.
next(NS, Tab, Key) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:next(Tid, Key)
    end.

%% @doc Returns the previous key +Key2+, following the key +Key1+ in
%% the table +Tab+.  If there is no previous key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:prev/2

-spec prev(gen_ns(), gen_tab(), key()) -> key() | '$end_of_table'.
prev(NS, Tab, Key) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        #gen_tid{mod=Mod}=Tid ->
            Mod:prev(Tid, Key)
    end.

%% @doc Matches the objects in the table +Tab+ against the spec
%% +Spec+.
%% @end
%% @see ets:select/2

-spec select(gen_ns(), gen_tab(), match_spec()) -> [match()].
select(NS, Tab, Spec) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:select(Tid, Spec)
    end.

%% @doc Matches the objects in the table +Tab+ against the spec +Spec+
%% and returns a limited (+Limit+) number of matching objects.
%% @end
%% @see ets:select/3

-spec select(gen_ns(), gen_tab(), match_spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select(NS, Tab, Spec, Limit) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:select(Tid, Spec, Limit))
    end.

%% @doc Continues a select started with +select/3+.
%% @end
%% @see ets:select/1

-spec select(gen_ns(), cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select(NS, {cont, Tab, Cont}) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:select(Cont))
    end;
select(_NS, '$end_of_table') ->
    '$end_of_table'.

%% @doc Counts all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number matched.
%% @end
%% @see ets:select_count/2

-spec select_count(gen_ns(), gen_tab(), match_spec()) -> pos_integer().
select_count(NS, Tab, Spec) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:select_count(Tid, Spec)
    end.

%% @doc Deletes all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number deleted.
%% @end
%% @see ets:select_delete/2

-spec select_delete(gen_ns(), gen_tab(), match_spec()) -> pos_integer().
select_delete(NS, Tab, Spec) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:select_delete(Tid, Spec)
    end.

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+.
%% @end
%% @see ets:select_reverse/2

-spec select_reverse(gen_ns(), gen_tab(), match_spec()) -> [match()].
select_reverse(NS, Tab, Spec) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:select_reverse(Tid, Spec)
    end.

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:select_reverse/3

-spec select_reverse(gen_ns(), gen_tab(), match_spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse(NS, Tab, Spec, Limit) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:select_reverse(Tid, Spec, Limit))
    end.

%% @doc Continues a select reverse started with +select_reverse/3+.
%% @end
%% @see ets:select_reverse/1

-spec select_reverse(gen_ns(), cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse(NS, {cont, Tab, Cont}) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            wrap_cont_reply(Tid, gen_ets_lib:select_reverse(Cont))
    end;
select_reverse(_NS, '$end_of_table') ->
    '$end_of_table'.

%% @doc Returns a list of all objects in the table +Tab+. The
%% operation is *not* guaranteed to be atomic and isolated.
%% @end
%% @see ets:tab2list/1

-spec tab2list(gen_ns(), gen_tab()) -> [object()].
tab2list(NS, Tab) ->
    case check_access(NS, Tab) of
        undefined ->
            erlang:error(badarg, [NS, Tab]);
        Tid ->
            gen_ets_lib:tab2list(Tid)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec check_access(gen_ns(), gen_tab()) -> gen_tid() | undefined.
check_access(NS, #gen_tid{impl=X, ns=Y})
  when X =:= undefined; Y =/= NS ->
    undefined;
check_access(NS, #gen_tid{ns=NS, protection=Protection, owner=Owner}=Tid)
  when Protection==public orelse Owner==self() ->
    Tid;
check_access(_NS, #gen_tid{}) ->
    undefined;
check_access(NS, Name) ->
    case gen_ets_reg:lookup(NS, Name) of
        undefined ->
            undefined;
        Tid ->
            check_access(NS, Tid)
    end.

create(NS, Op, Name, Opts) ->
    POpts = case options(Opts) of
                {POpts0, []} ->
                    POpts0;
                {_POpts0, BadArgs} ->
                    erlang:error(badarg, [NS, Name, BadArgs])
            end,

    Owner = self(),
    NamedTable = proplists:get_bool(named_table, POpts),
    Type =
        case proplists:get_bool(ordered_set, POpts) of
            true ->
                ordered_set;
            false ->
                set
        end,
    KeyPos = proplists:get_value(keypos, POpts, 1),
    Protection =
        case proplists:get_bool(private, POpts) of
            true ->
                private;
            false ->
                case proplists:get_bool(protected, POpts) of
                    true ->
                        protected;
                    false ->
                        case proplists:get_bool(public, POpts) of
                            true ->
                                public;
                            false ->
                                protected
                        end
                end
        end,
    Compressed = proplists:get_bool(compressed, POpts),
    Async = proplists:get_bool(async, POpts),

    {ImplMod, ImplOpts} = proplists:get_value(impl, POpts, {?DEFAULT_MOD, []}),
    Tid = #gen_tid{owner=Owner,
                   name=Name,
                   named_table=NamedTable,
                   type=Type,
                   keypos=KeyPos,
                   protection=Protection,
                   compressed=Compressed,
                   async=Async,
                   ns=NS,
                   mod=ImplMod},

    case gen_ets_reg:insert(Tid, Op, ImplOpts) of
        undefined ->
            erlang:error(badarg, [NS, Name, Opts]);
        Ok ->
            Ok
    end.

options(Options) ->
    Keys = [set, ordered_set, named_table, keypos, public, protected, private, compressed, async, impl],
    options(Options, Keys).

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options, [Key|Keys], L) when is_list(Options) ->
    case proplists:lookup(Key, Options) of
        none ->
            options(Options, Keys, L);
        {Key, Value} ->
            NewOptions = proplists:delete(Key, Options),
            options(NewOptions, Keys, [{Key,Value}|L])
    end;
options(Options, [], L) ->
    {lists:reverse(L), Options}.

wrap_cont_reply(_Tid, '$end_of_table'=Reply) ->
    Reply;
wrap_cont_reply(_Tid, {_Match, '$end_of_table'}=Reply) ->
    Reply;
wrap_cont_reply(Tid, {_Match, Cont}=Reply) ->
    setelement(2, Reply, {cont, Tid, Cont}).

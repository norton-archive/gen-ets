%%% The MIT License
%%%
%%% Copyright (C) 2011-2016 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(gen_ets_reg).

-include("gen_ets.hrl").

%% External exports
-export([setup/1
         , teardown/1
         , list/1
         , insert/3
         , delete/1
         , lookup/2
         %% low-level API
         , insert_new/3
         , delete_object/2
        ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-record(gen_reg, {name :: gen_ets:name(), pid::pid(), tid :: #gen_tid{}}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

setup(NS) ->
    case ets:info(NS, name) of
        undefined ->
            Caller = self(),
            Fun = fun() ->
                          register(NS, self()),
                          NS = ets:new(NS, [ordered_set, public, named_table, {keypos, #gen_reg.name}, {read_concurrency, true}]),
                          Caller ! {NS, self()},
                          receive
                              {NS, Pid, stop} ->
                                  Pid ! {NS, self()}
                          end
                  end,

            {Pid, Ref} = spawn_monitor(Fun),
            try
                receive
                    {NS, Pid} ->
                        ok;
                    {'DOWN', Ref, process, _Object, _Info} ->
                        ok
                end
            after
                demonitor(Ref, [flush])
            end;
        _ ->
            ok
    end.

teardown(NS) ->
    case whereis(NS) of
        undefined ->
            ok;
        Pid ->
            Ref = monitor(process, Pid),
            try
                Pid ! {NS, self(), stop},
                receive
                    {NS, Pid} ->
                        ok;
                    {'DOWN', Ref, process, _Object, _Info} ->
                        ok
                end
            after
                demonitor(Ref, [flush])
            end
    end.

list(NS) ->
    setup(NS),
    [ if Named -> Name; true -> Tid end
      || #gen_reg{name=Name, tid=#gen_tid{named_table=Named}=Tid} <- ets:tab2list(NS) ].

insert(#gen_tid{name=Name, named_table=Named, ns=NS}=PreTid, open=Op, Opts) ->
    setup(NS),
    case insert_new(NS, Name, PreTid) of
        undefined ->
            undefined;
        PreReg ->
            try
                Parent = self(),
                Fun = fun() -> loop(PreTid, Op, Opts, PreReg, Parent) end,

                {Child, ChildRef} = spawn_monitor(Fun),
                receive
                    {NS, Child, Tid} ->
                        if Named ->
                                Name;
                           true ->
                                Tid
                        end;
                    {'DOWN', ChildRef, process, _Object, _Info} ->
                        undefined
                end
            after
                %% cleanup failures
                catch delete_object(NS, PreReg)
            end
    end;
insert(#gen_tid{name=Name, ns=NS}=PreTid, Op, Opts) ->
    setup(NS),
    case insert_new(NS, Name, PreTid) of
        undefined ->
            false;
        PreReg ->
            try
                Parent = self(),
                Fun = fun() -> once(PreTid, Op, Opts, Parent) end,

                {Child, ChildRef} = spawn_monitor(Fun),
                receive
                    {NS, Child, Reply} ->
                        Reply;
                    {'DOWN', ChildRef, process, _Object, _Info} ->
                        false
                end
            after
                %% cleanup failures
                catch delete_object(NS, PreReg)
            end
    end.

delete(#gen_tid{name=Name, ns=NS}) ->
    try
        case ets:lookup(NS, Name) of
            [#gen_reg{pid=undefined}] ->
                false;
            [#gen_reg{pid=Pid}] ->
                Ref = monitor(process, Pid),
                Pid ! {NS, stop},
                try
                    receive
                        {'DOWN', Ref, process, _Object, _Info} ->
                            true
                    end
                after
                    demonitor(Ref, [flush])
                end;
            [] ->
                false
        end
    catch
        error:badarg ->
            false
    end.

lookup(NS, Name) ->
    try
        case ets:lookup(NS, Name) of
            [#gen_reg{tid=undefined}] ->
                undefined;
            [#gen_reg{tid=Tid}] ->
                Tid;
            [] ->
                undefined
        end
    catch
        error:badarg ->
            undefined
    end.

insert_new(NS, Name, Tid) ->
    Reg = #gen_reg{name=Name, tid=Tid},
    case ets:insert_new(NS, Reg) of
        true ->
            Reg;
        false ->
            undefined
    end.

delete_object(NS, Reg) ->
    ets:delete_object(NS, Reg).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

loop(#gen_tid{ns=NS, mod=Mod}=PreTid, Op, Opts, PreReg, Parent) ->
    ParentRef = monitor(process, Parent),

    Tid = Mod:Op(PreTid, Opts),
    Reg = PreReg#gen_reg{pid=self(), tid=Tid},

    ets:insert(NS, Reg),
    try
        Parent ! {NS, self(), Tid},
        receive
            {NS, stop} ->
                ok;
            {'DOWN', ParentRef, process, _Object, _Info} ->
                ok
        end,
        %% delete
        Mod:delete(Tid)
    after
        %% cleanup
        delete_object(NS, Reg),
        demonitor(ParentRef, [flush])
    end.

once(#gen_tid{ns=NS, mod=Mod}=PreTid, Op, Opts, Parent) ->
    Reply = Mod:Op(PreTid, Opts),
    Parent ! {NS, self(), Reply}.



# Module gen_ets_impl_ets #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_ets_ns`](gen_ets_ns.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td></td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td></td></tr><tr><td valign="top"><a href="#first-2">first/2</a></td><td></td></tr><tr><td valign="top"><a href="#first_iter-1">first_iter/1</a></td><td></td></tr><tr><td valign="top"><a href="#first_iter-2">first_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#info_memory-1">info_memory/1</a></td><td></td></tr><tr><td valign="top"><a href="#info_size-1">info_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td></td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td></td></tr><tr><td valign="top"><a href="#last_iter-1">last_iter/1</a></td><td></td></tr><tr><td valign="top"><a href="#last_iter-2">last_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td></td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-3">next/3</a></td><td></td></tr><tr><td valign="top"><a href="#next_iter-2">next_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#next_iter-3">next_iter/3</a></td><td></td></tr><tr><td valign="top"><a href="#notify-4">notify/4</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev-3">prev/3</a></td><td></td></tr><tr><td valign="top"><a href="#prev_iter-2">prev_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev_iter-3">prev_iter/3</a></td><td></td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

`delete(Gen_tid) -> any()`


<a name="delete-2"></a>

### delete/2 ###

`delete(Gen_tid, Key) -> any()`


<a name="delete_all_objects-1"></a>

### delete_all_objects/1 ###

`delete_all_objects(Gen_tid) -> any()`


<a name="destroy-2"></a>

### destroy/2 ###

`destroy(Gen_tid, Opts) -> any()`


<a name="first-1"></a>

### first/1 ###

`first(Gen_tid) -> any()`


<a name="first-2"></a>

### first/2 ###

`first(Tid, N) -> any()`


<a name="first_iter-1"></a>

### first_iter/1 ###

`first_iter(Gen_tid) -> any()`


<a name="first_iter-2"></a>

### first_iter/2 ###

`first_iter(Tid, N) -> any()`


<a name="info_memory-1"></a>

### info_memory/1 ###

`info_memory(Gen_tid) -> any()`


<a name="info_size-1"></a>

### info_size/1 ###

`info_size(Gen_tid) -> any()`


<a name="insert-2"></a>

### insert/2 ###

`insert(Gen_tid, ObjOrObjs) -> any()`


<a name="insert_new-2"></a>

### insert_new/2 ###

`insert_new(Gen_tid, ObjOrObjs) -> any()`


<a name="last-1"></a>

### last/1 ###

`last(Gen_tid) -> any()`


<a name="last-2"></a>

### last/2 ###

`last(Tid, N) -> any()`


<a name="last_iter-1"></a>

### last_iter/1 ###

`last_iter(Gen_tid) -> any()`


<a name="last_iter-2"></a>

### last_iter/2 ###

`last_iter(Tid, N) -> any()`


<a name="lookup-2"></a>

### lookup/2 ###

`lookup(Gen_tid, Key) -> any()`


<a name="lookup_element-3"></a>

### lookup_element/3 ###

`lookup_element(Gen_tid, Key, Pos) -> any()`


<a name="member-2"></a>

### member/2 ###

`member(Gen_tid, Key) -> any()`


<a name="next-2"></a>

### next/2 ###

`next(Gen_tid, Key) -> any()`


<a name="next-3"></a>

### next/3 ###

`next(Tid, Key, N) -> any()`


<a name="next_iter-2"></a>

### next_iter/2 ###

`next_iter(Gen_tid, Key) -> any()`


<a name="next_iter-3"></a>

### next_iter/3 ###

`next_iter(Tid, Key, N) -> any()`


<a name="notify-4"></a>

### notify/4 ###

`notify(Gen_tid, Event, Pid, Msg) -> any()`


<a name="open-2"></a>

### open/2 ###

`open(Gen_tid, Opts) -> any()`


<a name="prev-2"></a>

### prev/2 ###

`prev(Gen_tid, Key) -> any()`


<a name="prev-3"></a>

### prev/3 ###

`prev(Tid, Key, N) -> any()`


<a name="prev_iter-2"></a>

### prev_iter/2 ###

`prev_iter(Gen_tid, Key) -> any()`


<a name="prev_iter-3"></a>

### prev_iter/3 ###

`prev_iter(Tid, Key, N) -> any()`


<a name="repair-2"></a>

### repair/2 ###

`repair(Gen_tid, Opts) -> any()`





#Module gen_ets#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


__This module defines the `gen_ets` behaviour.__
<br></br>
 Required callback functions: `open/2`, `destroy/2`, `repair/2`, `delete/1`, `delete/2`, `delete_all_objects/1`, `first/1`, `foldl/3`, `foldr/3`, `info/1`, `info/2`, `insert/2`, `insert_new/2`, `last/1`, `lookup/2`, `lookup_element/3`, `match/2`, `match/3`, `match/1`, `match_delete/2`, `match_object/2`, `match_object/3`, `match_object/1`, `member/2`, `next/2`, `prev/2`, `select/2`, `select/3`, `select/1`, `select_count/2`, `select_delete/2`, `select_reverse/2`, `select_reverse/3`, `select_reverse/1`, `tab2list/1`.
<a name="types"></a>

##Data Types##




###<a name="type-cont">cont()</a>##



__abstract datatype__: `cont()`



###<a name="type-impl_opt">impl_opt()</a>##



<pre>impl_opt() = term()</pre>



###<a name="type-impl_opts">impl_opts()</a>##



<pre>impl_opts() = [<a href="#type-impl_opt">impl_opt()</a>]</pre>



###<a name="type-item">item()</a>##



<pre>item() = owner | name | named_table | type | keypos | protection | compressed | async | memory | size</pre>



###<a name="type-key">key()</a>##



<pre>key() = term()</pre>



###<a name="type-limit">limit()</a>##



<pre>limit() = pos_integer()</pre>



###<a name="type-match">match()</a>##



<pre>match() = term()</pre>



###<a name="type-match_pattern">match_pattern()</a>##



<pre>match_pattern() = atom() | tuple()</pre>




<pre><tt>ets:match_pattern() is not exported!</tt></pre>




###<a name="type-match_spec">match_spec()</a>##



<pre>match_spec() = <a href="ets.md#type-match_spec">ets:match_spec()</a></pre>



###<a name="type-name">name()</a>##



<pre>name() = term()</pre>



###<a name="type-object">object()</a>##



<pre>object() = tuple()</pre>



###<a name="type-opt">opt()</a>##



<pre>opt() = set | ordered_set | named_table | {keypos, pos_integer()} | public | protected | private | compressed | async</pre>



###<a name="type-opts">opts()</a>##



<pre>opts() = [<a href="#type-opt">opt()</a> | {impl, {module(), <a href="#type-impl_opts">impl_opts()</a>}}]</pre>



###<a name="type-pos">pos()</a>##



<pre>pos() = pos_integer()</pre>



###<a name="type-tab">tab()</a>##



<pre>tab() = <a href="#type-name">name()</a> | <a href="#type-tid">tid()</a></pre>



###<a name="type-tid">tid()</a>##



__abstract datatype__: `tid()`
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td><p>Returns a list of all tables at the node.</p>.</td></tr><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td><p>Deletes the entire table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td><p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td><p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>.</td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td><p>Destroy the contents of the specified table.</p>.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td><p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td><p>Fold from left to right over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td><p>Fold from right to left over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td><p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td><p>Returns the information associated with <tt>Item</tt> for the table <tt>Tab</tt>.</p>


<pre><tt>Valid +Item+ options are:</tt></pre>

<ul>
<li>
<p>
<tt>owner</tt>
</p>
</li>
<li>
<p>
<tt>name</tt>
</p>
</li>
<li>
<p>
<tt>named_table</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>type</tt>
</p>
</li>
<li>
<p>
<tt>keypos</tt>
</p>
</li>
<li>
<p>
<tt>protection</tt>
</p>
</li>
<li>
<p>
<tt>compressed</tt>
</p>
</li>
<li>
<p>
<tt>async</tt> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<tt>memory</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>size</tt> <em>only the ets implementation</em>
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td><p>Inserts the object or all of the objects in the list
<tt>ObjOrObjs</tt> into the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td><p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td><p>Returns the last key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td><p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td><p>Returns the <tt>Pos</tt>:th element of the object with the key <tt>Key</tt>
in the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td><p>Continues a match started with <tt>match/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#match_delete-2">match_delete/2</a></td><td><p>Deletes all objects which match the pattern <tt>Pattern</tt> from the
table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-1">match_object/1</a></td><td><p>Continues a match started with <tt>match_object/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-2">match_object/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-3">match_object/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td><p>Returns <tt>true</tt> if one or more elements in the table <tt>Tab</tt> has
the key <tt>Key</tt>, <tt>false</tt> otherwise.</p>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td><p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><tt>Valid GEN_ETS properties for +Options+ are:</tt></pre>

<ul>
<li>
<p>
<tt>set</tt> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<tt>ordered_set</tt> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <tt><</tt> and <tt>></tt> operators.
</p>
</li>
<li>
<p>
<tt>named_table</tt> If this option is present, the name <tt>Name</tt> is
associated with the table identifier.
</p>
</li>
<li>
<p>
<tt>{keypos,pos_integer()}</tt> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <tt>Pos=1</tt>.
</p>
</li>
<li>
<p>
<tt>public</tt> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<tt>protected</tt> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<tt>private</tt> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<tt>compressed</tt> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<tt>async</tt> If this option is present and supported by the
implementation, the emulator's async thread pool will be used
when accessing the table data.
</p>
</li>
<li>
<p>
<tt>{impl, module(), [impl_option()]}</tt> The module that implements
GEN_ETS callback functions.  Implementation specific options can
be given. The default is <tt>{impl, gen_ets_impl_ets, []}</tt>.
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td><p>Returns the next key <tt>Key2</tt>, following the key <tt>Key1</tt> in the
table <tt>Tab</tt>.  If there is no next key, <tt><em>$end_of_table</em></tt> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td><p>Returns the previous key <tt>Key2</tt>, following the key <tt>Key1</tt> in
the table <tt>Tab</tt>.  If there is no previous key, <tt><em>$end_of_table</em></tt> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td><p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information.</p>.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td><p>Continues a select started with <tt>select/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the spec
<tt>Spec</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the spec <tt>Spec</tt>
and returns a limited (<tt>Limit</tt>) number of matching objects.</p>.</td></tr><tr><td valign="top"><a href="#select_count-2">select_count/2</a></td><td><p>Counts all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number matched.</p>.</td></tr><tr><td valign="top"><a href="#select_delete-2">select_delete/2</a></td><td><p>Deletes all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number deleted.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-1">select_reverse/1</a></td><td><p>Continues a select reverse started with <tt>select_reverse/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-2">select_reverse/2</a></td><td><p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-3">select_reverse/3</a></td><td><p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td><p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="all-0"></a>

###all/0##


<pre>all() -> [<a href="#type-tab">tab()</a>]</pre>
<br></br>


<p>Returns a list of all tables at the node.</p>


__See also:__ [ets:all/0](ets.md#all-0).<a name="behaviour_info-1"></a>

###behaviour_info/1##


`behaviour_info(Other) -> any()`

<a name="delete-1"></a>

###delete/1##


<pre>delete(Tab::<a href="#type-tab">tab()</a>) -> true</pre>
<br></br>


<p>Deletes the entire table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/1](ets.md#delete-1).<a name="delete-2"></a>

###delete/2##


<pre>delete(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> true</pre>
<br></br>


<p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/2](ets.md#delete-2).<a name="delete_all_objects-1"></a>

###delete_all_objects/1##


<pre>delete_all_objects(Tab::<a href="#type-tab">tab()</a>) -> true</pre>
<br></br>


<p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>


__See also:__ [ets:delete_all_objects/1](ets.md#delete_all_objects-1).<a name="destroy-2"></a>

###destroy/2##


<pre>destroy(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> true</pre>
<br></br>


<p>Destroy the contents of the specified table.</p>
<a name="first-1"></a>

###first/1##


<pre>first(Tab::<a href="#type-tab">tab()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>


<p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>


__See also:__ [ets:first/1](ets.md#first-1).<a name="foldl-3"></a>

###foldl/3##


<pre>foldl(Fun, Acc0::term(), Tab::<a href="#type-tab">tab()</a>) -> Acc1::term()</pre>
<ul class="definitions"><li><pre>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</pre></li></ul>

<p>Fold from left to right over the elements of the table.</p>


__See also:__ [ets:foldl/3](ets.md#foldl-3).<a name="foldr-3"></a>

###foldr/3##


<pre>foldr(Fun, Acc0::term(), Tab::<a href="#type-tab">tab()</a>) -> Acc1::term()</pre>
<ul class="definitions"><li><pre>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</pre></li></ul>

<p>Fold from right to left over the elements of the table.</p>


__See also:__ [ets:foldr/3](ets.md#foldr-3).<a name="info-1"></a>

###info/1##


<pre>info(Tab::<a href="#type-tab">tab()</a>) -> [{<a href="#type-item">item()</a>, term()}]</pre>
<br></br>


<p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>


__See also:__ [info/2](#info-2).<a name="info-2"></a>

###info/2##


<pre>info(Tab::<a href="#type-tab">tab()</a>, Item::<a href="#type-item">item()</a>) -> term()</pre>
<br></br>


<p>Returns the information associated with <tt>Item</tt> for the table <tt>Tab</tt>.</p>


<pre><tt>Valid +Item+ options are:</tt></pre>

<ul>
<li>
<p>
<tt>owner</tt>
</p>
</li>
<li>
<p>
<tt>name</tt>
</p>
</li>
<li>
<p>
<tt>named_table</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>type</tt>
</p>
</li>
<li>
<p>
<tt>keypos</tt>
</p>
</li>
<li>
<p>
<tt>protection</tt>
</p>
</li>
<li>
<p>
<tt>compressed</tt>
</p>
</li>
<li>
<p>
<tt>async</tt> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<tt>memory</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>size</tt> <em>only the ets implementation</em>
</p>
</li>
</ul>


__See also:__ [ets:info/2](ets.md#info-2).<a name="insert-2"></a>

###insert/2##


<pre>insert(Tab::<a href="#type-tab">tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -> true</pre>
<br></br>


<p>Inserts the object or all of the objects in the list
<tt>ObjOrObjs</tt> into the table <tt>Tab</tt>.</p>


__See also:__ [ets:insert/2](ets.md#insert-2).<a name="insert_new-2"></a>

###insert_new/2##


<pre>insert_new(Tab::<a href="#type-tab">tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -> true</pre>
<br></br>


<p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>


__See also:__ [ets:insert_new/2](ets.md#insert_new-2).<a name="last-1"></a>

###last/1##


<pre>last(Tab::<a href="#type-tab">tab()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>


<p>Returns the last key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>


__See also:__ [ets:last/1](ets.md#last-1).<a name="lookup-2"></a>

###lookup/2##


<pre>lookup(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> [<a href="#type-object">object()</a>]</pre>
<br></br>


<p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>


__See also:__ [ets:lookup/2](ets.md#lookup-2).<a name="lookup_element-3"></a>

###lookup_element/3##


<pre>lookup_element(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>, Pos::<a href="#type-pos">pos()</a>) -> term()</pre>
<br></br>


<p>Returns the <tt>Pos</tt>:th element of the object with the key <tt>Key</tt>
in the table <tt>Tab</tt>.</p>


__See also:__ [ets:lookup_element/3](ets.md#lookup_element-3).<a name="match-1"></a>

###match/1##


<pre>match(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Continues a match started with <tt>match/3</tt>.</p>


__See also:__ [ets:match/1](ets.md#match-1).<a name="match-2"></a>

###match/2##


<pre>match(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>


__See also:__ [ets:match/2](ets.md#match-2).<a name="match-3"></a>

###match/3##


<pre>match(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:match/3](ets.md#match-3).<a name="match_delete-2"></a>

###match_delete/2##


<pre>match_delete(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -> true</pre>
<br></br>


<p>Deletes all objects which match the pattern <tt>Pattern</tt> from the
table <tt>Tab</tt>.</p>


__See also:__ [ets:match_delete/2](ets.md#match_delete-2).<a name="match_object-1"></a>

###match_object/1##


<pre>match_object(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Continues a match started with <tt>match_object/3</tt>.</p>


__See also:__ [ets:match_object/1](ets.md#match_object-1).<a name="match_object-2"></a>

###match_object/2##


<pre>match_object(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>


__See also:__ [ets:match_object/2](ets.md#match_object-2).<a name="match_object-3"></a>

###match_object/3##


<pre>match_object(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:match_object/3](ets.md#match_object-3).<a name="member-2"></a>

###member/2##


<pre>member(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> true | false</pre>
<br></br>


<p>Returns <tt>true</tt> if one or more elements in the table <tt>Tab</tt> has
the key <tt>Key</tt>, <tt>false</tt> otherwise.</p>


__See also:__ [ets:member/2](ets.md#member-2).<a name="new-2"></a>

###new/2##


<pre>new(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> <a href="#type-tab">tab()</a></pre>
<br></br>


<p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><tt>Valid GEN_ETS properties for +Options+ are:</tt></pre>

<ul>
<li>
<p>
<tt>set</tt> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<tt>ordered_set</tt> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <tt><</tt> and <tt>></tt> operators.
</p>
</li>
<li>
<p>
<tt>named_table</tt> If this option is present, the name <tt>Name</tt> is
associated with the table identifier.
</p>
</li>
<li>
<p>
<tt>{keypos,pos_integer()}</tt> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <tt>Pos=1</tt>.
</p>
</li>
<li>
<p>
<tt>public</tt> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<tt>protected</tt> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<tt>private</tt> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<tt>compressed</tt> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<tt>async</tt> If this option is present and supported by the
implementation, the emulator's async thread pool will be used
when accessing the table data.
</p>
</li>
<li>
<p>
<tt>{impl, module(), [impl_option()]}</tt> The module that implements
GEN_ETS callback functions.  Implementation specific options can
be given. The default is <tt>{impl, gen_ets_impl_ets, []}</tt>.
</p>
</li>
</ul>


__See also:__ [ets:new/2](ets.md#new-2).<a name="next-2"></a>

###next/2##


<pre>next(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>


<p>Returns the next key <tt>Key2</tt>, following the key <tt>Key1</tt> in the
table <tt>Tab</tt>.  If there is no next key, <tt><em>$end_of_table</em></tt> is
returned.</p>


__See also:__ [ets:next/2](ets.md#next-2).<a name="prev-2"></a>

###prev/2##


<pre>prev(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>


<p>Returns the previous key <tt>Key2</tt>, following the key <tt>Key1</tt> in
the table <tt>Tab</tt>.  If there is no previous key, <tt><em>$end_of_table</em></tt> is
returned.</p>


__See also:__ [ets:prev/2](ets.md#prev-2).<a name="repair-2"></a>

###repair/2##


<pre>repair(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> true</pre>
<br></br>


<p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information.</p>
<a name="select-1"></a>

###select/1##


<pre>select(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Continues a select started with <tt>select/3</tt>.</p>


__See also:__ [ets:select/1](ets.md#select-1).<a name="select-2"></a>

###select/2##


<pre>select(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the spec
<tt>Spec</tt>.</p>


__See also:__ [ets:select/2](ets.md#select-2).<a name="select-3"></a>

###select/3##


<pre>select(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Matches the objects in the table <tt>Tab</tt> against the spec <tt>Spec</tt>
and returns a limited (<tt>Limit</tt>) number of matching objects.</p>


__See also:__ [ets:select/3](ets.md#select-3).<a name="select_count-2"></a>

###select_count/2##


<pre>select_count(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_pattern">match_pattern()</a>) -> pos_integer()</pre>
<br></br>


<p>Counts all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number matched.</p>


__See also:__ [ets:select_count/2](ets.md#select_count-2).<a name="select_delete-2"></a>

###select_delete/2##


<pre>select_delete(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_pattern">match_pattern()</a>) -> pos_integer()</pre>
<br></br>


<p>Deletes all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number deleted.</p>


__See also:__ [ets:select_delete/2](ets.md#select_delete-2).<a name="select_reverse-1"></a>

###select_reverse/1##


<pre>select_reverse(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Continues a select reverse started with <tt>select_reverse/3</tt>.</p>


__See also:__ [ets:select_reverse/1](ets.md#select_reverse-1).<a name="select_reverse-2"></a>

###select_reverse/2##


<pre>select_reverse(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>


<p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt>.</p>


__See also:__ [ets:select_reverse/2](ets.md#select_reverse-2).<a name="select_reverse-3"></a>

###select_reverse/3##


<pre>select_reverse(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>


<p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:select_reverse/3](ets.md#select_reverse-3).<a name="tab2list-1"></a>

###tab2list/1##


<pre>tab2list(Tab::<a href="#type-tab">tab()</a>) -> [<a href="#type-object">object()</a>]</pre>
<br></br>


<p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>


__See also:__ [ets:tab2list/1](ets.md#tab2list-1).
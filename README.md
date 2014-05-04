

# GEN_ETS - GEN(eric) Erlang Term Storage #

Copyright (c) 2011-2014 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>GEN_ETS is an generic wrapper for Erlang Term Storage with a callback
interface for a backend implementation.</p>
<p>GEN_ETS is not intended to be an exact clone of ETS.  The currently
supported ETS APIs are:</p>
<ul>
<li>
<p>
<code>all/0</code>
</p>
</li>
<li>
<p>
<code>delete/1</code>
</p>
</li>
<li>
<p>
<code>delete/2</code>
</p>
</li>
<li>
<p>
<code>delete_all_objects/1</code>
</p>
</li>
<li>
<p>
<code>first/1</code>
</p>
</li>
<li>
<p>
<code>foldl/3</code>
</p>
</li>
<li>
<p>
<code>foldr/3</code>
</p>
</li>
<li>
<p>
<code>info/1</code> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<code>info/2</code> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<code>insert/2</code>
</p>
</li>
<li>
<p>
<code>insert_new/2</code>
</p>
</li>
<li>
<p>
<code>last/1</code>
</p>
</li>
<li>
<p>
<code>lookup/2</code>
</p>
</li>
<li>
<p>
<code>lookup_element/3</code>
</p>
</li>
<li>
<p>
<code>match/1</code>
</p>
</li>
<li>
<p>
<code>match/2</code>
</p>
</li>
<li>
<p>
<code>match/3</code>
</p>
</li>
<li>
<p>
<code>match_delete/2</code>
</p>
</li>
<li>
<p>
<code>match_object/1</code>
</p>
</li>
<li>
<p>
<code>match_object/2</code>
</p>
</li>
<li>
<p>
<code>match_object/3</code>
</p>
</li>
<li>
<p>
<code>member/2</code>
</p>
</li>
<li>
<p>
<code>new/2</code>
</p>
</li>
<li>
<p>
<code>next/2</code>
</p>
</li>
<li>
<p>
<code>prev/2</code>
</p>
</li>
<li>
<p>
<code>select/1</code>
</p>
</li>
<li>
<p>
<code>select/2</code>
</p>
</li>
<li>
<p>
<code>select/3</code>
</p>
</li>
<li>
<p>
<code>select_count/2</code>
</p>
</li>
<li>
<p>
<code>select_delete/2</code>
</p>
</li>
<li>
<p>
<code>select_reverse/1</code>
</p>
</li>
<li>
<p>
<code>select_reverse/2</code>
</p>
</li>
<li>
<p>
<code>select_reverse/3</code>
</p>
</li>
<li>
<p>
<code>tab2list/1</code>
</p>
</li>
</ul>
<p>In particular, GEN_ETS differs from ETS in the following ways:</p>
<ul>
<li>
<p>
The name of a table can be any Erlang term.
</p>
</li>
<li>
<p>
All APIs require a namespace.  A namespace must be a unique,
  reserved atom.  Due to limitiations of the current implementation of
  GEN_ETS, this reserved atom is used to register a local named
  process and to create a named ets table.
</p>

<table><tr>
<td class="icon">
Caution
</td>
<td class="content">Choose your application's namespace(s) wisely.</td>
</tr></table>

</li>
</ul>
<p>For convience and testing purposes, GEN_ETS provides a default
namespace wrapper and backend implementation based on ETS. See
<code>gen_ets</code> and <code>gen_ets_impl_ets</code> for further details.</p>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find GEN_ETS useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download and build the gen_ets application in one shot, please follow
this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/gen-ets.git gen_ets
$ cd gen_ets
$ make deps clean compile</code></pre>

<p><em>OR</em> if QuickCheck is available then follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/gen-ets.git gen_ets
$ cd gen_ets
$ make deps clean compile-for-eqc
$ (cd .qc; erl -smp +A 5 -pz ../deps/qc/ebin)

1> qc_statem_gen_ets:qc_run(500).
  :
  :
OK, passed 500 tests

100.0% {1,attempts}

7.10% {delete,ok}
7.01% {new,ok}
3.76% {select_count,ok}
3.65% {select,ok}
3.57% {match_object,ok}
3.56% {match31,ok}
3.54% {select_reverse31,ok}
3.54% {insert,ok}
3.54% {select_reverse,ok}
3.52% {last,ok}
3.47% {match_delete,ok}
3.46% {tab2list,ok}
3.46% {lookup,ok}
3.45% {foldl,ok}
3.44% {insert_new,ok}
3.43% {member,ok}
3.41% {match,ok}
3.40% {match_object31,ok}
3.35% {foldr,ok}
3.34% {first,ok}
3.33% {all,ok}
3.32% {select_delete,ok}
3.30% {delete_all_objects,ok}
3.28% {select31,ok}
2.40% {lookup_element,{error,badarg}}
2.09% {next,ok}
1.90% {prev,ok}
1.38% {next,{error,badarg}}
1.36% {prev,{error,badarg}}
0.66% {lookup_element,ok}
true</code></pre>




<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation right now.</p>
<p>The QC (a.k.a. QuickCheck) tests underneath the "tests/qc" directory
should be helpful for understanding the specification and behavior of
ETS and GEN_ETS.  NIF-based implementations.</p>


<h3 id="_what_is_ets_and_dets">What is ETS and DETS?</h3>
<p>ETS and DETS are Erlang/OTP's standard library modules for Erlang
term storage.  ETS is a memory-based implementation.  DETS is a
disk-based implementation.</p>
<p>See <a href="http://www.erlang.org/doc/man/ets.html">http://www.erlang.org/doc/man/ets.html</a> and
<a href="http://www.erlang.org/doc/man/dets.html">http://www.erlang.org/doc/man/dets.html</a> for further details.</p>




<h2 id="_tools">Tools</h2>

<p>For further information and help for related tools, please refer to
the following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R15 or newer, 17.0 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.9.2 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
</ul>



<h2 id="_roadmap">Roadmap</h2>

<p><em>N/A</em></p>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets.md" class="module">gen_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_impl_ets.md" class="module">gen_ets_impl_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_lib.md" class="module">gen_ets_lib</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_ns.md" class="module">gen_ets_ns</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_reg.md" class="module">gen_ets_reg</a></td></tr></table>


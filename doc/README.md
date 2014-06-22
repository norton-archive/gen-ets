

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

3.81% {insert_new,2,ok}
3.60% {match_object,2,ok}
3.57% {select,2,ok}
3.57% {foldl,3,ok}
3.55% {foldr,3,ok}
3.53% {lookup,2,ok}
3.52% {select_count,2,ok}
3.52% {all,1,ok}
3.52% {match_delete,2,ok}
3.51% {select31,3,ok}
3.50% {delete,1,ok}
3.45% {match31,3,ok}
3.44% {member,2,ok}
3.43% {insert,2,ok}
3.39% {new,3,ok}
3.39% {match_object31,3,ok}
3.39% {match,2,ok}
3.35% {select_reverse,2,ok}
3.35% {tab2list,1,ok}
3.34% {select_delete,2,ok}
3.34% {new,2,ok}
3.33% {delete_all_objects,1,ok}
3.27% {delete,2,ok}
3.25% {select_reverse31,3,ok}
3.24% {last,1,ok}
3.23% {first,1,ok}
2.95% {lookup_element,3,{error,badarg}}
2.35% {prev,2,ok}
1.98% {next,2,ok}
1.40% {prev,2,{error,badarg}}
1.26% {next,2,{error,badarg}}
0.68% {lookup_element,3,ok}
true</code></pre>

<p><em>OR</em> if PropEr is available then follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/gen-ets.git gen_ets
$ cd gen_ets
$ make deps clean compile-for-proper
$ (cd .qc; erl -smp +A 5 -pz ../deps/qc/ebin)

1> qc_statem_gen_ets:qc_run(500).
  :
  :
OK: Passed 500 test(s).

8% {new,2,ok}
3% {all,1,ok}
3% {select31,3,ok}
3% {match_object31,3,ok}
3% {first,1,ok}
3% {match_delete,2,ok}
3% {select_count,2,ok}
3% {delete,1,ok}
3% {foldr,3,ok}
3% {select_reverse31,3,ok}
3% {match_object,2,ok}
3% {foldl,3,ok}
3% {select_delete,2,ok}
3% {select,2,ok}
3% {insert_new,2,ok}
3% {delete,2,ok}
3% {insert,2,ok}
3% {delete_all_objects,1,ok}
3% {new,3,ok}
3% {lookup,2,ok}
3% {match,2,ok}
3% {member,2,ok}
3% {match31,3,ok}
3% {select_reverse,2,ok}
3% {tab2list,1,ok}
2% {last,1,ok}
2% {lookup_element,3,{error,badarg}}
2% {next,2,ok}
1% {prev,2,ok}
1% {next,2,{error,badarg}}
1% {prev,2,{error,badarg}}
0% {lookup_element,3,ok}
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
<tr><td><a href="gen_ets.md" class="module">gen_ets</a></td></tr>
<tr><td><a href="gen_ets_impl_ets.md" class="module">gen_ets_impl_ets</a></td></tr>
<tr><td><a href="gen_ets_lib.md" class="module">gen_ets_lib</a></td></tr>
<tr><td><a href="gen_ets_ns.md" class="module">gen_ets_ns</a></td></tr>
<tr><td><a href="gen_ets_reg.md" class="module">gen_ets_reg</a></td></tr></table>


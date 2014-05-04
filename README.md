

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
$ make deps clean eqc
$ (cd .qc; erl -smp +A 5 -pz ../deps/qc/ebin)

1> qc_statem_gen_ets:qc_run(500, []).
  :
  :
OK, passed 500 tests

100.0% {1,attempts}

7.11% {delete,ok}
6.78% {new,ok}
3.91% {insert_new,ok}
3.83% {select_reverse31,ok}
3.76% {select31,ok}
3.69% {tab2list,ok}
3.69% {match31,ok}
3.69% {first,ok}
3.67% {delete_all_objects,ok}
3.66% {foldl,ok}
3.63% {select,ok}
3.63% {member,ok}
3.61% {select_count,ok}
3.61% {last,ok}
3.61% {foldr,ok}
3.57% {insert,ok}
3.50% {match_object31,ok}
3.49% {match_delete,ok}
3.44% {lookup,ok}
3.42% {select_delete,ok}
3.36% {match_object,ok}
3.34% {select_reverse,ok}
3.19% {match,ok}
2.92% {lookup_element,{error,badarg}}
2.17% {prev,ok}
1.88% {next,ok}
1.57% {next,{error,badarg}}
1.56% {prev,{error,badarg}}
0.72% {lookup_element,ok}
true</code></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation right now.</p>
<p>The QC (a.k.a. QuickCheck, PropEr, etc.) tests underneath the
"tests/qc" directory should be helpful for understanding the
specification and behavior of ETS and GEN_ETS.  NIF-based
implementations.</p>


<h3 id="_what_is_ets_and_dets">What is ETS and DETS?</h3>
<p>ETS and DETS are Erlang/OTP's standard library modules for Erlang
term storage.  ETS is a memory-based implementation.  DETS is a
disk-based implementation.</p>
<p>See <a href="http://www.erlang.org/doc/man/ets.html">http://www.erlang.org/doc/man/ets.html</a> and
<a href="http://www.erlang.org/doc/man/dets.html">http://www.erlang.org/doc/man/dets.html</a> for further details.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><code>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</code></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><code>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</code></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/norton/manifests.git -m gen_ets-default.xml</code></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:norton/manifests.git -m gen_ets-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><code>$ cd working-directory-name
$ repo sync</code></pre>

</li>
</ol>
<p>For further information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R15 or newer, R16B has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.8.2 has been tested most recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.3 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/rebar/rebar/wiki">https://github.com/rebar/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.html">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><code>$ cd working-directory-name
$ make compile</code></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><code>$ cd working-directory-name
$ make build-plt</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze</code></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze-nospec</code></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_to_test_quickcheck">To test - QuickCheck</h2>

<ol class="arabic">
<li>
<p>
Make sure QuickCheck is in your Erlang code path.  One simple way
   to accomplish this is by adding the code path to your <code>~/.erlang</code>
   resource file.
</p>


<pre><code>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/quviq/eqc-X.Y.Z/ebin").</code></pre>

</li>
<li>
<p>
Compile for QuickCheck
</p>


<pre><code>$ cd working-directory-name
$ make clean
$ make compile-for-eqc</code></pre>

</li>
<li>
<p>
Run 5,000 QuickCheck tests
</p>


<pre><code>$ cd working-directory-name/deps/gen_ets/.qc
$ erl -smp +A 5 -pz -pz ../../qc/ebin

1> qc_statem_gen_ets:qc_run(5000).
....
OK, passed 5000 tests

100.0% {1,attempts}

7.11% {delete,ok}
6.78% {new,ok}
3.91% {insert_new,ok}
3.83% {select_reverse31,ok}
3.76% {select31,ok}
3.69% {tab2list,ok}
3.69% {match31,ok}
3.69% {first,ok}
3.67% {delete_all_objects,ok}
3.66% {foldl,ok}
3.63% {select,ok}
3.63% {member,ok}
3.61% {select_count,ok}
3.61% {last,ok}
3.61% {foldr,ok}
3.57% {insert,ok}
3.50% {match_object31,ok}
3.49% {match_delete,ok}
3.44% {lookup,ok}
3.42% {select_delete,ok}
3.36% {match_object,ok}
3.34% {select_reverse,ok}
3.19% {match,ok}
2.92% {lookup_element,{error,badarg}}
2.17% {prev,ok}
1.88% {next,ok}
1.57% {next,{error,badarg}}
1.56% {prev,{error,badarg}}
0.72% {lookup_element,ok}
true
.......</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try<code>qc_statemc_gen_ets:qc_run(5000)</code>.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - PropEr</h2>

<ol class="arabic">
<li>
<p>
Make sure PropEr is in your Erlang code path.  One simple way to
   accomplish this is by adding the code path to your <code>~/.erlang</code>
   resource file.
</p>


<pre><code>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/proper/ebin").</code></pre>

</li>
<li>
<p>
Compile for PropEr
</p>


<pre><code>$ cd working-directory-name
$ make clean
$ make compile-for-proper</code></pre>

</li>
<li>
<p>
Run 5,000 PropEr tests
</p>


<pre><code>$ cd working-directory-name/deps/gen_ets/.qc
$ erl -smp +A 5 -pz -pz ../../qc/ebin

1> qc_statem_gen_ets:qc_run(5000).
....
OK: Passed 5000 test(s).

11% {new,ok}
8% {delete,ok}
4% {member,ok}
4% {select,ok}
4% {select_count,ok}
4% {select_reverse,ok}
4% {lookup,ok}
4% {match_object,ok}
4% {tab2list,ok}
4% {last,ok}
4% {match,ok}
4% {foldl,ok}
4% {match_delete,ok}
3% {prev,ok}
3% {select31,ok}
3% {select_delete,ok}
3% {foldr,ok}
3% {insert,ok}
3% {first,ok}
3% {next,ok}
3% {lookup_element,{error,badarg}}
1% {insert_new,ok}
0% {prev,{error,badarg}}
0% {lookup_element,ok}
0% {next,{error,badarg}}
true
.......</code></pre>

</li>
</ol>



<h2 id="_roadmap">Roadmap</h2>

<p><em>N/A</em></p>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets.md" class="module">gen_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_impl_ets.md" class="module">gen_ets_impl_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_lib.md" class="module">gen_ets_lib</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_ns.md" class="module">gen_ets_ns</a></td></tr>
<tr><td><a href="https://github.com/norton/gen-ets/blob/master/doc/gen_ets_reg.md" class="module">gen_ets_reg</a></td></tr></table>


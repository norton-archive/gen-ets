

#GEN_ETS - GEN(eric) Erlang Term Storage#


Copyright (c) 2011-2012 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>GEN_ETS is an generic wrapper for Erlang Term Storage with a callback
interface for a backend implementation.</p>
<p>For testing purposes, GEN_ETS supports a default backend
implementation:</p>
<ul>
<li>
<p>
<tt>ets</tt> Erlang ETS backend
</p>
</li>
</ul>
<p>GEN_ETS is not intended to be an exact clone of ETS.  The currently
supported ETS APIs are:</p>
<ul>
<li>
<p>
<tt>all/0</tt>
</p>
</li>
<li>
<p>
<tt>delete/1</tt>
</p>
</li>
<li>
<p>
<tt>delete/2</tt>
</p>
</li>
<li>
<p>
<tt>delete_all_objects/1</tt>
</p>
</li>
<li>
<p>
<tt>first/1</tt>
</p>
</li>
<li>
<p>
<tt>foldl/3</tt>
</p>
</li>
<li>
<p>
<tt>foldr/3</tt>
</p>
</li>
<li>
<p>
<tt>info/1</tt> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<tt>info/2</tt> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<tt>insert/2</tt>
</p>
</li>
<li>
<p>
<tt>insert_new/2</tt>
</p>
</li>
<li>
<p>
<tt>last/1</tt>
</p>
</li>
<li>
<p>
<tt>lookup/2</tt>
</p>
</li>
<li>
<p>
<tt>lookup_element/3</tt>
</p>
</li>
<li>
<p>
<tt>match/1</tt>
</p>
</li>
<li>
<p>
<tt>match/2</tt>
</p>
</li>
<li>
<p>
<tt>match/3</tt>
</p>
</li>
<li>
<p>
<tt>match_delete/2</tt>
</p>
</li>
<li>
<p>
<tt>match_object/1</tt>
</p>
</li>
<li>
<p>
<tt>match_object/2</tt>
</p>
</li>
<li>
<p>
<tt>match_object/3</tt>
</p>
</li>
<li>
<p>
<tt>member/2</tt>
</p>
</li>
<li>
<p>
<tt>new/2</tt>
</p>
</li>
<li>
<p>
<tt>next/2</tt>
</p>
</li>
<li>
<p>
<tt>prev/2</tt>
</p>
</li>
<li>
<p>
<tt>select/1</tt>
</p>
</li>
<li>
<p>
<tt>select/2</tt>
</p>
</li>
<li>
<p>
<tt>select/3</tt>
</p>
</li>
<li>
<p>
<tt>select_count/2</tt>
</p>
</li>
<li>
<p>
<tt>select_delete/2</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/1</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/2</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/3</tt>
</p>
</li>
<li>
<p>
<tt>tab2list/1</tt>
</p>
</li>
</ul>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find GEN_ETS useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download and build the gen_ets application in one shot, please follow
this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/gen_ets.git gen_ets
$ cd gen_ets
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile</tt></pre>

<p><em>OR</em> if QuickCheck is available then follow the this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/gen_ets.git gen_ets
$ cd gen_ets
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile -D QC -D QC_EQC
$ ./rebar test-compile -D QC -D QC_EQC
$ (cd .test; erl -smp +A 5 -pz ../../qc/ebin)

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
true</tt></pre>

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


<pre><tt>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</tt></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><tt>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/norton/manifests.git -m gen_ets-default.xml</tt></pre>


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


<pre><tt>$ cd working-directory-name
$ repo sync</tt></pre>

</li>
</ol>
<p>For futher information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R14 or newer, R15B02 has been tested recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.7.12 has been tested recently</strong>
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
<strong>Python 2.4 or newer, Python 2.7.2 has been tested recently
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
Rebar - <a href="https://github.com/basho/rebar/wiki">https://github.com/basho/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.md">http://source.android.com/source/git-repo.html</a>
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


<pre><tt>$ cd working-directory-name
$ make compile</tt></pre>

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


<pre><tt>$ cd working-directory-name
$ make build-plt</tt></pre>


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


<pre><tt>$ cd working-directory-name
$ make dialyze</tt></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .test directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><tt>$ cd working-directory-name
$ make dialyze-nospec</tt></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_to_test_quickcheck">To test - QuickCheck</h2>

<ol class="arabic">
<li>
<p>
Make sure QuickCheck is in your Erlang code path.  One simple way
   to accomplish this is by adding the code path to your <tt>~/.erlang</tt>
   resource file.
</p>


<pre><tt>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/quviq/eqc-X.Y.Z/ebin").</tt></pre>

</li>
<li>
<p>
Compile for QuickCheck
</p>


<pre><tt>$ cd working-directory-name
$ make clean
$ make eqc-compile</tt></pre>

</li>
<li>
<p>
Run 5,000 QuickCheck tests
</p>


<pre><tt>$ cd working-directory-name/deps/gen_ets/.test
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
.......</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try<tt>qc_statemc_gen_ets:qc_run(5000)</tt>.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - PropEr</h2>

<ol class="arabic">
<li>
<p>
Make sure PropEr is in your Erlang code path.  One simple way to
   accomplish this is by adding the code path to your <tt>~/.erlang</tt>
   resource file.
</p>


<pre><tt>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/proper/ebin").</tt></pre>

</li>
<li>
<p>
Compile for PropEr
</p>


<pre><tt>$ cd working-directory-name
$ make clean
$ make proper-compile</tt></pre>

</li>
<li>
<p>
Run 5,000 PropEr tests
</p>


<pre><tt>$ cd working-directory-name/deps/gen_ets/.test
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
.......</tt></pre>

</li>
</ol>



<h2 id="_roadmap">Roadmap</h2>

<p><em>TODO</em></p>




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/gen_ets/blob/master/doc/gen_ets.md" class="module">gen_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/gen_ets/blob/master/doc/gen_ets_impl_ets.md" class="module">gen_ets_impl_ets</a></td></tr></table>


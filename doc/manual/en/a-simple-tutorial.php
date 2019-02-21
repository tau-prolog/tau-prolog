<div class="match-width manual">
	<h2>A simple tutorial</h2>
	
    <h3 id="what-is"><a href="#what-is">What is Tau Prolog?</a></h3>
    <p>Tau Prolog is a Prolog interpreter fully implemented in JavaScript. Tau Prolog is not just an implementation of the logic programming resolution mechanism, but a complete Prolog interpreter. Furthermore, the implementation of this interpreter has been directed by the ISO Prolog Standard<span class="manual-annotation">(see <a target="_blank" href="http://fsl.cs.illinois.edu/images/9/9c/PrologStandard.pdf">[Prolog Standard]</a>, <a target="_blank" href="http://www.deransart.fr/prolog/bips.html">[ISO directives, control constructs and builtins]</a>, <a target="_blank" href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing">[ISO Prolog Conformity Testing]</a>)</span>.</p>
    <p>What makes Tau Prolog special regarding other server-side interpreters is its <strong>integration with web pages' elements</strong>. Tau Prolog installation is quick and simple: you only need to add a Javascript file to the header of the web page in which it's intended to be run.  On <a href="\downloads">Downloads</a>, you can download a single customized file including Tau Prolog's core and the modules you may require.</p>

    <h3 id="installation"><a href="#installation">Installation</a></h3>
    <p>Once the customized Tau Prolog file is downloaded and properly located, using it is just a matter of adding a script tag on the header of a web page.</p>
	<pre class="highlight highlight-html"><code>&lt;script type="text/javascript" src="tau-prolog.js"&gt;&lt;/script&gt;</code></pre>
    <p>You can also download the sources separately and add them to the web page one by one. In that case, keep in mind that <strong>the core script must be loaded before any other Tau Prolog script</strong> since the mechanism that allows instantiating modules is defined on the core. Else, the library won't work.</p>
    <pre class="highlight highlight-html"><code>&lt;script type="text/javascript" src="tau-prolog/core.js"&gt;&lt;/script&gt;
&lt;script type="text/javascript" src="tau-prolog/lists.js"&gt;&lt;/script&gt;
...</code></pre>

    <h3 id="sessions"><a href="#sessions">Sessions</a></h3>
    <p>All Tau Prolog functionality is embedded in a Javascript object named <span class="inline-code">pl</span>, which is visible in the global scope. This <span class="inline-code">pl</span> object is located under the <span class="inline-code">window</span> object.</p>
	<p>Tau Prolog is session-oriented. A session allows you to analyse and load multiple programs and modules, as well as submit goals and queries.In order to create a new session, the library provides with the <span class="inline-code">pl.create</span> function, which returns a <span class="inline-code">pl.type.Session</span> object (every prototype implemented on Tau Prolog is defined on <span class="inline-code">pl.type</span>).</p>
    <pre class="highlight highlight-javascript"><code>var session = pl.create();</code></pre>
    <p>This function can receive an optional argument <span class="inline-code">limit</span> which limits the number of resolution steps that the interpreter can make. This way, the browser won't crash, something which could be possible if the interpreter took too much time to find an answer or if it got caught in an infinite SLD tree. If, while looking for an answer, the interpreter returns <span class="inline-code">null</span>, this means that it has reached that set limit without finding anything. Nevertheless, if you repeat the query, Tau Prolog will keep looking for an answer starting from the last choice point (where the interpreter was when it returned <span class="inline-code">null</span>). The default limit is <span class="inline-code">1000</span>.</p>
    <h3 id="load"><a href="#load">Load programs and modules</a></h3>
    <p>With the aim of analysing and loading programs in a session, the <span class="inline-code">pl.type.Session</span> prototype includes a <span class="inline-code">consult</span> method, which receives the program as a string and, if it is correct, adds the rules on it to the database, returning <span class="inline-code">true</span> afterwards.</p>
    <pre class="highlight highlight-javascript"><code>var parsed = session.consult("
	% load lists module
	:- use_module(library(lists)).

	% fruit/1
	fruit(apple). fruit(pear). fruit(banana).

	% fruits_in/2
	fruits_in(Xs, X) :- member(X, Xs), fruit(X).
"); // true</code></pre>
    <p>Now, after parsing that program, <span class="inline-code">session</span> contains three facts defining the <span class="inline-code">fruit/1</span> predicate and a rule defining the <span class="inline-code">fruits_in/2</span> predicate. <span class="inline-code">member/2</span> is a predicate from <span class="inline-code">lists</span>module, so we need to import it using the <span class="inline-code">use_module</span> directive. This way, the predicates implemented on the <span class="inline-code">lists</span> module will be available in this session.</p>
    <p>Let's suppose that we forgot to write a dot after <span class="inline-code">fruit(banana)</span>. In this case, the interpreter would load the <span class="inline-code">lists</span> and the first two <span class="inline-code">fruit/1</span> facts, but after reaching the third fact it would stop parsing and return an error. To discover if there has been an error while parsing, we must check that the returned value is strictly (<span class="inline-code">===</span>, <span class="inline-code">!==</span>) distinct from <span class="inline-code">true</span>.</p>
    <pre class="highlight highlight-javascript"><code>if( parsed !== true ) {
    console.log( parsed ); // throw(error(syntax_error(line(8), column(1), found(fruits_in), cause('. or expression expected'))))
}</code></pre>
    <p>Errors are generated as Prolog terms <span class="manual-annotation">(see <a href="/manual/prototypes-and-prolog-objects#errors">[Prototypes and Prolog objects] #Errors</a> from Tau Prolog manual)</span>, with information about where has the error been raised (line and column), the found token (if any) and the next expected character.</p>
    <h3 id="query"><a href="#query">Queries and goals</a></h3>
    <p>We can query the database to check if a goal is true or not. In order to do so, we must first add the said goal to the states pile and, later, check for facts and/or rules which satisfy that goal. The <span class="inline-code">pl.type.Session</span> prototype has a <span class="inline-code">query</span> method, which receives a goal as a string and, after adding the goal to the states pile, returns (if there were no problems) <span class="inline-code">true</span>.</p>
    <pre class="highlight highlight-javascript"><code>var parsed = session.query("fruits_in([carrot, apple, banana, broccoli], X)."); // true</code></pre>
    <p>Once the goal has been added to the pile, the <span class="inline-code">answer</span> method in <span class="inline-code">pl.type.Session</span> allows us to look for answers (facts or rules) which make the goal true. Tau Prolog is <strong>asynchronous</strong>, which means that <span class="inline-code">answer</span> will not return the results, but call a callback function if it finds something. This feature gives the Prolog predicates the ability to make asynchronous operations, as sleeping the execution or to make Ajax queries <span class="manual-annotation">(see <a href="/manual/asynchronous-predicates">[Asynchronous predicates]</a> from Tau Prolog manual)</span>. In this case, the callback is given to <span class="inline-code">answer</span> as an argument. If no callback is passed, <span class="inline-code">console.log</span> will be called.</p>
    <pre class="highlight highlight-javascript"><code>var callback = console.log;
session.answer( callback ); // {X/apple}
session.answer( callback ); // {X/banana}
session.answer( callback ); // false</code></pre>
    <p>If an answer is found, this will be returned inside a <span class="inline-code">pl.type.Substitution</span> object, where every variable of the goal is linked to a value. The <span class="inline-code">pl.type.Substitution</span> prototype includes a <span class="inline-code">toString</span> method which returns the substitution as a string in the format <span class="inline-code">{X/a, Y/b, Z/c, ...}</span> <span class="manual-annotation">(see <a href="/manual/prototypes-and-prolog-objects#substitutions">[Prototypes and Prolog objects] #Substitutions</a> from Tau Prolog manual)</span>. Another way to express a substitution as a string is the <span class="inline-code">format_answer</span>method in <span class="inline-code">pl</span>, which receives a substitution object and returns a string in the format <span class="inline-code">X = a, Y = b, Z = c, ... ;</span>, or <span class="inline-code">true ;</span> if the Substitution object has no variables.</p>
<pre class="highlight highlight-javascript"><code>var callback = function( answer ) { console.log( pl.format_answer( answer ) ); };
session.answer( callback ); // X = apple ;
session.answer( callback ); // X = banana ;
session.answer( callback ); // false.</code></pre>
    <p>If the interpreter doesn't find an answer, it will call the callback with either <span class="inline-code">false</span> or <span class="inline-code">null</span>: <span class="inline-code">false</span> if there wasn't an answer in the whole database, <span class="inline-code">null</span> if the interpreter hasn't found an answer within the resolution steps limit. If the returned value is <span class="inline-code">null</span> and you try to find an answer again, the interpreter will keep looking for answers from the point where it reached the limit last time.</p>
</div>

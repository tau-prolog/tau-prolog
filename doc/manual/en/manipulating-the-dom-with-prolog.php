<div class="match-width manual">
	<h2>Manipulating the DOM with Prolog</h2>
	<p>Tau Prolog's <span class="inline-code">dom</span> module adds new term types and predicates that allow the user to modify the DOM and to handle browser events. 
    You can find more information about these new predicates in the <a href="http://tau-prolog.org/documentation#dom"> dom module reference predicates</a>.</p>
	
	<h3 id="selectors"><a href="#selectors">Selectors</a></h3>
	<p>Tau Prolog includes three non-deterministic predicates to look for DOM elements:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_id/2">get_by_id/2</a>: look for the DOM element with the specified id.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_class/2">get_by_class/2</a>: look for all the elements (by reevaluation) which have the specified class.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_tag/2">get_by_tag/2</a>: look for all the elements (by reevaluation) with the specified tag.</li>
	</ul>
    <p>If there are no elements in the DOM with the specified identifier, class or tag, the predicates will fail.</p>
	<pre><code class="html">&lt;div id="block-a" class="circle">content a&lt;/div>
&lt;div id="block-b" class="circle">content b&lt;/div>
&lt;div id="block-c" class="square">content c&lt;/div></code></pre>
	<pre><code>var session = pl.create();
session.consult(":- use_module(library(dom)).");

session.query("get_by_tag(div, B), html(B, X).");
session.answer(); // {B/&lt;html>(block-a), X/'content a'}
session.answer(); // {B/&lt;html>(block-b), X/'content b'}
session.answer(); // {B/&lt;html>(block-c), X/'content c'}
session.answer(); // false

session.query("get_by_class(circle, B), html(B, X).");
session.answer(); // {B/&lt;html>(block-a), X/'content a'}
session.answer(); // {B/&lt;html>(block-b), X/'content b'}
session.answer(); // false</code></pre>
	<p>The <span class="inline-code">dom</span> module also includes predicates to go through the DOM starting at the HTML objects retrieved with the previous predicates.</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/parent_of/2">parent_of/2</a>: 
        If the first argument is bounded but the second one is not, the second argument will be bound to the first element's parent.
        If the second argument is bounded but the first one is not, it will be bound (by reevaluation) to all the children of second argument.
        If both arguments are bounded, the predicate checks if the first parameter is a children of the second one.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/sibling/2">sibling/2</a>: if the first argument is bounded but the second one is not, it will be bound to the first argument's most immediate right sibling.
        If the first argument is unbounded but the second one is not, the first argument will be bound to the second argument's most immediate left sibling. If both arguments are bounded, the predicate checks if they are both immediate siblings.</li>
	</ul>
	
	<h3 id="modify"><a href="#modifier">Modifying the DOM</a></h3>
	
	<p>The <a href="http://www.tau-prolog.org/documentation/prolog/dom/create/2">create/2</a> method recives an atom as a representation of a HTML tag (<span class="inline-code">div</span>, <span class="inline-code">a</span>, <span class="inline-code">table</span>, etc.) 
    and created a new HTML object. Newly created HTML objects can be inserted in the DOM using the next predicates:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/append_child/2">append_child/2</a>: inserts the second argument as the last child of the first one.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/insert_after/2">insert_after/2</a>: inserts the second argument on the same level as the first one, as its most immediate right sibling.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/insert_before/2">insert_before/2</a>: inserts the second argument on the same level as the first one, as its most immediate left sibling.</li>
	</ul>
	<p>If we try to insert an element which is already part of the DOM, the predicate fails and the element is not inserted again. In any other case, the element in inserted and the predicate is satisfied.</p>
	<p>These predicates allow you to inquire or modify the content, the attributes or the styles of an HTML object:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/attr/3">attr/3</a>: receives an HTML object and an atom standing for an 
        attribute. If the third argument is a variable, it will be bound to the value of said attribute in that HTML object.
        If the third argument is an atom as well, the specified attribute will be set with that value.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/html/2">html/2</a>: receives an HTML object. If the second argument is a variable, it will be bound to the content of the said HTML object. If the second argument is an atom, the HTML object's content will be set with that value.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/style/3">style/3</a>: receives an HTML object and an atom representing a CSS property. If the third argument is a variable, it will be bound to the value of said CSS property in that HTML object.
        If the third argument is an atom as well, the specified property will be set with that value.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/add_class/2">add_class/2</a>: add a class to an HTML object. The predicate succeeds even if the object had that class already.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/remove_class/2">remove_class/2</a>: removes the specified class from an HTML object. The predicate succeeds even if the object didn't have that class.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/has_class/2">has_class/2</a>: this predicate succeeds when the HTML object has the specified class.</li>
	</ul>
	
	<h3 id="events"><a href="#events">Events</a></h3>
	<p>Tau Prolog's <span class="inline-code">dom</span> module also enables the dynamic assignation of events, in order to run a Prolog goal when some event is triggered. The <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">bind/4</a> method receives an HTML object, an atom representing an event type (<span class="inline-code">click</span>, <span class="inline-code">mouseover</span>, <span class="inline-code">mouseout</span>, etc), an event and a goal, and bind the HTML object with said goal for that type of event. The third argument is bound to a new term that represents an HTML event, from which we can read information using the <a href="http://www.tau-prolog.org/documentation/prolog/dom/event_property/3">event_property/3</a> predicate (<span class="manual-annotation">(see <a href="http://tau-prolog.org/examples/my-little-doge">[My little doge]</a> for a complete, functional example)</span>.</p>).</p>
	
	<pre><code class="html">&lt;div id="output">&lt;/div></code></pre>
	<pre><code>var session = pl.create();
session.consult(":- use_module(library(dom)).");

session.query("get_by_id(output, Output), get_by_tag(body, B), bind(B, keypress, Event, (
	event_property(Event, key, Key),
	html(Output, Key)
)).");
session.answer(); // {Output/&lt;html>(output), Body/&lt;html>(body), Event/&lt;event>(keypress)}
</code></pre>

	<p>In this example, the <span class="inline-code">keypress</span> event has been added to the page body, so when a key is pressed, the HTML object whose id is <span class="inline-code">output</span> shows what key has been pressed. Notice that the <a href="http://www.tau-prolog.org/documentation/prolog/dom/event_property/3">event_property/3</a> predicate and the <span class="inline-code">Event</span> term only make sense inside an event's goal, since they don't hold any information until the event is triggered. Any time an event is triggered, Tau Prolog creates a new thread in the session that assigned the event and runs the goal (just for the first answer).</p>
	<p>The <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">unbind/2</a> and <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">unbind/3</a> predicates allow us to remove the events attached to an HTML object. The <a href="http://www.tau-prolog.org/documentation/prolog/dom/prevent_default/1">prevent_default/1</a> predicate allows us to prevent the browser default behaviour regarding an event (for instance, to keep a form from being sent).</p>
	<p>A list with the events supported by the browser can be read in <a href="https://developer.mozilla.org/en-US/docs/Web/Events" target="_blank">[Event reference | MDN]</a>.</p>
	
	<h3 id="effects"><a href="#effects">Effects</a></h3>
	<p>Lastly, this module also includes predicates to create animations:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/hide/1">hide/1</a>: hide the HTML object.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/show/1">show/1</a>: show the HTML object.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/style/3">toggle/1</a>: hide the HTML object if it's visible; show it otherwise.</li>
	</ul>
</div>

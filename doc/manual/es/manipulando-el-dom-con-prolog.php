<div class="match-width manual">
	<h2>Manipulando el DOM con Prolog</h2>
	<p>El módulo <span class="inline-code">dom</span> de Tau Prolog incorpora nuevos tipos de términos y predicados para la manipulación del DOM y la gestión de los eventos del navegador. Puedes encontrar más información sobre estos predicados en los <a href="http://tau-prolog.org/documentation#dom">predicados de referencia del módulo dom</a>.</p>
	
	<h3 id="selectores"><a href="#selectores">Selectores</a></h3>
	<p>Tau Prolog incluye tres predicados (no deterministas) para buscar elementos del DOM:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_id/2">get_by_id/2</a>: busca el elemento con el identificador especificado.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_class/2">get_by_class/2</a>: busca todos los elementos (por reevaluación) que contengan la clase especificada.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/get_by_tag/2">get_by_tag/2</a>: busca todos los elementos (por reevaluación) de la etiqueta especificada.</li>
	</ul>
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
	<p>Si no existe un elemento con el identificador, la clase o la etiqueta especificada, los predicados simplemente fallan silenciosamente. Además, este módulo incluye predicados para navegar por el DOM a partir de otros objetos HTML:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/parent_of/2">parent_of/2</a>: si el primer parámetro está instanciado y el segundo no, devuelve el padre del primer elemento. Si el segundo parámetro está instanciado pero el primero no, devuelve (por reevaluación) todos los hijos del segundo. Si ambos parámetros están instanciados, comprueba que el primer elemento es hijo del segundo.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/sibling/2">sibling/2</a>: si el primer parámetro está instanciado y el segundo no, devuelve el hermano inmediantamente a la derecha del primero. Si el segundo parámetro está instanciado pero el primero no, devuelve el hermano inmediatamente a la izquierda del segundo. Si ambos parámetros están instanciados, comprueba que ambos elementos son hermanos inmediatos.</li>
	</ul>
	
	<h3 id="modificar"><a href="#modificar">Modificar el DOM</a></h3>
	
	<p>El método <a href="http://www.tau-prolog.org/documentation/prolog/dom/create/2">create/2</a> recibe un átomo representando una etiqueta HTML (<span class="inline-code">div</span>, <span class="inline-code">a</span>, <span class="inline-code">table</span>, etcétera) y crea un nuevo objeto HTML. Los nuevos objetos HTML creados pueden ser insertados en el DOM mediante los siguientes predicados:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/append_child/2">append_child/2</a>: inserta el segundo elemento como último hijo del primer elemento.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/insert_after/2">insert_after/2</a>: inserta el segundo elemento justo a la derecha del primer elemento.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/insert_before/2">insert_before/2</a>: inserta el segundo elemento justo a la izquierda del primer elemento.</li>
	</ul>
	<p>Si el nuevo elemento que se intenta insertar ya pertenece al DOM, el elemento no puede ser insertado nuevamente y el predicado falla. En cualquier otro caso, el elemento es insertado y el predicado se satisface.</p>
	<p>Es posible consultar o modificar el contenido, los atributos y los estilos de un objeto HTML mediante los sigueintes predicados:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/attr/3">attr/3</a>: recibe un objeto HTML y un átomo representando un atributo. Si el tercer argumento es una variable, devuelve el valor del atributo para dicho elemento; si el tercer argumento es un átomo, establece el valor de dicho atributo.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/html/2">html/2</a>: recibe un objeto HTML. Si el segundo argumento es una variable, devuelve el contenido HTML de dicho elemento; si el segundo argumento es un átomo, establece el valor del contenido HTML.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/style/3">style/3</a>: recibe un objeto HTML y un átomo representando una propiedad CSS. Si el tercer argumento es una variable, devuelve el valor de la propiedad para dicho elemento; si el tercer argumento es un átomo, establece el valor de dicha propiedad.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/add_class/2">add_class/2</a>: añade la clase especificada al objeto HTML. El predicado tiene éxito aunque el objeto ya tenga dicha clase.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/remove_class/2">remove_class/2</a>: elimina la clase especificada del objeto HTML. El predicado tiene éxito aunque el objeto no tenga dicha clase.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/has_class/2">has_class/2</a>: tiene éxito cuando el objeto HTML tiene la clase especificada.</li>
	</ul>
	
	<h3 id="eventos"><a href="#eventos">Eventos</a></h3>
	<p>El módulo <span class="inline-code">dom</span> de Tau Prolog también permite manejar dinámicamente la asignación de eventos, para ejecutar un objetivo Prolog ante un determinado evento del navegador. El método <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">bind/4</a> recibe un objeto HTML, un átomo representando un tipo de evento (<span class="inline-code">click</span>, <span class="inline-code">mouseover</span>, <span class="inline-code">mouseout</span>, etcétera), un evento y un objetivo, y asocia al elemento HTML el objetivo especificado para dicho tipo de evento. El tercer argumento queda instanciado con un nuevo término que representa un evento HTML, del cual es posible extraer información del evento producido mediante el predicado <a href="http://www.tau-prolog.org/documentation/prolog/dom/event_property/3">event_property/3</a> <span class="manual-annotation">(véase <a href="http://tau-prolog.org/examples/my-little-doge">[My little doge]</a> para un ejemplo completo funcional)</span>.</p>
	
	<pre><code class="html">&lt;div id="output">&lt;/div></code></pre>
	<pre><code>var session = pl.create();
session.consult(":- use_module(library(dom)).");

session.query("get_by_id(output, Output), get_by_tag(body, B), bind(B, keypress, Event, (
	event_property(Event, key, Key),
	html(Output, Key)
)).");
session.answer(); // {Output/&lt;html>(output), Body/&lt;html>(body), Event/&lt;event>(keypress)}
</code></pre>

	<p>En el ejemplo anterior, se ha añadido al cuerpo de la página un evento <span class="inline-code">keypress</span> para que cuando se pulse una tecla, se indique en el objeto HTML con identificador <span class="inline-code">output</span> la tecla que se ha pulsado. Nótese que el predicado <a href="http://www.tau-prolog.org/documentation/prolog/dom/event_property/3">event_property/3</a> y el término que contiene el evento (<span class="inline-code">Event</span> en el ejemplo), sólo tienen sentido dentro del objetivo de un evento, ya que no contendrán ninguna información útil hasta que un evento sea capturado. Cada vez que un evento es capturado, Tau Prolog crea automáticamente un nuevo hilo de la sesión que asignó el evento y ejecuta el objetivo (sólo para la primera respuesta).</p>
	<p>Los predicados <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">unbind/2</a> y <a href="http://www.tau-prolog.org/documentation/prolog/dom/bind/4">unbind/3</a> permiten eliminar los eventos asociados a un objeto HTML. El predicado <a href="http://www.tau-prolog.org/documentation/prolog/dom/prevent_default/1">prevent_default/1</a> permite prevenir el comportamiento por defecto de un evento del navegador (por ejemplo, puede evitar que un formulario sea enviado).</p>
	<p>Puede consultarse una lista con los eventos soportados por el navegador en <a href="https://developer.mozilla.org/en-US/docs/Web/Events" target="_blank">[Event reference | MDN]</a>.</p>
	
	<h3 id="efectos"><a href="#efectos">Efectos</a></h3>
	<p>Por último, este módulo incluye otros predicados para crear efectos de animación:</p>
	<ul>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/hide/1">hide/1</a>: oculta el elemento HTML.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/show/1">show/1</a>: muestra el elemento HTML.</li>
		<li><a href="http://www.tau-prolog.org/documentation/prolog/dom/style/3">toggle/1</a>: oculta el elemento HTML si es visible, o lo muestra en caso contrario.</li>
	</ul>
</div>

<div class="match-width manual">
	<h2>Un tutorial sencillo</h2>
	
	<h3 id="que-es"><a href="#que-es">¿Qué es Tau Prolog?</a></h3>
	<p>Tau Prolog es un intérprete de Prolog implementado completamente en JavaScript. No es simplemente una implementación del mecanismo de resolución de la lógica de predicados, sino una implementación de Prolog guiada por el estándar ISO Prolog <span class="manual-annotation">(véase <a target="_blank" href="http://fsl.cs.illinois.edu/images/9/9c/PrologStandard.pdf">[ISO Prolog: A Summary of the Draft Proposed Standard]</a>, <a target="_blank" href="http://www.deransart.fr/prolog/bips.html">[ISO directives, control constructs and builtins]</a>, <a target="_blank" href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing">[ISO Prolog Conformity Testing]</a>)</span>.</p>
	<p>Lo que distingue a Tau Prolog de otros intérpretes ejecutados en el lado del servidor, es su capacidad de integración e interacción con los elementos de las páginas web. La instalación de Tau Prolog es muy sencilla, sólo hay que incluir un fichero JavaScript en la cabecera de la página web donde se pretende ejecutar. En la sección <a href="\downloads">Descargas</a> es posible descargar un único fichero personalizado que incluya el núcleo del intérprete junto a los módulos requeridos.</p>
	
	<h3 id="instalacion"><a href="#instalacion">Instalación</a></h3>
	<p>Una vez descargado y alojado el correspondiente fichero de Tau Prolog, simplemente hay que insertarlo en la cabecera de la página web.</p>
	<pre class="highlight highlight-html"><code>&lt;script type="text/javascript" src="tau-prolog.js"&gt;&lt;/script&gt;</code></pre>
	<p>También es posible descargar los ficheros fuente por separado e insertarlos individualmente en la página web. En ese caso, es importante cargar primero el núcleo de la biblioteca antes que cualquier otro módulo.</p>
	<pre class="highlight highlight-html"><code>&lt;script type="text/javascript" src="tau-prolog/core.js"&gt;&lt;/script&gt;
&lt;script type="text/javascript" src="tau-prolog/lists.js"&gt;&lt;/script&gt;
...</code></pre>

	<h3 id="sesiones"><a href="#sesiones">Sesiones</a></h3>
	<p>Todos los métodos de Tau Prolog están contenidos en un objeto JavaScript llamado <span class="inline-code">pl</span>, que es visible en el ámbito global.</p>
	<p>El uso de Tau Prolog está orientado a la manipulación de sesiones. Una sesión permite analizar y cargar múltiples programas y módulos, así como lanzar objetivos. Para crear una nueva sesión se provee de la función <span class="inline-code">pl.create</span>, que devuelve un objeto <span class="inline-code">pl.type.Session</span> (todos los prototipos implementados por Tau Prolog están definidos en <span class="inline-code">pl.type</span>).</p>
	<pre class="highlight highlight-javascript"><code>var session = pl.create();</code></pre>
	<p>Esta función acepta un parámetro opcional <span class="inline-code">limit</span>, que indica el número máximo de pasos de resolución que puede dar el intérprete para encontrar una respuesta. Esto evita que el navegador se bloquee, ya sea porque el intérprete tarde mucho en encontrar una respuesta, o porque haya entrado en una rama infinita. Si al buscar una respuesta el intérprete devuelve <span class="inline-code">null</span>, indica que se han ejecutado el límite establecido de pasos de resolución sin hallar ninguna respuesta; no obstante, si se vuelve a solicitar una respuesta Tau Prolog seguirá buscando desde el último punto de elección. El valor por defecto es <span class="inline-code">1000</span>.</p>
	
	<h3 id="cargar"><a href="#cargar">Cargar programas y módulos</a></h3>
	<p>Para analizar y cargar programas en una sesión, el prototipo <span class="inline-code">pl.type.Session</span> disponde del método <span class="inline-code">consult</span>, que recibe un programa en forma de cadena de caracteres y, si todo va bien, añade las reglas analizadas a la base de datos de la sesión y devuelve <span class="inline-code">true</span>.</p>
	<pre class="highlight highlight-javascript"><code>var parsed = session.consult("
	% cargar módulo lists
	:- use_module(library(lists)).

	% fruit/1
	fruit(apple). fruit(pear). fruit(banana).

	% fruits_in/2
	fruits_in(Xs, X) :- member(X, Xs), fruit(X).
"); // true</code></pre>
	<p>Ahora <span class="inline-code">session</span> contiene tres hechos que definen el predicado <span class="inline-code">fruit/1</span> y una regla que define el predicado <span class="inline-code">fruits_in/2</span>. El predicado <span class="inline-code">member/2</span> forma parte del módulo <span class="inline-code">lists</span> de Tau prolog, así que es necesario importar dicho módulo mediante la directiva <span class="inline-code">use_module</span> para incluir sus predicados en la sesión.</p>
	<p>Supongamos ahora que al escribir este último programa, se nos ha olvidado poner un punto tras el hecho <span class="inline-code">fruit(banana)</span>. El intérprete habría cargado el módulo <span class="inline-code">lists</span> y habría analizado correctamente los dos primeros hechos de <span class="inline-code">fruit/1</span>, pero en el tercero dejaría de analizar y devolvería un error. Para saber si se ha producido un error, hay que comprobar estrictamente (<span class="inline-code">===</span>, <span class="inline-code">!==</span>) que el valor devuelto sea distinto de <span class="inline-code">true</span>.</p>
	<pre class="highlight highlight-javascript"><code>if( parsed !== true ) {
    console.log( parsed ); // throw(error(syntax_error(line(8), column(1), found(fruits_in), cause('. or expression expected'))))
}</code></pre>
	<p>Los errores se devuelven en formato de término Prolog <span class="manual-annotation">(véase <a href="/manual/es/prototipos-y-objetos-prolog#errores">[Prototipos y objetos Prolog] #Errores</a> del manual de Tau Prolog)</span>, con información acerca de dónde se ha producido el error, esto es la línea y la columna, el token encontrado (si existe) y el siguiente carácter esperado.</p>
	
	<h3 id="consultar"><a href="#consultar">Consultar objetivos</a></h3>
	<p>De forma análoga a la carga de programas, para consultar objetivos en una sesión el prototipo <span class="inline-code">pl.type.Session</span> disponde del método <span class="inline-code">query</span>, que recibe un objetivo en forma de cadena de caracteres y, si todo va bien, añade el objetivo a la pila de estados de la sesión y devuelve <span class="inline-code">true</span>.</p>
	<pre class="highlight highlight-javascript"><code>var parsed = session.query("fruits_in([carrot, apple, banana, broccoli], X)."); // true</code></pre>
	<p>Una vez añadido el objetivo, el método <span class="inline-code">answer</span> de <span class="inline-code">pl.type.Session</span> permite buscar las respuestas computadas. Tau Prolog es un intérprete asíncrono, por lo tanto <span class="inline-code">answer</span> no devuelve ningún resultado, sino que ejecuta una función a modo de callback. Esta asincronía permite que los predicados Prolog realicen operaciones asíncronas, como dormir la ejecución un cierto tiempo o hacer peticiones Ajax <span class="manual-annotation">(véase <a href="/manual/es/predicados-asincronos">[Predicados asíncronos]</a> del manual de Tau Prolog)</span>.</p>
	<pre class="highlight highlight-javascript"><code>var callback = console.log;
session.answer( callback ); // {X/apple}
session.answer( callback ); // {X/banana}
session.answer( callback ); // false</code></pre>
	<p>Si se encuentra una respuesta computada, esta se devuelve en un objeto del prototipo <span class="inline-code">pl.type.Substitution</span>, donde cada variable del objetivo se liga con un valor. Este prototipo implementa el método <span class="inline-code">toString</span> para obtener una representación textual de la substitución, de la forma <span class="inline-code">{X/a, Y/b, Z/c, ...}</span> <span class="manual-annotation">(véase <a href="/manual/es/prototipos-y-objetos-prolog#substituciones">[Prototipos y objetos Prolog] #Substituciones</a> del manual de Tau Prolog)</span>. Si se prefiere una representación más amigable, el objeto <span class="inline-code">pl</span> dispone del método <span class="inline-code">format_answer</span>, que recibe una substitución y devuelve una representación textual de la forma <span class="inline-code">X = a, Y = b, Z = c, ... ;</span> o <span class="inline-code">true ;</span> cuando la substitución no contiene variables.</p>
	<pre class="highlight highlight-javascript"><code>var callback = function( answer ) { console.log( pl.format_answer( answer ) ); };
session.answer( callback ); // X = apple ;
session.answer( callback ); // X = banana ;
session.answer( callback ); // false.</code></pre>
	<p>Si no se encuentra ninguna respuesta, el intérprete ejecuta el callback con el valor <span class="inline-code">false</span>. De igual forma con <span class="inline-code">null</span> cuando se alcanza el límite de pasos de resolución.</p>
	</div>

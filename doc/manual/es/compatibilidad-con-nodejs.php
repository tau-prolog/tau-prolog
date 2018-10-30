<div class="match-width manual">
	<h2>Compatibilidad con Node.js</h2>
	<p>Tau Prolog está preparado para ser utilizado tanto en Node.js como en un navegador, sin ser necesario ningún cambio. En esta página se describen los pasos necesarios para importar tanto la biblioteca como los módulos de Tau Prolog en Node.js.</p>
	<p class="manual-warning"><b>Nota</b>: Recuerda que Node.js busca los paquetes en su repositorio de paquetes globales, a menos que la ruta se preceda de un punto para indicar que busque en el directorio actual. Por lo tanto, aunque almacenes la biblioteca de Tau Prolog en el mismo directorio, tendrás que utilizar una ruta de la forma <b>"./tau-prolog.js"</b>.</p>
	
	<h3 id="importar"><a href="#importar">Importar la biblioteca</a></h3>
	<p>Para importar la biblioteca de Tau Prolog, hay que utilizar la función <span class="inline-code">require</span> de Node.js:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );</code></pre>
	<p>Todos los métodos de Tau Prolog están contenidos en un objeto JavaScript llamado <span class="inline-code">pl</span>. Esta variable puede tomar cualquier nombre en Node.js, pero, por compatibilidad con la versión de navegador, se recomienda utilizar el nombre <span class="inline-code">pl</span>. A partir de ahora, ya se puede utilizar la biblioteca tal y como se describe en otras secciones del manual <span class="manual-annotation">(véase <a href="/manual/es/un-tutorial-sencillo">[Un tutorial sencillo]</a> del manual de Tau Prolog)</span>, exceptuando la importación de la biblioteca y los módulos de Tau Prolog.</p>
	
	<h3 id="modulos"><a href="#modulos">Cargar módulos</a></h3>
	<p>Dado que no es posible asegurar que la biblioteca de Tau Prolog esté contenida en el objeto <span class="inline-code">pl</span>, al importar un módulo se devolverá una función de carga (un <i>loader</i>) que recibirá la referencia a la biblioteca y cargará el módulo correspondiente en ella:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
var loader = require( "./path/to/tau-prolog/lists.js" );
loader( pl );</code></pre>
	<p>Una versión abreviada de esto sería:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
require( "./path/to/tau-prolog/lists.js" )( pl );</code></pre>
	<p>Con esto, se han cargado todos los predicados que exportan los módulos importados. En nuestro ejemplo, los predicados del módulo <span class="inline-code">lists</span>. Es importante tener en cuenta que en Node.js, el núcleo y cada uno de los módulos deben ser descargados e importados por separado. Por ejemplo, si queremos cargar dos módulos:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
require( "./path/to/tau-prolog/lists.js" )( pl );
require( "./path/to/tau-prolog/random.js" )( pl );</code></pre>
	<h3 id="ejemplo"><a href="#ejemplo">Ejemplo de uso</a></h3>
	<p>El siguiente código carga un pequeño programa Prolog que almacena información sobre productos y tiendas donde se venden los mismos.</p>
	<pre class="highlight highlight-js"><code>// Importar Tau Prolog y crear una sesión
var pl = require( "./path/to/tau-prolog.js" );
var session = pl.create( 1000 );

// Cargar programa
var program = 
	// Productos
	"item(id(1), name(bread))." +
	"item(id(2), name(water))." +
	"item(id(3), name(apple))." + 
	// Tiendas
	"shop(id(1), name(tau), location(spain))." +
	"shop(id(2), name(swi), location(netherlands))." +
	// Inventario
	"stock(item(1), shop(1), count(23), price(0.33))." +
	"stock(item(2), shop(1), count(17), price(0.25))." +
	"stock(item(2), shop(2), count(34), price(0.31))." +
	"stock(item(3), shop(2), count(15), price(0.45)).";
session.consult( program );

// Obtener argumento de Node.js: nodejs ./script.js item
var item = process.argv[2];

// Cargar objetivo
session.query( "item(id(ItemID), name(" + item + ")), stock(item(ItemID), shop(ShopID), _, price(Price)), shop(id(ShopID), name(Shop), _)." );

// Mostrar respuestas
session.answers( x => console.log( pl.format_answer(x) ) );</code></pre>
	<p>Este script de Node.js recibe un producto como argumento, y consulta en la base de datos el nombre de las tiendas que venden dicho producto y su precio. Si ejecutamos este programa con distintas entradas (donde <span class="inline-code">$</span> representa el prompt de la línea de comandos), obtenemos:</p>
	<pre class="highlight highlight-js"><code>$ nodejs ./script.js bread
ItemID = 1, ShopID = 1, Price = 0.33, Shop = tau ;
false.

$ nodejs ./script.js water
ItemID = 2, ShopID = 1, Price = 0.25, Shop = tau ;
ItemID = 2, ShopID = 2, Price = 0.31, Shop = swi ;
false.

$ nodejs ./script.js apple
ItemID = 3, ShopID = 2, Price = 0.45, Shop = swi ;
false.

$ nodejs ./script.js milk
false.</code></pre>
</div>

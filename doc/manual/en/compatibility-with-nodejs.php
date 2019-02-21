<div class="match-width manual">
	<h2>Compatibility with Node.js</h2>
	<p>Tau Prolog is ready to be used with either Node.js or a browser seamlessly. 
    In this page you'll find how to import the Tau Prolog library as well as the modules in a Node.js app.</p>
	<p class="manual-warning"><b>Note</b>: 
    Keep in mind that Node.js looks for packages in its global repository unless the provided path starts with a dot, which means that the path starts at that folder.
    Hence, even if you save the Tau Prolog library in the same folder as the script using it, the path to it must be something like 
    <b>"./tau-prolog.js"</b>.</p>
	
	<h3 id="import"><a href="#import">Importing the library</a></h3>
	<p>In order to import Tau Prolog, we'll use the Node.js function <span class="inline-code">require</span>:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );</code></pre>
	<p>The whole functionality of Tau Prolog is inside a Javascript object called <span class="inline-code">pl</span>. 
    This variable could have any name, but, for the sake of compatibility with the browser version, it is recommended to keep the original name,
    <span class="inline-code">pl</span>. From now on, the library can be used just as it is described in other sections of this manual 
    <span class="manual-annotation">(see <a href="/manual/a-simple-tutorial">[A simple tutorial]</a></span>), excluding the importation tasks.</p>
	
	<h3 id="modules"><a href="#modules">Loading modules</a></h3>
	<p>Since the Tau Prolog library may not be contained on the <span class="inline-code">pl</span> object, importing a module 
    (again with <span class="inline-code">require</span>) will return a <i>loader</i> function. This function receives a reference to the 
	library and loads the corresponding module on it:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
var loader = require( "./path/to/tau-prolog/lists.js" );
loader( pl );</code></pre>
	<p>A shortened version of that could be:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
require( "./path/to/tau-prolog/lists.js" )( pl );</code></pre>
	<p>This way, we've loaded all the exported predicates contained in the imported module. In the previous example, the predicates from 
	the <span class="inline-code">lists</span> module have been imported. Please acknowledge that the core of the library and the modules must be downloaded 
	and imported separately. For instance, if we wanted to load two modules:</p>
	<pre class="highlight highlight-js"><code>var pl = require( "./path/to/tau-prolog.js" );
require( "./path/to/tau-prolog/lists.js" )( pl );
require( "./path/to/tau-prolog/random.js" )( pl );</code></pre>
	<h3 id="example"><a href="#example">Example</a></h3>
	<p>The next snippet loads a simple Prolog program which keeps information about products and shops selling them.</p>
	<pre class="highlight highlight-js"><code>// Import Tau Prolog core and create a session
var pl = require( "./path/to/tau-prolog.js" );
var session = pl.create( 1000 );

// Load the program
var program = 
	// Products
	"item(id(1), name(bread))." +
	"item(id(2), name(water))." +
	"item(id(3), name(apple))." + 
	// Shops
	"shop(id(1), name(tau), location(spain))." +
	"shop(id(2), name(swi), location(netherlands))." +
	// Stock
	"stock(item(1), shop(1), count(23), price(0.33))." +
	"stock(item(2), shop(1), count(17), price(0.25))." +
	"stock(item(2), shop(2), count(34), price(0.31))." +
	"stock(item(3), shop(2), count(15), price(0.45)).";
session.consult( program );

// Get Node.js argument: nodejs ./script.js item
var item = process.argv[2];

// Query the goal
session.query( "item(id(ItemID), name(" + item + ")), stock(item(ItemID), shop(ShopID), _, price(Price)), shop(id(ShopID), name(Shop), _)." );

// Show answers
session.answers( x => console.log( pl.format_answer(x) ) );</code></pre>
	<p>This Node.js script receives a product as an argument and queries the database for shops selling said product and its price.
	If we run the script with several inputs (where <span class="inline-code">$</span> stands for the command line prompt), this is what we get:</p>
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

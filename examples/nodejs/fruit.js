// Import Tau Prolog core
// Looks like this when tau-prolog is installed with npm:
// var pl = require( "tau-prolog" );
var pl = require( "../../modules/core" );

// Import and apply the lists module
// Looks like this when tau-prolog is installed with npm:
// require( "tau-prolog/modules/lists" )( pl );
require( "../../modules/lists" )( pl );

// Create a session
var session = pl.create( 1000 );

// Load the program
var program =
  // Load the lists module
	":- use_module(library(lists))." +

	// fruit/1
  "fruit(apple). fruit(pear). fruit(banana)." +

	// fruits_in/2
	"fruits_in(Xs, X) :- member(X, Xs), fruit(X).";

session.consult( program );

// Query the goal
session.query("fruits_in([carrot, apple, banana, broccoli], X).");

// Show answers
session.answers( x => console.log( pl.format_answer(x) ) );

// Import Tau Prolog core
// Looks like this when tau-prolog is installed with npm:
// var pl = require("tau-prolog");
var pl = require("../../modules/core");

// Import the lists module
// Looks like this when tau-prolog is installed with npm:
// require("tau-prolog/modules/lists")(pl);
require("../../modules/lists")(pl);

// Create a session
var session = pl.create(1000);

// Program
var program =
	// Load the lists module
	":- use_module(library(lists))." +
	// fruit/1
	"fruit(apple). fruit(pear). fruit(banana)." +
	// fruits_in/2
	"fruits_in(Xs, X) :- member(X, Xs), fruit(X).";

// Goal
var goal = "fruits_in([carrot, apple, banana, broccoli], X).";

// Load the program
session.consult(program, {
	success: function() {
		// Query the goal
		session.query(goal, {
			success: function() {
				// Look for answers
				session.answers(x => console.log(pl.format_answer(x)));
			}
		});
	}
});
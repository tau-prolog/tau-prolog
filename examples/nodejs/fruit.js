// Import Tau Prolog core
// Looks like this when tau-prolog is installed without npm:
// var pl = require("path/to/tau-prolog/modules/core.js");
var pl = require("tau-prolog");

// Import the lists module
// Looks like this when tau-prolog is installed without npm:
// require("path/to/tau-prolog/modules/lists.js")(pl);
require("tau-prolog/modules/lists")(pl);

// Create a session
var session = pl.create();

// Program
var program = `
	:- use_module(library(lists)).

	% fruit/1
	fruit(apple).
	fruit(pear).
	fruit(banana).

	% fruits_in/2
	fruits_in(Xs, X) :-
		member(X, Xs),
		fruit(X).
`;

// Goal
var goal = "fruits_in([carrot, apple, banana, broccoli], X).";

// Load the program
session.consult(program, {
	success: function() {
		// Query the goal
		session.query(goal, {
			success: function() {
				// Look for answers
				session.answers(x => console.log(session.format_answer(x)));
			}
		});
	}
});
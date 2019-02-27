if (typeof process === 'object') {
  var pl = require('../modules/core.js')
}

// Test 1 - Parse a correct program
QUnit.test("ParseProgram - Correct program", function (assert) {
	var session = new pl.type.Session(10000);
	session.consult("append([],X,X). \
		append([H|T], X, [H|S]) :- append(T, X, S)."
	);
	assert.ok(Object.keys(session.rules).length == 1, "Program>Predicates - Number of predicates correct!");
	assert.ok(session.rules["append/3"].length, "Program>Predicate>Rules - Number of rules correct!");
	assert.ok(session.rules["append/3"][0].body === null, "Program>Rule append (1) - Body set to null!");
	assert.ok(session.rules["append/3"][0].head.indicator === "append/3", "Program>Rule append (1) - Rule is append!");
	assert.ok(session.rules["append/3"][0].head.args.length === 3, "Program>Rule append (1) has 3 arguments!");
	assert.ok(session.rules["append/3"][1].body !== null, "Program>Rule append (2) - Body is not null!");
	assert.ok(session.rules["append/3"][1].head.indicator === "append/3", "Program>Rule append (2) - Rule is append");
});

QUnit.test("ParseProgram - Program that should be read correctly (2)", function (assert) {
	var session = new pl.type.Session(10000);
	var thread = session.thread;
	session.consult(":- op(1000, yf, ++).\
		p(a,b,[1,c,2]).\
		p(c,d,[1,X|Y]).\
		q(+,+).\
		q( * , * ).\
		q( <, <).\
		r(a,(b,c)).\
		r((c,d),e).\
		r(i, (j,k)).\
		r((f,g), h)."
	);
	session.query("p(X,Y,Z).");

	assert.notEqual(thread.points.length, 0, "Program parsed");
	assert.equal(thread.head_point().goal.id, "p", "Goal is predicate 'p'");
	
	// Rules
	assert.equal(Object.keys(session.rules).length, 3, "Number of distinct predicates correct");
	assert.ok("p/3" in session.rules, "p/3 in rules");
	assert.ok("q/2" in session.rules, "q/2 in rules");
	assert.ok("r/2" in session.rules, "r/2 in rules");
	assert.notOk("q/1" in session.rules, "q/1 not in rules");
	
	// Number of rules
	assert.equal(session.rules["p/3"].length, 2, "p/3 has two rules");
	assert.equal(session.rules["q/2"].length, 3, "q/2 has three rules");
	assert.equal(session.rules["r/2"].length, 4, "r/2 has four rules");
	
	// Rules in p
	assert.equal(session.rules["p/3"][0].body, null, "First p rule's body is null");
	assert.equal(session.rules["p/3"][0].head.args.length, 3, "First p rule has three arguments");
	assert.equal(session.rules["p/3"][0].head.args[0].id, "a", "First argument in p is 'a'");
	assert.equal(session.rules["p/3"][0].head.args[1].id, "b", "Second argument in p is 'b'");
	assert.equal(session.rules["p/3"][0].head.args[2].id, ".", "Third argument in p is [](.)");

	assert.equal(session.rules["p/3"][1].head.args.length, 3, "Second p rule has three arguments");
	assert.equal(session.rules["p/3"][1].head.args[0].id, "c", "First argument in p is 'c'");
	assert.equal(session.rules["p/3"][1].head.args[1].id, "d", "Second argument in p is 'd'");
	assert.equal(session.rules["p/3"][1].head.args[2].id, ".", "Third argument in p is [](.)");
	
	// Rules in q
	assert.equal(session.rules["q/2"][0].body, null, "First q rule's body is null");
	assert.equal(session.rules["q/2"][0].head.args.length, 2, "First q rule has two arguments");
	assert.equal(session.rules["q/2"][0].head.args[0].id, "+", "First argument in q is +");
	assert.equal(session.rules["q/2"][0].head.args[1].id, "+", "Second argument in q is +");

	assert.equal(session.rules["q/2"][1].head.args.length, 2, "Second q rule has two arguments");
	assert.equal(session.rules["q/2"][1].head.args[0].id, "*", "First argument in q is *");
	assert.equal(session.rules["q/2"][1].head.args[1].id, "*", "Second argument in q is *");

	assert.equal(session.rules["q/2"][2].head.args.length, 2, "Thrid q rule has two arguments");
	assert.equal(session.rules["q/2"][2].head.args[0].id, "<", "First argument in q is <");
	assert.equal(session.rules["q/2"][2].head.args[1].id, "<", "Second argument in q is <");
	
	// Rules in r
	assert.equal(session.rules["r/2"].length, 4, "r/2 has four rules");
	assert.equal(session.rules["r/2"][0].body, null, "First r rule's body is null");
	assert.equal(session.rules["r/2"][0].head.args[0].id, "a", "First argument in r is 'a'");
	assert.equal(session.rules["r/2"][0].head.args[1].id, ",", "Second argument in r is ','");
	assert.equal(session.rules["r/2"][0].head.args[1].args[0], "b", "First argument of ',' is 'b'");
	assert.equal(session.rules["r/2"][0].head.args[1].args[1], "c", "Second argument of ',' is 'c'");

	assert.equal(session.rules["r/2"][1].body, null, "Second r rule's body is null");
	assert.equal(session.rules["r/2"][1].head.args[0].id, ",", "First argument in r is ','");
	assert.equal(session.rules["r/2"][1].head.args[0].args[0], "c", "First argument of ',' is 'c'");
	assert.equal(session.rules["r/2"][1].head.args[0].args[1], "d", "Second argument of ',' is 'd'");
	assert.equal(session.rules["r/2"][1].head.args[1].id, "e", "Second argument in r is 'e'");

	assert.equal(session.rules["r/2"][2].body, null, "Third r rule's body is null");
	assert.equal(session.rules["r/2"][2].head.args[0].id, "i", "First argument in r is 'i'");
	assert.equal(session.rules["r/2"][2].head.args[1].id, ",", "Second argument in r is ','");
	assert.equal(session.rules["r/2"][2].head.args[1].args[0], "j", "First argument of ',' is 'j'");
	assert.equal(session.rules["r/2"][2].head.args[1].args[1], "k", "Second argument of ',' is 'k'");

	assert.equal(session.rules["r/2"][3].body, null, "Fourth r rule's body is null");
	assert.equal(session.rules["r/2"][3].head.args[0].id, ",", "First argument in r is ','");
	assert.equal(session.rules["r/2"][3].head.args[0].args[0], "f", "First argument of ',' is 'f'");
	assert.equal(session.rules["r/2"][3].head.args[0].args[1], "g", "Second argument of ',' is 'g'");
	assert.equal(session.rules["r/2"][3].head.args[1].id, "h", "Second argument in r is 'h'");
});

// TODO: When Parser Errors are coded, this test must be adapted
// Test 2 - Parse an incorrect program (missing dot)
QUnit.test("ParseProgram - Incorrect program (missing dot)", function (assert) {
	var session = new pl.type.Session(10000);
	session.consult("append([],X,X) \
		append([H|T], X, [H|S]) :- append(T, X, S). \
		member(X,[X|Xs]). \
		member(X,[_|Xs]) :- member(X,Xs)."
	);
	assert.ok(Object.keys(session.rules).length == 0 && session.rules.constructor == Object, "Program not parsed > 'rules' property is empty");
});

// Test 3 - Parse an incorrect program (bad syntax)
QUnit.test("ParseProgram - Incorrect program (bad syntax)", function (assert) {
	var session = new pl.type.Session(10000);
	session.consult("append([]X,X). \
		append([H|T], X, [H|S]) :- append(T, X, S). \
		member(X,[X|Xs]). \
		member(X,[_|Xs]) :- member(X,Xs)."
	);
	assert.ok(Object.keys(session.rules).length == 0 && session.rules.constructor == Object, "Program not parsed > 'rules' property is empty");
});

// Test 4 - Parse goal
QUnit.test("ParseGoal - Correct goal", function (assert) {
	var session = new pl.type.Session(10000);
	var thread = session.thread;
	session.consult("append([],X,X). \
		append([H|T], X, [H|S]) :- append(T, X, S). \
		member(X,[X|Xs]). \
		member(X,[_|Xs]) :- member(X,Xs)."
	);
	assert.ok(thread.points.length == 0, "Program before parsing goal > Substitution points empty !");
	session.query("append(X,Y,[1,2,3]).");
	assert.ok(thread.points.length != 0, "Program after parsing goal > Substitution points are not empty !");
	var subs_point = thread.head_point();
	assert.ok(pl.type.is_term(subs_point.goal), "Substitation point has goal");
	assert.ok(pl.type.is_substitution(subs_point.substitution), "Substitation point has substitution");
	var goal = subs_point.goal;
	assert.equal(goal.id, "append", "Goal id is append");
	assert.equal(goal.indicator, "append/3", "Goal indicator is append/3");
	assert.equal(goal.args.length, 3, "Length of arguments is 3");
	assert.ok(pl.type.is_variable(goal.args[0]), "First argument is Variable");
	assert.equal(goal.args[0].id, "X", "First argument - Correct id (X)");
	assert.ok(pl.type.is_variable(goal.args[1]), "Second argument is Variable");
	assert.equal(goal.args[1].id, "Y", "Second argument - Correct id (Y)");
	assert.ok(pl.type.is_term(goal.args[2]), "Third argument is Term");
	assert.equal(goal.args[2].id, ".", "Third argument - Correct id (.)");
	assert.equal(goal.args[2].args.length, 2, "Third argument - Correct number of arguments");
});

// Test 5 - Answer
QUnit.test("Answer - Complete execution of testing program (append)", function (assert) {
	var session = new pl.type.Session(10000);
	session.consult("append([],X,X). \
		append([H|T], X, [H|S]) :- append(T, X, S). \
		member(X,[X|Xs]). \
		member(X,[_|Xs]) :- member(X,Xs)."
	);
	session.query("append(X,Y,[1,2,3]).");
	session.answer(function (answer) {
		assert.ok(Object.keys(answer.links).length == 2, "Answer > Links - Number of elements correct !");
		assert.equal(answer.links.X.id, "[]", "Answer (1) > Links > X - Correct id ([]) !");
		assert.equal(answer.links.Y.id, ".", "Answer (1) > Links > Y - Correct id (.) !");
		assert.equal(answer.links.Y.args[0].value, 1, "Answer (1) > Links > Y.args[0] - Correct value (1) !");
		assert.equal(answer.links.Y.args[1].id, ".", "Answer (1) > Links > Y.args[1] - Correct id (.) !");
		assert.equal(answer.links.Y.args[1].args[0].value, 2, "Answer (1) > Links > Y.args[1].args[0] - Correct value (2) !");
		assert.equal(answer.links.Y.args[1].args[1].id, ".", "Answer (1) > Links > Y.args[1].args[1] - Correct id (.) !");
		assert.equal(answer.links.Y.args[1].args[1].args[0].value, 3, "Answer (1) > Links > Y.args[1].args[1].args[0] - Correct value (3) !");
		assert.equal(answer.links.Y.args[1].args[1].args[1].id, "[]", "Answer (1) > Links > Y.args[1].args[1].args[1] - Correct id ([]) !");
	});
	session.answer(function (answer) {
		assert.equal(answer.links.X.args[0].value, 1, "Answer (2) > Links > X.args[0] - Correct value (1) !");
		assert.equal(answer.links.X.args[1].id, "[]", "Answer (2) > Links > X.args[1] - Correct id ([]) !");
		assert.equal(answer.links.X.id, ".", "Answer (2) > Links > X - Correct id (.) !");
		assert.equal(answer.links.X.args[0].value, 1, "Answer (2) > Links > X.args[0] - Correct value (1) !");
		assert.equal(answer.links.X.args[1].id, "[]", "Answer (2) > Links > X.args[1] - Correct id ([]) !");
		assert.equal(answer.links.Y.id, ".", "Answer (2) > Links > Y - Correct id (.) !");
		assert.equal(answer.links.Y.args[0].value, 2, "Answer (2) > Links > Y.args[0] - Correct value (2) !");
		assert.equal(answer.links.Y.args[1].id, ".", "Answer (2) > Links > Y.args[1] - Correct id (.) !");
		assert.equal(answer.links.Y.args[1].args[0].value, 3, "Answer (2) > Links > Y.args[1].args[0] - Correct value (3) !");
		assert.equal(answer.links.Y.args[1].args[1].id, "[]", "Answer (2) > Links > Y.args[1].args[1] - Correct id ([]) !");
	});
	session.answer(function (answer) {
		assert.equal(answer.links.X.id, ".", "Answer (3) > Links > X - Correct id (.) !");
		assert.equal(answer.links.X.args[0].value, 1, "Answer (3) > Links > X.args[0] - Correct value (1) !");
		assert.equal(answer.links.X.args[1].id, ".", "Answer (3) > Links > X.args[1] - Correct id (.) !");
		assert.equal(answer.links.X.args[1].args[0].value, 2, "Answer (3) > Links > X.args[1].args[0] - Correct value (2) !");
		assert.equal(answer.links.X.args[1].args[1].id, "[]", "Answer (3) > Links > X.args[1].args[1] - Correct id ([]) !");
		assert.equal(answer.links.Y.id, ".", "Answer (3) > Links > Y - Correct id (.) !");
		assert.equal(answer.links.Y.args[0].value, 3, "Answer (3) > Links > Y.args[0] - Correct value (3) !");
		assert.equal(answer.links.Y.args[1].id, "[]", "Answer (3) > Links > Y.args[1] - Correct id ([]) !");
	});
	session.answer(function (answer) {
		assert.equal(answer.links.X.id, ".", "Answer (4) > Links > Y - Correct id (.) !");
		assert.equal(answer.links.X.args[0].value, 1, "Answer (4) > Links > Y.args[0] - Correct value (1) !");
		assert.equal(answer.links.X.args[1].id, ".", "Answer (4) > Links > Y.args[1] - Correct id (.) !");
		assert.equal(answer.links.X.args[1].args[0].value, 2, "Answer (4) > Links > Y.args[1].args[0] - Correct value (2) !");
		assert.equal(answer.links.X.args[1].args[1].id, ".", "Answer (4) > Links > Y.args[1].args[1] - Correct id (.) !");
		assert.equal(answer.links.X.args[1].args[1].args[0].value, 3, "Answer (4) > Links > Y.args[1].args[1].args[0] - Correct value (3) !");
		assert.equal(answer.links.X.args[1].args[1].args[1].id, "[]", "Answer (4) > Links > Y.args[1].args[1].args[1] - Correct id ([]) !");
		assert.equal(answer.links.Y.id, "[]", "Answer (4) > Links > X - Correct id ([]) !");
	});
	session.answer(function (answer) {
		assert.equal(answer, false, "Anwser (5) > False - There is no more answer.");
	});
});

QUnit.test("Answer - Limite reached ", function (assert) {
	var session = new pl.type.Session(1000);
	session.consult("p(X) :- p(X).");
	session.query("p(a).");
	session.answer(function (answer) {
		assert.equal(answer, null, "Answer is null !");
	});
});

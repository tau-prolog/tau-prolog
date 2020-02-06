/***********************************
 * Program to be tested
	:- op(1000, yf, ++).
	p(a,b,[1,c,2]).
	p(c,d,[1,X|Y]).
	q(+,+).
	q( * , * ).
	q( <, <).
	r(a,(b,c)).
	r((c,d),e).
	r(i, (j,k)).
	r((f,g), h).
 * Predicates
 * X is 10+20.
 * X is 2*3+6.
 * X is 2*3+6.
 * X is 2 -5.
 * X is 2-5.
 * X is 1+2+3+4.
 * X is 2**3.
 * X is 2**3**4.
 * p(X,Y,Z).
 * q(X,Y).
 * p(X,Y,Z).
 * r(X,Y).
 * X = (a++).
 * X = (a++ ++).
 * X = a++ .
***********************************/

var nodejs_flag = typeof module !== 'undefined' && module.exports !== undefined;
if (typeof process === 'object') {
  var pl = require('../modules/core.js')
}

var programToBeTested = (
  ":- op(1000, yf, ++).\
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

// TEST CASES

testGoal('X is 10+20.', function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "+", "Second 'is' argument is '+/2'");
  session.answer(function(success) {
    assert.equal(success.links.X.value, 30, "X is 30");
  });
});

testGoal("X is 2*3+6.", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "+", "Second 'is' argument is '+/2'");
  assert.equal(thread.head_point().goal.args[1].args[0].id, "*", "First '+' argument is '*/2'");
  session.answer(function(success) {
    assert.equal(success.links.X.value, 12, "X is 12");
  });
});

testGoal("X is 2*(3+6).", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "*", "Second 'is' argument is '*/2'");
  assert.equal(thread.head_point().goal.args[1].args[1].id, "+", "First '*' argument is '+/2'");
  session.answer(function(success) {
    assert.equal(success.links.X.value, 18, "X is 18");
  });
});

testGoal("X is 2 -5.", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "-", "Second 'is' argument is '-/2'");
  session.answer(function(success) {
    assert.equal(success.links.X.value, -3, "X is -3");
  });
});

testGoal("X is 2-5.", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "-", "Second 'is' argument is '-/2'");
  session.answer(function(success) {
    assert.equal(success.links.X.value, -3, "X is -3");
  });
});

testGoal("X is 1+2+3+4.", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "+", "Second 'is' argument is '+/2'");
  assert.equal(thread.head_point().goal.args[1].args[1], 4, "Second '+' argument is 4");
  assert.equal(thread.head_point().goal.args[1].args[0].id, "+", "First '+' argument is Term (+/2)");
  assert.equal(thread.head_point().goal.args[1].args[0].args[1], 3, "Second '+' argument is 3");
  session.answer(function(success) {
    assert.equal(success.links.X.value, 10, "X is 10");
  });
});

testGoal("X is 2**3.", function(assert, session, thread) {
  assert.equal(thread.head_point().goal.id, "is", "First term is 'is/2'");
  assert.ok(pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
  assert.equal(thread.head_point().goal.args[1].id, "**", "Second 'is' argument is '**/2'");
  assert.equal(thread.head_point().goal.args[1].args[0], 2, "First '**' argument is 2");
  assert.equal(thread.head_point().goal.args[1].args[1], 3, "Second '**' argument is 3");
  session.answer(function(success) {
    assert.equal(success.links.X.value, 8, "X is 8");
  });
});

testGoal("X is 2**3**4.", function(assert, session, thread) {
  assert.equal(thread.points.length, 0, "Program not parsed");
});

testGoal(
  "p(X,Y,Z).", 
  programToBeTested,
  function(assert, session, thread) {
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
    assert.equal(session.rules["q/2"][1].head.args[0].id, "*", "First argument in p is *");
    assert.equal(session.rules["q/2"][1].head.args[1].id, "*", "Second argument in p is *");
    assert.equal(session.rules["q/2"][2].head.args.length, 2, "Second q rule has two arguments");
    assert.equal(session.rules["q/2"][2].head.args[0].id, "<", "First argument in p is <");
    assert.equal(session.rules["q/2"][2].head.args[1].id, "<", "Second argument in p is <");

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

    session.answer(function(success) {
      assert.equal(success.links.X.id, "a", "X is a");
      assert.equal(success.links.Y.id, "b", "Y is b");
      assert.equal(success.links.Z.id, ".", "Z is a list ('.')");
      assert.equal(success.links.Z.args[0].value, 1, "First element in list is 1");
      assert.equal(success.links.Z.args[1].args[0].id, "c", "Second element in list is term (c)");
      assert.equal(success.links.Z.args[1].args[1].args[0].value, 2, "Third element in list is 2");
    });

    session.answer(function(success) {
      assert.equal(success.links.X.id, "c", "X is c");
      assert.equal(success.links.Y.id, "d", "Y is d");
      assert.equal(success.links.Z.id, ".", "Z is a list ('.')");
      assert.equal(success.links.Z.args[0].value, 1, "First element in list is 1");
      assert.equal(success.links.Z.args[1].args[0].id, "_1", "Second element in list is '_1'");
      assert.equal(success.links.Z.args[1].args[1].id, "_2", "Third element in list is '_2'");
    });
  }
);

testGoal(
  "q(X,Y).",
  programToBeTested,
  function(assert, session) {
    // First call
    session.answer(function(success) {
      assert.equal(success.links.X.id, "+", "X is +");
      assert.equal(success.links.Y.id, "+", "Y is +");
    });
    // Second call
    session.answer(function(success) {
      assert.equal(success.links.X.id, "*", "X is *");
      assert.equal(success.links.Y.id, "*", "Y is *");
    });
    // Third call
    session.answer(function(success) {
      assert.equal(success.links.X.id, "<", "X is <");
      assert.equal(success.links.Y.id, "<", "Y is <");
    });
  }
);

testGoal(
  "r(X,Y).",
  programToBeTested,
  function(assert, session) {

    session.answer(function(success) {
      // console.log(success);
      assert.equal(success.links.X.id, "a", "X is a");
      assert.equal(success.links.Y.id, ",", "Y is a couple (',')");
      assert.equal(success.links.Y.args[0].id, "b", "First element in couple is b");
      assert.equal(success.links.Y.args[1].id, "c", "Second element in couple is c");
    });

    session.answer(function(success) {
      // console.log(success);
      assert.equal(success.links.X.id, ",", "X is a couple (',')");
      assert.equal(success.links.X.args[0].id, "c", "First element in couple is c");
      assert.equal(success.links.X.args[1].id, "d", "Second element in couple is d");
      assert.equal(success.links.Y.id, "e", "Y is e");
    });
    
    session.answer(function(success) {
      // console.log(success);
      assert.equal(success.links.X.id, "i", "X is i");
      assert.equal(success.links.Y.id, ",", "Y is a couple (',')");
      assert.equal(success.links.Y.args[0].id, "j", "First element in couple is j");
      assert.equal(success.links.Y.args[1].id, "k", "Second element in couple is k");
    });
    
    session.answer(function(success) {
      // console.log(success);
      assert.equal(success.links.X.id, ",", "X is a couple (',')");
      assert.equal(success.links.X.args[0].id, "f", "First element in couple is f");
      assert.equal(success.links.X.args[1].id, "g", "Second element in couple is g");
      assert.equal(success.links.Y.id, "h", "Y is h");
    });
  }
);



// TEST UTILS

function testGoal(goal, program, testFn) {
  
  function runTest(goal, session, testFn, postfix) {
    var title = "Goal '" + goal + "'" + (postfix || '');
    QUnit.test(title, function(assert){
      testFn(assert, session, session.thread);
    });
  }

  if(typeof program === 'function' && typeof testFn === 'undefined') {
    testFn = program;
    program = undefined;
  }

  if(program){
    var session = withProgram(program, goal);
    runTest(goal, session, testFn);
    
    if(nodejs_flag) {
      var session2 = withProgram(tmpProgramFilename(program), goal);
      runTest(goal, session2, testFn, ' -file');
    }
  } else {
    var session = withoutProgram(goal);
    runTest(goal, session, testFn);
  }

}

function withProgram (program, query) {
  var session = new pl.type.Session( 10000 );
  session.consult(program);
  session.query(query);
  return session;
}

function withoutProgram (query) {
  var session = new pl.type.Session( 10000 );
  session.query(query);
  return session;
}

function tmpProgramFilename (program) {
  var tmpFolder = '/tmp';
  var fs = require('fs');
  var tmpFilename = tmpFolder + '/pl_program_' + new Date().getTime();
  fs.writeFileSync(tmpFilename, program, 'utf8');
  return tmpFilename;
}
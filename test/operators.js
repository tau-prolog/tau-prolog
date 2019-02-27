if (typeof process === 'object') {
  var pl = require('../modules/core.js')
}

QUnit.test("Operation 'X is 4 + 5 - 9'", function(assert){
    var session = new pl.type.Session( 10000 );
    var thread = session.thread;
    session.query("X is 4 + 5 - 9.");
    assert.ok( pl.type.is_term(thread.head_point().goal), "First point is Term");
    assert.equal( thread.head_point().goal.id, "is", "First term is 'is/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
    assert.equal( thread.head_point().goal.args[1].id, "-", "Second 'is' argument is '-/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1]), "First '-' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].id, "+", "First '-' argument is '+/2'");
    assert.equal( thread.head_point().goal.args[1].args[1].value, 9, "Second '-' argument is 9");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0], 4, "First '+' argument is 4");
    assert.equal( thread.head_point().goal.args[1].args[0].args[1], 5, "First '+' argument is 5");
    session.answer( function(success){
        // console.log(success);
        assert.equal( success.links.X.value, 0, "X is 0");
    });
});

QUnit.test("Operation 'X is 4 + (5 - 9)'", function(assert){
    var session = new pl.type.Session( 10000 );
    var thread = session.thread;
    session.query("X is 4 + (5 - 9).");
    assert.ok( pl.type.is_term(thread.head_point().goal), "First point is Term");
    assert.equal( thread.head_point().goal.id, "is", "First term is 'is/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
    assert.equal( thread.head_point().goal.args[1].id, "+", "Second 'is' argument is '+/2'");
    assert.equal( thread.head_point().goal.args[1].args[0].value, 4, "First '+' argument is 4");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[1]), "Second '+' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[1].id, "-", "Second '+' argument is '-/2'");
    assert.equal( thread.head_point().goal.args[1].args[1].args[0], 5, "First '-' argument is 5");
    assert.equal( thread.head_point().goal.args[1].args[1].args[1], 9, "First '-' argument is 9");
    session.answer( function(success){
        // console.log(success);
        assert.equal( success.links.X.value, 0, "X is 0");
    });
});

QUnit.test("Operation 'X is 10 + 7 \\/ 6 - 1 /\\ 15 + 3'", function(assert){
    var session = new pl.type.Session( 10000 );
    var thread = session.thread;
    session.query("X is 10 + 7 \\/ 6 - 1 /\\ 15 + 3.");
    assert.ok( pl.type.is_term(thread.head_point().goal), "First point is Term");
    assert.equal( thread.head_point().goal.id, "is", "First term is 'is/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
    assert.equal( thread.head_point().goal.args[1].id, "+", "Second 'is' argument is '+/2'");
    assert.equal( thread.head_point().goal.args[1].args[1].value, 3, "Second '+' argument is 3");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0]), "Second '+' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].id, "/\\", "Second '+' argument is '/\\/2'");
    assert.equal( thread.head_point().goal.args[1].args[0].args[1].value, 15, "Second '/\\' argument is 15"); 
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0].args[0]), "First '/\\' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].id, '-', "First '/\\' argument is '-/2'");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[1].value, 1, "Second '-' argument is 1");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0].args[0].args[0]), "First '-' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].id, '\\/', "First '-' argument is '\\/'");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].args[1].value, 6, "Second '\\/' argument is 6");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0].args[0].args[0].args[0]), "First '\\/' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].args[0].id, '+', "First '\\/' argument is '+/2'");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].args[0].args[0], 10, "First '+' argument is 10");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].args[0].args[1], 7, "First '+' argument is 7");
    session.answer( function(success){
        // console.log(success);
        assert.equal( success.links.X.value, 9, "X is 9");
    });
});

QUnit.test("Operation 'X is (10 + 7) \\/ (6 - 1) /\\ (15 + 3)'", function(assert){
    var session = new pl.type.Session( 10000 );
    var thread = session.thread;
    session.query("X is (10 + 7) \\/ (6 - 1) /\\ (15 + 3).");
    assert.ok( pl.type.is_term(thread.head_point().goal), "First point is Term");
    assert.equal( thread.head_point().goal.id, "is", "First term is 'is/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1]), "Second 'is' argument is Term");
    assert.equal( thread.head_point().goal.args[1].id, "/\\", "Second 'is' argument is '/\\/2'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0]), "First '/\\' argument is Term");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[1]), "Second '/\\' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].id, '\\/', "First '/\\' term id is '\\/'");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0].args[0]), "First '\\/' argument is Term");
    assert.ok( pl.type.is_term(thread.head_point().goal.args[1].args[0].args[1]), "Second '\\/' argument is Term");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].id, '+', "First '\\/' term is '+/2");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[0].value, 10, "First '+' term is 10");
    assert.equal( thread.head_point().goal.args[1].args[0].args[0].args[1].value, 7, "First '+' term is 7");
    assert.equal( thread.head_point().goal.args[1].args[0].args[1].id, '-', "Second '\\/' term is '-/2'");
    assert.equal( thread.head_point().goal.args[1].args[0].args[1].args[0].value, 6, "Second '-' term is 6");
    assert.equal( thread.head_point().goal.args[1].args[0].args[1].args[1].value, 1, "Second '-' term is 1");
    assert.equal( thread.head_point().goal.args[1].args[1].id, '+', "Second '/\\' term id is '+'");
    assert.ok( thread.head_point().goal.args[1].args[1].args[0], 15, "First '+' argument is 15");
    assert.ok( thread.head_point().goal.args[1].args[1].args[1], 3, "Second '+' argument is 3");
    session.answer( function(success){
        // console.log(success);
        assert.equal( success.links.X.value, 16, "X is 16");
    });
});

/*
 * QUnit.test("Operation ''", function(assert){
    
});
* */

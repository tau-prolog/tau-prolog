if (typeof process === 'object') {
  var pl = require('../modules/core.js');
}

QUnit.test("query_all variables", async function(assert){
  const session = pl.create();
  session.consult(`
    foo(a). foo(b). foo(c).
    bar(x). bar(b). bar(c).
  `);
  const solutions = await session.query_all`foo(X), bar(X).`;
  assert.deepEqual(solutions, [ { X: 'b' }, { X: 'c' } ]);
});

QUnit.test("query_all no variables", async function(assert){
  const session = pl.create();
  session.consult(`
    foo(x).
  `);
  const solutions = await session.query_all`foo(x).`;
  assert.deepEqual(solutions, [ {} ]);

  const no_solutions = await session.query_all`foo(y).`;
  assert.deepEqual(no_solutions, [ ]);
});

QUnit.test("query_all escaping", async function(assert){
  const session = pl.create();
  session.consult(`
    path([one]).
    path([two, one, 'an end']).
    path([three, two, one, 'an end']).
  `);
  const end = ['one', 'an end'];
  const solutions = await session.query_all`path([X | ${end}]).`;
  assert.deepEqual(solutions, [ { X: 'two' } ]);
});

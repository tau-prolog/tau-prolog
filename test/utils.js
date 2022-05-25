// TEST UTILS

const isNodeJs = typeof module !== 'undefined' && module.exports !== undefined;

function testGoal (goal, testFn, maybeProgram) {
  function runTest (goal, session, testFn, postfix) {
    const title = "Goal '" + goal + "'" + (postfix || '');
    QUnit.test(title, function (assert) {
      testFn(assert, session, session.thread);
    });
  }

  const session = new pl.type.Session(10000);

  if (maybeProgram) {
    withProgram(session, maybeProgram, goal);
    runTest(goal, session, testFn);

    if (isNodeJs) {
      const session = new pl.type.Session(10000);
      withProgram(session, tmpProgramFilename(maybeProgram), goal);
      runTest(goal, session, testFn, ' -file');
    }
  } else {
    session.query(goal);
    runTest(goal, session, testFn);
  }
}

function withProgram (session, program, query) {
  session.consult(program);
  session.query(query);
}

function tmpProgramFilename (program) {
  const tmpFolder = '/tmp';
  const fs = require('fs');
  const tmpFilename = tmpFolder + '/pl_program_' + new Date().getTime();
  fs.writeFileSync(tmpFilename, program, 'utf8');
  return tmpFilename;
}

function checkAnswers (session, assert, checks) {
  const done = assert.async(checks.length);
  for (const i in checks) {
    session.answer(function (ans) {
      checks[i](assert, ans);
      done();
    });
  }
}

if (isNodeJs) {
  exports.testGoal = testGoal;
  exports.checkAnswers = checkAnswers;
  exports.isNodeJs = isNodeJs;
}

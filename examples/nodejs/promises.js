const pl = require("../../modules/core");
require("../../modules/promises")(pl);

(async() => {

    const program = `
        plus(z, Y, Y).
        plus(s(X), Y, s(Z)) :- plus(X, Y, Z).
    `;
    const goal = "plus(X, Y, s(s(s(z)))).";
    const session = pl.create();
    await session.promiseConsult(program);
    await session.promiseQuery(goal);

    for await (let answer of session.promiseAnswers())
        console.log(session.format_answer(answer));
    // X = z, Y = s(s(s(z))) ;
    // X = s(z), Y = s(s(z)) ;
    // X = s(s(z)), Y = s(z) ;
    // X = s(s(s(z))), Y = z.

})();
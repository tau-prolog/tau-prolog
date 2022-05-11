var readlineSync = require("readline-sync");
var pl = require("./modules/core.js");
require("./modules/charsio.js")(pl);
require("./modules/format.js")(pl);
require("./modules/js.js")(pl);
require("./modules/lists.js")(pl);
require("./modules/os.js")(pl);
require("./modules/random.js")(pl);
require("./modules/statistics.js")(pl);

var options = {};
var args = process.argv.slice(2);
for(var i = 0; i < args.length; i++) {
    if((args[i] === "-g" || args[i] === "--goal") && args[i+1]) {
        options.goal = args[i+1];
        i++;
    }
}

var session = pl.create(0);
const compose = (f,g) => x => f(g(x));
const show = function(answer) {
    console.log(pl.format_answer(answer, session, {quoted: true}));
};
const action = function(callback) {
    var input = readlineSync.question("", {keepWhitespace: true}).trim();
    switch(input) {
        // next answer
        case ";":
            session.answer(next_answer(callback));
            break;
        // break
        case "":
            callback();
            break;
        // help
        case "h":
            console.log("Actions: ");
            console.log(";\tredo");
            console.log("RET\tbreak");
            console.log("h\thelp");
            action(callback);
            break;
        // unknown
        default:
            console.log("Unknown action: " + input + " (h for help)");
            action(callback);
            break;
    }
};
const next_answer = function(callback) {
    return function(answer) {
        show(answer);
        if(answer === false || pl.type.is_error(answer))
            callback();
        else
            action(callback);
    };
};
const query = function(goal, callback) {
    if(goal.trim().length === 0) {
        callback();
        return;
    }
    session.query(goal, {success: function(_goal) {
        session.answer(next_answer(callback));
    }, error: compose(repl, show)});
};
const repl = function() {
    var goal = readlineSync.question("?- ", {keepWhitespace: true}) + "\n";
    query(goal, repl);
};

if(options.goal)
    query(options.goal, repl);
else
    repl();
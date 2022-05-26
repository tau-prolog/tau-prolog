var readlineSync = require("readline-sync");
var fs = require("fs");
var pl = require("./modules/core.js");
require("./modules/charsio.js")(pl);
require("./modules/format.js")(pl);
require("./modules/js.js")(pl);
require("./modules/lists.js")(pl);
require("./modules/os.js")(pl);
require("./modules/random.js")(pl);
require("./modules/statistics.js")(pl);

// Default options.
var options = {
    goal: null,
    limit: 0
};

// Read command line options.
var args = process.argv.slice(2);
for(var i = 0; i < args.length; i++) {
    // initial goal
    if((args[i] === "-g" || args[i] === "--goal") && args[i+1]) {
        options.goal = args[i+1];
        i++;
    }
    // limit resolution steps
    if((args[i] === "-l" || args[i] === "--limit") && args[i+1]) {
        options.limit = parseInt(args[i+1], 10);
        if(isNaN(options.limit))
            options.limit = 0;
        i++;
    }
}

// Create session.
const session = pl.create(options.limit);

// Start REPL.
if(options.goal)
    query(options.goal, repl);
else
    repl();

// Function composition.
function compose(f, g) {
    return function(x) {
        return f(g(x));
    };
}

// Write to the standard output.
function write(text) {
    fs.writeSync(process.stdout.fd, text);
}

// Write a formated answer to the standard output.
function show(answer) {
    write(pl.format_answer(answer, session, {quoted: true}));
}

// Manage the REPL actions.
function action(callback) {
    var input = readlineSync.question(" ", {keepWhitespace: true}).trim();
    switch(input) {
        // next answer
        case ";":
            session.answer(next_answer(callback));
            break;
        // break
        case "":
            write("\n");
            callback();
            break;
        // help
        case "h":
            write("Actions: \n");
            write(";\tredo\n");
            write("RET\tbreak\n");
            write("h\thelp\n");
            action(callback);
            break;
        // unknown
        default:
            write("Unknown action: " + input + " (h for help)\n");
            action(callback);
            break;
    }
}

// Look for the next answer in the REPL.
function next_answer(callback) {
    return function(answer) {
        show(answer);
        if(answer === false || pl.type.is_error(answer) || session.thread.points.length === 0) {
            write(".\n\n");
            callback();
        } else {
            action(callback);
        }
    }
}

// Query a goal in the REPL.
function query(goal, callback) {
    if(goal.trim().length === 0) {
        callback();
        return;
    }
    session.query(goal, {
        success: function(_goal) { session.answer(next_answer(callback)); },
        error: compose(repl, compose(function(){ write(".\n\n"); }, show))
    });
}

// Start the read–eval–print loop.
function repl() {
    var goal = readlineSync.question("?- ", {keepWhitespace: true}) + "\n";
    query(goal, repl);
}
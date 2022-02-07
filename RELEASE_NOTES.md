# Release notes

## Version 0.3.2 - Febrary 7, 2022

### Core (0.3.2)

* ADDED: hyperbolic arithmetic functions.
* FIXED: `op/3` directive permission error with empty list.
* FIXED: writing curly bracketed terms in canonical format.
* FIXED: scope of calls in `Thread.prototype.again` method.
* ADDED: `compare` method for `Stream` objects.
* FIXED: writing with `quoted` option for multiline comment tokens `/*{...}`.
* FIXED: exception when raising zero to a negative power.
* FIXED: DCG's in user-defined modules.
* FIXED: `reconsult` option for user-defined modules.
* ADDED: check for integer overflow in parser.

### Module `system` (0.3.2)

* FIXED: `op/3` built-in predicate permission error with empty list.
* FIXED: `findall/4` built-in predicate type-checking of the tail argument.
* ADDED: `atomic_concat/3` built-in predicate.
* FIXED: `listing/[0-1]` built-in predicates in user-defined modules.
* FIXED: variable renaming in all solutions built-in predicates (`findall/[3-4]`, `setof/3` and `bagof/3`).
* FIXED: `get_byte/2` built-in predicate in Node.js.

### Module `js` (0.3.2)

* ADDED: `new/3` predicate to create instances of JavaScript objects.
* ADDED: `set_prop/[2-3]` predicates to set the value of a property on a JavaScript object. 
* ADDED: `get_prop/[2-3]` predicates to retrieve the value of a property on a JavaScript object.
* DEPRECATED: `prop/[2-3]` predicates (use `get_prop/[2-3]` instead).

### Module `format` (0.3.2)

* UPDATED: to the latest version (see [scryer-prolog/src/lib/format.pl](https://github.com/mthom/scryer-prolog/blob/master/src/lib/format.pl)).

### Acknowledgments (0.3.2)

* Thanks to **Paulo Moura** for his help in detecting and correcting errors and for adding new predicates to the `system` module.
* Thanks to **Markus Triska** for his updates on the `format` module.

## Version 0.3.1 - May 3, 2021

### Core (0.3.1)

* ADDED: unicode escape sequences `\uXXXX`.
* ADDED: `promises` package.
* ADDED: `setMaxInferences` method to `Session` and `Thread` prototypes.

### Module `system` (0.3.1)

*  FIXED: `(=..)/2` built-in predicate when called with terms other than atoms or numbers.
* ADDED: `variable_names/1` option to `write_term/2` built-in predicate.
*  FIXED: `atom_chars/2` and `atom_codes/2` built-in predicates when containing variables in the second argument.

### Module `dom` (0.3.1)

* UPDATED: replaced `end_of_html` by `end_of_stream`. 

### Module `statistics` (0.3.1)

* ADDED: `walltime` statistics key.

### Documentation and examples (0.3.1)

* ADDED: [promises.js](examples/nodejs/promises.js) example.

## Version 0.3.0 - August 31, 2020

> **Note:** This version introduces important incompatibilities w.r.t. previous versions.

The `consult` and `query` methods of `pl.type.Session` and `pl.type.Thread` prototypes become asynchronous. In previous versions, you can load programs and query goals in the following way:

```js
var session = pl.create();
var show = answer => console.log(pl.format_answer(answer, session))
session.consult("foo(a). foo(b)."); // true
session.query("foo(X)."); // true
session.answer(show); // X = a.
session.answer(show); // X = b.
```

From version 0.3.0, you must do it in the following way:

```js
var session = pl.create();
var show = answer => console.log(session.format_answer(answer))
session.consult("foo(a). foo(b).", {
    success: function() {
        session.query("foo(X).", {
            success: function(goal) {
                session.answer(show); // X = a.
                session.answer(show); // X = b.
            },
            error: function(err) { /* Error parsing query */ }
        })
    },
    error: function(err) { /* Error parsing program */ }
}
```

In addition, you can pass an object with the following properties instead of a generic callback function to the `answer` and `answers` methods of `pl.type.Session` and `pl.type.Thread` prototypes: 

```js
{
    success: function(answer) { /* Successful answer */ },
    fail:    function { /* Failure */ },
    error:   function(err) { /* Uncaught exception */ },
    limit:   function { /* Limit exceeded */ }
}
```

### Core (0.3.0)

- ADDED: `system` module for built-in predicates.
- ADDED: `os` module for operating system interaction.
- ADDED: `charsio` module for I/O on lists of characters.
- ADDED: `format` module for formatted write.
- ADDED: support for user-defined modules.
- ADDED: support for `meta_predicate/1` directive.
- ADDED: support for `initialization/1` directive.
- UPDATED: default value for `double_quotes` flag from `codes` to `chars`.
- ADDED: `compile` method to `pl.type.Module` prototype.
- UPDATED: default value of `quoted` write option to `false` in the `toString` method of `pl.type.Term` prototype.
- ADDED: `user_error` stream alias.
- IMPROVED: parser efficiency (less recursion).
- UPDATED: `query` and `consult` methods of `pl.type.Session` and `pl.type.Thread` to make them asynchronous.
- FIXED: properties of standard streams.
- FIXED: `0'e` returning a float instead of an integer.
- FIXED: a Node.js error when trying to open a non-existent file for reading.
- FIXED: `sqrt/1`, `acos/1`, `asin/1`, `atan2/2` and `(^)/2` evaluable functors for undefined inputs.
- ADDED: `log/2`, `log10/1` and `gcd/2` evaluable functors.
- FIXED: typo in zero divisor evaluation error term.
- FIXED: a bug when writing atoms containing operators.
- FIXED: wrong reporting of line number in syntax errors.
- ADDED: binary operations for streams in Node.js.
- IMPROVED: `op/3` directive for working with lists of operators.
- FIXED: problems with asynchronous predicates when nesting calls to `answer` method of `pl.type.Session` and `pl.type.Thread` prototypes.

### Module `system` (0.3.0)

- ADDED: `numbervars/3` built-in predicate.
- ADDED: `predicate_property/2` built-in predicate.
- ADDED: `current_module/1` built-in predicate.
- ADDED: `time_property/2` built-in predicate.
- ADDED: `stream_position_data/3` built-in predicate.
- ADDED: `call_cleanup/2` and `setup_call_cleanup/3` built-in predicates.
- FIXED: `end_of_file` detection in `read_term/3` built-in predicate.
- FIXED: `bagof/3` and `setof/3` bug with existentially qualified variables when using multiple `(^)/2`.
- FIXED: a `setof/3` bug that was keeping duplicate elements in the final result.
- FIXED: multiple `read_term/3` bugs that cause the predicate to fail when trying to parse valid terms.
- FIXED: a `functor/3` bug when applied to floats.
- ADDED: `sort/2` and `keysort/2` built-in predicates (moved from `lists` module).
- FIXED: unexpected `subsumes_term/2` failures due to an incorrect definition of the predicate.
- FIXED: a `read_term/3` bug in the `variable_names/1` option.
- FIXED: a `between/3` bug when the lower bound is greater than the upper bound.
- FIXED: detection of closed streams in `current_input/1` and `current_output/1` built-in predicates.
- FIXED: if part isn't cut opaque in `(->)/2` and `(->;)/3` built-in predicates.
- FIXED: typos in `get_char/2`, `get_code/2`, `get_byte/2`, `peek_char/2`, `peek_code/2` and `peek_byte/2` built-in predicates causing errors.
- FIXED: a typo when resetting standard output in `close/2` built-in predicate.
- IMPROVED: `op/3` built-in predicate for working with lists of operators.
- FIXED: a `copy_term/2` bug when renaming variables.
- FIXED: a `bagof/3` bug which didn't bind the value of some variables in the final result.
- FIXED: error-checking of arguments in `set_prolog_flag/2`, `sub_atom/5`, `compare/3`, `current_op/3`, `arg/3`, `(=..)/2`, `open/4`, `peek_char/2`, `functor/3`, `retract/1`, `asserta/1`, `assertz/1`,  `stream_property/2`, `at_end_of_stream/1`, `current_predicate/1`, `atom_chars/2`, `atom_codes/2` and `keysort/2` built-in predicates.

### Module `lists` (0.3.0)

- FIXED: wrong position of the accumulator in `foldl/[4-7]` predicates.
- REMOVED: `sort/2` and `keysort/2` built-in predicates (moved to `system` module).

### Module `random` (0.3.0)

- ADDED: `get_seed/1` and `set_seed/1` predicates.

### Documentation and examples (0.3.0)

- ADDED: [TESTING.md](TESTING.md) documentation for testing Tau Prolog with [Logtalk](https://github.com/LogtalkDotOrg/logtalk3).
- UPDATED: [fruit.js](examples/nodejs/fruit.js) example to v0.3.0.
- UPDATED: [doge.js](examples/doge/doge.js) example to v0.3.0.

### Acknowledgments (0.3.0)

- Thanks to **Paulo Moura** for his help in detecting and correcting errors, for adding new predicates to the `os` module, and for his work in the integration of Tau Prolog with [Logtalk](https://github.com/LogtalkDotOrg/logtalk3).
- Thanks to **Markus Triska** for allowing us to adapt the `format` module from his original package of [Scryer Prolog](https://github.com/mthom/scryer-prolog/blob/master/src/lib/format.pl).

![Tau Prolog](http://tau-prolog.org/logo/tauprolog64.png "Tau Prolog")

# Tau Prolog

* [tau-prolog.org](http://tau-prolog.org) | [@tau_prolog](https://twitter.com/tau_prolog)
* [jariaza.es](http://jariaza.es) | [@jariazavalverde](https://twitter.com/jariazavalverde)

## A Prolog interpreter in JavaScript
Tau Prolog is a Prolog interpreter fully implemented in JavaScript. While most online interpreters are remote servers with an installed version of the interpreter which receive code, execute it and return the results, Tau Prolog is fully implemented on JavaScript and the code is analysed and parsed on the client side.

Tau Prolog is not just an implementation of the logic programming resolution mechanism, but a complete Prolog interpreter. Furthermore, the implementation of this interpreter has been directed by the ISO Prolog Standard.

Tau Prolog is released under the [BSD 3-Clause License](http://tau-prolog.org/license). This means that anyone is free to use, download, modify and share any version of this project. Tau Prolog has been developed by a team of students from [UCLM](http://uclm.es/) (University of Castilla-La Mancha, Spain) as an open source project. See [Collaborate](http://tau-prolog.org/collaborate).

## A brief look

Using the Tau Prolog library is simple. Just include a script tag in your html file, like this.
```html
<script src="tau-prolog.js"></script>
```
That's it! When the page loads, the `window` object is set with a `pl` object, which contains the logic related to Prolog.

In order to really use the library, you must create a `Session` object, which will contain the methods for parsing programs, looking for answers, etc. This way, we'll use `Session.consult()` to read a program, `Session.query()` to determine the goal we want to satisfy and `Session.answer()` to check if the goal is satisfied within the present database, and what values make it true if there were any variables on the goal. For example:

```html
<script id="likes.pl" type="text/prolog">
    likes(sam, salad).
    likes(dean, pie).
    likes(sam, apples).
    likes(dean, whiskey).
</script>
```

```javascript
var session = pl.create();
session.consult( "likes.pl" );
```

By calling the `pl.create()` method, we create a `Session` object, which contais a `consult()` method. This method receives the Prolog program. To set the goal we want to check, we need to call the `query()` method.
```javascript
session.query( "likes(sam, X)." );
```
Now, when we call the `answer()` method on the session variable, the interpreter tries to compute an answer for the goal, looking for facts or rules which unify with the goal. If the search succeeds, a `Substitution` object is passed to the callback provided to `answer()` as an argument. This `Substitution` object contains the variables of the goal and their values. Using a pile of states, the interpreter remembers the last choice point, so it can continue looking for facts from that point in a future search. If there is not any computed answer, the callback invoked on `answer()` will return `false`.

If `answer()` is called again after having unified the goal previously, the interpreter will continue looking for answers starting from the last choice point.
```javascript
var callback = console.log;
session.answer(callback); // X = salad ;
session.answer(callback); // X = apples ;
session.answer(callback); // false.
```
For further information, check the [Manual](http://tau-prolog.org/documentation#manual). It contains information about every function in the library, as well as code snippets to try them.

## Downloads
You can get the current stable fully-tested version [here](http://tau-prolog.org/downloads#latest). In the [Downloads](http://tau-prolog.org/downloads) section you will find the different versions of the Tau Prolog library. You can download the one that best fits your needs.

When downloading the library, you can customize the download so you only get the functionality you need. You can download the whole library, or maybe you just the core and the list module, or maybe you already had the library and only need a specific module, etc.

## Installation using the npm registry
You can install Tau Prolog from [npm](https://www.npmjs.com/), which is common practice when using [Node.js](https://nodejs.org/en/):
```shell
$ npm install tau-prolog
```
There is an example on how to use Tau Prolog together with NodeJS in `examples/nodejs`. If you have cloned this repository, you can run the example with the following command:
```shell
$ npm run example:fruit
```

## Documentation
The different predicates and modules available in this interpreter are documented on the [Documentation](http://tau-prolog.org/documentation) section. Even though most of the elements included in this interpreter were implemented following the ISO Prolog Standard, some of them have been modified according to the team judgement. Those differences between the way something was implemented and the ISO Prolog Standard are documented as well.

## Testing
Automated unit tests are written using the [QUnit](https://qunitjs.com) test framework and located in the `test` directory of this repository. To run the tests in a node runtime use the following command:
```shell
$ npm test
```
You should see output in the terminal similar to the following:
```shell
> tau-prolog@0.2.49 test /Users/username/path-to-repo/tau-prolog
> qunit test

TAP version 13
ok 1 Goal  'X is 10+20.'
ok 2 Goal  'X is 2*3+6'
ok 3 Goal  'X is 2*(3+6).'
...
ok 22 Answer - Limite reached
1..22
# pass 22
# skip 0
# todo 0
# fail 0
```
To run the tests in a client runtime, simply open `test/test.html` in your favourite browser.

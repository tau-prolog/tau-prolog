![Tau Prolog](http://tau-prolog.org/logo/tauprolog64.png "Tau Prolog")

[![www](https://img.shields.io/badge/www-tau--prolog.org-442178)](http://tau-prolog.org)
[![twitter](https://img.shields.io/badge/twitter-%40tau__prolog-blue)](https://twitter.com/tau_prolog)
[![license](https://img.shields.io/github/license/tau-prolog/tau-prolog?color=green)](http://tau-prolog.org/license)
[![npm version](https://img.shields.io/npm/v/tau-prolog?color=red "npm version")](https://www.npmjs.com/tau-prolog)
[![donate](https://img.shields.io/badge/donate-paypal-yellow)](https://www.paypal.com/donate/?hosted_button_id=JW48YT93SDLP8)

# Tau Prolog

## A Prolog interpreter in JavaScript
**Tau Prolog** is a client-side Prolog interpreter fully implemented in JavaScript, whose development has been directed by the ISO Prolog Standard.

> **ISO Prolog Standard compliance.** Tau Prolog development has been directed by the ISO Prolog Standard, designed to promote the applicability and portability of Prolog text and data among several data processing systems.

> **Compatible with browsers and Node.js.** Tau Prolog has been developed to be used with either Node.js or a browser seamlessly. Just use the `<script>` tag or the `require` function to add Tau Prolog to your project and start coding.

> **DOM manipulation and event handling.** Taking the best from JavaScript and Prolog, Tau Prolog allows you to handle browser events and modify the DOM of a web using Prolog predicates, making Prolog even more powerful.

> **Asynchronous predicates.** Tau Prolog has been developed following a non-blocking, callback-based approach, allowing you, for instance, to sleep the main thread or to do AJAX requests without blocking the browser.

## A brief look

1. **Load the library**
```html
<script src="tau-prolog.js"></script>
```
2. **Create a session**
```javascript
var session = pl.create();
```
3. **Consult a program**
```javascript
session.consult(`
    likes(sam, salad).
    likes(dean, pie).
    likes(sam, apples).
    likes(dean, whiskey).
`, {
    success: function() { /* Program loaded correctly */ },
    error: function(err) { /* Error parsing program */ }
});
```
or
```javascript
session.consult("path/to/src.pl", {
    success: function() { /* Program loaded correctly */ },
    error: function(err) { /* Error parsing program */ }
});
```
4. **Query a goal**
```javascript
session.query("likes(sam, X).", {
    success: function(goal) { /* Goal loaded correctly */ },
    error: function(err) { /* Error parsing goal */ }
});
```
5. **Look for answers**
```javascript
session.answer({
    success: function(answer) {
        console.log(session.format_answer(answer)); // X = salad ;
        session.answer({
            success: function(answer) {
                console.log(session.format_answer(answer)); // X = apples ;
            },
            // ...
        });
    },
    fail: function() { /* No more answers */ },
    error: function(err) { /* Uncaught exception */ },
    limit: function() { /* Limit exceeded */ }
});
```

This is a general scheme of how to use Tau Prolog:

```javascript
// Consult
session.consult(program, {
    success: function() {
        // Query
        session.query(goal, {
            success: function(goal) {
                // Answers
                session.answer({
                    success: function(answer) { /* Answer */ },
                    error:   function(err) { /* Uncaught error */ },
                    fail:    function() { /* Fail */ },
                    limit:   function() { /* Limit exceeded */ }
                })
            },
            error: function(err) { /* Error parsing goal */ }
        });
    },
    error: function(err) { /* Error parsing program */ }
});
```

For further information, check the [Documentation](http://tau-prolog.org/documentation).

## Downloads
You can download a custom bundle including only the modules you need [here](http://tau-prolog.org/downloads#custom). Source code of Tau Prolog is available on [GitHub](/modules). You can also install Tau Prolog from [npm](https://www.npmjs.com/tau-prolog):
```shell
$ npm install tau-prolog
```

## Documentation

### [**Get Started with Tau Prolog**](http://tau-prolog.org/documentation#manual)
* [A simple tutorial](http://tau-prolog.org/manual/a-simple-tutorial)
* [Compatibility with Node.js](http://tau-prolog.org/manual/compatibility-with-nodejs)
* [Manipulating the DOM with Prolog](http://tau-prolog.org/manual/manipulating-the-dom-with-prolog)
* [Making your own packages](http://tau-prolog.org/manual/making-your-own-packages)
* [Promises interface](http://tau-prolog.org/manual/promises-interface)
* [Prototypes and Prolog objects](http://tau-prolog.org/manual/prototypes-and-prolog-objects)

### [**Prolog Predicate Reference**](http://tau-prolog.org/documentation#prolog)
* [Built-in predicates](http://tau-prolog.org/documentation#builtin)
* [Lists module](http://tau-prolog.org/documentation#lists)
* [DOM module](http://tau-prolog.org/documentation#dom)
* [Random module](http://tau-prolog.org/documentation#random)
* [Statistics module](http://tau-prolog.org/documentation#statistics)
* [JavaScript module](http://tau-prolog.org/documentation#js)
* [OS module](http://tau-prolog.org/documentation#os)
* [CharsIO module](http://tau-prolog.org/documentation#charsio)
* [Format module](http://tau-prolog.org/documentation#format)
* [Concurrent module](http://tau-prolog.org/documentation#concurrent)

## License
Tau Prolog source code is released under the terms of the [BSD 3-Clause License](http://tau-prolog.org/license).

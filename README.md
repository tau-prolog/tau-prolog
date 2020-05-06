![Tau Prolog](http://tau-prolog.org/logo/tauprolog64.png "Tau Prolog")

# Tau Prolog

*[`@tau_prolog`](https://twitter.com/tau_prolog) ([`tau-prolog.org`](http://tau-prolog.org)) by [`@jariazavalverde`](https://twitter.com/jariazavalverde) ([`jariaza.es`](http://jariaza.es))*

## A Prolog interpreter in JavaScript
**Tau Prolog** is a client-side Prolog interpreter fully implemented in JavaScript, whose development has been directed by the ISO Prolog Standard.

> **ISO Prolog Standard compliance.** Tau Prolog development has been directed by the ISO Prolog Standard, designed to promote the applicability and portability of Prolog text and data among several data processing systems.

> **Compatible with browsers and Node.js.** Tau Prolog has been developed to be used with either Node.js or a browser seamlessly. Just use the `<script>` tag or the `require` function to add Tau Prolog to your project and start coding.

> **DOM manipulation and event handling.** Taking the best from JavaScript and Prolog, Tau Prolog allows you to handle browser events and modify the DOM of a web using Prolog predicates, making Prolog even more powerful.

> **Asynchronous predicates.** Tau Prolog has been developed following a non-blocking, callback-based approach, allowing you, for instance, to sleep the main thread or to do AJAX requests without blocking the browser.

## A brief look

1. **Load the library:**
```html
<script src="tau-prolog.js"></script>
```
2. **Consult a program:**
```javascript
var session = pl.create();
session.consult(`
    likes(sam, salad).
    likes(dean, pie).
    likes(sam, apples).
    likes(dean, whiskey).
`);
```
3. **Query a goal:**
```javascript
session.query("likes(sam, X).");
```
4. **Look for answers:**
```javascript
var callback = console.log;
session.answer(callback); // X = salad ;
session.answer(callback); // X = apples ;
session.answer(callback); // false.
```

For further information, check the [Documentation](http://tau-prolog.org/documentation).

## Downloads
You can download a custom bundle including only the modules you need [here](http://tau-prolog.org/downloads#custom). Source code of Tau Prolog is available on [GitHub](/modules). You can install Tau Prolog from [npm](https://www.npmjs.com/tau-prolog):
```shell
$ npm install tau-prolog
```

## Documentation

### [**Get Started with Tau Prolog**](http://tau-prolog.org/documentation#manual)
* [A simple tutorial](http://tau-prolog.org/manual/a-simple-tutorial)
* [Compatibility with Node.js](http://tau-prolog.org/manual/compatibility-with-nodejs)
* [Manipulating the DOM with Prolog](http://tau-prolog.org/manual/manipulating-the-dom-with-prolog)
* [Prototypes and Prolog objects](http://tau-prolog.org/manual/prototypes-and-prolog-objects)

### [**Prolog Predicate Reference**](http://tau-prolog.org/documentation#prolog)
* [Built-in predicates](http://tau-prolog.org/documentation#builtin)
* [Lists module](http://tau-prolog.org/documentation#lists)
* [DOM module](http://tau-prolog.org/documentation#dom)
* [Random module](http://tau-prolog.org/documentation#random)
* [Statistics module](http://tau-prolog.org/documentation#statistics)
* [JavaScript module](http://tau-prolog.org/documentation#js)

## License
Tau Prolog is released under the [BSD 3-Clause License](http://tau-prolog.org/license).

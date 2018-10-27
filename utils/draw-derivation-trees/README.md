![Tau Prolog](http://tau-prolog.org/logo/tauprolog64.png "Tau Prolog")

# Draw derivation trees

## Installation

Include the script [draw-derivation-trees.js](/utils/draw-derivation-trees/draw-derivation-trees.js) in your web page, after the [core.js](/modules/core.js) script.

```html
<script type="text/javascript" src="tau-prolog/core.js"></script>
<script type="text/javascript" src="tau-prolog/utils/draw-derivation-trees.js"></script>
```

## Usage

This tool adds a new method `draw(max_answers, canvas, styles)` to the `pl.type.Session` and `pl.type.Thread` prototypes. This method must be called after querying a goal (with the method `query( goal )`).

- **max_answers**: maximum number of answers to find in the derivation (to avoid infinite trees).
- **canvas**: identifier of the canvas HTML object, or canvas object.
- **styles** (optional): JavaScript object with style properties. The default styles are shown below.

```js
{
  "font-size": 14,
  "font-family": "Monospace, Courier New",
  "border-width": 2,
  "border-color": "#43207a",
  "padding": 5,
  "margin-x": 10,
  "margin-y": 20,
  order: {
    "radius": 15,
    "background-color": "#43207a",
    "border-width": 4,
    "border-color": "#43207a",
    "font-color": "#ffffff"
  },
  state: {
    "background-color": "#e0ccfd",
    "border-width": 4,
    "border-color": "#43207a",
    "font-color": "#43207a"
  },
  answer: {
    "background-color": "#a7e3a7",
    "border-width": 4,
    "border-color": "#0b6a0d",
    "font-color": "#0b6a0d"
  },
  error: {
    "background-color": "#e0ccfd",
    "border-width": 4,
    "border-color": "#881717",
    "font-color": "#881717"
  }
}

```

## Example

```html
<html>
  <head>
    <script type="text/javascript" src="tau-prolog/core.js"></script>
    <script type="text/javascript" src="tau-prolog/utils/draw-derivation-trees.js"></script>
    <script id="program.pl" type="text/prolog">
        powerset([], []).
        powerset([_|T], P) :- powerset(T, P).
        powerset([H|T], [H|P]) :- powerset(T, P).
    </script>
  </head>
  <body>
    <canvas id="derivation"></canvas>
    <script type="text/javascript">
        var session = pl.create();
        session.consult( "program.pl" );
        session.query( "powerset([1,2,3], X)." );
        session.draw( 10, "derivation" );
    </script>
  </body>
</html>
```

![Derivation tree](/utils/draw-derivation-trees/examples/powerset.png)

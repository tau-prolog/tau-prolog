![Tau Prolog](http://tau-prolog.org/logo/tauprolog64.png "Tau Prolog")

# Prolog Sandbox

## Installation

Upload all the files from the [sandbox](/utils/sandbox/) folder to your web server. This includes installing [codemirror](https://codemirror.net/) in `/sandbox/codemirror/`.

Import the [database.sql](/utils/sandbox/database.sql) file into your database manager. This file contains the database scheme.

Install the tool [draw-derivation-trees](/utils/draw-derivation-trees/README.md).

Install [Tau Prolog](/modules/). Make sure that the relative routes in the [index.php](/utils/sandbox/index.php) files are pointing to the Tau Prolog files.

```html
<!-- Tau Prolog modules -->
<script type="text/javascript" src="/code/core-latest.js?update=<?php echo $_time_core; ?>"></script>
<script type="text/javascript" src="/code/lists-latest.js?update=<?php echo $_time_lists; ?>"></script>
<script type="text/javascript" src="/code/random-latest.js?update=<?php echo $_time_random; ?>"></script>
<script type="text/javascript" src="/code/statistics-latest.js?update=<?php echo $_time_statistics; ?>"></script>
<script type="text/javascript" src="/code/dom-latest.js?update=<?php echo $_time_dom; ?>"></script>
<script type="text/javascript" src="/code/js-latest.js?update=<?php echo $_js_statistics; ?>"></script>
<!-- Tau Prolog utils -->
<script type="text/javascript" src="/utils/draw-derivation-trees.js"></script>
```

Fill the template file [mysql.php.sample](/utils/sandbox/mysql.php.sample) with your database login credentials and rename the file to `mysql.php`.

[Here](http://tau-prolog.org/sandbox/) you can see how the default installation looks.
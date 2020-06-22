# Testing

## Testing Prolog standards compliance

Using the latest git versions of both Tau Prolog and Logtalk, is now possible
to manually use Logtalk's Prolog compliance test suite to check Tau Prolog. We
expect to enable autmated testing soon.

Assuming a POSIX system, follow this [guide](https://logtalk.org/running_developer_versions.html)
to install the Logtalk git version, which includes a `taulgt.sh`
integration script (for details, use `man taulgt`). This script
requires that the `NODE_PATH` environment variable be set to the
`node_modules` directory path where Tau Prolog and its dependencies
are installed:

```bash
$ ll $NODE_PATH
total 0
drwxr-xr-x   7 johndoe  staff  224 May 21 09:42 balanced-match
drwxr-xr-x   6 johndoe  staff  192 May 21 09:42 brace-expansion
drwxr-xr-x   8 johndoe  staff  256 May 21 09:42 commander
drwxr-xr-x   9 johndoe  staff  288 May 21 09:42 concat-map
drwxr-xr-x  17 johndoe  staff  544 May 21 09:42 js-reporters
drwxr-xr-x   6 johndoe  staff  192 May 21 09:42 minimatch
drwxr-xr-x  10 johndoe  staff  320 May 21 09:42 node-watch
drwxr-xr-x   8 johndoe  staff  256 May 21 09:42 path-parse
drwxr-xr-x   9 johndoe  staff  288 May 21 09:42 qunit
drwxr-xr-x   7 johndoe  staff  224 May 21 09:42 readline-sync
drwxr-xr-x  16 johndoe  staff  512 May 21 09:42 resolve
drwxr-xr-x   9 johndoe  staff  288 May 21 09:42 tau-prolog
```

For good performance when testing, edit the `$LOGTALKHOME/core/core.pl`
and `$LOGTALKHOME/adapters/tau.pl` files to turn off the `clean` flag.
In the `core.pl` file, search for the definition of the predicate
`'$lgt_load_built_in_entity'/4` (near the end of the file) and
change `clean(on)` to `clean(off)`. In the `tau.pl` file, search for
`'$lgt_default_flag'(clean, on)` and change it to `'$lgt_default_flag'(clean, off)`.

Next, test the setup by running the `taulgt.sh` integration script.
First run will be slow but you should get the default Logtalk startup
messages that include a banner and a list of the default flag values
followed by tips on developer tools. Something like:

```text
$ taulgt.sh

Logtalk 3.40.0(-)b01
Copyright (c) 1998(-)2020 Paulo Moura

% Default lint compilation flags(:)
...
% For Logtalk compiler warnings(/)errors explanations and fix suggestions(,) use(:)
% (?)(-) {tutor(loader)}. or (?)(-) logtalk_load(tutor(loader)).
% 
?-
```

For the actual testing, change the current directory to one of the
leaf sub-directories of the `$LOGTALKHOME/tests/prolog` directory.
For example:

```text
$ cd $LOGTALKHOME/tests/prolog/predicates/term_variables_2
$ taulgt.sh
?- {tester}.
```

The first time a test set is run, it will be slow. But thanks to
the setting of the `clean` flag to `off`, subsequent runs will be
much faster as they will use the pre-compiled libraries and tools
from the first run.

When you get a failed test, check the Tau Prolog current open issues
before reporting it. Note that different failed tests may have a common
cause. Also, keep in mind that standards complaince is hard work.
Prioritize your bug reports. For example, failed tests due to an
exception being different from the one specified by the standards are
likely not as important at this point as failed tests where a goal
that is expected to succeed fails or a goal that is expected to fail
suceeds.

This guide will be updated for Tau Prolog standards compliance progress
and for improved Logtalk support for Tau Prolog. Do a `git pull` on both
your Tau Prolog and Logtalk git clones before running any tests to ensure
that you will be testing the current versions.

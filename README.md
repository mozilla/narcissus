# Narcissus

Narcissus is a JavaScript interpreter written in pure JavaScript (i.e., a [meta-circular evaluator](http://en.wikipedia.org/wiki/Meta-circular_evaluator)), using the [SpiderMonkey](http://www.mozilla.org/js/spidermonkey/) engine.

Originally a proof-of-concept by [Brendan Eich](http://brendaneich.com/), Narcissus is being revived as a test-bed for rapidly prototyping new language features for the JavaScript language (as well as the ECMAScript standard).

# Usage

To run the Narcissus shell, install the [SpiderMonkey shell](https://developer.mozilla.org/en/Introduction_to_the_JavaScript_shell) and either set the `NJS_SHELL` environment variable to the path to the `js` executable, or creating a symbolic link to `js` in the top-level Narcissus directory. Then run Narcissus with the `njs` script.

  Usage: njs
  
  Options:
    -h, --help            show this help message and exit
    -f FILE, --file=FILE  JS file to load
    -e JS_EXPS, --expression=JS_EXPS
                          JS expression to evaluate
    -i, --interactive     enable interactive shell
    -H, --harmony         enable ECMAScript Harmony mode
    -S, --ssa             enable parse-time SSA construction

# Dependencies

The front end of Narcissus, consisting of a lexer and parser (`js{defs,lex,parse}.js`), is written in portable ECMAScript Edition 3 code. The back end, consisting of the interpreter (`js{ssa,exec}.js`), is written using JavaScript extensions currently supported only by [SpiderMonkey](http://www.mozilla.org/js/spidermonkey/).

# Development

More to come.

# Contributors

* Tom Austin
* Brendan Eich
* Andreas Gal
* Shu-yu Guo
* Dave Herman
* Dimitris Vardoulakis
* Patrick Walton

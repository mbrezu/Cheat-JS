
# Cheat-JS - macros for JavaScript. Kinda.

## Important Note

Cheat-JS includes a modified version of
[`parse-js`](http://marijnhaverbeke.nl/parse-js/), written by Marijn
Haverbeke. This is necessary because I (Miron Brezuleanu) needed to
modify `parse-js` a little. The license of `parse-js` is in the
`LICENSE-parse-js.txt` file. The modified files from `parse-js`
included in Cheat-JS are `parse.lisp`, `tokenize.lisp` and
`util.lisp`. The modifications were permitted by the `parse-js`
license. This is not an official copy of `parse-js` and is not
supported by Marijn Haverbeke. If the modified parsing code in
Cheat-JS breaks, it's exclusively my fault - I messed up the code.

Cheat-JS also uses
[`cl-uglify-js`](https://github.com/mishoo/cl-uglify-js) unmodified,
via [Quicklisp](http://www.quicklisp.org/). These two libraries do
most of the work, Cheat-JS is mostly 'glue code'.

## About Cheat-JS

Lisp macros are powerful and easy to implement because Lisp programs
are made of s-expressions.

Lisp-style macros are difficult to add to other languages because most
languages have very non-uniform syntax compared to Lisp. Source
transformations (and most importantly macros) would be easier in, say,
JavaScript, if it were possible to convert the JavaScript code into
s-expressions, transform it, and convert it back into JavaScript code.

Turns out that we can transform JavaScript code into an AST made of
s-expressions using `parse-js`. Converting back into JavaScript code
can be done with `cl-uglify-js` (ironically,
`cl-uglify-js:ast-gen-code` is a capable pretty printer). All that
remains to be done to have macros (well, defined in another language)
is define transformations to be applied on the output of
`parse-js`. This is what Cheat-JS does: get the `parse-js` AST, apply
the transformations, convert back to JavaScript code.

## Some simple examples

Instead of writing:

    var Person = function(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    };

Cheat-JS makes it possible to write:

    var Person = @defclass(name, shoeSize);

(Some irregularities in JavaScript syntax make it much harder to
expand something like `@defclass(Person, name, shoeSize);` into
`function Person(name, shoeSize) { this.name = name; this.shoeSize =
shoeSize; };`).

This assumes that we have defined a `@defclass` macro which does the
above expansion - we'll define two such macros in this document.

One of the `parse-js` modifications necessary for this to work is
allow `@` as a character in identifiers (the recommended convention
for naming macros is `@` followed by the macro name). Currently macro
names can't be nested (i.e. `some.namespace.@iife` is not valid
syntax).

Instead of writing:

    var greeter = (function () {
        return {
            'hello': function(name) {
                console.log('hello, '+ name);
            };
        };
    }());

Cheat-JS makes it possible to write:

    var greeter = @iife(
        return {
            'hello': function(name) {
                console.log('hello, '+ name);
            }
        };
    );

This assumes that we have defined a `@iife` macro that wraps its
arguments with `(function () { ... }())`
([meaning of the IIFE acronym](http://www.benalman.com/news/2010/11/immediately-invoked-function-expression/)).

I also had to modify `parse-js` to convince it to parse macro
invocations that look like function calls, but have a list of
statements instead of a list of parameters (see the invocation of
`@iife` above).

Instead of writing:

    (function () {
        var test = someTest();
        if (test) {
            console.log("Yes!");
            console.log("We passed the test!");
        }
    })();

Cheat-JS makes it possible to write:

    @when-let(test, someTest();
        console.log("Yes!");
        console.log("We passed the test!");
    );

This macro is similar to
[`alexandria:when-let`](http://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow). `@when-let`
has the most complicated interface possible for a Cheat-JS macro: its
argument list has both expressions (`test`, `someTest()`) and
statements (the `console.log` call). When invoking such macros,
separate the expressions and the statements with a semicolon, as in
the example above.

It is of course possible to define the anaphoric version of
`@when-let`, `@awhen` (from
[OnLisp](http://paulgraham.com/onlisp.html), page 190).

The guide on how to write Cheat-JS macros is based on defining
`@defclass` (and even a safer version of `@defclass`), `@iife`,
`@when-let` and `@awhen`. So all potential misteries introduced by
this section will be cleared by the end of this document.

## Getting started

You can get Cheat-JS at `http://github.com/mbrezu/cheat-js`. It's
probably best to `git clone` it inside the `local-projects` directory
of your Quicklisp install, so you can load it with `(ql:quickload
:cheat-js)` in your REPL.

Note: I've only tested it with SBCL, so I recommend you use SBCL
too. It should work with other CL implementations, though (all the
code required is standard CL).

Running the tests with:

    (cheat-js:run-tests)

should give you some confidence that things are not obviously broken
(They are most likely broken, but in subtle ways).

The next section should provide you with some pointers on how to
define your Cheat-JS macros.

## Your first Cheat-JS macros

Before running `cheat-js:macroexpand-all` on your JavaScript source
code, you need to install your macros.

First reset the list of installed macros,

    > (cheat-js:clear-macros)

To define a macro, you need to know three things:

 * how the macro invocation call looks like (what you want to write in
   the JavaScript source code); this is the macro's "API"; it is
   JavaScript code (or almost) and the parsed AST;
 * how the macro expansion looks like; this is the macro's "result";
   it is an AST tree;
 * how to transform the AST of the invocation into the AST of the
   expansion; this is the macro's "implementation", written in Common
   Lisp.

Let's define `@defclass`.
  
### Defining `@defclass`

We need to see how the macro invocation looks like in JavaScript:

    var Person = @defclass(name, shoeSize)

We can tell that the macro is an 'args only' macro (i.e. the
invocation looks like a normal JavaScript function invocation, it does
not contains statements). We can inform Cheat-JS about this:

    > (cheat-js:register-args-macro "@defclass")

We also want to see the AST for the invocation (we use Cheat-JS's
parsing function because the above snippet is not parsable by
`parse-js` without tweaks; in particular, the parsing won't work as
expected if we don't call `register-args-macro` as above, so don't
skip that step):

    > (cheat-js:parse-js "var Person = @defclass(name, shoeSize);")

    (:TOPLEVEL
     ((:VAR
       (("Person" :MACRO-CALL (:NAME "@defclass")
         (:ARGS (:SEQ (:NAME "name") (:NAME "shoeSize"))))))))

The part that starts with `:MACRO-CALL` is the interesting part; this
is the AST representation of our macro invocation; this is what we
need to transform into the expansion.

What does the expansion look like? Let's see:

    > (cheat-js:parse-js "var Person = function(name, shoeSize)
                          {
                              this.name = name;
                              this.shoeSize = shoeSize;
                          };")

    (:TOPLEVEL
     ((:VAR
       (("Person" :FUNCTION NIL ("name" "shoeSize")
         ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
          (:STAT
           (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))))))

Comparing the two ASTs reveals that we need to transform the
`(:MACRO-CALL` s-expression into the `(:FUNCTION` s-expression (don't
worry about the apparently missing `(` in front of `:FUNCTION` above,
it's because the list following `:VAR` is made of conses, not lists).

To make it clearer, we need to write a lisp function to transform this:

    (:MACRO-CALL (:NAME "@defclass")
     (:ARGS (:SEQ (:NAME "name") (:NAME "shoeSize"))))

into this:

    (:FUNCTION NIL ("name" "shoeSize")
     ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
      (:STAT (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))

This part is easy. Just like writing CL macros :-)

    > (defun defclass-expander (invocation)
        (let* ((raw-names (cdr (second (third invocation))))
               (names (mapcar #'second raw-names)))
          `(:function nil ,names
                      ,(mapcar (lambda (name)
                                 `(:stat
                                   (:assign t
                                            (:dot (:name "this") ,name)
                                            (:name ,name))))
                               names))))

The parameter `invocation` is the s-expression starting with
`(:MACRO-CALL...` above. The value returned by the function should be
the expansion show above (s-expression starting with `(:FUNCTION...`).

Let's test it:

    > (let ((invocation '(:MACRO-CALL
                          (:NAME "@defclass")
                          (:ARGS (:SEQ (:NAME "name") (:NAME "shoeSize"))))))
        (defclass-expander invocation))
    (:FUNCTION NIL ("name" "shoeSize")
     ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
      (:STAT
       (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))

Looks OK. Let's tell Cheat-JS about our function:

    > (cheat-js:register-macro-expander "@defclass" #'defclass-expander)

Now we can ask Cheat-JS to macroexpand our code:

    > (cheat-js:macroexpand-all "var Person = @defclass(name, shoeSize);")
    "var Person = function(name, shoeSize)
    {
        this.name = name;
        this.shoeSize = shoeSize;
    };"

`cl-uglify-js` is a really good pretty printer, isn't it?

On to `@iife`.

### Defining `@iife`

### Defining a safer `@defclass`

### Defining `@when-let`

### Defining `@awhen`

## Closing thoughts

Still reading? Wow!

One thing is obvious: Cheat-JS makes it possible to write macro-like
transformations on JavaScript code, but it's not nearly as easy as
writing Common Lisp macros. Maybe this isn't a bad thing - we should
be writing macros only when there's no other way to avoid code
duplication.

There are plenty of quirks. There's only so many transformations you
can do (function calls are not as frequent in JavaScript as they are
in Common Lisp, and macro invocations are 'hooked' to function
calls). Maybe `parse-js` could be tweaked harder to make it possible
to insert macros at other points. For now, the transformations
possible with 'function call' macros are enough for me.

You need to be able to 'pattern match' ASTs and figure out how to
transform macro invocations ASTs into macro expansions ASTs. This is a
basic macro writing skill, but with an indirection (in Common Lisp the
source code is the AST, not so with JavaScript).

You also need to know Common Lisp. In theory, a Cheat-JS based
preprocessor could be distributed and used by people who are only
'consuming' macros produced by someone else (a 'macro
producer'). Hmmm, people will certainly be amused if a 'macro
producer' starts distributing a 40MB executable (this is about the
minimum size for SBCL standalone executables) that just
explodes some constructs in the source code :-)

With a JavaScript parser written in JavaScript it would be possible to
do what Cheat-JS does without Common Lisp (though without backquotes
the generation of macro expansions is probably a pain, and the AST
would have to be uniform - nested arrays, no classes, maybe? - to make
it easier to 'pattern match' and analyze).

Right now, the audience of Cheat-JS is probably the audience of
ParenScript (mostly because you need to be a lisper to fully use
Cheat-JS). I (Miron Brezuleanu) wrote a few thousands of lines of
ParenScript code and found that there is some 'impedance mismatch'
between ParenScript and JavaScript (especially around the '.' operator
in JavaScript and modules in JavaScript). This was most likely my
fault: instead of writing Lisp to be compiled to JavaScript, I was
trying to write JavaScript with s-expressions. I found it harder to
write code in ParenScript than in JavaScript, and the presence of
macros didn't compensate for this extra effort. I tried to find a way
to have macros while writing something closer to JavaScript. Cheat-JS
is what I came up with.

Thanks for taking the time to read about Cheat-JS; I hope you'll find
it useful (or at least amusing - or both)!

Please use the Github
[issues page](https://github.com/mbrezu/cheat-js/issues) to report any
bugs or to post feature requests.

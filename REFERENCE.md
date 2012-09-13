
# Cheat-JS macro library reference

This file documents the macros included with Cheat-JS and how to
install them in your code. See the
[`tests.lisp`](https://github.com/mbrezu/Cheat-JS/blob/master/tests.lisp)
file for code using these macros.

## Introduction

The Common Lisp interface for the Cheat-JS macro library is the
package `CJ-MACRO-LIBRARY`. The functions are:

 * `list-macros` - returns a list with all the available macros;
 * `install-macros` - argument `MACROS` is a list of macro names to be
   installed; if a macro depends on other macros, the dependencies are
   added recursively to the list of macros to install; the list of all
   installed macros is returned by the function;
 * `reset-gensym-counter` - resets the gensym counter used by some
   macros; useful if you want to macroexpand some source code multiple
   times with identical results (e.g. when writing test code for
   macros :-) ).

These functions let you install all the available macros (use
`(cj-macro-library:install-macros (cj-macro-library:list-macros))`) or
only the macros you like (and their dependencies, so you'd better like
the dependencies too :-) ).

## All macros

Below is the list of all macros available in this library, with
documentation and examples.

### @iife

Short for
['immediately invoked function expression'](http://www.benalman.com/news/2010/11/immediately-invoked-function-expression/),
this macro expands

    var Test = @iife(return 2;);

into

    var Test = function() {
        return 2;
    }();

and

    @iife(
        doThis();
        thenDoThat();
    );

into

    (function() {
        doThis();
        thenDoThat();
    })();

Not much of a shortcut, but helps with diminishing the syntactic
clutter usually associated with IIFEs.

Need a new scope anywhere in your code? `@iife` makes it easy to get
one.

### @defclass

A 'safe' class definition looks like this:

    var Person = @iife(
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        };
        return function (name, shoeSize) {
            return new Person(name, shoeSize);
        };
    );

(note the use of the `@iife` macro to create a new scope)

The idea is to make it safe to create a new object by calling
`Person('John', 10)` (without `new`) or `new Person('John', 10)`. This
class definition is a bit verbose, so it's more comfortable to use
`@defclass`:

    var Person = @defclass(name, shoeSize);

Now, what if we want to do some processing in our 'safe constructor'?
For instance, let's say we want to store the first name of the
person. The long version could be:

    var Person = @iife(
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        };
        return function (name, shoeSize) {
            var self = new Person(name, shoeSize);
            self.firstName = name.split(' ')[0];
            return self;
        };
    );

Which `@defclass` lets us write simply as:

    var Person = @defclass(
        name, shoeSize;
        self.firstName = name.split(' ')[0];
    );

This is an extension not described in the
[`README.md`](https://github.com/mbrezu/Cheat-JS/blob/master/README.md)
file. If you don't like `self`, you can try something like this:

    var Person = @defclass(
        name, shoeSize, $that;
        that.firstName = name.split(' ')[0];
    );

which expands into:

    var Person = @iife(
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        };
        return function (name, shoeSize) {
            var that = new Person(name, shoeSize);
            that.firstName = name.split(' ')[0];
            return that;
        };
    );

An argument starting with a `$` sign is used to form the name of the
variable holding the new object; just don't use `$this`, please!

### @if

This is just a wrapper for the ternary operator in JavaScript. One can write:

    return @if(someCondition,
               thenResult,
               elseResult);

which expands into:

    return someCondition ? thenResult : elseResult;

This is not much of a shortcut, but it can improve readability.

### @and and @or

Instead of:

    if (cond1 && cond2 && cond3 && (cond4 || cond5)) {
        doSomething();
    }

it's possible to write:

    if (@and(cond1,
             cond2,
             cond3,
             @or(cond4,
                 cond5))) {
        doSomething();
    }

This second version may be friendlier to some (especially lispers). I
personally find that the prefix form is ideal for 'and' and 'or' (even
if I sometimes find its usability debatable for arithmetic operators).

### @fn

What if you could write:

    @fn(a, b; a < b)

instead of:

    function(a, b) { return a < b; }

Well, you can :-)

### @fn0

This is `@iife`'s little cousin: it creates a parameterless function,
without invoking it. Sometimes useful.

    var action = @fn0(
        doThis();
        thenDoThat();
    );

expands into:

    var action = function() {
        doThis();
        thenDoThat();
    }

### @dbind

Common Lisp's `destructuring-bind` little cousin. Use it like this:

    @dbind([a, b], [1, 2]);

which expands into:

    {
        var tmp1 = [ 1, 2 ], a = tmp1[0], b = tmp1[1];
    }

(the `1` in `tmp1` may vary, it's the gensym counter of the library).

Right now `@dbind` only knows how to destructure simple array
assignments like the one above.

This macro will probably be expanded to handle more cases.

### @dotimes

Instead of writing:

    for (var i = 0; i < 100; i++) {
        doRepeatableAction();
        doAnotherRepeatableAction();
    }

`@dotimes` lets you write:

    @dotimes(
        i, 100;
        doRepeatableAction();
        doAnotherRepeatableAction();
    );

Wrap it in an `@iife` if you want a new scope for `i`:

    @iife(@dotimes(
        i, 100;
        doRepeatableAction();
        doAnotherRepeatableAction();
    ));

Or if you don't care about `i` at all, just write:

    @dotimes(
        100;
        doRepeatableAction();
        doAnotherRepeatableAction();
    );

and `@dotimes` will generate a variable for you.

### @dolist

Iterating over a list can be bothersome:

    for (var i = 0, l = myList.length; i < l; i++) {
        var item = myList[i];
        doSomething(item);
        doSomethingElse(item);
    }

With `@dolist`:

    @dolist(
        item, myList;
        doSomething(item);
        doSomethingElse(item);
    );
    
What if we also need the iterator variable?

    for (var i = 0, l = myList.length; i < l; i++) {
        var item  = myList[i];
        doSomething(item, i);
        doSomethingElse(item);
    }

`@dolist` can handle that too:

    @dolist(
        item, myList, $i;
        doSomething(item, i);
        doSomethingElse(item);
    );
    
This is the same pattern used for `@defclass`: if we need an optional
variable, specify its name by prefixing it with a `$` and adding it to
the list of arguments. In the case of `@dolist`, an argument prefixed
with `$` specifes the name of the iterator variable.

`@dolist` can be replaced with a `map`/`foreach` function with the
same effects, so it's probably not a very useful macro (until you need
the speed of 'inlining' the call into a `for` iteration, which is
a rather improbable need).

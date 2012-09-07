
# Cheat-JS - macro for JavaScript. Kinda.

## Important Note

Cheat-JS includes a modified version of `parse-js`, written by Marijn
Haverbeke. This is necessary because I (Miron Brezuleanu) needed to
modify `parse-js` a little. The license of `parse-js` is in the
`LICENSE-parse-js.txt` file. The modified files from `parse-js`
included in Cheat-JS are `parse.lisp`, `tokenize.lisp` and
`util.lisp`. The modifications were permitted by the `parse-js`
license. This is not an official copy of `parse-js` and is not
supported by Marijn Haverbeke. If the modified parsing code in
Cheat-JS breaks, it's exclusively Miron Brezuleanu's fault, because he
messed up the code.

## About Cheat-JS

Cheat-JS is a JavaScript preprocessor that permits applying AST
transformations on JavaScript source. The AST transformations (macros)
are written in Common Lisp. Instead of writing:

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

Instead of writing:

    function Person(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    }
    
Cheat-JS makes it possible to write:

    @defclass(Person, name, shoeSize)
    
(assuming that `@iife` and `@defclass` were defined as AST
transformations in Common Lisp code).

## Getting started

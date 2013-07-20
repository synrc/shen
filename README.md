
Erlang Shen: Erlang JavaScript Transformation
=============================================

Till now all existed attempts to bring Erlang to the browser is nothing than playing with mind.
No emulation of terrible Erlang bytecode is needed. In fact Erlang bytecode is relict
that is even translated by BEAM into more modern internal bytecode. So every project
that attempt to translate on BEAM byte-code level not only slow but in fact is a trash.

The only real pratical fast solution is to translate Erlang AST into JavaScript
using JavaScript helpers like matches.js and tailrec.js. E.g:

fac.erl:

    start() ->
        N = fac(5),
        console:log("factorial ~p", [J, N]).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).

fac.js:

    var pattern = require("matches").pattern;
    var fac = pattern({
        '0' : function (x) { 
            return 1;
        },
        'n' : function (N) {
            return N * fac(N-1);
        }
    });
    var start = pattern({
        ' ' : function() {
            N = fac(5);
            console.log("Fac ~p ~p", 5, fac (5) );
        }
    });
    start();

Credits
-------

    * Maxim Sokhatsky
    * Andrew Zadorozhny

OM A HUM

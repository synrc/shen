Erlang JavaScript Parse Transform
=================================

Till now all existed attempts to bring Erlang to the browser is nothing than playing with mind.
No emulation of terrible Erlang bytecode is needed. In fact Erlang bytecode is relict
that is even translated by BEAM into more modern internal bytecode. So every project
that attempt to translate on BEAM byte-code level not only slow but in fact is a useless.

The only real pratical fast solution is to translate Erlang AST into JavaScript
using JavaScript helpers like matches.js and tailrec.js.

Compilation to JavaScript
-------------------------

fac.erl:

    -module(fac).
    -compile({parse_transform, shen}).
    -compile(export_all).

    -js([start/0,fac/1]).

    start() ->
        N = fac(5),
        console:log("factorial ~p", [J, N]).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).

Compile with Erlang:

    $ erlc shen.erl
    $ erlc -pa . fac.erl

And you will get fac.js:

    var pattern = require("matches").pattern;
    var start = pattern({
        '': function() {
            j = 5;
            n = fac(j);
            return console.log('factorial ~p',[j,[n,[]]]);
    }});
    var fac = pattern({
        '0': function(x1) {
            return 1;
        },
        'n': function(n) {
            return n * fac(n - 1);
    }});
    start();

You can try this:

    $ node fac.js
    factorial ~p [ 5, [ 120, [] ] ]

JavaScript Macros
-----------------

Let say we want to generate JavaScript in our code, so you can write
programs in Erlang and expand them into JavaScript.

    -module(fac).
    -compile({parse_transform, shen}).
    -compile(export_all).

    -jsmacro([macro/3]).

    macro(A,B,C) ->
        ws:send('Bert':encodebuf(
            [{source,'Bert':binary(A)},
             {x,C},
             {pickle,'Bert':binary(B)},
             {linked,C}])).

    main() ->
        A = "1",
        B = "2",
        Script = macro(A,B,"3"),
        io:format("JS Macro: ~s",[Script]).

Lets try it:

    7> fac:main().
    JS Macro: ws.send(Bert.encodebuf({source:Bert.binary(1),x:3,pickle:Bert.binary(2),linked:3}));
    ok

Credits
-------

    * Maxim Sokhatsky
    * Andrew Zadorozhny

OM A HUM

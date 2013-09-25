Erlang JavaScript Parse Transform
=================================

Till now all existed attempts to bring Erlang to the browser is nothing than playing with mind.
No emulation of terrible Erlang bytecode is needed. In fact Erlang bytecode is relict
that is even translated by BEAM into more modern internal bytecode. So every project
that attempt to translate on BEAM byte-code level not only slow but in fact is a useless.

Libraries
---------

We support following stack by Erlang JavaScript compiler:

* matches.js -- Erlang-like matching syntax
* tailrec.js -- optimize tail calls
* beacon.js -- FRP event streaming

The only real pratical fast solution is to translate Erlang AST into JavaScript
using JavaScript helpers like matches.js and tailrec.js.

Usage
-----

* Compilation to JavaScript, node.js, Browser, Client-Side FRP
* Macros, String Templates, Embedding Mode, Server-Side, N2O

Compilation to Pure JavaScript
------------------------------

In case of Client-Logic, Off-line clients, Client-side FRP Event System
use can export you functions in module with -js attribute.
All function will be stored to the same filename with js extension.

fac.erl:

```erlang
    -module(fac).
    -compile({parse_transform, shen}).
    -compile(export_all).

    -js([start/0,fac/1]).

    start() ->
        N = fac(5),
        console:log("factorial ~p", [J, N]).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).
```

Compile with Erlang:

    $ erlc shen.erl
    $ erlc -pa . fac.erl

And you will get fac.js:

```javascript
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
```

Now you can check:

    $ node fac.js
    factorial ~p [ 5, [ 120, [] ] ]

JavaScript Macros
-----------------

Let say we want to generate JavaScript in our Erlang code which is very useful
for Server-Logic Event System, so you can write programs in Erlang and expand
them into JavaScript using -jsmacro attribute. Specified functions will be
expanded to JavaScript during compilation.

```erlang
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
```

Lets try it:

    7> fac:main().
    JS Macro: ws.send(Bert.encodebuf({source:Bert.binary(1),x:3,pickle:Bert.binary(2),linked:3}));
    ok

Roadmap
-------

1. multiple clauses for lambdas
2. list comprehensions
3. more JavaScript/OTP mappings
4. if statement :-)

Credits
-------

    * Maxim Sokhatsky
    * Andrew Zadorozhny

OM A HUM

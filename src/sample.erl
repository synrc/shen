-module(sample).
-compile({parse_transform, shen}).
-compile(export_all).

-output("priv").
-js([start/0,start2/2,fac/1]).
-jsmacro([macro/3]).

macro(A,B,C) ->
    X = document,
    jq(X),
    jq("document"),
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).

start2(X,Y) -> 
    case X of
        1 -> console:log([X,Y]);
        _ -> console:log("ok") end.

start() ->
    start2(1,3),
    J = 5,
    N = fac(J),
    console:log("factorial ~p", [J, N]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

main() ->
    A = "1",
    B = "2",
    Script = macro(A,B,"3"),
    io:format("JS Macro: ~s",[Script]).


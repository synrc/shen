-module(a).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([macro/3,tabshow/0,doc_ready/1]).

tabshow() ->
    X = jq("a[data-toggle=tab]"),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

doc_ready(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

macro(A,B,C) ->
    X = jq("document"),
    E = fun(Y,Z) -> X:at("ss") end,
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).
main() ->
    A = "1",
    B = "2",
    Script1 = macro(A,B,"3"),
    Script = doc_ready("'tab'"),
    io:format("JS Macro: ~s",[Script]).

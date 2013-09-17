-module(a).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([macro/3,tabshow/0,doc_ready/1]).

tabshow() ->
    X = jq("a[data-toggle=tab]"),
    lists:map(fun(X) -> X*X end,[1,2,3,4]),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

doc_ready(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

macro(A,B,C) ->
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).
main() ->
    Script1 = tabshow(),
    Script2 = macro("1","2","3"),
    Script3 = doc_ready("tab"),
    io:format("tabshow/0:~n~s~nevent/3:~n~s~ndoc_ready/1:~n~s~n",[Script1,Script2,Script3]).

-module(a).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([macro/3,tabshow/0,doc_ready/1,on_show/0,append_header/0]).

macro(A,B,C) ->
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},
         {x,C},
         {pickle,'Bert':binary(B)},
         {linked,C}])).


tabshow() ->
    X = jq("a[data-toggle=tab]"),
    S = lists:map(fun(X) -> X*X end,[1,2,3,4]),
    M = lists:foldl(fun(X,Acc) -> Acc+X end,0,[1,2,3,4]),
    SS = 12,
    A = case SS and (SS == 12) of
            12 -> M;
            false -> "11" end,
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

doc_ready(E) ->
    macro("1","2",E),
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

on_show() ->
     X = jq("a[data-toggle=\"tab\"]"),
     X:on("shown", fun(E) ->
         T = jq(E:at("target")),T:addClass("text-warning"),
         Sb = T:siblings(),Sb:removeClass("text-warning"),
         tabshow(T:attr("href")) end).

append_header() ->
     on_show(),
     'appendHeader' = fun(T) ->
         console:log(jq(T)),
         B = jq("body"),
         H = jq("#header"),
         console:log(H),
         D = H:detach(),
         Tab = jq(T),
         D:prependTo(Tab),
         setTimeout(fun()-> console:log(Tab) end, 1)
     end.

main() ->
    Script1 = tabshow(),
    Script2 = macro("1","2","3"),
    Script3 = doc_ready("'tab'"),
    io:format("tabshow/0:~n~s~nevent/3:~n~s~ndoc_ready/1:~n~s~n",[Script1,Script2,Script3]).

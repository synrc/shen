-module(fac).
-compile({parse_transform, shen}).

-js([start/0,fac/1,macro/3]).
-jsmacro([macro/3]).

macro(A,B,C) ->
    ws:send('Bert':encodebuf(
        [{source,'Bert':binary(A)},{x,C},{pickle,'Bert':binary(B)},{linked,C}])).

start() ->
    J = 5,
    N = fac(J),
    console:log("factorial ~p", [J, N]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

main() ->
    Script = macro("1","2","3"),
    io:format("JS Macro: ~p",[Script]).

% $('#~s').parent('.file_upload').after(\"<img src='~s'>\").remove();

%html_box(Box) ->
%    chain([{jq,[Box]},
%           {parent,['.fileupload']},
%           {after,["<img src='",Box,"'"]},
%           {remove,[]}]).

-module(shen).
-copyright('Synrc Research Center').
-compile(export_all).

parse(File) ->
    {ok,Content} = file:read_file(File),
    {ok,Tokens,X} = erl_scan:string(binary_to_list(Content)),
    tokens(Tokens,[]).

tokens([],Res) -> Res;
tokens(Tokens,Res) ->
    Fun = fun(X)-> case X of {dot,_} ->false; _  -> true end end,
    Cut = lists:takewhile(Fun,Tokens),
    case Cut of
        [] -> tokens([],Res);
        _ -> [H|T] = lists:dropwhile(Fun,Tokens),
             tokens(T,Res ++ [Cut ++ [H]]) end.

forms(File) -> [ begin {ok,Form} = erl_parse:parse_form(Line), Form end || Line <- parse(File)].

prelude() -> io_lib:format("~n~s~n",["var pattern = require(\"matches\").pattern;"]).
intermezzo(Forms) -> [ compile(F) || F <- Forms ].
coda() -> io_lib:format("~s~n",["start();"]).

compile({function,X,Name,Args,Clauses}) -> function(Name,Args,Clauses);
compile(Form) -> io:format("ICE: unknown form: ~p",[Form]).

function(Name,Args,Clauses) ->
  [ io_lib:format("var ~s = pattern({~n", [ Name ]),
    string:join([ clause(Args,C) || C <- Clauses ],",\n"),
    io_lib:format("~s~n",["});"]) ].

clause(Argc,{clause,X,Argv,Guard,Expressions}) ->
    Match = string:join([ exp(Arg) || Arg <- Argv ],","),
    Args = string:join([ arg(Arg,N) || {Arg,N} <- lists:zip(Argv,lists:seq(1,Argc))],","),
  [ io_lib:format("\t'~s': function(~s) {~n", [Match,Args]),
    ["\t\t"++case N == length(Expressions) of true -> "return "; _ -> "" end ++ exp(E)++";\n" || {E,N} <- lists:zip(Expressions,lists:seq(1,length(Expressions)))],
    io_lib:format("~s",["\t}"]) ].

arg({integer,X,Value},N) -> io_lib:format("x~s",[integer_to_list(N)]);
arg({var,X,Value},N) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]).
par(List) -> io_lib:format("~s",[lists:flatten(string:join([exp(V)||V<-List],","))]).
exp({integer,X,Value}) -> io_lib:format("~s",[integer_to_list(Value)]);
exp({string,X,Value}) -> io_lib:format("'~s'",[Value]);
exp({cons,X,Left,Right}) -> io_lib:format("[~s,~s]",[exp(Left),exp(Right)]);
exp({nil,X}) -> "[]";
exp({var,X,Value}) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]);
exp({op,X,'-',Left,Right}) -> io_lib:format("~s - ~s",[exp(Left),exp(Right)]);
exp({op,X,'*',Left,Right}) -> io_lib:format("~s * ~s",[exp(Left),exp(Right)]);
exp({call,X,{atom,Y,Name},Params}) -> io_lib:format("~s(~s)",[Name,par(Params)]);
exp({call,X,{remote,XX,{atom,Y,Module},{atom,Z,Name}},Params}) -> io_lib:format("~s.~s(~s)",[Module,Name,par(Params)]);
exp({match,X,Left,Right}) -> io_lib:format("~s = ~s",[exp(Left),exp(Right)]);
exp(X) -> X.

main() ->
    Forms = forms("fac.erl"),
    io:format("Forms: ~p",[Forms]),
    io:format("~s",[lists:flatten([prelude(),intermezzo(Forms),coda()])]).

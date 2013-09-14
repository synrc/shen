-module(shen).
-author('Maxim Sokhatsky').
-copyright('Synrc Research Center').
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, Options) ->
    io:format("Forms ~p~n", [Forms]),
    Directives = directives(Forms),
    File = proplists:get_value(file,Directives),
    Macros = proplists:get_value(jsmacro,Directives),
    Exp = proplists:get_value(js,Directives),
    Inlines = intermezzo(Forms,Macros,inline),
    io:format("Macros ~p~nExp: ~p~n", [Macros, Exp]),
    io:format("Inlines ~p~n", [Inlines]),
    [ io:format("Stack ~p: ~p~n",[Name,get({macro,Name})]) || {Name,_} <- Macros],
    Result = lists:flatten([prelude(),intermezzo(Forms,Exp,compile),coda()]),
    file:write_file(File,list_to_binary(Result)),
    Forms.

directives(Forms) -> lists:flatten([ directive(F) || F <- Forms ]).
forms(File) -> {ok,Forms} = epp:parse_file(File,[],[]), Forms.
prelude() -> io_lib:format("~n~s~n",["var pattern = require(\"matches\").pattern;"]).
intermezzo(Forms,Exp,Type) -> [ compile(F,Type) || F={function,X,Name,Args,_} <- Forms, lists:member({Name,Args},Exp) ].
coda() -> io_lib:format("~s~n",["start();"]).

directive({attribute,X,module,Name}) -> {file,atom_to_list(Name)++".js"};
directive({attribute,X,js,List}) -> {js,List};
directive({attribute,X,jsmacro,List}) -> {jsmacro,List};
directive(Form) -> [].

compile({attribute,X,Word,Name},_) -> "";
compile({function,X,Name,Args,Clauses},Type) -> function(Name,Args,Clauses,Type);
compile({eof,X},_) -> "";
compile(Form,_) -> ":-)".

function(Name,Args,Clauses,Type) ->
    case Type of
        compile -> [ io_lib:format("var ~s = pattern({~n", [ Name ]),
                     string:join([ clause(Args,C,Type) || C <- Clauses ],",\n"),
                     io_lib:format("~s~n",["});"]) ];
        inline -> {macro,Name,string:join([ clause(Args,C,{inline,Name}) || C <- Clauses ],",\n")}
    end.

clause(Argc,{clause,X,Argv,Guard,Expressions},{inline,Name}) -> 
    put({macro,Name},[]),
    R = [ exp(E,{inline,Name})++";\n" || E <- Expressions ],
    put({macro,Name},lists:reverse(get({macro,Name}))),
    R;
clause(Argc,{clause,X,Argv,Guard,Expressions},compile) ->
    Match = string:join([ exp(Arg,compile) || Arg <- Argv ],","),
    Args = string:join([ arg(Arg,N) || {Arg,N} <- lists:zip(Argv,lists:seq(1,Argc))],","),
  [ io_lib:format("\t'~s': function(~s) {~n", [Match,Args]),
    ["\t\t"++case N == length(Expressions) of true -> "return "; _ -> "" end ++ exp(E,compile)++";\n" 
      || {E,N} <- lists:zip(Expressions,lists:seq(1,length(Expressions)))],
    io_lib:format("~s",["\t}"]) ].

check_proplist({nil,X},L,Mode) -> {true,L};
check_proplist({cons,X,Left,Right},L,Mode) ->
    case Left of
         {tuple,_X,[Key,Val]} -> check_proplist(Right,L++[{exp(Key,Mode),exp(Val,Mode)}],Mode);
         _ -> false end.

arg({integer,_X,Value},N) -> io_lib:format("x~s",[integer_to_list(N)]);
arg({var,_X,Value},N) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]).
par(List,Mode) -> io_lib:format("~s",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]).
exp({integer,_X,Value},_) -> io_lib:format("~s",[integer_to_list(Value)]);
exp({string,_X,Value},_) -> io_lib:format("'~s'",[Value]);
exp({atom,_X,Value},_) -> io_lib:format("~s",[Value]);
exp({tuple,_X,List},Mode) -> io_lib:format("{~s}",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]);
exp(Cons={cons,_X,Left,Right},Mode) -> 
    case check_proplist(Cons,[],Mode) of
        {true,L} -> io_lib:format("{~s}",[string:join([[K,":",V]||{K,V}<-L],",")]);
        false -> io_lib:format("[~s,~s]",[exp(Left,Mode),exp(Right,Mode)]) end;
exp({nil,X},_) -> "[]";
exp({var,_X,Value},compile) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]);
exp({var,_X,Value},{inline,Name}) -> put({macro,Name},[Value|get({macro,Name})]), "~s";
exp({op,_X,'-',Left,Right},Type) -> io_lib:format("~s - ~s",[exp(Left,Type),exp(Right,Type)]);
exp({op,_X,'*',Left,Right},Type) -> io_lib:format("~s * ~s",[exp(Left,Type),exp(Right,Type)]);
exp({call,_X,{atom,Y,Name},Params},Mode) -> io_lib:format("~s(~s)",[Name,par(Params,Mode)]);
exp({call,_X,{remote,XX,{atom,Y,Module},{atom,Z,Name}},Params},Mode) -> 
    io_lib:format("~s.~s(~s)",[Module,Name,par(Params,Mode)]);
exp({match,_X,Left,Right},Type) -> io_lib:format("~s = ~s",[exp(Left,Type),exp(Right,Type)]);
exp(X,_) -> X.

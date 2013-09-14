-module(shen).
-author('Maxim Sokhatsky').
-copyright('Synrc Research Center').
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, _Options) ->
    Directives = directives(Forms),
    File = proplists:get_value(file,Directives),
    Macros = proplists:get_value(jsmacro,Directives),
    Exp = proplists:get_value(js,Directives,[]),
    Inlines = intermezzo(Forms,Macros,inline),
    io:format("Macros ~p~nExp: ~p~n", [Macros, Exp]),
    [ io:format("Stack ~p: ~p~n",[Name,get({macro,Name})]) || {Name,_} <- Macros],
    [ io:format("Inline ~p: ~s", [Name,get({inline,Name})]) || {Name,_} <- Macros],
    F = compile_macros(Forms,Macros),
    io:format("Forms ~p~n", [F]),
    Result = lists:flatten([prelude(),intermezzo(Forms,Exp,compile),coda()]),
    file:write_file(File,list_to_binary(Result)),
    F.

directives(Forms) -> lists:flatten([ directive(F) || F <- Forms ]).
forms(File) -> {ok,Forms} = epp:parse_file(File,[],[]), Forms.
prelude() -> io_lib:format("~n~s~n",["var pattern = require(\"matches\").pattern;"]).
coda() -> io_lib:format("~s~n",["start();"]).
intermezzo(Forms,Exp,Type) -> [ compile(F,Type) || F={function,_,Name,Args,_} <- Forms, lists:member({Name,Args},Exp) ].
compile_macros(Forms,Exp) -> [ xform(F,Exp) || F <- Forms ].

directive({attribute,_X,module,Name}) -> {file,atom_to_list(Name)++".js"};
directive({attribute,_X,js,List}) -> {js,List};
directive({attribute,_X,jsmacro,List}) -> {jsmacro,List};
directive(_Form) -> [].

xform({function,X,Name,Args,Clauses},Exp) ->
    case lists:member({Name,Args},Exp) of
        true ->  function(Name,X,Args,Clauses,expand);
        false -> {function,X,Name,Args,Clauses} end;
xform(X,_Exp) -> X.

compile({attribute,_X,_Word,_Name},_) -> "";
compile({function,X,Name,Args,Clauses},Type) -> function(Name,X,Args,Clauses,Type);
compile({eof,_X},_) -> "";
compile(_Form,_) -> ":-)".

function(Name,X,Args,Clauses,Type) ->
    case Type of
        compile -> [ io_lib:format("var ~s = pattern({~n", [ Name ]),
                     string:join([ clause(Args,C,Type) || C <- Clauses ],",\n"),
                     io_lib:format("~s~n",["});"]) ];
        inline -> {macro,Name,string:join([ clause(Args,C,{inline,Name}) || C <- Clauses ],",\n")};
        expand -> io:format("M: ~p~n",[Name]),
               {function,X,Name,Args,[ clause(Args,C,{macroexpand,Name}) || C <- Clauses]}
    end.

cons(X,[]) -> {nil,X};
cons(X,[H|T]) -> {cons,X,{var,X,H},cons(X,T)}.

clause(_Argc,{clause,X,Argv,Guard,_Expressions},{macroexpand,Name}) ->
    {clause,X,
        Argv,
        Guard,
        [{call,X,
            {remote,X,{atom,X,io_lib},{atom,X,format}},
            [{string,X,lists:flatten(get({inline,Name}))}, cons(X,get({macro,Name}))]
        }]};
clause(_Argc,{clause,_X,_Argv,_Guard,Expressions},{inline,Name}) -> 
    put({macro,Name},[]),
    R = [ exp(E,{inline,Name})++";\n" || E <- Expressions ],
    put({inline,Name},R),
    put({macro,Name},lists:reverse(get({macro,Name}))),
    R;
clause(Argc,{clause,_X,Argv,_Guard,Expressions},compile) ->
    Match = string:join([ exp(Arg,compile) || Arg <- Argv ],","),
    Args = string:join([ arg(Arg,N) || {Arg,N} <- lists:zip(Argv,lists:seq(1,Argc))],","),
  [ io_lib:format("\t'~s': function(~s) {~n", [Match,Args]),
    ["\t\t"++case N == length(Expressions) of true -> "return "; _ -> "" end ++ exp(E,compile)++";\n" 
      || {E,N} <- lists:zip(Expressions,lists:seq(1,length(Expressions)))],
    io_lib:format("~s",["\t}"]) ].

check_proplist({nil,_X},L,_Mode) -> {true,L};
check_proplist({cons,_X,Left,Right},L,Mode) ->
    case Left of
         {tuple,_X,[Key,Val]} -> check_proplist(Right,L++[{exp(Key,Mode),exp(Val,Mode)}],Mode);
         _ -> false end.

arg({integer,_X,_Value},N) -> io_lib:format("x~s",[integer_to_list(N)]);
arg({var,_X,Value},_N) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]).
par(List,Mode) -> io_lib:format("~s",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]).
exp({integer,_X,Value},_) -> io_lib:format("~s",[integer_to_list(Value)]);
exp({string,_X,Value},_) -> io_lib:format("'~s'",[Value]);
exp({atom,_X,Value},_) -> io_lib:format("~s",[Value]);
exp({tuple,_X,List},Mode) -> io_lib:format("{~s}",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]);
exp(Cons={cons,_X,Left,Right},Mode) -> 
    case check_proplist(Cons,[],Mode) of
        {true,L} -> io_lib:format("{~s}",[string:join([[K,":",V]||{K,V}<-L],",")]);
        false -> io_lib:format("[~s,~s]",[exp(Left,Mode),exp(Right,Mode)]) end;
exp({nil,_X},_) -> "[]";
exp({var,_X,Value},compile) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]);
exp({var,_X,Value},{inline,Name}) -> put({macro,Name},[Value|get({macro,Name})]), "~s";
exp({op,_X,'-',Left,Right},Type) -> io_lib:format("~s - ~s",[exp(Left,Type),exp(Right,Type)]);
exp({op,_X,'*',Left,Right},Type) -> io_lib:format("~s * ~s",[exp(Left,Type),exp(Right,Type)]);
exp({call,_X,{atom,_Y,Name},Params},Mode) -> io_lib:format("~s(~s)",[Name,par(Params,Mode)]);
exp({call,_X,{remote,_XX,{atom,_Y,Module},{atom,_Z,Name}},Params},Mode) -> 
    io_lib:format("~s.~s(~s)",[Module,Name,par(Params,Mode)]);
exp({match,_X,Left,Right},Type) -> io_lib:format("~s = ~s",[exp(Left,Type),exp(Right,Type)]);
exp(X,_) -> X.

-module(shen).
-author('Maxim Sokhatsky').
-author('Andrii Zadorozhnii').
-copyright('Synrc Research Center s.r.o.').
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, _Options) ->
    Directives = directives(Forms),
    File = proplists:get_value(file,Directives,"default.js"),
    Path = proplists:get_value(output,Directives,"."),
    Macros = proplists:get_value(jsmacro,Directives,[]),
    put(macros,Macros),
    put({macroargs,"match"},[]),
    put({macroargs,"lambda"},[]),
    Exp = proplists:get_value(js,Directives,[]),
    collect_vars(Forms,Macros),
%    io:format("Macros ~p~nExp: ~p~n", [Macros, Exp]),
%    [ io:format("Signatures ~p: ~p~n",[Name,get({macroargs,Name})]) || {Name,_} <- Macros],
%    io:format("Forms ~p", [Forms]),
    _Inlines = intermezzo(Forms,Macros,inline),
%    [ io:format("Stack ~p: ~p~n",[Name,get({stack,Name})])          || {Name,_} <- Macros],
%    [ io:format("Inline ~p: ~s~n", [Name,get({inline,Name})])       || {Name,_} <- Macros],
    F = compile_macros(Forms,Macros),
    Result = lists:flatten([prelude(),intermezzo(Forms,Exp,compile),coda()]),
    file:write_file(filename:join([Path,File]),list_to_binary(Result)),
    compile:forms(F,[binary,export_all]),
    F.

directives(Forms) -> lists:flatten([ directive(F) || F <- Forms ]).
forms(File) -> {ok,Forms} = epp:parse_file(File,[],[]), Forms.
%prelude() -> io_lib:format("~n~s~n",["var pattern = require(\"matches\").pattern;"]).
prelude() -> io_lib:format("~n~s~n",["var pattern = window.matches.pattern;"]).
coda() -> io_lib:format("~s~n",["start();"]).
intermezzo(Forms,Exp,Type) -> [ compile(F,Type) || F={function,_,Name,Args,_} <- Forms, lists:member({Name,Args},Exp) ].
compile_macros(Forms,Exp) -> [ xform(F,Exp,expand) || F <- Forms ].
collect_vars(Forms,Exp) -> [ xform(F,Exp,vars) || F <- Forms ].

directive({attribute,_X,module,Name}) -> {file,atom_to_list(Name)++".js"};
directive({attribute,_X,js,List}) -> {js,List};
directive({attribute,_X,output,List}) -> {output,List};
directive({attribute,_X,jsmacro,List}) -> {jsmacro,List};
directive(_Form) -> [].

xform({function,X,Name,Args,Clauses},Exp,Method) ->
    case lists:member({Name,Args},Exp) of
        true ->  function(Name,X,Args,Clauses,Method);
        false -> {function,X,Name,Args,Clauses} end;
xform(X,_Exp,_) -> X.

compile({attribute,_X,_Word,_Name},_) -> "";
compile({function,X,Name,Args,Clauses},Type) -> function(Name,X,Args,Clauses,Type);
compile({eof,_X},_) -> "";
compile(_Form,_) -> ":-)".

% compile  -- function declaration for compile
% match    -- case clause
% inline   -- macro call
% lambda   -- inline anonymous function declarations in fun body
% vars     -- collect var for macro expand
% expand   -- macro expand

function(Name,X,Args,Clauses,Type) ->
    case Type of
        compile -> [ io_lib:format("var ~s = pattern({~n", [ Name ]),
                     string:join([ clause(Args,C,Type) || C <- Clauses ],",\n"),
                     io_lib:format("~s~n",["});"]) ];
        match -> [ io_lib:format("pattern({~n",[]),
                     string:join([ clause(Args,C,{match,Name}) || C <- Clauses ],",\n"),
                     io_lib:format("})~n",[]) ];
        inline -> {macro,Name,string:join([ clause(Args,C,{inline,Name}) || C <- Clauses ],",\n")};
        lambda -> string:join([ clause(Args,C,{lambda,Name}) || C <- Clauses ],",\n");
        vars -> [ clause(Args,C,{collectvars,Name}) || C <- Clauses];
        expand -> {function,X,Name,Args,[ clause(Args,C,{macroexpand,Name}) || C <- Clauses]} end.

cons(X,[]) -> {nil,X};
cons(X,[H|T]) -> {cons,X,{var,X,H},cons(X,T)}.

clause(_Argc,_C={clause,_X,XArgv,_Guard,Expressions},{lambda,Name}) ->
    Argv = [ {T,L,Na} || {T,L,Na} <- XArgv, Na /= 'This'],
    Args = string:join([ arg(Arg,N) || {Arg,N} <- lists:zip(Argv,lists:seq(1,length(Argv)))],","),
  [ io_lib:format("function(~s) {~n", [Args]),
    ["\t\t"++case N == length(Expressions) of true -> "return "; _ -> "" end ++ exp(E,{inline,Name})++";\n" 
      || {E,N} <- lists:zip(Expressions,lists:seq(1,length(Expressions)))],
    io_lib:format("~s",["\t}"])];
clause(_Argc,C={clause,_X,Argv,_Guard,_Expressions},{collectvars,Name}) ->
    put({macroargs,Name},[ XName || {var,_,XName} <- Argv]), C;
clause(_Argc,{clause,X,Argv,Guard,_Expressions},{macroexpand,Name}) ->
    {clause,X,
        Argv,
        Guard,
        [{call,X,
            {remote,X,{atom,X,io_lib},{atom,X,format}},
            [{string,X,lists:flatten(get({inline,Name}))}, cons(X,get({stack,Name}))]
        }]};
clause(_Argc,{clause,_X,_Argv,_Guard,Expressions},{inline,Name}) -> 
    put({stack,Name},[]),
    R = [ exp(E,{inline,Name})++";\n" || E <- Expressions ],
    put({inline,Name},R),
    put({stack,Name},lists:reverse(get({stack,Name}))),
    R;
clause(_Argc,{clause,_X,Argv,_Guard,Expressions},{match,_Name}) ->
    Match = string:join([ exp(Arg,compile) || Arg <- Argv ],","),
 [ io_lib:format("\t'~s': function() {~n", [Match]),
    ["\t\t"++case N == length(Expressions) of true -> "return "; _ -> "" end ++ exp(E,compile)++";\n" 
      || {E,N} <- lists:zip(Expressions,lists:seq(1,length(Expressions)))],
    io_lib:format("~s",["\t}"]) ];
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

normalize_list({nil,_X},_L,_Mode) -> [];
normalize_list({cons,_X,Left,Right},L,Mode) -> [{exp(Left,Mode)},normalize_list(Right,L,Mode)].

arg({integer,_X,_Value},_N) -> io_lib:format("_~s",[integer_to_list(_Value)]);
arg({string,_X,_Value},N) -> io_lib:format("~s",[N]);
arg({atom,_X,_Value},N) -> io_lib:format("~s",[N]);
arg({var,_X,Value},_N) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]).

par(List,Mode) -> io_lib:format("~s",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]).

exp({integer,_X,Value},_) -> io_lib:format("~s",[integer_to_list(Value)]);
exp({string,_X,Value},_) -> io_lib:format("'~s'",[Value]);
exp({atom,_X,Value},{inline,_}) -> io_lib:format("'~w'",[Value]);
exp({atom,_X,Value},_) -> io_lib:format("~w",[Value]);
exp({'fun',X,{clauses,Value}},{inline,Name}) -> function(Name,X,0,Value,lambda);
exp({'fun',X,{clauses,Value}},_) -> function("lambda",X,0,Value,lambda);
exp({tuple,_X,List},Mode) -> io_lib:format("[~s]",[lists:flatten(string:join([exp(V,Mode)||V<-List],","))]);
exp(Cons={cons,_X,_Left,_Right},Mode) -> 
    case check_proplist(Cons,[],Mode) of
        {true,L} -> io_lib:format("{~s}",[string:join([[K,":",V]||{K,V}<-L],",")]);
           false -> io_lib:format("[~s]",[string:join(
                        lists:map(fun({[X]})->X end,lists:flatten(normalize_list(Cons,[],Mode))),",")]) end;
exp({nil,_X},_) -> "[]";
exp(V={var,_X,Value},{inline,Name}) ->
    Macroargs = get({macroargs,Name}),
    case lists:member(Value,Macroargs) of
         true -> put({stack,Name},[Value|get({stack,Name})]), "~s";
         false -> exp(V,compile) end;
exp({var,_X,Value},compile) -> io_lib:format("~s",[string:to_lower(atom_to_list(Value))]);
exp({'case',X,Condition,Clauses},Type) ->
    io_lib:format("(~s)(~s)",[function("match",X,1,Clauses,match),exp(Condition,Type)]);
exp({op,_X,'++',Left,Right},Type) -> io_lib:format("~s + ~s",[exp(Left,Type),exp(Right,Type)]);
exp({lc,_X,_Var,_GenerateList},_Type) -> "lc";
exp({op,_X,'=:=',Left,Right},Type) -> io_lib:format("~s == ~s",[exp(Left,Type),exp(Right,Type)]);
exp({op,_X,'=/=',Left,Right},Type) -> io_lib:format("~s != ~s",[exp(Left,Type),exp(Right,Type)]);
exp({op,_X,'/=',Left,Right},Type) -> io_lib:format("~s != ~s",[exp(Left,Type),exp(Right,Type)]);
exp({op,_X,Op,Left,Right},Type) -> io_lib:format("~s ~s ~s",[exp(Left,Type),Op,exp(Right,Type)]);
exp({call,_X,{atom,_Y,jq},Params},Mode) -> io_lib:format("~s(~s)",["$",par(Params,Mode)]);
exp({call,_X,{atom,_Y,Name},Params},Mode) ->
    case {lists:member({Name,length(Params)},get(macros)),Mode} of
         {true,{inline,_Outer}} -> 
            {MacroArgs,MacroStack} = {get({macroargs,Name}),get({stack,Name})},
            InlineName = get({inline,Name}),
            case InlineName of undefined -> "nop()"; _ -> io_lib:format(lists:flatten(InlineName),
               [ exp(lists:nth(string:str(MacroArgs,[P]),Params),Mode) || P <- MacroStack]) end;
         {false,_} -> io_lib:format("~s(~s)",[Name,par(Params,Mode)]) end;
exp({call,_,{remote,_,_VarAtom={atom,_,lists},{atom,_,map}},[Fun,List]},Mode) -> shen_lists:map(Fun,List,Mode);
exp({call,_,{remote,_,_VarAtom={atom,_,lists},{atom,_,foldl}},[Fun,Acc,List]},Mode) -> shen_lists:foldl(Fun,Acc,List,Mode);
exp({call,_,{remote,_,_VarAtom={atom,_,lists},{atom,_,foldr}},[Fun,Acc,List]},Mode) -> shen_lists:foldr(Fun,Acc,List,Mode);
exp({call,_X,{remote,_XX,VarAtom={_Tag,_Y,_Module},{atom,_Z,at}},Params},Mode) -> 
    io_lib:format("~s[~s]",[exp(VarAtom,compile),par(Params,Mode)]);
exp({call,_X,{remote,_XX,VarAtom={_Tag,_Y,_Module},{atom,_Z,Name}},Params},Mode) -> 
    io_lib:format("~s.~s(~s)",[exp(VarAtom,compile),Name,par(Params,Mode)]);
exp({remote,_XX,{var,_Y,_Module},{_,_Z,Name}},_Mode) -> 
    X = io_lib:format("~s.~s",[string:to_lower(lists:concat([_Module])),
                               string:to_lower(lists:concat([Name])) ]),
    X;
exp({call,_X,{var,_XX,Name},Params},Mode) -> 
    io_lib:format("~s(~s)",[string:to_lower(lists:concat([Name])),par(Params,Mode)]);
exp({match,_X,Left,Right},Type) -> io_lib:format("var ~s = ~s",[exp(Left,Type),exp(Right,Type)]);
exp({record_field,_X,{_,_,Name},Value},Mode) ->
    io_lib:format("~s: ~s",[lists:concat([Name]),exp(Value,Mode)]);
exp({record,_X,react,Fields},Mode) ->
    L = [ io_lib:format("~s",[exp(F,Mode)]) || F <- Fields],
    io_lib:format("React.createClass({~s});",[string:join(L,",\n")]);
exp({record,_X,Tag,Fields},Mode) ->
    case lists:member(Tag,[h1,'div',panel]) of
      true ->
        L = lists:flatten([ io_lib:format("~s",[exp(Value,Mode)]) 
              || F={record_field,_,{_,_,Name},Value} <- Fields, Name == body]),
        io_lib:format("React.DOM.~s(null,~s);",[Tag,L]);
      false -> skip end;
exp({record,_X,{var,_,Var},react,Fields},Mode) ->
    L = [ io_lib:format("~s",[exp(F,Mode)]) || F <- Fields],
    io_lib:format("~s({~s});",[string:to_lower(lists:concat([Var])),L]);
exp(X,_) -> X.


-module(shen_react).
-compile({parse_transform, shen}).
-compile(export_all).

-output("priv").
-js([start/0,value/2]).

-record(react,{props=[],state,name,init,render,willMount=fun(X)->X end}).
-record(h1,{body}).

value(Key,O) -> Props = O:at("props"), Props:at(Key).

start() ->
    User = #react{
        render = fun(This) -> #h1{body=value(email,This)} end
            },

    CommentList = #react{
        props = [{data,[]}],
        render = fun(This) -> 
            Users = lists:map(fun(Item) ->
                User#react{props=Item} end, value(data,This)),
            Users
        end
    },

    CommentList.

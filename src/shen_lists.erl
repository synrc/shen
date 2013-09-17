-module(shen_lists).
-author('Maxim Sokhatsky').
-copyright('Synrc Research Center').
-compile(export_all).

map(Fun,List,Mode) -> io_lib:format("~s.map(~s);\n",[shen:exp(List,Mode),shen:exp(Fun,Mode)]).

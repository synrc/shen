-module(shen_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> shen_sup:start_link().
stop(_State) -> ok.


start() ->
    J = 5,
    N = fac(J),
    console:log("factorial ~p", [J, N]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

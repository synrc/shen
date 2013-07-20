var pattern = require("matches").pattern;
var fac = pattern({
    '0' : function (x) { 
        return 1;
    },
    'n' : function (N) {
        return N * fac(N-1);
    }
});
var start = pattern({
    ' ' : function() {
        N = fac(5);
        console.log("Fac ~p ~p", 5, fac (5) );
    }
});
start();

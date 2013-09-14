
var pattern = require("matches").pattern;
var start = pattern({
	'': function() {
		j = 5;
		n = fac(j);
		return console.log('factorial ~p',[j,[n,[]]]);
	}});
var fac = pattern({
	'0': function(x1) {
		return 1;
	},
	'n': function(n) {
		return n * fac(n - 1);
	}});
start();

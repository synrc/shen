
var pattern = window.matches.pattern;
var start2 = pattern({
	'x,y': function(x,y) {
		var f = function(a) {
		return a.log('100');
	};
		f(console);
		return (pattern({
	'1': function() {
		return console.log([x,y]);
	},
	'_': function() {
		return console.log('ok');
	}})
)(x);
	}});
var start = pattern({
	'': function() {
		start2(1,3);
		var j = 5;
		var n = fac(j);
		return console.log('factorial ~p',[j,n]);
	}});
var fac = pattern({
	'0': function(_0) {
		return 1;
	},
	'n': function(n) {
		return n * fac(n - 1);
	}});
start();

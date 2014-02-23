
var pattern = window.matches.pattern;
var value = pattern({
	'key,o': function(key,o) {
		var props = o['props'];
		return props[key];
	}});
var start = pattern({
	'': function() {
		var user = React.createClass({render: function() {
		return value(email,this);
	}});;
		return user;
	}});
start();

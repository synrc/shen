
var pattern = window.matches.pattern;
var value = pattern({
	'key,o': function(key,o) {
		var props = o['props'];
		return props[key];
	}});
var start = pattern({
	'': function() {
		var user = React.createClass({render: function() {
		return React.DOM.h1(null,value('email',this));;
	}});;
		var commentlist = React.createClass({props: {data:[]},
render: function() {
		var users = value('data',this).map(function(item) {
		return user({props: item});;
	});
;
		return users;
	}});;
		return commentlist;
	}});
start();

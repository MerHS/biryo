var x="";
function repl(obj, id, htmlStr) {
	if (obj) {
		for (var key in obj) {
			htmlStr = htmlStr.replace('@'+key+'@', obj[key]);
		}
	}
	$('#틀-' + id).html(htmlStr);
}
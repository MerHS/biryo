var x="";
var href = $('link[rel=stylesheet]').attr('href');
var contentHref = href.substring(0, href.lastIndexOf('/')) + '/';
var entryHref = contentHref.replace('/mdd/', '/entry/')
function repl(obj, id, htmlStr) {
	var hrefR = new RegExp('href="/w/', 'g');
	var hrefE = new RegExp('href="entry://', 'g');
	var hrefN = new RegExp('href="', 'g');
	htmlStr = htmlStr.replace(hrefR,'href="entry://').replace(hrefE, 'href="').replace(hrefN, 'href="' + entryHref);
	if (obj) {
		for (var key in obj) {
			var keyR = new RegExp('@'+key+'@', 'g');
			htmlStr = htmlStr.replace(keyR, obj[key]);
		}
	}
	$('#틀-' + id).html(htmlStr).css('display','inline');
}

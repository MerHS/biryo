var x="";
function repl(obj, id, htmlStr) {
	var hrefR = new RegExp('href="/w/', 'g');
	var hrefE = new RegExp('href="entry://', 'g');
	var hrefN = new RegExp('href="', 'g');
	htmlStr = htmlStr.replace(hrefR,'href="entry://').replace(hrefE, 'href="');
	htmlStr = htmlStr.replace(hrefN, 'href="content://mdict.cn/entry/109/');
	if (obj) {
		for (var key in obj) {
			var keyR = new RegExp('@'+key+'@', 'g');
			htmlStr = htmlStr.replace(keyR, obj[key]);
		}
	}
	$('#틀-' + id).html(htmlStr).css('display','inline');
}

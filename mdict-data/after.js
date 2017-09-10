var dtList = [];
var dtStr = '분류: ';
$('.doctype').each(function(v) { 
	dtList.push($(this).find('a')[0].outerHTML); 
});
for (var i = 0; i < dtList.length; i++) {
	dtStr += dtList[i] + (i === dtList.length - 1 ? '' : ', ');
}
$('.doctype').remove();
if (dtList.length > 0) {
	$('#doctype-list').html(dtStr);
} else {
	$('#doctype-list').remove();
}
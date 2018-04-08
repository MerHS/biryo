var dtList = [];
var dtStr = '분류: ';
$('.dtype').each(function() {
	dtList.push($(this).find('a')[0].outerHTML); 
});
for (var i = 0; i < dtList.length; i++) {
	dtStr += dtList[i] + (i === dtList.length - 1 ? '' : ', ');
}
$('.dtype').remove();
if (dtList.length > 0) {
	$('#doctype-list').html(dtStr);
} else {
	$('#doctype-list').remove();
}
$('.fold dt').click(function(){ $(this).siblings().toggle() })
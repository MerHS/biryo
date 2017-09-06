var dtList = [];
var dtStr = '';
$('.doctype').each(function(v) { 
	dtList.push($(this).find('a')[0].outerHTML); 
});
for (var i = 0; i < dtList.length; i++) {
	dtStr += dtList[i] + ', ';
}
$('.doctype').remove();
$('#doctype-list').html(dtStr);
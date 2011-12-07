
function rstars(a){
	w=document.getElementById('vote_value');
	w.value=a;
	for (i=1;i<=5;i++){
		w=document.getElementById('star'+i);
		if (i>a){ w.src='/img/star1-wb.gif';
	 	} else { w.src='/img/star1.gif'; }
	}
}

function hstars(){
	for (i=1;i<=5;i++){
		w=document.getElementById('star'+i);
		w.src='/img/star1-wb.gif';
	}
}

function cstars(){
	for (i=1;i<=5;i++){
		document.write('<img alt="" id="star'+i+'" src="/img/star1-wb.gif" width="16" height="16" OnMouseOver="rstars('+i+');" OnClick="document.vote_form.submit();" />');
	} 
}

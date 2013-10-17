function dateReplace(toDwarvish){
    var spans=document.getElementsByClassName('bilingual');
    for(var i in spans) {
	var elem = spans[i];
	if (! (elem && elem.dataset)) continue;
	
	if (toDwarvish)
	    elem.innerHTML = elem.dataset['dwarvish'];
	else
	    elem.innerHTML = elem.dataset['english'];
    }
};

window.onload = function() {
    function toggle(){
	is_dwarf=!is_dwarf;
	if (window.localStorage)
	    window.localStorage.setItem('dwarvish',is_dwarf);
	dateReplace(is_dwarf);
    };
    function scroll() {
	var posY=( document.documentElement.scrollTop
		 ? document.documentElement.scrollTop
		 : window.pageYOffset);
	bg.style.backgroundPosition='' + (-posY*(0.01)) + 'px ' + (-posY*(0.1)) + 'px';
    };
    var bg=document.getElementById('bg');
    var is_dwarf=(window.localStorage&&window.localStorage.getItem('dwarvish')=='true');
    var elems=document.getElementsByClassName('bilingual');
    for (var i in elems) {
	elems[i].onclick=toggle;
    }
    dateReplace(is_dwarf);
    scroll();
    
    window.onscroll=scroll;
};


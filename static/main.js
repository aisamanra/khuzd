function toDwarfMonth(t){
    return(t.replace('January','Granite')
	   .replace('February','Slate')
	   .replace('March','Felsite')
	   .replace('April','Hematite')
	   .replace('May','Malachite')
	   .replace('June','Galena')
	   .replace('July','Limestone')
	   .replace('August','Sandstone')
	   .replace('September','Timber')
	   .replace('October','Moonstone')
	   .replace('November','Opal')
	   .replace('December','Obsidian'));
};

function fromDwarfMonth(t) {
    return(t.replace('Granite','January')
	   .replace('Slate','February')
	   .replace('Felsite','March')
	   .replace('Hematite','April')
	   .replace('Malachite','May')
	   .replace('Galena','June')
	   .replace('Limestone','July')
	   .replace('Sandstone','August')
	   .replace('Timber','September')
	   .replace('Moonstone','October')
	   .replace('Opal','November')
	   .replace('Obsidian','December'));
};

function dateReplace(f){
    var spans=document.getElementsByTagName('span');
    for(var i in spans) {
	if(spans[i].className&&spans[i].className.search('date')!== -1)
	    spans[i].innerHTML=f(spans[i].innerHTML);
    }
};

window.onload = function() {
    function toggle(){
	is_dwarf=!is_dwarf;
	if (window.localStorage)
	    window.localStorage.setItem('dwarvish',is_dwarf);
	dateReplace(is_dwarf?toDwarfMonth:fromDwarfMonth);
    };
    function scroll() {
	var posY=( document.documentElement.scrollTop
		 ? document.documentElement.scrollTop
		 : window.pageYOffset);
	bg.style.backgroundPosition='' + (-posY*(0.01)) + 'px ' + (-posY*(0.1)) + 'px';
    };
    var bg=document.getElementById('bg');
    var is_dwarf=(window.localStorage&&window.localStorage.getItem('dwarvish')=='true');
    var spans=document.getElementsByTagName('span');
    for (var i in spans) {
	if (spans[i].className&&spans[i].className.search('date')!==-1)	{ 
	    spans[i].onclick=toggle
	}
    }
    dateReplace(is_dwarf?toDwarfMonth:fromDwarfMonth);
    scroll();
    
    window.onscroll=scroll;
};


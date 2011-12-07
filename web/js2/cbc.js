
//alert(navigator.userAgent);
 
var Opera = (navigator.userAgent.indexOf('Opera') != -1);
var IE = (navigator.userAgent.indexOf('IE') != -1);
var MSIE = (navigator.appVersion.indexOf('MSIE') != -1);
var MSIE7 = (navigator.appVersion.indexOf('MSIE 7.') != -1);
var MSIE6 = (navigator.appVersion.indexOf('MSIE 6.') != -1);
var Mozilla = (navigator.userAgent.indexOf('Mozilla') != -1);

function applyStyles(){
 if (Mozilla && !MSIE){document.write('<link rel="stylesheet" type="text/css" href="/css/css_firefox.css">');}
 if (Opera){document.write('<link rel="stylesheet" type="text/css" href="/css/css_opera.css">');}
 if (MSIE7){document.write('<link rel="stylesheet" type="text/css" href="/css/css_ie7.css">');}
 if (MSIE6){document.write('<link rel="stylesheet" type="text/css" href="/css/css_ie6.css">');}

}

function pullDown(){
 if (Mozilla && !MSIE){
   var w = document.getElementById('main_table');
   w.style.height=(Math.round(window.innerHeight*0.3)+'px');
  }
}


var pluginlink = 'http://ru.active-video.net/downloads/avplayer-avplugins-last.exe';
var servername = location.protocol+'//'+location.host;
var cbutton = servername + '/images/play.png';
var plugin_width = 486;
var plugin_height = 279;
var delay_before_insert = 1000;
var delay_before_show = 1000;
var loading_image = 'images/video-loading.gif';
var noplugin_image = 'images/video-noplugin.png';
var plugin_image = servername+'/images/video-start.png';
var plugin_tooltip = 'смотреть';
var plugin_autoplay = 1;
var plugin_autoloop = 0;
var plugin_playbutton_width = 100;
var plugin_playbutton_height= 100;
var plugin_playbutton_left  = 200;
var plugin_playbutton_top   = 150;

var mplugin_bgimage;
//if (mplugin_bgimage) plugin_image = mplugin_bgimage;
if (mplugin_bgimage) noplugin_image = mplugin_bgimage;
var noplugin_text_locales = new Array();
noplugin_text_locales[1] = 'Для просмотра фильма установите <a href="'+pluginlink+'">плеер</a>.';
noplugin_text_locales[2] = 'Download <a href="'+pluginlink+'">player</a> to play this video.';
var mplugin_language;
var global_plugin_instance;


/*
if(!mplugin_language) mplugin_language = 1;
noplugin_text = noplugin_text_locales[mplugin_language];
document.write('<div id="mplugin_overall_wrap" style="width:'+plugin_width+'px;height:'+plugin_height+'px;overflow:hidden;position:relative;">');
	document.write('<div id="mplugin_noplugin_box" style="visibility:hidden;position:absolute;top:0;left:0;width:'+plugin_width+'px;height:'+plugin_height+'px;background-repeat:no-repeat;background-image:url(\''+noplugin_image+'\');">');
		document.write('<div id="mplugin_noplugin_note" style="position:absolute;bottom:-1px;left:0;width:'+plugin_width+'px;height:50px;background-color:#000000;color:#ffffff;text-align:center">'+noplugin_text+'</div>');
	document.write('</div>');
	document.write('<div id="mplugin_activex_box" style="visibility:hidden;position:absolute;top:0;left:0;width:'+plugin_width+'px;height:'+plugin_height+'px;">');
	document.write('</div>');
	document.write('<div id="mplugin_loading_box" style="visibility:visible;position:absolute;top:0;left:0;background-color:#000000;width:'+plugin_width+'px;height:'+plugin_height+'px;background-repeat:no-repeat;background-image:url(\''+loading_image+'\');">');
	document.write('</div>');
document.write('</div>');

window.setTimeout('checkPlugin();', delay_before_insert);
*/




/* --- */

var notWin = (navigator.userAgent.indexOf('Win') == -1);
var notIE = (navigator.userAgent.indexOf('IE') == -1);
var noPlugin = (!navigator.plugins["Active Video Player"]);
var isMacOs = (navigator.userAgent.indexOf('Mac') != -1);
var isLinux = (navigator.userAgent.indexOf('Linux') != -1);

var p_w = 400;
var p_h = 300;
var p_flags = 0;
var p_parent = 0;
var p_autoplay = 0;
var p_autoloop = 0;
var p_url = '';
var p_skin = 'FHNY2009Plugin ';
var p_uuid = '';

var FORCE_CREATE_WIDGET = 1;
var AUTO_LOOP_WIDGET = 2;
var AUTO_PLAY_WIDGET = 4;
var USE_OWN_STYLE = 8;
var IMPLICIT_SKIN = 16;

function setWidgetUrl(param_url){
	if (param_url) p_url = param_url;
}

function setWidgetUUID(param_uuid){
	if (param_uuid) p_uuid = param_uuid;
}

function parseFlags(){
	if (p_flags&AUTO_PLAY_WIDGET) p_autoplay = 1;
	if (p_flags&AUTO_LOOP_WIDGET) p_autoloop = 1;
}

function invokePlugin(p_parent_id, p_param_flags){
    p_parent = $(p_parent_id);
    if (p_url.length==0) return false;
	if (p_url.indexOf(".sce")<=0) p_url=p_url+"/default.sce";
	p_url.replace(/ /g, "");
	if (!p_parent) return false;
	if (p_parent.length == 0) return false;
	if (p_param_flags) p_flags = p_param_flags;
	p_w = p_parent.width();
	p_h = p_parent.height();
    //p_parent[0].innerHTML = '123';
    parseFlags();
	checkforPlugin();
    bindCallback();
}

function checkforPlugin(){
    if ( p_flags&FORCE_CREATE_WIDGET ){
		embedPlugin();
		return true;
	}
	if(notWin){
	offerPlugin(1);
	return;
	}
	if(notIE){
		if(noPlugin){
			offerPlugin(2);
		} else {
			embedPlugin();
			window.setTimeout('finishPlugin();', delay_before_show);
		}
	} else {
		embedPlugin();
        var plugin_instance = document.getElementById("ntpPlugin");
        if (plugin_instance.mrl){
            bind_IEcallback(plugin_instance);
            window.setTimeout('finishPlugin();', delay_before_show);
		} else {
			offerPlugin(3);
		}
	}
}

function offerPlugin(reason){
	p_parent.empty();
    if ( p_flags&USE_OWN_STYLE ){
		p_parent.append("no plugin found");
	} else {
		var p_npl = $("div.widget_noplugin").clone();
		if (isMacOs) p_npl = $("div.widget_macos").clone();
		if (isLinux) p_npl = $("div.widget_linux").clone();
		$("div.plugin_bg").append( p_npl );
		$("div.quote_area div.pic").append( p_npl );
		p_npl.show();
	}
}


function bindCallback(){
    if (p_parent.find("object").length==0) return false;
    if (notIE){
        global_plugin_instance = p_parent.find("object")[0];
        global_plugin_instance.signal_What = function(){ stepPlaylist(); };
    }
}


function codePlugin(){
	var cd = "";
	cd += '<object id="ntpPlugin" width="'+p_w+'" height="'+p_h+'" type="application/active-video-mime" skin="'+p_skin+'" UserID="'+p_uuid+'">\n';
	
	var mrl = '<param name="mrl" value="';
	
	if(p_autoplay==0) mrl+= 'playbutton:backgroundpicture='+plugin_image+';tooltip='+plugin_tooltip+';x='+plugin_playbutton_left+';y='+plugin_playbutton_top+';w='+plugin_playbutton_width+';h='+plugin_playbutton_height+';buttonpicture='+cbutton+';media=';
	
	mrl+= p_url+'">';
	
	cd += mrl;
	cd += '<param name="skin" value="'+p_skin+'" />';
	cd += '<param name="UserID" value="'+p_uuid+'" />';
	cd += '<param name="autoplay" value="1" />';
	//cd += '<param name="autoloop" value="1" />';
	cd += '</object>';
	return cd;
}


function DOMPlugin(){
	var dp = document.createElement('object');
	var param_mrl = document.createElement('param');
	var param_skin = document.createElement('param');
	var param_uuid = document.createElement('param');
	var param_autoplay = document.createElement('param');
	var param_autoloop = document.createElement('param');
	dp.setAttribute('id', 'ntpPlugin');
	dp.setAttribute('width', p_w);
	dp.setAttribute('height', p_h);
	dp.setAttribute('skin', p_skin);
	dp.setAttribute('UserID', p_uuid);
	dp.setAttribute('type', 'application/active-video-mime');
	
	var mrl_value = '';
	if(p_autoplay==0) mrl_value+= 'playbutton:backgroundpicture='+plugin_image+';tooltip='+plugin_tooltip+';x='+plugin_playbutton_left+';y='+plugin_playbutton_top+';w='+plugin_playbutton_width+';h='+plugin_playbutton_height+';buttonpicture='+cbutton+';media=';
	mrl_value+= p_url;

	param_mrl.setAttribute('name', 'mrl');
	param_mrl.setAttribute('value', mrl_value);
	param_autoplay.setAttribute('name', 'autoplay');
	param_autoplay.setAttribute('value', '1');
	param_autoloop.setAttribute('name', 'autoloop');
	param_autoloop.setAttribute('value', p_autoloop);
	param_skin.setAttribute('name', 'skin');
	param_skin.setAttribute('value', p_skin);
	param_uuid.setAttribute('name', 'UserID');
	param_uuid.setAttribute('value', p_uuid);
	dp.appendChild(param_mrl);
	dp.appendChild(param_autoplay);
	//dp.appendChild(param_autoloop);
	dp.appendChild(param_skin);
	dp.appendChild(param_uuid);
	return dp;
}

function finishPlugin(){
	return true;
}

function embedPlugin(){
	p_parent.empty();
	//if (notIE) p_parent[0].appendChild( DOMPlugin() );
	//else p_parent[0].innerHTML = codePlugin();
	p_parent[0].innerHTML = codePlugin();
}
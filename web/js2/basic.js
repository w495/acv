
var __username;
var __session;
var video_quotes;
var banners;
var banner_cycle;

function getQuerystring(key, default_)
{
  if (default_==null) default_="";
  key = key.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
  var regex = new RegExp("[\\?&]"+key+"=([^&#]*)");
  var qs = regex.exec(window.location.href);
  if(qs == null)
    return default_;
  else
    return qs[1];
}



function push_element(a, e){
    for(j=0;j<=a.length-1;j++){
        if(a[j]==e) return a;
    }
    a.push(e);
    a.sort();
    return a;
}



function drop_element(a, e){
    var b = [];
    for(j=0;j<=a.length-1;j++){
        if(a[j]!=e) b.push(a[j]);
    }
    b.sort();
    return b;
}


function createCookie(name,value,days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
	var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function eraseCookie(name) {
	createCookie(name,"",-1);
}


    function authorizeShow(url){
        if (!url || url.length==0){
        $("img#play_button").remove();
        $("div.plugin_box").empty();
        $("div.plugin_bg").empty();
        $("div.plugin_bg").append($("div.widget_noauth").clone());
        $("div.quote_area div.pic a").remove();
        $("div.quote_area div.pic").append($("div.widget_noauth").clone());
        }
    }


    function stepPlaylist(){
        if ($("form.follow_url").length>0) $("form.follow_url")[0].submit();
    }


$(document).ready(function(){


    // make actual links
    /*
    $("a.rebuild").each(function(){
        return false;
        var base_link = location.href;
        var cats = [];
        var tags = [];
        var sort_method = '';
        cats = getQuerystring("cats").split(",");
        tags = getQuerystring("tags").split(",");
        sort_method = getQuerystring("sort");
        letter = getQuerystring("letter");
        page = getQuerystring("page");
 
        if (tags[0]=='') tags=[]
        if (cats[0]=='') cats=[];
     
        if (base_link.indexOf("?")>-1) var base_link = base_link.substring(0,base_link.indexOf("?"));
        if (this.href.indexOf("/")>-1) var addon = this.href.substring(this.href.lastIndexOf("/")+1);
        else var addon=this.href;
    
        if (addon.indexOf('cats+')>-1) cats=[addon.substring(5)];
        if (addon.indexOf('cats-')>-1) cats=drop_element(cats, addon.substring(5));
        if (addon.indexOf('cats***')>-1) cats=[];
        if (addon.indexOf('cats')>-1){
            letter = "";
            page = "";
        }
        if (addon.indexOf('tags+')>-1) tags=push_element(tags, addon.substring(5));
        if (addon.indexOf('tags-')>-1) tags=drop_element(tags, addon.substring(5));
        if (addon.indexOf('tags***')>-1) tags=[];
        
        if (cats.length==0) cats='';
        else cats="cats="+cats.join(",");
        if (tags.length==0) tags='';
        else tags="tags="+tags.join(",");
        if (sort_method.length==0) sort_method='';
        else sort_method="sort="+sort_method;
        if (letter.length==0) letter='';
        else letter="letter="+letter;
        if (page.length==0) page='';
        else page="page="+page;
        
        base_link = '/video/catalog/';
        this.href=base_link+"?"+[cats,tags,sort_method,page,letter].join("&");
        this.href = this.href.replace(/\&\&/g,"&");
        this.href = this.href.replace(/\?\&/g,"?");
        this.href = this.href.replace(/\&*$/g,"");
    });
    */

    function requestSession(){
    $.get("/session", {seed: Math.round(Math.random()*10000)}, function(data, textStatus){
            __session = readCookie('vlog-ssid');
        }, "text");    
    }


    $("div.my_select").click(function(){
        var origin = $(this).children("span");
        $("div.my_select div.dropitem").unbind();
        origin.hide();
        if (origin.siblings("select").length==0) return 0;
        var id = origin.siblings("select")[0].id;
        var v = origin.siblings("select").children("option");
        var b = '<div class="dropdown">';
        var factor = ["odd", "even"];
        for (j=0;j<=v.length-1;j++){
            
            b+='<div class="dropitem '+id+' '+factor[j%2]+'">'+$(v[j]).text()+'<input type="hidden" value="'+$(v[j]).val()+'"></div>';
        }
        b+='</div>';
        origin.parent().append(b);
        
        $("div.my_select div.dropitem").click(function(){
            $(this).parents("div.dropdown").parents("div.my_select").children("span").text($(this).text());
            $(this).parents("div.dropdown").parents("div.my_select").children("input").val($(this).children("input").val());
            $(this).parents("div.dropdown").parents("div.my_select").children("span").css("display","inline");
            $("div.my_select div.dropdown").remove();
            var mode = $(this).children("input").val();
            if (!mode) mode="";
            if  ($(this).hasClass("theme")){
                var a = $("a.theme_button")[0];
                a.href=a.href.replace('theme=','theme='+mode);
                location.href=a.href;
            }
            if  ($(this).hasClass("sort")){
                var a = $("a.sort_button")[0];
                a.href=a.href.replace('sort=','sort='+mode);
                location.href=a.href;
            }
            if  ($(this).hasClass("playlist_select")){
                location.href="/video/playlist/?list="+mode;
            }
            return false;

        });
    });

    $("a.sort_button").click(function(){
    var mode = $(this).siblings("div.my_select").children("input").val();
    if (!mode) mode="last";
    this.href=this.href.replace('sort=','sort='+mode);
    return true;
    });


    $("input.remove_from_favorites").click(function(){
        //$.post("/action/remove-video-from-favorites/", { id: $(this).find("input").val() } );
        var checkboxes = $("input.removable_clip_checkbox");
        for (j=0;j<=checkboxes.length;j++){
            if ($(checkboxes[j]).attr("checked")){
                var c = $(checkboxes[j]);
                var p = c.parents(".personal_item");
                var id = p.find("b.remove_from_favorites input").val();
                $.post("/action/remove-video-from-favorites/", { id: id, seed: Math.round(Math.random()*10000) } );
                p.remove();
            }
        }
    });
    $("b.remove_from_favorites").click(function(){
        $.post("/action/remove-video-from-favorites/", { id: $(this).find("input").val(), seed: Math.round(Math.random()*10000) } );
        $(this).hide();
        $(this).siblings("b.add_to_favorites").show();
        $(this).parents("table.user_list_item").css("background-color", "#eee");
    });
    $("b.add_to_favorites, img.add_to_favorites").click(function(){
        $.post("/action/add-video-to-favorites/", { id: $(this).siblings("input").val(), seed: Math.round(Math.random()*10000) } );
        $(this).hide();
        $(this).siblings("b.remove_from_favorites").show();
        $(this).parents("table.user_list_item").css("background-color", "#fff");
    });
        $("div.add_to_favorites").click(function(){
        $.post("/action/add-video-to-favorites/", { id: $(this).children("input").val(), seed: Math.round(Math.random()*10000) } );
        $(this).hide();
    });

    $("div.remove_from_favorites").click(function(){
        $.post("/action/remove-video-from-favorites/", { id: $(this).find("input").val(), seed: Math.round(Math.random()*10000) } );
        location.href="/video/favorites/";
    });

    function setLoggedState(state){
        if (state==true){
            $(".__username").text(__username);
            $(".unlogged").hide();
            $(".loggedin").show();
        } else {
            $(".loggedin").hide();
            $(".unlogged").show();
        }
    }

        $("img#play_button").click(function(){
            invokePlugin("div.pluginbox", AUTO_PLAY_WIDGET);
            $.post("/action/watch-video/", { id: clip_id, seed: Math.round(Math.random()*10000) } );
            $("img#play_button").hide();
            //$("div.pluginbox").empty();
            //$("div.plugin_bg").remove();
        });

    $("b.postpost").click(function(){
        stepPlaylist();
    });

    function nextPlaylistItem(playlist_queue){
        if (playlist_queue.length<1) return false;
        $("form.follow_url").remove();
        var fff='<form action="/show-clip/'+clip_id+'" method="post" style="display:none" class="follow_url">';
        for(j=0;j<=playlist_queue.length-1;j++){
            fff+='<input type="hidden" name="item'+(j+1)+'" value="'+playlist_queue[j]+'" />';
        }
        fff+='</form>';
        $("body").append(fff);
        if ($("form.follow_url").length>0) $("form.follow_url")[0].submit();
    }

    testTest=function(){
    alert('test raised');
    }

    function renderVideoQuotes(){
        if (!video_quotes) return false;
        if (video_quotes.length<1) return false;
        $("span.quote_place").empty();
        $("div.quote_area").empty();
        j=0;
        //$("div.quote_place").append('<div class="video_quote"><img class="video_quote_bg" src="/cache/'+video_quotes[j].id+'-cpw.jpg"><div class="video_quote_area"></div><div class="video_quote_name">'+video_quotes[j].name+'</div><div class="video_quote_text">'+video_quotes[j].text+'</div><img src="/img/play.png" class="video_quote_play"/><input type="hidden" value="'+video_quotes[j].url+'"></div>');
        
        var ci2 = '<div class="pic"><a href="/show-clip/'+video_quotes[j].id+'"><div class="pseudo_img" style="background-image:url(';
        ci2+= "'/cache/"+video_quotes[j].id+"-cpw.jpg')";
        ci2+='"></div></a></div><div class="quote_bg">';
        ci2+='<img src="/m_images/play-button.png" class="video_quote_play" alt="" /><input type="hidden" value="'+video_quotes[j].id+'" />';
        ci2+='</div>';
        var ci = '<div class="pic"><a href="/show-clip/'+video_quotes[j].id+'"><img alt="" src="/cache/'+video_quotes[j].id+'-cpw.jpg"></a><div class="nw label"><div class="nw shade"></div> <div class="title">'+video_quotes[j].name+'</div><div class="subtitle">'+video_quotes[j].country+'</div></div></div>';
        ci+='<div class="synopsis">';

        if(video_quotes[j].country.length>0) ci+='Производство: '+video_quotes[j].country;
        if(video_quotes[j].year.length>0) ci+=', '+video_quotes[j].year+' год<br />' ;
        ci+='<br />';
        
        if(video_quotes[j].director.length>0) ci+='Режиссер: '+video_quotes[j].director+'<br />';
        if(video_quotes[j].actors.length>0){
            ci+='В ролях: ';
            for(m=0;m<=video_quotes[j].actors.length-1;m++){
                ci+='<span>'+video_quotes[j].actors[m].name+'</span>';
                if (m!=video_quotes[j].actors.length-1) ci+=', ';
            }
        }
        ci+='<br /><br />'+video_quotes[j].text+'</div>';
        ci+='<img src="/images/play-button.png" class="video_quote_play" alt="" /><input type="hidden" value="'+video_quotes[j].url+'" />';
        $("span.quote_place").append(ci);
        $("div.quote_area").append(ci2);
        //$("span.quote_place").html('<div style="color:#fff;font-size:100px;">!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#</div>');
        //alert($("span.quote_place").html());        
        $("span.quote_place img.video_quote_play").unbind();
        $("span.quote_place img.video_quote_play").click(function(){
            $(this).hide();
            var url = $(this).siblings("input").val()
            setWidgetUrl(url);
            invokePlugin("div.quoter_box div.pic", AUTO_PLAY_WIDGET);
        });
        $("div.quote_area img.video_quote_play").unbind();
        $("div.quote_area img.video_quote_play").click(function(){
            $(this).hide();
            var clip_id = $(this).siblings("input").val()
            $("div.quote_area div.quote_bg").remove();
            $.get("/action/get-clip-url/"+clip_id, {seed: Math.round(Math.random()*10000)}, function(data, textStatus){
                clip_url=data;
                authorizeShow(clip_url);
                setWidgetUrl(clip_url);
                invokePlugin("div.quote_area div.pic", AUTO_PLAY_WIDGET);
            }, "text");
        });
        $("span.quote_place div.pic img").unbind();
        $("span.quote_place div.pic img").click(function(){
            //alert(5);
            //location.href=$(this).parents("a")[0].href;
        });
    window.setTimeout(' $("div.quote_block img.next").trigger("click") ', 5000);
    }

/*
    $("div.quoter_box img.prev, div.quote_block img.prev").click(function(){
        if (!video_quotes) return false;
        if (video_quotes.length<2) return false;
        var t = video_quotes[0];
        for(j=1;j<=video_quotes.length-1;j++){
            video_quotes[j-1]=video_quotes[j];
        }
        video_quotes[video_quotes.length-1]=t;
        renderVideoQuotes();
    });

    $("div.quoter_box img.next, div.quote_block img.next").click(function(){
        if (!video_quotes) return false;
        if (video_quotes.length<2) return false;
        var t = video_quotes[video_quotes.length-1];
        for(j=video_quotes.length-1;j>=1;j--){
            video_quotes[j]=video_quotes[j-1];
        }
        video_quotes[0]=t;
        renderVideoQuotes();
    });
*/


    $("div.quoter_box img.prev, div.quote_block img.prev").click(function(){
        if (!banners) return false;
        if (banners.length<2) return false;
        var t = banners[0];
        for(j=1;j<=banners.length-1;j++){
            banners[j-1]=banners[j];
        }
        banners[banners.length-1]=t;
        renderBanners();
    });

    $("div.quoter_box img.next, div.quote_block img.next").click(function(){
        if (!banners) return false;
        if (banners.length<2) return false;
        var t = banners[banners.length-1];
        for(j=banners.length-1;j>=1;j--){
            banners[j]=banners[j-1];
        }
        banners[0]=t;
        renderBanners();
    });


    function renderBanners(){
        if (!banners) return false;
        if (banners.length<1) return false;
        $(".quote_area div.pic").fadeOut(500, function(){        
            $("span.quote_place").empty();
            $("div.quote_area").empty();
            var ci='<div class="pic"><a href="'+banners[0].link+'"><div class="pseudo_img" style="background-image:url(';
            ci+="'/cache/banner/"+banners[0].image+"');";
            ci+='"></div></a></div>';
            $("div.quote_area").append(ci);
            window.clearTimeout(banner_cycle);
            banner_cycle=window.setTimeout(' $("div.quote_block img.prev").trigger("click") ', 5000);
       });
    }



    $("div.open_playlist_tab img, div.add_to_playlist").click(function(){
    $("div.playlist_tab").empty();
    $("div.open_playlist_tab b").remove();
        $.getJSON("/action/get-playlists/", {seed: Math.round(Math.random()*10000)}, function(data){
            if (data=="") location.href="/video/playlist";
            var b = '<div class="dropdown">';
            var factor = ["odd", "even"];
            for(j=0;j<=data.length-1;j++){
                b+='<div class="dropitem '+factor[j%2]+' item'+j+'">'+data[j].name+'<input type="hidden" value="'+data[j].list_id+'"></div>';
                //$("div.open_playlist_tab").append("<li>"+data[j].name+"<input type=hidden value='"+data[j].list_id+"' /></li>");
            }
            b+='</div>';
            $("div.playlist_tab").append(b);

            $("div.open_playlist_tab li, div.playlist_tab div.dropitem").unbind();
            $("div.open_playlist_tab li, div.playlist_tab div.dropitem").click(function(){
                $.post("/action/add-to-playlist/", {list: $(this).find("input").val(), clip: clip_id, seed: Math.round(Math.random()*10000)}, function(){
                    $("div.playlist_tab").empty();
                    $("div.open_playlist_tab li").remove();
                    $("div.open_playlist_tab").append("<b><br />Фильм добавлен!</b>");
                    $("body").append("<div class='tmp_ok' style='position:absolute;top:50%;left:50%;font-size:10px;color:#000;'>OK!</div>");
                    $("div.tmp_ok").animate({fontSize:'500px',marginLeft:'-350px',marginTop:'-250px',opacity:'0'},400,function(){ $(this).remove(); });
                    return false;
                });
            });
        });
    });

    

    $("input.remove_from_playlist").click(function(){
        //$.post("/action/remove-video-from-favorites/", { id: $(this).find("input").val() } );
        var checkboxes = $("input.removable_clip_checkbox");
        for (j=0;j<=checkboxes.length;j++){
            if ($(checkboxes[j]).attr("checked")){
                var c = $(checkboxes[j]);
                var p = c.parents(".playlist_video_item");
                $.post("/action/remove-video-from-playlist/", {list: p.find("b.remove_video input.pl_video_l").val(), position: p.find("b.remove_video input.pl_video_p").val(), seed: Math.round(Math.random()*10000)});
                p.remove();
            }
        }
    });

    $("input.remove_preferences").click(function(){
        var checkboxes = $("input.removable_preference_checkbox");
        for (j=0;j<=checkboxes.length;j++){
            if ($(checkboxes[j]).attr("checked")){
                var c = $(checkboxes[j]);
                var p = c.parents(".preference_item");
                $.post("/action/delete-preference/", {id: p.find("b.remove_preference input").val(), seed: Math.round(Math.random()*10000)});
                p.remove();
            }
        }
    });
    $("b.remove_preference").click(function(){
         var p = $(this).parents(".preference_item");
         $.post("/action/delete-preference/", {id: $(this).find("input").val(), seed: Math.round(Math.random()*10000)});
         p.remove();
    });


    $(".nav_lurker1").click(function(){
        $(this).parents("div.item").next("div.inner_cat").toggle();
    });


    $("b.raise_video").click(function(){
        $.post("/action/raise-video-in-playlist/", {list: $(this).find("input.pl_video_l").val(), position: $(this).find("input.pl_video_p").val(), seed: Math.round(Math.random()*10000)});
        location.reload();        
        //var cc = $(this).parents("table.playlist_video_item");
        //var pc = cc.prev("table.playlist_video_item");
        //cc.after(pc);
    });
    $("b.lower_video").click(function(){
        $.post("/action/lower-video-in-playlist/", {list: $(this).find("input.pl_video_l").val(), position: $(this).find("input.pl_video_p").val(), seed: Math.round(Math.random()*10000)});
        location.reload();
        //var cc = $(this).parents("table.playlist_video_item");
        //var nc = cc.next("table.playlist_video_item");
        //nc.after(cc);
    });
    $("b.remove_video").click(function(){
        $.post("/action/remove-video-from-playlist/", {list: $(this).find("input.pl_video_l").val(), position: $(this).find("input.pl_video_p").val(), seed: Math.round(Math.random()*10000)});
        location.reload();
        //var cc = $(this).parents("table.playlist_video_item");
        //cc.remove();
    });
    $("input.cat_tree_input").change(function(){
        var parents = $(this).parents("div.cat_tree_wrap").children("input.cat_tree_input");
        if ($(this).attr("checked")) parents.attr("checked", "true");
    });
    $("b.deselect_radiobuttons").click(function(){
        $("input[type='radio']").removeAttr("checked");
    });
    $("div.mark_list td div b").click(function(){
        $(this).hide();
        $(this).after('<input class="edit_mark" style="border:none;color:#900;width:140px;" type="text" name="name" value="'+$(this).text()+'" /><input type="button" class="commit_mark" style="font-size:8pt;width:35px;" value="OK" />');
        //location.reload();
        //var cc = $(this).parents("table.playlist_video_item");
        //cc.remove();
        //alert(5);
        $("input.commit_mark").unbind();
        $("input.commit_mark").click(function(){
            $.post("/action/edit-mark-name/", {id: $(this).siblings("input.mark_id").val(), name: $(this).siblings("input.edit_mark").val(), seed: Math.round(Math.random()*10000)}, function(response){
                if (response!=''){
                    $("input#tag_"+response).siblings("b").text($("input#tag_"+response).siblings("input.edit_mark").val());
                    $("input#tag_"+response).siblings("input.edit_mark").remove();
                    $("input#tag_"+response).siblings("input.commit_mark").remove();
                }
            });
            $(this).hide();
            $(this).siblings("input.edit_mark").hide();
            $(this).siblings("b").show();
        });
});
    


   // render login state
    __username = readCookie('username');
    if (!__username) setLoggedState(0);
    else{
        if (__username.length){
            if (__username.length>0) setLoggedState(1);
            else setLoggedState(0);
        } else setLoggedState(0);
    }

    __session = readCookie('vlog-ssid');
    if (!__session) requestSession();
    else{
        if (__session.length){
            if (__session.length<1) requestSession();
        } else requestSession();
    }
        
    //setLoggedState(__username.length>0);

   //$("ul.tabs").tabs("div.panes > div"); 
   //renderVideoQuotes();
   renderBanners();
   
   //alert( $("body").width() );
   //$("body").css("height", $("div.footer_box").offset().top+$("div.footer_box").height()+"px");

    var IE6 = (navigator.userAgent.indexOf('MSIE 6.0') != -1);
    if (IE6){
        $(".osw, .ose, .onw, .one, .sw, .se, .nw, .ne").remove();
    }    

    $("div.backgrounder").css("height", $("div.footer_box").offset().top-$("div.header").height()+"px");
    $("div.backgrounder_bottom").css("top", $("div.footer_box").offset().top-$("div.backgrounder_bottom").height()+"px");
    $("div.footer_shade").css("top", $("div.footer_box").offset().top+"px");
    if (location.pathname.indexOf("login")==-1) $("input#return_url").val(location.href);
    
    // weird opera thing
    $("div.backgrounder table").css("width", "100.01%");
    
      
   //$("div.backgrounder").css("width", document.body.offsetWidth+"px");
    //new_W = Math.min(1920, document.body.offsetWidth);
   //h1 = $("div.a1").width()+"px";
   //$("body").css("width", new_W+"px");
   //alert(document.body.offsetWidth);
   


});
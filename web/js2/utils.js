 jQuery.fn.check = function(mode) {
   // if mode is undefined, use 'on' as default
   var mode = mode || 'on';
   
   return this.each(function() {
     switch(mode) {
       case 'on':
         this.checked = true;
         break;
       case 'off':
         this.checked = false;
         break;
       case 'toggle':
         this.checked = !this.checked;
         break;
     }
   });
 };



replace_placeholders = function( html,  chunks ) {
    h = html;
    for( x in h.match(/:(\d+)/g) ) {
        h = h.replace( ':'+x, chunks[x] );
        h = h.replace( '::', ':');
    }
    return h;
}


ignoreEvent = function (ev) {
    if (ev && ev.preventDefault) {
        ev.preventDefault();
        ev.stopPropagation();
    } else if (typeof(event) != 'undefined') {
        event.cancelBubble = false;
        event.returnValue = false;
    }
}

getAttribute = function (dom, key) {
    try {
        return dom.getAttribute(key);
    } catch (e) {
        return null;
    }
}

doXHTMLRequest = function (url, callback) {
    var req = getXMLHttpRequest();
    if (req.overrideMimeType) {
        req.overrideMimeType("text/xml");
    }
        req.open("GET", url, true);
    return sendXMLHttpRequest(req) 
}


toggleInput = function(id) {
	$('#'+id).check('toggle');
}

toggleDisplay = function (id) {
    $('#'+id).toggle()
}

ScrollTo = function (id) {
    var opt = {};
    var off = {};
	el = $('#'+id);
	if(el) {
		el.offset(opt, off);
		window.scrollTo(0, off.top - 30);
	}
}

tracker_page_startup = function () {
	var opt = {};
	var off = {};
	var pb = $('#property_box');
	if ( pb.is('div') ) {
		pb.offset(opt, off);
		var bottom = off.top + pb.height() + 10;
		var proposal = $('#proposal');
		proposal.offset(opt, off);
		if( off.top < bottom ) {
			$('#description').height('22em');
		}
	}

	Nifty('div.taskitem,div#left_side,div.toolbar', 'small');
	Nifty('h3.rounded,h4.rounded,div.rounded', 'small');
	Nifty('div.message_header', 'small');
    
	if(scroll_to != null) {
		ScrollTo(scroll_to);
	}
}


sendReply = function () {
//    $('#send_reply_form').attr('action', '/account/do-reply-on-message');
//	$('#yoffset').attr('value', window.pageYOffset);
	$('#send_reply_form').submit();
}

removeMessage = function () {
	if( confirm("Are you sure to remove this message?") ) {
		$('#message_actions_form').attr('action', '/account/do-remove-message');
		$('#message_actions_form').submit();
	}
}



deletePapers = function () {
        $('#manage_papers_form').attr('action', '/admin/papers/do-remove-papers');
        $('#manage_papers_form').submit();
}

publishPapers = function () {
        $('#manage_papers_form').attr('action', '/admin/papers/do-publish-papers');
        $('#manage_papers_form').submit();
}

unpublishPapers = function () {
        $('#manage_papers_form').attr('action', '/admin/papers/do-unpublish-papers');
        $('#manage_papers_form').submit();
}

addPtag = function () {
        $('#manage_ptags_form').attr('action', '/admin/ptags/do-add-ptag');
        $('#manage_ptags_form').submit();
}

deletePtags = function () {
        $('#manage_ptags_form').attr('action', '/admin/ptags/do-remove-ptags');
        $('#manage_ptags_form').submit();
}


replyOnMessage = function (id) {
    $('#'+id+' .message_body .markdown').hide();
    $('#'+id+' .message_body .message_reply_wrap').show();
}

cancelReplyOnMessage = function (id) {
    $('#'+id+' .message_body .message_reply_wrap').hide();
    $('#'+id+' .message_body .markdown').show();
}

deleteProposal = function (id) {
    if(confirm('Вы точно хотите удалить данное предложение?')) {
        var f = $('#'+id+' .message_form');
        f.attr('action', '/account/proposal/del');
        f.submit();
    }
}


deleteMessage = function (id) {
    if(confirm('Вы точно хотите удалить данное сообщение?')) {
        var f = $('#'+id+' .message_form');
        f.attr('action', '/account/inbox/do-remove');
        f.submit();
    }
}

doReplyOnMessage= function (id) {
    var f = $('#'+id+' .message_form');
    f.attr('action', '/account/inbox/do-reply');
    f.submit();
}


doReplyOnDashboardMessage= function (id) {
    var f = $('#'+id+' .message_form');
    f.attr('action', '/account/projects/task-dashboard/do-reply');
    f.submit();
}

doReplyOnProposal = function (id) {
    var f = $('#'+id+' .message_form');
    f.attr('action', '/account/proposal/do-reply');
    f.submit();
}


doReplyOnComment = function (id, paper_id) {
    var f = $('#'+id+' .message_form');
    f.attr('action', '/blog/do-send-comment/'+paper_id);
    f.submit();
}

acceptProposal = function (id) {
    var f = $('#'+id+' .message_form');
	f.attr('action', '/account/projects/do-accept-proposal');
	f.submit();
}

approveTask = function () {
        $('#approve_task_form').attr('action', '/admin/task/do-approve-tasks');
        $('#approve_task_form').submit();
}

disapproveTask = function () {
        $('#approve_task_form').attr('action', '/admin/papers/do-disapprove-tasks');
        $('#approve_task_form').submit();
}

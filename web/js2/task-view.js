
taskStatus = [];
ticker = null;
tbody = null;


toggleSubtasks = function (ev) {
    ignoreEvent(ev);
    id = ev.target.id.split("_")[1];
    if( taskStatus[id].loaded ) {
        ch = []
        to_rm = []
        ch = ch.concat( taskStatus[id].children );
        taskStatus[id].children = [];
        while( ch.length ) {
            num = ch.pop();
            if( taskStatus[num].row ) {
                to_rm.push( taskStatus[num].row );
                taskStatus[num].row = null;
            }
            ch = ch.concat( taskStatus[num].children );
            taskStatus[num].children = []; 
        }
        while( to_rm.length ) {
            node = to_rm.pop();
            tbody.removeChild(node);
            delete node;
        }
        taskStatus[id].loaded = false;
        return;
    }
    doXHTMLRequest('/'+ticker+'/tasks/sub_tasks?task_id='+id).addCallback(loadSubTasks);
}

loadSubTasks = function (req) {
    xml = req.responseXML;

    var pr = xml.getElementsByTagName('parent')[0];
    var pid = null;
    if(pr)
        pid = scrapeText(pr.getElementsByTagName('id')[0]);

    if(!pid)
        return;

    taskStatus[pid].loaded = true;

    var i=0;
    rows = tbody.getElementsByTagName('TR');
    for(i=0; i<rows.length; i++)
        if(rows[i].id == 'row_'+pid)
            break;

    forEach( xml.getElementsByTagName('item'), function (node)
    {
        id = scrapeText(node.getElementsByTagName('id')[0]);
        title = scrapeText(node.getElementsByTagName('title')[0]);
        row = tbody.insertRow(++i);
        row.id = 'row_' + id;
        params1 = {
            href:('/'+ticker+'/tasks'),
            onclick:toggleSubtasks,
            id:'tglbtn_'+id,
            'class':'toggle-subtasks-button'
        };
        params2 = params1
        params2['id'] = 'tglttl_'+id;
        params2['class'] = 'toggle-subtasks-title';
        td   = TD( null, A(params1, '+'));
        td1  = TD( null, A(params2, title));
        taskStatus[pid]["children"].push(id);
        initTask(id);
        appendChildNodes(row, [td, td1]);
    });
    updateRowNums();
}


initTask = function (id) {
    taskStatus[id] = {loaded:false, children:[], row:-1};
    updateRowNums();
}

updateRowNums = function () {
    var rowNum = 0;
    if( !tbody )
        return;
    forEach( tbody.getElementsByTagName('TR'), function (node) {
        s = getAttribute(node, 'id');
        id = s.split('_')[1];
        taskStatus[id].row = node;
    });
}

initTaskPage = function(t) {
    ticker = t;
    tbody  = $('task_tbl').getElementsByTagName('tbody')[0];
    var elems = getElementsByTagAndClassName("A", null);
    forEach( elems, function (node) {
        klz = getAttribute(node, 'class');
        if('toggle-subtasks-title'==klz||'toggle-subtasks-button'==klz) 
            node.onclick = toggleSubtasks;
    });
}


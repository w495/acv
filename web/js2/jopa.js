function doXHTMLRequest(url, callback) {
    var req = getXMLHttpRequest();
    if (req.overrideMimeType) {
        req.overrideMimeType("text/xml");
    }
    req.open("GET", url, true);
    return sendXMLHttpRequest(req) 
}

show_sub_tasks = function(ticker, id) {
    doXHTMLRequest('/'+ticker+'/tasks/sub_tasks?task_id='+id).addCallback(load_sub_tasks);
}

load_sub_tasks = function (req) {
    xml = req.responseXML;
    tbody  = $('task_tbl').getElementsByTagName('tbody')[0];

    var pr = xml.getElementsByTagName('parent')[0];
    var pid = null;
    if(pr)
        pid = scrapeText(pr.getElementsByTagName('id')[0]);

    if(!pid)
        return;

    var i=0;
    rows = tbody.getElementsByTagName('tr');
    for(i=0; i<rows.length; i++)
        if(rows[i].id == 'row_'+pid)
            break;

    forEach( xml.getElementsByTagName('item'), function (node)
    {
        id = scrapeText(node.getElementsByTagName('id')[0]);
        title = scrapeText(node.getElementsByTagName('title')[0]);
        row = tbody.insertRow(++i);
        row.id = 'row_' + id;
        url  = "javascript:show_sub_tasks('" + 'FOOBAR' + "', '" + id + "')";
        html = "<td>+</td><td><a href=\""+url+"\">" + title + "</a>";
        row.innerHTML = html;
    });
}

add_row = function(req) {

randint = function(n) {
    return Math.floor(Math.random()*n);
}
ROW = createDOMFunc('tr');
CELL = createDOMFunc('td');
newRow = function(cells) {
    return ROW({'type':'name'}, map(partial(CELL, null), cells));
};

fn = ['John','Jane','Jim','Jill','June'][randint(5)];
ln = ['Wu','Williams','Wegner','Wulu','Watanabi'][randint(5)];
dn = ['gnosis.cx','google.com','gmail.com',
           'gnu.org','groklaw.net'][randint(5)];


appendChildNodes($('task_tbl').getElementsByTagName('tbody')[0], newRow([fn, dn]) )

}

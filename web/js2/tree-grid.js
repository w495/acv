

XmlTreeGrid = function (id) {

    this.table_id = id;
    this.table    = null;
    this.thead    = null;
    this.tbody    = null;
    this.tr       = null;
    this.urlTplGen = new Object();
    this.rows      = null;
    this.selectedRowId = null;
    this.onClickClass_ = null;
    this.rowsIndex = [];
    this.rowId_ = function (cells) { return 0; }
    this.indentStep  = 1.5;
    this.indentUnits = 'em'; 

    this.onInit = function (url) {
        this.initUrl = url;
    }

    this.rowId = function ( func ) {
        this.rowId_ = func;
    }

    this.onClickClass = function (kls) {
        this.onClickClass_ = kls;
    }

    this.urlTemplate = function (klass, action) {
        this.urlTplGen[klass] = action;
    }

    this.init  = function () {
        this.table = $(id);
        if( !this.table )
            return;
        try {
            this.thead = this.table.getElementsByTagName('THEAD')[0];
        }
        catch(e) {
            this.thead = null;
        }
        try {
            this.tbody = this.table.getElementsByTagName('TBODY')[0];
        }
        catch(e) {
            this.tbody = null;
        }
        try {
            tr = this.tbody.getElementsByTagName('TR')[0];
            this.tr = tr.cloneNode(true);
            this.tbody.removeChild(tr);
        }
        catch(e) {
            this.tr = null;
        }
        if( !this.thead || !this.tbody || !this.tr )
            return;
        
        this.doRequest(this.initUrl, this.initGridData);
    }

    this.loadRows = function ( rows, current ) {
        var self = this;
        forEach( rows, function (row) {
            html = replace_placeholders(self.tr.innerHTML, row);
            rrow = createDOM("TR");
            if( !current )
                appendChildNodes(self.tbody, rrow);
            else
                insertSiblingNodesAfter(current, rrow);
            rrow.innerHTML = html;
            self.initializeRow( rrow, row, current );
        });
    }

    this.initGridData = function ( self ) {
        self.loadRows( self.rows, null );
        self.afterLoad();
    }

    this.afterLoad = function () {
        if( this.onClickClass_ ) {
            //alert( this );
            var self = this;
            forEach( getElementsByTagAndClassName(null, this.onClickClass_), function (el) {
                el.onclick = function (e) {
                    ignoreEvent(e);
                    self.clickHandler(e.target);
                }
            });
        }
        this.colorize();      
    }

    this.colorize = function () {
        var i = 0;
        klz = ['row-even','row-odd'];
        var self = this;
        forEach( this.tbody.getElementsByTagName('TR'), function (el) {
            sel = el.getAttribute('class');

            el.setAttribute('class', klz[i%2]);

            if( el.id == self.selectedRowId ) {
                el.setAttribute('class', klz[i%2]+'-selected');
            }

            i++;
            td = getFirstElementByTagAndClassName('TD', 'indent', el);
            if( td ) {
                l = self.rowsIndex[el.id].level;
                units = self.indentUnits;
                step  = self.indentStep;
                td.setAttribute('style', 'padding-left: ' + step*l + units + ';' );
                td.onclick = function (e) {
                    ignoreEvent(e);
                    row = getFirstParentByTagAndClassName(e.target, 'TR');
                    self.selectRow(row);
                }
            }

        });
    }

    this.clickHandler = function (target) {
        par = getFirstParentByTagAndClassName(target, 'TR');
        this.selectRow(par);
        try {
            url = this.urlTplGen[ target.getAttribute('class') ];
            url = replace_placeholders(url, this.rowsIndex[par.id].cells );
            if( !this.rowsIndex[par.id].opened ) {
                var self = this;
                this.doRequest(url, function () {
                    self.loadRows( self.rows.reverse(), par );
                    self.afterLoad();
                    self.rowsIndex[par.id].opened = true;
                });
            }
            else
            {
                this.collapseRow( par.id );
            }
        } catch(e) { return; }
        this.colorize();
    }

    this.selectRow = function ( row ) {
        old = $(this.selectedRowId);
        if(old) {
            cls = old.getAttribute('class');
            a = '';
            if( 'row-odd-selected' == cls )
                a = 'row-odd';
            else
                a = 'row-even';
            old.setAttribute('class', a);
        }
        this.selectedRowId = row.id;
        cls = row.getAttribute('class');
        if( 'row-odd' == cls || 'row-even' == cls ) {
            row.setAttribute('class', cls+'-selected');
        }
    }

    this.collapseRow  = function ( id ) {
        rd = this.rowsIndex[id];
        q = [].concat(rd.children);
        while( q.length ) {
            item = q.pop();
            q = q.concat( this.rowsIndex[item].children );
            removeElement($(item));
            this.rowsIndex[item].children = [];
            this.rowsIndex[item].opened = false;
        }
        rd.children = [];
        rd.opened = false;
    }

    this.initializeRow = function ( row, row_data, current ) {
        row.id = this.rowId_(row_data);
        this.rowsIndex[row.id] = {opened:false, cells:row_data, children:[], level:1 };
        if(current) {
            this.rowsIndex[current.id].children.push(row.id);
            this.rowsIndex[row.id].level = this.rowsIndex[current.id].level + 1;
        }
    }

    this.doRequest = function (url, handler) {
        var d = doXHR(url, {
            mimeType: 'text/xml',
            headers: {Accept: 'text/xml'}
        });
        var self = this;
        d.addCallback( function (r) 
        {
            self.rows = [];
            forEach( r.responseXML.getElementsByTagName('row'), function (row) {
                cells = [];
                self.rows.push( cells );
                forEach( row.getElementsByTagName('cell'),  function (cell) {
                    cells.push( scrapeText(cell) );
                });
            });
            return self;
        });
        d.addCallback(handler);
    }
}


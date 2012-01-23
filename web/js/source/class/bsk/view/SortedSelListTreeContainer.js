/* ************************************************************************
************************************************************************ */

qx.Class.define("bsk.view.SortedSelListTreeContainer",
{
    extend : qx.ui.container.Composite,
    
    /**
     *
     *  @param url --- URL запроса.
     *  @param labelFieldName
     *  @param descrFieldName
     *  @param paramdict --- список параметоров url (может быть undefined)
     */
    construct : function(url, labelFieldName, descrFieldName, paramdict) {
        this.base(arguments);
        
        this.field = new qx.ui.form.TextField();
        this.tree = new bsk.view.SortedSelListTree(this, url, labelFieldName, descrFieldName, paramdict)
        
        var vbox = new qx.ui.layout.VBox()
        this.setLayout(vbox);
        this.add(this.field);
        this.add(this.tree);
    },

    members : {
        
    }
});



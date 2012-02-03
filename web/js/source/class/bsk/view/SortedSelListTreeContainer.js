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
        
        this.field = new qx.ui.form.TextField()
            .set({placeholder: "Фраза для поиска в списке"});
        this.tree = new bsk.view.SortedSelListTree(this, url, labelFieldName, descrFieldName, paramdict)
        
        this.selected_tree = new bsk.view.SortedSelListTree(this, url, labelFieldName, descrFieldName, paramdict)
        
        
        var vbox = new qx.ui.layout.VBox();
        this.setLayout(vbox);
        this.add(this.field);
        
        var treeComposite = new qx.ui.container.Composite();
        var hbox = new qx.ui.layout.HBox();
        
        treeComposite.setLayout(hbox);
        treeComposite.add(this.tree);
        treeComposite.add(this.selected_tree);
        
        this.add(treeComposite);
        
        this.field.addListener("input", this._OnChange, this);
    },

    members : {
        
        _OnChange : function(e){
            console.log("getData = ", e.getData());
            this.tree.testItems(e.getData());
        },
        
        getSelectedId : function() {
            this.tree.getSelectedId();
        }
    }
});



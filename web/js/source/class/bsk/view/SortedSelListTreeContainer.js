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
    construct : function(url, labelFieldName, descrFieldName, Options) {
        this.base(arguments);
        
        this.field = new qx.ui.form.TextField()
            .set({placeholder: "Фраза для поиска в списке"});
        this.tree = new bsk.view.SortedSelListTree(this, url, labelFieldName, descrFieldName, Options)
        
        var vbox = new qx.ui.layout.VBox();
        this.setLayout(vbox);
        this.add(this.field);
        this.add(this.tree);
        
        this.field.addListener("input", this._OnChange, this);
        //this.field.addListener("input", this._OnChange, this);
        
        this.TIMER_INTERVAL = 200;
    },

    members : {
        
        _OnChange : function(e){
            var _this = this;
            var _data = e.getData();
            qx.event.Timer.once(function() {
                _this.tree.testItems(_data);
            }, this, this.TIMER_INTERVAL);
        },
        
        getSelectedId : function() {
            this.tree.getSelectedId();
        }
    }
});



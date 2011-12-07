
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.FormController",
{
    //extend : qx.ui.groupbox.GroupBox,
    extend : qx.ui.container.Composite,

    construct : function(biz, Row, formModel, objName) {
        this.biz = biz;
        this.base(arguments, new qx.ui.layout.VBox());

        this.toolbar = new bsk.view.ToolBar(this.biz, this, formModel.toolbar);
        this.add(this.toolbar);

        this.dockBox = new qx.ui.container.Composite();

        this.add(this.dockBox);

        var label;
        if(Row == undefined)
            label = "Создание " + objName;
        else
            label = "Редактирование " + objName;

        var d = new qx.ui.layout.Dock();
        d.setSort("y");

        this.dockBox.setLayout(d);

        this.set({allowGrowX : true});

        this.formCont = new qx.ui.groupbox.GroupBox(label);
        this.formCont.setLayout(new qx.ui.layout.VBox());
        var w1 = new qx.ui.core.Widget();
        var w2 = new qx.ui.core.Widget();
        var w3 = new qx.ui.core.Widget();
        var w4 = new qx.ui.core.Widget();
        this.dockBox.add(w2, {edge:"west", flex:1})
        this.dockBox.add(w3, {edge:"south", flex:1});
        this.dockBox.add(w4, {edge:"east", flex:1});
        this.dockBox.add(this.formCont, {edge:"center", flex:0});

        if(formModel.controller == undefined) {
            this.form = new bsk.view.GenericForm(this, Row, formModel);
        }
        else {
            if(formModel.controller == "UsersForm") {
                this.form = new bsk.view.UsersForm(this, Row);
            }
            if(formModel.controller == "ForecastForm") {
                this.form = new bsk.view.ForecastForm(this, Row);
            }
            if(formModel.controller == "AdvComForm") {
                this.form = new bsk.view.Form.AdvComForm(this, Row, formModel);
            }
        }
    },

    members : {

        placeForm :function(f) {
            this.formCont.add(f);
        },

        submited : function(result) {
            this.biz.back();
        },

        onCancelClick : function() {
            this.biz.back();
        },

        getExtraParams : function(params) {
            return params;
        }

    }
});


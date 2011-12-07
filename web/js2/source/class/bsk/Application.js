/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(bsk/*)

************************************************************************ */

/**
 * This is the main application class of your custom application "bsk"
 */
qx.Class.define("bsk.Application",
{
  extend : qx.application.Standalone,



  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

    members :
    {
        main : function()
        {
            // Call super class
            this.base(arguments);

            qx.locale.Manager.getInstance().setLocale("ru");
            qx.io.remote.RequestQueue.getInstance().setDefaultTimeout(60000*5);

            // Enable logging in debug variant
            if(qx.core.Variant.isSet("qx.debug", "on")) {
                qx.log.appender.Native;
                qx.log.appender.Console;
            }
            this.history = [];
            this._createLayout();
        },

        _createLayout : function() {
            var dockLayout = new qx.ui.layout.Dock();
            dockLayout.setSeparatorY("separator-vertical");
            var dockLayoutComposite = new qx.ui.container.Composite(dockLayout);
            this.getRoot().add(dockLayoutComposite, {edge:0});

            this._pane = new qx.ui.splitpane.Pane();
            dockLayoutComposite.add(this._pane);

            this.left_cont = new qx.ui.container.Composite(new qx.ui.layout.VBox(12));
            this.left_cont.set({width:200});

            this.right_cont = new qx.ui.container.Composite(new qx.ui.layout.VBox());

            this._pane.add(this.left_cont, 0);
            this._pane.add(this.right_cont, 1);

            this.navTree = new bsk.view.NavTree(this);
            this.left_cont.add(this.navTree, {flex:1});
        },

        onMenuChange : function(curMenu) {
            this.curMenu = curMenu;
            if(curMenu.model != undefined) {
                this.history = [];
                this.cur_controller = undefined;
                this.ActionRow = undefined;
                this.FilterVal = undefined;
                this.loadActionModel(curMenu.model);
                this.show_global_pb();
            }
            else {
                alert("не задано описание модели");
            }
        },

        loadActionModel : function(ActionUrl) {
            var req = new qx.io.remote.Request(ActionUrl, "GET", "application/json");
            req.addListener("completed", this._onIncomeActionModel, this);
            req.send();
        },

        _onIncomeActionModel : function(response) {
            this.hide_global_pb();
            var result = response.getContent();
            var cont = null;
            if(this.cur_controller != undefined)
                this.history.push(this.cur_controller);
            switch(result.type) {
                case "table" :
                    cont = new bsk.view.TabController(this, this.ActionRow, this.FilterVal, result);
                    break;
                case "form" :
                    cont = new bsk.view.FormController(this, this.ActionRow, result, "");
                    break;
                case "chart":
                    cont = new bsk.view.ChartController(this, this.ActionRow, result);
                    break;
            }
            if(cont!= null) {
                this.right_cont.removeAll();
                this.right_cont.add(cont,{flex:1});
                this.cur_controller = cont;
            }
            else {
                this.history.pop();
            }
        },

        onEditClick : function() { // toolbar
            this.ActionRow = undefined;
            this.FilterVal = undefined;
            if(this.cur_controller && this.cur_controller["getActionUrl"] != undefined)
                this.loadActionModel(this.cur_controller.getActionUrl());
        },

        onAction : function(Row, FilterVal, ActionUrl) {//tab dblClick
            this.ActionRow = Row;
            this.FilterVal = FilterVal;
            this.loadActionModel(ActionUrl);
        },

        back : function() {
            this.right_cont.removeAll();
            var cont = this.history.pop();
            if(cont)
                this.right_cont.add(cont,{flex:1});
            this.cur_controller = cont;
        },

        hide_global_pb : function() {
            document.getElementById("global_progress_bar").style.visibility="hidden";
        },

        show_global_pb : function() {
            document.getElementById("global_progress_bar").style.visibility="visible";
        }
    }
});


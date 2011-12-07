
/* ************************************************************************

#asset(qx/icon/Tango/32/actions/go-previous.png)
#asset(qx/icon/Tango/32/actions/system-log-out.png)
#asset(qx/icon/Tango/32/actions/list-add.png)
#asset(qx/icon/Tango/32/actions/zoom-in.png)
#asset(qx/icon/Tango/32/apps/office-chart.png)
#asset(qx/icon/Tango/32/apps/preferences-users.png)
************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("bsk.view.ToolBar",
{
  //extend : qx.ui.toolbar.ToolBar,
    extend : qx.ui.container.Composite,

    construct : function(biz, cntl, model)
    {
        this.biz = biz;
        this.cntl = cntl;
        this.model = model;
        this.base(arguments, new qx.ui.layout.Dock());
        
        this.cnt = new qx.ui.container.Composite(new qx.ui.layout.HBox(12));
        this.add(this.cnt, {edge: "west"});

        this.cntRight = new qx.ui.container.Composite(new qx.ui.layout.HBox(12));
        this.add(this.cntRight, {edge: "east"});

        this.goBackBtn = new qx.ui.toolbar.Button("Назад", "icon/32/actions/go-previous.png");
        this.goBackBtn.setAlignY("top");
        this.cnt.add(this.goBackBtn, {flex:0});
        this.goBackBtn.addListener("execute", this._onGoBackBtnClick, this);


        this.logOutBtn = new qx.ui.toolbar.Button("Выход", "icon/32/actions/system-log-out.png");
        this.cntRight.add(this.logOutBtn, {flex:0});
        this.logOutBtn.addListener("execute", this._onLogOutBtnClick, this);

        if(this.biz.history.length > 0)
            this.goBackBtn.setEnabled(true)
        else
            this.goBackBtn.setEnabled(false);
/*
        this.addBtn = new qx.ui.toolbar.Button("Создать", "icon/32/actions/list-add.png");
        this.addBtn.setAlignY("top");
        this.add(this.addBtn, { flex : 0});
        this.addBtn.addListener("execute", this._onAddBtnClick, this);

        this.addBtn.setEnabled(false);
*/
        this.buildToolbar(model);
    },

    members : {

        buildToolbar : function(model) {
            for(var k in model) {
                var E = model[k];
                switch(E.type) {
                    case "button" :
                        var btn = new qx.ui.toolbar.Button(E.name, E.icon||"icon/32/actions/list-add.png");
                        btn.setAlignY("top");
                        this.cnt.add(btn, { flex : 0});
                        var own = this;
                        btn.addListener("execute", function() {
                            own._onBtnClick(E.action, E.specParam);
                        })
                        break;
                }
            }
        },

        _onBtnClick : function(action, specParam) {
            this.cntl.onToolbarBtn(action, specParam);
        },

        _onGoBackBtnClick : function(e) {
            this.biz.back();
        },
        _onLogOutBtnClick : function(e) {
            var logout_req = new qx.io.remote.Request("/do-logout", "GET", "application/json");
            logout_req.addListener("completed", function(result){ location.reload() }, this);
            logout_req.send();
        }
    }
});


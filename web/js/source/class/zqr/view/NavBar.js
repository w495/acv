
/* ************************************************************************

#asset(qx/icon/Tango/16/*)
#asset(qx/icon/Tango/16/apps/utilities-dictionary.png)
#asset(qx/icon/Tango/16/places/network-workgroup.png)
#asset(qx/icon/Tango/16/apps/preferences-theme.png)

************************************************************************ */

qx.Class.define("zqr.view.NavBar",
{
    extend : qx.ui.container.Composite,
 
   include : [zqr.view.NavMixin],
   
    construct : function(root) {
        this.base(arguments);
        this.setLayout(new qx.ui.layout.HBox());
        this.init(root);
    },

    members : {
        toolbar: null,
        focus : function() {
            return this.toolbar.focus();
        },

        buildMenu : function(menuModel) {
            this.toolbar = new qx.ui.toolbar.ToolBar();
            this.add(this.toolbar, {flex: 1});
            this.menuPart = this.makeMenuPart(menuModel);
            this.toolbar.add(this.menuPart);
            this.toolbar.addSpacer();
            this.biz.hide_global_pb();
        },
 
        makeMenuPart : function(menuModel) {
            var menuPart = new qx.ui.toolbar.Part();
            for(var key = 0; key != menuModel.length; ++key){
                var item = menuModel[key];
                var itemMenu = new qx.ui.toolbar.MenuButton(item.name);
                if(item.subitems)
                    itemMenu.setMenu(this.getItemMenu(item.subitems));
                menuPart.add(itemMenu);
            }
            return menuPart;
        },

        getItemMenu : function(itemMenuModel) {
            var menu = new qx.ui.menu.Menu();
            console.log("+1");
            for(var key = 0; key != itemMenuModel.length; ++key){
                console.log("+2 i");
                var item = itemMenuModel[key];
                var button = new qx.ui.menu.Button(item.name);
                this.menu[item.name] = item;
                button.itemMenuModel = item;
                button.ths_ = this;
                button.addListener("execute", function(){
                    this.ths_.biz.onMenuChange(this.itemMenuModel);
                }, button);
                menu.add(button);
            }
            console.log("+3");
            return menu;
        },

        debugRadio : function(e) {
            console.log("Execute button: ", e);
        },

        debugCommand : function(e) {
            console.log("Execute button: ", e);
        },

        debugButton : function(e) {
            console.log("Execute button: ", e);
        },

        debugCheckBox : function(e) {
            console.log("Execute button: ", e);
        }

    }
});



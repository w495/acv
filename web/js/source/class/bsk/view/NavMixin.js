/* ************************************************************************
#asset(qx/icon/Tango/16/apps/office-project.png)
#asset(qx/icon/Tango/16/apps/office-calendar.png)
#asset(qx/icon/Tango/16/apps/office-chart.png)
#asset(qx/icon/Tango/16/apps/utilities-calculator.png)
#asset(qx/icon/Tango/16/apps/utilities-dictionary.png)
#asset(qx/icon/Tango/16/apps/utilities-statistics.png)
#asset(qx/icon/Tango/16/categories/system.png)
************************************************************************ */

qx.Mixin.define("bsk.view.NavMixin",
{
    /** 
     * Используем "смесь" как альтернативу
     * множественному наследованию.
    **/
    
    members : {
        
        init : function(root) {
            this.biz = root;
            this.addListener("changeSelection", this._onMenuSelect, this);
            this.__buildMenu();
        },
    
        _onMenuSelect : function(e) {
            var I = this.getSelection()[0];
            var L = I.getLabel();
            if(this.menu[L] != undefined){
                console.log("this.menu[L] = ", this.menu[L]);
                this.biz.onMenuChange(this.menu[L]);
            }
        },

        __buildMenu : function() {
            var req = new qx.io.remote.Request('resource/bsk/descr/menu.json', "GET", "application/json");
            req.addListener("completed", this.__onGetMenuResource, this);
            req.send();
        },

        __onGetMenuResource : function(response) {
            var result = response.getContent();
            if (bsk.util.errors.process(this, result)==false)
                return false;
            this.buildMenu(result);
            return true;
        }
    }
});



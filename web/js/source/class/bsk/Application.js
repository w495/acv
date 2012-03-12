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
  //extend : qx.application.Standalone,
  extend : qx.application.Inline,
  
  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

    members :
    {
        title : "Система рекламы tvzavr: кабинет рекламодателя",
        
        main : function()
        {
            /*
            if(typeof(console) === 'undefined') {
                var console = {};
                console.log = console.error = console.info = console.debug = console.warn = console.trace = console.dir = console.dirxml = console.group = console.groupEnd = console.time = console.timeEnd = console.assert = console.profile = function() {};
            }
            */
            // Call super class
            this.base(arguments);
            
            qx.locale.Manager.getInstance().setLocale("ru");
            qx.io.remote.RequestQueue.getInstance().setDefaultTimeout(60000*5);

            // Enable logging in debug variant
            /*
                if(qx.core.Variant.isSet("qx.debug", "on")) {
                    qx.log.appender.Native;
                    qx.log.appender.Console;
                }
            */
            
            this.history = [];
            this.screenMap = {};
            this.curMenu = null;
            
            // var themeName = qx.core.Setting.get("qx.theme");
            
            // var t = eval(themeName);
            // this.appearance = t.meta.appearance.appearances;
            
            // this.color = t.meta.color.colors;
            
            document.title = this.title;

            this._createLayout();
           
        },
        
        /**
         * Выводит сообщение останавливающее пользователя уйти со страницы.
         * @see qx.application.AbstractGui
         */
        close : function() {
            return "Вы уверены, что хотите покинуть страницу?";
        },
        
        _createLayout : function() {
            var USEMENUBAR = true;
            var USEHEADER = true;
            
            //var dockLayout = new qx.ui.layout.Dock();
            var dockLayout = new qx.ui.layout.VBox();
            //dockLayout.setSeparatorY("separator-vertical");
            var dockLayoutComposite = new qx.ui.container.Composite(dockLayout);
           
            
            this.tcont = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            if(!USEMENUBAR){
                this._pane = new qx.ui.splitpane.Pane();
                this.tcont.add(this._pane, {flex: 1});
            }
            if(USEMENUBAR){
                this.navBar = new bsk.view.NavBar(this).set({
                    width: 1000
                });
                this.tcont.add(this.navBar);
            }
            
            if(!USEMENUBAR){
                this.left_cont = new qx.ui.container.Composite(new qx.ui.layout.VBox(12));
                this.left_cont.set({width:200});
            }
            //this.right_cont = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var windowManager = new qx.ui.window.Manager();
            this.right_cont = new qx.ui.window.Desktop(windowManager);
                
            if(!USEMENUBAR){
                this._pane.add(this.left_cont, 0);
                this._pane.add(this.right_cont, 1);
                this.navTree = new bsk.view.NavTree(this);
                this.left_cont.add(this.navTree, {flex:1});
            }
            if(USEMENUBAR){
                this.tcont.add(this.right_cont, {flex: 1});
            }

            this._createInitialView();

            var isle = new qx.ui.root.Inline(document.getElementById("ria"))
                .set({
                decorator: "main",
                padding: 0,
                width: 1000,
                height: 700,
                textColor: "black",
                backgroundColor: "#cccccc"
            });
            dockLayoutComposite.add(this.tcont);
            isle.add(dockLayoutComposite);
            //this.getRoot().add(dockLayoutComposite, {edge:0});
        },
        
        _createInitialView : function() {
            this.right_cont.removeAll();
        },
        
        _createInitialView_old : function() {
            this.right_cont.removeAll();
            this._createTopView();
        },
        
        _createTopView : function() {
            var igl = new qx.ui.layout.Grid(2, 1);
            //igl.setColumnFlex(0, 1);
            igl.setColumnAlign(0, "center", "middle");
            
            var initCont = new qx.ui.container.Composite(igl)
                .set({
                    alignX: "center",
                    alignY: "middle",
                    allowGrowX: true,
                    allowGrowY: true
                });
            var labeltext =
                "<div>" +
                "Добро пожаловать в&nbsp;рекламный центр TVZavr!<br/>" +
                "Здесь вы сможете размещать рекламу " + 
                "в&nbsp;фильмах портала tvzavr " +
                "и&nbsp;получать статистику по&nbsp;рекламным кампаниям. " +
                "Для&nbsp;начала работы ознакомьтесь с&nbsp;видео-инструкцией." +
                "</div>";
                
            var label = new qx.ui.basic.Label().set({
                value: labeltext,
                alignX: "center",
                textAlign: "center",
                alignY: "middle",
                rich : true,
                width: 640,
                marginBottom: 20
              });
            var flashPlayer = new qx.ui.embed.Flash("resource/bsk/flash/gddflvplayer.swf").set({
                width: 640,
                height: 480,
                alignX: "center",
                alignY: "middle",
                variables : {
                    vdo: "/static/data/acv-video/common/5831108/adv02.mp4",
                    autoplay : "false"
                }
            });
            initCont.add(label,     {row:1, column:0});
            initCont.add(flashPlayer , {row:2, column:0});
            this.right_cont.add(initCont,{
                left:"33%",
                top:"10%",
                width:"33%"
            });
        },
        
        onMenuChange : function(curMenu) {
            var cScreen = {
                history  : this.history,
                controller : this.cur_controller,
                actionRow : this.ActionRow,
                filterVal : this.FilterVal
            };
            if(this.curMenu != undefined && this.curMenu != null && this.cur_controller != undefined) {
                this.screenMap[this.curMenu.model] = cScreen;
            }

            this.curMenu = curMenu;

            if(curMenu.model != undefined) {
                if(this.screenMap[curMenu.model] == undefined || this.screenMap[curMenu.model].controller == undefined) {
                    this.history = [];
                    this.cur_controller = undefined;
                    this.ActionRow = undefined;
                    this.FilterVal = undefined;
                    this.loadActionModel(this.curMenu.model);
                    this.show_global_pb();
                }
                else {
                    var screen = this.screenMap[this.curMenu.model];
                    this.history = screen.history;
                    this.cur_controller = screen.controller;
                    this.ActionRow = screen.actionRow;
                    this.FilterVal = screen.filterVal;

                    console.log("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
                    //this.right_cont.removeAll();
                    this._makeWindow(this.cur_controller)
                    //this.right_cont.add(this.cur_controller, {flex:1});
                }
            }
            else {
                alert("не задано описание модели");
            }
        },

        loadActionModel : function(ActionUrl) {
            bsk.util.utils.getStaticJson(ActionUrl, this._onIncomeActionModel, this);
        },
         
        _onIncomeActionModel : function(response) {
            this.hide_global_pb();
            
            console.log("1");
            
            console.log("response = ", response);
            //console.log("response.getContent() = ", response.getContent());
            
            var result = bsk.util.utils.parseStaticJsonRsp(response);

            console.log(result);
            
            console.log("1.2");
            
            if (bsk.util.errors.process(this, result)==false) {
                return false;
            }
            
            console.log("2");
            
            var cont = null;
            if(this.cur_controller != undefined) {
                this.history.push(this.cur_controller);
            }
            switch(result.type) {
                case "table" :
                    cont = new bsk.view.Controller.TabController(this, this.ActionRow, this.FilterVal, result);
                    break;
                case "dir-double-table" :
                    cont = new bsk.view.Controller.DirDoubleTabController(this, this.ActionRow, this.FilterVal, result);
                    break;
                case "form" :
                    cont = new bsk.view.Controller.FormController(this, this.ActionRow, result, "");
                    break;
                case "inline-form" :
                    var tmpActionRow = {id:0,isInline:true}
                    cont = new bsk.view.Controller.FormController(this, tmpActionRow, result, "");
                    break;
            }
            if(cont!= null) {
                this.right_cont.removeAll();
                console.log("3");
                console.log("<xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx>");
                this._makeWindow(cont)
                console.log("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
            }   
            else {
                this.history.pop();
            }
            return true;
        },

        _makeWindow : function(cont) {
                var win = new qx.ui.window.Window(
                    bsk.util.utils.capitalize(this.curMenu.name),
                    this.curMenu.icon).set({
                        allowMaximize: false,
                        allowMinimize: false,
                        showMinimize: false,
                        showStatusbar: false,
                        movable: false,
                        resizable: false,
                        showClose: false,
                        showMaximize: false
                    });
                win.setLayout(new qx.ui.layout.HBox());
                
                win.setHeight(1000);
                
                console.log("--->", this.curMenu.name);
                
                this.cur_controller = cont;
                var windows = this.right_cont.getWindows();
                
                /*
                for(var i = 0; i != windows.length; ++i){
                    var window = windows[i];
                    if ((window == win) || (window.getCaption() == win.getCaption())){
                        console.log("===>", window.getCaption(), " -- ", win.getCaption());
                        window.removeAll();
                        window.add(this.cur_controller, {flex: 1});
                        window.open();
                        return window;
                    }
                }
                */
                

                console.log("xxxx>");
                win.add(this.cur_controller, {flex: 1});
                this.right_cont.add(win, {left: 10, top: 10});
                win.open();
                win.maximize();
                return win;
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

        getActiveWindow : function() {
            console.log("this.right_cont = ", this.right_cont);
            return this.right_cont.getActiveWindow();
        },
    
        back : function() {
            //this.right_cont.removeAll();
            var cont = this.history.pop();
            
            if(!cont)
                cont = this.cur_controller;
            //!!!!!
            console.log("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
            
            this.getActiveWindow().removeAll();
            this.getActiveWindow().add(cont);
            
            cont.refresh();
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



/* ************************************************************************

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("bsk.view.TabController",
{
    extend : qx.ui.container.Composite,

    construct : function(biz, Data, FilterVal, tabModel)
    {
        this.tabModel = tabModel;
        this.initData = Data;
        this.biz = biz;
        this.base(arguments, new qx.ui.layout.VBox());

        this.gBox = new qx.ui.groupbox.GroupBox();
        this.gBox.setLayout(new qx.ui.layout.VBox(5));

        this.toolbar = new bsk.view.ToolBar(this.biz, this, tabModel.toolbar);
        this.add(this.toolbar);
        
        /*
        if (this.tabModel.dblclick_action!=undefined){
            this.verboseButton = new qx.ui.toolbar.Button("Подробнее", "icon/32/actions/zoom-in.png");
            this.verboseButton.setAlignY("top");
            this.toolbar.cnt.add(this.verboseButton, {flex:0});
            //this.verboseButton.setEnabled(false);
            this.verboseButton.addListener("execute", this.rowDblClick, this);
        }*/

        
        this.add(this.gBox, {flex : 1});
        this.buildTable(tabModel, FilterVal);
    },

    members : {
        disableForm : function() {
            this.gBox.setEnabled(false);
            this.biz.show_global_pb();
        },

        enableForm : function() {
            this.gBox.setEnabled(true);
            this.biz.hide_global_pb();
        },

        onToolbarBtn : function(actionUrl, specParam) {
            switch(specParam){
                case "tab-row":
                var id = this.tab.getSelectionModel().getSelectedRanges()[0];
                if (id==undefined) return false;
                var d = this.tab.model.getData();
                var rowId = d[id.minIndex];
                this.biz.onAction(this.tab.model.data[rowId], this.filterForm.getValues(), actionUrl);
                break;
                default:
                this.biz.onAction(this.tab.model.data, this.filterForm.getValues(), actionUrl);
            }
        },

        getActionUrl : function() {
            return this.tabModel.dblclick_action;
        },

        getExtraParams : function(params) {
            if(this.pager != undefined) {
                params.pager_offset = this.pager.getOffset();
                params.pager_limit = this.pager.getLimit();
                params.sort_column = this.tab.model.getSortColumnName();
                params.sort_direction = this.tab.model.getSortDirection();
            }
            if(this.tabModel.index_name != undefined && this.tabModel.index_name.length != undefined && this.tabModel.index_name.length>0 && this.initData != undefined ) {
                for(var k=0;k<this.tabModel.index_name.length;k++){
                    if ( this.initData[this.tabModel.index_name[k]] != undefined ){
                        params[this.tabModel.index_name[k]] = this.initData[this.tabModel.index_name[k]];
                    }
                }
            }
            return params;
        },

        buildTable : function(tabDescription, FilterVal) {
            this.biz.show_global_pb();
            if(tabDescription.filter == undefined) {
                alert("не определено обязательное поле filter в описании таблицы");
                return;
            }
            this.filterForm = new bsk.view.GenericForm(this, undefined, tabDescription.filter, FilterVal);
            this.tab = new bsk.view.GenericTable(this, tabDescription);
            this.tabDescription = tabDescription;
            this.gBox.add(this.tab, {flex:1});
            this.tab.model.updateDataCellRenderers();
            if(tabDescription.pager != undefined) {
                var pCnt = new qx.ui.container.Composite(new qx.ui.layout.Dock());
                this.gBox.add(pCnt);
                this.pager = new bsk.view.Pager(this, tabDescription.pager.pageSize||25);
                pCnt.add(this.pager, {edge:"east"});
            }
            this.filterForm._onSubmitClick(); // загружаем подефолту данные
        },

        submited : function(result) {
            if (bsk.util.errors.process(result)==false) return false;
            this.tab.model.clear();
            this.tab.model.onRowDataIncome(result);
            if(this.pager != undefined)
                this.pager.updatePager(result);
            this.biz.hide_global_pb();
        },

        onSortChange : function() {
            if(this.pager != undefined) {
                this.filterForm._onSubmitClick(); // делаем запрос на сервер, только если есть пейджер
            }
        },

        placeForm : function(f) {
            this.gBox.add(f);
        },

        onPageChange : function() {
            this.filterForm._onSubmitClick(); 
        },

        onCancelClick : function() {
            this.filterForm.form.reset();
        },

        rowDblClick : function(Row) {
            /*
            var id= this.tab.getFocusedRow();
            var ddd = this.model.data[id];
            if (id==undefined) return false;
            var Row = this.tab.model.getRowData(id);
            var obj = {};
            for (var k=0;k<=Row.length-1;k++){
                obj[this.tabModel.columns[k].name] = Row[k];
            }
            */
            this.biz.onAction(Row, this.filterForm.getValues(), this.tabModel.dblclick_action);
        }

    }

});


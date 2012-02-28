/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.CategoryTargeting",
{
    extend : bsk.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        // this.Options = Options;
        alert("!");
        this.base(arguments, uReq, Row, Options);
        alert("!");
    },

    members : {
        
        /* Upload request берется из конструктора */
        uReq : null,
        
        /* Download request делаем сами*/
        dReq : null,
        
        /**
         * Download  request config
         *
         * Предполагается, что загружать данные каждая страница
         * мастера будет самостоятельно, а вот выгружаться на сервер они будут
         * одним запросом.
         * 
        **/
        drc : {
            url: "/get-acv-video/category-targeting",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        categoryListOptions: {
            url:            "/get-all-cats",
            labelFieldName: "name",
            descrFieldName: "seo_alias"
        },
        
        getComposite : function(){
            return this.composite;
        },

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
            
        },
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Таргетирование по категориям",  font: "bold",
                    alignX: "left", rich : true
                });
            var layout = new qx.ui.layout.Grid(2, 1);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);

            this.inp.List = new bsk.view.
                SortedSelListTreeDoubleContainer(
                    this.categoryListOptions.url,
                    this.categoryListOptions.labelFieldName,
                    this.categoryListOptions.descrFieldName,
                    this.Options
                );
            
            var vertical_offset = -1;
            
            this.composite.add(pageName,
                {row:++vertical_offset, column:0});
            this.composite.add(this.inp.List,
                {row:++vertical_offset, column:0});
            
            return this.composite;
        },
        
        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (false == bsk.util.errors.process(this, result))
                return false;
            this.fillForm(result);
            return true;
        },
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {

        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            return flag;
        },
        
        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            var list = this.inp.List.ltree.getSelectedId();
            
            console.log("cat_list ---<")
            console.log(this.inp.List.ltree.getSelectedId());
            console.log(">---")
            
            if(this.validateForm()) {
                this.uReq.setParameter("cat_list", list, true);
            }
            
            return true;
        }
    }
});


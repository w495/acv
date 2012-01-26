/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.RegionTargeting",
{
    extend : Object,
    
    construct : function(uReq) {
        this.uReq = uReq;
        this.buildForm();
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
            url: "",        // 
            method: "",     // POST \ GET
            mimetype: ""    // application/json
        },
        
        getComposite : function(){
            return this.composite;
        },

        regionListOptions: {
            url:            "/get-customer-groups",
            labelFieldName: "name",
            descrFieldName: "description"
        },
        
        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
            Id:null,
            Url:null,
            File:null,
            ShowVariant:null
        },
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Таргетирование",  font: "bold",
                    alignX: "left", rich : true
                });
            var layout = new qx.ui.layout.Grid(2, 1);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            this.composite  = new qx.ui.container.Composite (layout);
            
            this.inp.List = new bsk.view.
                SortedSelListTreeContainer(
                    this.regionListOptions.url,
                    this.regionListOptions.labelFieldName,
                    this.regionListOptions.descrFieldName
                );
                
            var vertical_offset = -1;
            this.composite.add(pageName,
                {row:++vertical_offset, column:0});
            this.composite.add(this.inp.List,
                {row:++vertical_offset, column:0});
            return this.composite;
        },
        
        /**
            Получает данные с сервера.
        **/
        loadFormData : function(id, paramName) {
            this.dReq = new qx.io.remote.Request
                (this.drc.url, this.drc.method, this.drc.mimetype);
            this.dReq.setTimeout(60000);
            this.dReq.setParameter(paramName, id);
            this.dReq.addListener("completed", this._onLoadFormDataCompl, this);
            this.dReq.send();
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
            for(var fieldName in this.inp){
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item])
            }
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
            var formIsValid = this.validateForm();
            if(formIsValid){

            }
            return formIsValid;
        }
    }
});


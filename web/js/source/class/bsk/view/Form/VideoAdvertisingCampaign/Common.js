/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.VideoAdvertisingCampaign.Common",
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

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
            Id:null,
            Name:null,
            StartDate:null,
            StopDate:null,
            Number:null
        },
        
        // -------------------
        textfield1 : null,
        textfield2 : null,
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);

            this.inp.Name = new qx.ui.form.TextField();
            this.inp.StartDate = new qx.ui.form.DateField()
                .set({value: new Date()});
            this.inp.StopDate = new qx.ui.form.DateField()
                .set({value: new Date()});
            this.inp.Number = new qx.ui.form.Spinner(0, 0, 1152921504606846976);
        
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Общая информация",  font: "bold",
                    alignX: "left", rich : true
                });

            var vertical_offset = -1;
            this.composite.add(pageName, {row:++vertical_offset, column:0, colSpan:2})
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Название",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Name,   {row:vertical_offset, column:1});
            
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата начала",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.StartDate,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата конца",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.StopDate,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Количество",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Number,   {row:vertical_offset, column:1});
            
            /**
             * В идеале, если мы хотим гибкость,
             * тут нужно ввести, еще один Сomposite,
             * положить его в this.composite, и уже поля раскладывать в него.
             * 
             * Для чего-то  простого сойдет и так.
            **/
            
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


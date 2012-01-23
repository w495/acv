/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.AdvComVidFormMaster.__Template",
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
        inp :       null,
        fake_inp:   null,
        // -------------------
        textfield1 : null,
        textfield2 : null,
        
        buildForm : function(){
            this.composite  = new qx.ui.container.Composite ();
            var vbox = new qx.ui.layout.VBox(1);
            this.composite.setLayout(vbox);
            
            /**
             * В идеале, если мы хотим гибкость,
             * тут нужно ввести, еще один Сomposite,
             * положить его в this.composite, и уже поля раскладывать в него.
             * 
             * Для чего-то  простого сойдет и так.
            **/
            
            this.textfield1 = new qx.ui.form.TextField("text field #1");
            this.textfield2 = new qx.ui.form.TextField("text field #2");

            this.composite.add(this.textfield1);
            this.composite.add(this.textfield2);
            
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
                this.uReq.setParameter("textfield1-name", this.textfield1.getValue(), true);
                this.uReq.setParameter("textfield2-name", this.textfield2.getValue(), true);
            }
            return formIsValid;
        }
    }
});


/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.BasePage",
{
    type : "abstract",
    
    extend: qx.core.Object,
        /* qx.core.Object !=  Object */
    
    construct : function(uReq, Row) {
        this.uReq = uReq;
        this.buildForm();
        if(Row != undefined && Row["id"] != undefined)
            this.loadFormData(Row["id"], "id");
        this.addListeners();
        this.disableAll();
        
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
            url:        null,
            method:     null,
            mimetype:   null
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
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {            
            var _this = this;
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
        
        disableAll: function() { this.changeEnabled(false);},
        
        /**
            Функция блокировки\разблокировки элементов ввода.
        **/
        changeEnabled: function(enabled) {
            if(this.inp){
                for(var name in this.inp){
                    var field = this.inp[name]
                    if(field.setReadOnly)
                    {
                        console.log("field.setReadOnly");
                        this.inp[name].setReadOnly(!enabled);
                    }
                    else if(field.setEnabled)
                    {
                        console.log("field.setEnabled");
                        this.inp[name].setEnabled(enabled);
                    }else
                    {
                        console.log("not false");
                    }
                    this.onChangeEnabled(enabled);
                    
                    //field.addListener('changeEnabled',function(enabled){
                    //    console.log("change enabled");
                    //});
                }
            }
        },
        
        /**
            Функция блокировки\разблокировки элементов ввода, которые не относятся
                к this.inp, и там их нельзя обработать.
        **/
        onChangeEnabled: function(enabled) {

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
            
            return true;
        }
    }
});


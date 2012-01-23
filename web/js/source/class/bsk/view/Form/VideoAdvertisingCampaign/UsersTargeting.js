/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.VideoAdvertisingCampaign.UsersTargeting",
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
            Gender:null
        },
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Таргетирование",  font: "bold",
                    alignX: "left", rich : true
                });
                
            var layout = new qx.ui.layout.Grid(1, 5);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            
            var boxGender = this.makeBoxGender();
            var boxAge = this.makeBoxAge();
            var boxTime = this.makeBoxTime();
            
            var vertical_offset = -1;
            
            this.composite.add(pageName,
                {row:++vertical_offset, column:0});
            
            this.composite.add(boxGender,
                {row:++vertical_offset, column:0});
            
            this.composite.add(boxAge,
                {row:++vertical_offset, column:0});
            
            this.composite.add(boxTime,
                {row:++vertical_offset, column:0});
            
            return this.composite;
        },
        
        makeBoxGender : function() {
            this.inp.Gender = new qx.ui.form.SelectBox();
            this.__fillSelect(this.inp.Gender, [{name:"М"}, {name:"Ж"}], "name", "name");
            var vertical_offset = 0;
            var boxGender = new qx.ui.groupbox.CheckGroupBox("Пол");
            var layout2 = new qx.ui.layout.Grid(1, 1)
            boxGender.setLayout(layout2);
            boxGender.setValue(false);
            layout2.setColumnFlex(0, 1);
            boxGender.add(this.inp.Gender, {row:0, column:0});
            return boxGender;
        },
        
        makeBoxAge : function() {
            var spinner1 = new qx.ui.form.Spinner(1, 1, 100);
            var spinner2 = new qx.ui.form.Spinner(2, 2, 100);
            var vertical_offset = 0;
            var boxAge = new qx.ui.groupbox.CheckGroupBox("Возраст");
            var layout2 = new qx.ui.layout.Grid(1, 2)
            boxAge.setLayout(layout2);
            boxAge.setValue(false);
            layout2.setColumnFlex(1, 1);
            boxAge.add(new qx.ui.basic.Label().set({value: "От:",  rich : true}), {row:++vertical_offset, column:0});
            boxAge.add(spinner1, {row:vertical_offset, column:1});
            boxAge.add(new qx.ui.basic.Label().set({value: "До:",  rich : true}), {row:++vertical_offset, column:0});
            boxAge.add(spinner2, {row:vertical_offset, column:1});
            return boxAge;
        },

        makeBoxTime : function() {
            var spinner3 = new qx.ui.form.Spinner(0, 0, 24);
            var spinner4 = new qx.ui.form.Spinner(0, 24, 24);
            var vertical_offset = 0;
            var boxTime = new qx.ui.groupbox.CheckGroupBox("Время показа");
            var layout3 = new qx.ui.layout.Grid(1, 2);
            layout3.setColumnFlex(1, 1);
            boxTime.setLayout(layout3);
            boxTime.setValue(false);
            boxTime.add(new qx.ui.basic.Label().set({value: "От:",  rich : true}), {row:++vertical_offset, column:0});
            boxTime.add(spinner3, {row:vertical_offset, column:1});
            boxTime.add(new qx.ui.basic.Label().set({value: "До:",  rich : true}), {row:++vertical_offset, column:0});
            boxTime.add(spinner4, {row:vertical_offset, column:1});
            return boxTime;
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
        
        __fillSelect : function(sel, vals, alias, value) {
            sel.itemMap = [];
            for(var j=0; j<vals.length; j++) {
                var SI = vals[j];
                var selItem = new qx.ui.form.ListItem(SI[alias], null, SI[value]);
                sel.itemMap[SI[value]] = selItem;
                sel.add(selItem);
            }
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


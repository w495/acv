/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.Common",
{
    extend : bsk.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, IsModerator) {
        this.base(arguments, uReq, Row);
        this.IsModerator = IsModerator;
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
            url: "/get-acv-video/common",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
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
            Comment:null,
            DateStart:null,
            DateStop:null
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

            this.inp.Id = new qx.ui.form.TextField();
            this.inp.Name = new qx.ui.form.TextField() 
                .set({placeholder: "Название рекламной кaмпании"});
            this.inp.Comment = new qx.ui.form.TextField() 
                .set({placeholder: "Его кроме Вас никто не увидит"});
                
            var dateStart = new Date();
            dateStart.setDate(dateStart.getDate() - 1);
            this.inp.DateStart = new qx.ui.form.DateField()
                .set({value: dateStart});
            var dateStop = new Date();
            dateStop.setDate(dateStop.getDate() + 1);
            this.inp.DateStop = new qx.ui.form.DateField()
                .set({value: dateStop});
            this.inp.Active  = new qx.ui.form.CheckBox("Активна")
                .set({value: false});
                
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
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Комментарий",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Comment,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата начала",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.DateStart,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата конца",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.DateStop,   {row:vertical_offset, column:1});
            
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
            for(var fieldName in this.inp){
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item]);
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
                var res = {}
                for(var fieldName in this.inp){
                    item = fieldName.toLowerCase()
                    if(("datestart" == item) || ("datestop" == item)){
                        // приведение даты к виду воспринимаем
                        res[item] = bsk.util.utils.
                            normalize_date(this.inp[fieldName].getValue());
                    }
                    else{
                        res[item] = this.inp[fieldName].getValue();
                    }
                }  
                for(var item in res){
                    console.log(item, res[item]);
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});



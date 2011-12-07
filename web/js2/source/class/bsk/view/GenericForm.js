
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.GenericForm",
{
    extend : Object,

    construct : function(biz, Row, formDescription, defaultValue) {
        this.biz = biz;
        this.defaultValue = defaultValue;
        this.formDescription = formDescription;
        this.formRow = Row;
        this.form = new qx.ui.form.Form();
        this.reqMap = [];
        this.buildForm(formDescription);
    },

    statics : {
        num : function() {
            return bsk.view.GenericForm.checkRequired;
        },

        checkRequired : function(value, item) {
            var tmp = "" + value;
            if(item.getRequired() && (tmp.length == 0 || value == null || value == undefined))
                throw new qx.core.ValidationError("Validation Error: ", "Это поле должно быть заполнено");
        },

        checkNumber : function(value, item) {
            bsk.view.GenericForm.checkRequired(value, item);
            if ((typeof value !== "number" && (!(value instanceof Number))) && (!(isFinite(value)))) {
                throw new qx.core.ValidationError("Validation Error: ", value + " не число.");
            }
        },

        checkStringLength : function(from, to, field) {
            field.minValueLength = from;
            field.maxValueLength = to;
            return bsk.view.GenericForm._checkStrLength;
        },

        _checkStrLength : function(value, item) {
            bsk.view.GenericForm.checkRequired(value, item);
            if(value == undefined || value.length == undefined || value.length < item.minValueLength || value.length > item.maxValueLength) {
                throw new qx.core.ValidationError("Validation Error: ", "Значение должно быть не менее " + item.minValueLength + " и не более " + item.maxValueLength + " символов");
            }
        }

    },

    members : {

        buildForm : function(formDescription) {
            if(formDescription.fields == undefined)
                return;
            this.formFieldDescr = [];
            for(var i=0; i<formDescription.fields.length; i++) {
                var E = formDescription.fields[i];
                this.formFieldDescr[E.name] = E;

                var FField;
                switch(E.input.type) {
                    case "title":
                        this.form.addGroupHeader(E.input.value);
                        continue;
                    case "hidden":
                        FField = new qx.ui.form.TextField();
                        FField.setEnabled(false);
                        //FField.setVisibility("hidden");
                        break;
                    case "text-field": 
                        FField = new qx.ui.form.TextField();
                        if(E.input.regexp != undefined)
                            FField.setFilter(E.input.regexp);
                        break;
                    case "check-box":
                        FField = new qx.ui.form.CheckBox();
                        break;
                    case "date-field":
                        FField = new qx.ui.form.DateField();
                        FField.setValue(new Date());
                        break;
                    case "password":
                        FField = new qx.ui.form.PasswordField();
                        break;
                    case "text-area":
                        FField = new qx.ui.form.TextArea();
                        break;
                    case "spinner":
                        FField = new qx.ui.form.Spinner();
                        if(E.input.min != undefined)
                            FField.set({minimum: E.input.min});
                        if(E.input.max != undefined)
                            FField.set({maximum: E.input.max});
                        break;
                    case "select-box":
                        FField = new qx.ui.form.SelectBox();
                        FField.itemMap = [];
                        if(E.input.values != undefined) {
                            var idName = E.input.select_id
                            var aliasName = E.input.select_display_name;
                            for(var j=0; j<E.input.values.length; j++) { //sKey in E.input.values) {
                                var SI = E.input.values[j];
                                var selItem = new qx.ui.form.ListItem(SI[aliasName], null, SI[idName]);
                                FField.itemMap[SI[idName]] = selItem;
                                FField.add(selItem);
                            }
                        }
                        else if(E.input.values_url != undefined){
                            var req = new qx.io.remote.Request(E.input.values_url, "GET", "application/json");
                            req.setTimeout(36000000);
  			    //req.setParameter("name", E.name);
                            this.reqMap[req] = E.name;
                            req.addListener("completed", this._onFetchSelectFields, this);
                            req.send();
                        }
                        break;
                    case "list-box":
                        FField = new qx.ui.form.List();
                        FField.setAllowStretchY(false);
                        FField.itemMap = [];
                        if(E.input.selectionMode!=undefined) FField.setSelectionMode(E.input.selectionMode);
                        if(E.input.values != undefined) {
                            var idName = E.input.select_id
                            var aliasName = E.input.select_display_name;
                            for(var j=0; j<E.input.values.length; j++) { //sKey in E.input.values) {
                                var SI = E.input.values[j];
                                var selItem = new qx.ui.form.ListItem(SI[aliasName], null, SI[idName]);
                                FField.itemMap[SI[idName]] = selItem;
                                FField.add(selItem);
                            }
                        }
                        else if(E.input.values_url != undefined){
                            var req = new qx.io.remote.Request(E.input.values_url, "GET", "application/json");
                            req.setTimeout(36000000);
			    //req.setParameter("name", E.name);
                            this.reqMap[req] = E.name;
                            req.addListener("completed", this._onFetchSelectFields, this);
                            req.send();
                        }
                        FField.height=50;
                        break;
/*                    case "radio-group":
                        FField = new qx.ui.form.RadioButtonGroup();
                        var rgLayout; 
                        if(E.input.align == "horisontal")
                            rgLayout = new qx.ui.layout.HBox(5);
                        else
                            rgLayout = new qx.ui.layout.VBox(12);

                        for(var rgKey in E.input.values) {
                            var GI = E.input.values[rgKey];
                            FField.add(new qx.ui.form.RadioButton(GI.alias));
                        }
                        break; */
                    default:
                        alert(E.input.type + " - неизвестный тип поля формы");
                        continue;
                }
                
                if(E.input.value!=undefined){
                    FField.setValue(E.input.value);
                    }

                var FValidation = null;
                if(E.validation != undefined) {
                    FField.setRequired(E.validation.required == true);
                    switch(E.validation.type) {
                        case "int":
                            FValidation = bsk.view.GenericForm.num();
                            break;
                        case "range":
                            FValidation = qx.util.Validate.range(E.validation.min, E.validation.max);
                            break; 
                        case "string-length":
                            FValidation = bsk.view.GenericForm.checkStringLength(E.validation.min, E.validation.max, FField);
                            //FValidation = qx.util.Validate.range(E.validation.type.min, E.validation.type.max);
                            break;
                    }
                }

                if(E.width != undefined)
                    FField.setWidth(E.width);
                this.form.add(FField, E.alias, FValidation, E.name);
            }

            var submitButton = new qx.ui.form.Button(formDescription.submit_btn_label || "Отправить");
            this.form.addButton(submitButton);
            var cancelButton = new qx.ui.form.Button(formDescription.cancel_btn_label || "Отмена");
            this.form.addButton(cancelButton);
            var auxButtons = Array();
            if (formDescription.buttons){
                for(var k=0;k<formDescription.buttons.length;k++){
                    var btn = formDescription.buttons[k];
                    var auxButton = new qx.ui.form.Button(btn.alias || "Кнопка");
                    auxButton.url = btn.url;
                    this.form.addButton(auxButton);
                    auxButton.addListener("execute", this._onAuxButtonClick, this);
                }
            }

            if(formDescription.render == "single")
                this.biz.placeForm(new qx.ui.form.renderer.Single(this.form));
            else if(formDescription.render == "horisontal")
                this.biz.placeForm(new bsk.view.HorisontalRenderer(this.form));
            else if(formDescription.render == "complex")
                this.biz.placeForm(new bsk.view.MegaRenderer(this.form));
            else
                this.biz.placeForm(new qx.ui.form.renderer.Double(this.form));
            
            this.fController = new qx.data.controller.Form(null, this.form);
            this.formModel = this.fController.createModel();

            submitButton.addListener("execute", this._onSubmitClick, this);
            cancelButton.addListener("execute", this._onCancelClick, this);
            if(this.formRow != undefined && formDescription.index_name != undefined && formDescription.formdata_url != undefined)
                this.loadFormData(formDescription.formdata_url, this.formRow[formDescription.index_name], formDescription.index_name);
            else if(this.defaultValue != undefined)
                this.fillForm(this.defaultValue);
        },

        _onFetchSelectFields : function(response) {
            var result = response.getContent();
            if(this.reqMap[response._target] == undefined)
                return;
            var ffieldName = this.reqMap[response._target];
            this.reqMap[response._target] = undefined;
            var FField = this.form.getItems()[ffieldName];
            var fieldDescr = this.formFieldDescr[ffieldName];
            var idName = fieldDescr.input.select_id;
            var aliasName = fieldDescr.input.select_display_name;
            if(FField == undefined)
                return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI[aliasName], null, SI[idName]);
                FField.itemMap[SI[idName]] = selItem;
                FField.add(selItem);
            }
        },

        getValues : function() {
            return qx.util.Serializer.toNativeObject(this.formModel);
        },

        _onSubmitClick : function(e) {
            this.biz.disableForm();
            if (this.form.validate()) {

                var req = new qx.io.remote.Request(this.formDescription.submit_url, "GET", "application/json");
                req.setTimeout(36000000);
                var methodFlag = false;
                if(this.formDescription.req_type == "POST") {
                    req.setMethod("POST");
                    methodFlag = true;
                }
                if(this.formFieldDescr != undefined && this.formFieldDescr != []) {
                    var params = qx.util.Serializer.toNativeObject(this.formModel);
                    params = this.biz.getExtraParams(params); // доп параметры запроса - пейджер и сортировка
                    /*if (params['fromdate']!=undefined && params['todate']!=undefined && params['fromdate'].toString()!=params['todate'].toString()){
                       alert("Слишком большой временной интервал!\nДля получения данных используйте экспорт отчетов в CSV или HTML.");
                       this.biz.enableForm();
                       return false;
                    }*/
                    if (params['group_by']){
                    if (params['fromdate'].toString()!=params['todate'].toString() && params['group_by']=='hour'){
                       alert("Слишком большой временной интервал для группировки по часам!\nДля группировки по часам суток необходимо выбрать временной интервал не более 1 суток.");
                       this.biz.enableForm();
                       return false;
                    }
                    }
                    for(var k in params) {
                        var val = params[k];
                        req.setParameter(k, val, methodFlag)
                    }
                }
                req.addListener("completed", this._onSubmitCompleted, this);
                req.send();
            }
            else {
                var fFields = this.form.getItems();
                for(var key in fFields) {
                    if(fFields[key].getValid() == false) {
                        switch(fFields[key].getInvalidMessage()) {
                            case "This field is required" : 
                                fFields[key].setInvalidMessage("Это поле должно быть заполнено");
                                break;
                        }
                    }
                }
            }
        },

        _onSubmitCompleted : function(response) {
            this.biz.enableForm();
            var result = response.getContent();
            if(result.valid == false) {
                var fFields = this.form.getItems();
                for(var fName in result.info) {
                    if(fFields[fName] != undefined) {
                        fFields[fName].setInvalidMessage(result.info[fName]);
                        fFields[fName].setValid(false);
                    }
                }
            }
            else {
                this.biz.submited(result);
            }
        },

        _onCancelClick : function(e) {
            this.biz.onCancelClick();
        },

        _onAuxButtonClick : function(e) {
                var btnCaller = e.getCurrentTarget();
                var param_string = '';
                if(this.formFieldDescr != undefined && this.formFieldDescr != []) {
                    var params = qx.util.Serializer.toNativeObject(this.formModel);
                    params = this.biz.getExtraParams(params); // доп параметры запроса - пейджер и сортировка
                    for(var k in params) {
                        param_string=param_string+"&"+k+"="+params[k];
                    }
                }
                window.open(btnCaller.url+'?'+param_string, 'popup', 'toolbar=0,width=800,height=200,scrollbars=0,location=0,directories=0,status=0,left='+Math.round(document.body.offsetWidth/2-400)+',top='+Math.round(document.body.offsetHeight/2-100));
                //var req = new qx.io.remote.Request("/csv/stats", "GET", "text/plain");
                //req.send();
        },

        loadFormData : function(url, id, paramName) {
            this.biz.disableForm();
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            req.setTimeout(36000000);
  	    req.setParameter(paramName, id);
            req.addListener("completed", this._onLoadFormDataCompl, this);
            req.send();
        },

        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (bsk.util.errors.process(result)==false) return;
            this.fillForm(result);
        },

        fillForm : function(data) {
            var fFields = this.form.getItems();
            for(var key in data) {
                if(fFields[key] != undefined) {
                    var fField = fFields[key];
                    if(fField instanceof qx.ui.form.SelectBox) {
                        if(fField.itemMap[data[key]] != undefined)
                            fField.setSelection([fField.itemMap[data[key]]]);
                    }
                    else {
                        var v = data[key];
                        if(fFields[key] instanceof qx.ui.form.TextField)
                            v = "" + v;
                        fFields[key].setValue(v);
                    }
                }
            }
            this.biz.enableForm();
        }
    }
});


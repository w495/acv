/* ************************************************************************

    Класс абстрактной формы 

************************************************************************ */



qx.Class.define("zqr.view.Form.AbstractForm",
{
    type : "abstract",

    extend : qx.core.Object,

    construct : function(controller) {
        this.controller = controller;
        this.form = new qx.ui.form.Form();

        this.submitButton =  new qx.ui.form.Button("Отправить");
        this.cancelButton = new qx.ui.form.Button("Отмена");
    },

    statics : {

        /* Константы */

        REQUIRED_FIELD_MARKER   : "&nbsp;<span style='color:red;'>*</span>",

        /* Статичные методы */

        password : function(fName, form, item) {
            item.fName = fName;
            item.form = form;
            return zqr.view.Form.AbstractForm.checkPassword;
        },

        checkPassword : function(value, item) {
            var i = item.form.getItems();
            var secondVal = i[item.fName].getValue();
            if(secondVal != value)
                throw new qx.core.ValidationError("Validation Error: ", "Пароли не совпадают");
        },

        num : function() {
            return zqr.view.Form.AbstractForm.checkRequired;
        },

        checkRequired : function(value, item) {
            var tmp = "" + value;
            if(item.getRequired() && (tmp.length == 0 || value == null || value == undefined))
                throw new qx.core.ValidationError("Validation Error: ", "Это поле должно быть заполнено");
        },

        checkNumber : function(value, item) {
            zqr.view.Form.AbstractForm.checkRequired(value, item);
            if ((typeof value !== "number" && (!(value instanceof Number))) && (!(isFinite(value)))) {
                throw new qx.core.ValidationError("Validation Error: ", value + " не число.");
            }
        },

        checkStringLength : function(from, to, field) {
            field.minValueLength = from;
            field.maxValueLength = to;
            return zqr.view.Form.AbstractForm._checkStrLength;
        },

        _checkStrLength : function(value, item) {
            zqr.view.Form.AbstractForm.checkRequired(value, item);
            if(value == undefined || value.length == undefined || value.length < item.minValueLength || value.length > item.maxValueLength) {
                throw new qx.core.ValidationError("Validation Error: ", "Значение должно быть не менее " + item.minValueLength + " и не более " + item.maxValueLength + " символов");
            }
        },

        checkStringMax : function(max, field) {
            field.maxStrLength = max;
            return zqr.view.Form.AbstractForm._checkStrMax;
        },

        _checkStrMax : function(value, item) {
            zqr.view.Form.AbstractForm.checkRequired(value, item);
            if(value != undefined && value.length != undefined && value.length > item.maxStrLength) {
                throw new qx.core.ValidationError("Validation Error: ", "Значение должно занимать не более " + item.maxStrLength + " символов");
            }
        },

        customFormChkMaxLength : function(max, field) {
            var value = field.getValue();
            if(value != undefined && value.length != undefined && value.length > max) {
                field.setValid(false);
                field.setInvalidMessage("Значение должно занимать не более " + max + " символов");
                return false;
            }
            return true;
        },

        /**
            Проверка длинны введенного текста в поле.
        **/
        customFormChkLength : function(from, to, field) {
            var value = field.getValue();
            if(value == undefined || value.length == undefined || value.length < from || value.length > to) {
                field.setValid(false);
                field.setInvalidMessage("Значение должно быть не менее " + from + " и не более " + to + " символов");
                return false;
            }
            return true;
        },
        
        customFormChkSymb : function(field) {
            var strpatt = "` ~ ! @ # $ % ^ & * \\[ \\] () _ + { } \" '";
            var _strpatt = "[" + strpatt + "]";
            var value = field.getValue();
            if (value && (-1 != value.search(new RegExp(_strpatt, "gi")))){
                field.setValid(false);
                field.setInvalidMessage("Не должно быть символов "
                    + strpatt.replace("\\", "") +
                    " и пробелов ");
                return false;
            }
            return true;
        },
        
        customFormChkVideoFileName : function(field) {
            var value = field.getValue();
            //if (value && (!(/^(\w(\w|_|-|\/)+[.](mp4|m4v|flv|swf))$/i).test(value))){
            if (value && (!(/^(.+[.](mp4|m4v|flv|swf))$/i).test(value))){
                field.setValid(false);
                field.setInvalidMessage("Неверное имя. Пример: video-file_1.mp4 (*.m4v, *.flv, *.swf)");
                return false;
            }
            return true;
        },
        
        customFormChkImgFileName : function(field) {
            var value = field.getValue();
            //if (value && (!(/^((\w|_|-|\/)+[.](png|jpg|jpeg|gif))$/i).test(value))){
            if (value && (!(/^(.+[.](png|jpg|jpeg|gif))$/i).test(value))){
                field.setValid(false);
                field.setInvalidMessage("Неверное имя. Пример: userpic.png (*.jpg, *.jpeg, *.gif)");
                return false;
            }
            return true;
        },
        
        customFormChkUrl : function(field) {
            var value = field.getValue();
            if (value && (!(/^(https?|ftp):\/\/(((([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(%[\da-f]{2})|[!\$&'\(\)\*\+,;=]|:)*@)?(((\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5]))|((([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.)+(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.?)(:\d*)?)(\/((([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(%[\da-f]{2})|[!\$&'\(\)\*\+,;=]|:|@)+(\/(([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(%[\da-f]{2})|[!\$&'\(\)\*\+,;=]|:|@)*)*)?)?(\?((([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(%[\da-f]{2})|[!\$&'\(\)\*\+,;=]|:|@)|[\uE000-\uF8FF]|\/|\?)*)?(\#((([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(%[\da-f]{2})|[!\$&'\(\)\*\+,;=]|:|@)|\/|\?)*)?$/i.test(value)))){
                field.setValid(false);
                field.setInvalidMessage("Не валидный адрес ");
                return false;
            }
            return true;
        },
        
        customFormChkEmail : function(field) {
            var value = field.getValue();
            //if (value && (!(/^[-a-z0-9~!$%^&*_=+}{\'?]+(\.[-a-z0-9~!$%^&*_=+}{\'?]+)*@([a-z0-9_][-a-z0-9_]*(\.[-a-z0-9_]+)*\.(aero|arpa|biz|com|coop|edu|gov|info|int|mil|museum|name|net|org|pro|travel|mobi|[a-z][a-z])|([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}))(:[0-9]{1,5})?$/i.test(value)))){
            if (value && (!(/^([\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+\.)*[\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+@((((([a-z0-9]{1}[a-z0-9\-]{0,62}[a-z0-9]{1})|[a-z])\.)+[a-z]{2,6})|(\d{1,3}\.){3}\d{1,3}(\:\d{1,5})?)$/i.test(value)))){
                field.setValid(false);
                field.setInvalidMessage("Не валидный адрес ");
                return false;
            }
            return true;
        },
        
        customFormPassCheck : function(pass1, pass2) {
            if(pass1.getValue() != pass2.getValue()) {
                pass1.setValid(false);
                pass2.setValid(false);
                pass1.setInvalidMessage("Пароли не совпадают");
                pass2.setInvalidMessage("Пароли не совпадают");
                return false;
            }
            return true;
        },

        customFormcheckNumber : function(field) {
            var value = field.getValue();
            if (value  && ((typeof value != "number" && (!(value instanceof Number))) && (!(isFinite(value))))) {
                field.setValid(false);
                field.setInvalidMessage("Должно быть число");
                return false;
            }
            return true;
        },

        customFormcheckDate : function(field) {
            var value = field.getValue();
            if (value  && ((typeof value != "object" && (!(value instanceof Date))) && (!(isFinite(value))))) {
                field.setValid(false);
                field.setInvalidMessage("Должна быть дата");
                return false;
            }
            return true;
        },
        
        /**
         * Не позднее чем вчера
         **/
        customFormcheckDateNow : function(field, now_offset){
            if(!zqr.view.Form.AbstractForm.customFormcheckDate(field))
                return false;
            var value = field.getValue();
            var yesterday = (function(s){s.setDate(s.getDate() - now_offset);return s;})
                (new Date());
            //alert("~! value = " + value  + " : yesterday = " + yesterday + " : " + (value < yesterday));
            if (value < yesterday){
                field.setValid(false);
                field.setInvalidMessage("Должна корректная быть дата");
                return false;
            }
            return true;
        },

        customFormCheckRequired : function(field) {
            var value = field.getValue();
            if(value == undefined || value.length == undefined || value.length == 0) {
                field.setValid(false);
                field.setInvalidMessage("Поле обязательно для заполнения");
                return false;
            }
            return true;
        }

    },
 
    members : {

        /* Кнопки */
        submitButton: null, // new qx.ui.form.Button("Отправить"),
        cancelButton: null, // new qx.ui.form.Button("Отмена"),

        /**
            Выводит сообщение об ошибке для данного поля
        **/
        showEMsg : function(fieldName, msg) {
            var fFields = this.form.getItems();
            if(fFields[fieldName] != undefined) {
                fFields[fieldName].setInvalidMessage(msg);
                fFields[fieldName].setValid(false);
                alert("fieldName = " + fieldName);
            }
            else {
                alert("Ошибка сервера - " + msg + " для " + fieldName);
            }
        },
        
        setEnabled: function(bool) {
            console.log(">>>>>>>>>>>>>>> setEnabled = ", bool);
            console.log(">>>>>>>>>>>>>>> this.form = ", this.form);
            console.log(">>>>>>>>>>>>>>> this.form.getItems() = ", this.form.getItems());
            var fFields = this.form.getItems();
            for(var field in fFields){
                field.setEnabled(bool);
                console.log("field", field);
            }            
        },

        submit : function(req) {
            req.addListener("completed", this._onSubmitCompleted, this);
            req.send();
        },

        // @depricated
        _onSubmitCompleted : function(response) {
            this.controller.enableForm();
            var result = response.getContent();
            if(result.ERROR != undefined) {
                switch(result.ERROR.type) {
                    case "unknown":
                        alert("Ошибка сервера: " + result.ERROR.info);
                        break;
                    case "not_null":
                        this.showEMsg(result.ERROR.info, "поле не заполнено");
                        break;
                    case "not_unique":
                        this.showEMsg(result.ERROR.info, "поле должно быть уникально");
                        break;
                    default:
                        alert("Неизвестная ошибка сервера: " + result.ERROR.type);
                        break;
                }
            }
            else {
                this.onFormClose();
                this.controller.submited(result);
            }
        },

        /* Виртуальная фукция посылки на сервер */
        _onSubmitClick : function() {},

        /* Виртуальная отмены */
        _onCancelClick : function() {},

        onFormClose : function() {},

        show_error : function(etype, emsg) {
            alert("Ошибка (" + etype + "): " + emsg);
        }
    }
});


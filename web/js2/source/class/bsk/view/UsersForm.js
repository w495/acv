
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.UsersForm",
{
    extend : Object,

    construct : function(biz, Row) {
        this.biz = biz;
        this.formRow = Row;
        this.form = new qx.ui.form.Form();
        this.reqMap = [];
        this.buildForm();
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

        buildForm : function() {
            var layout = new qx.ui.layout.Grid(12, 6);
//            layout.setSpacing(6);
            var cnt = new qx.ui.container.Composite(layout);

            this.inpUid = new qx.ui.form.TextField();
            this.inpUid.setEnabled(false);
            this.inpName = new qx.ui.form.TextField();

            this.pass1 = new qx.ui.form.PasswordField();
            this.pass2 = new qx.ui.form.PasswordField();

            this.rList = new qx.ui.form.List();
            this.rList.setWidth(400);

            this.selRType = new qx.ui.form.SelectBox();
            this.selRType.setWidth(200);
            this.selObj = new qx.ui.form.SelectBox();
            this.selObj.setEnabled(false);

            this.delRight = new qx.ui.form.Button("Удалить");
            this.addRight = new qx.ui.form.Button("Добавить");
            //this.addRight.setEnabled(false);

            var submitButton = new qx.ui.form.Button("Отправить");
            var cancelButton = new qx.ui.form.Button("Отмена");

            this.permission_pending = new Array();
            
            layout.setColumnFlex(0, 1);
            layout.setColumnFlex(3, 1);
            layout.setColumnAlign(0, "right", "top");
            layout.setColumnAlign(3, "right", "top");

            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2}); 
            cnt.add(new qx.ui.basic.Label("uid"), {row:1, column:0});
            cnt.add(new qx.ui.basic.Label("Логин"), {row:2, column:0});
            cnt.add(new qx.ui.basic.Label("Пароль"), {row:3, column:0});
            cnt.add(new qx.ui.basic.Label("Повторите пароль"), {row:4, column:0});

            cnt.add(this.inpUid,  {row:1, column:1});
            cnt.add(this.inpName, {row:2, column:1});
            cnt.add(this.pass1,   {row:3, column:1});
            cnt.add(this.pass2,   {row:4, column:1});

            var l2 = new qx.ui.basic.Label("Права");
            l2.setFont("bold");
            cnt.add(l2, {row:0, column:2});
            cnt.add(this.rList, {row:1, column:2, rowSpan:3});
            cnt.add(this.delRight, {row:4, column:2});

            var l3 = new qx.ui.basic.Label("Добавить право");
            l3.setFont("bold");
            l3.setAlignX("left");

            cnt.add(l3, {row:0, column:3, colSpan:2});
            cnt.add(new qx.ui.basic.Label("Тип"), {row:1, column:3});
            cnt.add(new qx.ui.basic.Label("Объект"), {row:2, column:3});

            cnt.add(this.selRType, {row:1, column:4});
            cnt.add(this.selObj, {row:2, column:4});
            cnt.add(this.addRight, {row:3, column:4});


            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            buttonRow.add(submitButton);
            buttonRow.add(cancelButton);
            cnt.add(buttonRow, {row:5, column:0, colSpan:5});

            this.biz.placeForm(cnt);

            submitButton.addListener("execute", this._onSubmitClick, this);
            cancelButton.addListener("execute", this._onCancelClick, this);
            this.addRight.addListener("execute", this._onAddPermissionClick, this);
            this.delRight.addListener("execute", this._onDeletePermissionClick, this);

            if(this.formRow != undefined)
                this.loadFormData("/get-user", this.formRow.uid, "uid");
            var perm_types_req = new qx.io.remote.Request("/permission-types/", "GET", "application/json");
            perm_types_req.addListener("completed", this._onLoadPermissionTypes, this);
            perm_types_req.send();
   
                
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

        _onSubmitClick : function(e) {
            if (this.pass1.getValue()==this.pass2.getValue()) {

                var req = new qx.io.remote.Request("/edit-user", "GET", "application/json");
                var methodFlag = false;
                req.setMethod("POST");
                req.setParameter('uid', this.formRow.uid);
                req.setParameter('name', this.inpName.getValue());
                if (this.pass1.getValue()!=null) req.setParameter('password', this.pass1.getValue());
                req.addListener("completed", this._onSubmitCompleted, this);
                req.send();
            }
            else {
                this.pass1.setValid(false);
                this.pass2.setValid(false);
                this.pass1.setInvalidMessage("Пароли не совпадают!");
            }
        },

        _onSubmitCompleted : function(response) {
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
                for(var j=0; j<this.permission_pending.length; j++) {
                    var req_set_permissions = new qx.io.remote.Request("/"+this.permission_pending[j].operation+"-permission/", "GET", "application/json");
                    req_set_permissions.setParameter('uid', result.uid);
                    req_set_permissions.setParameter('type', this.permission_pending[j].type);
                    req_set_permissions.setParameter('subject', this.permission_pending[j].subject);
                    req_set_permissions.send();
                }
                this.biz.submited(result);
            }
        },

        _onCancelClick : function(e) {
            this.biz.onCancelClick();
        },

        loadFormData : function(url, id, paramName) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            req.setParameter(paramName, id);
            req.addListener("completed", this._onLoadFormDataCompl, this);
            req.send();
        },

        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            this.fillForm(result);
        },

        _onLoadPermissionTypes : function(response) {
            var result = response.getContent();
            if ('values' in result) {} else return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var name = SI.description;
                //if SI.
                var selItem = new qx.ui.form.ListItem(name, null, SI.id);
                this.selRType.add(selItem);
            }
            this.selRType.addListener("changeSelection", this._onPermTypeChange, this);
        },

        _onPermTypeChange : function() {
            var perm_subj_req = new qx.io.remote.Request("/permission-subjects", "GET", "application/json");
            if (this.selRType.getSelection().length==0) return;
            var id = this.selRType.getSelection()[0].getModel();
            perm_subj_req.setParameter('id', id);
            perm_subj_req.addListener("completed", this._onLoadPermissionSubjects, this);
            perm_subj_req.send();
        },

        _onLoadPermissionSubjects : function(response) {
            var result = response.getContent();
            if ('values' in result) {} else return;
            this.selObj.removeAll();
            this.selObj.setEnabled(false);
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI.name, null, SI.id);
                this.selObj.add(selItem);
            }
            if (result.values.length>0) this.selObj.setEnabled(true);
            this.addRight.setEnabled(true);

        },

        _onAddPermissionClick : function(e) {
            if (this.selRType.getSelection().length==0) return;
            var permission_type = this.selRType.getSelection()[0].getModel();
            var permission_type_name = this.selRType.getSelection()[0].getLabel();
            var permission_subject = null;
            var permission_subject_name = "";
            if (this.selObj.getSelection().length!=0){ 
                permission_subject = this.selObj.getSelection()[0].getModel();
                permission_subject_name = " "+this.selObj.getSelection()[0].getLabel();
            }
            var permission_items = this.rList.getSelectables();
            for(var j=0; j<permission_items.length; j++) {
                var permission_item =  permission_items[j].getModel();
                if ( (permission_item['type']==permission_type) && (permission_item['subject']==permission_subject) ) return;
            }
            var permission = {'type': permission_type, 'subject': permission_subject};
            this.permission_pending.push({'operation': 'add', 'type': permission_type, 'subject': permission_subject});
            var selItem = new qx.ui.form.ListItem(permission_type_name+permission_subject_name, null, permission);
            this.rList.add(selItem);
        },

        _onDeletePermissionClick : function(e) {
            var permission_items = this.rList.getSelection();
            for(var j=0; j<permission_items.length; j++) {
                this.permission_pending.push({'operation': 'remove', 'type': permission_items[j].getModel()['type'], 'subject': permission_items[j].getModel()['subject']});
                this.rList.remove(permission_items[j]);
            }
       },

        fillForm : function(data) {
            if (!data.uid) return false;
            this.inpUid.setValue(data.uid.toString());
            this.inpName.setValue(data.login);
            for(var j=0; j<data.permissions.length; j++) {
                var SI = data.permissions[j];
                var name = SI.permission_description;
                if (SI.target_name!=null)
                    if (SI.target_name.length>0) name=name+" "+SI.target_name;
                var permission = {'type': SI.permission_type_id, 'subject': SI.target_id};
                var selItem = new qx.ui.form.ListItem(name, null, permission);
                this.rList.add(selItem);
            }
        }
    }
});


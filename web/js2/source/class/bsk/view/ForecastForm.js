
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.ForecastForm",
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
            var layout = new qx.ui.layout.Grid(12, 10);
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

            var submitButton = new qx.ui.form.Button("Построить прогноз");
            this.engageButton = new qx.ui.form.Button("Зарезервировать!");
            this.selectionProfile = {};
	    var cancelButton = new qx.ui.form.Button("Отмена");

            this.permission_pending = new Array();
            
            layout.setColumnFlex(0, 1);
            layout.setColumnFlex(3, 1);

            var l1 = new qx.ui.basic.Label("Рекламная кампания");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:1}); 
            this.DateFrom = new qx.ui.form.DateField();
            this.DateFrom.setValue(new Date());
            this.DateTo = new qx.ui.form.DateField();
            this.DateTo.setValue(new Date());
            cnt.add(new qx.ui.basic.Label("Старт"), {row:1, column:0});
            cnt.add(this.DateFrom, {row:2, column:0});
            this.DateFrom.setHeight(40);
            cnt.add(new qx.ui.basic.Label("Окончание"), {row:3, column:0});
            cnt.add(this.DateTo, {row:4, column:0});
            this.DateTo.setHeight(40);
            cnt.add(new qx.ui.basic.Label("Часы показов"), {row:6, column:0});
            this.Hours = new qx.ui.form.List();
            this.Hours.itemMap = [];
            this.Hours.setSelectionMode("multi");
            var selItem = new qx.ui.form.ListItem("Все", null, undefined);
            this.Hours.itemMap["hours"] = selItem;
            this.Hours.add(selItem);
            for(var j=0; j<=23; j++) {
                selItem = new qx.ui.form.ListItem(j+"-"+(j+1), null, j);
                this.Hours.itemMap["hours"] = selItem;
                this.Hours.add(selItem);
            }
            cnt.add(this.Hours, {row:7, column:0});

            var l2 = new qx.ui.basic.Label("Контент");
            l2.setFont("bold");
            cnt.add(l2, {row:0, column:2, colSpan:1});
            cnt.add(new qx.ui.basic.Label("Категория"), {row:1, column:2});
            this.Categories = new qx.ui.form.List();
            this.Categories.itemMap = [];
            this.Categories.setSelectionMode("multi");
            cnt.add(this.Categories, {row:2, column:2});
            this.Categories.setHeight(40);
            this.Categories.setWidth(200);
            var req = new qx.io.remote.Request("/stats-categories/", "GET", "application/json");
            req.addListener("completed", this._onFetchCategories, this);
            req.send();
            cnt.add(new qx.ui.basic.Label("Теги"), {row:6, column:2});
            this.Tags = new qx.ui.form.List();
            this.Tags.itemMap = [];
            this.Tags.setSelectionMode("multi");
            cnt.add(this.Tags, {row:7, column:2});
            this.Tags.setHeight(200);
            var req = new qx.io.remote.Request("/stats-categories/", "GET", "application/json");
            req.addListener("completed", this._onFetchTags, this);
            req.send();
            

            var l3 = new qx.ui.basic.Label("Аудитория");
            l3.setFont("bold");
            l3.setAlignX("left");
            cnt.add(l3, {row:0, column:4, colSpan:1});

            cnt.add(new qx.ui.basic.Label("Минимальный возраст"), {row:1, column:4});
            cnt.add(new qx.ui.basic.Label("Максимальный возраст"), {row:3, column:4});
            this.minAge = new qx.ui.form.SelectBox();
            this.minAge.itemMap = [];
            this.maxAge = new qx.ui.form.SelectBox();
            this.maxAge.itemMap = [];
            var selItem1 = new qx.ui.form.ListItem("Любой", null, undefined);
            var selItem2 = new qx.ui.form.ListItem("Любой", null, undefined);
            this.minAge.itemMap["min_age"] = selItem1;
            this.minAge.add(selItem1);
            this.maxAge.itemMap["max_age"] = selItem2;
            this.maxAge.add(selItem2);
            for(var j=1; j<=99; j++) {
                selItem1 = new qx.ui.form.ListItem(j.toString(), null, j);
                selItem2 = new qx.ui.form.ListItem(j.toString(), null, j);
                this.minAge.itemMap["min_age"] = selItem1;
                this.minAge.add(selItem1);
                this.maxAge.itemMap["max_age"] = selItem2;
                this.maxAge.add(selItem2);
            }
            cnt.add(this.minAge, {row:2, column:4});
            cnt.add(this.maxAge, {row:4, column:4});

        cnt.add(new qx.ui.basic.Label("Пол"), {row:6, column:4});
            this.Gender = new qx.ui.form.List();
            this.Gender.itemMap = [];
            var selItem = new qx.ui.form.ListItem("Любой", null, undefined);
            this.Gender.itemMap["gender"] = selItem;
            this.Gender.add(selItem);
            selItem = new qx.ui.form.ListItem("Мужской", null, "male");
            this.Gender.itemMap["gender"] = selItem;
            this.Gender.add(selItem);
            selItem = new qx.ui.form.ListItem("Женский", null, "female");
            this.Gender.itemMap["gender"] = selItem;
            this.Gender.add(selItem);
            cnt.add(this.Gender, {row:7, column:4});
            
             var l4 = new qx.ui.basic.Label("География");
            l4.setFont("bold");
            l4.setAlignX("left");
            cnt.add(l4, {row:0, column:6, colSpan:1});
            cnt.add(new qx.ui.basic.Label("Страна"), {row:1, column:6});
            this.Country = new qx.ui.form.SelectBox();
            this.Country.itemMap = [];
            cnt.add(this.Country, {row:2, column:6});
            this.Country.setHeight(40);
            var req = new qx.io.remote.Request("/countries/", "GET", "application/json");
            req.setTimeout(30000);
            req.addListener("completed", this._onFetchCountries, this);
            req.send();
            cnt.add(new qx.ui.basic.Label("Город"), {row:3, column:6});
            this.City = new qx.ui.form.SelectBox();
            this.City.itemMap = [];
            cnt.add(this.City, {row:4, column:6});
            this.City.setHeight(40);
            var req = new qx.io.remote.Request("/cities/", "GET", "application/json");
            req.setTimeout(30000);
            req.addListener("completed", this._onFetchCities, this);
            req.send();

            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            this.engageButton.setEnabled(false);
	    buttonRow.add(this.engageButton);
            buttonRow.add(submitButton);

            this.biz.placeForm(cnt);
            var layout2 = new qx.ui.layout.Grid(1, 2);
            var cnt2 = new qx.ui.container.Composite(layout2);
            cnt2.add(buttonRow,{row:0, column:0});
            this.Results=new qx.ui.form.TextArea("");
            cnt2.add(this.Results,{row:1, column:0});
            this.Results.setWidth(640);
            this.Results.setHeight(200);

            this.biz.placeForm(cnt2);

            submitButton.addListener("execute", this._onSubmitClick, this);
            this.engageButton.addListener("execute", this._onEngageClick, this);
   
                
        },

        _onFetchCategories : function(response) {
            var result = response.getContent();
            if(this.Categories == undefined)
                return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI['alias'], null, SI['id']);
                this.Categories.itemMap[SI['id']] = selItem;
                this.Categories.add(selItem);
            }
        },


        _onFetchTags : function(response) {
            var result = response.getContent();
            if(this.Tags == undefined)
                return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI['alias'], null, SI['id']);
                this.Tags.itemMap[SI['id']] = selItem;
                this.Tags.add(selItem);
            }
        },

        _onFetchCountries : function(response) {
            var result = response.getContent();
            if(this.Country == undefined)
                return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI['geoip'], null, SI['geoip']);
                this.Country.itemMap[SI['geoip']] = selItem;
                this.Country.add(selItem);
            }
        },

        _onFetchCities : function(response) {
            var result = response.getContent();
            if(this.City == undefined)
                return;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI['city'], null, SI['city']);
                this.City.itemMap[SI['city']] = selItem;
                this.City.add(selItem);
            }
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
            //this.biz.show_global_pb();
	    this.selectionProfile = {};
	    var req = new qx.io.remote.Request("/planner/", "GET", "application/json");
	    req.setTimeout(30000);
            req.setParameter('fromdate', this.DateFrom.getValue());
            req.setParameter('todate', this.DateTo.getValue());
	    this.selectionProfile['fromdate'] = this.DateFrom.getValue();
	    this.selectionProfile['todate'] = this.DateTo.getValue()
            if (this.City.getSelection().length>0){
                var city = this.City.getSelection()[0].getModel();
                if (city!=undefined){ 
			req.setParameter('city', city);
			this.selectionProfile['city'] = city;
		}
            }
            if (this.Country.getSelection().length>0){
                var country = this.Country.getSelection()[0].getModel();
                if (country!=undefined){
			req.setParameter('country', country);
			this.selectionProfile['country'] = country;
		}
            }
            if (this.Gender.getSelection().length>0){
                var gender = this.Gender.getSelection()[0].getModel();
                if (gender!=undefined){
			req.setParameter('gender', gender);
			this.selectionProfile['gender'] = gender;
		}
            }
            if (this.minAge.getSelection().length>0){
                var age_start = this.minAge.getSelection()[0].getModel();
                if (age_start!=undefined){
			req.setParameter('age_start', age_start);
			this.selectionProfile['age_start'] = age_start;
		}
            }
            if (this.maxAge.getSelection().length>0){
                var age_end = this.maxAge.getSelection()[0].getModel();
                if (age_end!=undefined){
			req.setParameter('age_end', age_end);
			this.selectionProfile['age_end'] = age_end;
		}
            }
            if (this.Categories.getSelection().length>0){
                var category_id = this.Categories.getSelection()[0].getModel();
                if (category_id!='None'){
			req.setParameter('category', category_id);
			this.selectionProfile['category'] = category_id;
		}
            }
            if (this.Hours.getSelection().length>0){
                var HoursSel=this.Hours.getSelection();
                var HoursArr = [];
                for(var j=0;j<HoursSel.length;j++){
                    if (HoursSel[j].getModel()!=undefined) HoursArr.push(HoursSel[j].getModel());
                }
                if (HoursArr.length>0){
			this.selectionProfile['hours'] = HoursArr.join(",");
			req.setParameter('hours', HoursArr.join(","));
		}
            }
            if (this.Tags.getSelection().length>0){
                var TagsSel=this.Tags.getSelection();
                var TagsArr = [];
                for(var j=0;j<TagsSel.length;j++){
                    if (TagsSel[j].getModel()!=undefined && TagsSel[j].getModel()!='None') TagsArr.push(TagsSel[j].getModel());
                }
                if (TagsArr.length>0){
			this.selectionProfile['tags'] = TagsArr.join(",");
			req.setParameter('tags', TagsArr.join(","));
		}
            }
            //req.setParameter('name', this.inpName.getValue());
            req.addListener("completed", this._onSubmitCompleted, this);
            this.Results.setValue("");
            this.engageButton.setEnabled(false);
	    req.send();
        },

        _onSubmitCompleted : function(response) {
            //this.biz.hide_global_pb();
            var result = response.getContent();
            this.Results.setValue("Прогнозируемый хронометраж просмотров: "+this.representDate(result.wtime)+"\nКоличество просмотров: "+result.watches.toString()+"\nСреднее время просмотра: "+this.representDate(result.avg_wtime)+"\nVIDEO_PREROLL показов: "+result.video_prerolls+"\nVIDEO_INROLL показов: "+result.video_inrolls+"\nBANNER показов: "+result.banner_shows+"\nCRLINE показов: "+result.crline_shows);
	    if (result.watches>0) this.engageButton.setEnabled(true);
        },


        _onEngageClick : function(e) {
	    var req = new qx.io.remote.Request("/engage/", "GET", "application/json");
	    req.setTimeout(30000);
	    for(p in this.selectionProfile){
		 req.setParameter(p, this.selectionProfile[p]);  
 	   }
            req.addListener("completed", this._onEngageCompleted, this);
            this.Results.setValue("Резервирование...");
            this.engageButton.setEnabled(false);
	    req.send();
        },


        _onEngageCompleted : function(response) {
            var result = response.getContent();
            this.Results.setValue("Резервирование завершено");
        },

	
        formatDTItem : function(i) {
            if(String(i).length == 1)
                i = "0" + i;
            return i;
        },

        representDate: function(rowVal){
            rowVal = rowVal *1;
            var hour = Math.floor(rowVal/3600);
            rowVal = rowVal%3600;
            var min = Math.floor(rowVal / 60);
            var sec = rowVal % 60;
            rowVal = "" + this.formatDTItem(hour) + ":" + this.formatDTItem(min) + ":" + this.formatDTItem(sec);
            return rowVal;
        },


        loadFormData : function(url, id, paramName) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            req.setParameter(paramName, id);
            req.addListener("completed", this._onLoadFormDataCompl, this);
            req.send();
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


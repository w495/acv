/* ************************************************************************

************************************************************************ */


qx.Class.define("bsk.view.Form.AdvComVidForm",
{

    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        if(Row)
            this.createNew = (Row.isNew == true);
        this.base(arguments, controller, Row);
        this.addListeners();

        this.fStatus = "wdata";
        this.fData = null;

        this.step = 0;
    },

    members : {

        urc : {  // upload request config
            url: "/update-adv-com-vid",
            imgurl: "/update-adv-com/upload-video",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-adv-com-vid",
            method: "GET",
            mimetype: "application/json"
        },

        buildForm : function(){
            this.inp.Id = null;
            this.inp.Name = new qx.ui.form.TextField();
            this.picButton = new bsk.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new bsk.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);

            this.inp.datestart = new qx.ui.form.DateField();
            this.inp.datestart.setValue(new Date());
            this.inp.datestop = new qx.ui.form.DateField()
            this.inp.datestop.setValue(new Date());

            this.inp.Pic_url = new qx.ui.form.TextField();

            this.inp.ref = new qx.ui.form.TextField();

            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            var layout = new qx.ui.layout.Grid(5, 5);
            var cnt = new qx.ui.container.Composite(layout);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            this.inp.Name.focus();

            this.inp.selGender = new qx.ui.form.SelectBox();

            this.fillSelect(this.inp.selGender, [{name:"М"}, {name:"Ж"}], "name", "name");

            var spinner1 = new qx.ui.form.Spinner(0, 0, 100);
            var spinner2 = new qx.ui.form.Spinner(0, 0, 100);

            var spinner3 = new qx.ui.form.Spinner(0, 0, 24);
            var spinner4 = new qx.ui.form.Spinner(0, 24, 24);

            var cbGender = new qx.ui.form.CheckBox("Пол:");

            cbGender.childWidget = this.inp.selGender;

            var cbList = [cbGender];//, cbOldFrom, cbOldTo];

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


            vertical_offset = 0;

            var boxTime = new qx.ui.groupbox.CheckGroupBox("Время показа");
            var layout3 = new qx.ui.layout.Grid(1, 2);
            layout3.setColumnFlex(1, 1);
            boxTime.setLayout(layout3);
            boxTime.setValue(false);
            boxTime.add(new qx.ui.basic.Label().set({value: "От:",  rich : true}), {row:++vertical_offset, column:0});
            boxTime.add(spinner3, {row:vertical_offset, column:1});
            boxTime.add(new qx.ui.basic.Label().set({value: "До:",  rich : true}), {row:++vertical_offset, column:0});
            boxTime.add(spinner4, {row:vertical_offset, column:1});


            var cbPreroll = new qx.ui.form.CheckBox("Preroll")
            var cbMidroll = new qx.ui.form.CheckBox("Midroll")
            var cbPostroll = new qx.ui.form.CheckBox("Postroll")
            var cbPauseroll = new qx.ui.form.CheckBox("Pauseroll")

            var boxPlace = new qx.ui.groupbox.GroupBox("Размещение ролика");
            boxPlace.setLayout(new qx.ui.layout.VBox(2));
            boxPlace.add(cbPreroll);
            boxPlace.add(cbMidroll);
            boxPlace.add(cbPostroll);
            boxPlace.add(cbPauseroll);

            var boxCat = new qx.ui.groupbox.CheckGroupBox("Категории");
            boxCat.setLayout(new qx.ui.layout.VBox(2));
            boxCat.setValue(false);


            var catList = new bsk.view.SelListTree(this,
                "",
                "name",
                "id"
            );

            boxCat.add(catList);


            vertical_offset = 0;

            var l1 = new qx.ui.basic.Label("Общая информация");
            var l2 = new qx.ui.basic.Label("Таргетирование");

            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});
            l2.setFont("bold");
            l2.setAlignX("left");
            cnt.add(l2, {row:0, column:2, colSpan:2});

 
            cnt.add(new qx.ui.basic.Label().set({value: "Название:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name, {row:vertical_offset , column:1});

            cnt.add(cbGender, {row:vertical_offset, column:2});
            cnt.add(this.inp.selGender, {row:vertical_offset , column:3});


            cnt.add(cbGender, {row:vertical_offset, column:2});
            cnt.add(this.inp.selGender, {row:vertical_offset , column:3});


            cnt.add(new qx.ui.basic.Label().set({value: "Дата начала:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.datestart, {row:vertical_offset , column:1});

            cnt.add(boxAge, {row:vertical_offset, column:2, colSpan:2, rowSpan:3});

            cnt.add(new qx.ui.basic.Label().set({value: "Дата конца:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.datestop, {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "URL:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.ref, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Ролик:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});

            cnt.add(boxTime, {row:vertical_offset, column:2, colSpan:2, rowSpan:2});

            cnt.add(boxPlace, {row:++vertical_offset , column:0, colSpan:2/*, rowSpan:4*/});



            cnt.add(boxCat, {row:1, column:4, rowSpan:vertical_offset})
            this.addbuttonRow(cnt, ++vertical_offset);

            this.controller.placeForm(cnt);

            for(var i=0; i<cbList.length; i++) {
                var cb = cbList[i];
                cb.childWidget.setEnabled(cb.getValue());
                cb.addListener("changeValue", function(response) {
                    var t = response.getTarget();
                    t.childWidget.setEnabled(t.getValue());
                }, this);
            }
            return {controller : cnt, offset: vertical_offset};
        },
        fillSelect : function(sel, vals, alias, value) {
            sel.itemMap = [];
            for(var j=0; j<vals.length; j++) {
                var SI = vals[j];
                var selItem = new qx.ui.form.ListItem(SI[alias], null, SI[value]);
                sel.itemMap[SI[value]] = selItem;
                sel.add(selItem);
            }
        },
        /**
        **/
        addListeners: function() {            
            var _this = this;
            
            /* События виджетов для сопровождающей картикни  */
            this.picButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    bsk.view.Form.Upload.UploadFakeStatusBar.on();
                    
                    _this.picForm.setParameter("prev", _this.inp.Pic_url.getValue());
                    _this.inp.Pic_url.setValue(_this.picButton.getFileName());
                    _this.picForm.send();    
                }
            });
            this.picForm.addListener('completed',function(e) {
                var response = _this.picForm.getIframeTextContent();
                bsk.view.Form.Upload.UploadFakeStatusBar.off();
                _this.inp.Pic_url.setValue(response);
//                _this.imgBanner.setSource("/" + response);
            });  
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() {
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Pic_url)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Pic_url,  {row:0, column:0});
            this.picForm.setParameter('rm','upload');
            this.picForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.picForm, {row:0, column:1});
            this.picForm.add(this.picButton , {left:0,top:0});
            return picFormCnt;
        },

        fillForm : function(data) {
            if(this.fStatus == 0) {
                this.fData = data;
                return;
            }

            var dtStart = bsk.util.utils.getDateLocal(data.datestart, 0);
            var dtStop = bsk.util.utils.getDateLocal(data.datestop, 0);

            this.inp.Id = data.id;
            this.inp.Name.setValue(data.name);
            this.inp.ref.setValue(data.ref);
            this.inp.Pic_url.setValue(data.url);
            this.inp.datestart.setValue(dtStart);
            this.inp.datestop.setValue(dtStop);
            var bannerId = data.banner_place_id;
//            var item = this.inp.selBannerPlace.itemMap[bannerId];
//            this.inp.selBannerPlace.setSelection([item]);
/*            if(data.url && data.url != "")
                this.imgBanner.setSource("/" + data.url); */
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;

            flag &= bsk.view.Form.AbstractForm.customFormChkLength(1, bsk.Config.DOC_NAME_MAX_LEN, this.inp.Name);

            return flag;
        },
        
        _uploadData : function(e) {
//            this._dropInvalid();
            var res = {
                id : this.inp.Id,
                pic_url : this.inp.Pic_url.getValue(),
                datestart : this.inp.datestart.getValue().getTime(),
                datestop : this.inp.datestop.getValue().getTime(),
                name : this.inp.Name.getValue(),
                ref : this.inp.ref.getValue()
            };

            if(this.validateForm()) {
                this.uReq = new qx.io.remote.Request(this.urc.url, this.urc.method, this.urc.mimetype);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        }
    }
});


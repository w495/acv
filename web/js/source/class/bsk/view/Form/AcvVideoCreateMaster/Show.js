/* ************************************************************************
    https://gist.github.com/1639960
    
#asset(qx/icon/Tango/16/actions/document-save.png)
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.Show",
{
    extend : Object,
    
    construct : function(uReq, isNew) {
        
        this.isNew = isNew;
        
        console.log(this.isNew);
        
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
            url: "/get-acv-video/show",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        urc : {  // upload request config
            imgurl: "/update-acv-video/uload-video"
        },
        
        getComposite : function(){
            return this.composite;
        },

        inp : {
            Wish:       null,
            Shown:       null,
            Preroll:    null,
            Midroll:    null,
            Postroll:   null,
            Pauseroll:  null,
            Rerun_hours: null,
            Rerun_minutes: null
        },
        
        boxPlace: null,
        boxRe: null,
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Показ видео",  font: "bold",
                    alignX: "left", rich : true
                });
                
            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Показ видео",  font: "bold",
                    alignX: "left", rich : true
                });
                
            this.boxPlace = this.makeBoxPlace();
            this.boxRerun = this.makeBoxRerun();
            this.inp.Wish = new qx.ui.form.Spinner(0, 10, 1152921504606846976);
            this.inp.Shown = new qx.ui.form.Spinner(0, 0, 1152921504606846976)
                .set({enabled: false});
                
                
            var vertical_offset = -1;
            
            this.composite.add(pageName,
                {row:++vertical_offset, column:0, colSpan:2});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Желаемое количество",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Wish,   {row:vertical_offset, column:1});
            
            if(this.isNew){
                this.composite.add(new qx.ui.basic.Label().set({value: "Фактическое количество",  rich : true}),
                        {row:++vertical_offset, column:0});
                this.composite.add(this.inp.Wish,   {row:vertical_offset, column:1});
            }
            
            
            
            this.composite.add(this.boxPlace,
                {row:++vertical_offset, column:0,colSpan:2});
            
            this.composite.add(this.boxRerun,
                {row:++vertical_offset, column:0,colSpan:2});
            
            return this.composite;
        },
        
        
        makeBoxPlace : function() {
            this.inp.Preroll = new qx.ui.form.CheckBox("Preroll")
                .set({value: true});
            this.inp.Midroll = new qx.ui.form.CheckBox("Midroll")
                .set({value: true});
            this.inp.Postroll = new qx.ui.form.CheckBox("Postroll")
                .set({value: true});
            this.inp.Pauseroll = new qx.ui.form.CheckBox("Pauseroll")
                .set({value: false});
            
            var boxPlace = new qx.ui.groupbox.GroupBox("Размещение ролика");
            boxPlace.setLayout(new qx.ui.layout.VBox(2));
            boxPlace.add(this.inp.Preroll);
            boxPlace.add(this.inp.Midroll);
            boxPlace.add(this.inp.Postroll);
            //boxPlace.add(this.inp.Pauseroll);
            return boxPlace;
        },
        
        makeBoxRerun : function() {
            this.inp.Rerun_hours = new qx.ui.form.Spinner(0, 1, 24);
            this.inp.Rerun_minutes = new qx.ui.form.Spinner(0, 1, 60);
            var vertical_offset = 0;
            var boxRerun  = new qx.ui.groupbox.CheckGroupBox("Повтор ролика");
            //var boxRerun = new qx.ui.groupbox.GroupBox("Повтор ролика");
            var layout = new qx.ui.layout.Grid(1, 2);
            layout.setColumnFlex(1, 1);
            boxRerun.setLayout(layout);
            boxRerun.setValue(false);
            boxRerun.add(new qx.ui.basic.Label().set({value: "Часы",  rich : true}), {row:++vertical_offset, column:0});
            boxRerun.add(this.inp.Rerun_hours, {row:vertical_offset, column:1});
            boxRerun.add(new qx.ui.basic.Label().set({value: "Минуты",  rich : true}), {row:++vertical_offset, column:0});
            boxRerun .add(this.inp.Rerun_minutes, {row:vertical_offset, column:1});
            return boxRerun ;
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {            
            var _this = this;
            /* События виджетов для сопровождающей картикни  */
            this.picButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    bsk.view.Form.Upload.UploadFakeStatusBar.on();
                    
                    _this.picForm.setParameter("prev", _this.inp.Url.getValue());
                    _this.inp.Url.setValue(_this.picButton.getFileName());
                    _this.picForm.send();    
                }
            });
            this.picForm.addListener('completed',function(e) {
                var response = _this.picForm.getIframeTextContent();
                bsk.view.Form.Upload.UploadFakeStatusBar.off();
                _this.inp.Url.setValue(response);
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
            if(!this.inp.Url)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Url,  {row:0, column:0});
            this.picForm.setParameter('rm','upload');
            this.picForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.picForm, {row:0, column:1});
            this.picForm.add(this.picButton , {left:0,top:0});
            return picFormCnt;
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
                var res = {}
                
                if(this.boxRerun.getValue()){
                    res.rerun_hours     = this.inp.Rerun_hours.getValue();
                    res.rerun_minutes   = this.inp.Rerun_minutes.getValue();
                }else{
                    res.rerun_hours     = "null";
                    res.rerun_minutes   = "null";
                }
                
                for(var fieldName in this.inp){
                    if(("Rerun_hours" == fieldName) ||
                        ("Rerun_minutes" == fieldName))
                            continue;
                    item = fieldName.toLowerCase()
                    res[item] = this.inp[fieldName].getValue();
                }
                
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});


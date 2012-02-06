/* ************************************************************************
    https://gist.github.com/1639960
    
#asset(qx/icon/Tango/16/actions/document-save.png)
************************************************************************ */

qx.Class.define("bsk.view.Form.AcvVideoCreateMaster.Upload",
{
    extend : bsk.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        this.Options = Options;
        console.log("this.Options = = ", this.Options);
        this.base(arguments, uReq, Row, Options);
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
            url: "/get-acv-video/upload",
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
            Link_title:        null,
            Alt_title:         null,
            Ref:        null,
            Url:        null,
            Duration:          null
        },
        
        buildForm : function(){
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            /* Сопровождающая картинка */
            this.refButton = new bsk.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.refForm = new bsk.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);
                        
            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            
            this.inp.Duration =    new qx.ui.form.Spinner(1, 1, 134217728);
            this.inp.Link_title =   new qx.ui.form.TextField()
                .set({placeholder: "Текст ссылки"});
            this.inp.Alt_title =    new qx.ui.form.TextField()
                .set({placeholder: "Текст подсказки"});
            
            this.inp.Ref = new qx.ui.form.TextField()
                .set({placeholder: "http://my-company.com/"});
            this.inp.Url = new qx.ui.form.TextField()
                .set({placeholder: "http://my-company.com/"});
            
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Загруска видео",  font: "bold",
                    alignX: "left", rich : true
                });
                
            var vertical_offset = -1;
            this.composite.add(pageName, {row:++vertical_offset, column:0, colSpan:2})

            this.composite.add(new qx.ui.basic.Label().set({value: "Alt_title",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Alt_title,   {row:vertical_offset, column:1});

            this.composite.add(new qx.ui.basic.Label().set({value: "Link_title",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Link_title,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Продолжительность",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Duration,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Урл",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Url,   {row:vertical_offset, column:1});
            
            if((this.Options) && (!this.Options.disabled)){
                this.composite.add(new qx.ui.basic.Label().set({value: "Файл",  rich : true}),
                        {row:++vertical_offset, column:0});
                this.composite.add(this._buildPicFormCnt(),   {row:vertical_offset, column:1});
            }
            
            return this.composite;
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {
            var _this = this;
            /* События виджетов для сопровождающей картикни  */
            this.refButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    bsk.view.Form.Upload.UploadFakeStatusBar.on();
                    
                    _this.refForm.setParameter("prev", _this.inp.Ref.getValue());
                    _this.inp.Ref.setValue(_this.refButton.getFileName());
                    _this.refForm.send();    
                }
            });
            this.refForm.addListener('completed',function(e) {
                var response = _this.refForm.getIframeTextContent();
                bsk.view.Form.Upload.UploadFakeStatusBar.off();
                _this.inp.Ref.setValue(response);
            });
        },
        
        /**
            @overload
            Функция блокировки\разблокировки элементов ввода,
            которые не относятся
                к this.inp, и там их нельзя обработать.
        **/
        onChangeEnabled: function(enabled) {
            this.refForm.setEnabled(enabled)
            this.refButton.setEnabled(enabled)
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() {
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Ref)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Ref,  {row:0, column:0});
            this.refForm.setParameter('rm','upload');
            this.refForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.refForm, {row:0, column:1});
            this.refForm.add(this.refButton , {left:0,top:0});
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
                if("duration" == item)
                    this.inp[fieldName].setValue(parseInt(data.value[item]));
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


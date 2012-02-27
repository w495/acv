/* ************************************************************************



************************************************************************ */


qx.Class.define("bsk.view.Form.ConfigForm",
{
    extend : bsk.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
        this.addListeners();
    },

    members : {

        urc : {  // upload request config
            url: "/update-config",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-config",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : {
            Id: null,
            Acv_video_loadnext              : null
        },


        buildForm : function() {
            this.base(arguments);

            this.inp.Id = new qx.ui.form.TextField();
            this.inp.Acv_video_loadnext = new qx.ui.form.TextField();
            
            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);


            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Параметры системы");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = bsk.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label().set({value: "Acv_video_loadnext" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Acv_video_loadnext,      {row:vertical_offset , column:1});
            
            this.addbuttonRow(cnt, ++vertical_offset);

            this.controller.placeForm(cnt);
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
            Проверяет коректность данных
        **/
        validateForm : function() {

            var flag = true;

            /**
                TODO  Проверка введенного E-mail.
            **/

            return flag;
        },

        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e);
        },

        /**
            Заполняет форму
        **/
        fillForm : function(data) {
            this.base(arguments, data);

        }
    }
});


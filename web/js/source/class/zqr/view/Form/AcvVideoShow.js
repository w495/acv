/* ************************************************************************
    Мастер создания видео рекламы.
    
************************************************************************ */


qx.Class.define("zqr.view.Form.AcvVideoShow",
{
    
    extend : zqr.view.Form.BaseForm,
    
    construct : function(controller, Row, formDescr) {
        console.log("formDescr = ", formDescr);
        this.isModerator = formDescr.isModerator;
        this.base(arguments, controller, Row);
        this.addListeners();
    },
    
    members : {
        urc : {  // upload request config
        
            /**
                не удалено, но заменено на /chstate-acv-video
                """
                    url: {
                        accept: "/activate-acv-video",
                        reject: "/disactivate-acv-video"
                    },
                """
            */
            url: "/chstate-acv-video",
            method: "POST",
            mimetype: "application/json"
        },
        drc : {  // download request config
            url: "/get-acv-video",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : {
            Id                  : null,
            Pay_status          : null,
            Active              : null,
            Sum                 : null
        },
        
        taSummary   : null, /* текстовое описание рекламной кампании */
        flashBar    : null, /* поле флеш */
        flashPlayer : null, /* объект плеера */
        mailtoLink  : null, /* кнопка c mailto: */

        buildForm : function() {
            this.base(arguments);
            var layout = new qx.ui.layout.Grid(2, 1);
            var cnt = new qx.ui.container.Composite(layout);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            var mholder = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var lholder = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            this.taSummary = new qx.ui.form.TextArea()
                .set({width:400, height:300, readOnly: true});
            lholder.add(this.taSummary, {flex : 1});
            if(this.isModerator){
                /**
                    Если видео просматривает модератор,
                    то появляется дополнительный набор полей,
                    которые модератор может отредактировать.
                **/
                this.inp.Id = new qx.ui.form.TextField();
<<<<<<< HEAD
                this.inp.Active = new qx.ui.form.CheckBox("");
                this.inp.Sum = new qx.ui.form.Spinner(0, 0, 134217728).set({
                    enabled: false
                });
                this.inp.Pay_status = new qx.ui.form.TextField().set({
                    readOnly: true
                });
                
                var lhtl = new qx.ui.layout.Grid(3, 1);
                lhtl.setColumnFlex(1, 1);
                lhtl.setColumnAlign(0, "right", "top");
                var lhtable = new qx.ui.groupbox.GroupBox();
                lhtable.setLayout(lhtl);

                this.mailtoLink = new qx.ui.embed.Html();
                
                var vertical_offset = -1;
                /* -------------------------------------------------------- */
                lhtable.add(new qx.ui.basic.Label().set({value: "Разрешена",  rich : true}),
                        {row:++vertical_offset, column:0});
                lhtable.add(this.inp.Active,   {row:vertical_offset, column:1});
                /* -------------------------------------------------------- */
                lhtable.add(new qx.ui.basic.Label().set({value: "Сумма",  rich : true}),
                        {row:++vertical_offset, column:0});
                lhtable.add(this.inp.Sum,   {row:vertical_offset, column:1});
                lhtable.add(new qx.ui.basic.Label().set({value: "[руб]",  rich : true}),
                        {row:vertical_offset, column:2});
                /* -------------------------------------------------------- */
//                 lhtable.add(new qx.ui.basic.Label().set({value: "Состояние счета",  rich : true}),
//                         {row:++vertical_offset, column:0});
//                 lhtable.add(this.inp.Pay_status,   {row:vertical_offset, column:1});
                /* -------------------------------------------------------- */
                
                lholder.add(lhtable);
                lholder.add(this.mailtoLink);
=======
                this.inp.Active = new qx.ui.form.CheckBox("Разрешена");
//        	this.inp.email = new qx.ui.basic.Label("").set({rich: true});
                lholder.add(this.inp.Active);
//                lholder.add(this.inp.email);
>>>>>>> f6b84ad28136c0e218cb42ac0e12f67c01a886ae
            }
            mholder.add(lholder, {flex : 1});
            this.flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox())
                .set({width:448, height:336});
            mholder.add(this.flashBar);
            var vertical_offset = 0;
            cnt.add(mholder,       {row:vertical_offset , column:1});
            if(this.isModerator){
                this.addbuttonRow(cnt, ++vertical_offset);
            }
            this.controller.placeForm(cnt);
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function(){
            var _this = this;
            this.inp.Active.addListener("changeValue",function(event){
               _this.inp.Sum.setEnabled(_this.inp.Active.getValue());
            });
        },


        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            
            return flag;
        },

        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this._dropInvalid();
            var res = {}
            for(var fieldName in this.inp){
                item = fieldName.toLowerCase()
                res[item] = this.inp[fieldName].getValue();
            }
            if(this.validateForm()) {
                this.uReq = new qx.io.remote.Request
                    (this.urc.url, this.urc.method, this.urc.mimetype);
                this.uReq.setTimeout(60000);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        },
        
        /**
            Заполняет форму
        **/

        fillForm : function(result) {

            var clip = result.value;
            if(this.isModerator){
                /**
                    Если видео просматривает модератор,
                    то появляется дополнительный набор полей,
                    которые модератор может отредактировать.
                **/
                this.inp.Id.setValue(clip.id);
                this.inp.Active.setValue(RegExp("^true$").test(clip.active));
                this.inp.Sum.setValue(parseInt(clip.sum));

                this.mailtoLink.setHtml("<button><a target='_blank' href='mailto:"+clip.email+"' style='text-decoration:none;color:black !important;'>Отправить сообщение</a></button>");
            }
            console.log("clip = ", clip);
            var catList = result.cats.values;
            var geoList = result.geo.values;
            var txt = "Рекламная кампания, размещение видео в видео.\n";
            txt += "Название: " + clip.name + "\n";
            txt += "Комментарий: " + clip.comment + "\n";
            
            txt += "-------------------\n";
            txt += "Состояние счета: ";
            switch(clip.pay_status) {
                case "":        txt += "счет не выставлен"; break;
                case "false":   txt += "счет выставлен, но не оплачен"; break;
                case "true":    txt += "счет выставлен и оплачен"; break;
            }
            txt += "\n";
            txt += "Статус: ";
            switch(clip.active) {
                case "":        txt += "на модерации"; break;
                case "false":   txt += "запрещен"; break;
                case "true":    txt += "разрешен"; break;
            }
            txt += "\n";
            txt += "-------------------\n";

            txt += "Дата начала: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestart, 0)) + "\n";
            txt += "Дата конца: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestop, 0)) + "\n";
            txt += "Внешняя ссылка: " + clip.url + "\n";
            txt += "URL ролика: " + clip.ref + "\n";
            txt += "Продолжительность ролика: " + clip.duration + " секунд\n";
            txt += "Желаемое количество показов: " + clip.wish + "\n";
            txt += "Показано: " + clip.shown + "\n";
            var tpl = "";
            var apl = ["preroll", "midroll", "postroll"];
            for(var i=0; i<apl.length; i++) {
                if(clip[apl[i]] == "true") {
                    if(tpl != "")
                        tpl += ", ";
                    tpl += apl[i];
                }
            }
            txt += "Размещение ролика: " + tpl + "\n";
            txt += "Повторный показ ролика учтенному пользователю: ";
            if(clip.rerun_hours == "")
                txt += "не ограничен";
            else
                txt += clip.rerun_hours + ":" + clip.rerun_minutes;
            txt += "\n";
            txt += "\nТаргетирование пользователей:\n";
            txt += "Пол: ";
            switch(clip.user_male) {
                case "true": txt += "для мужчин"; break;
                case "false": txt += "для женщин"; break;
                default: txt += "---";
            }
            txt += "\n";
            txt += "Возраст:";
            if(clip.age_from == "")
                txt += "---";
            else
                txt += "от " + clip.age_from + " до " + clip.age_to + " лет";
            txt += "\n";
            txt += "Время показа: ";
            if(clip.time_from == "")
                txt += "---";
            else
                txt += "с " + clip.time_from + " до " + clip.time_to + " часов";
            txt += "\n";
            txt += "\nТаргетирование по регионам: ";
            if(geoList.length == 0)
                txt += "весь мир\n";
            else
                txt += "\n";
            for(var i=0; i<geoList.length; i++)
                txt += geoList[i].name_ru + " (" + geoList[i].code + ")\n";
            txt += "\nТаргетирование по жанрам: ";
            if(!catList)
                catList = [];
            if(catList.length == 0)
                txt += "все жанры\n";
            else
                txt += "\n";
            for(var i=0; i<catList.length; i++)
                txt += catList[i].name + "\n";
            txt += "\n";

            if(this.isModerator) {
                txt += "Email пользователя: " + clip.email;
            }
            this.taSummary.setValue(txt);


            this.flashPlayer = new qx.ui.embed.Flash("resource/zqr/flash/tvzavrplayer2.swf").set({
                width: 448,
                height: 336,
                variables : {
            	    autoplay:0,
            	    src:"/" + clip.ref
                }
            });

            this.flashBar.add(this.flashPlayer);//, {flex: 1});
            return true;
        }
    }
});


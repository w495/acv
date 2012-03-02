/* ************************************************************************
    Мастер создания видео рекламы.
    
************************************************************************ */


qx.Class.define("bsk.view.Form.AcvVideoShow",
{
    
    extend : bsk.view.Form.BaseForm,
    
    construct : function(controller, Row, formDescr) {
        console.log("formDescr = ", formDescr);
        this.isModerator = formDescr.isModerator;
        this.base(arguments, controller, Row);
        this.addListeners();
    },
    
    members : {
        urc : {  // upload request config
            url: "/update-video-state",
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
            Active              : null
        },
        
        taSummary   : null,
        flashBar    : null,
        flashPlayer : null,
        
        buildForm : function() {
            this.base(arguments);
            var layout = new qx.ui.layout.Grid(2, 1);
            var cnt = new qx.ui.container.Composite(layout);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            var mholder = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var lholder = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            this.taSummary = new qx.ui.form.TextArea()
                .set({width:500, height:300, readOnly: true});
            lholder.add(this.taSummary, {flex : 1});
            if(this.isModerator){
                this.inp.Active = new qx.ui.form.CheckBox("Разрешена");
                lholder.add(Preroll);
            }
            mholder.add(lholder, {flex : 1});
            this.flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox())
                .set({width:640, height:480});
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
            this.base(arguments, e);
        },

        /**
            Заполняет форму
        **/

        fillForm : function(result) {
            var clip = result.value;
            
            console.log("clip = ", clip);
            
            var catList = result.cats.values;
            var geoList = result.geo.values;
            var txt = "Рекламная кампания, размещение видео в видео.\n";
            txt += "Название: " + clip.name + "\n";
            txt += "Комментарий: " + clip.comment + "\n";
            txt += "Статус: ";
            switch(clip.active) {
                case "":        txt += "на модерации"; break;
                case "false":   txt += "запрещен"; break;
                case "true":    txt += "разрешен"; break;
            }
            txt += "\n";
            txt += "Дата начала: " + bsk.util.utils.formatJsDateTime(bsk.util.utils.getDate(clip.datestart, 0)) + "\n";
            txt += "Дата конца: " + bsk.util.utils.formatJsDateTime(bsk.util.utils.getDate(clip.datestop, 0)) + "\n";
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
                txt += geoList[i].name + " (" + geoList[i].code + ")\n";
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
            this.taSummary.setValue(txt);
            this.flashPlayer = new qx.ui.embed.Flash("resource/bsk/flash/gddflvplayer.swf").set({
//                scale: "noscale",
                width: 640,
                height: 480,
                variables : {
                    vdo: "/" + clip.ref,
//                    vdo: "/static/data/acv-video/common/5831108/adv02.mp4",
                    autoplay : "false"
                }
            });
            this.flashBar.add(this.flashPlayer);//, {flex: 1});
            return true;
        }
    }
});


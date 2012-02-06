
/* ************************************************************************
    Соединение описания форм с элементами меню
************************************************************************ */

qx.Class.define("bsk.view.StaticForms",
{
    extend : Object,

    statics : {
        // форма создания и редактирования рекламной компании
        advComVidForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.AdvComVidForm(biz, Row, formDescr);
        },
        
        // форма создания и редактирования рекламной компании
        advComForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.AdvComForm(biz, Row, formDescr);
        },
        
        // мастер создания и редактирования рекламной компании
        acvVideoCreate : function(biz, Row, formDescr) {
            return new bsk.view.Form.AcvVideoCreateMaster(biz, Row, formDescr);
        },

        // мастер создания и редактирования рекламной компании
        acvVideoCopy : function(biz, Row, formDescr) {
            // пока показываем тоже самое, что и при создании
            return new bsk.view.Form.AcvVideoCreateMaster(biz, Row, formDescr);
        },
        
        // мастер создания и редактирования рекламной компании
        acvVideoShow : function(biz, Row, formDescr) {
            // пока показываем тоже самое, что и при создании
            return new bsk.view.Form.AcvVideoShow(biz, Row, formDescr);
        },
        
        // форма создания и редактирования рекламной компании
        advComVidForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.AdvComVidForm(biz, Row, formDescr);
        },
        
        /// Форма создания и редактирования групп пользователей
        customerGroupForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.CustomerGroupForm(biz, Row, formDescr);
        },
        
        /// Форма создания и редактирования пользователей
        customerForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.CustomerForm(biz, Row, formDescr);
        }
    }
});


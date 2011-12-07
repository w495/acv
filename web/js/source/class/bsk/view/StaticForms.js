
/* ************************************************************************

************************************************************************ */

qx.Class.define("bsk.view.StaticForms",
{
    extend : Object,

    statics : {
        // форма создания и редактирования рекламной компании
        advComForm : function(biz, Row, formDescr) {
            return new bsk.view.Form.AdvComForm(biz, Row, formDescr);
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


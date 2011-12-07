

qx.Class.define("bsk.util.errors",
{
    statics : {
         process : function(resp) {
            if(resp.REDIRECT != undefined) {
                window.location=resp.REDIRECT;
                return false;
            }
            if(resp.RELOAD != undefined) {
                location.reload();
                return false;
            }
            if(resp.ERROR != undefined) {
                //alert(resp.ERROR);
//                this.login_dialog = new christine.view.LoginDialog(this.root);
                return false;
            }
            return true;

        }
    }
});

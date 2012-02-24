/*lib*/
(function($){
    $.fn.geti = function(n){for(var i = 0; i != n; ++i)
        if($(this).parent().parent().children()[i]==$(this).parent()[0])
            return (i+1); return -1;
    };
    $.fn.swp = function(a,n,c){$(this).click(function(){
        var i = $(this).geti(n);
        $(a).hide();
        $(a+":nth-child("+i+")").show();
        $(this).parent().parent().children().children().removeClass(c)
        $(this).addClass(c);
        return false;
    });};
})(jQuery);
/*main*/
$(function() {
   $(".e-rn-but").swp(".s-roller-frame", 3, "cur");
});

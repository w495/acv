/*lib*/
(function($){
    $.fn.geti = function(n){for(var i = 0; i != n; ++i)
        if($(this).parent().parent().children()[i]==$(this).parent()[0])
            return (i+1); return -1;
    };
    $.fn.swp = function(a, n){$(this).click(function(){
        var i = $(this).geti(n);
        $(a).hide('slow');
        $(a+":nth-child("+i+")").show('slow');
        $(this).parent().parent().children().children().removeClass("current")
        $(this).addClass("current");
        return false;
    });};
})(jQuery);
/*main*/
$(function() {
   $(".e-rn-but").swp(".s-roller-frame", 3);
});

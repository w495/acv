# -*- coding: utf-8 -*-

###
### UTIL
###

# -----------------------------------------------------------------------------
# to base 64
# -----------------------------------------------------------------------------

import base64

from generate.gen_config import Use as Use;

def tobase64(picname):
    '''
        Конструирует строку для BASE64 изображения.
        MIME определяется на основе расширения.
            >>> import generate
            >>> generate.tobase64("css-images-arc/ph-1/doc.png")
    '''
    if(not Use.compilation):
        return picname
    picext = picname.split(".")[-1]
    return tobase64mimed(picname, picext)

    #

def tobase64mimed(picname, mime):
    '''
        Конструирует строку для BASE64 изображения с указанным MIME
    '''
    return "data:image/%s;base64,%s"%(mime, tobase64core(picname))

def tobase64core(fname):
    '''
        Считывает данный из файла и переводит их в BASE64
    '''
    fstr = open(fname, "rb").read()
    return base64.b64encode(fstr)

# -----------------------------------------------------------------------------
# etc
# -----------------------------------------------------------------------------

import re

def trunc(string):
    if(not Use.compilation):
        return string;
    string = re.sub("\s+", " ", string)
    string = re.sub("\s+@", "@", string)
    string = re.sub("[,]\s", ",", string)
    string = re.sub("[;]\s", ";", string)
    string = re.sub("[:]\s", ":", string)
    string = re.sub("\s?{\s?", "{", string)
    string = re.sub("}\s?", "}\n", string)
    return string + "\n"

def lgt(color_1, color_2, alt_color, image=None):
    res = """
        background-color: %(alt_color)s;
        background: -moz-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -webkit-gradient(linear, left top, left bottom, color-stop(0%%,%(color_1)s), color-stop(100%%,%(color_2)s));
        background: -webkit-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -o-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -ms-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -khtml-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: linear-gradient(top, %(color_1)s, %(color_2)s);
        filter: progid:DXImageTransform.Microsoft.gradient(GradientType=0,startColorstr='%(color_1)s',endColorstr='%(color_2)s');
        -ms-filter: "progid:DXImageTransform.Microsoft.gradient(GradientType=0, startColorstr='%(color_1)s', endColorstr='%(color_2)s')";
    """ % {'color_1': color_1, 'color_2': color_2, 'alt_color': alt_color}
    return res

def rgc(x, y, color_1, color_2, alt_color, image=None):
    res = """
        background-color: %(alt_color)s;
        background: -moz-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -webkit-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -ms-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -o-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -o-radial-gradient(%(x)s, %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -khtml-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: radial-gradient(%(x)s, %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
    """ % {'x': x, 'y': y, 'color_1': color_1, 'color_2': color_2, 'alt_color': alt_color}
    return res

def br(radius):
    return """
        -moz-border-radius: %(radius)s;
        -webkit-border-radius: %(radius)s;
        -khtml-border-radius: %(radius)s;
        border-radius: %(radius)s;
    """ % {'radius': radius}

def __reset():
    return "html, body, div, span, applet, object, iframe,h1, h2, h3, h4, h5, h6, p, blockquote, pre,a, abbr, acronym, address, big, cite, code,del, dfn, em, img, ins, kbd, q, s, samp,small, strike, strong, sub, sup, tt, var,b, u, i, center,dl, dt, dd, ol, ul, li,fieldset, form, label, legend,table, caption, tbody, tfoot, thead, tr, th, td,article, aside, canvas, details, embed, figure, figcaption, footer, header, hgroup, menu, nav, output, ruby, section, summary,time, mark, audio, video {margin: 0;padding: 0;border: 0;font-size: 100%;font-family:'PT Sans Narrow','Cuprum',sans-serif; vertical-align: baseline;}article, aside, details, figcaption, figure, footer, header, hgroup, menu, nav, section {display: block;}body {line-height: 1;}ol, ul {list-style: none;}blockquote, q {quotes: none;} blockquote:before, blockquote:after, q:before, q:after {content: '';content: none;} table {border-collapse: collapse;border-spacing: 0;}\n"

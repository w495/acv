#! /usr/bin/env python
# -*- coding: utf-8 -*-


import re
import time

### 
### UTIL
###

# -----------------------------------------------------------------------------
# to base 64
# -----------------------------------------------------------------------------

import base64

def tobase64(picname):
    '''
        Конструирует строку для BASE64 изображения.
        MIME определяется на основе расширения.
            >>> import generate
            >>> generate.tobase64("css-images-arc/ph-1/doc.png")
    '''
    picext = picname.split(".")[-1]
    return tobase64mimed(picname, picext)

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

def trunc(string):
    string = re.sub("\s+", " ", string)
    string = re.sub("[,]\s", ",", string)
    string = re.sub("[;]\s", ";", string)
    string = re.sub("[:]\s", ":", string)
    string = re.sub("\s?{\s?", "{", string)
    string = re.sub("}\s?", "}\n", string)
    return string + "\n"

def reset():
    return "html, body, div, span, applet, object, iframe,h1, h2, h3, h4, h5, h6, p, blockquote, pre,a, abbr, acronym, address, big, cite, code,del, dfn, em, img, ins, kbd, q, s, samp,small, strike, strong, sub, sup, tt, var,b, u, i, center,dl, dt, dd, ol, ul, li,fieldset, form, label, legend,table, caption, tbody, tfoot, thead, tr, th, td,article, aside, canvas, details, embed, figure, figcaption, footer, header, hgroup, menu, nav, output, ruby, section, summary,time, mark, audio, video {margin: 0;padding: 0;border: 0;font-size: 100%;font: inherit;vertical-align: baseline;}article, aside, details, figcaption, figure, footer, header, hgroup, menu, nav, section {display: block;}body {line-height: 1;}ol, ul {list-style: none;}blockquote, q {quotes: none;} blockquote:before, blockquote:after, q:before, q:after {content: '';content: none;} table {border-collapse: collapse;border-spacing: 0;}\n"

def linear_gradient_top(color_1, color_2, alt_color):
    return """
        background: %(alt_color)s;
        background: -moz-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -webkit-gradient(linear, left top, left bottom, color-stop(0%%,%(color_1)s), color-stop(100%%,%(color_2)s));
        background: -webkit-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -o-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: -ms-linear-gradient(top, %(color_1)s, %(color_2)s);
        background: linear-gradient(top, %(color_1)s, %(color_2)s);
        filter: progid:DXImageTransform.Microsoft.gradient(GradientType=0,startColorstr='%(color_1)s',endColorstr='%(color_2)s');
        -ms-filter: "progid:DXImageTransform.Microsoft.gradient(GradientType=0, startColorstr='%(color_1)s', endColorstr='%(color_2)s')";
    """ % {'color_1': color_1, 'color_2': color_2, 'alt_color': alt_color}

def radial_gradient_circle(x, y, color_1, color_2, alt_color):
    return """
        background: %(alt_color)s;
        background: -moz-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -webkit-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: radial-gradient(%(x)s, %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
        background: -ms-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
    """ % {'x': x, 'y': y, 'color_1': color_1, 'color_2': color_2, 'alt_color': alt_color}

#         background: -o-radial-gradient((%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
#         background: radial-gradient(%(x)s, %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
#         background: -ms-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
#         background: -webkit-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
#         background: -o-radial-gradient((%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);
#         background: -ms-radial-gradient(%(x)s %(y)s, circle, %(color_1)s, %(color_2)s, %(color_2)s);

###
### Base
###
class Base:
    def __init__(self, filename):
        f = open(filename, "wb")
        string = ""
        string += reset()
        ### Тут не надо применять мета-классы,
        ###     так как порядок для css имеет значение
        string += self.body()
        string += self.s_main()
        string += self.s_header()
        string += self.s_roller()
        string += self.s_news()
        string += self.s_footer()
        string = "/* auto gen %s */\n"% time.ctime() + trunc(string)
        f.write(string)
        f.close()

    def body(self):
        res = "body{text-align:center;}"
        res = "body{%s}"%(
            linear_gradient_top('#ffffff', '#cccccc', '#cccccc'))
        return res

    def s_main(self):
        return  """
            .s-main{
            }
            .s-main{
                background-color: #cccccc;
            }
            .s-header, .s-main, .s-footer{
                display: block;
                position: relative;
                width: 1000px;
                margin-left:auto;
                margin-right:auto;
            }

            .b-header-group{
                display: block;
            }

            .b-thehead{
                display: block;
                height: 100px;
            }
        """

    def s_header(self):
        def _b_thehead():
            return ".b-thehead{" \
                + linear_gradient_top('#ffffff', '#cccccc', '#cccccc') \
                + "}"
        def _b_thehead_caption():
            return  """
                .b-thehead-caption {
                    display: block;
                    text-align: right;
                }
                .b-thehead-caption {
                    background-color: black;
                    color: white;
                }
                .b-thehead-caption {
                    padding-right: 1em;
                    color: white;
                }
                .b-siginblock{
                    display: block;
                    background-color: red;
                    float: right;
                    position: relative;
                    top: -100px;
                    width: 200px;
                    height: 200px;
                }
                .s-roller{
                    border-top: solid 4px #0898cd;
                    height: 300px;
                }
                .s-news{
                    height: 200px;
                }
                .s-about{
                    height: 200px;
                }
            """
        res = "";
        res += _b_thehead()
        res += _b_thehead_caption()
        return res;

    def s_roller(self):
        return ".s-roller{" \
            + radial_gradient_circle('60%', '60%', '#1758a8', '#05254b', '#cfcfcf') \
            + "}\n"

    def s_news(self):
        def _b_news_header():
            res = ""
            res += """.b-news-header{
            }
            """
            res += ".b-news-header { height: 50px;" \
                + linear_gradient_top('#0792e7', '#0ab6e6', '#cccccc') \
                + "}\n"
            return res

        padding_top = "15px;"
            # для замыканий _e_news_head, _b_news_doc

        def _e_news_head():
            return """.e-news-head{
                display: block;
                width: 500px;
                color: white;
                margin-top: %s;
                padding: 8px 10px 10px 40px;
                float:left;
            }
            """%(padding_top)

        def _b_news_doc():
            res = """.b-news-doc {
                display: block;
                position: relative;
                float:right;
                padding: 8px 10px 10px 40px;
                margin-top: %s;
                background-image: url(%s);
                background-repeat: no-repeat;
            }
            """%(padding_top, tobase64("css-images/doc.png"))
            res += """.b-news-doc {
                color:white;
                text-decoration: none;
            }
            """
            res += """.b-news-doc:hover{
                color:white;
                text-decoration: none;
            }
            """
            return res;

        res = ""
        res += ".s-news{%s}"%(
            linear_gradient_top('#888888', '#cccccc', '#cccccc'))
        res += _b_news_header()
        res += _e_news_head()
        res += _b_news_doc()
        return res;

    def s_about(self):

        return res;
        
    def s_footer(self):
        return ".b-footer{" \
            + linear_gradient_top('#0192d5', '#02618d', '#cfcfcf') \
            + "}"

def main():
    Base("base.css")
    return "ok";

if(__name__ == '__main__'):
    print main()


# -*- coding: utf-8 -*-
## Тут аццкий былокод, но так было быстрее и проще

import generate.gen_css

from generate.gen_config import d as d;
from generate.gen_config import Metrics as Metrics;
from generate.gen_config import Use as Use;

class Base(generate.gen_css.Gen_css):
    def css(self):
        string = ""
        ### Тут не надо применять мета-классы,
        ###     так как порядок для css имеет значение
        string += self.font_face()
        string += self.reset()
        string += self.common_modifiers()
        string += self.body()
        string += self.s_main()
        string += self.s_header()
        string += self.s_roller()
        string += self.s_news()
        string += self.s_about()
        string += self.s_footer()
        return string

    def font_face(self):
        res = ""
        res += """@font-face{
            font-family: 'Cuprum';
            font-style: normal;
            font-weight: normal;
            src: local('Cuprum'), url('css-fonts/cuprum.woff') format('woff');
        }
        """
        # src: local('Cuprum'), url('http://themes.googleusercontent.com/static/fonts/cuprum/v3/wHBEZCjwNI3HN2fD1RQJgw.woff') format('woff');
        res += """@font-face{
            font-family: 'PT Sans Narrow';
            font-style: normal;
            font-weight: normal;
            src: local('PT Sans Narrow'), local('PTSans-Narrow'), url('css-fonts/psn.woff') format('woff');
        }
        """
        # src: local('PT Sans Narrow'), local('PTSans-Narrow'), url('http://themes.googleusercontent.com/static/fonts/ptsansnarrow/v2/UyYrYy3ltEffJV9QueSi4S4mX3cpNo8MnLri8k21-rs.woff')
        return res

    def reset(self): # outline: solid 1px red;
        return """
        html, body, div, span, applet, object, iframe,
        h1, h2, h3, h4, h5, h6, p, blockquote, pre,a, abbr, acronym,
        address, big, cite, code,del, dfn, em, img, ins, kbd, q, s,
        samp,small, strike, strong, sub, sup, tt, var,b, u, i, center,
        dl, dt, dd, ol, ul, li,fieldset, form, label, legend,table,
        caption, tbody, tfoot, thead, tr, th, td,article, aside,
        canvas, details, embed, figure, figcaption, footer, header,
        hgroup, menu, nav, output, ruby, section, summary,time,
        mark, audio, video{
            margin: 0;
            padding: 0;
            border: 0;
            font-size: 100%;
            font-family:'PT Sans Narrow','Cuprum',sans-serif;
            vertical-align: baseline;
        }
        article, aside, details, figcaption, figure, footer, header,
        hgroup, menu, nav, section{
            display: block;
        }
        body{
            line-height: 1;
        }
        ol, ul{
            list-style: none;
        }
        blockquote, q{
            quotes: none;
        }
        blockquote:before, blockquote:after, q:before, q:after {
            content: '';content: none;
        }
        table{
            border-collapse: collapse;
            border-spacing: 0;
        }
        """

    def common_modifiers(self):
        res = ""
        res += """.hidden{display:none;}"""
        res += """.visible{display:block;}"""
        res += """.m-l{
            cursor:pointer;
        }"""
        res += """.m-l:hover{
            cursor:pointer;
        }"""

        return res
        
    def body(self):
        res = """body{
            text-align:center;
            font-family:Cuprum,Georgia,Serif;
        }"""
        res = "body{%s%s}"%(
            generate.util.lgt('#ffffff', '#cccccc', '#e4e6e3'),
            "background: #e4e6e3 url(%s) repeat-x;"%(
                generate.util.tobase64("css-images/body.png"),
            )
        )

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
                width: %(gwidth)s;
                margin-left:auto;
                margin-right:auto;
            }
            .b-header-group{
                display: block;
            }
        """%(d(Metrics))

    def s_header(self):
        def _b_thehead():
            res = """.b-thehead{0/login
                display: block;
                height: 81px;
            }"""
            if(Use.bgi_grad):
                res += ".b-thehead{%s}"%(
                    "background: #cccccc url(%s);"%(
                        generate.util.tobase64("css-images/b-thehead.gif")
                    )
                )
            if(Use.css3_grad):
                res += ".b-thehead{%s}"%(
                    generate.util.lgt('#ffffff', '#cccccc', '#cccccc'),
                )
            return res

        def _b_thehead_caption():
            res = ""
            res += """.b-thehead-caption {
                    display: block;
                    text-align: right;
                }
            """
            res += """.b-thehead-caption {
                    background-color: black;
                    color: white;
                }
            """
            res += """.b-thehead-logo{
                    position: relative;
                    top:10px;
                    left:20px;
                }
            """
            res += """.b-thehead-caption {
                    padding-right: 1em;
                    color: white;
                }
            """
            res += """.s-signin{
                    display: block;
                    /*background-color: red;*/
                    float: right;
                    text-align:right;
                    padding-right: 10px;
                    position: relative;
                    top: -60px;
                    width: 410px;
                    height: 30px;
                    vertical-align: middle;
                    padding-left: 10px;
                }
            """
            res += """.s-sfm-input{
                    font-size: 12px;
                    background-color: transparent;
                    padding-left: 10px;
                    margin-left: 10px;
                    -moz-border-radius: 20px;
                    -webkit-border-radius: 20px;
                    -khtml-border-radius: 20px;
                    border-radius: 20px;
                }
            """
            if(Use.css3_corners):
                res += """.s-sfm-input{
                        width:165px;
                        height:24px;
                        background-color: white;
                    }
                """
            if(Use.bgi_corners):
                res += ".s-sfm-input{%s%s}"%(
                    "background: url(%s) no-repeat;"%(
                        generate.util.tobase64("css-images/s-sfm-input.png"),
                    ),
                    "border:none;width:160px;height:28px;"
                )

            res += """.s-sfm-lc{
                    padding-top: 5px;
                    font-size: 12px;
                }
            """
            res += """.s-sfm-lsignup{
                    padding-top: 5px;
                    font-size: 12px;
                }
            """
            res += """.s-sfm-l{
                    padding-top: 5px;
                    color: black;
                    text-decoration: underlined;
                }
            """
            res += """.s-sfm-l:hover{
                    color: black;
                    text-decoration: none;
                }
            """
            res += """.s-sfm-but{
                    font-size: 16px;
                    background-color: transparent;
                    border: none;
                }
            """
            return res;
            
        res = "";
        res += _b_thehead()
        res += _b_thehead_caption()
        return res;

    def s_roller(self):
        res = ""
        res += """.s-roller{
                border-top: solid 4px #0898cd;
                height: 344px;
                width: %(gwidth)s;
                overflow:hidden;
            }
        """%(d(Metrics))
        
        res += """.b-roller{
                height: 240px;
            }
        """
        res += """.s-roller-frames{
                display: block;
                max-height: 140px;
            }
        """
        
        if(Use.css3_grad):
            res += ".s-roller{%s}"%(
                generate.util.rgc("60%", "60%",'#1758a8', '#05254b', '#05254b')
            )
        if(Use.bgi_grad):
            res += ".s-roller{%s}"%(
                "background: #05254b url(%s);"%(
                    generate.util.tobase64("css-images/s-roller.jpg")
                )
            )

        res +=""".b-rf-text{
            float: left;
            min-width:200px;
            max-width:500px;
            margin-left: 40px;
        }
        """

        res +=""".b-rf-head{
            color: #259a3f;
            font-size: 48px;
            font-weight: normal;
        }
        """

        res +=""".b-rf-content{
            margin: 10px 0px 0px 0px;
            color: white;
        }
        """
        
        res += """.b-roller-nav{
            text-align: center;
            clear: both;
        }
        """
        
        res += """.b-rn-ul{
            margin: auto;
            width: 200px;
        }
        """
        
        res += """.e-rn-ul{
            float: left;
        }
        """

        res += """.e-rn-but{
            margin: 0px 5px 10px 0px;
            width: 20px;
            height: 20px;
            display: block;
            -moz-border-radius: 20px;
            -webkit-border-radius: 20px;
            -khtml-border-radius: 20px;
            border-radius: 20px;
            background-color: white;
            border: inset 2px #cccccc;
            outline: none;
        }
        """

        res += """.e-rn-but:hover{
            background-color: #cccccc;
        }
        """
        res += """.e-rn-but:active{
            background-color: #c0c0c0;
        }
        """
        
        res += """.e-rn-but.cur{
            background-color: #cccccc;
        }
        
        """
        res +=""".b-rf-ul{
            margin: 0px 0px 0px 15px;
            list-style: outside url(%s);
        }
        """%(
            generate.util.tobase64("css-images/b-rf-ul.png")
        )

        res +=""".e-rf-ul{
            margin: 5px 0px 10px 0px;
        }
        """

        res +=""".b-rf-caption{
            color: white;
            font-size: 36px;
            font-weight: normal;
        }
        """

        res +=""".b-rf-picture{
            float: right;
            position: relative;
            right:  100px;
            top:    -10px;
            width:  400px;
            height: 300px;
        }
        """
        return res

    def s_news(self):
        def _b_news_header():
            res = ""
            res += """.b-news-header{
                clear: both;
                height: 50px;
            }
            """
            if(Use.css3_grad):
                res += ".b-news-header{%s}"%(
                    generate.util.lgt('#0792e7', '#0ab6e6', '#0ab6e6')
                )
            if(Use.bgi_grad):
                res += ".b-news-header{%s}"%(
                    "background: #0ab6e6 url(%s);"%(
                        generate.util.tobase64("css-images/b-news-header.png")
                    )
                )
            return res

        top_offset = "10px";
        
        def _e_news_head():
            return """.e-news-head{
                display: block;
                width: 600px;
                font-size: 30px;
                font-variant: normal;
                font-width: normal;
                font-weight: normal;
                color: white;
                margin-top: 10px;
                padding: 0px 0px 0px 10px;
                float:left;
            }
            """

        def _b_news_doc():
            res = """.b-news-doc {
                display: block;
                position: relative;
                float:right;
                font-size: 14px;
                padding: 8px 10px 10px 33px;
                margin-top: %s;
                background: url(%s) no-repeat;
            }
            """%(top_offset, generate.util.tobase64("css-images/doc.png"))
            res += """.b-news-doc:before{content:"— ";}"""
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

        def _b_news():
            res =  ""
            res += """.b-news{
                clear: both;
                height:368px;
            }
            """

            if(Use.css3_grad):
                res += ".b-news{%s}"%(
                    generate.util.lgt('#a7a7a7', '#ececec', '#a7a7a7')
                )
            if(Use.bgi_grad):
                res += ".b-news{%s}"%(
                    "background: #a7a7a7 url(%s);"%(
                        generate.util.tobase64("css-images/b-news.png")
                    )
                )

            res += """.s-news-list{
                margin-left: 60px;
            }

            .s-news-all{
                display: block;
                float:right;
                width: 100px;
            }
            """

            res += """.s-news-list-item{
                padding: 4px;
                margin: 4px;
                display: block;
                float:left;
            }
            """

            res += """.b-nlil{
                display: block;
                color:black;
                text-decoration: none;
            }
            """
            
            res += """.b-nli{
                background-color: white;
                border:  4px solid #cccccc;
                padding: 4px;
                display: block;
            }
            """

            res += """.b-nli:hover{
                border-color: gray;
                background-color: #cccccc;
            }
            """
            return res;

        res = """.s-news{
            clear: both;
        }
        """

        res += _b_news_header()
        res += _e_news_head()
        res += _b_news_doc()
        res += _b_news()

        return res;
        

    def s_about(self):
        res = ""
        
        res += """.s-about{
                height: 303px;
                background-color: white;
                padding-left: 22px;
            }
        """

        res += """.b-a{
            }
        """

        res += """.b-ah{
                font-size: 20pt;
                color: #0191d3;
            }
        """

        res += """.b-ac{
            }
        """

        res += """.b-ac-p{
            }
        """

        res +=""".b-ac-ul{
            margin: 0px 0px 0px 15px;
            list-style: outside url(%s);
        }
        """%(
            generate.util.tobase64("css-images/b-ac-ul.png")
        )


        res += """.e-ac-ul{
            }
        """
        
        return res;

    def s_footer(self):
        res = ""
        res = """.s-footer{
            clear: both;
        }
        """
        res += ".s-footer{" \
            + generate.util.lgt('#0192d5', '#02618d', '#cfcfcf') \
            + "}"
            
        return res;

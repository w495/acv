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
        # string += self.font_face()
        string += self.reset()
        string += self.common_modifiers()
        string += self.body()
        string += self.s_main()
        string += self.s_signup()
        string += self.s_header()
        string += self.s_roller()
        string += self.s_news()
        string += self.s_about()
        string += self.s_pers()
        string += self.s_footer()
        return string

    def font_face(self):
        res = ""
        #res += """
        #@font-face {
            #font-family: 'Cuprum';
            #src: url('css-fonts/cuprum.eot');
            #src: url('css-fonts/cuprum.eot?#iefix') format('embedded-opentype'),
                #url('css-fonts/cuprum.woff') format('woff'),
                #url('css-fonts/cuprum.ttf') format('truetype'),
                #url('css-fonts/cuprum.svg#CuprumRegular') format('svg');
            #font-weight: normal;
            #font-style: normal;
        #}
        #"""
        
        #res += """
        #@font-face {
            #font-family: 'CuprumRegular';
            #src: url('/c/cuprum-webfont.eot');
            #src: local('Cuprum'), local('CuprumRegular'), url('/c/cuprum-webfont.eot?#iefix') format('embedded-opentype'),
                #url('/c/cuprum-webfont.woff') format('woff'),
                #url('/c/cuprum-webfont.ttf') format('truetype'),
                #url('/c/cuprum-webfont.svg#CuprumRegular') format('svg');
            #font-weight: normal;
            #font-style: normal;

        #}
        #"""
        #res += """
        #@font-face {
            #font-family: 'PTSansNarrowRegular';
            #src: url('/c/psn-webfont.eot');
            #src: local('PT Sans Narrow'), local('PTSans-Narrow'),  url('/c/psn-webfont.eot?#iefix') format('embedded-opentype'),
                #url('/c/psn-webfont.woff') format('woff'),
                #url('/c/psn-webfont.ttf') format('truetype'),
                #url('/c/psn-webfont.svg#PTSansNarrowRegular') format('svg');
            #font-weight: normal;
            #font-style: normal;
        #}

        #"""
        res += """@font-face{
            font-family: 'cuprum';
            src: url('css-fonts/cuprum.eot');
            src: local('Cuprum'),
            url('css-fonts/cuprum.woff') format('woff'),
            url('css-fonts/cuprum.ttf') format("truetype"),
            url("css-fonts/cuprum.svg#CuprumRegular") format("svg");
            font-style: normal;
            font-weight: normal;
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
            font-family: cuprum, sans-serif;
            vertical-align: baseline;
            outline: none;
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
        res += """.m-hidden{display:none;}"""
        res += """.m-visible{display:block;}"""
        
        res += """.m-link{
            cursor:pointer;
        }"""
        res += """.m-link:hover{
            cursor:pointer;
        }"""

        res += """.m-button{
            display:inline-block;
            cursor:pointer;
            text-decoration:none;
        }"""

        res += """.m-button:hover{
            cursor:pointer;
            text-decoration:none;
        }"""
        
        return res
        
    def body(self):
        res = """body{
            font-family:Cuprum,Georgia,Serif;
        }"""

        #if(Use.css3_grad):
        #res += "body{%s}"%(
            #generate.util.lgt('#ffffff', '#ced0cd', '#e4e6e3'),
        #)
        #if(Use.bgi_grad):
        res += "body{%s}"%(
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
                background-color: white;
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

    def s_signup(self):
        res = ""

        res += """.e-captcha{
            cursor: pointer;
        }
        """

        res += """.s-signup{
            padding-left: 22px;
            background-color: white;
        }
        """
        res += """.b-csif-c{
            display: inline-block;
            overflow: hidden;
        }
        """

        res += """.b-csif-c.m-2{
            margin: 0px 0px 0px 50px;
        }
        """
        
        res += """.b-csif{
            height: 29px;
            width: 400px;
            overflow: hidden;
            position: relative;
            margin: 10px 0px 10px 0px;
            
            
            -moz-border-radius: 100px;
            -webkit-border-radius: 100px;
            -khtml-border-radius: 100px;
            border-radius: 20px;
            
            
        }
        """

        if(Use.css3_grad):
            res += """.b-csif{%s
                border: 0px inset #ccc;
                border-top: 1px inset #ccc;
                border-bottom: 1px inset #ccc;
            }"""%(
                generate.util.lgt('#f5f5f2', '#cfd1cf', '#cfd1cf')
            )
        if(Use.bgi_grad):
            res += ".b-csif{%s}"%(
                "background-image: url(%s);"%(
                    generate.util.tobase64("css-images/b-csif.png")
                )
            )

        res += """.e-csif-fl{
            display: block;
            float: left;

            min-width:20px;
            max-width:400px;
            font-size: 15px;
            height: 15px;

            padding: 8px 10px 8px 10px;


        }
        """

        if(Use.css3_grad):
            res += """.e-csif-fl{
                border-left: 1px inset #ccc ;
                -moz-border-radius: 100px 0px 0px 100px;
                -webkit-border-radius: 100px 0px 0px 100px;
                -khtml-border-radius: 100px 0px 0px 100px;
                border-radius: 100px 0px 0px 100px;
            }"""
        if(Use.bgi_grad):
            res += ".e-csif-fl{%s}"%(
                "background: url(%s)  no-repeat;"%(
                    generate.util.tobase64("css-images/e-csif-fl.png")
                )
            )
            
        #res += """.b-csif{
        #    background-position: 10px; 20px;
        #}
        #"""

        res += """.e-csif-fi{
            position: absolute;
            z-index: 0;
            right: 15px;
            top: 0px;
            width: 230px;
            font-size: 15px;

            border: none;

            height: 23px;
            padding: 5px 10px 5px 10px;
            background-color: white;
            outline: none;
        }
        """

        if(Use.bgi_grad):
            res += ".e-csif-fi{%s}"%(
                "background: white url(%s);"%(
                    generate.util.tobase64("css-images/e-csif-fi.png")
                )
            )



        res += """.e-csif-fm{
            position: relative;
            z-index: 1;
            display: block;
            background: white;
            float: right;

            width: 28px;
            height: 32px;

        }
        """

        if(Use.css3_grad):
            res += """.e-csif-fm{
                border-right: 1px inset #ccc ;
                -moz-border-radius: 0px 100px 100px 0px ;
                -webkit-border-radius: 0px 100px 100px 0px ;
                -khtml-border-radius: 0px 100px 100px 0px ;
                border-radius: 0px 100px 100px 0px ;
            }"""

        if(Use.bgi_grad):
            res += ".e-csif-fm{%s}"%(
                "background: url(%s) right top no-repeat;"%(
                    generate.util.tobase64("css-images/e-csif-fm.png")
                )
            )


        res += """.e-csif-w, .e-csif-r{
            position: relative;
            vertical-align: middle;
            top: 4px;
            z-index: 2;
            width:  21px;
            height: 20px;
            cursor: pointer;
        }
        """
        
        res += ".e-csif-w{%s}"%(
                "background: url(%s) no-repeat;"%(
                    generate.util.tobase64("css-images/e-csif-w.png")
                )
            )

        res += ".e-csif-r{%s}"%(
                "background: url(%s) no-repeat;"%(
                    generate.util.tobase64("css-images/e-csif-r.png")
                )
            )

        return res;
        
    def s_header(self):
        b_thehead_h = 81;
        b_thehead_caption_h = 17;

        def _b_thehead():
            res = """.b-thehead{
                display: block;
                height: %spx;
            }"""%(b_thehead_h);

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
                    font-size: 13px;
                    line-height: 16px;
                    text-align: right;
                    height: %spx;
                }
            """%(b_thehead_caption_h);
            
            res += """.b-thehead-caption {
                    background-color: black;
                    color: white;
                }
            """
            res += """.b-thehead-logo{
                    position: relative;
                    top:10px;
                    left: %(header_loffset)s;
                }
            """%(d(Metrics))
            
            res += """.b-thehead-caption {
                    padding-right: 10px;
                }
            """
            res += """.s-signin{
                    display: block;
                    /*background-color: red;*/
                    float: right;
                    text-align:right;
                    position: relative;
                    top: -60px;
                    width: 420px;
                    height: 30px;
                    vertical-align: middle;
                    margin-right: 10px;
                }
            """
            res += """.s-sfm-input{
                    font-size: 12px;
                    border: 1px inset #ccc;
                    outline: none;
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
                    margin-right: 13px;
                    font-size: 12px;
                    line-height: 16px;
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
                    padding: 0px 0px 0px 2px;
                    margin: 0px 0px 0px 0px;
                    tex-align: left;
                    width: 14px;
                    font-size: 12px;
                    font-family: Georgia, 'Times New Roman', Times, serif;
                    background-color: transparent;
                    border: none;
                }
            """
            return res;
            
        res = "";
        res = """.s-header{
            height: %spx;
            border-bottom: solid 4px #0898cd;
            overflow: hidden;
        }
        """%(b_thehead_h + b_thehead_caption_h);
        
        res += _b_thehead()
        res += _b_thehead_caption()
        return res;

    def s_roller(self):
        res = ""
        res += """.s-roller{
                height: 344px;
                width: %(gwidth)s;
                overflow:hidden;
            }
        """%(d(Metrics))
        
        res += """.b-roller{
                padding-top: 15px;
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
            margin-left: 67px;
        }
        """

        res +=""".b-rf-head{
            color: #259a3f;
            font-size: 55px;
            font-weight: normal;
        }
        """

        res +=""".b-rf-caption{
            color: white;
            font-size: 35px;
            line-height: 65px;
            font-weight: normal;
        }
        """
        
        res +=""".b-rf-content{
            margin: 0px 0px 0px 0px;
            color: white;
        }
        """

        res +=""".b-rf-act{
            
        }
        """
        
        res +=""".b-rfa-bat{
            text-align:center;
            color: white;
            margin-top: 10px;
            padding-top: 7px;
            font-size: 18px;
            height:  26px;
            width: 202px;

            background-color: #098fc1;
            -moz-border-radius: 5px;
            -webkit-border-radius: 5px;
            -khtml-border-radius: 5px;
            border-radius: 5px;
        }
        """

        if(Use.bgi_grad):
            res += ".b-rfa-bat{%s}"%(
                "background: url(%s) no-repeat;"%(
                    generate.util.tobase64("css-images/b-rfa-bat.png")
                )
            )

        res += """.b-roller-nav{
            text-align: center;
            clear: both;
        }
        """
        
        res += """.b-rn-ul{
            margin: auto;
            width: 90px;
        }
        """
        
        res += """.e-rn-ul{
            float: left;
        }
        """

        # display: inline-block;
        
        res += """.e-rn-but{
            top: 3px;
            margin: 0px 5px 10px 0px;
            width: 20px;
            height: 20px;
            display: block;
            -moz-border-radius: 20px;
            -webkit-border-radius: 20px;
            -khtml-border-radius: 20px;
            border-radius: 20px;
            background-color: white;
            outline: none;
        }
        """

        if(Use.bgi_grad):
            res += """.e-rn-but{
                    %s
                    width: 22px;
                    height: 22px;
                }"""%(
                "background: url(%s) no-repeat;"%(
                    generate.util.tobase64("css-images/e-rn-but.png")
                )
            )
            res += ".e-rn-but:hover, .e-rn-but:active, .e-rn-but.cur{%s}"%(
                "background: url(%s) no-repeat;"%(
                    generate.util.tobase64("css-images/e-rn-but.cur.png")
                )
            )
        if(Use.css3_grad):
            res += """.e-rn-but{
                border: inset 1px #cccccc;
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
            margin: 0px 0px 10px 0px;
            font-size: 14px;
            line-height: 16px;
        }
        """

        res +=""".b-rf-picture{
            float: right;
            position: relative;
            right:  100px;
            top:    -3px;
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
                overflow: hidden;
            }
            """
            if(Use.css3_grad):
                res += ".b-news-header{%s}"%(
                    generate.util.lgt('#0796ea', '#09b8e6', '#0796ea')
                )
            if(Use.bgi_grad):
                res += ".b-news-header{%s}"%(
                    "background: #0796ea url(%s);"%(
                        generate.util.tobase64("css-images/b-news-header.png")
                    )
                )
            return res

        top_offset = "10px";
        
        def _e_news_head():
            return """.e-news-head{
                display: block;
                width: 800px;
                font-size: 30px;
                font-variant: normal;
                font-width: normal;
                font-weight: normal;
                color: white;
                float:left;
                margin: 10px 0px 0px %(header_loffset)s;
            }
            """%(d(Metrics))

        def _b_news_doc():
            res = """.b-news-doc {
                display: block;
                position: relative;
                float:right;
                font-size: 14px;
                line-height: 16px;
                padding: 8px 23px 10px 33px;
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
                height: 368px;
                overflow: hidden;
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
            """
            
            res += """.s-news-all{
                padding: 10px 0px 0px 0px;
                display: block;
                float:right;
                text-align:right;
                width: 160px;
                margin: 0px 23px 0px 0px
            }
            """

            res +=""".b-na-bat{
                text-align:center;
                color: white;
                padding-top: 4px;
                height:  22px;
                width: 134px;
                font-size: 18px;
                -moz-border-radius: 5px;
                -webkit-border-radius: 5px;
                -khtml-border-radius: 5px;
                border-radius: 5px;
            }
            """

            if(Use.css3_grad):
                res += ".b-na-bat{%s%s}"%(
                    "border: solid 1px #166436;",
                    generate.util.lgt('#26963f', '#176636', '#176636')
                )

            if(Use.bgi_grad):
                res += ".b-na-bat{%s}"%(
                    "background: url(%s) no-repeat;"%(
                        generate.util.tobase64("css-images/b-na-bat.png")
                    )
                )

            res += """.s-news-list-item{
                padding: 2px;
                margin: 2px;
                display: block;
                float:left;
                width: 286px
            }
            """

            res += """.b-nlic{
                margin: 4px 0px 0px 0px;
                height: 75px;
                font-size: 18px;
            }
            """
            
            res += """.b-nlil{
                display: block;
                color:black;
                text-decoration: none;
            }
            """
            
            res += """.b-nli{
                height: 292px;
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
                background-color: white;
                overflow: hidden;
            }
        """%(d(Metrics))

        res += """.b-a{
            padding: 20px 23px 20px %(header_loffset)s;
        }
        """%(d(Metrics))

        res += """.b-ah{
                font-size: 30px;
                color: #0191d3;
            }
        """

        res += """.b-ac{
        font-size: 15px;
            line-height: 20px;
        }
        """

        res += """.b-ac-p{
            margin-top: 20px;
        }
        """

        res +=""".b-ac-ul{
            margin: 20px 0px 0px 15px;
            list-style: outside url(%s);
        }
        """%(
            generate.util.tobase64("css-images/b-ac-ul.png")
        )


        res += """.e-ac-ul{
            margin: 20px 0px 0px 15px;
        }
        """

        return res;


    def s_pers(self):
        s_pers = """
        .s-pers{
            position: relative;
            top: -4zpx;
            background-color: #cccccc;
        }

        .b-fpb{
            display:none;
        }

        .b-spb, .b-fpb{
            top: 45%;
            left: 45%;
            position: absolute;
            text-align: center;
        }
        """
        
        ria_bg = """
        .ria-bg{
            top: -2px;
            width: %(gwidth)s;
            height: %(rheight)s;
            position: relative;
            background-color: #cccccc;
        }
        """%(d(Metrics))
        
        ria_fg = """
        .ria-fg{
            width: %(gwidth)s;
            height: %(rheight)s;
            top: 0px;
            left: 0px;
            position: absolute;
        }
        """%(d(Metrics))
        
        __ria = """
        #ria{
            position: absolute;
            width: %(gwidth)s;
            height: %(rheight)s;
        }
        """%(d(Metrics))
        
        __ria_ = """
        #ria *{
            font-size: 12px;
            font-family: Tahoma ,sans-serif;
        }
        """
        
        res = s_pers + ria_bg + ria_fg + __ria + __ria_
        return res

        
    def s_footer(self):
        res = ""
        res = """.s-footer{
            clear: both;
            height: 55px;
            padding-top: 20px;
            text-align: center;
            color: white;
        }
        """
        if(Use.css3_grad):
            res += ".s-footer{%s}"%(
                generate.util.lgt('#0192d5', '#02618d', '#02618d')
            )
        if(Use.bgi_grad):
            res += ".s-footer{%s}"%(
                "background: #02618d url(%s);"%(
                    generate.util.tobase64("css-images/s-footer.png")
                )
            )



        return res;

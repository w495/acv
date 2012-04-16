<?xml version="1.0" encoding="utf-8"?>
<!--
    ЛИЧНЫЙ КАБИНЕТ РЕКЛАМОДАТЕЛЯ
-->

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />
<xsl:include href="includes/signup-form.xsl" /> 

<xsl:template name="head-scripts-pers">
    <script src="/j/ria.js" type="text/javascript" >
        <xsl:text><![CDATA[ ]]></xsl:text>
    </script>
    <style>
        #upload_progress_bar{
            width:              100%;
            height:             100%;
            z-index:            65533;
            background-color:   black;
            -moz-opacity:       0.7;
            -khtml-opacity:     0.7;
            opacity:            0.7;
            position:           absolute;
            text-align:         center;
            vertical-align:     middle;
            display:            none;
        }
        .b-spb,.b-upb{
            display:                block;
            color:                  black;
            background-color:       white;
            width:                  200px;
            height:                 20px;
            padding:                10px;
            -webkit-border-radius:  10px;   /*Ch*/
            -moz-border-radius:     10px;   /*FF*/
            border-radius:          10px;   /*IE*/
        }
    </style>
</xsl:template>
 

<xsl:template name="s-header-signin"> 
	<xsl:call-template name="s-logout-link" />
</xsl:template>

<xsl:template name="s-main-pers">
    <section class="s-pers">
        <xsl:call-template name="s-pers" />
    </section>
</xsl:template>

<xsl:template name="s-pers">
    <div class="b-ria">
        <div class="ria-bg">
            <div id="global_progress_bar">
                <div class="b-spb">
                    <img src="/i/ldg.gif" class="site-uppload-image" />
                    <span><xsl:text>&nbsp;Загрузка сайта</xsl:text></span>
                </div>
            </div>
            <div id="upload_progress_bar">
                <div class="b-spb">
                    <img src="/i/ldg.gif" class="file-uppload-image" />
                    <span><xsl:text>&nbsp;Загрузка файла</xsl:text></span>
                </div>
                <!--
                <div class="b-spb">
                    <img src="/i/ldg.gif" class="file-uppload-image" />
                    <span><xsl:text>&nbsp;Загрузка файла</xsl:text></span>
                </div>
                -->
            </div>
        </div>
        <div class="ria-fg">
            <div id="ria">
                <xsl:text><![CDATA[ ]]></xsl:text>
            </div>
        </div>
    </div>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

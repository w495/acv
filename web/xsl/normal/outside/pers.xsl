<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />
<xsl:include href="includes/signup-form.xsl" />


<xsl:template name="s-title-1">
    <xsl:text>регистрация</xsl:text>
</xsl:template>

<xsl:template name="link-css-pers">
    <style type="text/css">
        <xsl:text>
        <![CDATA[
        .ria-cnt-cnt{
            width: 1000px;
            height: 700px;
            position: relative;
            background-color: #d7d7d7;
        }

        .ria-cnt{
            width: 1000px;
            height: 700px;
            top: 0px;
            left: 0px;
            position: absolute;
            background-color: #d7d7d7;
        }

        #ria{
            position: absolute;
            width: 1000px;
            height: 700px;
        }

        #ria *{
            font-size: 12px;
            font-family: Tahoma ,sans-serif;
        }
        ]]>
        </xsl:text>
    </style>
</xsl:template>

<xsl:template name="head-scripts-pers">
    <script type="text/javascript">
        <xsl:text>
        <![CDATA[
            qxsettings = new Object();
            qxsettings["qx.enableApplicationLayout"] = false;
        ]]>
        </xsl:text>
   </script>
    <script src="/j/ria.js" type="text/javascript" >
        <xsl:text><![CDATA[ ]]></xsl:text>
    </script>
</xsl:template>

<xsl:template name="s-main-pers">
    <section class="s-pers">
        <xsl:call-template name="s-pers" />
    </section>
</xsl:template>

<xsl:template name="s-pers">
    <div class="ria-cnt-cnt">
        <div id="global_progress_bar">
            <div  id="upload_progress_back">&nbsp;</div>
            <div class="inner-box file-uppload-box">
                <img src="resource/bsk/img/ff-pb.gif" class="file-uppload-image" />
                <span><xsl:text>&nbsp;Загрузка сайта</xsl:text></span>
            </div>
        </div>
        <div id="upload_progress_bar">
            <div  id="upload_progress_back">&nbsp;</div>
            <div class="inner-box file-uppload-box">
                <img src="resource/bsk/img/ff-pb.gif" class="file-uppload-image" />
                <span><xsl:text>&nbsp;Загрузка файла</xsl:text></span>
            </div>
        </div>
    </div>
    <div class="ria-cnt">
        <div id="ria">
            <xsl:text><![CDATA[ ]]></xsl:text>
        </div>
    </div>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

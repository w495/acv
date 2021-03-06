<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:include href="includes/signup-form.xsl" />

<xsl:template name="s-title-1">
    <xsl:text>регистрация</xsl:text>
</xsl:template>

<xsl:template name="s-main-2">
    <!--
        Основное содержимое страницы
    -->
    <hgroup>
        <h1>Регистрация</h1>
    </hgroup>

    <div class="signup-form">
        <xsl:call-template name="signup-form">
            <xsl:with-param name="Action" select="concat('/signup/post', /data/meta/self-retpath)" />
            <xsl:with-param name="Method" select="'POST'"/>
            <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
            <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
        </xsl:call-template>
    </div>

</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

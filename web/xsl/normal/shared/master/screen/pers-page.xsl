<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="base.xsl"/>

<xsl:template name="s-title-base">
    <xsl:text>Личный кабинет рекламодателя</xsl:text>
</xsl:template>

<xsl:template name="link-css-base">
    <xsl:call-template name="link-css-pers"/>
</xsl:template>

<xsl:template name="link-css-pers">
</xsl:template>

<xsl:template name="head-scripts-base">
    <xsl:call-template name="head-scripts-pers"/>
</xsl:template>

<xsl:template name="head-scripts-pers">
</xsl:template>


<xsl:template name="s-main-base">
    <xsl:call-template name="s-main-pers"/>
</xsl:template>

<xsl:template name="s-main-pers">
</xsl:template>

<!--
    ###########################################################################
    ###
    ###########################################################################
-->

</xsl:stylesheet>


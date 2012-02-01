<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="base.xsl"/>

<xsl:template name="s-main-1">
    <xsl:call-template name="s-main-2" />
</xsl:template>

<xsl:template name="s-main-2">
    <xsl:text>шаблон</xsl:text>
</xsl:template>


</xsl:stylesheet>

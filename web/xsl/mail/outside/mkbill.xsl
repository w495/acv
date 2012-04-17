<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/chstate.xsl"/>

<xsl:template name="s-main-chstate">
    <xsl:text>Вам выстален счет.</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Для облаты счет вам необходимо перейти по ссылке:</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:value-of select="/data/meta/sys-dns" />
    <xsl:text>/pay?id=</xsl:text>
    <xsl:value-of select="/data/video/id" />

    <xsl:text>&#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
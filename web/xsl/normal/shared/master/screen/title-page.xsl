<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp   "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
    <!ENTITY bull   "&#8226;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="base.xsl"/>

<xsl:template name="s-title-base">
    <xsl:text>главная</xsl:text>
</xsl:template>


<xsl:template name="s-main-base">
    <section class="s-roller">
        <xsl:call-template name="s-roller" />
    </section>
    <section class="s-news">
        <xsl:call-template name="s-news" />
    </section>
    <section class="s-about">
        <xsl:call-template name="s-about" />
    </section>
</xsl:template>

<xsl:template name="s-roller">
    <xsl:text>&nbsp;</xsl:text>
</xsl:template>

<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>Обращение к рекамодателям</xsl:text>
        </h1>
        <a class="b-news-doc" href="" >
            <xsl:text>Документ</xsl:text>
        </a>
    </header>
    <ul class="s-news-list">
        <xsl:call-template name="s-news-list" />
    </ul>
</xsl:template>

<xsl:template name="s-news-list">
<!-- <xsl:for> -->
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item" />
        </li>
<!-- </xsl:for> -->
</xsl:template>

<xsl:template name="s-news-list-item">
    <a>
        <figure>
            <img />
            <figcaption>
                <xsl:text>Документ</xsl:text>
            </figcaption>
        </figure>
    </a>
</xsl:template>

<xsl:template name="s-about">
    <xsl:text>&nbsp;</xsl:text>
</xsl:template>

</xsl:stylesheet>

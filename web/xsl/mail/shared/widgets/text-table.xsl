<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY tab "&#9;">
    <!ENTITY long_wc "    ">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="text-table-line">
    <xsl:param name="VF" select="''" />
    <xsl:param name="Name" />
    <xsl:param name="Value" />
    <xsl:text>&long_wc;&long_wc;</xsl:text>
    <xsl:value-of select="$Name" />
    <xsl:text>: </xsl:text>
    <xsl:value-of select="$VF" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="$Value" />
    <xsl:text>;&#xa;</xsl:text>
</xsl:template>

<xsl:template name="customer-text-table">
    <xsl:param name="Title" select="'Данные о пользователе'" />
    <xsl:param name="Customer" select="/data/сustomer" />
    <xsl:value-of select="$Title" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'номер'" />
        <xsl:with-param name="Value" select="$Customer/id" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'логин'" />
        <xsl:with-param name="Value" select="$Customer/login" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'email'" />
        <xsl:with-param name="Value" select="$Customer/email" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'имя'" />
        <xsl:with-param name="Value" select="$Customer/firstname" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'фамилия'" />
        <xsl:with-param name="Value" select="$Customer/lastname" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'отчество'" />
        <xsl:with-param name="Value" select="$Customer/patronimic" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'организация'" />
        <xsl:with-param name="Value" select="$Customer/organization" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'должность'" />
        <xsl:with-param name="Value" select="$Customer/position" />
    </xsl:call-template>
</xsl:template>

<xsl:template name="acv-video-fields-text-table">
    <xsl:param name="Title" select="'Данные о видео-кампании '" />
    <xsl:param name="Acv-video" select="/data/acv-video" />
    <xsl:param name="STF" select="'&long_wc;'" />
    <xsl:value-of select="$Title" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:value-of select="$STF" />
    <xsl:text>Общая информация &#xa;</xsl:text>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'номер'" />
        <xsl:with-param name="Value" select="$Acv-video/id" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'имя'" />
        <xsl:with-param name="Value" select="$Acv-video/name" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'дата начала'" />
        <xsl:with-param name="Value">
            <xsl:call-template name="erlangFormatDate">
                <xsl:with-param name="DateTime" select="$Acv-video/datestart"/>
            </xsl:call-template>
        </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'дата конца'" />
        <xsl:with-param name="Value">
            <xsl:call-template name="erlangFormatDate">
                <xsl:with-param name="DateTime" select="$Acv-video/datestop"/>
            </xsl:call-template>
        </xsl:with-param>
    </xsl:call-template>
    <xsl:value-of select="$STF" />
    <xsl:text>Ссылки &#xa;</xsl:text>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'ссылка'" />
        <xsl:with-param name="Value" select="$Acv-video/url" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'текст ссылки'" />
        <xsl:with-param name="Value" select="$Acv-video/link_title" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'файл'" />
        <xsl:with-param name="Value" select="concat(concat(/data/meta/sys-dns, '/'), $Acv-video/ref)" />
    </xsl:call-template>
    <xsl:value-of select="$STF" />
    <xsl:text>Отображение &#xa;</xsl:text>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'желаемое количество показов'" />
        <xsl:with-param name="Value" select="$Acv-video/wish" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'фактическое количество показов'" />
        <xsl:with-param name="Value" select="$Acv-video/shown" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'количество кликов'" />
        <xsl:with-param name="Value" select="$Acv-video/clicks" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'preroll'" />
        <xsl:with-param name="Value" select="$Acv-video/preroll" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'midroll'" />
        <xsl:with-param name="Value" select="$Acv-video/midroll" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'postroll'" />
        <xsl:with-param name="Value" select="$Acv-video/postroll" />
    </xsl:call-template>
    <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template name="acv-video-text-table">
    <xsl:param name="Title" select="'Данные о видео-кампании'" />
    <xsl:param name="Acv-video" select="/data/acv-video" />
    <xsl:call-template name="acv-video-fields-text-table">
        <xsl:with-param name="Title" select="$Title" />
        <xsl:with-param name="Acv-video" select="$Acv-video" />
    </xsl:call-template>
    <xsl:call-template name="customer-text-table">
        <xsl:with-param name="Title" select="'Данные о создателе кампании'" />
        <xsl:with-param name="Customer" select="$Acv-video/customer" />
    </xsl:call-template>
</xsl:template>

</xsl:stylesheet>

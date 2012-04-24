<?xml version="1.0" encoding="utf-8"?>
<!--
    Это основной шаблон для вывода на экран монитора.

    # НАПОМИНАНИЕ:

        ## ФОРМАТИРОВАНИЕ:
            * все текстовые элементы должны помещаться в тег <xsl:text>

        ## ИМЕНОВАНИЕ ШАБЛОНОВ:
            * если шаблон описан в теле документа, то
                название шаблона должно совпадать
                с классом содержащим этот шаблон;
            * если описанный шаблон необходим в дочернем шаблоне,
                то но при этом, в текущем шаблоне необходимо содержать
                некоторую информацию, то к шаблону в дочернем элементе
                нужно добавить постфикс с числовым выражением
                уговня вложенности. Нулевой уровень не указывается.
                    `s-foo' ~~> `s-foo-1' ~~> `s-foo-2'.

        ## ИМЕНОВАНИЕ КЛАССОВ:
            * классы блочных элементов должны начинаться на с префикса b.

        ## ЗАПРЕЩЕНО:
            * использование атрибутов id.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output
    method="text"
    encoding="utf-8"
/>

<xsl:template match="/" name="s-html">
    <!--
        Описание страницы
    -->
    <xsl:call-template name="s-body" />
    <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template name="s-head">
</xsl:template>

<xsl:template name="s-body">
    <!--
        Тело документа.
    -->
    <xsl:call-template name="s-header" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="s-main" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="s-footer" />
</xsl:template>

<xsl:template name="s-header">
    <xsl:text>Здравствуйте, </xsl:text>
    <xsl:value-of select="/data/meta/username" />
    <xsl:text>!</xsl:text>
    <xsl:text>&#xa;</xsl:text>
</xsl:template>


<xsl:template name="s-main">
    <!--
        Основное содержимое документа.
    -->
    <xsl:call-template name="s-main-root" />
</xsl:template>

<xsl:template name="s-main-root">
    <!--
        Основное содержимое документа.
        Первый уровенгь вложенности.
    -->
    <xsl:call-template name="s-main-base" />
</xsl:template>


<xsl:template name="s-main-base">
    <!--
        Основное содержимое документа.
        Второй уровенгь вложенности.

        <xsl:call-template name="s-main-concrete" />
    -->
</xsl:template>


<xsl:template name="s-footer">
    <!--
        Подол страницы
    -->
    <xsl:text>С уважением,</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>администрация Tvzavr.</xsl:text>
    <xsl:text>&#xa;&#xa;</xsl:text>
    <xsl:text>© 2011 OOO «ТиВиЗавр»</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Все права защищены.</xsl:text>
</xsl:template>


<xsl:template name="text-table-line">
    <xsl:param name="Name" />
    <xsl:param name="Value" />
    <xsl:text>    </xsl:text>
    <xsl:value-of select="$Name" />
    <xsl:text>:&#xa;    </xsl:text>
    <xsl:text>          </xsl:text>
    <xsl:text>          </xsl:text>
    <xsl:value-of select="$Value" />
    <xsl:text>;&#xa;</xsl:text>
</xsl:template>


<xsl:template name="customer-text-table">
    <xsl:param name="Title" select="'Данные о пользователе'" />
    <xsl:param name="Customer" select="/data/сustomer" />
    <xsl:value-of select="$Title" />
    <xsl:text>&#xa;&#xa;</xsl:text>
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
    <xsl:value-of select="$Title" />
    <xsl:text>&#xa;&#xa;</xsl:text>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'номер'" />
        <xsl:with-param name="Value" select="$Acv-video/id" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'имя'" />
        <xsl:with-param name="Value" select="$Acv-video/name" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'ссылка'" />
        <xsl:with-param name="Value" select="$Acv-video/url" />
    </xsl:call-template>
    <xsl:call-template name="text-table-line">
        <xsl:with-param name="Name" select="'файл'" />
        <xsl:with-param name="Value" select="concat(concat(/data/meta/sys-dns, '/'), $Acv-video/ref)" />
    </xsl:call-template>
</xsl:template>

<xsl:template name="acv-video-text-table">
    <xsl:param name="Title" select="'Данные о видео-кампании'" />
    <xsl:param name="Acv-video" select="/data/acv-video" />

    <xsl:call-template name="acv-video-fields-text-table">
        <xsl:with-param name="Title" select="$Title" />
        <xsl:with-param name="Acv-video" select="$Acv-video" />
    </xsl:call-template>
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="customer-text-table">
        <xsl:with-param name="Title" select="'Данные о создателе кампании'" />
        <xsl:with-param name="Customer" select="$Acv-video/customer" />
    </xsl:call-template>

</xsl:template>


</xsl:stylesheet>

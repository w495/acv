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


<!--
    Включаем сторонние статичные элементы.
    Они предельно конкретны и меняться от шаблона к шаблону не будут.
    Все include могут быть только в этом документе
-->


<!-- ====================================================================  -->

<xsl:template match="/" name="s-html">
    <!--
        Описание страницы
    -->
    <xsl:call-template name="s-head" />
    <xsl:text>&#xa;<!-- \n, чтобы файл не был в 1 строку --></xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="s-body" />
    <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template name="s-head">

</xsl:template>

<xsl:template name="s-title">
    <!--
        Заголовок страницы, вида
        {Заголовок}     ::= {надзаголовок}{разделитель}{подзаголовок}
        {надзаголовок}  ::= "название сайта"
        {разделитель}   ::= ": "
        {подзаголовок}  ::= "название раздела"
    -->
    <xsl:param name="Delim" select="': '" />
    <xsl:text>Система рекламы tvzavr</xsl:text>
    <xsl:value-of select="$Delim" />
    <xsl:call-template name="s-title-root"/>
</xsl:template>

<xsl:template name="s-title-root">
    <!--
        Подзаголовок страницы.
    -->
    <xsl:text>шаблон документа</xsl:text>
</xsl:template>

<xsl:template name="links">
    <!--
        Ссылки на ресурсы не относящиеся к html.
                icon
                shortcut icon
                apple-touch-icon
            тоже вряд ли будут меняться в рамках одного шаблона,
            Для единнообразия, мы решили положить их в этот шаблон.
    -->
    <link rel="icon" href="/favicon.ico" />
    <link rel="shortcut icon" href="/favicon.ico" />
    <link rel="apple-touch-icon" href="/favicon.png" />
    <xsl:call-template name="link-css" />
</xsl:template>

<xsl:template name="link-css">
    <!--
        Стили страницы - нулевой уровень
    -->
    <link rel="stylesheet" type="text/css" media="all" href="/c/base.css" />
    <xsl:call-template name="link-css-1" />
</xsl:template>

<xsl:template name="link-css-1">
    <!--
        Стили страницы - первый уровень
    -->
</xsl:template>

<xsl:template name="head-scripts">
    <!--
        Скрипты добавляемые вверху страницы.
        Рекомендовано это использовать, если сами скрипты
        изменяют начальный вид страницы или ее DOM (до загрузки).
        Например, modernizr.
        Для обычного использования НЕ РЕКОМЕНДОВАНЫ.
    -->
    <script src="/j/mm.js" type="text/javascript" >
        <xsl:text><![CDATA[ ]]></xsl:text>
    </script>
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
    <!--
        Основной заголовок сайта.
    -->
</xsl:template>

<xsl:template name="s-signin">
    <xsl:text><![CDATA[ ]]></xsl:text>
</xsl:template>

<xsl:template name="s-nav">
    <!--
        Навигация по внешней части сайта.
        Должна содержать ссылки на все разделы, благо их не много
    -->
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
    <xsl:text>© 2011 OOO «ТиВиЗавр»</xsl:text>
    <xsl:text>Все права защищены.</xsl:text>
</xsl:template>

<xsl:template name="foot-scripts">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <xsl:call-template name="foot-scripts-root" />
</xsl:template>


</xsl:stylesheet>

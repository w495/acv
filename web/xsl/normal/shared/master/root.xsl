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
    omit-xml-declaration="no"
    method="html"
    indent="yes"
    encoding="utf-8"
/>

<!--
    Включаем сторонние статичные элементы.
    Они предельно конкретны и меняться от шаблона к шаблону не будут.
    Все include могут быть только в этом документе
-->

<!-- Заголовок с DOCTYPE html и определениями для IE -->
<xsl:include href="../includes/_html5header.xsl" />

<!-- ====================================================================  -->

<xsl:template match="/" name="s-html">
    <!--
        Описание страницы
    -->
    <xsl:call-template name="html5header-min" />
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru" >
        <head xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
            <xsl:call-template name="s-head" />
        </head>
        <body xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
            <xsl:call-template name="s-body" />
        </body>
        <xsl:call-template name="foot-scripts" />
    </html>
</xsl:template>

<xsl:template name="s-head">
    <!--
        Голова страницы.
        Мы не выносили из страницы META,
            так как в рамках представления одной структуры
                и одной основы шаблоны оно вряд ли будет меняться.
    -->
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8" charset="UTF-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
    <meta name="author" content="ζAVρ λαβ" />
    <meta name="description" content="adv com system" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>
        <xsl:call-template name="s-title"/>
    </title>
    <xsl:call-template name="links" />
    <xsl:call-template name="head-scripts" />
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
    <xsl:call-template name="s-title-1"/>
</xsl:template>

<xsl:template name="s-title-1">
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
    <link rel="stylesheet" type="text/css" media="all" href="/static/base.css" />
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
        Например, modernizer.js.
        Для обычного использования НЕ РЕКОМЕНДОВАНЫ.
    -->
</xsl:template>

<xsl:template name="s-body">
    <!--
        Тело документа.
    -->
    <header class="s-header">
        <xsl:call-template name="s-header" />
    </header>
    <nav>
        <xsl:call-template name="s-nav" />
    </nav>
    <section class="s-main">
        <xsl:call-template name="s-main" />
    </section>
    <footer class="s-footer">
        <xsl:call-template name="s-footer" />
    </footer>
</xsl:template>

<xsl:template name="s-header">
    <!--
        Основной заголовок сайта.
    -->
    <hgroup class="b-header-group">
        <h1><xsl:text>&#964;&#965;&#950;&#945;&#957;&#961; &#8704;&#916;V C&#937;M</xsl:text></h1>
        <h2 class="b-header-caption"><xsl:text>система рекламы &#964;&#965;&#950;&#945;&#957;&#961;</xsl:text></h2>
    </hgroup>
</xsl:template>


<xsl:template name="s-nav">
    <!--
        Навигация по внешней части сайта.
        Должна содержать ссылки на все разделы, благо их не много
    -->
    <ul>
        <li><a href="/index"><xsl:text>Головная</xsl:text></a></li>
        <li><a href="/about"><xsl:text>О проекте</xsl:text></a></li>
        <li><a href="/signin"><xsl:text>Войти</xsl:text></a></li>
        <li><a href="/signup"><xsl:text>Регистрация</xsl:text></a></li>
    </ul>
</xsl:template>


<xsl:template name="s-main">
    <!--
        Основное содержимое документа.
    -->
    <xsl:call-template name="s-main-1" />
</xsl:template>

<xsl:template name="s-main-1">
    <!--
        Основное содержимое документа.
        Первый уровенгь вложенности.
    -->
    <xsl:call-template name="s-main-2" />
</xsl:template>


<xsl:template name="s-main-2">
    <!--
        Основное содержимое документа.
        Второй уровенгь вложенности.

        <xsl:call-template name="s-main-3" />
    -->
</xsl:template>


<xsl:template name="s-footer">
    <!--
        Подол страницы
    -->
    <xsl:text>© ζAVρ λαβ</xsl:text>
</xsl:template>

<xsl:template name="foot-scripts">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
</xsl:template>


</xsl:stylesheet>

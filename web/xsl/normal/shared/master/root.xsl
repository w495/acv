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
            <xsl:text>&#xa;<!-- \n, чтобы файл не был в 1 строку --></xsl:text>
        </head>
        <body xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
            <xsl:text>&#xa;</xsl:text>
            <xsl:call-template name="s-body" />
            <xsl:text>&#xa;</xsl:text>
            <xsl:call-template name="foot-scripts" />
        </body>
    </html>
    <xsl:text>&#xa;</xsl:text>
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
    <header class="s-header">
        <xsl:call-template name="s-header" />
    </header>
    <xsl:text>&#xa;</xsl:text>
    <section class="s-main">
        <xsl:call-template name="s-main" />
    </section>
    <xsl:text>&#xa;</xsl:text>
    <footer class="s-footer">
        <xsl:call-template name="s-footer" />
    </footer>
</xsl:template>

<xsl:template name="s-header">
    <!--
        Основной заголовок сайта.
    -->
    <hgroup class="b-header-group">
        <h2 class="b-thehead-caption"><xsl:text>портал для рекламодателей</xsl:text></h2>
        <h1 class="b-thehead">
            <img class="b-thehead-logo" src="/i/logo.png" alt="tvzavr" title="tvzavr" />
        </h1>
    </hgroup>
    <div class="s-signin">
        <xsl:call-template name="s-signin" />
    </div>
</xsl:template>

<xsl:template name="s-signin">
    <xsl:text><![CDATA[ ]]></xsl:text>
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
    <div class="b-footer">
        <p>
            <xsl:text>© 2011 OOO «ТиВиЗавр»</xsl:text>
        </p>
        <p>
            <xsl:text>Все права защищены.</xsl:text>
        </p>
    </div>
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

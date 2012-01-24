<?xml version="1.0" encoding="utf-8"?>
<!--
    Это основной шаблон для вывода на экран монитора.
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

<!-- Верхнее меню -->
<xsl:include href="../includes/menu-container.xsl" />

<!-- Подол страницы -->
<xsl:include href="../includes/footer-container.xsl" />

<!-- Виджет игры -->
<xsl:include href="../includes/game-container.xsl" />

<!-- Виджет нижнего баннера -->
<xsl:include href="../includes/bottom-banner-container.xsl" />

<!-- Список видов транспорта -->
<xsl:include href="../includes/transport-container.xsl" />

<!-- Авторизация -->
<xsl:include href="../includes/athorise-container.xsl" />

<!-- ====================================================================  -->
<!-- <HTML></HTML> -->
<!-- ====================================================================  -->

<xsl:template match="/" name="root">
<xsl:call-template name="html5header-min" /> 
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru" >
    <head xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
        <xsl:call-template name="head" />
    </head>
    <body xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
        <xsl:call-template name="body" />
    </body>
    <xsl:call-template name="foot-scripts" />
</html>
</xsl:template>

<!-- ====================================================================  -->
<!-- ГОЛОВА -->
<!-- ====================================================================  -->

<xsl:template name="head">
    <xsl:call-template name="meta" />    
    <xsl:call-template name="viewport" />
    <title>
        <xsl:call-template name="title" />
    </title>
    
    <xsl:call-template name="links" />
    <xsl:call-template name="head-scripts" />
</xsl:template>

<xsl:template name="head-scripts">
</xsl:template>

<!-- МETA -->
<!-- ====================================================================  -->
<xsl:template name="meta">
    <xsl:call-template name="meta-http-equiv" />
    <xsl:call-template name="meta-oth" />
</xsl:template>

<xsl:template name="meta-http-equiv">
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8" charset="UTF-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />    
</xsl:template>

<xsl:template name="meta-oth">
    <meta name="author" content="DiSiSta MaTraSy" />
    <meta name="description" content="distributed simple statistical machine translation system" />
</xsl:template>

<!-- viewport -->
<!-- ====================================================================  -->
<xsl:template name="viewport">
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
</xsl:template>

<!-- links -->
<!-- ====================================================================  -->
<xsl:template name="links">
    <link rel="icon" href="/favicon.ico" />
    <link rel="shortcut icon" href="/favicon.ico" />

    <!-- <link rel="apple-touch-icon" href="" />-->
    <xsl:call-template name="link-css" />
</xsl:template>

<xsl:template name="link-css">
    <link rel="stylesheet" type="text/css" media="all" href="/static/base.css" />
</xsl:template>

<!-- ====================================================================  -->
<!-- ТЕЛО -->
<!-- ====================================================================  -->

<xsl:template name="body">
    <header class="s-header">
        <xsl:call-template name="s-header" />
    </header>
    <section class="s-main">
        <xsl:call-template name="s-main" />
    </section>
    <footer class="s-footer">
        <xsl:call-template name="s-footer" />
    </footer>
</xsl:template>

<xsl:template name="s-header">
    <hgroup class="b-header-group">
        <h1>&#964;&#965;&#950;&#945;&#957;&#961; &#8704;&#916;V C&#937;M</h1>
        <h2 class="b-header-caption">система рекламы &#964;&#965;&#950;&#945;&#957;&#961;</h2>
    </hgroup>
</xsl:template>

<xsl:template name="s-main">
    <xsl:comment>МЕНЮ</xsl:comment>
    <ul>
        <li><a href="/index">Головная</a></li>
        <li><a href="/about">О проекте</a></li>
        <li><a href="/login">Войти</a></li>
        <li><a href="/signup">Регистрация</a></li>
    </ul>
    <xsl:comment>ОПИСАНИЕ</xsl:comment>
    <article>
        <p>
            Мы работаем с правообладателями на основе лицензионных договоров.
            На нашем сайте исключено использование нелегального контента,
            а также видео откровенного эротического характера.
        </p>
        <p>
            Ведение статистики просмотров рекламных продуктов позволяет
            устранить обезличенность рекламы, свойственную рекламе
            телевизионной, и сделать каждый ее показ адресным.
            Мы можем ограничить количество показов рекламного креатива
            для одного пользователя и донести информацию
            до максимального количества потенциальных потребителей,
            при этом экономя рекламный бюджет
            и увеличивая эффективность рекламной кампании.
        </p>
        <p>
            Таргетирование по пользователям можно проводить
            с учетом их интересов, возраста, пола и других параметров,
            а также учитывая их местонахождение – геотаргетинг.
            Мы предоставляем возможность самого точного геотаргетирования
            вплоть до района и улицы пользователя. Мы не просто донесем
            информацию о вашем продукте или услуге, но и сделаем так,
            чтобы ее узнала именно ваша целевая аудитория.
        </p>
        <p>
            Вы получите доступ к личному кабинету и сможете видеть:
            где и когда ваша реклама размещалась, кто и когда ее просмотрел
            или же кликнул на то или иное рекламное объявление.
            Невозможность заблокировать или прокрутить ролики гарантирует
            показ рекламы пользователям, а возможность вставки
            гиперссылок – отследить ее эффективность по сделанным кликам.
        </p>
        <p>
            Кроме того, возможности информационных технологий позволяют выйти
            за рамки стандартных форм подачи рекламы. Рекламные ролики теперь
            можно делать в игровой развлекательной форме,
            с возможностью вовлечения пользователя в ее показ.
            Можно показывать пользователям многосерийные видеоролики
            со связанным сюжетом, которые будут заинтриговывать пользователя,
            а в самом конце предлагать ему продолжение в следующей серии,
            и таким образом, вызывать интерес к будущим просмотрам.
            Или почему бы не использовать эмоциональное состояние пользователя
            и не рекламировать, например, туристические поездки
            в фильмах «Индиана Джонс» или «Мумия»,
            а рекламу нижнего белья ̶ вставлять именно в романтические сцены?
        </p>
        <p>
            Идей может быть много, они ограничиваются только фантазией,
            а реализовать все это вы можете через платформу
            размещения рекламы TVzavr.
        </p>
    </article>

</xsl:template>


<xsl:template name="s-footer">
</xsl:template>

<!-- ====================================================================  -->
<!-- ХВОСТ -->
<!-- ====================================================================  -->

<xsl:template name="foot-scripts">

</xsl:template>


</xsl:stylesheet>

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
	<xsl:text>Главная</xsl:text>
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
    <div class="b-roller">
        <ul class="b-roller-frames">
            <li class="s-roller-frame">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'pre-roll'"/>
                    <xsl:with-param name="Head" select="'Видеореклама'"/>
                    <xsl:with-param name="Subhead" select="'PRE-ROLL'"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-1.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li>
            <li class="s-roller-frame m-hidden">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'mid-roll'"/>
                    <xsl:with-param name="Head" select="'Видеореклама2'"/>
                    <xsl:with-param name="Subhead" select="'MID-ROLL'"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-1.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li>
            <li class="s-roller-frame m-hidden">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'post-roll'"/>
                    <xsl:with-param name="Head" select="'Видеореклама3'"/>
                    <xsl:with-param name="Subhead" select="'POST-ROLL'"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-1.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li>
        </ul>
        <!--
	        <div class="b-roller-nav">
	            <ul class="b-rn-ul">
	                <li class="e-rn-ul">
	                    <a class="e-rn-but cur" href="#pre-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	                <li class="e-rn-ul">
	                    <a class="e-rn-but" href="#mid-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	                <li class="e-rn-ul">
	                    <a class="e-rn-but" href="#post-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	            </ul>
	        </div>
    	-->
    </div>
</xsl:template>

<xsl:template name="s-roller-frame">
    <xsl:param name="Name" select="'name'"/>
    <xsl:param name="Head" select="'Видеореклама'"/>
    <xsl:param name="Subhead" select="'PRE-ROLL'"/>
    <xsl:param name="Content" select="'s'"/>
    <xsl:param name="Pic_url" select="'/s/'"/>
    <xsl:param name="Ct" select="'s-roller-frame-1'"/>
    <div class="b-rf-text" id="{$Name}">
        <hgroup class="b-rf-hg">
            <h1 class="b-rf-head">
                <xsl:value-of select="$Head" />
            </h1>
            <h2 class="b-rf-caption">
                <xsl:value-of select="$Subhead" />
            </h2>
        </hgroup>
        <div class="b-rf-content" >
            <xsl:copy-of select="$Content" />
        </div>
        <div class="b-rf-act" >
            <xsl:call-template name="u-button">
                <xsl:with-param name="Href"  select="'/'"/>
                <xsl:with-param name="Class" select="'b-rfa-bat'"/>
                <xsl:with-param name="Title" select="'разместить рекламу'"/>
                <xsl:with-param name="Text"  select="'Разместить рекламу'"/>
            </xsl:call-template>
        </div>
    </div>
    <div class="b-rf-picture">
        <img class="b-rfp" src="{$Pic_url}" alt="{$Head}:{$Subhead}" title="{$Head}:{$Subhead}"/>
    </div>
</xsl:template>

<xsl:template name="s-roller-frame-pre-roll">
    <ul class="b-rf-ul">
        <li class="e-rf-ul">
            <xsl:text>Запускается автоматически перед видеороликом</xsl:text>
        </li>
        <li class="e-rf-ul">
            <xsl:text>Может быть до 20 секунд</xsl:text>
        </li>
        <li class="e-rf-ul">
            <xsl:text>Содержит видео и звук</xsl:text>
        </li>
        <li class="e-rf-ul">
            <xsl:text>rCTR до 15%</xsl:text>
        </li>
        <li class="e-rf-ul">
            <xsl:text>Оплата за полный просмотр ролика</xsl:text>
        </li>
    </ul>
</xsl:template>



<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>О проекте</xsl:text>
        </h1>
        <!--
        <a class="b-news-doc" href="/docs" >
            <xsl:text>Документация</xsl:text>
        </a>
        -->
    </header>
    <div class="b-news">
        <ul class="s-news-list">
            <xsl:call-template name="s-news-list" />
        </ul>
        <div class="s-news-all">
        <!--
            <xsl:call-template name="u-button">
                <xsl:with-param name="Href"  select="'/'"/>
                <xsl:with-param name="Class" select="'b-na-bat'"/>
                <xsl:with-param name="Title" select="'посмотреть все новости'"/>
                <xsl:with-param name="Text"  select="'Посмотреть все'"/>
            </xsl:call-template>
        -->
        </div>
    </div>
</xsl:template>

<xsl:template name="s-news-list">
<!-- <xsl:for> -->
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-1.jpg'" />
            </xsl:call-template>
        </li>
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-2.jpg'" />
            </xsl:call-template>
        </li>
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-3.jpg'" />
            </xsl:call-template>
        </li>
<!-- </xsl:for> -->
</xsl:template>

<xsl:template name="s-news-list-item">
    <xsl:param name="Id" select="'id'"/>
    <xsl:param name="Link" select="'/1/'"/>
    <xsl:param name="Pic_url" select="'Pic_url'"/>
    <xsl:param name="Caption"  select="'default'" />
    <xsl:param name="Alt" select="'default'" />
    <xsl:param name="Title"  select="'default'" />
    <div class="b-nlil" href="{$Link}" title="{$Caption}">
        <figure class="b-nli">
            <img
                class="b-nliim"
                src="{$Pic_url}"
                alt="{$Caption}"
                title="{$Caption}"
            />
            <figcaption class="b-nlic">
                <xsl:value-of select="$Caption"/>
            </figcaption>
        </figure>
    </div>
</xsl:template>

<xsl:template name="s-about">
    <article class="b-a"> 
        <div class="b-ac">
            <p class="b-ac-p" >
                <xsl:text>
                	TVzavr.ru – Интернет-кинотеатр, осуществляющий бесплатную онлайн-трансляцию лицензионных кино и видеофильмов.
					Интернет-портал TVzavr.ru создан в апреле 2010 года и принадлежит  ООО «Ти Ви Завр»
				</xsl:text>
            </p> 
            <br />
        <h2 class="b-ah">
            <xsl:text>Устройства доступа</xsl:text>
        </h2>
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
                    <xsl:text>Персональный компьютер</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text> Мобильные устройства ( приложения для iOS и Android )</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Smart TV</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Set-top box</xsl:text>
                </li>
            </ul> 
            <center>
				<img src="/i/ipad.jpg" />
				<img src="/i/smarttv.jpg" />
			</center>
		</div>
    </article>
</xsl:template>


<xsl:template name="foot-scripts-base">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
</xsl:template>

<!--
    ###########################################################################
    ### 
    ###########################################################################
-->

<!--
    TODO: Вынести в util
-->
<xsl:template name="u-button">
    <xsl:param name="Href" select="'Href'"/>
    <xsl:param name="Class" select="'but'"/>
    <xsl:param name="Title" select="'title'"/>
    <xsl:param name="Text" select="'text'"/>

    <a class="{$Class}"><xsl:text><![CDATA[ ]]></xsl:text></a>

    <!--
    <a class="{$Class} m-button" href="{$Href}" title="{$Title}">
        <xsl:value-of select="$Text" />
    </a>
    -->
    <!-- Alt:
        <form>
            <input type="button" />
        </form>
    -->
</xsl:template>


</xsl:stylesheet>


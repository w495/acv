<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/title-page.xsl"/>


<xsl:include href="includes/signin-form-mini.xsl" />
<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="s-title-base">
    <xsl:text>Документация</xsl:text>
</xsl:template>

<xsl:template name="s-signin">
    <xsl:call-template name="signin-form-mini">
        <xsl:with-param name="Action" select="concat('/signin/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
        <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
    </xsl:call-template>
</xsl:template>



<xsl:template name="s-about">
    <article class="b-a"> 
        <div class="b-ac">
            <p class="b-ac-p" >  
            	<p>  
					Мы размещаем рекламу, ориентированную на конечного потребителя –  посетителя TVzavr.ru и его семью.
            	</p>   
            	<p>  
					Посетители нашего ресурса являются целевой аудиторией для многих компаний, как оказывающих финансовые, телекоммуникационные, образовательные и развлекательные услуги, так и предлагающих различные товары              средней ценовой категории. 
				</p>    
            </p> 
        </div>
    </article> 
    <article class="b-a">
        <h2 class="b-ah">
			Потенциальные и ожидаемые рекламодатели 
        </h2>
        <div class="b-ac"> 
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
 					Операторы сотовой связи
                </li>
                <li class="e-ac-ul">
 					Салоны связи с рекламой своих услуг и продуктов
                </li>
                <li class="e-ac-ul">
 					Банки с рекламой различных услуг для населения
                </li>
                <li class="e-ac-ul">
 					Производители и дилеры автомобилей среднего класса
                </li>
                <li class="e-ac-ul">
 					Производители и дилеры различных продуктов высоких технологий (мобильные телефоны и др. устройства, компьютеры, телевизоры, домашние кинотеатры, музыкальные центры и т.п.)
                </li>
                <li class="e-ac-ul">
 					Производители и продавцы бытовой техники
                </li>
                <li class="e-ac-ul">
 					Фармацевтические продукты для населения
                </li>
                <li class="e-ac-ul">
 					Магазины одежды и обуви среднего класса и т.д.
                </li>
                <li class="e-ac-ul">
 					Производители и продавцы косметической продукции и бытовой химии
                </li>  
            </ul>
        </div>
    </article> 
    <article class="b-a">
        <h2 class="b-ah">
			Не принимаются к рекламе 
        </h2>
        <div class="b-ac"> 
            <ul class="b-ac-ul">
                <li class="e-ac-ul"> 
                	материалы эротического характера 
                </li>
                <li class="e-ac-ul">
 					материалы с заведомо ложной информацией
                </li>
                <li class="e-ac-ul">
 					материалы, сделанные вопреки Закону о рекламе
                </li>
                <li class="e-ac-ul">
 					Реклама алкоголя и табака не показывается в детском контенте (детскиефильмы, сериалы, мультфильмы, детские программы)
                </li> 
            </ul>
        </div>
    </article> 
</xsl:template>

<xsl:template name="s-roller">
    <div class="b-roller">
        <ul class="b-roller-frames">
            <li class="s-roller-frame">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'pre-roll'"/>
                    <xsl:with-param name="Head" select="'Документация'"/>
                    <xsl:with-param name="Subhead" select="'Список документов'"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-0.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li> 
        </ul> 
    </div>
</xsl:template>

<xsl:template name="s-roller-frame-pre-roll">
    <ul class="b-rf-ul">
        <li class="e-rf-ul">
        	<a href="/docs">
            	<xsl:text>Размещение рекламы</xsl:text>
        	</a>  
        </li>
        <li class="e-rf-ul">
        	<a href="/docs/audience">
            	<xsl:text>Аудитория</xsl:text>
        	</a> 
        </li>  
    </ul>
</xsl:template>


<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>Размещение рекламы</xsl:text>
        </h1>
        <a class="b-news-doc" href="/docs" >
            <xsl:text>Документация</xsl:text>
        </a>
    </header> 
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

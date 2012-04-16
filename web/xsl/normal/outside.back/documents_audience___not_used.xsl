<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="documents.xsl"/>


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
 


<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>Аудитория</xsl:text>
        </h1>
        <a class="b-news-doc" href="/docs" >
            <xsl:text>Документация</xsl:text>
        </a>
    </header> 
</xsl:template>



<xsl:template name="s-about">
    <article class="b-a"> 
        <div class="b-ac">
            <p class="b-ac-p" >  
            	<p>  
					Ежемесячная аудитория TVzavr.ru составляет около 2 млн посетителей, ежедневно ресурс посещают более 100.000 человек.
				</p>
				<p>
					Более 70.000 зарегистрированных пользователей, активно участвующих в проводимых порталом различного рода мероприятиях, и это число увеличивается ежедневно. 
				</p>
				<p>
					Ядро аудитории TVzavr.ru - люди с активной жизненной позицией и современными взглядами, умеющие оценить по достоинству качество предлагаемого продукта и  воспользоваться современными технологиями для его приобретения. 
				</p>    
            </p> 
        </div>
    </article> 
    <article class="b-a"> 
        <h2 class="b-ah">
            Динамика посещаемости 
        </h2>
        <div class="b-ac">
            <p class="b-ac-p" >  
				<p>
					<small>Данные Google Analytics, период 01.04.2010 – 28.02.2012</small> 
				</p>   
            	<p>  
					<img src="/i/au-0.jpg"  style="width: 954px;" />
				</p> 
            </p> 
        </div>
    </article> 
    <article class="b-a"> 
        <h2 class="b-ah">
            Пересечение с аудиториями конкурентов 
        </h2>
        <div class="b-ac">
            <p class="b-ac-p" >   
            	<p>  
					<img src="/i/au-1.jpg"  style="width: 954px;" />
				</p> 
            </p> 
        </div>
    </article> 
    <article class="b-a"> 
        <h2 class="b-ah">
            Методы наращивания/удержания трафика
        </h2>
        <div class="b-ac"> 
            <ul class="b-ac-ul">
                <li class="e-ac-ul"> 
					Рекламные кампании (Интернет, пресса, наружная реклама) 2 раза в год
                </li>
                <li class="e-ac-ul">
					Контекстная реклама (Google, Yandex) 
                </li>
                <li class="e-ac-ul">
					Активное продвижение в соцсетях 
					<img src="/i/social/facebook.jpg" />
					<img src="/i/social/vkontakte.jpg" />
					<img src="/i/social/mail.jpg" />
					<img src="/i/social/twitter.jpg" />
					<img src="/i/social/livejournal.jpg" />
                </li>
                <li class="e-ac-ul">
					Присутствие в блогах
                </li>
                <li class="e-ac-ul">
					Баннерообмен и другие партнёрские отношения с ресурсами, близкими по тематике
                </li>
                <li class="e-ac-ul">
					Участие в выставках, кинорынках
                </li>
                <li class="e-ac-ul">
					PR (интервью, комментарии, релизы) в прессе
                </li>
                <li class="e-ac-ul">
					Инфоспонсорство мероприятий
                </li>
                <li class="e-ac-ul">
					Регулярное проведение конкурсов, викторин, акций
                </li>
                <li class="e-ac-ul">
					Регулярная рассылка новостей ресурса среди его подписчиков 
                </li> 
            </ul>
        </div>
    </article> 
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

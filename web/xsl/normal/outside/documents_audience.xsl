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
     
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

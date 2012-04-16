<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="docs.xsl"/>

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
					<xsl:text>Ежемесячная аудитория TVzavr.ru составляет около 2 млн посетителей,</xsl:text>
                    <xsl:text>ежедневно ресурс посещают более 100.000 человек.</xsl:text>
				</p>
				<p>
					<xsl:text>Более 70.000 зарегистрированных пользователей,</xsl:text>
                    <xsl:text>активно участвующих в проводимых порталом различного рода мероприятиях,</xsl:text>
                    <xsl:text>и это число увеличивается ежедневно. </xsl:text>
				</p>
				<p>
					<xsl:text>Ядро аудитории TVzavr.ru - люди с активной жизненной позицией</xsl:text>
                    <xsl:text>и современными взглядами,</xsl:text>
                    <xsl:text>умеющие оценить по достоинству качество предлагаемого продукта</xsl:text>
                    <xsl:text>и воспользоваться современными технологиями для его приобретения.</xsl:text>
				</p>    
            </p> 
        </div>
    </article> 
     
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

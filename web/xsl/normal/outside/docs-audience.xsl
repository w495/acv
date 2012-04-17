<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="local-name">
    <xsl:text>Aудитория</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-news-head">
    <xsl:call-template name="local-name"/>
</xsl:template>

<!--
    Быдлогод.
    Но для макета сгодиться.
    Всего скорее надо переверстывать.

    <br/> ==>  <p>*</p>
-->

<xsl:template name="s-about-docs">
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

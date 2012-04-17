<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="local-name">
    <xsl:text>Руководство пользователя</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:call-template name="local-name"/>
</xsl:template>


<!--
    Быдлогод.
    Но для макета сгодится.
    Всего скорее надо переверстывать.

    <br/> ==>  <p class="m-docs-p">*</p>
-->

<xsl:template name="s-about-docs">
    <article class="b-a" id="offer-video">
        <xsl:call-template name="l-video"/>
    </article>
    <article class="b-a" id="offer-over">
        <xsl:call-template name="l-over"/>
    </article>
</xsl:template>

<!--
    ВИДЕО-РЕКЛАМА
-->
<xsl:template name="l-video">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Видео-реклама</xsl:text>
</h3>
<section class="b-ac">
    <table width="100%" cellspacing="0" cellpadding="0" style="border-collapse:collapse;border:solid 1px #00697f;font-size:12px;" class="e">
        <tbody>
            <tr style="height:45px;background-image:url('/m_images/sp-5.png');background-repeat:repeat-x;">
                <td width="220">Формат</td>
                <td width="360">Описание</td>
                <td width="170">Стоимость<br>(без учета НДС)</td>
                <td width="120">Трафик в неделю/<br>тыс. показов</td>
            </tr>
            <tr style="height:75px;">
                <td>Pre-roll</td>
                <td>рекламный ролик перед показом видео (до 30 сек)</td>
                <td>700 руб/<br>тыс. показов</td>
                <td>800</td>
            </tr>
            <tr style="height:75px;">
                <td>Post-roll</td>
                <td>рекламный ролик после  показа видео</td>
                <td>400 руб/<br>тыс. показов</td>
                <td>300</td>
            </tr>
            <tr style="height:75px;">
                <td>Mid-roll</td>
                <td>рекламный ролик в середине просматриваемого видео</td>
                <td>500 руб/<br>тыс. показов</td>
                <td>700</td>
            </tr>
            <tr style="height:75px;">
                <td>Pause-roll</td>
                <td>Рекламный баннер при нажатии кнопки «Pause» во время просмотра видео</td>
                <td>300 руб/<br>тыс. показов</td>
                <td>400</td>
            </tr>
            <tr style="height:75px;">
                <td>Вирусное видео</td>
                <td>Ролик рекламодателя на главной странице в  одном из окон раздела «Рекомендуемые»</td>
                <td>20.000 руб/сутки</td>
                <td>-</td>
            </tr>
        </tbody>
    </table>
</section>
</xsl:template>

<!--
    Наценки
-->
<xsl:template name="l-over">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Наценки</xsl:text>
</h3>
<section class="b-ac">
    <table width="100%" cellspacing="0" cellpadding="0" style="border-collapse:collapse;border:solid 1px #00697f;font-size:12px;" class="e">
        <tbody>
            <tr style="height:45px;background-image:url('/m_images/sp-5.png');background-repeat:repeat-x;">
                <td width="225">Таргетинг</td>
                <td width="140">Наценка</td>
                <td width="520">Комментарий</td>
            </tr>
            <tr style="height:75px;">
                <td>Частота показов</td>
                <td>20%</td>
                <td>При настройке количества показов рекламы одному пользователю в сутки</td>
            </tr>
            <tr style="height:75px;">
                <td>Гео-таргетинг</td>
                <td>20%</td>
                <td>При настройке показа рекламы в определённых городах России </td>
            </tr>
            <tr style="height:75px;">
                <td>Rich-media</td>
                <td>50%</td>
                <td>При заказе форматов рекламы с использованием RichMedia технологии</td>
            </tr>
        </tbody>
    </table>
</section>
</xsl:template>



<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

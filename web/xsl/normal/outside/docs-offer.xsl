<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="local-name">
    <xsl:text>Расценки</xsl:text>
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
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0">
    <caption class="b-ah m-ah-ht1">
        <xsl:text>Видео-реклама</xsl:text>
    </caption>
    <tbody>
        <tr class="m-docs-ttrh">
            <th scope="col" width="220">
                <p>
                    <xsl:text>Формат</xsl:text>
                </p>
            </th>
            <th scope="col" width="360">
                <p>
                    <xsl:text>Описание</xsl:text>
                </p>
            </th>
            <th scope="col" width="170">
                <p>
                    <xsl:text>Стоимость</xsl:text>
                </p>
                <p>
                    <xsl:text>(без учета НДС)</xsl:text>
                </p>
            </th>
            <th scope="col" width="120">
                <p>
                    <xsl:text>Трафик в неделю/</xsl:text>
                </p>
                <p>
                    <xsl:text>тыс. показов</xsl:text>
                </p>
            </th>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Pre-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик перед показом видео (до 30 сек)</xsl:text>
            </td>
            <td>
                <xsl:text>700 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>800</td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Post-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик после  показа видео</xsl:text>
            </td>
            <td>
                <xsl:text>400 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>300</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Mid-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик в середине просматриваемого видео</xsl:text>
            </td>
            <td>
                <xsl:text>500 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>700</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Pause-roll</xsl:text>
            </th>
            <td>
                <xsl:text>Рекламный баннер при нажатии кнопки «Pause» во время просмотра видео</xsl:text>
            </td>
            <td>
                <xsl:text>300 руб/</xsl:text><br/><xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>400</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Вирусное видео</xsl:text>
            </th>
            <td>
                <xsl:text>Ролик рекламодателя на главной странице в  одном из окон раздела «Рекомендуемые»</xsl:text>
            </td>
            <td>
                <xsl:text>20.000 руб/сутки</xsl:text>
            </td>
            <td>
                <xsl:text>-</xsl:text>
            </td>
        </tr>
    </tbody>
</table>
</xsl:template>

<!--
    Наценки
-->

<xsl:template name="l-over">
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0">
    <caption class="b-ah m-ah-ht1">
        <xsl:text>Наценки</xsl:text>
    </caption>
    <tbody>
        <tr class="m-docs-ttrh">
            <th scope="col" width="225"  >
                <xsl:text>Таргетинг</xsl:text>
            </th>
            <th scope="col" width="140">
                <xsl:text>Наценка</xsl:text>
            </th>
            <th scope="col" width="520">
                <xsl:text>Комментарий</xsl:text>
            </th>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Частота показов</xsl:text>
            </th>
            <td>
                <xsl:text>20%</xsl:text>
            </td>
            <td>
                <xsl:text>При настройке количества показоврекламы одному пользователю в сутки</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Гео-таргетинг</xsl:text>
            </th>
            <td>
                <xsl:text>20%</xsl:text>
            </td>
            <td>
                <xsl:text>При настройке показа рекламы в определённых городах России</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Rich-media</xsl:text>
            </th>
            <td>
                <xsl:text>50%</xsl:text>
            </td>
            <td>
                <xsl:text>При заказе форматов рекламы с использованием RichMedia технологии</xsl:text>
            </td>
        </tr>
    </tbody>
</table>
</xsl:template>



<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>

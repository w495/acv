<?xml version="1.0" encoding="utf-8"?>
<!--
    ЛИЧНЫЙ КАБИНЕТ РЕКЛАМОДАТЕЛЯ
-->

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="head-scripts-pers">
</xsl:template>

<xsl:template name="s-header-signin">
    <xsl:call-template name="s-logout-link" />
</xsl:template>

<xsl:template name="s-main-pers">
    <section class="s-pers">
        <xsl:call-template name="s-pers" />
    </section>
</xsl:template>

<xsl:template name="s-pers">
    <form method="POST" action="https://83.229.137.196:8080/api/pay-ua/">
        <input type="hidden" name="user_id"       value="{/data/pay/user_id}" />
        <input type="hidden" name="product_id"    value="{/data/pay/product_id}"  />
        <input type="hidden" name="amount"        value="{/data/pay/amount}"  />
        <input type="hidden" name="shop_id"       value="{/data/pay/shop_id}"  />
        <input type="hidden" name="ps_id"         value="{/data/pay/ps_id}"  />
        <input type="hidden" name="success_url"    value="{/data/pay/success_url}"  />
        <input type="hidden" name="failure_url"    value="{/data/pay/failure_url}"  />
        <input type="hidden" name="shop_f1"    value="{/data/pay/shop_f1}"  />
        <input type="hidden" name="shop_f2"    value="{/data/pay/shop_f2}"  />
        <input type="hidden" name="shop_f3"    value="{/data/pay/shop_f3}"  />
        <input type="hidden" name="shop_f4"    value="{/data/pay/shop_f4}"  />
        <input type="hidden" name="shop_f5"    value="{/data/pay/shop_f5}"  />

        <input type="hidden" name="sign" value="{/data/sign}" />
        <input type="submit" value="Оплатить"   />
    </form>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

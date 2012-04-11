<?xml version="1.0" encoding="utf-8"?>
<!--
    ЛИЧНЫЙ КАБИНЕТ РЕКЛАМОДАТЕЛЯ
-->

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />
<xsl:include href="includes/signup-form.xsl" />

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
    <form method="POST" action="http://83.229.137.196:8080/api/pay-ua/">
<!--
1006268-0001-000110advchronopayhttp://127.0.0.1:8001/success_urlhttp://127.0.0.1:8001/failure_urlahGoh6elShoa5vei
-->
        <input type="text" name="user_id" value="1" />
        <input type="text" name="product_id" value="006268-0001-0001"  />
        <input type="text" name="amount" value="10"  />
        <input type="text" name="shop_id" value="adv"  />
        <input type="text" name="ps_id" value="chronopay"  />
        <input type="text" name="success_url" value="http://127.0.0.1:8001/success_url"  />
        <input type="text" name="failure_url" value="http://127.0.0.1:8001/failure_url"  />

        <input type="text" name="sign" value="1bc285c358fefd8858e005844f6c38aded3d3494eaa635a65c5f6bdb4542cdf1fff900bd08c1ba0640aa842344237b6cb1657357fb62280786a38094e110dc78" />
<!--
        <input type="text" name="secret" value="ahGoh6elShoa5vei"  />
-->
        <input type="submit" value="Оплатить"   />

    </form>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

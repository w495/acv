<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="s-title-1">
    <xsl:text>регистрация</xsl:text>
</xsl:template>

<xsl:template name="s-main-common">
    <section class="s-signup">
        <xsl:call-template name="s-signup" />
    </section>
</xsl:template>

<xsl:template name="u-csif-rs">
    <span class="u-csif-rstar">*</span>
</xsl:template>


<xsl:template name="u-csif">
    <xsl:param name="Label_name" select="'Поле'" />
    <xsl:param name="Type" select="'text'"/>
    <xsl:param name="Name" select="''"/>
    <xsl:param name="Placeholder" select="'Введите текст'"/>
    <xsl:param name="Required" select="''"/>
    <xsl:param name="Size" select="'25'"/>
    <xsl:param name="Max_length" select="'41'"/>
    <xsl:param name="Value" select="''"/>
    <xsl:param name="Is_error" select="''"/>
    <xsl:param name="Error_message" select="''"/>

    <div class="b-csif">
        <label class="e-csif-fl m-{$Name}">
            <xsl:if test="$Type != 'hidden'">
                <xsl:value-of select="$Label_name" />
                <xsl:if test="$Required">
                    <xsl:call-template name="u-csif-rs"/>
                </xsl:if>
            </xsl:if>
        </label>
        <input type="{$Type}" placeholder="{$Placeholder}"
            size="{$Size}" maxlength="{$Max_length}"
            name="{$Name}" class="e-csif-fi {$Type}-{$Name}"
        >
            <xsl:if test="$Required != ''">
                <xsl:attribute name="required">required</xsl:attribute>
            </xsl:if>
            <xsl:if test="$Value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Value" />
                </xsl:attribute>
            </xsl:if>
        </input>
        <xsl:if test="$Is_error != ''">
            <div class="e-csif-e">
                <xsl:value-of select="$Error_message" />
            </div>
        </xsl:if>
    </div>
</xsl:template>

<xsl:template name="s-signup">
    <xsl:text>asds</xsl:text>

    <div>
        <div class="b-csif-c m-1">
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Логин'"  />
                <xsl:with-param name="Name" select="'login'"  />
                <xsl:with-param name="Placeholder" select="'только латинские буквы'"  />
                <xsl:with-param name="Required" select="'true'"/>
                <xsl:with-param name="Size" select="'25'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Пароль'" />
                <xsl:with-param name="Name" select="'password'"  />
                <xsl:with-param name="Placeholder" select="'не короче 6 символов'"  />
                <xsl:with-param name="Type" select="'password'"  />
                <xsl:with-param name="Required" select="'true'"/>
                <xsl:with-param name="Size" select="'25'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Повторите пароль'" />
                <xsl:with-param name="Name" select="'password-c'"  />
                <xsl:with-param name="Placeholder" select="'например: mY_PaSsW0rD'"  />
                <xsl:with-param name="Type" select="'password'"  />
                <xsl:with-param name="Required" select="'true'"/>
                <xsl:with-param name="Size" select="'20'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Электронная почта'" />
                <xsl:with-param name="Name" select="'email'"  />
                <xsl:with-param name="Placeholder" select="'mail@my-company.ru'"  />
                <xsl:with-param name="Required" select="'true'"/>
                <xsl:with-param name="Size" select="'20'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Город'" />
                <xsl:with-param name="Name" select="'city'"  />
                <xsl:with-param name="Placeholder" select="'Введите город'"  />
                <xsl:with-param name="Size" select="'25'"/>
            </xsl:call-template>
        </div>
        <div class="b-csif-c m-2">
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Организация'" />
                <xsl:with-param name="Name" select="'organization'"  />
                <xsl:with-param name="Placeholder" select="'Введите организацию'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Должность'" />
                <xsl:with-param name="Name" select="'position'"  />
                <xsl:with-param name="Placeholder" select="'Введите позицию'"  />
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Фамилия'" />
                <xsl:with-param name="Name" select="'lastname'"  />
                <xsl:with-param name="Placeholder" select="'Назовите Фамилию'"  />
                <xsl:with-param name="Required" select="'true'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Имя'" />
                <xsl:with-param name="Name" select="'firstname'"  />
                <xsl:with-param name="Placeholder" select="'Назовите Имя'"  />
                <xsl:with-param name="Required" select="'true'"/>
            </xsl:call-template>
            <xsl:call-template name="u-csif">
                <xsl:with-param name="Label_name" select="'Отчество'" />
                <xsl:with-param name="Name" select="'patronimic'"  />
                <xsl:with-param name="Placeholder" select="'Отчество'"  />
            </xsl:call-template>
        </div>
    </div>

    <!--
    <xsl:call-template name="signup-form">
        <xsl:with-param name="Action" select="concat('/signup/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
        <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
    </xsl:call-template>
    -->
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

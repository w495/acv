<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="s-title-common">
    <xsl:text>регистрация</xsl:text>
</xsl:template>

<xsl:template name="s-main-common">
    <h1>
        <xsl:text>Регистрация рекламодателя</xsl:text>
    </h1>
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
    <xsl:param name="Is_error" select="''"/>
    <xsl:param name="Error_type" select="''"/>
    <xsl:param name="Error_message" select="'поле введено не верно'"/>
    <xsl:param name="Val" select="''"/>


        <xsl:param name="Value" select="$Val/*[name()=$Name]"/>


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
            <xsl:if test="$Value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Value" />
                </xsl:attribute>
            </xsl:if>
        </input>
        <xsl:if test="$Error_type  != ''">
            <div class="e-csif-fm">
                <xsl:choose>
                    <xsl:when test="$Error_type = $Name">
                        <div class="e-csif-w m-{$Error_type}"
                            title="{$Error_message}">&nbsp;</div>
                    </xsl:when>
                    <xsl:when test="$Error_type != $Name">
                        <div class="e-csif-r">&nbsp;</div>
                    </xsl:when>
                    <xsl:otherwise>
                        <div class="e-csif-r">&nbsp;</div>
                    </xsl:otherwise>
                </xsl:choose>
            </div>
        </xsl:if>
    </div>
</xsl:template>

<xsl:template name="signup-form2">
    <xsl:param name="Action" select="'/signup_post'" />
    <xsl:param name="Method" select="'POST'"/>
    <xsl:param name="Error_type" select="/data/meta/error-mess"/>
    <xsl:param name="Val" select="/data/val"/>

    <xsl:if test="$Error_type != ''">
        <div class="signup-form-alert">
            <span>
                <xsl:text> Есть ошибки: </xsl:text>
            </span>
            <xsl:choose>
                <xsl:when test="$Error_type = 'email'">
                    <xsl:text>пользователь с таким E-mail уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'login'">
                    <xsl:text>пользователь с таким логином уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'captcha'">
                    <xsl:text>Каптца введена не верно</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'password'">
                    <xsl:text>пароли не совпадают</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$Error_type" />
                </xsl:otherwise>
            </xsl:choose>
        </div>
    </xsl:if>

    <form action="{$Action}" method="{$Method}" >
        <div>
            <div class="b-csif-c m-1">
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Логин'"  />
                    <xsl:with-param name="Name" select="'login'"  />
                    <xsl:with-param name="Placeholder" select="'только латинские буквы'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'25'"/>
                    <xsl:with-param name="Error_message" select="'пользователь с таким логином уже существует'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Пароль'" />
                    <xsl:with-param name="Name" select="'password'"  />
                    <xsl:with-param name="Placeholder" select="'не короче 6 символов'"  />
                    <xsl:with-param name="Type" select="'password'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'25'"/>
                    <xsl:with-param name="Error_message" select="'пароли не совпадают'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Повторите пароль'" />
                    <xsl:with-param name="Name" select="'password-c'"  />
                    <xsl:with-param name="Placeholder" select="'например: mY_PaSsW0rD'"  />
                    <xsl:with-param name="Type" select="'password'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'20'"/>
                    <xsl:with-param name="Error_message" select="'пароли не совпадают'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Электронная почта'" />
                    <xsl:with-param name="Name" select="'email'"  />
                    <xsl:with-param name="Placeholder" select="'mail@my-company.ru'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'20'"/>
                    <xsl:with-param name="Error_message" select="'пользователь с таким E-mail уже существует'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Город'" />
                    <xsl:with-param name="Name" select="'city'"  />
                    <xsl:with-param name="Placeholder" select="'Введите город'"  />
                    <xsl:with-param name="Size" select="'25'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
            </div>
            <div class="b-csif-c m-2">
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Организация'" />
                    <xsl:with-param name="Name" select="'organization'"  />
                    <xsl:with-param name="Placeholder" select="'Введите организацию'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Должность'" />
                    <xsl:with-param name="Name" select="'position'"  />
                    <xsl:with-param name="Placeholder" select="'Введите позицию'"  />
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Фамилия'" />
                    <xsl:with-param name="Name" select="'lastname'"  />
                    <xsl:with-param name="Placeholder" select="'Назовите Фамилию'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Имя'" />
                    <xsl:with-param name="Name" select="'firstname'"  />
                    <xsl:with-param name="Placeholder" select="'Назовите Имя'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Отчество'" />
                    <xsl:with-param name="Name" select="'patronimic'"  />
                    <xsl:with-param name="Placeholder" select="'Отчество'"  />
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template>
            </div>
        </div>
        <div>
            Введите текст, изображенный на картинке:<br/>
            <img src="/captcha.png" alt="captcha" class="e-captcha"/>
            <input type="text" name="captcha" /> <br/>
        </div>
        <input type="submit" value="Принять"/>
        <div>
            <xsl:text>Поля, обязательные для заполнения, помечены: </xsl:text>
            <xsl:text>.</xsl:text>
        </div>
    </form>
</xsl:template>


<xsl:template name="s-signup">
    <xsl:call-template name="signup-form2">
        <xsl:with-param name="Action" select="concat('/signup/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Error_type" select="/data/error"/>
        <xsl:with-param name="Val" select="/data/val"/>
    </xsl:call-template>
</xsl:template>

<xsl:template name="foot-scripts-common">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>

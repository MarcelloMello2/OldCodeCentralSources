<?xml version="1.0" encoding="UTF-8"?>

<!-- ********************************************************************* -->
<!-- BDS XML Documentation Viewer Wizard 3.1                               -->
<!-- (pawel.glowacki@borland.com)                                          -->
<!-- ********************************************************************* -->
<!-- XmlDocDelphi_Namespaces_Simple.xsl                                    -->
<!--                                                                       -->
<!-- This is test stylesheet to display any xml doc for Delphi project     -->
<!-- !!! Under construction !!! -> This is work in progress                --> 
<!-- ********************************************************************* -->

<xsl:stylesheet version="1.1" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

<xsl:template match="/namespace">
  <xsl:text>Delphi Namespace: </xsl:text>
  <B><xsl:value-of select="@name"/></B>
  <hr/>
  <table border="1">
  <tr>
    <th>Contains:</th>
  </tr>
  <xsl:apply-templates select="contains"/>
  </table>
  <hr/>
</xsl:template>
	
<xsl:template match="contains">
  <tr><td><U>
  <xsl:value-of select="@name"/>
  </U></td></tr>
</xsl:template>

<xsl:template match="/">
  <font face="Verdana"> 
  <xsl:apply-templates/>
  </font>
</xsl:template>


</xsl:stylesheet>

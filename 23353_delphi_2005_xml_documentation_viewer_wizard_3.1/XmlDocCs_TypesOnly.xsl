<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- ********************************************************************* -->
<!-- XmlDocCs_TypesOnly.xsl                                                -->
<!-- ********************************************************************* -->
<!-- BDS XML Documentation Viewer Wizard 3.1                               -->
<!-- (pawel.glowacki@borland.com)                                          -->
<!-- ********************************************************************* -->
<!-- This is modified version of stylesheet that appeared in the           -->
<!-- MSDN Mag article msdn.microsoft.com/msdnmag/issues/02/06/XMLC/        -->
<!-- ********************************************************************* -->

<!-- ********************************************************************* -->
<xsl:template match="doc">
	<BODY style="font-family:arial">
	<xsl:apply-templates select="./assembly/name" />
	<H3>Types</H3>
	<table border="1" width="100%" cellpadding="10">
	<xsl:apply-templates select="./members/member[starts-with(@name,'T:')]" />
	</table>
	</BODY>
</xsl:template>
<!-- ********************************************************************* -->
<xsl:template match="name">
	<TABLE width="100%" border="1" cellspacing="0" cellpadding="10" bgcolor="lightskyblue">
	<TR>
	<TD align="center">
	<H2>
	<xsl:text>Assembly: </xsl:text>
	<xsl:value-of select="." />
	</H2>
	</TD>
	</TR>
	</TABLE>
</xsl:template>
<!-- ********************************************************************* -->
<xsl:template match="member[starts-with(@name,'T:')]">
	<tr>
	  <td width="20%" valign="top">
				 <H5>
				 <xsl:value-of select="substring-after(@name,':')" />
			 </H5>
		 </td>
	  <xsl:apply-templates select="summary" />
	</tr>
</xsl:template>
<!-- ********************************************************************* -->
<xsl:template match="summary">
		 <td width="80%" valign="top">
			<H6>
	<xsl:value-of select="." />
			</H6>
	</td>
</xsl:template>
<!-- ********************************************************************* -->
</xsl:stylesheet>

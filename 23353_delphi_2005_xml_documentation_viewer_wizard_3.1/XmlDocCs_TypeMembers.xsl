<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- ********************************************************************* -->
<!-- XmlDocCs_TypeMembers.xsl                                              -->
<!-- ********************************************************************* -->
<!-- BDS XML Documentation Viewer Wizard 3.1                               -->
<!-- (pawel.glowacki@borland.com)                                          -->
<!-- ********************************************************************* -->
<!-- This is modified version of stylesheet that appeared in the           -->
<!-- MSDN Mag article msdn.microsoft.com/msdnmag/issues/02/06/XMLC/        -->
<!-- ********************************************************************* -->

<xsl:output method="html" />
<xsl:param name="WhichMember" />
<xsl:param name="WhichType" select="concat(concat( substring-before($WhichMember,'.'),  '.'), substring-before(substring-after($WhichMember,'.'),'.'))" />
<xsl:template match="doc">
	<xsl:apply-templates select="./assembly/name" />
	<xsl:apply-templates select="./members/member[contains(@name,$WhichMember)]" />
</xsl:template>
<xsl:template match="name">
	<TABLE width="100%" border="1" cellspacing="0" cellpadding="10" bgcolor="lightskyblue">
		<TR>
			<TD align="center">
				<H1>
					<xsl:text>Assembly: </xsl:text>
					<xsl:value-of select="." />
					<BR />
					<xsl:text>Type: </xsl:text>
					<xsl:value-of select="substring-after($WhichType,'.')" />
				</H1>
			</TD>
		</TR>
	</TABLE>
</xsl:template>
<xsl:template match="member">
	<H2>
		<xsl:choose>
			<xsl:when test="starts-with(@name,'M:')">
				Method
			</xsl:when>
			<xsl:when test="starts-with(@name,'F:')">
				Field
			</xsl:when>
			<xsl:when test="starts-with(@name,'P:')">
				Property
			</xsl:when>
			<xsl:when test="starts-with(@name,'E:')">
				Event
			</xsl:when>
		</xsl:choose>
	</H2>
	<TABLE width="100%" border="1" cellspacing="0" cellpadding="10" bgcolor="silver">
		<TR>
			<TD>
				<B>
					<xsl:value-of select="substring-after($WhichMember,concat($WhichType,'.'))" />
				</B>
			</TD>
		</TR>
	</TABLE>
	<P></P>
	<B>Description:</B>
	<DL>
		<xsl:apply-templates select="remarks" />
	</DL>
	<DL>
		<xsl:apply-templates select="summary" />
	</DL>
	<DL>
		<xsl:apply-templates select="value" />
	</DL>
	<B>Permission:</B>
	<DL>
		<xsl:apply-templates select="permission" />
	</DL>
	<xsl:if test="not(starts-with(@name,'F:'))">
		<B>Parameters:</B>
		<DL>
			<xsl:apply-templates select="param" />
		</DL>
		<xsl:if test="not(starts-with(@name,'E:')) and not(starts-with(@name,'P:'))">
			<B>Returns:</B>
			<DL>
				<xsl:apply-templates select="returns" />
			</DL>
		</xsl:if>
		<B>Exceptions:</B>
		<DL>
			<xsl:apply-templates select="exception" />
		</DL>
	</xsl:if>
	<B>Example:</B>
	<DL>
		<xsl:apply-templates select="example" />
	</DL>
	<B>See Also:</B>
	<DL>
		<xsl:apply-templates select="seealso" />
	</DL>
</xsl:template>
<xsl:template match="summary | remarks | value">
	<DT></DT>
	<DD>
		<xsl:apply-templates />
	</DD>
</xsl:template>
<xsl:template match="permission">
	<DT></DT>
	<DD>
		<DL>
			<DT>
				<xsl:if test="string-length(@cref) > 0">
						<xsl:variable name="MemberString" select="substring-after(@cref,':') " />
						<xsl:value-of select="$MemberString" />
				</xsl:if>
			</DT>
			<DD>
				<xsl:apply-templates />
			</DD>
		</DL>
	</DD>
</xsl:template>
<xsl:template match="param">
	<DT></DT>
	<DD>
		<DL>
			<DT>
				<I>
					<xsl:value-of select="@name" />
				</I>
			</DT>
			<DD>
				<xsl:apply-templates />
			</DD>
		</DL>
	</DD>
</xsl:template>
<xsl:template match="returns">
	<DT></DT>
	<DD>
		<xsl:apply-templates />
	</DD>
</xsl:template>
<xsl:template match="exception">
	<DT></DT>
	<DD>
		<DL>
			<DT>
				<xsl:choose>
					<xsl:when test="string-length(@cref) > 0 and starts-with(@cref,'T:')">
						<A>
							<xsl:variable name="TypeString" select="substring-after(@cref,':') " />
							<xsl:value-of select="$TypeString"/>
						</A>
					</xsl:when>
					<xsl:when test="starts-with(@cref,'!:')">
						<xsl:value-of select="substring-after(@cref,':')" />
					</xsl:when>
				</xsl:choose>
			</DT>
			<DD>
				<xsl:apply-templates />
			</DD>
		</DL>
	</DD>
</xsl:template>
<xsl:template match="example">
	<DT></DT>
	<DD>
		<xsl:apply-templates />
	</DD>
</xsl:template>
<xsl:template match="seealso">
	<DT></DT>
	<DD>
		<A>
			<xsl:variable name="MemberString" select="substring-after(@cref,':') " />
			<xsl:variable name="TypeString" select="concat(concat( substring-before($MemberString,'.'),  '.'), substring-before(substring-after($MemberString,'.'),'.'))" />
			<xsl:attribute name="href">
				GiveTypeMemberHelp.aspx?Type=
				<xsl:value-of select="$TypeString" />
				&amp;Member=
				<xsl:value-of select="$MemberString" />
			</xsl:attribute>
			<xsl:value-of select="substring-after(@cref,'.')" />
			<xsl:apply-templates />
		</A>
	</DD>
</xsl:template>
<xsl:template match="c">
	<CODE>
		<xsl:apply-templates />
	</CODE>
</xsl:template>
<xsl:template match="para">
	<P>
		<xsl:apply-templates />
	</P>
</xsl:template>
<xsl:template match="paramref">
	<I>
		<xsl:apply-templates />
	</I>
</xsl:template>
<xsl:template match="list[@type='table']">
	<TABLE border="1">
		<xsl:apply-templates mode="table" />
	</TABLE>
</xsl:template>
<xsl:template match="list[@type='bullet']">
	<UL>
		<xsl:apply-templates mode="list" />
	</UL>
</xsl:template>
<xsl:template match="list[@type='number']">
	<OL>
		<xsl:apply-templates mode="list" />
	</OL>
</xsl:template>
<xsl:template match="listheader" mode="table">
	<THEAD>
		<xsl:apply-templates mode="table" />
	</THEAD>
</xsl:template>
<xsl:template match="listheader" mode="list">
	<LI>
		<B>
			<xsl:apply-templates mode="list" />
		</B>
	</LI>
</xsl:template>
<xsl:template match="item" mode="table">
	<TR>
		<xsl:choose>
			<xsl:when test="count(child::*) = 0">
				<xsl:choose>
					<xsl:when test="name(parent::*)='listheader'">
						<TH>
							<xsl:apply-templates mode="table" />
						</TH>
					</xsl:when>
					<xsl:otherwise>
						<TD>
							<xsl:apply-templates mode="table" />
						</TD>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates mode="table" />
			</xsl:otherwise>
		</xsl:choose>
	</TR>
</xsl:template>
<xsl:template match="item" mode="list">
	<LI>
		<xsl:apply-templates mode="list" />
	</LI>
</xsl:template>
<xsl:template match="term | description" mode="table">
	<xsl:choose>
		<xsl:when test="name(parent::*)='listheader'">
			<TH>
				<xsl:apply-templates mode="table" />
			</TH>
		</xsl:when>
		<xsl:when test="name(parent::*)='item'">
			<TD>
				<xsl:apply-templates mode="table" />
			</TD>
		</xsl:when>
	</xsl:choose>
</xsl:template>
<xsl:template match="term" mode="list">
	<xsl:apply-templates mode="list" />
	<xsl:text> - </xsl:text>
</xsl:template>
<xsl:template match="description" mode="list">
	<xsl:apply-templates mode="description" />
</xsl:template>
<xsl:template match="code">
	<pre>
		<xsl:apply-templates />
	</pre>
</xsl:template>
<xsl:template match="*">
	<xsl:copy>
		<xsl:apply-templates />
	</xsl:copy>
</xsl:template>
</xsl:stylesheet>

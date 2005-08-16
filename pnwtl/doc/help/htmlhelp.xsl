<?xml version="1.0"?>
<xsl:stylesheet version='1.0' 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
	<xsl:import href="/utils/docbook/xsl/htmlhelp/htmlhelp.xsl" />
	<xsl:param name="htmlhelp.chm" select="'pn2.chm'"/>
	<xsl:param name="htmlhelp.hhc.binary" select="0"/>
	<xsl:param name="htmlhelp.hhc.folders.instead.books" select="0"/>
	<xsl:param name="toc.section.depth" select="3"/>
	<xsl:param name="chunk.section.depth" select="3"/>
	<!--<xsl:param name="chunk.tocs.and.lots" select="1"/>-->
	<xsl:param name="chunk.first.sections" select="1"/>
	
</xsl:stylesheet>
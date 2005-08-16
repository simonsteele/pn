@echo off
set XSLTP=c:\utils\xsltproc\xsltproc
set HHC="c:\Program Files\HTML Help Workshop\hhc"
%XSLTP% --xinclude htmlhelp.xsl help.xml
%HHC% htmlhelp.hhp
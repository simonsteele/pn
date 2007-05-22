@echo off
svn update
c:\utils\nant\bin\nant -D:"ver=%1" %2
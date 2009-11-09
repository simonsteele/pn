@echo off
hg update
c:\utils\nant\bin\nant -D:"ver=%1" %2 %3 %4
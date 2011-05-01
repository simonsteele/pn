@echo off
call hg update
c:\utils\nant\bin\nant -D:"ver=%1" %2 %3 %4
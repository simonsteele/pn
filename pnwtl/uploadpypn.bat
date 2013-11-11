@echo off
call hg update
c:\utils\nant\bin\nant -D:"ver=%1" -D:"password=%2" uploadpypn %3 %4
@echo off
hg update
c:\utils\nant\bin\nant -D:"ver=%1" -D:"password=%2" upload %3 %4
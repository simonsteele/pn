@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsallxp.bat" x86
c:\utils\nant\bin\nant -D:"ver=%1" %2 %3 %4
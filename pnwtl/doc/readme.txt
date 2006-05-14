========================================
Release Name: 2.0.7.x
Build: 2.0.7.x, Version Label: 
========================================

What's new?
-----------

1) Scripting interface and Python scripting implementation
2) Detection of UTF-8 without a Byte Order Mark, also an encoding option (parts by Oleg Gerasimov)
3) Sets unicode character set automatically when a unicode file encoding is selected.
4) Decorators in Python scheme
5) Add New File in Magic Folders context menu
6) Tag browser window (Klaus)
7) Full VHDL scheme instead of user scheme (Klaus)
8) VHDL tag support (Klaus)
9) Remove non-existant MRU entries when selected (SF RFE #1469296)

Bug Fixes:
----------

1) Command-line paramters not working (SF Bug #1334272)
2) Control-R doesn't update text in replace window (SF Bug #1360767)
3) Crash on undo after file revert (SF Bug #1371138)
4) Drop down lists too small (SF Bug #1373484)
5) Extension Editbox input error (SF Bug #1358022)
6) Filenames with ampersand (&) (SF Bug #1241368)
7) Sanitise relative path strings in output parser (Oleg Gerasimov)
8) <Enter> key in project window open selected file (Oleg Gerasimov)
9) Opening non-existant file from recent menu, opens new file (SF Bug #1471053)
10) Browse for folder inappriopriately shows New Folder button (SF Bug #1471056)
11) Can't remove file association if text is entered (SF Bug #1463848)

If you find a bug:
------------------
There are quite possibly bugs with these features (or others), 
please let us know if you find any. You can help by providing 
us with a copy of the file(s):

%appdata%\Echo Software\PN2\User*.xml

e.g.
C:\Documents and Settings\Simon\Application Data\Echo Software\PN2\User*.xml

E-mail: ss at pnotepad dot org with your reports, or use the handy 
"Report a Bug" option on the Help menu to add it straight to the
sourceforge bug tracker.

Thanks for testing!


Known things:
-------------

Please don't moan about the following, they are all quite obvious to us already:

1) Missing schemes from pn1.
2) Toolbars aren't dockable.
3) No Hex Editor.

So what is good?
----------------

1) New editing engine (The excellent Scintilla: http://www.scintilla.org/).
2) Now written in C++ using the Windows Template Library.
3) Fast. Really Fast - compare opening times for large files with PN1.
4) Four (count-em!) toolbars.
5) Lovely XP-Style tabs.
6) Save-As dialog contains a "File Format" feature.
7) Support for C++, HTML, XML and Object Pascal. More schemes coming soon.
8) Highly configurable XML backend for scheme configuration.
9) The minibar from pn1 is here (see at the bottom of the edit windows).
10) Scoping / Folding / Outlining (Open a C++, Pascal or Java file to see this, it's cool!).
11) Word-wrap.
12) Schemes configuration.
13) Tools with output capturing, running in a separate thread. Much better than PN1.
14) Smart-Start.
15) Projects much more powerful - more like those in Visual Studio.
========================================
Release Name: 2.0.5.0
Build: 2.0.5.x, Version Label: (none)
========================================

What's new?
-----------

1) Scheme-selector toolbar.
2) Find toolbar.
3) Many find improvements (thanks mostly to Bjoern Graf).
4) Tile Vertical / Tile Horizontal.
5) Show path of file to be executed (SF Feature #765270).
6) Visual Help options (highlight current line, long line help).
7) Sanitise file names from output window to better find files.
8) Text clips browser.
9) Projects/Workspaces.

Bug Fixes:
----------

1) Show Full Path turns off after a revert. (SF Bug #773608)
2) Stop Tools doesn't work. (SF Bug #773294)
3) If you don't update a file when it's changed on disk it is marked as changed.
4) Maximized window state not saved. (SF Bug #770302)
5) Mixed path styles now sanitised in output window. (SF Bug #797397)

If you find a bug:
------------------
There are quite possibly bugs with these features (or others), 
please let us know if you find any. You can help by providing 
us with a copy of the file(s):
%appdata%\Echo Software\PN2\User*.xml
e.g.
C:\Documents and Settings\Simon\Application Data\Echo Software\PN2\User*.xml

E-mail: ss@pnotepad.org with your reports, or use the handy 
"Report a Bug" option on the Help menu to add it straight to the
sourceforge bug tracker.

Thanks for testing!


Known things:
-------------

Please don't moan about the following, they are all quite obvious to us already:

1) Missing schemes from pn1.
2) Toolbars aren't dockable.
3) You can't turn the tabs off.
4) PN needs a "Monospaced Font" mode.

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
10) Scoping / Folding / Outlining (Open a C++, Pascal or Java file to see this, it's lovely).
11) Word-wrap.
12) Schemes configuration.
13) Tools with output capturing, running in a separate thread. Much better than PN1.
14) Smart-Start.
15) Projects much more powerful - more like those in Visual Studio.
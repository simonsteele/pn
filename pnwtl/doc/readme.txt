========================================
Release Name: 2.3
Build: 2.3, Version Label: Charles
========================================

What's new in 2.3?
------------------------------------------

1) Jump dialog now filters results as you type.
2) Custom scheme is now implemented as a PN extension.
3) Double-click on tab bar (not on tab) starts a new doc (request #145).
4) 3 types of block comment supported in custom schemes (schemedef).
5) Tags only auto-closed in the correct state in HTML/XML, hr br and img not auto-closed.
6) Miscellaneous updates to schemes to improve highlighting (including HTML5 elements and attributes).
7) RTF export reworked, background colours now working better.
8) Properties scheme folding.
9) Two identifier types in customscheme independant of keyword recognition.
10) Select a URL and right-click to go to it.
11) New German and Russian translations.
12) Scheme Presets: Solarized (Light), improved Obsidian, thanks to contributors.

Bug Fixes:
----------

1) Disable non-relevant project menu items when using magic folders. Fixes issue #1049.
2) Exit code should be 0. Fixes issue #1070.
3) Fix issue leading to crash after closing split view. Fixes issue #1161, #1137, #1301.
4) Allow customizing New Default keyboard shortcut. Allow localising Plain Text scheme name. Fixes issue #1167.
5) More protection for invalid editor scenarios. Fixes issue #1174.
6) Fix window positioning where left-most monitor has negative co-ordinates. Fixes issue #500.
7) Schemes now always rebuilt at install time, remove duplicate Powershell scheme.
8) Fixed an issue where undo didn't correctly clear clip insertion state. Fixes issue #1183.
9) Tools/New/Language menus missing on Chinese Windows XP. Fixes issue #704.
10) Translations not working in Portable distribution. Fixes issue #1220.
11) Auto complete of tags now works when using "ignore case". Fixes issue #1067.
12) Docking window content focused rather than docking window itself, improves behaviour on showing output. Issue #351.
13) Bookmark image now non-aliased. Fixes issue #198.
14) Disable C++ preprocessor tracking.
15) Line comment insertion fixed. Fixes issues #30, #458, #673.
16) Smartstart option serialization. Fixes issue #1316.
17) Jump dialog fixed. Fixes issue #1313.
18) Fix folding. Fixes issue #1284.
19) Remove "-" from PHP character set. Fixes issue #1300.
20) Fix translation of the Page Setup dialog, and several other translation issues. Issues #1307, #1306.
21) Make sure alternate streams and file attributes are retained across saves. Fixes issue #242.
22) Provide a command to return focus to editor at all times, and make sure docking window content is focused on activation. Fixes issue #351.
23) Fix clip indents to avoid breaking multi-line python scripts. Fixes issue #1346.
24) Fix line endings before eval'ing script. Fixes issue #1339.
25) Allow escape to cancel find dialogs. Fixes issues #1074 and #1385.
26) Fixes to the following schemes: Pascal, Modula-2, Matlab, TCL.

What's new in 2.3 under the cover?
------------------------------------------

1) Scintilla updated to 2.23
2) Boost updated to 1.45
3) Now built with Visual Studio 2010


Extensions Interface Changes:
-----------------------------

1) All extensions should be built with Boost 1.45 and VS2010
2) IScriptRunner and IScriptRunner2 merged
3) OpenFile will now switch to an active open window rather than always opening.


If you find a bug:
------------------
There are quite possibly bugs with these features (or others), 
please let us know if you find any. You can help by providing 
us with a copy of the file(s):

%appdata%\Echo Software\PN2\User*.xml

e.g.
XP: C:\Documents and Settings\Simon\Application Data\Echo Software\PN2\User*.xml
Vista+: C:\Users\Simon\AppData\Roaming\Echo Software\PN2\User*.xml

E-mail: ss at pnotepad dot org with your reports, or use the handy 
"Report a Bug" option on the Help menu to add it straight to the
Google Code bug tracker.

Thanks for using Programmer's Notepad!

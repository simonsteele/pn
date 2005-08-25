========================================
Release Name: 2.0.6.0
Build: 2.0.6.??, Version Label: 
========================================

What's new?
-----------

1) Project Properties
2) Editable Magic Folder Properties
3) Ctrl+H for Replace.
4) Right-Click on a file to add to a project.
5) Selected text insertion into find dialog is more selective (no pun intended).
6) Project changed indicator (appends * to name).
7) Project tools.
8) Tab usage option per scheme (SF RFE #1050938).
9) Stack Based Ctrl-Tab Ordering (SF RFE #995790).
10) Browse for folder in find in files dialog.
11) Support for vbc compiler output (SF Bug #1168477).
12) Docbook Scheme
13) Scintilla version 1.6.3
14) CSS-2 Support in CSS lexer
15) Projects remember folder expansion state.
16) Added Ctrl-W to close the current file (as well as Ctrl-F4) (SF RFE #1169428).
17) New Project dialog.
18) Save project from context menu.
19) Refresh magic folder from context menu.
20) Ctrl+[ jumps to matching or nearest brace.
21) Ruby scheme
22) Better behaviour with files opened using relative paths.
23) Wrap text clips around selected text (SF RFE #1199676).
24) Escape key closes find results if no output windows are open.
25) Optionally save workspace between runs ("Remember open files and projects between sessions" in General options).
26) Open to save workspace files from File menu, opening a .pnws file opens the saved workspace.
27) SmartStart applied to opened files.
28) New Incremental FindBar (Press Ctrl+/ and start typing, escape to cancel).
29) Customisable scheme mappings (choose schemes for file types - Options|Files).
30) Find Previous - Shift+F3
31) Support for Java properties files (SF RFE #1267017).

Bug Fixes:
----------

1) Find in Files gets selected text.
2) Find in selection fixed.
3) Find Next and Replace One no longer supported in "in selection" mode.
4) Find Next replaces selection in find text box when text not found.
5) Switching tabs causes "re-maximization" (SF Bug #1169434).
6) Launch Search Dialog With No Document (SF Bug #1168100).
7) "Allow backslash expressions" does not work in search.
8) Crash on Running Find in Files With Operation in Progress (SF Bug #1168101).
9) code-folding works not correct (SF Bug #1050483).
10) HTML export uses wrong CSS syntax (SF Bug #1074120).
11) Normal Search does not work with UTF-8 (Unicode) chars > 127 (SF Bug #1200456).
12) Relative paths don't work from command-line with single-instance PN (SF Bug #1161783).
13) Window Dimensions After Un-Maximizing (SF Bug #1167299).
14) File -> New Broken (SF Bug #1221755).
15) Don't exit if save is cancelled for a previously unsaved item.
16) Selecting the XML lexer after another does not correctly style.
17) No code-folding for XML (SF Bug #1212335).
18) Keyword customisation causes hang (SF Bug #1209962).
19) Delete Line same as Cut Line (SF Bug #1234653).
20) "Search Up" box not visible for replace.
21) Bad coloring in Text Clips combo box with non-white window colors (SF Bug #1065666).
22) Tried to open a file twice when that file was not found.
23) VHDL Syntax file inclusion (SF Bug #1165830).
24) Crash when adding a project tool (SF Bug #1195786).

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
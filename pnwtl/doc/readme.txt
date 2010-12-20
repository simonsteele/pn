========================================
Release Name: 2.2
Build: 2.2, Version Label: ellington
========================================

What's new in 2.2?
------------------------------------------

1) Complete Unicode conversion, files, searching, projects, UI, clips...
2) Complete redesign of text clips, with editing built in to new UI view and code templates and text clips consolidated
3) Multiple concurrent selections (ctrl+select)
4) Type into multiple selections and block selections
5) Virtual space
6) Translations - PN in your language
7) Prototype Command bar for PyPN users, supports customisable modal commands
8) Flexible split views
9) Support for notepad.exe interception via -z (uses Image File Execution options)
10) Numerous small visual tweaks
11) Line padding options - increase visual space between lines for improved readability
12) CTags upgraded to 5.8
13) Converting between ANSI/Unicode properly converts current file contents (#1020, #1021, #1018, #148)
14) New TextUtil extension
15) Improvements to features supported in extensions SDK
16) Explorer menu integration in Options | Integration
17) Vastly improved file encoding options and defaults
18) Strip line endings on save
19) Make sure file ends in blank line
20) Double-click on position brings up Go To dialog
21) Double-click on INS to change to OVR and vice-versa
22) Fixed-width fonts highlighted in Options
23) File exlusions for magic folders
24) Tagging support for batch, basic, matlab, ocaml
25) 

Bug Fixes:
----------

This is just a small set of fixes in 2.2 over 2.0.10:

1) Save All now available regardless of current file save state
2) Long line length recalculated better, scrolling improved
3) Scheme command line argument was broken (#899)
4) Cannot autocomplete after cancelling text clip insertion (#857)
5) New default 0 scrollbar width (#897)
6) Assorted autocomplete fixes, should now be more reliable
7) Fixed incorrect calltip display (#792)
8) Fix default style continuation with custom schemes (#890)
9) Remove magic folder if add is cancelled (#868)
10) Minor UI fixes (#869)
11) Fix for slow loading when there are large zip files on the desktop
12) Error in command bar when pressing enter with no text (#842)
13) Menu access via keyboard was broken when focused on a docking window
14) Avoid docking windows being minimised etc. (#776)
15) Problem with file save modifiers (#836)
16) Hidden files can now be saved (#859)
17) Remapped jump to matching brace to Alt-[ to avoid stomping on jump to previous paragraph which is Ctrl-[ (#831)
18) PHP scheme includes $ as a word char
19) Commenting fixed for vbscript scheme
20) Incorrect menu item hints fixed
21) Smart highlight works across whole document (#525)
22) Workspace save now remembers tab order
23) Allow forcing readonly from the command line
24) Various ctags-related crashes fixed
25) Check for updates stability fixes
26) Changing magic folder path works properly (#760)
27) Fix for project case issues (#527)
28) Fixed side-by-side warning for ctagsnavigator (#159)
29) Projects default to case-insensitive sort (#116)
30) Support escaping slashes with regular expressions (#594)
31) Some UTF-16 related fixes (#592)


What's new in 2.2 under the cover?
------------------------------------------

1) Scintilla updated to 2.11
2) Boost updated to 1.42
3) Now built with Visual Studio 2008
4) Code now in Mercurial


Extensions Interface Changes:
-----------------------------

1) Many changes for unicode, all paths are now wchar_t


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

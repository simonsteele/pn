========================================
Release Name: 2.1.4.x
Build: 2.1.4.x, Version Label: 
========================================

What's new in 2.1?
------------------------------------------

1) Multiple concurrent selections (ctrl+select)
2) Type into multiple selections and block selections
3) Virtual space
4) Translations - PN in your language
5) Command bar for PyPN users, supports customisable modal commands
6) Complete Unicode conversion, files, searching, projects, UI, clips...
7) Flexible split views
8) New -z command-line parameter for use with Image File Execution options
9) Complete redesign of text clips, with editing built in to new UI view and code templates and text clips consolidated
10) Small visual tweaks
11) Line padding options - increase visual space between lines for improved readability
12) CTags upgraded to 5.8

What's new in 2.1 under the cover?
------------------------------------------

1) Scintilla updated to latest
2) Boost updated to 1.42
3) Now built with Visual Studio 2008

Bug Fixes:
----------

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

Thanks for testing!

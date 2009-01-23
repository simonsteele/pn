========================================
Release Name: 2.0.9.x
Build: 2.0.9.x, Version Label: 
========================================

What's new?
-----------

1) Switched to vista-style message dialogs
2) Updated to Boost 1.36
3) Updated to Scintilla 1.76
4) Added new Open Files view
5) Added new File Browser view
6) New icons for docking windows
7) Changed text search regular expressions engine to Boost Xpressive, now supports multi-line regular expressions
8) Added support for word character set specification in schemes
9) New style presets (ZenBurn, Murky, Turbo)
10) Auto-complete choices are backed by file instead of the registry
11) Goto line highlights the text that was jumped to when using tags
12) Support for folding at "else" added to schemes (see cpp.scheme)
13) Better default caret x positioning behaviour
14) Minor visual improvements
15) Marked as vista-aware, no more VirtualStore problems
16) Automatic update check
17) Go to previous bookmark
18) Switched to vista open/save dialogs on appropriate platforms
19) New SDK package
20) New Find in Files option: All Open Files
21) New link to Forums in Help menu (#76)
22) New Move Line Up and Move Line Down commands in Edit|Line (#68)
23) Added readonly toggle to editor window (from Klaus, #83)
24) (re-)Added Mark All
25) Added Clear Bookmarks command (#90)
26) Explorer menus in context menus
27) Improved extension crash debugging (#127)
28) Option to create backups when saving files (#106)
29) New commands for working with current text selection (#245)
30) File changed notification can be disabled for a single file (#214)
31) Text clip editor now uses default scheme (#144)
32) Improved PHP styling with colour presets (#195)
33) Comment commands and text transforms on context menu (#223, #221)
34) Re-use unchanged blank document window when opening a file (#17)
35) Clean up the UI for first run (#211)

Bug Fixes:
----------

1) Don't auto-append find text
2) Some message dialogs appear oddly on Win9x
3) Can't use magic folder wizard without C: (SF #1703085)
4) Correctly show toolbar toggles on load (#29)
5) Don't show quickfind bar on toggle of find toolbar
6) Make sure custom line number colouring works for default scheme
7) Minor folding fix
8) Check open files before opening from most recently used
9) Fixed tags parsing where tag includes the dollar sign
10) Find in files extensions can now be separated by space, comma, semicolon
11) Inform the user when they're trying to search with an invalid regex
12) Fix problem with tabs getting hidden
13) Maximum scheme name length ignored when matching schemes to files (#71)
14) OnAfterLoad not called when document first loaded
15) Undo buffer not cleared properly at load and revert (#35)
16) Support forward-slash paths in command-line parameters (#49)
17) When wrapping text, end key goes to the end of the wrap line (#69)
18) Odd behaviour when cancelling removal of unsaved project (SF #1957796)
19) Fixed long line help options
20) Don't change to file directory on open, allows directory deletion
21) Allow "Del" key in keyboard shortcut editor (#45)
22) Hopefully fixed HTML help window positioning (#110)
23) Fixed scheme selection in options dialog (#13)
24) Fix Windows 2000 support (#124)
25) Restore document windows on jumps (#130)
26) Allow opening workspace files again (#25)
27) Fixed Win7 Close Hang
28) Fixed Tab and Space conversion errors (#301)
29) Fixed extended character entry (#48)
30) Project file icon changes when rename changes type (#259)
31) Tool running crash (#250)
32) Launching non-captured console tool disables Ctrl-C (#78)
33) Space can't be entered as a keyboard shortcut (#284)
34) Fix up ascii text clips, and allow insertion of special characters (#274)
35) Cancelling parameters window doesn't cancel tool running (#285)
36) Search hidden files option missing (#229)
37) files are reopened when loading workspaces (#228)
38) Project tools missing from menus (#227)
39) Make sure jumping to a bookmark scrolls into view (#219)
40) Middle click close on updated file causes crash (#203)

Extensions Interface Changes:
-----------------------------

1) Split IDocumentEventSink and ITextEditorEventSink (previously you had to implement both)
2) More IDocumentEventSink methods

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
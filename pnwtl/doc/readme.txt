========================================
Release Name: 2.0.9.x
Build: 2.0.9.x, Version Label: 
========================================

What's new?
-----------

1) Switched to vista-style message dialogs
2) Updated to Boost 1.35
3) Updated to Scintilla 1.76
4) Added new Open Files view
5) Added new File Browser view
6) New icons for docking windows
7) Changed text search regular expressions engine to Boost Xpressive
8) Added support for word character set specification in schemes
9) New style presets (ZenBurn, Murky)
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
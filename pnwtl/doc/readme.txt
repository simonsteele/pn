========================================
Release Name: 2.0.4-miles
Build: 2.0.4.14, Version Label: miles
========================================

What's new?
-----------

1) Groups of scheme settings can now be configured using their style group head item.

example: To change the background for all PHP styles, simply change the background
on the main PHP item that contains the styles below it. This will not override any
custom backgrounds already set on those items - those will need to be reset.

2) User-Schemes

Define your own schemes using simple XML - can support a large number of languages. 
See the included vhdl.schemedef sample. There will be documentation for this soon.

3) Tool output parsing for a number of built-in types.
4) Output parsing for user-defined output strings.
5) Matched output jumps to the correct position in the correct file when clicked.
6) Tool editor dialog re-designed (a bit)
7) Docking global output window.
8) Clipboard Swap (Ctrl-Shift-C)

paste clipboard into selection and place selection on the clipboard.

9) Options dialog more keyboard accessible.
10) Maximise on opening.
11) PN remembers its window positioning between loads.
12) Perl scheme.
13) Better pascal scheme.
14) Folding keyboard shortcuts: 

Ctrl-Alt-+ Expand All, Ctrl-Alt-- Collapse All, Ctrl+* toggle current fold block

15) Copy as RTF and export RTF.

16-ish) A Modula-2 scheme will be released separately in a week or two. Some code changes were made to Scintilla for this.

Please let me know about any problems with this release: ss@pnotepad.org.

If you find a bug:
------------------
There are quite possibly bugs with these features (or others), 
please let us know if you find any. You can help by providing 
us with a copy of the file(s):
%usersettings%\Echo Software\PN2\User*.xml

E-mail: ss@pnotepad.org with your reports, or use the handy 
"Report a Bug" option on the Help menu to add it straight to the
sourceforge bug tracker.

Thanks for testing!


Known things:
-------------

Please don't moan about the following, they are all quite obvious to us already:

1) Missing schemes from pn1.
2) No project management.
3) Toolbars aren't dockable.
4) You can't turn the tabs off.
5) PN needs a "Monospaced Font" mode.

So what is good?
----------------

1) New editing engine (The excellent Scintilla: http://www.scintilla.org/).
2) Now written in C++ using the Windows Template Library.
3) Fast. Really Fast - compare opening times for large files with PN1.
4) Two (count-em!) toolbars. Obviously this sort of stuff will get better as time goes on.
5) Lovely XP-Style tabs.
6) Save-As dialog contains a "File Format" feature.
7) Support for C++, HTML, XML and Object Pascal. More schemes coming soon.
8) Highly configurable XML backend for scheme configuration.
9) The minibar from pn1 is here (see at the bottom of the edit windows).
10) Scoping / Folding / Outlining (Open a C++ or Pascal file to see this, it's lovely).
11) Word-wrap.
12) Schemes configuration.
13) Tools with output capturing, running in a separate thread. Much better than PN1.
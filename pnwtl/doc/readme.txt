========================================
Release Name: 2.0.6.0
Build: 2.0.6.??, Version Label: 
========================================

What's new?
-----------

1) View Whitespace button on the editor mini-toolbar.
2) Choose a file encoding at file open time, allows opening unicode files with no byte order marker.
3) Command-line parameters:

--line x 	(-l x) Open a file on a given line.
--col x 	(-c x) Open a file on a given column.
--scheme x	(-s x) Open a file using a named scheme (short-name only).

e.g. 

pn -l 49 -c 14 --scheme cpp test.dat

4) MagicFolder wizard instead of simple folder selector. Now allows 
   specification of file and folder filters for magic folders.
5) Trying to save a readonly file will now allow you to attempt to overwrite
   that file.
6) New Find dialog, encompassing Find, Replace and Find in Files.
7) Find in Files (in case you missed it on the above line)!

Bug Fixes:
----------

1) The external file change notification window now appears on the correct
   monitor in multiple monitor systems.
2) losing scheme options when changing scheme (SF Bug #1033170).
3) inability to reduce font size beneath 10 for all styles (SF Bug #1070782).
4) Font changes not set for common style items (SF Bug #952562).
5) AutoComplete boxes on Win98 (SF Bug #1075138).
6) XML parser does not like ´ character (SF Bug #1036079).
7) Find dialog focus problem.
8) 'Find in files' doesn't do anything? (SF Bug #969334).
9) "More Options" in schemes not saving changes (SF Bug #1028974).
10) Option to choose whether Open should start in current file's directory (SF Bug #1055115).
11) Option to choose whether to close Find dialog on Find Next (SF Bug #945135).

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
========================================
Release Name: 2.0.7.680
Build: 2.0.7.680, Version Label: 
========================================

What's new?
-----------

1) Extensions interface
2) Detection of UTF-8 without a Byte Order Mark, also an encoding option (parts by Oleg Gerasimov)
3) Sets unicode character set automatically when a unicode file encoding is selected.
4) Decorators in Python scheme
5) Add New File in Magic Folders context menu
6) Tag browser window (Klaus, Manuel)
7) Full VHDL scheme instead of user scheme (Klaus)
8) VHDL tag support (Klaus)
9) Remove non-existant MRU entries when selected (SF RFE #1469296)
10) Python scripting extension (pypn)
11) New lovely toolbar images courtesy of famfamfam.com
12) Save All toolbar button
13) Keyboard customisation
14) Text clips maintain current indentation (SF Bug #1412316)
15) Default Character Set option for non-unicode language support (experimental)
16) Comment text automatically (scheme support dependant)
17) New tool parameter: ${PNPath} - the path PN is installed in
18) Comment-out code: Edit|Comments|Line or Stream or Block - depending on language support
19) Keyword and tags based autocomplete (Manuel)
20) Cut, copy, paste, undo, redo buttons enabled only when available
21) $(ProjectName) for tools - the active project name (SF RFE #1482915))
22) Find in files auto-locations in drop-down
23) New command-line parameters (see help)
24) Minor master scheme changes
25) Allows you to set scheme properties for all languages in one place (global styles)
26) Scheme changes: Hypertext->HTML, PHP added, PHP Script added
27) New extension points for OnBeforeSave and OnAfterLoad
28) ctags tag provider is now an extension, custom tagger 
   plugin format removed
29) Extension discovery method: run "pn -findexts"
30) Added $(ProjectGroupName) tools property
31) Added Rename shortcut to project context menus (you can already rename using the two clicks convention)
32) Added $(ProjectProp:*) and $(FileProp:*) tools shortcuts, these retrieve values from project item configurations
33) Added alpha settings for line highlight, new default colour to make use of alpha
34) Added option to disable folding
35) New code templates feature (try typing "if" and then Edit->Insert Template in c/c++)
36) Now supports custom ctags tagger definitions (see in taggers\ctags directory)
37) Added Open Containing Folder, Open in Default Folder and Copy File Path to various context menus
38) Text clips now cached in user settings directory to avoid user needing to change files in install path
39) New extensions method pn_get_extension_info
40) New extensions types for passing strings safely with stored allocators

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
12) Can't use a relative directory path to store PN settings (see config.xml)
13) Drag & drop file into project folder: appears twice (SF Bug #1490889)
14) Bugs with projects window (SF Bug #1330960)
15) "Recent Files" List Not Updated (SF Bug #1377909)
16) Project tools keyboard shortcuts don't work (SF Bug #1422790)
17) Line endings detection didn't work properly for UTF-16 (SF Bug #1480945)
18) Using INI file settings did not store re-load options properly
19) UI settings not stored in INI file when using INI storage
20) Searching back and forward broken (SF Bug #1451538)
21) PN crashes when trying to access non-existent project (SF Bug #1496940)
22) Numbered bookmarks don't work properly (SF Bug #1326469)
23) Global output window flickers when output being added
24) New default magic folder file filter (SF Bug #1371183)
25) Numbered bookmarks don't work properly (SF Bug #1326469)
26) Quick Find Textbox Border Color Problem (SF Bug #1465805)
27) Find bar corruption after documents closed (SF Bug #1361852)
28) BOM present even when 'UTF-8 No mark' selected as encoding (SF Bug #1545264)
29) Can't copy file from magic folder to project properly (SF Bug #1576076)
30) File type edits don't show new value in list.
31) Minor Vista bug fixes
32) Don't reset scheme when saving files with no extension
33) Occasional failure to save settings on exit (particularly with debug builds)
34) Line numbers toggle not updated when default (SF Bug #1729577)
35) File corruption with utf-16 files > 65536 characters (SF Bug #1741342)
36) Some php keywords only being matched in uppercase
37) Tabs/Spaces override changes don't save (SF Bug #1632540)

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
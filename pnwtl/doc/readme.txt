========================================
Release Name: 2.0.3-coltrane
Build: 2.0.3.27, Version Label: coltrane
========================================

Breaking Change Notices:
------------------------
User configuration files have moved from:
%usersettings%\Echo Software\Programmers Notepad\Schemes\
to:
%usersettings%\Echo Software\PN2\

this will lose any scheme colouring changes you have made. 
You can simply move all files from the previous version into
the new correct location to fix this. Sorry for any 
inconvenience...

What's new?
-----------

1) Configurable scheme keywords.
2) Now opens filenames from the command-line (or explorer associations).
3) Basic Tools functionality. Configure from Options->Tools.
4) CSS Scheme now works.
5) Colourised output capturing in tools (configurable in Options->Schemes).
6) Batch, Diff and Make files schemes.

Please hammer the tools stuff, it uses threads and I'm worried 
about a couple of the synchronisation bits. Please let me know 
about any problems with it: ss@pnotepad.org.

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
3) No tools.
4) Toolbars aren't dockable.
5) You can't turn the tabs off.
6) Tools are currently only on a per-scheme basis, there are no global tools. This will change.
7) Output current shows up as a new file type, and you can switch the highlighter to it. This is a bug.
8) PN needs a "Monospaced Font" mode.

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
13) Tools with output capturing, running a separate process. Much better than PN1.
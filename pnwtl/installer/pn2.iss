[Setup]
AppName=Programmers Notepad
AppVerName=Programmers Notepad 2.0.3
AppPublisher=Simon Steele
AppPublisherURL=http://www.pnotepad.org/
AppSupportURL=http://www.pnotepad.org/
AppUpdatesURL=http://www.pnotepad.org/
DefaultDirName={pf}\Programmers Notepad
DefaultGroupName=Programmers Notepad
LicenseFile=doc\license.txt
OutputDir=installer

[Tasks]
Name: desktopicon; Description: Create a &desktop icon; GroupDescription: Additional icons:
Name: quicklaunchicon; Description: Create a &Quick Launch icon; GroupDescription: Additional icons:; Flags: unchecked

[Files]
Source: doc\license.txt; DestDir: {app}
Source: doc\license.html; DestDir: {app}
Source: bin\pn.exe; DestDir: {app}; Flags: ignoreversion
Source: bin\libexpat.dll; DestDir: {app}; Flags: ignoreversion
Source: bin\SciLexer.dll; DestDir: {app}; Flags: ignoreversion
Source: bin\schemes\*.scheme; DestDir: {app}; Flags: ignoreversion
Source: bin\schemes\extmap.dat; DestDir: {app}; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\Programmers Notepad 2; Filename: {app}\pn.exe
Name: {group}\License; Filename: {app}\license.html
Name: {userdesktop}\Programmers Notepad; Filename: {app}\pn.exe; Tasks: desktopicon
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Programmers Notepad; Filename: {app}\pn.exe; Tasks: quicklaunchicon

[Run]
Filename: {app}\pn.exe; Description: Launch Programmers Notepad; Flags: nowait postinstall skipifsilent

[_ISTool]
EnableISX=false
UseAbsolutePaths=false

[Setup]
AppName=Programmers Notepad
AppVerName=Programmers Notepad 2 0.5.6
AppPublisher=Simon Steele
AppPublisherURL=http://www.pnotepad.org/
AppSupportURL=http://www.pnotepad.org/
AppUpdatesURL=http://www.pnotepad.org/
DefaultDirName={pf}\Programmers Notepad
DefaultGroupName=Programmers Notepad
LicenseFile=..\doc\license.txt
OutputDir=output
InternalCompressLevel=ultra
SolidCompression=true
VersionInfoVersion=2.0.5.6
VersionInfoCompany=Echo Software (Simon Steele)
VersionInfoDescription=Programmers Notepad 2 0.5.6 Setup
VersionInfoTextVersion=2.0.5.6
AppCopyright=Simon Steele
InfoBeforeFile=..\doc\readme.txt
AppMutex={{FCA6FB45-3224-497a-AC73-C30E498E9ADA}
DisableStartupPrompt=true
ShowLanguageDialog=no
WizardImageFile=sidebar.bmp
AppVersion=2.0.5.6
AppID={{52CF142B-7B0E-41E7-98F5-B834122523E7}
AppReadmeFile={app}\readme.txt
UninstallDisplayIcon={app}\pn.exe
UninstallDisplayName=Programmers Notepad 2
WizardSmallImageFile=C:\Source\pnwtl\installer\small.bmp
Compression=lzma/ultra

[Tasks]
Name: desktopicon; Description: Create a &desktop icon; GroupDescription: Additional icons:
Name: quicklaunchicon; Description: Create a &Quick Launch icon; GroupDescription: Additional icons:; Flags: unchecked

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: ..\bin\pn.exe; DestDir: {app}; Flags: ignoreversion

Source: ..\bin\dbghelp.dll; DestDir: {app}
Source: ..\bin\libexpat.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\bin\SciLexer.dll; DestDir: {app}; Flags: ignoreversion

Source: ..\bin\schemes\*.scheme; DestDir: {app}\schemes
Source: ..\bin\schemes\*.lexer; DestDir: {app}\schemes
Source: ..\bin\schemes\extmap.dat; DestDir: {app}\schemes
Source: ..\bin\clips\*.clips; DestDir: {app}\clips

Source: ..\bin\taggers\ctagsnavigator.dll; DestDir: {app}\taggers; Flags: ignoreversion
Source: ..\bin\ctags.exe; DestDir: {app}; Flags: ignoreversion

Source: ..\doc\readme.txt; DestDir: {app}; Flags: isreadme
Source: ..\doc\license.txt; DestDir: {app}
Source: ..\doc\license.html; DestDir: {app}
Source: ..\doc\credits.txt; DestDir: {app}
Source: ..\doc\roadmap.txt; DestDir: {app}
Source: ..\doc\pcre-license.txt; DestDir: {app}
Source: ..\doc\ctags_README; DestDir: {app}
Source: ..\doc\ctags_COPYING; DestDir: {app}

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

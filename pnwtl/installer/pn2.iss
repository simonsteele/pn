[Setup]
AppName=Programmer's Notepad
AppVerName=Programmer's Notepad {#PNVersion}
AppPublisher=Simon Steele
AppPublisherURL=http://www.pnotepad.org/
AppSupportURL=http://www.pnotepad.org/
AppUpdatesURL=http://www.pnotepad.org/
DefaultDirName={pf}\Programmer's Notepad
DefaultGroupName=Programmer's Notepad
LicenseFile=..\doc\license.txt
OutputDir=output
InternalCompressLevel=ultra
SolidCompression=true
VersionInfoVersion={#PNVersion}
VersionInfoCompany=Echo Software (Simon Steele)
VersionInfoDescription=Programmer's Notepad {#PNVersion} Setup
VersionInfoTextVersion={#PNVersion}
AppCopyright=Simon Steele
InfoBeforeFile=..\doc\readme.txt
AppMutex={{FCA6FB45-3224-497a-AC73-C30E498E9ADA}
DisableStartupPrompt=true
ShowLanguageDialog=no
WizardImageFile=sidebar.bmp
AppVersion={#PNVersion}
AppID={{52CF142B-7B0E-41E7-98F5-B834122523E7}
UninstallDisplayIcon={app}\pn.exe
UninstallDisplayName=Programmer's Notepad 2
WizardSmallImageFile=small.bmp
Compression=lzma/ultra
OutputBaseFilename=pnsetup

[Tasks]
Name: desktopicon; Description: Create a &Desktop icon; GroupDescription: Additional icons:
Name: quicklaunchicon; Description: Create a &Quick Launch icon; GroupDescription: Additional icons:; Flags: unchecked; OnlyBelowVersion: 0,6.01.7000

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: ..\bin\pn.exe; DestDir: {app}; Flags: ignoreversion

Source: ..\bin\pnse.dll; DestDir: {app}; Flags: ignoreversion restartreplace uninsrestartdelete; Tasks: 

Source: configs\default\config.xml; DestDir: {app}; Flags: onlyifdoesntexist

Source: ..\bin\dbghelp.dll; DestDir: {app}
;Source: ..\bin\libexpat.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\bin\libexpatw.dll; DestDir: {app}; Flags: ignoreversion

Source: ..\bin\SciLexer.dll; DestDir: {app}; Flags: ignoreversion

Source: ..\bin\schemes\*.scheme; DestDir: {app}\schemes
Source: ..\bin\schemes\*.lexer; DestDir: {app}\schemes
Source: ..\bin\schemes\*.api; DestDir: {app}\schemes
Source: ..\bin\schemes\extmap.dat; DestDir: {app}\schemes
Source: ..\bin\clips\*.clips; DestDir: {app}\clips
Source: ..\bin\presets\*.xml; DestDir: {app}\presets

Source: ..\bin\ctagsnavigator.dll; DestDir: {app}; Flags: ignoreversion
Source: ..\bin\ctags\*.*; DestDir: {app}\ctags; Flags: ignoreversion
Source: ..\bin\ctags.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\doc\help\pn2.chm; DestDir: {app}

Source: ..\doc\readme.txt; DestDir: {app}
Source: ..\doc\license.txt; DestDir: {app}
Source: ..\doc\credits.txt; DestDir: {app}
Source: ..\doc\roadmap.txt; DestDir: {app}
Source: ..\doc\ctags_README; DestDir: {app}
Source: ..\doc\ctags_COPYING; DestDir: {app}

;Source: ..\userschemes\*.schemedef; DestDir: {app}\schemes
Source: ..\schemes\user\*.schemedef; DestDir: {app}\samples

;Source: reqfiles\msvcp71.dll; DestDir: {app}
;Source: reqfiles\msvcr71.dll; DestDir: {app}

Source: reqfiles\Microsoft.VC90.CRT\Microsoft.VC90.CRT.manifest; DestDir: {app}\Microsoft.VC90.CRT; MinVersion: 0,5.01.2600
Source: reqfiles\Microsoft.VC90.CRT\msvcm90.dll; DestDir: {app}\Microsoft.VC90.CRT; MinVersion: 0,5.01.2600
Source: reqfiles\Microsoft.VC90.CRT\msvcp90.dll; DestDir: {app}\Microsoft.VC90.CRT; MinVersion: 0,5.01.2600
Source: reqfiles\Microsoft.VC90.CRT\msvcr90.dll; DestDir: {app}\Microsoft.VC90.CRT; MinVersion: 0,5.01.2600

Source: reqfiles\Microsoft.VC90.CRT\msvcp90.dll; DestDir: {app}; OnlyBelowVersion: 0,5.01.2600; Flags: ignoreversion
Source: reqfiles\Microsoft.VC90.CRT\msvcr90.dll; DestDir: {app}; OnlyBelowVersion: 0,5.01.2600; Flags: ignoreversion

[Icons]
Name: {group}\Programmer's Notepad 2; Filename: {app}\pn.exe
Name: {userdesktop}\Programmer's Notepad; Filename: {app}\pn.exe; Tasks: desktopicon
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Programmer's Notepad; Filename: {app}\pn.exe; Tasks: quicklaunchicon

[Run]
Filename: {app}\pn.exe; Description: Launch Programmer's Notepad; Flags: nowait postinstall skipifsilent; OnlyBelowVersion: 0,6
Filename: {app}\pn.exe; Description: Update Extensions; Parameters: --findexts; WorkingDir: {app}; StatusMsg: Updating Extension Registration; Flags: runhidden runasoriginaluser; Tasks: 

[_ISTool]
EnableISX=false
UseAbsolutePaths=false

[Registry]
Root: HKCU; SubKey: Software\Classes\SystemFileAssociations\text\shell\edit.PN2; ValueType: string; Flags: uninsdeletekey dontcreatekey
Root: HKCU; SubKey: Software\Classes\SystemFileAssociations\text\OpenWithList\pn.exe; ValueType: string; Flags: uninsdeletekey dontcreatekey
[Dirs]
Name: {app}\Microsoft.VC90.CRT
[InstallDelete]
Name: {app}\taggers\ctagsnavigator.*; Type: files; Tasks: 

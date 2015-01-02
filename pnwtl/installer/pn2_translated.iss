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
VersionInfoCompany=pnotepad.org (Simon Steele)
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
UninstallDisplayName=Programmer's Notepad
WizardSmallImageFile=small.bmp
Compression=lzma/ultra
OutputBaseFilename=pnsetuptrans

[Tasks]
Name: desktopicon; Description: Create a &Desktop icon; GroupDescription: Additional icons:
Name: quicklaunchicon; Description: Create a &Quick Launch icon; GroupDescription: Additional icons:; Flags: unchecked; OnlyBelowVersion: 0,6.01.7000

[Files]
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: "..\bin\pn.exe"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\bin\pnse.dll"; DestDir: "{app}"; Flags: ignoreversion restartreplace uninsrestartdelete

Source: "configs\default\config.xml"; DestDir: "{app}"; Flags: onlyifdoesntexist

Source: "..\bin\dbghelp.dll"; DestDir: "{app}"
Source: "..\bin\libexpatw.dll"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\bin\SciLexer.dll"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\bin\schemes\*.scheme"; DestDir: "{app}\schemes"
Source: "..\bin\schemes\*.api"; DestDir: "{app}\schemes"
Source: "..\bin\schemes\extmap.dat"; DestDir: "{app}\schemes"
Source: "..\bin\clips\*.clips"; DestDir: "{app}\clips"; Flags: recursesubdirs
Source: "..\bin\presets\*.xml"; DestDir: "{app}\presets"

Source: "..\bin\ctagsnavigator.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\customscheme.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\ctags\*.*"; DestDir: "{app}\ctags"; Flags: ignoreversion
Source: "..\bin\ctags.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\doc\help\pn2.chm"; DestDir: "{app}"

Source: "..\doc\readme.txt"; DestDir: "{app}"
Source: "..\doc\license.txt"; DestDir: "{app}"
Source: "..\doc\credits.txt"; DestDir: "{app}"
Source: "..\doc\roadmap.txt"; DestDir: "{app}"
Source: "..\doc\ctags_README"; DestDir: "{app}"
Source: "..\doc\ctags_COPYING"; DestDir: "{app}"

Source: "..\schemes\user\*.schemedef"; DestDir: "{app}\samples"

Source: "reqfiles\msvcp120.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "reqfiles\msvcr120.dll"; DestDir: "{app}"; Flags: ignoreversion

;Translations
Source: "..\bin\pnlang_*.dll"; DestDir: "{app}"; Flags: ignoreversion

;PyPN
Source: "..\bin\pypn.dll"; DestDir: "{app}"; Flags: ignoreversion; Components: PyPN
Source: "..\bin\boost_python-vc120-mt-1*.dll"; DestDir: "{app}"; Flags: ignoreversion; Components: PyPN
Source: "..\pypn\scripts3k\*.*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs; Components: PyPN

[Icons]
Name: {group}\Programmer's Notepad; Filename: {app}\pn.exe
Name: {commondesktop}\Programmer's Notepad; Filename: {app}\pn.exe; Tasks: desktopicon
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Programmer's Notepad; Filename: {app}\pn.exe; Tasks: quicklaunchicon

[Run]
Filename: {app}\pn.exe; Description: Launch Programmer's Notepad; Flags: nowait postinstall skipifsilent; OnlyBelowVersion: 0,6
Filename: {app}\pn.exe; Description: Update Extensions; Parameters: --findexts; WorkingDir: {app}; StatusMsg: Updating Extension Registration; Flags: runhidden runasoriginaluser; Tasks: 
Filename: {app}\pn.exe; Description: Clean Schemes; Parameters: --cleancschemes; WorkingDir: {app}; StatusMsg: Compiling Scheme Definitions; Flags: runhidden runasoriginaluser; Tasks: 

[_ISTool]
EnableISX=false
UseAbsolutePaths=false

[Registry]
Root: HKCU; SubKey: Software\Classes\SystemFileAssociations\text\shell\edit.PN2; ValueType: string; Flags: uninsdeletekey dontcreatekey
Root: HKCU; SubKey: Software\Classes\SystemFileAssociations\text\OpenWithList\pn.exe; ValueType: string; Flags: uninsdeletekey dontcreatekey

[InstallDelete]
Name: {app}\taggers\ctagsnavigator.*; Type: files; Tasks: 
Name: {app}\schemes\customscheme.lexer; Type: files; Tasks:
Name: {app}\schemes\powershell.schemedef; Type: files; Tasks:

[ThirdPartySettings]
CompileLogMethod=append

[Components]
Name: "PyPN"; Description: "Python Scripting Extension (Requires Python 3.4)"

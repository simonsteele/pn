{*******************************************************}
{                                                       }
{       HTML Help helper functin                        }
{                                                       }
{       Copyright (c) 1999  The Helpware Group          }
{                                                       }
{*******************************************************}

{
========================================================
  hh_funcs.pas
  Version 1.5
  Html Help helper functions

  Copyright (c) 1999 The Helpware Group
  Email: support@helpware.net
  Web: http://www.helpware.net
  Platform: Delphi 2, 3, 4, 5, ...
  Changes Notes: See hh_doc.txt

  New Changes by Simon Steele, marked with //Simon

========================================================
}
unit hh_funcs;

interface

uses Windows,   //This line will not compile under Delphi 1 -- D1 is not supported
     SysUtils, Classes, Forms, Dialogs, ShellApi, Registry, FileCtrl;


{ >> Create conditional symbols.
     Note: This module is Delphi 2/3/4/5/.. compatible

     VER90     - Predefined by Delphi 2 compiler.
     VER100    - Predefined by Delphi 3 compiler.
     D3PLUS    - Compiler is Delphi 3 or greater
     D4PLUS    - Compiler is Delphi 4 or greater
}

{$DEFINE D3PLUS}
{$DEFINE D4PLUS}

{$IFDEF VER90}        //Dephi 2
  {$UNDEF D3PLUS}
  {$UNDEF D4PLUS}
{$ENDIF}

{$IFDEF VER100}       //Dephi 3
  {$UNDEF D4PLUS}
{$ENDIF}


{ Host Type }
type THostType = (htHHAPI, htKeyHHexe, htHHexe);

{ HH comand line prefix}
type TPrefixType = (ptNone, ptIE3, ptIE4);

{Exports}
  procedure HHCloseAll;

  function HHDisplayTopic(aChmFile, aTopic, aWinDef: String; aHostType: THostType): Integer;
  function HHHelpContext(aChmFile: String; aContextID: DWord; aWinDef: String; aHostType: THostType): Integer;

  function HHTopic(aCHMPath: String; aHostType: THostType): Integer;
  function HHContext(aChmPath: String; aContextId: Integer; aHostType: THostType): Integer;
  function HHPopupHelp(CHMTextFile: String; StringID: Integer; XYPos: TPoint): HWND;

  function HHFormat(aChmFile, aTopic, aWinDef: String; aPrefixType: TPrefixType): String;
  procedure HHSlitCmdStr(s: String; var aChmFile, aTopic, aWinDef: String);  //typo kept for backward compatibility
  procedure HHSplitCmdStr(s: String; var aChmFile, aTopic, aWinDef: String);

  procedure HHShowError(err: Integer);


{Callbacks available for THookHelpSystem}
type
  THelpProcCallback1 = procedure (Data: Longint);
  THelpProcCallback2 = procedure (Data: Longint; X, Y: Integer);


{THookHelpSystem}
type
  THookHelpSystem = class(TObject)
  private
    FOldHelpEvent: THelpEvent;
    FChmFile: String;
    FWinDef: String;
    FHostType: THostType;
    FPopupXY: TPoint;
    function HelpHook(Command : Word; Data : Longint; Var CallHelp : Boolean) : Boolean;
  public
    {Optional callback funcs called when Help events come in}
    HelpCallback1: THelpProcCallback1;
    HelpCallback2: THelpProcCallback2;

    constructor Create(aDefChmFile, aDefWinDef: String; aHostType: THostType);
    destructor Destroy; override;

    function HelpContext(aContextId: DWord): Integer;
    function HelpTopic(aTopic: String): Integer;
    function HelpTopic2(aChmFile, aTopic, aWinDef: String): Integer;
    function HelpTopic3(aChmPath: String): Integer;

    property ChmFile: String read FChmFile write FChmFile;
    property WinDef: String read FWinDef write FWinDef;
    property HostType: THostType read FHostType write FHostType;
  end;


{ See Module initialization }
var
  { 'hhctrl.ocx' version info }
  _hhInstalled: Boolean = FALSE;          //Is Html Help 'hhctrl.ocx' installed
  _hhVerStr: String = '';                 //eg. '4.73.8252.1' or '' if not found
  _hhMajVer: word = 0;                    //eg. 4
  _hhMinVer: word = 0;                    //eg. 73
  _hhBuildNo: word = 0;                   //eg. 8252
  _hhSubBuildNo: word = 0;                //eg. 1
  _hhFriendlyVerStr: String = '';         //eg. '1.2'

  { 'Shdocvw.dll' version info }
  _ieInstalled: Boolean = FALSE;          //Is Internet Explorer Installed
  _ieVerStr: String = '';                 //eg. '5.00.0910.1309'
  _ieFriendlyVerStr: String = '';         //eg. 'Internet Explorer 5'

  { General }
  _RunDir: String = '';                   //applications run directory. Or Host EXE directory if part of DLL.
  _ModulePath: String;                    //If part of DLL this is the full path to the DLL
  _ModuleDir: String;                     //If part of DLL this is the DLL Dir and different from _RunDir
  _ModuleName: String;                    //If part of DLL this is the DLL name otherwise it is host exe name

  _DebugMode: Boolean = false;            //Set TRUE to enable debug file output. Or create a file 'debug.debug' in the rundir



{ Host Apps - Live in the Windows Dir }
const
  HOST_HHEXE = 'HH.EXE';
  HOST_KEYHHEXE = 'KeyHH.EXE';

{ HH comand line prefix}
const
  HH_PREFIX_IE4 = 'ms-its:';             //IE4 and above compatible command line prefix
  HH_PREFIX_IE3 = 'mk:@MSITStore:';      //IE3 and above compatible command line prefix


{ HH Errors }
const
  HH_ERR_AllOK              = 0;
  HH_ERR_HHNotInstalled     = 1;     //Html Help is not installed on this PC
  HH_ERR_KeyHHexeNotFound   = 2;     //KeyHH.EXE was not found in the Windows folder
  HH_ERR_HHexeNotFound      = 3;     //HH.EXE was not found in the Windows folder


{ exports - General functions }

procedure DosToUnix(var filename: String);
function StrPosC(const s: String; const find: String): Integer;
function StrPosI(const s: String; const find: String): Integer;
function StrRepC(var s: String;  const find, repl: String): Integer;
function StrRepI(var s: String;  const find, repl: String): Integer;
function StrRepCA(var s: String;  const find, repl: String): Integer;
function StrRepIA(var s: String;  const find, repl: String): Integer;
procedure StripL(var s: String; c: char);
procedure StripR(var s: String; c: char);
procedure StripLR(var s: String; c: char);
function MkStr(c: Char; count: Integer): String;
function BoolToYN(b: Boolean): String;

function GetWinDir: String;
function GetWinSysDir: String;
function GetWinTempDir: String;

function VerCompare(va1, va2, va3, va4, vb1, vb2, vb3, vb4: Word): Integer;
function GetFileVer(aFilename: String; var aV1, aV2, aV3, aV4: word): String;
function GetFileVerStr(aFilename: String): String;
function GetIEVer(var V1, V2, V3, V4: word): String;
function Check_HH_Version(x1, x2, x3, x4: Integer): Integer;
function Check_IE_Version(x1, x2, x3, x4: Integer): Integer;
function GetHHFriendlyVer: String;
function GetIEFriendlyVer: String;

function ShellExec(aFilename: String; aParams: String): Boolean;
function GetLastErrorStr: String;
function GetRegStr(rootkey: HKEY; const key, dataName: string): string;
procedure PutRegStr(rootkey: HKEY; const key, name, value: string);

procedure DebugOut(msgStr: String; const Args: array of const);
procedure DebugOut2(msgStr: String; const Args: array of const);
procedure ShowDebugFile;
procedure ResetDebugFile;
function IsDirWritable(aDir: String): Boolean;

procedure ReportError( errStr: String; const Args: array of const );


{$IFDEF D3PLUS} // -- Delphi >=3
resourcestring
{$ELSE}         // -- Delphi 2
const
{$ENDIF}

  //Error Strings
  st_HH_ERR_HHNotInstalled = 'MS Html Help is not installed on this PC.';
  st_HH_ERR_KeyHHexeNotFound = 'System file KeyHH.EXE was not found in the Windows folder.';
  st_HH_ERR_HHexeNotFound = 'System file HH.EXE was not found in the Windows folder.';
  st_HH_ERR_Unknown = 'Unknown error returned by HHHelpContext';

  //For GetLastError
  st_GLE_FileNotFound = 'File Not Found';
  st_GLE_PathNotFound = 'Path Not Found';
  st_GLE_AccessDenied = 'Access Denied';
  st_GLE_InsufficientMemory = 'Insufficient Memory';
  st_GLE_MediaIsWriteProtected = 'Media Is Write Protected';
  st_GLE_DeviceNotReady = 'Device Not Ready';
  st_GLE_FileInUse = 'File In Use';
  st_GLE_DiskFull = 'Disk Full';
  st_GLE_WindowsVersionIncorrect = 'Windows Version Incorrect';
  st_GLE_NotAWindowsOrMSDosProgram = 'Not A Windows Or MSDos Program';
  st_GLE_CorruptFileOrDisk = 'Corrupt File Or Disk';
  st_GLE_CorruptRegistry = 'Corrupt Registry';
  st_GLE_GeneralFailure = 'General Failure';

{Debug}
  var DBG_FILENAME: String = '\HHDebug.txt';
  var DBG_DIR: String = '';

implementation

uses
  hh;  //HH API

{---------------------------------------------------------------------]
  Hook Help System

  Delphi allows you to trap all help calls and redirect them
  to your own handler. Thus we get Html Help working under D3/4.

  Usage:

    var mHHelp: THookHelpSystem;

    procedure TMainForm.FormCreate(Sender: TObject);
    begin
      //Set CHM file, Window Definition to use if reqired and Mode of operation
      mHHelp := THookHelpSystem.Create(pathToCHM, '', htHHAPI);
      ...

    procedure TMainForm.FormDestroy(Sender: TObject);
    begin
      //Unhook and free
      mHHelp.Free;
      ...

  Show help in the normal way
  o Set "Form.HelpContext := xx" to display page sensitive help via F1 key.
  o Set "Control.HelpContext := xx" to display field sensitive help via F1 and "whats this" help.
  o Call Application.HelpContext(xx) to show help directly from a memu or help button.
  o Make sure that Topic xx, xx is a context ID, is defined in the CHM help file.
  eg. Application.HelpContext(1133)

  To display a topic by topic filename use
  mHHelp.HelpTopic('index.html');

[---------------------------------------------------------------------}

constructor THookHelpSystem.Create(aDefChmFile, aDefWinDef: String; aHostType: THostType);
begin
  inherited Create;
  FChmFile := aDefChmFile;
  FWinDef := aDefWinDef;
  FHostType := aHostType;

  {Hook in our help}
  FOldHelpEvent := Application.OnHelp;
  Application.OnHelp := HelpHook;

  {Debug}
  if _DebugMode then
    DebugOut('THookHelpSystem.Create("%s","%s", %d)', [aDefChmFile, aDefWinDef, ord(aHostType)]);
end; { THookHelpSystem.Create }

destructor THookHelpSystem.Destroy;
begin
  {Must call this or get access violation}
  if FHostType = htHHAPI then
    hh_funcs.HHCloseAll;

  {Unhook our help}
  Application.OnHelp := FOldHelpEvent;
  inherited destroy;
  if _DebugMode then
    DebugOut('THookHelpSystem.Destroy',['']);
end; { THookHelpSystem.Destroy }

{ Debug aid - Commands to pass to WinHelp() }
function WinHelpCmdToStr(cmd: Integer): string;
begin
  case cmd of
    HELP_CONTEXT: result := 'HELP_CONTEXT';       { Display topic in ulTopic  }
    HELP_QUIT: result := 'HELP_QUIT';            { Terminate help  }
    HELP_INDEX: result := 'HELP_INDEX or HELP_CONTENTS';         { Display index  }
    HELP_HELPONHELP: result := 'HELP_HELPONHELP';    { Display help on using help  }
    HELP_SETINDEX: result := 'HELP_SETINDEX or HELP_SETCONTENTS';      { Set current Index for multi index help  }
    HELP_CONTEXTPOPUP: result := 'HELP_CONTEXTPOPUP';
    HELP_FORCEFILE: result := 'HELP_FORCEFILE';
    HELP_KEY: result := 'HELP_KEY';         { Display topic for keyword in offabData  }
    HELP_COMMAND: result := 'HELP_COMMAND';
    HELP_PARTIALKEY: result := 'HELP_PARTIALKEY';
    HELP_MULTIKEY: result := 'HELP_MULTIKEY';
    HELP_SETWINPOS: result := 'HELP_SETWINPOS';
    HELP_CONTEXTMENU: result := 'HELP_CONTEXTMENU';
    HELP_FINDER: result := 'HELP_FINDER';
    HELP_WM_HELP: result := 'HELP_WM_HELP';
    HELP_SETPOPUP_POS: result := 'HELP_SETPOPUP_POS';
  else result := '??';
  end;
  result := inttostr(cmd) + ' (' + result +')';
end;


{ All application help calls to help come here }
function THookHelpSystem.HelpHook(Command: Word; Data: Longint; Var CallHelp: Boolean) : Boolean;
begin
   if _DebugMode then
     DebugOut('THookHelpSystem.HelpHook(%s, %d)',[WinHelpCmdToStr(Command), Data]);

   CallHelp := false;
   case Command of
    Help_Context:      //help button
      begin
        if Assigned(HelpCallback1)
          then HelpCallback1(Data)           //Call back
          else Self.HelpContext( Data );     //Call help
      end;
    HELP_SETPOPUP_POS: //call #1 of F1 Popup (Whats This) help
      FPopupXY := SmallPointToPoint(TSmallPoint(Data));           //data = x,y pos for popup
    Help_ContextPopup: //call #2 of F1 Popup (Whats This) help
      begin
        if Assigned(HelpCallback2)
          then HelpCallback2(Data, FPopupXY.X, FPopupXY.Y)   //Call back
          else Self.HelpContext(Data);                       //Call help
      end
    else
      CallHelp := TRUE; //Default handling - WinHelp
  end;
  result := TRUE;
end; { THookHelpSystem.HelpHook }


{ No need to call this directly. Instead call Application.HelpContext(xx) and it will call this
  function because of the hook we have installed.
  Uses ChmFile, WinDef & Hosttype specified by create}
function THookHelpSystem.HelpContext(aContextId: DWord): Integer;
begin
  // result := HHHelpContext(FChmFile, aContextId, FWinDef, FHostType); //SIMON
  Result := 0;
  HHPopupHelp(FChmFile + '::/cshelp.txt', aContextId, FPopupXY);
  // HHShowError(result); //SIMON
end;

{Show a help topic - 1
 Uses ChmFile, Topic, WinDef & HostType specified by create}
function THookHelpSystem.HelpTopic(aTopic: String): Integer;
begin
  result := HHDisplayTopic(FChmFile, aTopic, FWinDef, FHostType);
  HHShowError(result);
end;

{Show a help topic - 2
 overrides default Chm and WinDef - still uses initially specified Host Type}
function THookHelpSystem.HelpTopic2(aChmFile, aTopic, aWinDef: String): Integer;
begin
  result := HHDisplayTopic(aChmFile, aTopic, aWinDef, FHostType);
end;

{Show a help topic - 3
 overrides default Chm and WinDef - Specify a full path EG. c:\help\help.chm::/htm/topic.htm}
function THookHelpSystem.HelpTopic3(aChmPath: String): Integer;
begin
  Result := HHTopic(aCHMPath, FHostType);
end;


{ Show Error }
procedure HHShowError(err: Integer);
var s: String;
begin
  case err of
    HH_ERR_AllOK:            s := '';
    HH_ERR_HHNotInstalled:   s := st_HH_ERR_HHNotInstalled;
    HH_ERR_KeyHHexeNotFound: s := st_HH_ERR_KeyHHexeNotFound;
    HH_ERR_HHexeNotFound:    s := st_HH_ERR_HHexeNotFound;
    else                     s := st_HH_ERR_Unknown;
  end;
  if s <> '' then
  begin
    MessageDlg(s, mtWarning, [mbOK], 0);
    if _DebugMode then
      DebugOut('HHShowError(%d), "%s"',[err, s]);
  end;
end;


{---------------------------------------------------------------------]
   HH Functions
[---------------------------------------------------------------------}

{ Call HHCloseAll if you are calling help using the HH API.
  It will all tell HH API to close all HH Windows opened by this application.

 Warning: if you are calling HH API function to display help you must call this before
 application shutdown or your application will crash}
procedure HHCloseAll;
begin
  HH.HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
end;

{ HHDisplayTopic()
  Display a Topic from the CHM file using a Window Definition
    aChmFile: In
      Name of compressed help file to display.
      Generally this should be full path as NT is less forgiving with relative paths.
    aTopic: In
      Path to html file in Chm file.
      Leave blank to display open the Chm at the default page
    aWinDef: In
      Specify a window definition. Leading slash will be added if missing.
      Leave blank to display open the Chm with the default window definition
      Note: not supported by some versions of HH.EXE and KeyHH.EXE
    aHostType: In
      Who will host the HH Window
      - htHHAPI:  This application will host via API calls.
      - htKeyHHexe:  Windows KeyHH.EXE will.
      - htHHexe:   Windows HH.EXE will.
    Returns:
      Possible returns
       0 = All OK
       HH_ERR_HHNotInstalled
       HH_ERR_KeyHHexeNotFound (aHostType = htKeyHHexe)
       HH_ERR_HHexeNotFound (aHostType = htHHexe)
  Other Info
      - No checking is done on any of the params.
        Caller should first verify that the chmFile exists.
  Example:
      HHDisplayTopic('windows.chm','about_magnify.htm','windefault', htHHAPI);
}
function HHDisplayTopic(aChmFile, aTopic, aWinDef: String; aHostType: THostType): Integer;
var target: String;
begin
  //Showmessage(format('%s, %s, %s, %d',[aChmFile, aTopic, aWinDef, ord(aHostType)]));
  if _DebugMode then
    DebugOut('HHHelpContext("%s", %s, "%s", %d)', [aChmFile, aTopic, aWinDef, Ord(aHostType)]);

  if aHostType = htHHexe then  //Prefix required by early versions - use IE3 prefix
    target := HHFormat(aChmFile, aTopic, aWinDef, ptIE3)
  else                         //No prefix needed
    target := HHFormat(aChmFile, aTopic, aWinDef, ptNone);
  result := HHTopic( target, aHostType );
end;

{
   HHTopic()
   Same as above except aCHMPath may be a combination
   chmfile, Topic, WinDef in the form chmfile::/Topic>WinDef
   Note: HH.EXE normally requires a path prefix.
}
function HHTopic(aCHMPath: String; aHostType: THostType): Integer;
var appPath: String; h: HWND;
begin
  if _DebugMode then
    DebugOut('ShowTopic("%s", %d)', [aChmPath, Ord(aHostType)]);
  result := HH_ERR_AllOK;  {0}

  { Check HH Installed on this PC }
  if not _hhInstalled then
    result := HH_ERR_HHNotInstalled
  else
  case aHostType of

    //Host Type = This app using HH API
    htHHAPI:
      begin
        h := HH.HtmlHelp(GetDesktopWindow, PChar(aCHMPath), HH_DISPLAY_TOPIC, 0);
        if h > 0 then
          SetForegroundWindow(h);
      end;

    //Host Type = KeyHH.EXE (must be installed)
    htKeyHHexe:
      begin
        appPath := GetWinDir + '\' + HOST_KEYHHEXE;
        if not FileExists(appPath) then
          result := HH_ERR_KeyHHexeNotFound
        else
        begin
          { Pass the parameters to KeyHH.exe using "-win" for single window.
            hh path prefix is not required by KeyHH.EXE
          }
          ShellExec(appPath, '-win ' + aCHMPath);
        end;
      end;

    //Host Type = HH.EXE (part of Html Help)
    htHHexe:
      begin
        appPath := GetWinDir + '\' + HOST_HHEXE;
        if not FileExists(appPath) then
          result := HH_ERR_HHexeNotFound
        else
        begin
          { HH.EXE requires a prefix. }
          ShellExec(appPath, aCHMPath);
        end;
      end;
  end; {case}
  if _DebugMode then
    DebugOut('  returned - %d', [result]);
end;




{ HHHelpContext()
  Displays a help topic based on a mapped topic ID.

  Function documentation is the same as above except replace "aTopic" by...

    aContextID
      Specifies the numeric ID of the topic to display

  returns same errors

  Example:
     HHHelpContext('windows.chm',200,'windefault', htHHAPI);
}
function HHHelpContext(aChmFile: String; aContextID: DWord; aWinDef: String; aHostType: THostType): Integer;
var target: String;
begin
  if _DebugMode then
    DebugOut('HHHelpContext("%s", %d, "%s", %d)', [aChmFile, aContextID, aWinDef, Ord(aHostType)]);
  if aHostType = htHHexe //Prefix required by early versions - use IE3 prefix
    then target := HHFormat(aChmFile, '', aWinDef, ptIE3)
    else target := HHFormat(aChmFile, '', aWinDef, ptNone);  //No prefix needed
  result := HHContext( target, aContextID, aHostType );
end;

{Show HH Popup using a string resource}
function HHPopupHelp(CHMTextFile: String; StringID: Integer; XYPos: TPoint): HWND;
var hhpopup: HH.THHPopup;
begin
  with hhpopup do
  begin
    cbStruct := sizeof(hhpopup);     // sizeof this structure
    hinst := 0;                      // instance handle for string resource
    idString := StringID;            // Specifies zero, a resource ID, or a topic number in a text file.
    pszText := nil;                  // Specifies the text to display if idString is zero.
    pt := XYPos;                     // top center (in pixels) of popup window
    clrForeground := COLORREF(-1);   // use -1 for default - RGB value (red)
    clrBackground := COLORREF(-1);   // use -1 for default - RGB value
    rcMargins  := Rect(-1,-1,-1,-1); // amount of space between edges of window and text, -1 for each member to ignore
    pszFont := '';                  // facename, point size, char set, BOLD ITALIC UNDERLINE
  end;
  Result := HtmlHelp(GetDesktopWindow, PChar(CHMTextFile), HH_DISPLAY_TEXT_POPUP, DWORD(@hhpopup));
end;


{
   HHContext()
   Same as above except aCHMPath may be a combination
   chmfile & WinDef in the form chmfile>WinDef
   Note: HH.EXE does not support context mapped help - use KeyHH.exe instead
}
function HHContext(aChmPath: String; aContextId: Integer; aHostType: THostType): Integer;
var appPath: String; h: HWND;
begin
  if _DebugMode then
    DebugOut('ShowContext("%s", %d)', [aChmPath, Ord(aHostType)]);
  result := HH_ERR_AllOK;  {0}

  { Check HH Installed on this PC }
  if not _hhInstalled then
    result := HH_ERR_HHNotInstalled
  else
  case aHostType of

    //Host Type = This app using HH API
    htHHAPI:
      begin
        h := HH.HtmlHelp(GetDesktopWindow, PChar(aChmPath), HH_HELP_CONTEXT, aContextID);
        if h > 0 then
          SetForegroundWindow(h);
      end;

    //Host Type = KeyHH.EXE (must be installed)
    htKeyHHexe:
      begin
        appPath := GetWinDir + '\' + HOST_KEYHHEXE;
        if not FileExists(appPath) then
          result := HH_ERR_KeyHHexeNotFound
        else
        begin
          { Pass the parameters to KeyHH.exe
            using "-win" for single window and "-#mapid xx " for the context
            hh path prefix is not required by KeyHH.EXE
          }
          ShellExec(appPath, '-win -#mapid ' + IntToStr(aContextID) + ' ' + aChmPath);
        end;
      end;

    //Host Type = HH.EXE (part of Html Help)
    htHHexe:
      begin
        appPath := GetWinDir + '\' + HOST_HHEXE;
        if not FileExists(appPath) then
          result := HH_ERR_HHexeNotFound
        else
          ShellExec(appPath, '-mapid ' + IntToStr(aContextID) + ' ' + aChmPath);
      end;

  end; {case}
  if _DebugMode then
    DebugOut('  returned - %d', [result]);
end;



{
  This creates a command line suitable for use with HH.EXE, KeyHH or HHServer.EXE
    chmFile:
       Name of CHM file. Full or relative path.
    Topic:
       Html filename in Chm. Can be blank.
       Under IE4 this can include a bookmark.
    WinDef:
       Window Definition to use. Can be blank.
    aPrefixType:
       What to prefix string to add
       ptNone - No Prefix added
       ptIE3 - IE3 and above compatible prefix added - 'mk:@MSITStore:'
       ptIE4 - IE4 and above compatible prefix added - 'ms-its:'
  Result examples
    HHFormat('windows.chm', 'about_magnify.htm', 'windefault', ptIE3);
    => 'mk:@MSITStore:windows.chm::/about_magnify.htm>windefault'

    chmFile.chm
    chmFile.chm>WinDef
    Helpfile.chm::/Topic.htm>WinDef
    ms-its:chmFile.chm>WinDef
    mk:@MSITStore:Helpfile.chm::/Topic.htm>WinDef

}
function HHFormat(aChmFile, aTopic, aWinDef: String; aPrefixType: TPrefixType): String;
begin
  //  Rename all %20 to space
  StrRepCA( aChmFile, '%20', ' ');
  StrRepCA( aTopic, '%20', ' ');
  StrRepCA( aWinDef, '%20', ' ');

  StripLR(aChmFile, ' ');   StripLR(aTopic, ' ');   StripLR(aWinDef, ' ');  //no lead trail spaces

  {make chm and topic}
  if aTopic = '' then
    result := aChmFile
  else
  begin
    DosToUnix(aTopic);                    //Topics should always contain '/' unix slashes
    if aTopic[1] <> '/' then              //we want a leading slash
      aTopic := '/' + aTopic;
    result := aTopic;
    if aChmFile <> '' then                //Allow no chmfile so we can format the topic
      result := aChmFile + '::' + result
  end;

  {add win definition}
  if aWinDef <> '' then
    result := result + '>' + aWinDef;

  {add prefix}
  case aPrefixType of
    ptIE3: result := HH_PREFIX_IE3 + result;
    ptIE4: result := HH_PREFIX_IE4 + result;
  end;
end;


{
  Given a string s like
    mk:@MSITStore:aChmFile::aTopic>aWinDef
  eg.
    chmFile.chm
    chmFile.chm>WinDef
    Helpfile.chm::/Topic.htm>WinDef
    ms-its:chmFile.chm>WinDef
    mk:@MSITStore:Helpfile.chm::/Topic.htm>WinDef
  return the components
    aChmFile, aTopic, aWinDef
}
//Backward compatible Fix - Typo - Should be Split not Slit
procedure HHSlitCmdStr(s: String; var aChmFile, aTopic, aWinDef: String);
begin
  HHSplitCmdStr(s, aChmFile, aTopic, aWinDef);
end;

procedure HHSplitCmdStr(s: String; var aChmFile, aTopic, aWinDef: String);
var i: Integer;
begin
   //  Replace all %20 to space
   StrRepCA( s, '%20', ' ');

   {Get WinDef}
   i := StrPosC(s, '>');
   if i > 0 then
   begin
     aWinDef := Copy(s, i+1, Maxint);
     SetLength(s, i-1);
   end;

   {Get Topic}
   i := StrPosC(s, '::');
   if i > 0 then
   begin
     aTopic := Copy(s, i+2, Maxint);
     SetLength(s, i-1);
     DosToUnix(aTopic);                    //Topics should always contain '/' unix slashes
   end;

   {Get chmFile}
   i := StrPosI(s, 'its:'); //'ms-its:'
   if i > 0 then
     aChmFile := Copy(s, i+length('its:'), Maxint)
   else
   begin
     i := StrPosI(s, 'store:');  //'mk:@MSITStore:'
     if i > 0 then
       aChmFile := Copy(s, i+length('store:'), Maxint)
     else
       aChmFile := s;
   end;

   StripLR(aChmFile, ' ');
   StripLR(aTopic, ' ');
   StripLR(aWinDef, ' ');
end;



{---------------------------------------------------------------------]
   General Functions
[---------------------------------------------------------------------}


{ Sometimes safest to work in Unix / slashes }
procedure DosToUnix(var filename: String);
begin
  repeat until StrRepC(filename, '\', '/') = 0;
end;

{Find pos of sub string in string. Case Sensitive - returns 0 not found or 1..n}
function StrPosC(const s: String; const find: String): Integer;
var p: PChar;
begin
{$IFDEF D3PLUS} // -- Delphi >=3
  p := AnsiStrPos( PChar(s) , PChar(find) );   //double byte safe
{$ELSE}         // -- Delphi 2
  p := StrPos( PChar(s) , PChar(find) );   //double byte safe
{$ENDIF}
  if p = nil then
    result := 0
  else
    result := p - PChar(s) + 1;
end;

{Same as Above only ignores case}
function StrPosI(const s: String; const find: String): Integer;
var s2, find2: String;
begin
{$IFDEF D3PLUS} // -- Delphi >=3
  s2 := AnsiUpperCase(s);
  find2 := AnsiUpperCase(find);
{$ELSE}         // -- Delphi 2
  s2 := UpperCase(s);
  find2 := UpperCase(find);
{$ENDIF}

  result := StrPosC(s2, find2);
end;


{returns pos where subString replacements was done - 0 = none done - Case Sensitive}
function StrRepC( var s: String;  const find, repl: String): Integer;
begin
  result := StrPosC(s, find);
  if result > 0 then     {found - replace}
  begin
    Delete( s, result, Length(find) );
    Insert( repl, s, result );
  end;
end;

{returns pos where subString replacements was done - 0 = none done - Ignore Sensitive}
function StrRepI( var s: String;  const find, repl: String): Integer;
begin
  result := StrPosI(s, find);
  if result > 0 then     {found - replace}
  begin
    Delete( s, result, Length(find) );
    Insert( repl, s, result );
  end;
end;


{Replace all ocurrences (Ignore Case) - returns replacements done}
function StrRepIA( var s: String;  const find, repl: String): Integer;
begin
  result := 0;
  repeat
    if StrRepI(s, find, repl) > 0 then
      inc(result)
    else
       break;
  until false;
end;

{Replace all ocurrences (Case Sensitive) - returns replacements done}
function StrRepCA( var s: String;  const find, repl: String): Integer;
begin
  result := 0;
  repeat
    if StrRepC(s, find, repl) > 0 then
      inc(result)
    else
       break;
  until false;
end;

{Strip leading chars}
procedure StripL(var s: String; c: char);
begin
  while (s <> '') and (s[1] = c) do
    Delete(s, 1, 1);
end;

{Strip trailing chars}
procedure StripR(var s: String; c: char);
var p: PChar;
begin
{$IFDEF D3PLUS} // -- Delphi >=3
  repeat
    p := AnsiLastChar(S);    //nil if S = empty
    if (p <> nil) and (p = c) then
      SetLength(s, Length(s)-1)
    else
      break;
  until p = nil;
{$ELSE}         // -- Delphi 2
  repeat
    if (s <> '') and (s[length(s)] = c) then
      SetLength(s, Length(s)-1)
    else
      break;
  until FALSE;
{$ENDIF}
end;


{Strip leading and trailing chars}
procedure StripLR(var s: String; c: char);
begin
  StripL(s, c);
  StripR(s, c);
end;

{Make string of chars}
function MkStr(c: Char; count: Integer): String;
var i: Integer;
begin
  result := '';
  for i := 1 to count do
    result := result + c;
end;

{ Boolean to Yes / No }
function BoolToYN(b: Boolean): String;
begin
  if b then result := 'YES' else result := 'NO';
end;


{Return Windows Dir}
function GetWinDir: String;
var path: array[0..260] of Char;
begin
  GetWindowsDirectory(path, SizeOf(path));
  result := path;
  StripR(result, '\');  //no trailing slash
end;

{Return Windows System Dir}
function GetWinSysDir: String;
var path: array[0..260] of Char;
begin
  GetSystemDirectory(path, SizeOf(path));
  result := path;
  StripR(result, '\');  //no trailing slash
end;

{Get Windows Temp Dir - with no trailing slash}
function GetWinTempDir: String;
var dwLen: DWORD;
begin
  SetLength(result, 300);
  dwLen := GetTempPath(300, @result[1]);
  SetLength(result, dwLen);

  //problems
  if DirectoryExists(result) = FALSE then
    result := 'c:';
  StripR(result, '\');  //no trailing slash
end;

{
  Get the product version number from a file (exe, dll, ocx etc.)
  Return '' if info not available - eg. file not found
  eg. Returns '7.47.3456.0', aV1=7, aV2=47, aV3=3456 aV4=0
  ie. major.minor.release.build
}
function GetFileVer(aFilename: String; var aV1, aV2, aV3, aV4: word): String;
var  InfoSize: DWORD; Wnd: DWORD; VerBuf: Pointer; VerSize: DWORD; FI: PVSFixedFileInfo;
begin
  result := '';
  aV1 := 0;  aV2 := 0;  aV3 := 0;  aV4 := 0;
  if (aFilename = '') or (not FileExists(aFilename)) then exit;  //don't continue if file not found

  InfoSize := GetFileVersionInfoSize(PChar(aFilename), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(aFilename), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          aV1 := HiWord(FI^.dwFileVersionMS);
          aV2 := LoWord(FI^.dwFileVersionMS);
          aV3 := HiWord(FI^.dwFileVersionLS);
          aV4 := LoWord(FI^.dwFileVersionLS);
          result := IntToStr( HiWord(FI^.dwFileVersionMS) ) + '.' +
                    IntToStr( LoWord(FI^.dwFileVersionMS) ) + '.' +
                    IntToStr( HiWord(FI^.dwFileVersionLS) ) + '.' +
                    IntToStr( LoWord(FI^.dwFileVersionLS) );
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end; //GetFileVer


{ Same as above but only returns version string }
function GetFileVerStr(aFilename: String): String;
var aV1, aV2, aV3, aV4: word;
begin
  result := GetFileVer(aFilename, aV1, aV2, aV3, aV4);
end;


function GetIEVer(var V1, V2, V3, V4: word): String;
begin
  result := GetFileVer(GetWinSysDir + '\Shdocvw.dll', V1, V2, V3, V4);
  //trick -- Early versions of IE had only 3 numbers
  if (v1=4) and (v2<=70) and (v3=0) then
  begin
    v3 := v4;  v4 := 0;
    result := format('%d.%d.%d.%d',[v1,v2,v3,v4]);
  end;
end;

{
  Version Compare : returns -1 if Va < Vb, 0 if Va = Vb, 1 if Va > Vb
  eg. VerCompar(1,0,0,1, 1,0,0,2) will return -1
  eg. VerCompar(2,0,0,1, 1,0,6,90) will return 1 because 2.0.0.1 is > 1.0.6.90
}
function VerCompare(va1, va2, va3, va4, vb1, vb2, vb3, vb4: Word): Integer;
begin
  if (va1 = vb1) AND (va2 = vb2) AND (va3 = vb3) AND (va4 = vb4) then
    result := 0
  else if (va1 > vb1)
  or ((va1 = vb1) AND (va2 > vb2))
  or ((va1 = vb1) AND (va2 = vb2) AND (va3 > vb3))
  or ((va1 = vb1) AND (va2 = vb2) AND (va3 = vb3) AND (va4 > vb4)) then
    result := 1
  else
    result := -1;
end;


{ Get Friendly version numbers for HTML Help 'hhctrl.ocx'
    V1.0 is   4.72.7290 - IE4
    V1.1 is   4.72.7323
    V1.1a is  4.72.7325 - Windows98
    V1.1b is  4.72.8164 - MSDN
    V1.2 is   4.73.8252 - Adds extra search control & Favorites tab
    V1.21 is  4.73.8412 - Bug fixes
    V1.21a is 4.73.8474 - Quick update to fix FTS on CDROM
    V1.22 is  4.73.8561 - This release fixes three bugs in 1.21a that caused problems for Arabic, Hebrew, and Far East languages.
    V1.3 is   4.74.8702 - Win2000 Unicode support
    V1.31 is  4.74.8793 - Minor update
    V2.0 is   ???????
  return '' if hhctrl.ocx not found, otherwise a version string like '1.2'.

  Get up to date version info from http://helpware.net/htmlhelp/hh_info.htm
}
function GetHHFriendlyVer: String;
var  v1,v2,v3,v4: Word; fn, s: String;
begin
  fn := hh.GetPathToHHCtrlOCX;
  s := GetFileVer(fn, v1,v2,v3,v4);
  if s = '' then
    result := ''
  else
  if VerCompare( v1,v2,v3,v4, 4,74,8793,0) > 0 then
    result := '> 1.31'
  else if VerCompare( v1,v2,v3,v4, 4,74,8793,0) >= 0 then
    result := '1.31'
  else if VerCompare( v1,v2,v3,v4, 4,74,8702,0) >= 0 then
    result := '1.3'
  else if VerCompare( v1,v2,v3,v4, 4,73,8561,0) >= 0 then
    result := '1.22'
  else if VerCompare( v1,v2,v3,v4, 4,73,8474,0) >= 0 then
    result := '1.21a'
  else if VerCompare( v1,v2,v3,v4, 4,73,8412,0) >= 0 then
    result := '1.21'
  else if VerCompare( v1,v2,v3,v4, 4,73,8252,0) >= 0 then
    result := '1.2'
  else if VerCompare( v1,v2,v3,v4, 4,72,8164,0) >= 0 then
    result := '1.1b'
  else if VerCompare( v1,v2,v3,v4, 4,72,7325,0) >= 0 then
    result := '1.1a'
  else if VerCompare( v1,v2,v3,v4, 4,72,7323,0) >= 0 then
    result := '1.1'
  else if VerCompare( v1,v2,v3,v4, 4,72,7290,0) >= 0 then
    result := '1.0'
  else
    result := '< 1.0';
end;


{
  Check is IE Version x.x.x.x is installed.
  returns
    -1   ... A lesser version of x.x.x.x is installed.
     0   ... x.x.x.x is the version installed
    +1   ... A greater version of x.x.x.x is installed.

  Example
    if Check_IE_Version(4,70,1300,0) < 0 then
      ShowMessage('HtmlHelp requires that you installed IE3.02 or better.');
}
function Check_IE_Version(x1, x2, x3, x4: Integer): Integer;
var  v1,v2,v3,v4: Word; fn: String;
begin
  result := -1;
  fn := GetWinSysDir + '\Shdocvw.dll';
  if GetFileVer(fn, v1,v2,v3,v4) <> '' then
  begin
    //trick -- Early versions of IE had only 3 numbers
    if (v1=4) and (v2<=70) and (v3=0) then
    begin
      v3 := v4;  v4 := 0;
    end;

    result := VerCompare(v1,v2,v3,v4, x1,x2,x3,x4);  //Compare installed version with x.x.x.x 
  end;
end;


{ Get Friendly version numbers of IE (see above)
  return '' if Shdocvw.dll not found. otherwise a descriptive version string

  The following are the versions of Shdocvw.dll and the browser version that each represents
  <major version>.<minor version>.<build number>.<sub-build number>

  From http://support.microsoft.com/support/kb/articles/q164/5/39.asp
  or get up to date version info from http://helpware.net/htmlhelp/hh_info.htm

Shdocvw.dll -------------- May be different from the about box

   Version         Product
   --------------------------------------------------------------
   4.70.1155       Internet Explorer 3.0
   4.70.1158       Internet Explorer 3.0 (OSR2)
   4.70.1215       Internet Explorer 3.01
   4.70.1300       Internet Explorer 3.02
   4.71.1008.3     Internet Explorer 4.0 PP2
   4.71.1712.5     Internet Explorer 4.0
   4.72.2106.7     Internet Explorer 4.01
   4.72.3110.3     Internet Explorer 4.01 Service Pack 1
   4.72.3612.1707  Internet Explorer 4.01 SP2
   5.00.0518.5     Internet Explorer 5 Developer Preview (Beta 1)
   5.00.0910.1308  Internet Explorer 5 Beta (Beta 2)
   5.00.2014.213   Internet Explorer 5.0
   5.00.2314.1000  Internet Explorer 5.0a -- Released with Win98 SE and MSDN
   5.00.2614.3500  Internet Explorer 5.0b -- Contains Java VM and DCOM security patch as an update to Win98 SE
   5.00.2721.1400  Internet Explorer 5 with Update for "ImportExport - Favorites()" Security Issue installed
   5.0.2723.2900   Internet Explorer 5.0 with Update for "Server-side Page Reference Redirect" Issue installed.

   5.00.2919.800    Internet Explorer 5.01 (Windows 2000 RC1, build 5.00.2072)
   5.00.2919.3800   Internet Explorer 5.01 (Windows 2000 RC2, build 5.00.2128)
   5.00.2919.6307   Internet Explorer 5.01
   5.00.2919.6400   Internet Explorer 5.01 with Update for "Server-side Page Reference Redirect" Issue installed.
   5.50.3825.1300   Internet Explorer 5.5 Developer Preview (Beta)


}
function GetIEFriendlyVer: String;
var  v1,v2,v3,v4: Word; fn, s: String;
begin
  fn := GetWinSysDir + '\Shdocvw.dll';
  s := GetFileVer(fn, v1,v2,v3,v4);
  //trick -- Early versions of IE had only 3 numbers
  if (v1=4) and (v2<=70) and (v3=0) then
  begin
    v3 := v4;  v4 := 0;
    s := format('%d.%d.%d.%d',[v1,v2,v3,v4]);
  end;

  if s = '' then
    result := ''
  else
  if VerCompare( v1,v2,v3,v4, 5,50,3825,1300) > 0 then
    result := '> Internet Explorer 5.5 Developer Preview'
  else if VerCompare( v1,v2,v3,v4, 5,50,3825,1300) >= 0 then
    result := 'Internet Explorer 5.5 Developer Preview'
  else if VerCompare( v1,v2,v3,v4, 5,00,2919,6400) >= 0 then
    result := 'Internet Explorer 5.01'
  else if VerCompare( v1,v2,v3,v4, 5,00,2919,6307) >= 0 then
    result := 'Internet Explorer 5.01'
  else if VerCompare( v1,v2,v3,v4, 5,00,2919,3800) >= 0 then
    result := 'Internet Explorer 5.01 (Windows 2000 RC2, build 5.00.2128)'
  else if VerCompare( v1,v2,v3,v4, 5,00,2919,800) >= 0 then
    result := 'Internet Explorer 5.01 (Windows 2000 RC1, build 5.00.2072)'
  else if VerCompare( v1,v2,v3,v4, 5,00,2723,2900) >= 0 then
    result := 'Internet Explorer 5.0 updated'
  else if VerCompare( v1,v2,v3,v4, 5,00,2721,1400) >= 0 then
    result := 'Internet Explorer 5.0 updated'
  else if VerCompare( v1,v2,v3,v4, 5,00,2614,0) >= 0 then
    result := 'Internet Explorer 5.0b'
  else if VerCompare( v1,v2,v3,v4, 5,00,2314,0) >= 0 then
    result := 'Internet Explorer 5.0a'
  else if VerCompare( v1,v2,v3,v4, 5,00,2014,0) >= 0 then
    result := 'Internet Explorer 5.0'
  else if VerCompare( v1,v2,v3,v4, 5,00,0910,0) >= 0 then
    result := 'Internet Explorer 5 Beta (Beta 2)'
  else if VerCompare( v1,v2,v3,v4, 5,00,0518,0) >= 0 then
    result := 'Internet Explorer 5 Developer Preview (Beta 1)'
  else if VerCompare( v1,v2,v3,v4, 4,72,3612,0) >= 0 then
    result := 'Internet Explorer 4.01 Service Pack 2 (SP2)'
  else if VerCompare( v1,v2,v3,v4, 4,72,3110,0) >= 0 then
    result := 'Internet Explorer 4.01 Service Pack 1 (SP1)'
  else if VerCompare( v1,v2,v3,v4, 4,72,2106,0) >= 0 then
    result := 'Internet Explorer 4.01'
  else if VerCompare( v1,v2,v3,v4, 4,71,1712,0) >= 0 then
    result := 'Internet Explorer 4.0'
  else if VerCompare( v1,v2,v3,v4, 4,71,1008,0) >= 0 then
    result := 'Internet Explorer 4.0 Platform Preview 2.0 (PP2)'
  else if VerCompare( v1,v2,v3,v4, 4,71,544,0 ) >= 0 then
    result := 'Internet Explorer 4.0 Platform Preview 1.0 (PP1)'
  else if VerCompare( v1,v2,v3,v4, 4,70,1300,0) >= 0 then
    result := 'Internet Explorer 3.02'
  else if VerCompare( v1,v2,v3,v4, 4,70,1215,0) >= 0 then
    result := 'Internet Explorer 3.01'
  else if VerCompare( v1,v2,v3,v4, 4,70,1158,0) >= 0 then
    result := 'Internet Explorer 3.0 (OSR2)'
  else if VerCompare( v1,v2,v3,v4, 4,70,1155,0) >= 0 then
    result := 'Internet Explorer 3.0'
  else if VerCompare( v1,v2,v3,v4, 4,40,520,0 ) >= 0 then
    result := 'Internet Explorer 2.0'
  else if VerCompare( v1,v2,v3,v4, 4,40,308,0 ) >= 0 then
    result := 'Internet Explorer 1.0 (Plus!)'
  else
    result := '< Internet Explorer 1.0 (Plus!)';
end;


{
  Check is HtmlHelp Version x.x.x.x is installed.
  returns
    -1   ... A lesser version of x.x.x.x is installed.
     0   ... x.x.x.x is the version installed
    +1   ... A greater version of x.x.x.x is installed.

  Example
    if Check_HH_Version(4,73,8252,0) < 0 then
      ShowMessage('HtmlHelp 1.2 or greater is required. Please download a new version.');
}
function Check_HH_Version(x1, x2, x3, x4: Integer): Integer;
var  v1,v2,v3,v4: Word; fn: String;
begin
  result := -1;
  fn := hh.GetPathToHHCtrlOCX;
  if GetFileVer(fn, v1,v2,v3,v4) <> '' then
    result := VerCompare(v1,v2,v3,v4, x1,x2,x3,x4);
end;


{
  ShellExec()
  =============================
  Calls Windows shellexecute(h,'open',...)
  eg. Shellexec('mailto:robert.chandler@osi.varian.com', '');
  Returns TRUE if windows reports no errors
}
function ShellExec(aFilename: String; aParams: String): Boolean;
var h: THandle; handle: hWnd;
begin
  {
    Get Handle of parent window
  }
  if (Screen <> nil) AND (Screen.ActiveForm <> nil) AND (Screen.ActiveForm.handle <> 0) then
    handle := Screen.ActiveForm.handle
  else
  if Assigned(Application) AND Assigned(Application.Mainform) then
    handle := Application.Mainform.handle
  else
    handle := 0;

  h := ShellExecute(handle, 'open', Pchar(aFilename), Pchar(aParams), nil, SW_SHOWDEFAULT);
  result := (h > 32);  //success?
  if NOT result then
    ReportError('Function ShellExecute(%s)' + #13
              + 'Returned: %s', [aFilename+', '+aParams, GetLastErrorStr]);
end;


{
  Return error description of last error
}
function GetLastErrorStr: String;
var ErrCode: Integer;
begin
  ErrCode := GetlastError;
  case ErrCode of
    ERROR_FILE_NOT_FOUND:	  result := st_GLE_FileNotFound;
    ERROR_PATH_NOT_FOUND:	  result := st_GLE_PathNotFound;
    ERROR_ACCESS_DENIED:          result := st_GLE_AccessDenied;
    ERROR_NOT_ENOUGH_MEMORY:      result := st_GLE_InsufficientMemory;
    ERROR_WRITE_PROTECT:          result := st_GLE_MediaIsWriteProtected;
    ERROR_NOT_READY:              result := st_GLE_DeviceNotReady;
    ERROR_SHARING_VIOLATION,
    ERROR_LOCK_VIOLATION:         result := st_GLE_FileInUse;
    ERROR_HANDLE_DISK_FULL,
    ERROR_DISK_FULL:              result := st_GLE_DiskFull;
    ERROR_OLD_WIN_VERSION:        result := st_GLE_WindowsVersionIncorrect;
    ERROR_APP_WRONG_OS:           result := st_GLE_NotAWindowsOrMSDosProgram;
    ERROR_EA_FILE_CORRUPT,
    ERROR_UNRECOGNIZED_VOLUME,
    ERROR_FILE_CORRUPT,
    ERROR_DISK_CORRUPT:           result := st_GLE_CorruptFileOrDisk;
    ERROR_BADDB,
    ERROR_INTERNAL_DB_CORRUPTION: result := st_GLE_CorruptRegistry;
  else                            result := st_GLE_GeneralFailure;
  end; {case}
  result := '[Error:'+IntToStr(ErrCode) + '] ' + result;
end;


{
  Get a value from the registry
  dataName = '' for default value.
  Returns '' if not found
}
function GetRegStr(rootkey: HKEY; const key, dataName: string): string;
var rg: TRegistry;
begin
  result := '';  //default return
  rg := TRegistry.Create;
  rg.RootKey :=  rootkey;

{$IFDEF D4PLUS} // -- Delphi >=4
  if rg.OpenKeyReadOnly(key) AND rg.ValueExists(dataName) then //safer call under NT
{$ELSE}        // -- Delphi 2, 3
  if rg.OpenKey(key, false) AND rg.ValueExists(dataName) then
{$ENDIF}
  begin
    result := rg.ReadString(dataName);
    rg.CloseKey;
  end;
  rg.Free;
end;

{
  Creates a Key and addes a Value
  An absolute key begins with a backslash (\) and is a subkey of the root key.
}
procedure PutRegStr(rootkey: HKEY; const key, name, value: string);
var rg: TRegistry;
begin
  rg := TRegistry.Create;
  rg.RootKey :=  rootkey;
  if rg.OpenKey(key, TRUE {create if not found}) then
  begin
    rg.WriteString(name, value);
    rg.CloseKey;
  end;
  rg.Free;
end;


{
  Sometimes the only way we can test if a drive is writable is to write a test file.
  aDir is some Dir on a valid disk drive
}
function IsDirWritable(aDir: String): Boolean;
var F: File; fn: String;
begin
  StripR(aDir, '\');  //no trailing slash
  fn := aDir + '\$_Temp_$.$$$';   //Any abnormal filename will do
  FileMode := 2;  //read/write
  AssignFile(F, fn);
  {$I-} Rewrite(F, 1);
  result := (IOResult = 0);
  if result then
  begin
    CloseFile(F);
    DeleteFile(fn);
  end;
end;


{
  Debug file - takes same params as the Delphi Format() function.
  Output is to DBG_FILENAME in the application run folder.
  File is cleared when the exe is started.

  _DebugMode can be enabled by creating a file debug.debug in the
  applications run folder.

  Normal usage:
    if _DebugMode then
      DebugOut('File was not found "%s"', [filename]);
}
procedure DebugOut(msgStr: String; const Args: array of const);
var f: TextFile; s: String;
begin
  {$I-}
  AssignFile(f, DBG_DIR + DBG_FILENAME);
  if (not FileExists(DBG_DIR + DBG_FILENAME)) then
    Rewrite(f)  //create
  else
    Append(f);
  if ioresult = 0 then
  begin
    s := format(msgStr, Args);
    if s = '-' then   //separator
      s := MkStr('-', 80);
    if s = '=' then   //separator
      s := MkStr('=', 80);
    if (s <> '') and (s[1] in ['-', '=', '!']) then
      s := Copy(S, 2, maxint)
    else
      s := TimeToStr(now) + '   ' + s;
    Writeln(f, s);
    Flush(f);
    CloseFile(f);
  end;
end; //DebugOut


{same function but this one checks the debug flag}
procedure DebugOut2(msgStr: String; const Args: array of const);
begin
  if _DebugMode then
    DebugOut(msgStr, Args);
end;


{Display debug file in default window}
procedure ShowDebugFile;
var fn: String;
begin
  fn := DBG_DIR + DBG_FILENAME;
  if FileExists(fn) then
    ShellExec(fn, '')
  else
    ShowMessage('File not found'#13+fn+#13+'Debug Enabled = '+IntToStr(Integer(_DebugMode)));
end;

{Delete and start a new debug file}
procedure ResetDebugFile;
begin
  if FileExists(DBG_DIR + DBG_FILENAME) then
    DeleteFile(DBG_DIR + DBG_FILENAME);
  if _DebugMode then
  begin
    DebugOut('!Filename:             %s',[#9 + DBG_DIR + DBG_FILENAME]);
    DebugOut('!Date:                 %s',[#9 + DateTimeToStr(now)]);
{$IFDEF D3PLUS} // -- Delphi >=3
    DebugOut('!SysLocale.DefaultLCID: %s',[#9+'0x'+IntToHex(SysLocale.DefaultLCID, 4)]);
    DebugOut('!SysLocale.PriLangID:   %s',[#9+'0x'+IntToHex(SysLocale.PriLangID, 4)]);
    DebugOut('!SysLocale.SubLangID:   %s',[#9+'0x'+IntToHex(SysLocale.SubLangID, 4)]);
{$ENDIF}
    DebugOut('!DecimalSeparator:      %s',[#9+DecimalSeparator]);

    DebugOut('-', ['']);
    DebugOut('!EXE Path =          %s',[#9 + ParamStr(0)]);
    DebugOut('!_RunDir =           %s',[#9 + _RunDir]);
    DebugOut('!_ModuleName =       %s',[#9 + _ModuleName]);
    DebugOut('!_ModuleDir =        %s',[#9 + _ModuleDir]);
    DebugOut('-', ['']);
    DebugOut('!_hhInstalled =      %s', [#9 + BoolToYN(_hhInstalled)]);
    DebugOut('!_hhVerStr =         %s', [#9 + _hhVerStr]);
    DebugOut('!_hhFriendlyVerStr = %s', [#9 + _hhFriendlyVerStr]);
    DebugOut('-', ['']);
    DebugOut('!_ieInstalled =      %s', [#9 + BoolToYN(_ieInstalled)]);
    DebugOut('!_ieVerStr =         %s', [#9 + _ieVerStr]);
    DebugOut('!_ieFriendlyVerStr = %s', [#9 + _ieFriendlyVerStr]);
    DebugOut('=', ['']);
  end;
end;


{
  All Errors reported here
  Uses same format as the Delphi Format() function
}
procedure ReportError( errStr: String; const Args: array of const );
var s: String;
begin
  s := format( errStr, Args);
  MessageDlg(s, mtWarning, [mbOK], 0);
  if _DebugMode then
    DebugOut(s, ['']);
end;


{ Module initialization }
procedure ModuleInit;
var
  v1,v2,v3,v4, i: Word;
  FileName: array[0..300] of Char;
begin
  //Get run dir & Progname - or DLL or EXE
  GetModuleFileName(HInstance, FileName, SizeOf(FileName));
  _ModulePath := Filename;
  _ModuleDir := SysUtils.ExtractFilePath(_ModulePath);
  _ModuleName := SysUtils.ExtractFileName(_ModulePath);
  StripR(_ModuleDir, '\');

  { get run dir }
  _RunDir := ExtractFilePath(ParamStr(0));
  StripR(_RunDir, '\');

  { Debug Dir is current dir, Or root of Windows dir if readonly. CD? }
  If IsDirWritable(_ModuleDir) then
    DBG_DIR := _ModuleDir        //Where EXE or DLL lives
  else
    DBG_DIR := GetWinTempDir;    //Window Temp folder

  {debug mode enabled is file debug.debug found in the Modules dir OR a /debug or -debug cmdline switch}
  _DebugMode := FileExists(_ModuleDir + '\debug.debug');
  if not _DebugMode then
     for i := 1 to ParamCount do
       if (CompareText(paramstr(i), '/debug') = 0) or (CompareText(paramstr(i), '-debug') = 0) then
       begin
         _DebugMode := TRUE;
         break;
       end;

  {get version info of 'hhctrl.ocx' - returns '' and 0s if not found}
  _hhVerStr := GetFileVer(hh.GetPathToHHCtrlOCX, _hhMajVer, _hhMinVer, _hhBuildNo, _hhSubBuildNo);
  _hhInstalled := (_hhVerStr <> '');
  _hhFriendlyVerStr := GetHHFriendlyVer;

  {ie info}
  _ieVerStr := GetIEVer(v1,v2,v3,v4);
  _ieInstalled := (_ieVerStr <> '');
  _ieFriendlyVerStr := GetIEFriendlyVer;

  ResetDebugFile;
end;


initialization
  ModuleInit;
end.

{***************************************************************
 *
 * Unit Name: pntypes
 * Purpose  : Various types used in Programmers Notepad
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 *			  Portions copyright GExperts.
 * GExperts : Some of the code in this unit is taken from the
 * 			  GExperts Source Code distribution. Please see
 *			  gexpertslicense.html.
 **************************************************************}
 
unit pntypes;

{$Define plugin}

interface

uses Messages, SysUtils, Windows, comctrls, menus, Graphics;

// The following declaration is necessary because of an error in
// the declaration of BroadcastSystemMessage() in the Windows unit
function BroadcastSystemMessage(Flags: DWORD; Recipients: PDWORD;   uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;   external 'user32.dll';

const
  // Plugin response flags...
  plgOnStartup = 1;
  plgOnShutdown = 2;
  plgOnFocus = 4;
  plgOnLostFocus = 8;
  WM_UPDATESETTINGS = WM_USER + 2;

//The following constants are for using WS_EX_LAYERED
const
  WS_EX_LAYERED = $00080000;
  LWA_COLORKEY = $00000001;
  LWA_ALPHA = $00000002;
  ULW_COLORKEY = $00000001;
  ULW_ALPHA = $00000002;
  ULW_OPAQUE = $00000004;
  AC_SRC_ALPHA = $1;

type
  tEditorMode = (emNormal, emHex);
  tEditorOption = (emShowingResults, emRunning, emShowHighlightMenu);
  tEditorOptions = set of tEditorOption;
  tResultPosition = (rpSide, rpBottom, rpTop);
  tSysEditFile = (seAutoExec, seConfig, seWinIni, seSysIni, seProt);
  tParseDataType = (pdtFilename, pdtExtraParms, pdtLine, pdtColumn, pdtFolder, pdtBuildFile);
  tSysEditFiles = set of tSysEditFile;
  tNewType = Record
    TypeDesc : String;
    TypeFilter : string;
    TypeUnix : Boolean;
    TypeHex : Boolean;
  end;
  tTransProc = function(Hndle : THandle) : Integer; stdcall;
  tTransColProc = function(Hndle : THandle; Color : COLORREF; TransP : Integer) : Integer; stdcall;
  {$ifdef plugin}
  TPluginInit = function(OwnerApp : Integer) : Integer; stdcall;
  TIntResult  = function : integer; stdcall;
  TBoolResult = function : boolean; stdcall;
  TStrResult  = function(str : pChar) : integer; stdcall;
  TPlgMsgNum  = procedure(num : Longint); stdcall;
  TPlgProc    = procedure; stdcall;
  TPlgFlags   = function : Longint; stdcall;
  // The following are used to initialise the plugins interface...
  // They are used to pass callback references...
  pdLongInt    = function : Longint; stdcall;
  pcLongInt    = procedure(Index : Longint; dwProc : pdLongInt); stdcall;
  pdBuffer_A   = function(Buffer : PChar; var BufferSize : Longint) : Boolean; stdcall;
  pcBuffer_A   = procedure(Index : Longint; dwProc : pdBuffer_A); stdcall;
  pdTwoInt_A   = function(LI1, LI2 : Longint) : Boolean; stdcall;
  pcTwoInt_A   = procedure(Index : Longint; dwProc : pdTwoInt_A); stdcall;
  pdTwoInt_B   = function(var LI1, LI2 : Longint) : Boolean; stdcall;
  pcTwoInt_B   = procedure(Index : Longint; dwProc : pdTwoInt_B); stdcall;
  pdMenuItem   = function(PluginName, Caption, EventProc : PChar; Menu, Position : Integer) : Boolean; stdcall;
  pcMenuItem   = procedure(Index : Longint; dwProc : pdMenuItem); stdcall;
  pdUIItem     = function(PluginName, Caption, EventProc, ShortCut : PChar; Container, Position, Data : Integer) : Boolean; stdcall;
  pcUIItem     = procedure(Index : Longint; dwProc : pdUIItem); stdcall;
  pdBoolean    = function : Boolean; stdcall;
  pcBoolean    = procedure(Index : Longint; dwProc : pdBoolean); stdcall;
  {$endif}
  TSpecialLocations = (slNone,slDesktop,slFavorites,slFonts,slNetHood,slPersonal,
                       slPrograms,slRecent,slSendTo,slStartMenu,slStartup,slTemplates);

  //Filters:'Text File|*.txt|document File|*.doc'
  //there are two filter in it above
  //the 1st filter is 'Text File|*.txt' which
  //'Text File' is Filter description
  //'*.txt' is filter file extension
  TFilterList=class
  private
    { Private declarations }
    function GetFilter(Index: Integer): string;
    Procedure SetFilter(Index: Integer; Value:string);

    function GetFilterDescript(Index: Integer): string;
    Procedure SetFilterDescript(Index: Integer; Value:string);

    function GetFilterExt(Index: Integer): string;
    Procedure SetFilterExt(Index: Integer; Value:string);

    function  GetFilterPos(const aIndex:integer; var StartPos,endPos :Integer) :Boolean;
  public
    Filters: string;
    procedure Add(const aDescript, aExt:string);
    procedure AddFilter(const aFilter:string);
    procedure Delete(const aIndex:Integer);
    function Count:Integer;

    property Filter[Index :Integer]:string read GetFilter write SetFilter;
    property Description[Index :Integer]:string read GetFilterDescript write SetFilterDescript;
    property Extension[Index :Integer]:string read GetFilterExt write SetFilterExt;
  end;  //TFilterList
  PPluginData = ^TPluginData;
  TPluginData = record
    Name : string;
    FileName : string;
    OnQuit : Boolean;
    OnGotFocus : Boolean;
    OnLostFocus : Boolean;
    Next : PPluginData;
  end;
  TPlugins = class
  private
    fTop : PPluginData;
    fLast : PPluginData;
    function GetItem(Index : Integer) : PPluginData;
  public
    Count : Integer;
    property Items[Index: Integer]: PPluginData read GetItem;
    procedure Clear;
    procedure Add(nwp : TPluginData);
    constructor Create;
    destructor Destroy; override;
  end;
  TPluginMI = class(TMenuItem)
  public
    plugin : string;
    evname : string;
  end;
  type
  TSetLayeredWindowAttributes = function(
    hWnd: HWND;             // handle to the layered window
    crKey: COLORREF;        // specifies the color key
    bAlpha: byte;           // value for the blend function
    dwFlags: DWORD          // action
  ): BOOL; stdcall;

function SetLayeredWindowAttributes(hWnd: HWND) : BOOL;
procedure RemoveLayeredWindowAttributes(hWnd : HWND);

function SeperatorPosition(const Index :integer; const s :string; const Seperator :char):Integer;
function SeperatorCount(const s :string; const Seperator :char):Integer;

implementation

function TFilterList.GetFilterPos(const aIndex:integer; var StartPos,endPos :Integer):Boolean;
var
  t,m:Integer;
begin
  Result:=False;
  t:=Count;
  if (aIndex+1) > t then Exit;
  m:= aIndex * 2;
  if aIndex=0 then
    startPos:= 1
  else
    startPos:= SeperatorPosition(m, Filters, '|')+1;
  if (aIndex+1) = t then  //the last index
    endPos  := Length(Filters)
  else begin
    endPos  := SeperatorPosition(m+2, Filters, '|')-1;
  end;
  Result:=True;
end;

function TFilterList.GetFilter(Index: Integer): string;
var
   StartPos,endPos:Integer;
begin
   if not GetFilterPos(Index, StartPos,endPos) then exit;
   Result:=Copy(Filters, StartPos, endPos - StartPos+1);
end;

Procedure TFilterList.SetFilter(Index: Integer; Value:string);
begin
end;

function TFilterList.GetFilterDescript(Index: Integer): string;
var
  s:string;
   StartPos,endPos:Integer;
begin
   if not GetFilterPos(Index, StartPos,endPos) then exit;
   s:=Copy(Filters, StartPos, endPos - StartPos);
   StartPos:=Pos('|', s);
   Result:=Copy(s,1, StartPos-1);
end;

Procedure TFilterList.SetFilterDescript(Index: Integer; Value:string);
begin
end;

function TFilterList.GetFilterExt(Index: Integer): string;
var
  s:string;
   StartPos,endPos:Integer;
begin
   if not GetFilterPos(Index, StartPos,endPos) then exit;
   s:=Copy(Filters, StartPos, endPos - StartPos+1);
   StartPos:=Pos('|', s);
   Result:=Copy(s,StartPos+1, Length(s));
end;

Procedure TFilterList.SetFilterExt(Index: Integer; Value:string);
begin
end;

procedure TFilterList.Add(const aDescript, aExt:string);
begin
  Filters:=Filters + '|' + aDescript + '|'+ aExt;
end;

procedure TFilterList.AddFilter(const aFilter:string);
begin
  Filters:=Filters + '|' + aFilter;
end;

procedure TFilterList.Delete(const aIndex:Integer);
var
   StartPos,endPos:Integer;
begin
  if not GetFilterPos(aIndex, StartPos,endPos) then exit;
  Filters:=Copy(Filters,1, startPos-1)+Copy(Filters,endPos+2, Length(Filters));
end;

function TFilterList.Count:Integer;
begin
  Result:=(SeperatorCount(Filters,'|') + 1) div 2;
end;

function SeperatorCount(const s :string; const Seperator :char):integer;
var
  P : PChar;
  t : string;
begin
  Result:=0;
  t := S + #0;  // double null terminators
  P := AnsiStrScan(PChar(t), Seperator);
  while P <> nil do
  begin
    Inc(Result);
//    P^ := #0;
    Inc(P);
    P := AnsiStrScan(P, Seperator);
  end;
end;

function SeperatorPosition(const Index :integer; const s :string; const Seperator :char):integer;
var
  P : PChar;
  t : string;
begin
  Result:=0;
  t := S + #0;  // double null terminators
  P := AnsiStrScan(PChar(t), Seperator);
  while P <> nil do
  begin
    Inc(Result);
    if Result>=Index then
    begin
      Result:=P-PChar(t)+1;
      Exit;
    end;
    Inc(P);
    P := AnsiStrScan(P, Seperator);
  end;
end;

procedure TPlugins.Clear;
var
  it : PPluginData;
  ni : PPluginData;
begin
  it := fTop;
  while(it <> nil) do
  begin
    ni := it^.Next;
    Dispose(it);
    it := ni;
  end;
  fTop := nil;
  fLast := nil;
  Count := 0;
end;

procedure TPlugins.Add(nwp : TPluginData);
var
  ni : PPluginData;
begin
  ni := New(PPluginData);
  ni^.Name := nwp.Name;
  ni^.FileName := nwp.FileName;
  ni^.OnQuit := nwp.OnQuit;
  ni^.OnGotFocus := nwp.OnGotFocus;
  ni^.OnLostFocus := nwp.OnLostFocus;
  ni^.Next := nil;

  if fTop <> nil then
  begin
    fLast^.Next := ni;
    fLast := ni;
  end else
  begin
    fTop := ni;
    fLast := ni;
  end;
  Inc(Count);
end;

constructor TPlugins.Create;
begin
  fTop := nil;
  fLast := nil;
  Count := 0;
end;

destructor TPlugins.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TPlugins.GetItem(Index : Integer) : PPluginData;
var
  n : Integer;
  p : PPluginData;
begin
  p := fTop;
  for n := 1 to Index do
    p := p^.Next;
  Result := p;
end;

function SetLayeredWindowAttributes(hWnd: HWND): BOOL;
var
  FDLLHandle: HINST;
  FSetLayeredWindowAttrFunc: TSetLayeredWindowAttributes;
begin
  Result := False;
  FDLLHandle := LoadLibrary(user32);
  if(FDLLHandle <> 0) then
  begin
    SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or
        WS_EX_LAYERED);
    FSetLayeredWindowAttrFunc := GetProcAddress(FDLLHandle, 'SetLayeredWindowAttributes');
    Result := FSetLayeredWindowAttrFunc(hWnd, 0, 180, LWA_ALPHA);
    FreeLibrary(FDLLHandle);
  end;
end;

procedure RemoveLayeredWindowAttributes(hWnd : HWND);
begin
    SetWindowLong(hWnd, GWL_EXSTYLE,
        GetWindowLong(hWnd, GWL_EXSTYLE) and not WS_EX_LAYERED);
    RedrawWindow(hWnd, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or
        RDW_ALLCHILDREN);
end;

end.

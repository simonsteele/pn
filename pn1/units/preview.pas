{***************************************************************
 *
 * Unit Name: preview
 * Purpose  : HTML Previewer - uses Internet Explorer Control.
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}
unit preview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, SHDocVw, ComCtrls, TB97Tlbr, TB97Ctls,
  TB97, ActnList, OleCtrls, MOZILLACONTROLLib_TLB, Menus, ImgList, Registry;

type
  TPreviewState = (usingIE, usingMozilla);
  TfrmPreview = class(TForm)
    pnWebBrowser: TPanel;
    StatusBar1: TStatusBar;
    dokPreviewTop: TDock97;
    Toolbar971: TToolbar97;
    btnRefresh: TToolbarButton97;
    ToolbarSep971: TToolbarSep97;
    btnBack: TToolbarButton97;
    btnForward: TToolbarButton97;
    ToolbarSep972: TToolbarSep97;
    btnStop: TToolbarButton97;
    ActionList1: TActionList;
    actReload: TAction;
    actBack: TAction;
    actForward: TAction;
    actStop: TAction;
    ToolbarSep973: TToolbarSep97;
    btnBrowser: TToolbarButton97;
    ilsPreview: TImageList;
    popBrowser: TPopupMenu;
    InternetExplorer1: TMenuItem;
    Mozilla1: TMenuItem;
    actIE: TAction;
    actMozilla: TAction;
    procedure pnWebBrowserResize(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actBackExecute(Sender: TObject);
    procedure actForwardExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actIEExecute(Sender: TObject);
    procedure actMozillaExecute(Sender: TObject);
    procedure popBrowserPopup(Sender: TObject);
  private
    { Private declarations }
    FWorking : Boolean;
    fMozUnavail : Boolean;
    State: TPreviewState;
    FFileName: String;
    WebBrowser1: TWebBrowser;  {NOTE: may not be instantiated -- use accessors instead}
    Mozilla: TMozillaBrowser;  {NOTE: This is even less likely to be instantiated!}
    FCantShowBrowser: Boolean;
    FDeleteFiles: TStringList;
    procedure SetBrowser(Value: Boolean);
    function GetBrowser: Boolean;
    function SetURL(const AURL: string): Boolean;
    procedure SetFileName(New: String);
    procedure WebStatus(Sender: TObject; const Text: WideString);
  public
    { Public declarations }
    procedure Execute(InBuiltBrowser: Integer; inFileName: string);
    property HasBrowser: Boolean read GetBrowser write SetBrowser;
    property FileName: String read FFileName write SetFileName;
  end;

var
  frmPreview: TfrmPreview = nil;

const navOpenInNewWindow = 1;
      navNoHistory       = 2;
      navNoReadFromCache = 4;
      navNoWriteToCache  = 8;

implementation

{$R *.DFM}
uses Main, Editor, useful;

procedure TfrmPreview.WebStatus(Sender: TObject; const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

procedure TfrmPreview.pnWebBrowserResize(Sender: TObject);
begin
//  with Sender as TPanel do begin
//    if Assigned(WebBrowser1) then
//      WebBrowser1.SetBounds(0,0,ClientWidth,ClientHeight);
//  end;
end;

function TfrmPreview.SetURL(const AURL: string): Boolean;
var
  Flags, TargetFrameName, PostData, Headers,RefreshFlag: OLEVariant;
  TempURL: String;
  i: Integer;
begin
  if (Pos('file:///', AURL) = 0) then
  begin
    TempURL:='file:///'+AURL;
    for i:=1 to length(TempUrl) do
      if TempURL[i]='\' then TempURL[i]:='/';
  end;
  Result := False;
  if not HasBrowser then exit;
  Flags := navNoReadFromCache+NavNoWriteToCache;
  Headers := Null;
  TargetFrameName := Null;
  PostData := Null;
  RefreshFlag:=3;
  try
    case State of
      usingIE :
      begin
        if CompareText(WebBrowser1.LocationURL,tempURL)=0 then
          WebBrowser1.Refresh2(RefreshFlag)
        else
          WebBrowser1.Navigate(tempURL, Flags, TargetFrameName, PostData, Headers);
        Result := True;
      end;
      usingMozilla :
      begin
        // Is this really necessary
        Flags := null;
        if CompareText(Mozilla.LocationURL, TempURL)=0 then
          Mozilla.Refresh2(RefreshFlag)
        else
          Mozilla.Navigate(TempURL, Flags, TargetFrameName, PostData, Headers);
        Result := True;
      end;
    end;
  except
    {swallow exceptions}
  end;
end;


procedure TfrmPreview.btnRefreshClick(Sender: TObject);
var fname,
    Ext     : String;
    FEditor : TfrmClient;
    p       : Integer;
begin
  FEditor := frmMain.GetCurrentEditor;
  if FEditor = nil then exit;
  if FEditor.Parser <> frmMain.Parsers.HTML then
  Begin
     frmMain.ShowText('This is not an HTML file...');
     Exit;
  End;
  fname := FEditor.Filename;
  Ext := ExtractFileExt(FName);
  p := pos(Ext, fname);
  if p > 0 then
    Delete(fname, p, length(Ext));
  fname := fname + '.TMP';
  DeleteFile(fname);
  FEditor.synMDI.SaveToFile(fname);
  FDeleteFiles.Add(fname);
  SetURL(fname);
end;

procedure TfrmPreview.SetBrowser(Value: Boolean);

  procedure AssertBrowserIE;
  begin
    WebBrowser1 := TWebBrowser.Create(Self);
    {with WebBrowser1 do begin
      OnNavigateComplete2 := TMsWebBrowser_V1NavigateComplete
    end;}
    {WebBrowser1.PopupMenu := pmDummy;}
    TWinControl(WebBrowser1).Parent := pnWebBrowser;
    { adjust visible metrics -- note: must follow order}
    Application.ProcessMessages; {for aesthetic appeal}
    pnWebBrowserResize(pnWebBrowser);
    WebBrowser1.Visible:=True;
    WebBrowser1.OnStatusTextChange := WebStatus;
    btnBrowser.ImageIndex := 43;
 end;

 procedure AssertBrowserMozilla;
 begin
    if (fMozUnavail) then
    begin
      MessageDlg('Programmers Notepad is trying to use Mozilla which does not '+#13+#10+'appear to be installed on your system. If you wish to use Mozilla'+#13+#10+'to preview HTML, we suggest you download it from www.mozilla.org '+#13+#10+'and install it.', mtWarning, [mbOK], 0);
      Exit;
    end;
    Mozilla := TMozillaBrowser.Create(Self);
    TWinControl(Mozilla).Parent := pnWebBrowser;
    Mozilla.ParentWindow := pnWebBrowser.Handle;
    Mozilla.SetBounds(1, 1, pnWebBrowser.Width-1, pnWebBrowser.Height-1);
    { adjust visible metrics -- note: must follow order}
    Application.ProcessMessages; {for aesthetic appeal}
    pnWebBrowserResize(pnWebBrowser);
    Mozilla.OnStatusTextChange := WebStatus;
    btnBrowser.ImageIndex := 44;
 end;

begin
  case State of
    usingIE : if value = (Assigned(WebBrowser1)) then exit;
    usingMozilla : if Value = (Assigned(Mozilla)) then Exit;
  end;
  if value then begin
    if FCantShowBrowser then exit;
    try
      Screen.Cursor:=crHourglass;
      try
        case State of
          usingIE : AssertBrowserIE;
          usingMozilla : AssertBrowserMozilla;
        end;
      finally
        Screen.Cursor:=crDefault;
      end;
      pnWebBrowser.Visible := True;
    except
      FCantShowBrowser := True;
      ShowMessage('Browser could not be shown!');
    end;
    end
  else try
    pnWebBrowser.Visible := False;
    case State of
      usingIE : WebBrowser1.Free;
      usingMozilla : Mozilla.Free;
    end;
    finally
      case State of
        usingIE : WebBrowser1 := nil;
        usingMozilla : Mozilla := nil;
      end;
  end;
End;

function TfrmPreview.GetBrowser: Boolean;
Begin
  Result := False;
  case State of
    usingIE : Result:=(WebBrowser1<>nil);
    usingMozilla : Result := (Mozilla<>nil);
  end;
End;

procedure TfrmPreview.SetFileName(New: String);
Begin
 if New=FFilename then exit;
 HasBrowser:=True;
 FFilename:=New;
 SetUrl(FFileName);
 StatusBar1.SimpleText:=FFileName;
End;

function MozillaInstalled : Boolean;
var
  r : TRegistry;
  b : Boolean;
  s : string;
const
  CLSID = 'CLSID\{1339B54C-3453-11D2-93B9-000000000000}';
  IPS = CLSID + '\InProcServer32';
begin
  r := TRegistry.Create;
  r.RootKey := HKEY_CLASSES_ROOT;
  b := r.KeyExists(CLSID);
  if b then
  begin
    b := r.KeyExists(IPS);
    if b then
    begin
      r.OpenKey(IPS, False);
      s := r.ReadString('');
      b := FileExists(s);
      r.CloseKey;
    end;
  end;
  r.Free;
  Result := b;
end;

procedure TfrmPreview.FormCreate(Sender: TObject);
begin
  if not MozillaInstalled then
  begin
    fMozUnavail := True;
    actMozilla.Enabled := False;
  end;
  FDeleteFiles:=TStringList.Create;
  FDeleteFiles.Duplicates:=DupIgnore;
  FCantShowBrowser:=False;
  WebBrowser1:=nil;
  Mozilla:=nil;
  State := usingIE;
  FFileName:='';
end;

procedure TfrmPreview.Execute(InBuiltBrowser : Integer; inFileName : string);
begin
  if (FWorking) then
  begin
    if TPreviewState(InBuiltBrowser) <> State then
    begin
      case TPreviewState(InBuiltBrowser) of
        usingIE : actIE.Execute;
        usingMozilla : actMozilla.Execute;
      end;
    end;
  end;
  State := TPreviewState(InBuiltBrowser);
  HasBrowser:=True;
  Show;
  SetFileName(inFileName);
  FWorking := True;
end;

procedure TfrmPreview.FormDestroy(Sender: TObject);
begin
  While FDeleteFiles.Count>0 do
    Begin
    Try
    DeleteFile(FDeleteFiles.Strings[0]);
    Except

    End;
    FDeleteFiles.Delete(0);
    End;
  FDeleteFiles.Free;
end;

procedure TfrmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   If frmMain.WindowState <> wsMaximized Then
   Begin
      SetPosition('Echo Software', 'PN\HTMLPreview', Top, Left);
      SetSize('Echo Software', 'PN\HTMLPreview', frmMain.Height, frmMain.Width);
   End;
   SetStat('Echo Software', 'PN\HTMLPreview', WindowState);
   Action:=caFree;
   frmPreview:=nil;
end;

procedure TfrmPreview.actBackExecute(Sender: TObject);
begin
  If not HasBrowser then exit;
  Try
    case State of
      usingIE : WebBrowser1.GoBack;
      usingMozilla : Mozilla.GoBack;
    end;
  Except
    {Swallow exceptions}
  End;
end;

procedure TfrmPreview.actForwardExecute(Sender: TObject);
begin
  If not HasBrowser then exit;
  Try
    case State of
      usingIE : WebBrowser1.GoForward;
      usingMozilla : Mozilla.GoForward;
    end;
  Except
    {Swallow exceptions}
  End;
end;

procedure TfrmPreview.actStopExecute(Sender: TObject);
begin
  If not HasBrowser then exit;
  Try
    case State of
      usingIE : WebBrowser1.Stop;
      usingMozilla : Mozilla.Stop;
    end;
  Except
    {Swallow exceptions}
  End;
end;

procedure TfrmPreview.FormShow(Sender: TObject);
var i1, i2 : integer;
begin
   If GetSize('Echo Software', 'PN\HTMLPreview', i1, i2) Then
   Begin
      SetBounds(Left, Top, i2, i1);
   End;
   WindowState := GetStat('Echo Software', 'PN\HTMLPreview');
   If WindowState = wsNormal then
   Begin
      GetPosition('Echo Software', 'PN\HTMLPreview', Height, Width,
                  i1, i2);
      SetBounds(i2, i1, Width, Height);
   End;
end;

procedure TfrmPreview.FormResize(Sender: TObject);
begin
    case State of
      usingIE :
        if Assigned(WebBrowser1) then
          WebBrowser1.SetBounds(0,0,pnWebBrowser.ClientWidth,pnWebBrowser.ClientHeight);
      usingMozilla :
        if Assigned(Mozilla) then
          Mozilla.SetBounds(0,0,pnWebBrowser.ClientWidth,pnWebBrowser.ClientHeight);
    end;
end;

procedure TfrmPreview.actIEExecute(Sender: TObject);
var s : string;
    i : Integer;
begin
  if State = usingMozilla then
  begin
    s := Mozilla.LocationURL;
    SetBrowser(False);
    State := usingIE;
    SetBrowser(True);
    FormResize(pnWebBrowser);
    if(Copy(s, 1, 8) = 'file:///') then
    begin
      s := Copy(s, 9, Length(s));
      for i := 1 to Length(s) do
        if s[i] = '/' then s[i] := '\';
    end;
    FFileName := '';
    SetFilename(s);
    btnBrowser.ImageIndex := 43;
  end;
end;

procedure TfrmPreview.actMozillaExecute(Sender: TObject);
var
  s : string;
  i : Integer;
begin
  if State = usingIE then
  begin
    s := WebBrowser1.LocationURL;
    SetBrowser(False);
    State := usingMozilla;
    SetBrowser(True);
    FormResize(pnWebBrowser);
    if(Copy(s, 1, 8) = 'file:///') then
    begin
      s := Copy(s, 9, Length(s));
      for i := 1 to Length(s) do
        if s[i] = '/' then s[i] := '\';
    end;
    FFileName := '';
    SetFilename(s);
    btnBrowser.ImageIndex := 44;
  end;
end;

procedure TfrmPreview.popBrowserPopup(Sender: TObject);
begin
  actIE.Checked := State = usingIE;
end;

end.

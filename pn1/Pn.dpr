program PN;

uses
  Forms,
  Windows,
  Messages,
  SysUtils,
  Dialogs,
  genfuncs in 'units\genfuncs.pas',
  about in 'units\about.pas' {frmAbout},
  print in 'units\print.pas' {frmPrint},
  options in 'units\options.pas' {frmOptions},
  preview in 'units\preview.pas' {frmPreview},
  welcome in 'units\welcome.pas' {frmWelcome},
  newtool in 'units\newtool.pas' {frmNewToolLink},
  useful in 'units\useful.pas',
  hexedprv in 'units\hexedprv.pas' {frmHexEdPreview},
  main in 'units\main.pas' {frmMain},
  editor in 'units\editor.pas' {frmClient},
  pntypes in 'units\pntypes.pas',
  pndefs in 'units\pndefs.pas',
  pff in 'units\pff.pas' {frmPFF},
  plgiface in 'units\plgiface.pas',
  bkmrkmru in 'units\bkmrkmru.pas',
  parserrepos in 'units\parserrepos.pas' {Parsers},
  execute in 'units\execute.pas',
  HotKey in 'units\hotkey.pas',
  FindDlg in 'units\FindDlg.pas' {frmFind},
  ReplDlg in 'units\ReplDlg.pas' {frmReplace},
  GrepResultsDlg in 'units\GrepResultsDlg.pas' {frmGrepResults},
  GrepSearchDlg in 'units\GrepSearchDlg.pas' {frmGrepSearch},
  SearchFile in 'units\SearchFile.pas',
  helper in 'units\helper.pas' {frmHelper},
  eports in 'units\eports.pas',
  hh in 'units\hh.pas',
  hh_funcs in 'units\hh_funcs.pas';

{$R *.RES}

var
   NumFound     : Integer = 0;
   LastFound    : HWND = 0;
   MyPopup      : HWND = 0;
   Params       : Boolean;
   np,n         : Integer;
   RegMsg       : Integer;
   MessageID    : Integer;
   Atom         : tAtom;
   Buffer       : Array[0..255] of Char;
   MutHandle    : THandle = 0;
   WProc        : TFNWndProc = nil;
   MiError      : Integer;

const
   AllowedInstances     = 1;
   UniqueAppID          = 'EchoSoftwareProgrammersNotepad2';
   MessageIDKey         = 'EchoSoftwarePNMutexKey2';
   MI_NO_ERROR          = 0;
   MI_FAIL_SUBCLASS     = 1;
   MI_FAIL_CREATE_MUTEX = 2;

function NewWndProc(Handle: HWND; Msg: Integer; wParam, lParam:
  Longint):
  Longint; stdcall;
begin
  // If this code works, it was written by Simon Steele...
  // If it doesn't, I don't know who wrote it :)

  { If this is the registered message... }
  if Msg = MessageID then
  begin
    { if main form is minimized, normalize it }
    { set focus to application }
    if(IsWindow(Application.MainForm.ClientHandle)) then
    begin
      if IsIconic(Application.Handle) then
      begin
        Application.MainForm.WindowState := wsNormal;
        Application.Restore;
      end;
      SetForegroundWindow(Application.MainForm.Handle);
      MyPopup := GetLastActivePopup(Application.MainForm.Handle);
      if IsIconic(MyPopup) then
        ShowWindow(MyPopup, SW_RESTORE)
      else
        SetForeGroundWindow(MyPopup);
      Result := 0;
    end else
      Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam);
  end
    { Otherwise, pass message on to old window proc }
  else
    Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam);
end;

procedure SubClassApplication;
begin
  { We subclass Application window procedure so that }
  { Application.OnMessage remains available for user. }
  WProc := TFNWndProc(SetWindowLong(Application.Handle, GWL_WNDPROC,
    Longint(@NewWndProc)));
  { Set appropriate error flag if error condition occurred }
  if WProc = nil then
    MIError := MIError or MI_FAIL_SUBCLASS;
end;

procedure DoFirstInstance;
begin
  SubClassApplication;
  MutHandle := CreateMutex(nil, False, UniqueAppID);
  if MutHandle = 0 then
    MIError := MIError or MI_FAIL_CREATE_MUTEX;
end;

procedure BroadcastFocusMessage;
{ This is called when there is already an instance running. }
var
  BSMRecipients : DWORD;
  i             : Integer;
begin
  { Don't flash main form }
  Application.ShowMainForm := False;
  { Post message and inform other instance to focus itself }
  Sleep(1000);
  BSMRecipients := BSM_APPLICATIONS;
  BroadCastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
      @BSMRecipients, MessageID, 0, 0);

  if Params then
    for i := 1 to np do
    begin
      Atom := 0;
      Atom := GlobalAddAtom(StrPCopy(Buffer, ParamStr(i)));
      SendMessage(HWND_BROADCAST, RegMsg, Application.Handle, Atom);
      GlobalDeleteAtom(Atom);
    end;
//  Application.Terminate;
end;

function InitInstance : Boolean;
begin
  Result := False;
  MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, UniqueAppID);
  if MutHandle = 0 then
  begin
    { Mutex object has not yet been created, meaning that no previous }
    { instance has been created. }
    DoFirstInstance;
    Result := True;
  end else
    BroadcastFocusMessage;
end;

begin
{Initialize Variables}
  Params := False; np := 0;
{Register Message Class}
  RegMsg := RegisterWindowMessage(PChar(UniqueAppID));
  MessageID := RegisterWindowMessage(PChar(MessageIDKey));
{Setup Application, and check for multiple instances}
  Application.Title := 'PN';
  Application.HelpFile := 'pn.chm';
  If Length(ParamStr(1)) > 0 Then Params := True;
  If Params Then For n := 1 to 9 do
    If Length(Paramstr(n)) > 0 Then np := n;
  {Call our new nicer multiple-instance code...}
  if InitInstance then
  begin
    {If we have not exited by now, we enter the main program.}
    Application.Initialize;
    Application.CreateForm(TfrmMain, frmMain);
  frmMain.MsgNum := RegMsg;
    Application.Run;
    {Now we replace our old Window Procedure}
    if WProc <> nil then
      SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(WProc));
  end;
end.

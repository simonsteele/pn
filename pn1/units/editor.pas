{***************************************************************
 *
 * Unit Name: editor
 * Purpose  : MDI Client for PN
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html. 
 * History  : 18/04/1999 Creation of editor unit.
 *            24/04/1999 Added code for local toggles.
 *            25/04/1999 Update for better Word-Wrap Toggle.
 *            03/04/1999 Fixed .dfm saving with streams etc...
 *            29/05/1999 Further Integration of the Hex Editor
 *                       including toggling button states, and
 *                       updating the status bar and saving and
 *                       opening files. Added Hex Printing and Hex
 *                       print preview features. Printing Now also
 *                       takes notice of font size and face passed
 *                       by the print form...
 *            30/05/1999 Further Integration of Hex editor with
 *                       mdi client form. Added find functionality.
 *            03/06/1999 Fixed a typo which caused SetSyntax not to
 *                       work.
 *            01/07/1999 Added a plain text parser in order to allow
 *                       customization of the editor for plain text.
 *            17/08/1999 Updated to use the MRU Bookmarks feature.
 *            08/09/1999 Fixed the glyph-drawing problem...
 *            09/03/2000 Updated after system crash, fixed saving code.
 ****************************************************************}

unit editor;

{$define parsers}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SyntaxEd, ComCtrls, ImgList, ToolWin, ActnList, genfuncs, HexEditor, StdCtrls,
  pntypes, Printers, synParse, extctrls, Menus, Registry, Clipbrd;

type
  TfrmClient = class(TForm)
    ToolBar1: TToolBar;
    ilsToolbar: TImageList;
    btnColour: TToolButton;
    btnWordWrap: TToolButton;
    synMDI: TSyntaxMemo;
    ActionList1: TActionList;
    actColour: TAction;
    actWrap: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    actHexedit: TAction;
    btnLineNos: TToolButton;
    actLineNos: TAction;
    btnOutput: TToolButton;
    popResults: TPopupMenu;
    itmTop: TMenuItem;
    itmRight: TMenuItem;
    itmBottom: TMenuItem;
    popEditor: TPopupMenu;
    Close3: TMenuItem;
    N4: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    ToolButton4: TToolButton;
    actShowOutput: TAction;
    N1: TMenuItem;
    hide1: TMenuItem;
    Configure1: TMenuItem;
    N2: TMenuItem;
    EditorOptions1: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    ChangeCase1: TMenuItem;
    Lowercase1: TMenuItem;
    Uppercase1: TMenuItem;
    N6: TMenuItem;
    ReverseCase1: TMenuItem;
    Indentation1: TMenuItem;
    Indent1: TMenuItem;
    Unindent1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    N7: TMenuItem;
    ViewasHTML1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure synMDIChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actColourExecute(Sender: TObject);
    procedure actWrapExecute(Sender: TObject);
    procedure actHexeditExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure actLineNosExecute(Sender: TObject);
    procedure synMDIHyperlinkClick(Sender: TObject; HyperData: String;
      HyperType: Integer);
    procedure popResultsPopup(Sender: TObject);
    procedure itmTopClick(Sender: TObject);
    procedure synMDIDblClick(Sender: TObject);
    procedure synMDIMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure popEditorPopup(Sender: TObject);
    procedure actShowOutputExecute(Sender: TObject);
    procedure hide1Click(Sender: TObject);
    procedure Configure1Click(Sender: TObject);
  private
    { Private declarations }
    HexToCanvas : tHexToCanvas;
    FindPos : Integer;
    FindBuf : PChar;
    FindLen : Integer;
    FindStr : string;
    FindICase : Boolean;
    ResultsWin : tSyntaxMemo;
    ResultsSep : tSplitter;
    MPos       : tPoint;
    LaunchDouble : Boolean;
    fModified : Boolean;
    _doingagecheck : Boolean;
    procedure SetDefaults;
    procedure HexEditorStateChanged(Sender: TObject);
    procedure HexSaveFile;
    procedure CreateHexToCanvas;
    procedure FreeHexToCanvas;
    procedure OnFilesDropped(Sender: TObject; FileList: TStrings;
      AtPoint: TPoint);
    procedure CreateResults(fPos: tResultPosition);
    procedure FreeResults;
    procedure NotifySettings(var msg: TMessage); message WM_UPDATESETTINGS;
    procedure CopyURLHandler(Sender: TObject);
    procedure LaunchURLHandler(Sender: TObject);
    procedure ResultsLineClick(Sender: TObject; HyperData: String;
      HyperType: Integer);
    procedure DoTools(Bother: Boolean);
    procedure CommonExecute;
    procedure CommonExecuteEnd;
    procedure CheckForURL;
    procedure AddHighlightMenu;
    procedure SetModified(const Value: Boolean);
    function MakeErrorString(load : Boolean; i : Longint; fn : string) : string;
    procedure CheckLog;
    procedure SilentReplaceAll(findstr, replacestr: string;
      FindOptions: integer);
    function PrivateOpenFile(filename, ext : string) : Boolean;
  public
    { Public declarations }
    FileName   : string;
    Parser     : integer;
    Mode       : tEditorMode;
    HexEditor  : tHexEditor;
    JumpOffs   : Integer;
    Options    : tEditorOptions;
    ResultsPos : tResultPosition;
    Age        : Integer;
    Template   : Boolean;
    procedure SetMode(NewMode: tEditorMode);
    procedure OpenFile(filename, Ext: String);
    procedure SetParser(Ext: String); overload;
    procedure SetParser(uParser: Integer); overload;
    procedure Execute(FName, Ext: String); overload;
    procedure Execute(Ext: Integer; iFilename : string); overload;
    procedure SaveFile;
    function  CanIClose: Boolean;
    procedure UpdateWordWrap;
    procedure HexPrintPreview;
    procedure HexPrint(FFont : tFont);
    procedure HexFind;
    procedure HexFindAgain;
    procedure HexGoto;
    procedure SetJumpOffset;
    procedure HexJump(Direction : Boolean);
    procedure SetResults(msg: tStrings);
    procedure AddResult(Msg : string);
    procedure ClearResults;
    procedure ShowResults(Show: boolean);
    procedure JumpToLineNumber(ln : integer; select : boolean);
    function  IsNewFile: Boolean;
    property Modified : Boolean read fModified Write SetModified;
    procedure CheckAge;
    procedure Revert;
    procedure TabsToSpaces;
    procedure SpacesToTabs;
  end;

var
  frmClient: TfrmClient;

const
  udREPLACEALL = UDS_USER + 1;
  et_WEB = 253;
  et_MAIL = 254;
  CouldNotSaveError = 'PN cannot save this file to the specified location:'+#13+
        #10+'%s'+#13+#10+ 'This could be due to an absent disk, a broken ' +
        'network connection, or a full disk.' +#13#10+ 'Do you want to save in'+
        ' another location?';
  CouldNotLoadError = 'PN could not load %s.'+#13#10+'This could be because the'
                      +' file no longer exists, a disk is absent or due to a '
                      +#13#10+'broken network connection.';
  SaveAccessDenied = 'PN could not save %s because access was denied.' +#13#10+
                      'This could be due to the file being marked read-only, '
                      +'or the disk being write-protected.'
                      +#13#10+ 'Do you want to save in another location?';
  LoadAccessDenied = 'PN could not load %s because access was denied.' +#13#10+
                     'This could be because you do not have sufficient rights'
                     +#13#10+'to open this file.';
  SaveDiskFullError = 'PN could not save %s because the disk is full.'+#13#10+
                      'Do you want to save in another location?';
  SaveShareViolation = 'PN could not save %s because another program or user is'
                       +#13#10+'using the same file.'+#13#10+
                       'Do you want to save in another location?';
  LoadShareViolation = 'PN could not open %s because another program or user is'
                       +#13#10+'using the same file.';
  NetSaveError       = 'PN could not save %s because of a network error. This '+
                       #13#10 + 'could be because of a broken network' +#13#10+
                       ' connection or because the network was too busy.'+#13#10
                       + 'Do you want to save in another location?';
  NetLoadError       = 'PN could not load %s because of a network error. This '+
                       #13#10 + 'could be because of a broken network' +#13#10+
                       ' connection or because the network was too busy.';
implementation

{$R *.DFM}
{$R Ln.res}

uses options, main, hexedprv, useful;

procedure TfrmClient.FormCreate(Sender: TObject);
begin
  synMDI.PopupMenu := popEditor;
  SetDefaults;
  synMDI.GutterGlyphs.Delete(15);
  synMDI.GutterGlyphs.ResourceLoad(rtBitmap, 'IMG_LINECHANGE', clRed);
  frmMain.EnableButtons;
  frmMain.UpdateStatusBar;
  {Initialise Hex Variables}
  JumpOffs  := 4000;
  FindBuf   := nil;
  FindLen   := 0;
  FindPos   := -1;
  FindICase := False;
  FindStr   := '';
  Options := [];
  fModified := False;
  Age := 0;
  _doingagecheck := False;
  Template := False;
end;

procedure TfrmClient.SetDefaults;
begin
   With synMDI do
   Begin
      Font.Name := Config.FontName;
      Font.Size := Config.FontSize;
   end;
end;

procedure TfrmClient.CheckLog;
var s : string;
begin
  if synMDI.Lines.Count < 1 then Exit;
  if LowerCase(synMDI.Lines[0]) = '.log' then
  begin
    DateTimeToString(s, 'hh:nn dd/mm/yyyy', Now);
    synMDI.Lines.Add('');
    synMDI.Lines.Add(s);
    synMDI.Lines.Add('');
    synMDI.Perform(EM_LINESCROLL, 0, synMDI.Lines.Count);
    synMDI.Perform(EM_SCROLLCARET, 0,0);  
  end;
end;

function TfrmClient.PrivateOpenFile(Filename, Ext : string) : Boolean;
var
  i : Integer;
  MStream : TMemoryStream;
  FStream : TFileStream;
  DesperateLoad : Boolean;
  s : String;
begin
  Result := True;
  DesperateLoad := False;
  if (UpperCase(Ext) <> '.DFM') then
  begin
    synMDI.Lines.BeginUpdate;
    try
      try
        synMDI.LoadFromFile(FileName);
      except
        on E: Exception do
        begin
          i := GetLastError;
          s := MakeErrorString(True, i, FileName);
          if (E is EFOpenError) or (E is EReadError) then
          begin
            MessageDlg(s, mtWarning, [mbOK], 0);
          end;
          Result := False;
        end;
      end;
    finally
      synMDI.Lines.EndUpdate;
    end;
  end else
  begin
   // load DFM
   try
      FStream := TFileStream.Create(filename, fmOpenRead+fmShareCompat);
    except
      on E: Exception do
      begin
        i := GetLastError;
        s := MakeErrorString(True, i, FileName);
        if (E is EFOpenError) or (E is EReadError) then
        begin
          MessageDlg(s, mtWarning, [mbOK], 0);
        end;
        Result := False;
        Exit;
      end;
    end;
    MStream := TMemoryStream.Create;
    try
      try
        ObjectResourceToText(FStream, MStream);
        MStream.Position := 0;
        synMDI.LoadFromStream(MStream);
      except
        DesperateLoad := True;
      end;
    finally
      FStream.Free;
      MStream.Free;
      synMDI.LoadFromFile(FileName);
    end;
  end;
end;

Procedure TfrmClient.OpenFile(filename,Ext:String);
var {FStream : TFileStream;
    MStream : TMemoryStream;}
    index   : integer;
    {i       : Longint;
    s       : string;}
Begin
  if Ext = '' then
    Ext := ExtractFileExt(FileName);

  Index := frmMain.GetFileType(Ext);
  SetParser(Index);

  Ext := UpperCase(Ext);

  if frmMain.GetOpenAsHex(Ext) then
  begin
    Self.FileName := FileName;
    SetMode(emHex);
  end else
    if not PrivateOpenFile(filename, Ext) then
      Exit;

  {Ext := UpperCase(Ext);
  if Ext = '.DFM' then
  Begin
    try
      FStream := TFileStream.Create(filename, fmOpenRead+fmShareCompat);
    except
      on E: Exception do
      begin
        i := GetLastError;
        s := MakeErrorString(True, i, FileName);
        if (E is EFOpenError) or (E is EReadError) then
        begin
          MessageDlg(s, mtWarning, [mbOK], 0);
        end;
        Exit;
      end;
    end;
    MStream := TMemoryStream.Create;
    try
      ObjectResourceToText(FStream, MStream);
      MStream.Position := 0;
      synMDI.LoadFromStream(MStream);
      FStream.Free;
      MStream.Free;
    except
      FStream.Free;
      MStream.Free;
      synMDI.LoadFromFile(FileName);
    end;
  end else
  if frmMain.GetOpenAsHex(Ext) then
  begin
    Self.FileName := FileName;
    SetMode(emHex);
  end Else
  begin
    synMDI.Lines.BeginUpdate;
    try
      try
        synMDI.LoadFromFile(FileName);
      except
        on E: Exception do
        begin
          i := GetLastError;
          s := MakeErrorString(True, i, FileName);
          if (E is EFOpenError) or (E is EReadError) then
          begin
            MessageDlg(s, mtWarning, [mbOK], 0);
          end;
        end;
      end;
    finally
      synMDI.Lines.EndUpdate;
    end;
  end;}

  //frmMain.EnableButtons;
  frmMain.SetEditor;

  // Some things should be done only if we're not doing a template.
  if not Template then
  begin
    // Following line moved from above frmMain.setEditor;
    frmMain.mruFileList1.AddItem(filename);

     // Following two lines moved from below refreshbookmarklist.
    Modified := False;
    Age := FileAge(FileName);

    if frmMain.BookmarkMRU.HasBookmarks(FileName) then
      with frmMain.BookmarkMRU do
      begin
         BeginSession;
         SetFilename(FileName);
         GetBookmarks(synMDI);
         EndSession;
      end;

    // Notepad .Log File Emulation
    if (LowerCase(Ext) = '.log') or (LowerCase(Ext) = '.txt') then
    begin
      CheckLog;
    end;
  end else
    FileName := '<new>';

  frmMain.RefreshBookmarkList;
end;

procedure TfrmClient.CommonExecute;
begin
  synMDI.ActiveParser := 1;
  synMDI.Parser1 := frmMain.Parsers.parPlain;
  synMDI.Options := synMDI.Options - [smoProcessDroppedFiles];
  synMDI.OnFilesDropped := OnFilesDropped;
  Case frmMain.actPrintMargin.Checked of
    True  : synMDI.Options := synMDI.Options + [smoShowRMargin];
    False : synMDI.Options := synMDI.Options - [smoShowRMargin];
  End;
  case frmMain.GetSetting('HighlightOnContext', EditKey, True) of
    True  : Options := Options + [emShowHighlightMenu];
    False : Options := Options - [emShowHighlightMenu];
  end;
  Mode := emNormal;
end;

procedure TfrmClient.CommonExecuteEnd;
var i : Integer;
begin
  synMDI.WordWrap := frmMain.actWordWrap.Checked;
  actWrap.Checked := synMDI.WordWrap;
  frmMain.EnableButtons;
  frmMain.UpdateStatusBar;
  synMDI.Font.Name := Config.Fontname;
  synMDI.Font.Size := Config.FontSize;
  frmMain.SetEditor;
  for i := 1 to synMDI.Lines.Count do
    if synMDI.LineHasGlyph(16, i) then
      synMDI.RemoveLineGlyph(16, i);
  LaunchDouble := frmMain.GetSetting('URLLaunchDouble', netkey, true);
  DoTools(True);
end;

Procedure TfrmClient.Execute(FName, Ext: String);
Begin
  CommonExecute;
  FileName := FName;
  if Fname = '' then
  begin
     Parser := frmMain.GetFileType(Ext);
     SetParser(Parser);
  end else
  begin
    if Pos('~', FName) >0 then
         Self.FileName := ExtractFilePath(FName) + AlternateToLFN(FName) else
         Self.FileName := FName;
    OpenFile(FileName, '');
  end;
  Caption := ExtractFileName(FileName);
  CommonExecuteEnd;
end;

Procedure TfrmClient.Execute(Ext: Integer; iFilename : string);
Begin
  CommonExecute;
  FileName := '';
  Parser := Ext;
  Filename := iFilename;
  Caption := ExtractFileName(FileName);
  if Pos('~', Caption) <> 0 then
     Caption := AlternateToLFN(Caption);
  SetParser(Ext);
  CommonExecuteEnd;
end;


Procedure TfrmClient.SetParser(Ext: String);
begin
  // Should this code be in use any more???
  MessageDlg('Programmers Notepad has attempted to perform an obsolete operation, '+#13+#10+'please forward this information to the development team:'+#13+#10+''+#13+#10+'SetParser(Ext : string) called by child unit.', mtWarning, [mbOK], 0);
End;

procedure TfrmClient.SetParser(uParser: Integer);
begin
  synMDI.ActiveParser := 1;
  Parser := uParser;
  IF uParser <= frmMain.Parsers.Count then
    synMDI.Parser1 := tSyntaxMemoParser(frmMain.Parsers.TagItems(uParser))
  else
    synMDI.Parser1 := tSyntaxMemoParser(frmMain.Parsers.Items(0));
end;

procedure TfrmClient.synMDIChange(Sender: TObject);
begin
  if not Assigned(synMDI) then Exit;
  if (smrcText in synMDI.ChangeReason) or (smrcUserEdit in synMDI.ChangeReason) then
    Modified := synMDI.Modified;
  if (smrcText in synMDI.ChangeReason) and Config.ShowChange and synMDI.Modified then
    synMDI.AddLineGlyph(16, synMDI.CaretPos.Y);
  if (smrcSelection in synMDI.ChangeReason) then
  begin
    if (synMDI.SelLength > 0) then frmMain.SetStatus(Format('%d characters selected', [synMDI.SelLength])) else
    frmMain.SetStatus('');
  end else
  frmMain.SetStatus('');
  frmMain.EnableButtons;
  frmMain.UpdateStatusBar;
end;

function TfrmClient.MakeErrorString(load : Boolean; i : Longint; fn : string) : string;
var s : string;
begin
  s := '';
  case i of
    ERROR_ACCESS_DENIED,
    ERROR_NOT_DOS_DISK,
    ERROR_WRITE_PROTECT :
      if load then
        s := Format(LoadAccessDenied, [FileName])
      else
        s := Format(SaveAccessDenied, [FileName]);
    ERROR_DISK_FULL,
    ERROR_HANDLE_DISK_FULL :
      if load then
        s := Format(CouldNotLoadError, [fn])
      else
        s := Format(SaveDiskFullError, [fn]);
    ERROR_SHARING_VIOLATION,
    ERROR_LOCK_VIOLATION :
      if load then
        s := Format(LoadShareViolation, [fn])
      else
        s := Format(SaveShareViolation, [fn]);
    ERROR_DEV_NOT_EXIST,
    ERROR_BAD_NETPATH,
    ERROR_NETWORK_BUSY :
      if load then
        s := Format(NetLoadError, [fn])
      else
        s := Format(NetSaveError, [fn]);
  else
    begin
      if load then
        s := Format(CouldNotLoadError, [fn])
      else
        s := Format(CouldNotSaveError, [fn]);
    end;
  end;
  Result := s;
end;

Procedure TfrmClient.SaveFile;
var Ext     : String;
    FStream : TFileStream;
    MStream : TMemoryStream;
    i       : Integer;

function SaveWithErrorCheck : Boolean;
var s : string;
    i : Longint;
begin
  Result := True;
  Try
    synMDI.SaveToFile(FileName);
  Except
    on E : Exception do
    begin
      Result := False;
      i := GetLastError;
      s := MakeErrorString(False, i, FileName);
      // Check error messages...
      If E is EFCreateError then
        If MessageDlg(s, mtError, [mbYes, mbNo], 0) = mrYes then
          frmMain.SaveAs(Self);
      If E is EWriteError then
        If MessageDlg(s, mtError, [mbYes, mbNo], 0) = mrYes then
          frmMain.SaveAs(Self);
    end;
  end;
end;

function CreateStreamWithErrorCheck : Boolean;
var
  i : Longint;
  s : string;
begin
  Result := True;
  Try
    FStream := TFileStream.Create(filename, fmCreate + fmShareCompat);
  Except
    on E : Exception do
    begin
      Result := False;
      i := GetLastError;
      s := MakeErrorString(False, i, Filename);
      If E is EFCreateError then
        If MessageDlg(s, mtError, [mbYes, mbNo], 0) = mrYes then
          frmMain.SaveAs(Self);
      If E is EWriteError then
        If MessageDlg(s, mtError, [mbYes, mbNo], 0) = mrYes then
          frmMain.SaveAs(Self);
    end;
  End;
end;

Begin
  Screen.Cursor := crHourglass;
  if Mode = emHex then
  begin
     HexSaveFile;
     Exit;
  end;
  Ext := uppercase(ExtractFileExt(Filename));
  if (Ext = '.DFM') and frmMain.GetSetting('SaveDFMAsBinary', rootkey, True) then
  Begin
     if not CreateStreamWithErrorCheck then
     begin
       Screen.Cursor := crDefault;
       Exit;
     end;

     MStream := TMemoryStream.Create;
     Try
        synMDI.Lines.SaveToStream(MStream);
        MStream.Seek(0,0);
        ObjectTextToResource(MStream, FStream);
     finally
        FStream.Free;
        MStream.Free;
     End;
  End else
  if frmMain.GetSaveAsUnix(Ext) then
  begin
    synMDI.SaveFormat := sfUnix;
    if not SaveWithErrorCheck then
    begin
      synMDI.SaveFormat := sfTEXT;
      Screen.Cursor := crDefault;
      Exit;
    end;
    synMDI.SaveFormat := sfTEXT;
  end else
    if not SaveWithErrorCheck then
    begin
      Screen.Cursor := crDefault;
      Exit;
    end;

  Modified := False;
  Template := False;
  frmMain.EnableButtons;
  frmMain.UpdateStatusBar;
  for i := 1 to synMDI.Lines.Count do
    if synMDI.LineHasGlyph(16, i) then
      synMDI.RemoveLineGlyph(16, i);
  Caption := ExtractFileName(FileName);
  Screen.Cursor := crDefault;
  Age := FileAge(FileName);
end;

function TfrmClient.CanIClose: Boolean;
var A  :Word;
Begin
  Result:=True;
  if Modified then
  Begin
     A := MessageDlg('Save file ' + ExtractFileName(FileName) +
                     ' before closing?', mtConfirmation,
                     [mbYes,mbNo,mbCancel], 0);
     Case A of
       mrCancel: Result := False;
       mrYes:
       Begin
          if pos('>', Filename) = 0 Then SaveFile
             Else
             Begin
                frmMain.SaveDialog1.FilterIndex := frmMain.NewFilterIndex(FileName);
                frmMain.SaveDialog1.InitialDir  := ExtractFilePath(FileName);
                if frmMain.SaveDialog1.Execute then
                Begin
                   FileName := frmMain.SaveDialog1.FileName;
                   SaveFile;
                End;
             End;
       end;
       mrNo : begin Modified := False; synMDI.Modified := False; end;
     End;
  End;

  if (not IsNewFile) then
  begin
     try
       repeat until frmMain.BookmarkMRU.Busy = False;
       frmMain.BookmarkMRU.BeginSession;
       frmMain.BookmarkMRU.SetFilename(FileName);
       frmMain.BookmarkMRU.SetBookmarks(synMDI);
       frmMain.BookmarkMRU.Save;
       frmMain.BookmarkMRU.EndSession;
     except
       {swallow exceptions}
     end;
  end;
end;

procedure TfrmClient.FormClose(Sender: TObject; var Action: TCloseAction);
var n : integer;
begin
   Action := caFree;
   synMDI.Parser1 := nil;
   try
     for n := frmMain.tabs.Tabs.Count - 1 downto 0 do
        if tfrmClient(frmMain.tabs.tabs.Objects[n]) = Self then
           frmMain.tabs.tabs.Delete(n);
   except
     frmMain.logMain.Add('shutdown', 'Exception deleting my tab (Editor)');
   end;
   try
     frmMain.EnableButtons;
     frmMain.SetEditor;
   except {Swallow Exceptions} end;
   frmMain.lstBookmarks.Items.Clear;
   If emShowingResults in Options then
   begin
     try
       FreeResults;
     except
       {Swallow Exceptions}
     end;
   end;
   try FreeAndNil(synMDI); except {Swallow Exceptions} end;
   If frmMain.tabs.tabs.Count < 1 Then
   Begin
      frmMain.tabs.Visible := False;
      frmMain.panBack.Visible := False;
      frmMain.StatusBar1.Panels[0].Text := '';
      frmMain.StatusBar1.Panels[1].Text := '';
      frmMain.StatusBar1.Panels[2].Text := '';
      Application.ProcessMessages;
      try
        DoTools(False);
      except
        frmMain.logMain.Add('shutdown', 'Exception sorting the tools (Editor)');
      end;
   End;
end;

procedure TfrmClient.FormActivate(Sender: TObject);
var n : integer;
begin
  if frmMain._doingcloseall then Exit;
  CheckAge;

  if not Assigned(synMDI) then Exit;

   try
     for n := 0 to frmMain.tabs.Tabs.Count - 1 do
        if tfrmClient(frmMain.tabs.tabs.Objects[n]) = Self then
        begin
           frmMain.tabs.tabindex := n;
           if frmMain.actHelper.Checked then
           begin
             frmMain.frmHelper.UpdateCurrentSelection(n);
           end;
        end;
     case mode of
        emHex :    begin
                      HexEditorStateChanged(Self);
                      HexEditor.SetFocus;
                      DoTools(False);
                   end;
        emNormal : begin
                      synMDIChange(Self);
                      synMDI.SetFocus;
                      DoTools(True);
                   end;
     end;
     frmMain.SetEditor;
     frmMain.RefreshBookmarkList;
   except
     {Swallow Exceptions}
   end;
end;

procedure TfrmClient.DoTools(Bother : Boolean);
begin
  case Bother of
    True :
      begin
        if Assigned(tSyntaxMemoParser(synMDI.Parser1)) then
          frmMain.LoadSchemeTools(TSyntaxMemoParser(synMDI.Parser1).UI_Styles.LangName)
        else
          frmMain.LoadSchemeTools('');
      end;
    False : frmMain.ClearSchemeTools;
  end;
end;

procedure TfrmClient.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
   CanClose := CanIClose;
end;

procedure TfrmClient.actColourExecute(Sender: TObject);
begin
   actColour.Checked := not actColour.Checked;
   case actColour.Checked of
      True  : synMDI.Options := synMDI.Options + [smoSyntaxHighlight];
      False : synMDI.Options := synMDI.Options - [smoSyntaxHighlight];
   end;
end;

procedure TfrmClient.actWrapExecute(Sender: TObject);
begin
   actWrap.Checked := not actWrap.Checked;
   synMDI.WordWrap := actWrap.Checked;
end;

procedure TfrmClient.UpdateWordWrap;
begin
   actWrap.Checked := synMDI.WordWrap;
end;

procedure TfrmClient.actHexeditExecute(Sender: TObject);
var NewMode : tEditorMode;
begin
   NewMode := emNormal;
   case Mode of
      emNormal : NewMode := emHex;
      emHex    : NewMode := emNormal;
   end;
   SetMode(NewMode);
end;

procedure TfrmClient.SetMode(NewMode : tEditorMode);
var Transfer : TMemoryStream;
begin
   actHexEdit.Checked := NewMode = emHex;
   //Change from emNormal to emHex and Vice Versa...
   // 1: Check Saved File Status...
   // 1a: Load the data into the (tHexEditor / tSyntaxMemo)
   // 2: Hide the (tSyntaxMemo / tHexEditor)
   // 3: Show the (tHexEditor / tSyntaxMemo)
   case NewMode of
      emHex    : begin
                    {1}
                    if synMDI.Modified then
                    begin
                       case MessageDlg('Do you want to save changes before changing mode?'+#13+#10+'(Hex Editor will work with saved version)', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                         mrYes: Self.SaveFile;
                         mrNo :;
                         mrCancel: begin actHexEdit.Checked := Mode = emHex; Exit; end;
                       end;
                    end;
                    HexEditor := tHexEditor.Create(Self);
                    HexEditor.Align := alClient;
                    HexEditor.Parent := Self;
                    HexEditor.BringToFront;
                    HexEditor.OnStateChanged := HexEditorStateChanged;
                    HexEditor.ScrollBars := ssBoth;
                    if IsNewFile then
                    begin
                      Transfer := TMemoryStream.Create;
                      synMDI.TextData.ExportTextToStream(transfer);
                      Transfer.Seek(0,0);
                      HexEditor.LoadFromStream(Transfer);
                      Transfer.Clear;
                      Transfer.Free;
                    end else
                      HexEditor.LoadFromFile(FileName);
                 end;
      emNormal : begin
                    if HexEditor.Modified then
                    begin
                       case MessageDlg('Do you want to save changes before changing mode?'+#13+#10+'(The Editor will work with the saved version)', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                         mrYes: Self.SaveFile;
                         mrNo :;
                         mrCancel: begin actHexEdit.Checked := Mode = emHex; Exit; end;
                       end;
                    end;
                    HexEditor.Align := alNone;
                    if IsnewFile then
                    begin
                      Transfer := TMemoryStream.Create;
                      HexEditor.SaveToStream(Transfer);
                      Transfer.Seek(0,0);
                      synMDI.LoadFromStream(Transfer);
                      Transfer.Clear;
                      Transfer.Free;
                    end else
                      synMDI.LoadFromFile(FileName);
                    synMDI.BringToFront;
                    try HexEditor.Free; except end;
                    frmMain.RefreshBookmarkList;
                 end;
   end;
   Mode := NewMode;
   frmMain.EnableButtons;
   frmMain.UpdateStatusBar;
end;

procedure TfrmClient.HexEditorStateChanged(Sender: TObject);
const
     TFString : array [False..True] of string = ('No' , 'Yes' );
var
   pSS , pSE : Integer;
   SizeReads : Real;
begin
   with HexEditor, frmMain.StatusBar1 do
   begin
      Panels[0].Text := 'Pos : '+IntToStr ( GetCursorPos );
      if SelCount <> 0 then
      begin
         pSS := SelStart;
         pSE := SelEnd;
         Panels[3].Text := 'Sel : '+IntToStr ( Min ( pSS , pSE)) +' - '+IntToStr(Max ( pSS , pSE))
      end else Panels[3].Text := '';
      if Modified then Panels[1].Text := 'Modified' else
      if ReadOnlyFile then Panels[1].Text := 'Read Only' else
         Panels[1].Text := '';
      // Size
      if DataSize > 1024 then
      begin
         SizeReads := DataSize / 1024;
         Panels[2].Text := Format('Size: %fk', [SizeReads]);
      end else
      Panels[2].Text := 'Size: ' + IntToStr ( DataSize );
   end;
   frmMain.EnableButtons;
   Self.Modified := HexEditor.Modified;
end;

function TfrmClient.IsNewFile : Boolean;
begin
   if pos('>', filename) <> 0 Then Result := True else
                                   Result := False;
end;

procedure TfrmClient.HexSaveFile;
var i : integer;
begin
   // Note Saving from HexMode needn't take into account
   // things like .DFM and .PAS and .CGI file saving
   // as these are irrelevant to a HEX editor as it doesn't
   // automatically save any characters, just what are in
   // the editor.
   HexEditor.SaveToFile(FileName);
   Modified := False;
   frmMain.EnableButtons;
   frmMain.UpdateStatusBar;
   for i := 0 to synMDI.Lines.Count - 1 do
     if synMDI.LineHasGlyph(15, i) then
       synMDI.RemoveLineGlyph(15, i);
   Caption := ExtractFileName(FileName);
   Age := FileAge(FileName);
end;

procedure TfrmClient.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
   actWrap.Enabled   := mode <> emHex;
   actColour.Enabled := mode <> emHex;
   actLineNos.Enabled := mode <> emHex;
   actLineNos.Checked := smoLineNumbers in synMDI.Options;
   actWrap.Checked := synMDI.WordWrap;
   actShowOutput.Checked := emShowingResults in Options;
end;

procedure TfrmClient.HexPrintPreview;
var
   pBMP : TBitMap;
   l1 : Integer;
begin
   CreateHexToCanvas;
   pBMP := TBitmap.Create;
   try
      pBMP.Width := 1000 ;
      pBMP.Height := Round ( Printer.PageHeight / Printer.PageWidth * 1000 );
      with pBMP do
      begin
         Canvas.Brush.Color := clWhite;
         Canvas.Brush.Style := bsSolid;
         Canvas.FillRect ( Rect ( 0 , 0 , Width , Height ) );
         // nun ränder berechnen
         HexToCanvas.GetLayout;
         HexToCanvas.TopMargin := Height div 20;
         HexToCanvas.BottomMargin := Height - (Height div 20);
         HexToCanvas.LeftMargin := Width div 20;
         HexToCanvas.RightMargin := Width - (Width div 20);
         HexToCanvas.Draw ( Canvas , 0 , HexEditor.DataSize -1 , 'PNHex: '+ExtractFileName(HexEditor.FileName),'Page 1' );
      end;
      with tfrmHexEdPreview.Create ( Application ) do
      try
         Image1.Width := ClientWidth;
         l1 := Round ( Image1.Width / pBMP.Width * pBMP.Height );
         if l1 > ClientHeight
         then begin
              Image1.Height := ClientHeight;
              Image1.Width := Round ( Image1.Height / pBMP.Height * pBMP.Width );
         end
         else
             Image1.Height := l1;
         Image1.Picture.Bitmap.Assign ( pBMP );
         ShowModal;
      finally
         Free;
      end;
   finally
      pBMP.Free;
      FreeHexToCanvas;
   end;
end;

procedure TfrmClient.HexPrint;
var
   l1, l2  : Integer;
   OldFont : tFont;
begin
   if HexEditor.DataSize < 1 then Exit;
   l1 := 0;
   CreateHexToCanvas;
   OldFont := tFont.Create;
   OldFont.Assign(HexEditor.Font);
   HexEditor.Font.Name := FFont.Name;
   HexEditor.Font.Size := FFont.Size;
   try
      with Printer do
      begin
         // nun ränder berechnen
         HexToCanvas.GetLayout;
         HexToCanvas.TopMargin := PageHeight div 20;
         HexToCanvas.BottomMargin := PageHeight - (PageHeight div 20);
         HexToCanvas.LeftMargin := PageWidth div 20;
         HexToCanvas.RightMargin := PageWidth - (PageWidth div 20);
         l2 := 1;
         BeginDoc;
         repeat
               Canvas.Brush.Color := clWhite;
               Canvas.Brush.Style := bsSolid;
               Canvas.FillRect ( Rect ( 0 , 0 , PageWidth , PageHeight ) );
               l1 := HexToCanvas.Draw ( Canvas , l1 , HexEditor.DataSize -1 ,
               'PNHex: '+ExtractFileName(HexEditor.FileName),'Page '+IntToStr(l2) );
               if l1 < HexEditor.DataSize then NewPage;
               l2 := l2+1;
         until l1 >= HexEditor.DataSize;
         EndDoc;
      end;
   finally
      FreeHexToCanvas;
      HexEditor.Font.Assign(OldFont);
      OldFont.Free;
   end;
end;

procedure TfrmClient.CreateHexToCanvas;
begin
   //Create the HexToCanvas object
   HexToCanvas := tHexToCanvas.Create(Self);
   HexToCanvas.BytesPerLine       := 16;
   HexToCanvas.BytesPerColumn     := 2;
   HexToCanvas.CharFieldSeparator := #0;
   HexToCanvas.DisplayCharField   := True;
   HexToCanvas.HexEditor          := HexEditor;
   HexToCanvas.MemFieldDisplay    := odHex;
   HexToCanvas.MemFieldSeparator  := char(';');
   HexToCanvas.OffsetDisplay      := odHex;
   HexToCanvas.OffsetSeparator    := char(':');
   HexToCanvas.ShrinkToFit        := True;
   HexToCanvas.StretchToFit       := False;
end;

procedure TfrmClient.FreeHexToCanvas;
begin
   //Free HexToCanvas object
   HexToCanvas.HexEditor := nil;
   HexToCanvas.Free;
end;

procedure TfrmClient.HexFind;
const
     cHexChars = '0123456789abcdef';
var
   pSTR, pTMP : String;
   pCT, pCT1  : Integer;
begin
   //Hex Find procedure
     if HexEditor.DataSize < 1 then Exit;
     FindPos := -1;
     FindICase := False;
     if FindBuf <> nil then
     begin
        FreeMem ( FindBuf );
        FindBuf := nil;
     end;
     if not InputQuery ( 'Find Data' , '"t.." ascii, "T.." + ignore case, else search hex' , FindStr ) then Exit;
     // make a search string
     if FindStr = '' then Exit;
     pStr := '';
     if UpCase(FindStr[1]) = 'T' then
     begin
        pStr := Copy ( FindStr , 2 , MaxInt );
        pCT1 := Length ( pStr );
     end else
     begin
        // calculate Data from input
        pTMP := AnsiLowerCase ( FindStr);
        for pCT := Length ( pTMP ) downto 1 do
          if Pos( pTMP[pCT] , cHexChars ) = 0 then Delete ( pTMP , pCT , 1 );
        while (Length(pTMP) mod 2) <> 0 do
          pTMP := '0'+pTMP;
        if pTMP = '' then Exit;
        pSTR := '';
        pCT1 := Length ( pTMP ) div 2;
        for pCT := 0 to (Length ( pTMP ) div 2) -1 do
          pStr := pStr + Char ( (Pos ( pTMP[pCt*2+1] , cHexChars ) -1) * 16 + (Pos ( pTMP[pCt*2+2] , cHexChars ) -1));
     end;
     if pCT1 = 0 then Exit;
     GetMem ( FindBuf , pCT1 );
     try
        if FindStr[1] = 'T' then FindICase := True;
        FindLen := pCT1;
        Move ( pStr[1] , FindBuf^, pCt1 );
        FindPos := HexEditor.Find ( FindBuf , FindLen , HexEditor.GetCursorPos , HexEditor.DataSize -1 , FindICase , UpCase(FindStr[1]) = 'T' );
        if FindPos = -1 then
           ShowMessage ( 'Data not found.' )
        else begin
           HexEditor.SelStart := FindPos+FindLen-1;
           HexEditor.SelEnd := FindPos ;
        end;
     finally
     end;
end;

procedure TfrmClient.HexFindAgain;
begin
     if HexEditor.DataSize < 1 then Exit;
     if FindStr = '' then
         HexFind
     else begin
        if FindPos = HexEditor.SelEnd then Inc ( FindPos , 1 );
        FindPos := HexEditor.Find ( FindBuf , FindLen , FindPos , HexEditor.DataSize -1 , FindICase , False);
        if FindPos = -1 then
           ShowMessage ( 'Data not found.' )
        else begin
           HexEditor.SelStart := FindPos+FindLen-1;
           HexEditor.SelEnd := FindPos ;
        end;
     end;
end;

procedure tfrmClient.HexGoto;
var
   sr : string;
   s1 : LongInt;
begin
     if HexEditor.DataSize < 1 then Exit;
     s1 := HexEditor.GetCursorPos;
     sr := IntToStr ( s1 );
     if InputQuery ( 'Goto file position...' ,
                     '(prefix "0x" or "$" means hex)', sr ) then
     begin
        if Pos ( '0x' , AnsiLowerCase ( sr ) ) = 1 then sr := '$'+Copy ( sr , 3 , MaxInt );
        s1 :=  StrToIntDef ( sr , -1 );
        if not HexEditor.Seek ( s1 , soFromBeginning , True ) then ShowMessage( 'Invalid position' )
     end;
end;

procedure TfrmClient.SetJumpOffset;
var
   sr : string;
begin
   sr := IntToStr ( JumpOffs );
   if InputQuery ( 'Jump amount' , 'Enter new value:' , sr ) then JumpOffs := StrToIntDef ( sr , JumpOffs );
end;

procedure TfrmClient.HexJump(Direction : Boolean);
begin
   case Direction of
      True  : HexEditor.Seek ( JumpOffs , soFromCurrent , False );
      False : HexEditor.Seek ( -JumpOffs , soFromCurrent , False );
   end;
end;

procedure TfrmClient.OnFilesDropped(Sender: TObject; FileList: TStrings; AtPoint: TPoint);
var n : integer;
begin
  //Files dropped on tSyntaxMemo;
  for n := 0 to FileList.Count - 1 do
     frmMain.OpenFile(FileList[n], '');
end;

procedure TfrmClient.actLineNosExecute(Sender: TObject);
begin
   actLineNos.Checked := not actLineNos.Checked;
   case actLineNos.Checked of
      True  : synMDI.Options := synMDI.Options + [smoLineNumbers];
      False : synMDI.Options := synMDI.Options - [smoLineNumbers];
   end;
end;

procedure TfrmClient.synMDIHyperlinkClick(Sender: TObject;
  HyperData: String; HyperType: Integer);
begin
  If LaunchDouble or (not frmMain.DoURLS) then exit;
  If HyperType in [et_WEB, et_MAIL] then
    frmMain.URL(HyperData);
end;

procedure TfrmClient.ShowResults(Show : boolean);
var reg : tRegistry;
    wn  : Integer;
begin
  If not (emShowingResults in Options) then
  begin
    // Get the current default position for the results pane.
    Reg := tRegistry.Create;
    try
      Reg.OpenKey(rootkey, true);
      wn := Reg.ReadInteger('ResultsPos');
      Reg.Free;
    Except
      wn := 1;
      Reg.WriteInteger('ResultsPos', wn);
      Reg.Free;
    end;
    Case wn of
      0 : ResultsPos := rpTop;
      1 : ResultsPos := rpSide;
      2 : ResultsPos := rpBottom;
    else
      ResultsPos := rpSide;
    end;
  end;
  Case show of
    True  : CreateResults(ResultsPos);
    False : FreeResults;
  end;
end;

procedure TfrmClient.CreateResults(fPos : tResultPosition);
var na : boolean;
    Settings : tRegistry;
    wn       : integer;
begin
  na := false;
  If not assigned(ResultsWin) then na := true;
  If na then
  begin
    ResultsSep := tSplitter.Create(Self);
    ResultsWin := tSyntaxMemo.Create(Self);
    ResultsSep.Parent := Self;
    ResultsWin.Parent := Self;
    ResultsWin.Parser1 := frmMain.Parsers.parResults;
    ResultsWin.ActiveParser := 1;
    ResultsWin.Options := [smoSyntaxHighlight, smoWordWrap, smoWordSelect,
                                        smoShowWrapGlyph, smoOLEDragSource];
    ResultsWin.VisiblePropEdPages := [ppHIGHLIGHTING];
    ResultsWin.DialogInterface.HiddenSheets := [1,3,4,5];
    ResultsWin.DialogInterface.TabStyle := tsTabPages;
    ResultsWin.HyperCursor := crHandPoint;
    ResultsWin.OnHyperlinkClick := ResultsLineClick;
    ResultsWin.ReadOnly := True;
    ResultsWin.PopupMenu := popResults;
  end;
  Case fPos of
    rpTop :
    begin
      If ResultsPos = rpSide then
      begin
        ResultsSep.Hide;
        ResultsWin.Hide;
      end;
      ResultsSep.Height := 3;
      If ResultsPos = rpBottom then
      begin
        If ResultsWin.Height > (Self.Height div 2) then ResultsWin.Height := Self.Height div 2;
        ResultsWin.Align := alTop;
        ResultsSep.Align := alTop;
      end else
      begin
        ResultsSep.Align := alTop;
        ResultsWin.Align := alTop;
      end;
      If ResultsPos = rpSide then
      begin
        ResultsSep.Show;
        ResultsWin.Show;
      end;
      wn := 0;
    end;
    rpBottom :
    begin
      ResultsWin.Align := alBottom;
      ResultsSep.Height := 3;
      ResultsSep.Align := alBottom;
      wn := 2;
    end;
    rpSide :
    begin
      ResultsWin.Align := alRight;
      ResultsSep.Width := 3;
      ResultsSep.Align := alRight;
      wn := 1;
    end;
  else wn := 0;
  end;
  ResultsPos := fPos;
  Settings := TRegistry.Create;
  Settings.OpenKey(rootkey, True);
  Settings.WriteInteger('ResultsPos', wn);
  Settings.CloseKey;
  Settings.Free;
  Options := Options + [emShowingResults];
end;

procedure TfrmClient.SetResults(msg : tStrings);
begin
  // Set the results from a tSTrings;
  ResultsWin.Lines.Clear;
  ResultsWin.Lines.AddStrings(msg);
end;

procedure TfrmClient.FreeResults;
begin
  If assigned(ResultsSep) then
  begin
    ResultsSep.Free;
    ResultsSep := nil;
  end;
  If assigned(ResultsWin) then
  begin
    ResultsWin.Free;
    ResultsWin := nil;
  end;
  Options := Options - [emShowingResults];
end;

procedure TfrmClient.popResultsPopup(Sender: TObject);
begin
  Case ResultsPos of
    rpTop    : itmTop.Checked := True;
    rpSide   : itmRight.Checked := True;
    rpBottom : itmBottom.Checked := True;
  end;
end;

procedure TfrmClient.itmTopClick(Sender: TObject);
begin
  Case tMenuItem(Sender).Tag of
    1 : CreateResults(rpTop);
    2 : CreateResults(rpSide);
    3 : CreateResults(rpBottom);
  end;
end;

procedure TfrmClient.synMDIDblClick(Sender: TObject);
begin
  If (not LaunchDouble) or (not frmMain.DoURLs) then exit;
  LaunchURLHandler(Sender);
end;

procedure TfrmClient.synMDIMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mpos := Point(X, Y);
end;

procedure TfrmClient.NotifySettings(var msg: TMessage);
begin
  LaunchDouble := frmMain.GetSetting('URLLaunchDouble', netkey, true);
  Case frmMain.actPrintMargin.Checked of
    True  : synMDI.Options := synMDI.Options + [smoShowRMargin];
    False : synMDI.Options := synMDI.Options - [smoShowRMargin];
  End;
  case frmMain.GetSetting('HighlightOnContext', EditKey, True) of
    True  : Options := Options + [emShowHighlightMenu];
    False : Options := Options - [emShowHighlightMenu];
  end;  
  synMDI.KeepUndoOnSaveFile := not frmMain.LoseUndo;
  synMDI.Font.Name := Config.Fontname;
  synMDI.Font.Size := Config.FontSize;
end;

procedure TfrmClient.CopyURLHandler(Sender: TObject);
var mpIdx   : longint;
    ID      : string;
    IDIdx   : longint;
begin
  with synMDI do begin
    //
    // Get mouse index
    //
    mpIdx := Perform(EM_CHARFROMPOS, 0, longint(@mpos));
    //
    // Identifier at mouse ?
    //
    if (TokenAtCharPos[mpIdx] = et_WEB) or (TokenAtCharPos[mpIdx] = et_MAIL) then
    begin
       //
       // Pick up ID name
       //
       ID := TokenTextAtPos(mpIdx, IDIdx);
       Clipboard.SetTextBuf(pChar(ID));
      end;
   end;
end;

procedure TfrmClient.LaunchURLHandler(Sender : TObject);
var mpIdx   : longint;
    ID      : string;
    IDIdx   : longint;
begin
  with synMDI do begin
    //
    // Get mouse index
    //
    mpIdx := Perform(EM_CHARFROMPOS, 0, longint(@mpos));
    //
    // Identifier at mouse ?
    //
    if (TokenAtCharPos[mpIdx] = et_WEB) or (TokenAtCharPos[mpIdx] = et_MAIL) then
    begin
       //
       // Pick up ID name
       //
       ID := TokenTextAtPos(mpIdx, IDIdx);
       frmMain.URL(ID);
      end;
   end;
end;

procedure TfrmClient.CheckForURL;
var mpIdx   : longint;
    mi      : TMenuItem;
begin
  with synMDI do
  begin
    //
    // Get mouse index
    //
    mpIdx := Perform(EM_CHARFROMPOS, 0, longint(@mpos));
    //
    // Identifier at mouse ?
    //
    if (TokenAtCharPos[mpIdx] = et_WEB) or (TokenAtCharPos[mpIdx] = et_MAIL) then
    begin
       //
       // Pick up ID name
       //
       mi := tMenuItem.Create(Self);
       mi.Caption := '-';
       mi.Tag := 55;
       popEditor.Items.Add(mi);
       mi := tMenuItem.Create(Self);
       mi.Caption := '&Launch URL';
       mi.OnClick := LaunchURLHandler;
       mi.Tag := 55;
       popEditor.Items.Add(mi);
       mi := tMenuItem.Create(Self);
       mi.Caption := 'Copy URL to Clipboard';
       mi.OnClick := CopyURLHandler;
       mi.Tag := 55;
       popEditor.Items.Add(mi);
      end;
  end;
end;

procedure TfrmClient.AddHighlightMenu;
var
  mi : TMenuItem;
begin
  mi := TMenuItem.Create(popEditor);
  mi.Tag := 56;
  mi.Caption := 'Highlight';
  popEditor.Items.Insert(6, mi);
  frmMain.SetHighlightMenu(mi);
end;

procedure TfrmClient.popEditorPopup(Sender: TObject);
var
  n : integer;
  i : integer;
begin
  // Clear Highlight Menu...
  if emShowHighlightMenu in Options then
  begin
    i := -1;
    for n := 0 to popEditor.Items.Count - 1 do
    begin
      if popEditor.Items[n].Tag = 56 then i := n;
    end;
    if i > -1 then
    for n := popEditor.Items[i].Count - 1 downto 0 do
      popEditor.Items[i].Delete(n);
  end;
  // Clear Dynamic Menu Items...
  For n := popEditor.Items.Count - 1 downto 0 do
    IF (popEditor.Items[n].Tag = 55) or (popEditor.Items[n].Tag = 56) then
      popEditor.Items.Delete(n);
  CheckForURL;
  if emShowHighlightMenu in Options then AddHighlightMenu;
end;

procedure TfrmClient.actShowOutputExecute(Sender: TObject);
begin
  If emShowingResults in Options then
    ShowResults(False) else
    ShowResults(True);
end;

procedure TfrmClient.hide1Click(Sender: TObject);
begin
  ShowResults(False);
end;

procedure TfrmClient.Configure1Click(Sender: TObject);
begin
  if emShowingResults in Options then
    ResultsWin.ModifyProperties;
end;

procedure TfrmClient.ResultsLineClick(Sender: TObject; HyperData: String; HyperType: Integer);
var
  ln : integer;
begin
  If not (emShowingResults in Options) then exit;
  If HyperType = 6 then
  begin
    ln := GetNumbers(HyperData);
    JumpToLineNumber(ln, true);
  end;
end;

procedure TfrmClient.JumpToLineNumber(ln : integer; select : boolean);
var
  idx : integer;
begin
  idx := synMDI.Perform(em_lineindex, ln-1, 0);
  synMDI.SelStart := idx;
  if select then
  begin
    synMDI.SelLength := Length(synMDI.Lines[ln-1]);
    synMDI.Perform(em_scrollcaret, 0, 0);
  end;
  synMDI.SetFocus;
end;

procedure TfrmClient.SetModified(const Value: Boolean);
begin
  if fModified <> Value then
  begin
    fModified := Value;
    frmMain.tabs.Repaint;
  end else
    fModified := Value;
end;

procedure TfrmClient.AddResult(Msg: string);
begin
  ResultsWin.Lines.Add(Msg);
  synMDI.Perform(em_scrollcaret, 0, 0);
end;

procedure TfrmClient.ClearResults;
begin
  if Assigned(ResultsWin) then ResultsWin.Lines.Clear;
end;

procedure TfrmClient.CheckAge;
var
  i : Integer;
begin
  if IsNewFile then Exit;
  if _doingagecheck then Exit;
  if FileName = '' then Exit;
  _doingagecheck := True;
  if FileExists(FileName) then
  try
    i := FileAge(FileName);
    if i <> Age then
    begin
      Application.ProcessMessages;
      if MessageDlg(FileName + #13#10#13#10 + 'The above file has been modified outside of Programmers Notepad, do '+#13+#10+'you want to refresh and lose changes?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        // The user would like to refresh!
        Revert;
      end;
    end;
    Age := i;
  finally
    _doingagecheck := False;
  end;
end;

procedure TfrmClient.Revert;
var
  i : Integer;
  ext : string;
begin
  case Mode of
    emNormal :
    begin
      ext := ExtractFileExt(FileName);
      PrivateOpenFile(FileName, ext);
      synMDI.Modified := False;
      Modified := False;
      for i := 1 to synMDI.Lines.Count do
        if synMDI.LineHasGlyph(16, i) then
          synMDI.RemoveLineGlyph(16, i);
      end;
    emHex :
    begin
      HexEditor.LoadFromFile(FileName);
      HexEditor.Modified := False;
      Modified := False;
    end;
  end;
end;

procedure TfrmClient.SilentReplaceAll(findstr, replacestr : string; FindOptions : integer);
var
  iStartPoint : Integer;
  rText       : string;
  aSel        : TChRange;
begin
  with synMDI do
  begin
    // Begin an UNDO group...
    UndoTransBegin(udREPLACEALL);
    iStartPoint := 0;
    Lines.BeginUpdate;

    repeat
      Perform(EM_SETSEL, iStartPoint, iStartPoint);

      if (not FindReplace(findstr, replacestr, iStartPoint, rText, findoptions)) then
         Break;

       Perform(SEM_SELECTION,  eaGET, longint(@aSel));
       aSel := Normalise(aSel);
       Perform(SEM_REPLACESEL, euNODRAW, longint(PChar(rText)));

       Perform(SEM_SELECTION,  eaGET, longint(@aSel));
       aSel := Normalise(aSel);
       iStartPoint := aSel.chEnd;

    until False;

    Lines.EndUpdate;
    // End an UNDO group...
    UndoTransEnd(udREPLACEALL);

    Perform(SEM_SELECTION, eaGET, longint(@aSel));
    Perform(SEM_SELECTION, eaSET or euDRAW, longint(@aSel));
  end;
  fModified := True;
end;

procedure TfrmClient.TabsToSpaces;
var
  spacestr    : string;
  i           : Integer;
begin
  spacestr := '';
  for i := 0 to 3 do
    spacestr := spacestr + ' ';

  SilentReplaceAll('\t', spacestr, ft_REGEXPR);
end;

procedure TfrmClient.SpacesToTabs;
begin
  SilentReplaceAll('    ', '\t', ft_REGEXPR);
end;

end.


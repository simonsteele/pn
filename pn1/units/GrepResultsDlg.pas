{***************************************************************
 *
 * Unit Name   : GrepResultsDlg
 * Date        :
 * Purpose     : Grep Search Result Dialog
 * Copyright   : This Source Code is taken from GExperts, the excellent
 * 			     Delphi/C++Builder add-on available from GExperts.org.
 *				 Please see the file gexpertslicense.html for the license.
 *				 Any modifications from the original are copyright Echo
 *				 Software.
 * History     :
 *       2000-02-19 MBCS Support
 *               29/05/2000 Moved button code into actions, and
 *               replaced speedbuttons with Toolbar97 buttons.
 *
 ****************************************************************}

unit GrepResultsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SearchFile, ComCtrls, {AppUtils,} Registry,
  DropSource, ActnList, ImgList, TB97Tlbr, TB97Ctls, TB97, Menus
  {, vgNLS};

type
  TGrepAction = (gaAllFilesGrep, gaCurrentOnlyGrep, gaOpenFilesGrep, gaDirGrep);

  TSearchResult = class(TCollectionItem)
  private
    FLine: string;
    FLineNo: Integer;
    FSPos: Integer;
    FEPos: Integer;
  published
    property Line: string read FLine write FLine;
    property LineNo: Integer read FLineNo write FLineNo;
    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
  end;

  TSearchResults = class(TCollection)
  private
    FExpanded: Boolean;
    FFileName: string;
    function GetItem(Index: Integer): TSearchResult;
    procedure SetItem(Index: Integer; Value: TSearchResult);
  public
    constructor Create;
    function Add: TSearchResult;
    property Expanded: Boolean read FExpanded write FExpanded;
    property FileName: string read FFileName write FFileName;
    property Items[Index: Integer]: TSearchResult read GetItem write SetItem; default;
  end;

  // Saved grep settings (used for refresh)
  TGrepSettings = packed record
    NoComments,
      NoCase,
      WholeWord,
      RegEx,
      IncludeSubdirs: Boolean;
    Directory,
      Mask,
      Pattern: string;
    GrepAction: TGrepAction;
    CanRefresh: Boolean;
  end;

  TfrmGrepResults = class(TForm)
    StatusBar: TStatusBar;
    dlgGrepFont: TFontDialog;
    lbResults: TListBox;
    ilsGrep: TImageList;
    alsGrep: TActionList;
    actGrep: TAction;
    actRefresh: TAction;
    actAbort: TAction;
    actGotoLine: TAction;
    actPrint: TAction;
    actContract: TAction;
    actExpand: TAction;
    actFont: TAction;
    actGrepReplace: TAction;
    dokGrep: TDock97;
    tbrGrep: TToolbar97;
    ToolbarButton973: TToolbarButton97;
    ToolbarButton974: TToolbarButton97;
    ToolbarSep971: TToolbarSep97;
    ToolbarButton975: TToolbarButton97;
    ToolbarSep972: TToolbarSep97;
    ToolbarButton976: TToolbarButton97;
    ToolbarSep973: TToolbarSep97;
    ToolbarButton977: TToolbarButton97;
    ToolbarButton978: TToolbarButton97;
    ToolbarButton979: TToolbarButton97;
    ToolbarSep974: TToolbarSep97;
    ToolbarButton9710: TToolbarButton97;
    popGrepResults: TPopupMenu;
    ClearResults1: TMenuItem;
    N1: TMenuItem;
    Hide1: TMenuItem;
    actClear: TAction;
    actHide: TAction;
    ToolbarButton971: TToolbarButton97;
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuRefreshClick(Sender: TObject);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure actGrepExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAbortExecute(Sender: TObject);
    procedure actGotoLineExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actContractExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
    procedure actFontExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure lbResultsDblClick(Sender: TObject);
    procedure MakeVisible;
    procedure ClearResults1Click(Sender: TObject);
    procedure Hide1Click(Sender: TObject);
  private
    Total: Integer;
    DragSource: TDropFileSource;
    DragPoint: TPoint;
    {tran: TvgTranslator;}
    procedure Foundit(Sender: TObject; LineNo: Integer; Line: string; SPos, EPos: Integer);
    procedure StartSearch(Sender: TObject);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ExpandContract(n: Integer);
    procedure ResizeListBox;
  protected
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
  public
    GrepSettings: TGrepSettings;
    SAbort: Boolean;
    Searching: Boolean;
    OpenFiles: Boolean;
    Results: TSearchResults;
    Searcher: TSearcher;
    FileCount: Integer;
    IsDocked: Boolean;
    procedure Execute(Refresh: Boolean); overload;
    procedure Execute(Refresh: Boolean; SString : String); overload;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  end;

var
  frmGrepResults: TfrmGrepResults = nil;

const
  secGrepResult = 'GrepResult';
  SProcessing              = 'Processing: ';
  SNoFileOpen              = 'No file is currently open.';
  SGrepActive              = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepStatistics          = '%d files in %g seconds';
  SMatches                 = ' matches';
  SCouldNotOpenFile        = 'Could not open file:';
  SItemMatches             = 'Matches: ';

procedure SaveFont(Reg: TRegistry; Font: TFont);
procedure LoadFont(Reg: TRegistry; Font: TFont);

implementation

{$R *.DFM}

uses
  GrepSearchDlg,
  main, editor,
  ShellAPI;

procedure SaveFont(Reg: TRegistry; Font: TFont);
begin
  with Reg do
  begin
    // Do not localize any of the following strings
    WriteString('Name', Font.Name);
    WriteInteger('Size', Font.Size);
    WriteBool('Bold', (fsBold in Font.Style));
    WriteBool('Italic', (fsItalic in Font.Style));
    WriteBool('Underline', (fsUnderline in Font.Style));
  end;
end;

procedure LoadFont(Reg: TRegistry; Font: TFont);
begin
  with Reg do
  begin
    // Do not localize any of the following strings
    Try
      Font.Name := ReadString('Name');
      Font.Size := ReadInteger('Size');
      if ReadBool('Bold') then
        Font.Style := Font.Style + [fsBold];
      if ReadBool('Italic') then
        Font.Style := Font.Style + [fsItalic];
      if ReadBool('Underline') then
        Font.Style := Font.Style + [fsUnderLine];
    Except
      Font.Name := 'MS Sans Serif';
      Font.Size := 8;
      Font.Style := [];
    end;
  end;
end;

function MyTrim(var st: string): Integer;
begin
  Result := 0;
  while (Length(st) > 0) and (st[1] in [#9, #32]) do
  begin
    Delete(st, 1, 1);
    Inc(Result);
  end;
end;

constructor TSearchResults.Create;
begin
  inherited Create(TSearchResult);
end;

function TSearchResults.Add: TSearchResult;
begin
  Result := TSearchResult(inherited Add);
end;

function TSearchResults.GetItem(Index: Integer): TSearchResult;
begin
  Result := TSearchResult(inherited GetItem(Index));
end;

procedure TSearchResults.SetItem(Index: Integer; Value: TSearchResult);
begin
  inherited SetItem(Index, Value);
end;

procedure TfrmGrepResults.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGrepResults.StartSearch(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Text := SProcessing{tran.TMsg(SProcessing)} + Searcher.FileName;
  StatusBar.Repaint;
end;

procedure TfrmGrepResults.Foundit(Sender: TObject; LineNo: Integer; Line: string; SPos, EPos: Integer);
var
  AResult: TSearchResult;
begin
  Application.ProcessMessages;
  Inc(Total);
  if (Results = nil) or (Results.FileName <> Searcher.FileName) then
  begin
    Results := TSearchResults.Create;
    Results.FileName := Searcher.FileName;
    lbResults.Items.AddObject(Searcher.FileName, Results);
  end;
  AResult := Results.Add;
  AResult.Line := Line;
  AResult.LineNo := LineNo;
  AResult.SPos := SPos;
  AResult.EPos := EPos;
end;

procedure TfrmGrepResults.Execute(Refresh: Boolean);
begin
  Execute(Refresh, '');
end;

procedure TfrmGrepResults.Execute(Refresh: Boolean; SString : String);
var
  Dlg: TfrmGrepSearch;
  SStart: Integer;
  SEnd: Integer;

  procedure CurrentOnlyGrep;
  var
    CurrentFile  : string;
    fEditor      : tfrmClient;
  begin
    Results := nil;
    fEditor := frmMain.GetCurrentEditor;
    if Assigned(fEditor) then
      CurrentFile := fEditor.FileName;
    if CurrentFile <> '' then
    begin
      Searcher.FileName := CurrentFile;
      Searcher.Execute;
      Inc(FileCount);
    end
    else
      MessageDlg(SNoFileOpen{tran.TMsg(SNoFileOpen)}, mtError, [mbOK], 0);
  end;

  procedure AllFilesGrep;
  var
    I: Integer;
  begin
    if OpenFiles then
      with frmMain do
      begin //Current Open Files in Editor.
        for I := 0 to tabs.Tabs.Count - 1 do
        begin
          Searcher.FileName := TfrmClient(Tabs.Tabs.Objects[I]).FileName;
          Searcher.Execute;
          Inc(FileCount);
          if SAbort then Break;
        end;
      end
  end;

  procedure DirGrep(Dir, Mask: string);
  var
    Search: TSearchRec;
    Result: Integer;
    S: TStringList;
    i: Integer;
  begin
    if dir[Length(dir)] <> '\' then Dir := Dir + '\';
    S := TStringList.Create;
    try
      for i := 1 to Length(Mask) do
        if Mask[i] in [';', ','] then
          Mask[i] := #13;

      S.Text := Mask;

      { First do sub-directories if option is selected }
      if GrepSettings.IncludeSubdirs then
      begin
        Result := FindFirst(Dir + '*.*', faAnyFile, Search);
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
            begin
              if (Search.Name <> '.') and (Search.Name <> '..') then
                DirGrep(Dir + Search.Name, Mask);
            end;
            Result := FindNext(Search);
          end;
        finally
          FindClose(Search);
        end;
      end;

      for i := 0 to S.Count - 1 do
      begin
        Result := FindFirst(Dir + Trim(S.Strings[i]), faAnyFile, Search);
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
            begin
              Result := FindNext(Search);
            end
            else
            begin
              Results := nil;
              Searcher.FileName := Dir + Search.Name;
              Searcher.Execute;

              Application.ProcessMessages;
              if SAbort then Break;

              Inc(FileCount);
              Result := FindNext(Search);
            end;
          end;
        finally
          FindClose(Search);
        end;
      end;
    finally
      S.Free;
    end;
  end;
var
  GrepANSI: Boolean;
begin
  GrepANSI := False;
  //! StH: This code needs some cleanup attention
  if Searching then
  begin
    MessageDlg(SGrepActive{tran.TMsg(SGrepActive)}, mtInformation, [mbOK], 0);
    Exit;
  end;

  if not (Refresh and GrepSettings.CanRefresh) then
  begin
    Dlg := TfrmGrepSearch.Create(nil);
    try
      Dlg.cbText.Text := SString;
      if Dlg.ShowModal <> mrOk then
        Exit;

        // Save Dialog settings to local vars
      GrepSettings.NoComments := Dlg.chkNoComments.Checked;
      GrepSettings.NoCase := Dlg.chkNoCase.Checked;
      GrepSettings.WholeWord := Dlg.chkWholeWord.Checked;
      GrepSettings.RegEx := Dlg.chkRegEx.Checked;
      GrepSettings.Pattern := Dlg.cbText.Text;
      GrepSettings.Directory := Dlg.cbDirectory.Text;
      if GrepSettings.Pattern = '' then exit;
      GrepSettings.IncludeSubdirs := Dlg.chkInclude.Checked;
      if Dlg.rbAllFiles.Checked then
        GrepSettings.GrepAction := gaAllFilesGrep
      else if Dlg.rbCurrentOnly.Checked then
        GrepSettings.GrepAction := gaCurrentOnlyGrep
      else if Dlg.rbOpenFiles.Checked then
        GrepSettings.GrepAction := gaOpenFilesGrep
      else
      begin
        GrepSettings.Directory := Dlg.cbDirectory.Text;
        if GrepSettings.Directory = '' then exit;
        GrepSettings.Mask := Dlg.cbMasks.Text;
        GrepSettings.GrepAction := gaDirGrep;
      end;
      GrepSettings.CanRefresh := True;
      GrepANSI := Dlg.chkGrepANSI.Checked;
    finally
      Dlg.Free;
    end;
  end;

  try
    Searching := True;
    frmMain.FloatFIF.Visible := True;
    Visible := True;
    FormResize(Self);
    Total := 0;
    FileCount := 0;
    SAbort := False;
    OpenFiles := False;

    actGrep.Enabled := False;
    actRefresh.Enabled := False;
    actPrint.Enabled := False;
    actGotoLine.Enabled := False;
    actExpand.Enabled := False;
    actContract.Enabled := False;
    actFont.Enabled := False;
    actAbort.Enabled := True;

    SStart := GetTickCount;
    Self.Cursor := crHourglass;
    Searcher := TSearcher.Create('');
    try
      Searcher.BufSize := 30000;
      Searcher.OnFound := FoundIt;
      Searcher.OnStartSearch := StartSearch;

      Searcher.NoComments := GrepSettings.NoComments;
      if GrepSettings.NoCase then
        Searcher.SearchOptions := [soCaseSensitive];
      if GrepSettings.WholeWord then
        Searcher.SearchOptions := Searcher.SearchOptions + [soWholeWord];
      if GrepSettings.RegEx then
        Searcher.SearchOptions := Searcher.SearchOptions + [soRegEx];
      Searcher.ANSICompatible := GrepANSI;

      lbResults.Clear;
      Searcher.SetPattern(GrepSettings.Pattern);

      Application.ProcessMessages;
      case GrepSettings.GrepAction of
        gaAllFilesGrep: AllFilesGrep;
        gaCurrentOnlyGrep: CurrentOnlyGrep;
        gaOpenFilesGrep:
          begin
            OpenFiles := True;
            AllFilesGrep;
          end;
        gaDirGrep:
          begin
            if Length(Trim(GrepSettings.Mask)) = 0 then
              DirGrep(GrepSettings.Directory, '*.pas')
            else
              DirGrep(GrepSettings.Directory, UpperCase(GrepSettings.Mask));
          end;
      end; // end case
    finally
      Searching := False;

      SEnd := GetTickCount;
      Searcher.Free;
      Self.Cursor := crDefault;

      StatusBar.Panels.Items[0].Text := Format(SGrepStatistics{tran.TMsg(SGrepStatistics)}, [FileCount, (SEnd - SStart) / 1000]);

      lbResults.Refresh;
      lbResults.Sorted := True;
      lbResults.Sorted := False;
      if lbResults.Items.Count = 1 then
      begin
        lbResults.ItemIndex := 0;
        actExpandExecute(actExpand);
      end;
    end;
  finally
    actPrint.Enabled := True;
    actGrep.Enabled := True;
    actRefresh.Enabled := True;
    actExpand.Enabled := True;
    actContract.Enabled := True;
    actFont.Enabled := True;
    actGotoLine.Enabled := True;
    actAbort.Enabled := False;
  end;
  StatusBar.Panels.Items[1].Text := IntToStr(Total) + SMatches{tran.TMsg(SMatches)};
  //frmGrepResults.ManualDock(frmMain.panBottomDock);
end;

procedure TfrmGrepResults.lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  //Make sure mouse has moved at least 10 pixels before starting drag ...
  if (DragPoint.X = -1) or ((Shift <> [ssLeft]) and (Shift <> [ssRight])) or
    ((Abs(DragPoint.X - X) < 10) and (Abs(DragPoint.Y - Y) < 10)) then Exit;

  i := lbResults.ItemAtPos(Point(X, Y), True);

  if (i > -1) then
  begin
    DragSource.Files.Clear;
    if lbResults.Items.Objects[i] is TSearchResults then
      DragSource.Files.Add(TSearchResults(lbResults.Items.Objects[i]).FFileName)
    else if lbResults.Items.Objects[i] is TSearchResult then
      DragSource.Files.Add(TSearchResults(TSearchResult(lbResults.Items.Objects[i]).Collection).FFileName);
    if DragSource.Files.Count > 0 then
      DragSource.Execute;
  end;
end;

procedure TfrmGrepResults.lbResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DragPoint := Point(X, Y);
end;

procedure TfrmGrepResults.FormResize(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Width := StatusBar.Width - 100;
  SetBounds(0,0, frmMain.panFIFContainer.Width, frmMain.panFIFContainer.Height);
  tbrGrep.Invalidate;
  Invalidate;
end;

procedure TfrmGrepResults.FormDestroy(Sender: TObject);
begin
  actClearExecute(actClear);
  SaveSettings;
  frmGrepResults := nil;
  SAbort := True;
  DragSource.Free;
  DragSource := nil;
  inherited;
end;

procedure TfrmGrepResults.SaveSettings;
var
  Reg : TRegistry;
begin
  // do not localize any of the below strings
  Reg := TRegistry.Create;
  Reg.OpenKey(GrepKey, True);
  try
    SaveFont(Reg, lbResults.Font);
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure TfrmGrepResults.LoadSettings;
var
  Reg : TRegistry;
begin
  // do not localize any of the below strings
  Reg := TRegistry.Create;
  Reg.OpenKey(GrepKey, True);
  try
    LoadFont(Reg, lbResults.Font);
  finally
    Reg.Free;
  end;
end;

procedure TfrmGrepResults.FormCreate(Sender: TObject);
begin
  inherited;
  {tran := TvgTranslator.Create(Self);}
  Searching := False;
  LoadSettings;
  ResizeListBox;
  DragSource := TDropFileSource.Create(nil);
end;

procedure TfrmGrepResults.lbResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: Integer;
begin
  if Button = mbLeft then
  begin
    p := lbResults.ItemAtPos(Point(X, Y), True);
    if p <> -1 then
      if lbResults.Items.Objects[p] is TSearchResults then
        ExpandContract(p);
  end;
end;

procedure TfrmGrepResults.ExpandContract(n: Integer);
var
  Results: TSearchResults;
  i: Integer;
begin
  if (n < 0) or (n > lbResults.Items.Count - 1) or Searching then
    Exit;
  if lbResults.Items.Objects[n] is TSearchResults then
  begin
    try
      lbResults.Items.BeginUpdate;
      Results := TSearchResults(lbResults.Items.Objects[n]);
      if Results.Expanded then
      begin
        while (n + 1 <= lbResults.Items.Count - 1) and
          (not (lbResults.Items.Objects[n + 1] is TSearchResults)) do
        begin
          lbResults.Items.Delete(n + 1);
        end;
        Results.Expanded := False;
      end
      else
      begin
        for i := Results.Count - 1 downto 0 do
          lbResults.Items.InsertObject(n + 1, Results.Items[i].Line, Results.Items[i]);
        Results.Expanded := True;
      end
    finally
      lbResults.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmGrepResults.lbResultsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '+',
      '=',
      '-': ExpandContract(lbResults.ItemIndex);
    #13: actGotoLineExecute(actGotoLine);
  end;
end;

procedure TfrmGrepResults.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    if Searching then
      SAbort := True
    else
      Hide;
  end;
end;

procedure TfrmGrepResults.ResizeListBox;
begin
  with lbResults do
  begin
    Canvas.Font.Assign(Font);
    ItemHeight := Canvas.TextHeight('W') + 3; // "W" is any character
    Refresh;
  end;
end;

procedure TfrmGrepResults.lbResultsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  TopColor: TColor;
  BottomColor: TColor;
  ResultsCanvas: TCanvas;
  c: Integer;
  p: Integer;
  i: Integer;
  st: string;
  Result: TSearchResult;
  sb: TColor;
  sf: TColor;
  nb: TColor;
  nf: TColor;
begin
  ResultsCanvas := lbResults.Canvas;
  TopColor := clBtnHighlight;
  BottomColor := clBtnShadow;
  if lbResults.Items.Objects[Index] is TSearchResults then
  begin
    ResultsCanvas.Brush.Color := clBtnFace;
    ResultsCanvas.Font.Color := clBtnText;
    ResultsCanvas.FillRect(Rect);
    Rect.Right := Rect.Right + 2;
    if odSelected in State then
      Frame3D(ResultsCanvas, Rect, BottomColor, TopColor, 1)
    else
      Frame3D(ResultsCanvas, Rect, TopColor, BottomColor, 1);
    i := ResultsCanvas.TextWidth('+');
    ResultsCanvas.TextOut(Rect.Left + i + 8, Rect.Top, lbResults.Items[Index]);
    //c:=Rect.Top+((Rect.Bottom-Rect.Top) div 2);
    if TSearchResults(lbResults.Items.Objects[Index]).Expanded then
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '-')
    else
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '+');
    st := SItemMatches{tran.TMsg(SItemMatches)} + IntToStr(TSearchResults(lbResults.Items.Objects[Index]).Count);
    p := ResultsCanvas.TextWidth(SItemMatches + '00000') + 10; // do not localize
    if (ResultsCanvas.TextWidth(lbResults.Items[Index]) + i + 7) <= Rect.Right - p then
      ResultsCanvas.TextOut(lbResults.ClientWidth - p, Rect.Top, st);
  end
  else
  begin
    Result := TSearchResult(lbResults.Items.Objects[Index]);
    if odSelected in State then
    begin
      nb := clHighLight;
      nf := clHighLightText;
      sb := clWindow;
      sf := clWindowText;
    end
    else
    begin
      sb := clHighLight;
      sf := clHighLightText;
      nb := clWindow;
      nf := clWindowText;
    end;
    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(Rect);
    ResultsCanvas.TextOut(Rect.Left + 10, Rect.Top + 1, IntToStr(Result.LineNo));
    p := 60;
    st := lbResults.Items[Index];
    c := MyTrim(st);
    i := 1;
    while i <= Length(st) do
    begin
      if (i >= Result.SPos - c) and (i <= Result.EPos - c) then
      begin
        ResultsCanvas.Font.Color := sf;
        ResultsCanvas.Brush.Color := sb;
      end
      else
      begin
        ResultsCanvas.Font.Color := nf;
        ResultsCanvas.Brush.Color := nb;
      end;
      If ByteType(st, i) <> mbSingleByte Then
      Begin
        // It's a MBCS
        ResultsCanvas.TextOut(Rect.Left + p, Rect.Top + 1, Copy(st, i, 2));
        p := p + ResultsCanvas.TextWidth(Copy(st, i, 2));
        inc(i);
      end
      else begin
        ResultsCanvas.TextOut(Rect.Left + p, Rect.Top + 1, Copy(st, i, 1));
        p := p + ResultsCanvas.TextWidth(Copy(st, i, 1));
      End;
      inc(i);
    end;
  end;
end;

procedure TfrmGrepResults.WMExitSizeMove(var Message: TMessage);
begin
  lbResults.Repaint;
end;

procedure TfrmGrepResults.mnuRefreshClick(Sender: TObject);
begin
  Execute(True);
end;

procedure TfrmGrepResults.FormShow(Sender: TObject);
begin
  {tran.LanguageFile := CurrentLan;}
  {tran.Translate;}
end;

procedure TfrmGrepResults.FormDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  lbResults.Invalidate;
end;

procedure TfrmGrepResults.SpeedButton1Click(Sender: TObject);
begin
  lbResults.Invalidate;
  lbResults.Refresh;
  lbResults.Repaint;
  ShowMessage(IntToStr(lbResults.Items.Count));
end;

procedure TfrmGrepResults.Loaded;
begin
  inherited Loaded;
  Visible := false;
  Position := poDefault;
  BorderIcons := [];
  BorderStyle := bsNone;
  HandleNeeded;
end;

procedure TfrmGrepResults.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := frmMain.panFIFContainer.Handle;
  Params.Style := WS_CHILD or WS_CLIPSIBLINGS;
  Params.X := 0;
  Params.Y := 0;
  Params.Width := frmMain.panFIFContainer.Width;
  Params.Height := frmMain.panFIFContainer.Height;
end;

procedure TfrmGrepResults.actGrepExecute(Sender: TObject);
begin
  Execute(False);
end;

procedure TfrmGrepResults.actRefreshExecute(Sender: TObject);
begin
  Execute(True);
end;

procedure TfrmGrepResults.actAbortExecute(Sender: TObject);
begin
  sAbort := True;
end;

procedure TfrmGrepResults.actGotoLineExecute(Sender: TObject);
var
  Result          : TSearchResult;
  CurrentFileName : string;
  R               : TPoint;
  fEditor         : TfrmClient;
  n               : Integer;
  found           : Boolean;
begin
  fEditor := nil;
  found := False;
  if (lbResults.ItemIndex < 0) then
    Exit;
  if (lbResults.Items.Objects[lbResults.ItemIndex] is TSearchResults) then
  begin
    ExpandContract(lbResults.ItemIndex);
    Exit;
  end;
  Result := TSearchResult(lbResults.Items.Objects[lbResults.ItemIndex]);
  if Result = nil then Exit;
  with TSearchResults(Result.Collection) do
  begin
    CurrentFileName := FileName;
  end; //with
  R.X := Result.SPos;
  R.Y := Result.LineNo;

  for n := 0 to frmMain.MDIChildCount - 1 do
  begin
    if UpperCase(tfrmClient(frmMain.MDIChildren[n]).Filename) = uppercase(CurrentFileName) then
    begin
      frmMain.MDIChildren[n].BringToFront;
      fEditor := TfrmClient(frmMain.MDIChildren[n]);
      fEditor.synMDI.SelLength := 0;
      fEditor.synMDI.CaretPosition.OffsetLocation := R;
      found := True;
    end;
  end;
  if not found then
  begin
    frmMain.OpenFilePos(CurrentFileName, '', R);
    fEditor := frmMain.GetCurrentEditor;
  end;
  if Assigned(fEditor) then
  Begin
    fEditor.synMDI.SelLength := (Result.EPos + 1) - Result.SPos;
    fEditor.synMDI.Perform(EM_SCROLLCARET, 0, 0);
    Windows.SetFocus(fEditor.synMDI.Handle);
  end;
end;

procedure TfrmGrepResults.actPrintExecute(Sender: TObject);
var
  RichEdit: TRichEdit;
  Results: TSearchResults;
  Line: string;
  i, j, c: Integer;
  LinePos: Integer;

begin
  if lbResults.Items.Count = 0 then
    Exit;
  RichEdit := TRichEdit.Create(Self);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Self;
    RichEdit.Font.Name := 'Arial';
    RichEdit.Font.Size := 10;
    RichEdit.Clear;

    for i := 0 to lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResults then
      begin
        RichEdit.Lines.Add(''); // space between fileresults

        Results := TSearchResults(lbResults.Items.Objects[i]);

        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.Lines.Add(Results.FileName);
        RichEdit.SelAttributes.Style := [];

        for j := 0 to Results.Count - 1 do
        begin
          LinePos := RichEdit.GetTextLen;
          Line := Results.Items[j].Line;
          c := MyTrim(Line);
          with RichEdit do
          begin
            Lines.Add(Format('  %5d'#9, [Results.Items[j].LineNo]) + Line);
            // Now make the found Text bold
            SelStart := LinePos + 7 - c + Results.Items[j].SPos;
            SelLength := Results.Items[j].EPos - Results.Items[j].SPos + 1;
            SelAttributes.Style := [fsBold];
            SelLength := 0;
            SelAttributes.Style := [];
          end;
        end;
      end;
    RichEdit.Print('Grep Search Results');
  finally
    RichEdit.Free;
  end;
end;

procedure TfrmGrepResults.actContractExecute(Sender: TObject);
var
  i: Integer;
begin
  Self.Enabled := False;
  lbResults.Items.BeginUpdate;
  try
    i := 0;
    while i <= lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResult then
      begin
        lbResults.Items.Delete(i);
      end
      else
      begin
        TSearchResults(lbResults.Items.Objects[i]).Expanded := False;
        Inc(i);
      end;
  finally
    lbResults.Items.EndUpdate;
    Self.Enabled := True;
  end;
end;

procedure TfrmGrepResults.actExpandExecute(Sender: TObject);

  function Expand(n: Integer): Integer;
  var
    Results: TSearchResults;
    t: integer;
  begin
    Results := TSearchResults(lbResults.Items.Objects[n]);
    for t := Results.Count - 1 downto 0 do
      lbResults.Items.InsertObject(n + 1, Results.Items[t].Line, Results.Items[t]);
    Results.Expanded := True;
    Result := n + Results.Count - 1;
  end;

var
  i: integer;
begin
  Self.Enabled := false;
  lbResults.Items.BeginUpdate;
  try
    i := 0;
    while i <= lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResults then
      begin
        if not TSearchResults(lbResults.Items.Objects[i]).Expanded then
          i := Expand(i);
        Inc(i);
      end
      else
        Inc(i);
  finally
    lbResults.Items.EndUpdate;
    Self.Enabled := True;
  end;
end;

procedure TfrmGrepResults.actFontExecute(Sender: TObject);
begin
  dlgGrepFont.Font.Assign(lbResults.Font);
  if dlgGrepFont.Execute then
  begin
    lbResults.Font.Assign(dlgGrepFont.Font);
    ResizeListBox;
  end;

end;

procedure TfrmGrepResults.actClearExecute(Sender: TObject);
begin
  lbResults.Clear;
  actExpand.Enabled := False;
  actContract.Enabled := False;
  actGotoLine.Enabled := False;
  actPrint.Enabled := False;
end;

procedure TfrmGrepResults.lbResultsDblClick(Sender: TObject);
begin
  actGotoLineExecute(actGotoLine);
end;

procedure TfrmGrepResults.MakeVisible;
begin
  Visible := True;
  FormResize(Self);
end;

procedure TfrmGrepResults.ClearResults1Click(Sender: TObject);
begin
  lbResults.Clear;
end;

procedure TfrmGrepResults.Hide1Click(Sender: TObject);
begin
  frmMain.FloatFIF.Visible := False;
end;

end.


 {***************************************************************
 *
 * Unit Name: finddlg
 * Purpose  : Find Dialog
 * Author   : David Brock, mods by Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Portions by David Brock
 *			  Please read the license agreement at 
 *			  www.pnotepad.org/press/psidx.html.
 **************************************************************}
 
unit FindDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SyntaxEd;

type
  TfrmFind = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    FindHistoryBox: TComboBox;
    GroupBox1: TGroupBox;
    UseCaseBox: TCheckBox;
    WholeWordBox: TCheckBox;
    RegExBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox2: TGroupBox;
    FwdDirection: TRadioButton;
    BackDirection: TRadioButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GlobalScope: TRadioButton;
    SelectedScope: TRadioButton;
    CursorOrigin: TRadioButton;
    EntireOrigin: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FwdDirectionClick(Sender: TObject);
    procedure BackDirectionClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FindHistoryBoxChange(Sender: TObject);
    procedure GlobalScopeClick(Sender: TObject);
    procedure SelectedScopeClick(Sender: TObject);
    procedure CursorOriginClick(Sender: TObject);
    procedure EntireOriginClick(Sender: TObject);
  private
    { Private declarations }
    FFirstTime         : Boolean;
    FStartPoint        : longint;
    FOriginalSelection : TChRange;
    FFindText          : string;
    FFindOpt           : longint;
    FEditor            : TSyntaxMemo;
    procedure UpdateOptionBoxes;
    procedure ReadOptionsBoxes;
  public
    { Public declarations }
    procedure Execute(FindText: string; Options: longint; anEditor: TSyntaxMemo);
  end;

var
  frmFind: TfrmFind;

implementation

{$R *.DFM}

uses main;

{ TEdFindDlg }

procedure TfrmFind.Execute(FindText: string; Options: Integer; anEditor: TSyntaxMemo);
begin
  FFindText := FindText;
  FFindOpt  := Options;
  FEditor   := anEditor;
  FEditor.Perform(SEM_SELECTION, eaGET, longint(@FOriginalSelection));
  FOriginalSelection := Normalise(FOriginalSelection);
  FEditor.HideSelection := false;
  FFirstTime := true;
  Show;
end;

procedure TfrmFind.UpdateOptionBoxes;
begin
  UseCaseBox.Checked    := FFindOpt and ft_MATCHCASE <> 0;
  WholeWordBox.Checked  := FFindOpt and ft_WHOLEWORD <> 0;
  //RegExBox.Checked      := FFindOpt and ft_REGEXPR   <> 0;
  FwdDirection.Checked  := FFindOpt and ft_REVERSE   =  0;
  BackDirection.Checked := not (FwdDirection.Checked);
end;

procedure TfrmFind.ReadOptionsBoxes;
begin
  FFindOpt := 0;
  if UseCaseBox.Checked    then FFindOpt := FFindOpt or ft_MATCHCASE;
  if WholeWordBox.Checked  then FFindOpt := FFindOpt or ft_WHOLEWORD;
  if RegExBox.Checked      then FFindOpt := FFindOpt or ft_REGEXPR;
  if BackDirection.Checked then FFindOpt := FFindOpt or ft_REVERSE;
end;

procedure TfrmFind.FormShow(Sender: TObject);
begin
  Self.SetBounds(frmMain.Left + (frmMain.Width div 2) - (Self.Width div 2),
                frmMain.Top + (frmMain.Height div 2) - (Self.Height div 2),
                Self.Width,
                Self.Height);
  if FindHistoryBox.Items.IndexOf(FFindText) = -1 then FindHistoryBox.Items.Add(FFindText);
  FindHistoryBox.ItemIndex := FindHistoryBox.Items.IndexOf(FFindText);
  UpdateOptionBoxes;
  FindHistoryBoxChange(Self);
  FindHistoryBox.SetFocus;
end;

procedure TfrmFind.OKButtonClick(Sender: TObject);
var FoundAt : longint;
begin
  //
  // Initiate the find...
  //
  ReadOptionsBoxes;
  if FFirstTime then
  begin
     //
     // First time -- determine where to start the search
     //
   if GlobalScope.Checked then
     FStartPoint := 0
   else
     if EntireOrigin.Checked then
       if FwdDirection.Checked then
         FStartPoint := FOriginalSelection.chStart
       else
         FStartPoint := FOriginalSelection.chEnd
     else
       FStartPoint := FOriginalSelection.chStart;
   FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
  end
  else
    FEditor.Perform(EM_GETSEL, longint(@FStartPoint), longint(@FoundAt));

  FFirstTime := false;
  with FEditor do
  begin
    if (not RegExBox.Checked) then
      if (UseCaseBox.Checked and (SelText = FindHistoryBox.Text)) or
         ((not UseCaseBox.Checked) and (Lowercase(SelText) = Lowercase(FindHistoryBox.Text))) then
        if FwdDirection.Checked then
          FStartPoint := FStartPoint + 1
        else
          FStartPoint := FStartPoint - 1;
    FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
    if FindHistoryBox.Items.IndexOf(FindHistoryBox.Text) = -1 then FindHistoryBox.Items.Insert(0, FindHistoryBox.Text);
    if FindText(FindHistoryBox.Text, FoundAt, FFindOpt or ft_SILENT) and
       ( (not SelectedScope.Checked) or (SelectedScope.Checked and ((FoundAt >= FOriginalSelection.chStart)
                                         and (FoundAt <= FOriginalSelection.chEnd)))
       ) then
    begin
      if sizeof(TSM_char) = sizeof(WideChar) then
        if RegExBox.Checked then
          Perform(EM_SETSEL, CaretPosition.ByteOffsetFromCharOffset[FoundRESections[0]], CaretPosition.ByteOffsetFromCharOffset[FoundRESections[FoundRENumSections]])
        else
          Perform(EM_SETSEL, CaretPosition.ByteOffsetFromCharOffset[FoundAt],            CaretPosition.ByteOffsetFromCharOffset[FoundAt + length(FindHistoryBox.Text)])
      else
        if RegExBox.Checked then
          Perform(EM_SETSEL, FoundRESections[0], FoundRESections[FoundRENumSections])
        else
          Perform(EM_SETSEL, FoundAt,            FoundAt + length(FindHistoryBox.Text));

      FStartPoint := FoundAt;
    end
    else ShowMessage(format('"%s" not found', [FindHistoryBox.Text]));
  end;
end;

procedure TfrmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmFind.FwdDirectionClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       BackDirection.Checked := false;
      end;
end;

procedure TfrmFind.BackDirectionClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       FwdDirection.Checked := false;
      end;
end;

procedure TfrmFind.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FEditor.HideSelection := true;
end;

procedure TfrmFind.FindHistoryBoxChange(Sender: TObject);
begin
  OKButton.Enabled := FindHistoryBox.Text <> '';
end;

procedure TfrmFind.GlobalScopeClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       SelectedScope.Checked := false;
      end;
end;

procedure TfrmFind.SelectedScopeClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       GlobalScope.Checked := false;
      end;
end;

procedure TfrmFind.CursorOriginClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       EntireOrigin.Checked := false;
      end;
end;

procedure TfrmFind.EntireOriginClick(Sender: TObject);
begin
  with (Sender as TRadioButton) do
    if not Checked
     then begin
       Checked := true;
       CursorOrigin.Checked := false;
      end;
end;

end.

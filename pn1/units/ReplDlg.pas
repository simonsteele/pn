{***************************************************************
 *
 * Unit Name: repldlg
 * Purpose  : Replace Dialog
 * Author   : David Brock, mods by Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Portions by David Brock
 *			  Please read the license agreement at 
 *			  www.pnotepad.org/press/psidx.html.
 **************************************************************}
unit ReplDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SyntaxEd;

type
  TfrmReplace = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    FindHistoryBox: TComboBox;
    GroupBox1: TGroupBox;
    UseCaseBox: TCheckBox;
    WholeWordBox: TCheckBox;
    RegExBox: TCheckBox;
    GroupBox2: TGroupBox;
    FwdDirection: TRadioButton;
    BackDirection: TRadioButton;
    ReplaceAllButton: TButton;
    Label2: TLabel;
    ReplHistoryBox: TComboBox;
    PromptOnReplBox: TCheckBox;
    GroupBox3: TGroupBox;
    GlobalScope: TRadioButton;
    SelectedScope: TRadioButton;
    GroupBox4: TGroupBox;
    CursorOrigin: TRadioButton;
    EntireOrigin: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OKButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FindHistoryBoxChange(Sender: TObject);
  private
    { Private declarations }
    FFindOpt           : longint;
    FFindText          : string;
    FEditor            : TSyntaxMemo;
    FFirstTime         : Boolean;
    FStartPoint        : longint;
    FOriginalSelection : TChRange;
    procedure UpdateOptionBoxes;
    procedure ReadOptionsBoxes;
  public
    { Public declarations }
    procedure Execute(FindText: string; Options: longint; anEditor: TSyntaxMemo);
  end;

var
  frmReplace: TfrmReplace;

implementation

{$R *.DFM}

uses main, editor;

{ TEdReplDlg }

const
  udREPLACEALL = UDS_USER + 1;

procedure TfrmReplace.Execute(FindText: string; Options: Integer; anEditor: TSyntaxMemo);
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

procedure TfrmReplace.FormShow(Sender: TObject);
begin
  Self.SetBounds(frmMain.Left + (frmMain.Width div 2) - (Self.Width div 2),
                frmMain.Top + (frmMain.Height div 2) - (Self.Height div 2),
                Self.Width,
                Self.Height);
  if (FFindText <> '')
   then begin
     if FindHistoryBox.Items.IndexOf(FFindText) = -1 then FindHistoryBox.Items.Add(FFindText);
     FindHistoryBox.ItemIndex := FindHistoryBox.Items.IndexOf(FFindText);
    end;
  UpdateOptionBoxes;
  FindHistoryBoxChange(Self);
  FindHistoryBox.Setfocus;
end;

procedure TfrmReplace.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmReplace.OKButtonClick(Sender: TObject);
var FoundAt : longint;
    rText   : string;
begin
  ReadOptionsBoxes;
  if FFirstTime
   then begin
     //
     // First time -- determine where to start the search
     //
     if GlobalScope.Checked
      then FStartPoint := 0 else
     if EntireOrigin.Checked
      then
       if FwdDirection.Checked
        then FStartPoint := FOriginalSelection.chStart
        else FStartPoint := FOriginalSelection.chEnd
      else FStartPoint := FOriginalSelection.chStart;
     FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
    end
   else FEditor.Perform(EM_GETSEL, longint(@FStartPoint), longint(@FoundAt));
  FFirstTime := false;
  with FEditor do begin
    if (not RegExBox.Checked)
     then
      if (UseCaseBox.Checked and (SelText = FindHistoryBox.Text))
         or ((not UseCaseBox.Checked) and (Lowercase(SelText) = Lowercase(FindHistoryBox.Text)))
       then
        if FwdDirection.Checked
         then FStartPoint := FStartPoint + 1
         else FStartPoint := FStartPoint - 1;
    FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
    if FindHistoryBox.Items.IndexOf(FindHistoryBox.Text) = -1
     then FindHistoryBox.Items.Insert(0, FindHistoryBox.Text);
    if ReplHistoryBox.Items.IndexOf(ReplHistoryBox.Text) = -1
     then ReplHistoryBox.Items.Insert(0, ReplHistoryBox.Text);
    FoundAt := SelStart;
    if not FindReplace(FindHistoryBox.Text, ReplHistoryBox.Text, FoundAt, rText, FFindOpt)
     then ShowMessage(format('"%s" not found', [FindHistoryBox.Text]))
     else begin
       if PromptOnReplBox.Checked
        then case MessageDlg(format('Replace this instance of "%s" with "%s" ?', [FindHistoryBox.Text, rText]), mtConfirmation, [mbYES, mbNO, mbCANCEL], 0) of
          mrYES    : Perform(SEM_REPLACESEL, euDRAW, longint(PChar(rText)));
          mrNo     : ;
          mrCancel : exit;
         end
        else Perform(SEM_REPLACESEL, euDRAW, longint(PChar(rText)));
      end;
   end;
  Close;
end;

procedure TfrmReplace.ReadOptionsBoxes;
begin
  FFindOpt := 0;
  if UseCaseBox.Checked    then FFindOpt := FFindOpt or ft_MATCHCASE;
  if WholeWordBox.Checked  then FFindOpt := FFindOpt or ft_WHOLEWORD;
  if RegExBox.Checked      then FFindOpt := FFindOpt or ft_REGEXPR;
  if BackDirection.Checked then FFindOpt := FFindOpt or ft_REVERSE;
end;

procedure TfrmReplace.UpdateOptionBoxes;
begin
  UseCaseBox.Checked    := FFindOpt and ft_MATCHCASE <> 0;
  WholeWordBox.Checked  := FFindOpt and ft_WHOLEWORD <> 0;
  //RegExBox.Checked      := FFindOpt and ft_REGEXPR   <> 0;
  FwdDirection.Checked  := FFindOpt and ft_REVERSE   =  0;
  BackDirection.Checked := not (FwdDirection.Checked);
end;

procedure TfrmReplace.ReplaceAllButtonClick(Sender: TObject);
var aSel    : TChRange;
    FoundAt : longint;
    rText   : string;
begin
  //                                                                        
  // Replace all occurences...
  //
  ReadOptionsBoxes;
  if FFirstTime
   then begin
     //
     // First time -- determine where to start the search
     //
     if GlobalScope.Checked
      then FStartPoint := 0 else
     if EntireOrigin.Checked
      then
       if FwdDirection.Checked
        then FStartPoint := FOriginalSelection.chStart
        else FStartPoint := FOriginalSelection.chEnd
      else FStartPoint := FOriginalSelection.chStart;
     FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
    end
   else FEditor.Perform(EM_GETSEL, longint(@FStartPoint), longint(@FoundAt));
  FFirstTime := false;
  with FEditor do begin
    try TfrmClient(FEditor.Parent).Modified := True; except end;
    if (not RegExBox.Checked)
     then
      if (UseCaseBox.Checked and (SelText = FindHistoryBox.Text))
         or ((not UseCaseBox.Checked) and (Lowercase(SelText) = Lowercase(FindHistoryBox.Text)))
       then
        if FwdDirection.Checked
         then FStartPoint := FStartPoint + 1
         else FStartPoint := FStartPoint - 1;
    if FindHistoryBox.Items.IndexOf(FindHistoryBox.Text) = -1
     then FindHistoryBox.Items.Insert(0, FindHistoryBox.Text);
    if ReplHistoryBox.Items.IndexOf(ReplHistoryBox.Text) = -1
     then ReplHistoryBox.Items.Insert(0, ReplHistoryBox.Text);
    //FoundAt := 0;
    UndoTransBegin(udREPLACEALL);
    if not PromptOnReplBox.Checked then Lines.BeginUpdate;
    PromptOnReplBox.Enabled := false;
    {SelStart := 0;
    SelLength:= 0;}
    repeat
      FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
      if (not FindReplace(FindHistoryBox.Text, ReplHistoryBox.Text, FStartPoint, rText, FFindOpt))
         or (SelectedScope.Checked and ((FStartPoint <= FOriginalSelection.chStart) or (FStartPoint >= FOriginalSelection.chEnd)))
       then break
       else begin
         if PromptOnReplBox.Checked
          then case MessageDlg(format('Replace this instance of "%s" with "%s" ?', [FindHistoryBox.Text, rText]), mtConfirmation, [mbYES, mbNO, mbCANCEL], 0) of
            mrYES    : begin
                         Perform(SEM_SELECTION,  eaGET, longint(@aSel));
                         aSel := Normalise(aSel);
                         Perform(SEM_REPLACESEL, euNODRAW, longint(PChar(rText)));
                         if SelectedScope.Checked then inc(FOriginalSelection.chEnd, length(rText) - (aSel.chEnd - aSel.chStart));
                       end;
            mrNo     : ;
            mrCancel : break;
           end
          else begin
            Perform(SEM_SELECTION,  eaGET, longint(@aSel));
            aSel := Normalise(aSel);
            Perform(SEM_REPLACESEL, euNODRAW, longint(PChar(rText)));
            if SelectedScope.Checked then inc(FOriginalSelection.chEnd, length(rText) - (aSel.chEnd - aSel.chStart));
           end;
        end;
      Perform(SEM_SELECTION,  eaGET, longint(@aSel));
      aSel := Normalise(aSel);
      if FwdDirection.Checked
       then FStartPoint := aSel.chEnd
       else FStartPoint := aSel.chStart-1;
      //SelStart    := FoundAt;
    until false;
    PromptOnReplBox.Enabled := true;
    if not PromptOnReplBox.Checked then Lines.EndUpdate;
    UndoTransEnd(udREPLACEALL);
    Perform(SEM_SELECTION, eaGET, longint(@aSel));
    Perform(SEM_SELECTION, eaSET or euDRAW, longint(@aSel));
   end;
   Close;
end;

procedure TfrmReplace.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmReplace.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FEditor.HideSelection := true;
end;

procedure TfrmReplace.FindHistoryBoxChange(Sender: TObject);
begin
  OKButton.Enabled         := FindHistoryBox.Text <> '';
  ReplaceAllButton.Enabled := OKButton.Enabled;
end;

end.

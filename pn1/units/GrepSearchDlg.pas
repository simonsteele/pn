{***************************************************************
 *
 * Unit Name   : GrepSearchDlg
 * Date        :
 * Purpose     : Grep Search Search Dialog
 * Copyright   : This Source Code is taken from GExperts, the excellent
 * 			     Delphi/C++Builder add-on available from GExperts.org.
 *				 Please see the file gexpertslicense.html for the license.
 *				 Any modifications from the original are copyright Echo
 *				 Software and Simon Steele.
 *
 ****************************************************************}

unit GrepSearchDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, GrepResultsDlg, Editor, Main, Mask, pntypes,
  BrowseDr, EnhCBox, echocombo, StdCtrls{, vgNLS , Placemnt};

type
  TfrmGrepSearch = class(TForm)
    lblFind: TLabel;
    gbxOptions: TGroupBox;
    chkNoCase: TCheckBox;
    chkNoComments: TCheckBox;
    gbxWhere: TGroupBox;
    rbAllFiles: TRadioButton;
    rbOpenFiles: TRadioButton;
    rbDirectories: TRadioButton;
    gbxDirectories: TGroupBox;
    lblDirectory: TLabel;
    lblMasks: TLabel;
    cbMasks: TComboBox;
    chkInclude: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkWholeWord: TCheckBox;
    rbCurrentOnly: TRadioButton;
    btnHelp: TButton;
    chkRegEx: TCheckBox;
    chkGrepANSI: TCheckBox;
//    FormStorage: TFormStorage;  - no RX Library any more.
    btnBrowse: TButton;
    dlgBrowse: TdfsBrowseDirectoryDlg;
    cbDirectory: tEchoComboBox;
    cbText: tEchoComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbDirectoriesClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure cbDirectoryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    {tran: TvgTranslator;}
    procedure DirEnable(New: Boolean);
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  Menus{, AppUtils};

procedure TfrmGrepSearch.FormCreate(Sender: TObject);
Var
  I: integer;
begin
  {tran := TvgTranslator.Create(Self);}
  rbCurrentOnly.Enabled := frmMain.GetCurrentEditor <> nil;
  rbAllFiles.Enabled := False;                   //Allfiles in the Project
  rbOpenFiles.Enabled := rbCurrentOnly.Enabled;  //Current Open Files in Editor
  rbDirectories.Checked := True;
  DirEnable(rbDirectories.Checked);
  with TFilterList.Create do
  try
    Filters := frmMain.OpenDialog1.Filter;
    cbMasks.Items.Clear;
    For I := 0 to Count - 2 do
      cbMasks.Items.Add(Extension[i]);
  finally
    Free;
  end; //try
end;

procedure TfrmGrepSearch.DirEnable(New: Boolean);
begin
  cbDirectory.Enabled := New;
  cbMasks.Enabled := New;
  chkInclude.Enabled := New;
  if not New then
  begin
    cbDirectory.Color := clBtnface;
    cbMasks.Color := clBtnface;
  end
  else
  begin
    cbDirectory.Color := clWindow;
    cbMasks.Color := clWindow;
  end
end;

procedure TfrmGrepSearch.rbDirectoriesClick(Sender: TObject);
begin
  DirEnable(rbDirectories.Checked);
end;

procedure TfrmGrepSearch.FormShow(Sender: TObject);
begin
//  tran.LanguageFile := CurrentLan;
//  tran.Translate;
  cbText.RegistryBranch := HK_CU;
  cbDirectory.RegistryBranch := HK_CU;
  cbText.LoadMRU;
  cbDirectory.LoadMRU;
end;

procedure TfrmGrepSearch.btnBrowseClick(Sender: TObject);
begin
  if dlgBrowse.Execute then
    cbDirectory.Text := dlgBrowse.Selection;
end;

procedure TfrmGrepSearch.cbDirectoryKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = vk_down) and (ssAlt in Shift) then btnBrowse.Click;
end;

procedure TfrmGrepSearch.btnOKClick(Sender: TObject);
var i : Integer;
begin
  if cbText.Text <> '' then
  begin
    i := cbText.Items.IndexOf(cbText.Text);
    if i <> -1 then
      cbText.Items.Move(i, 0)
    else
      cbText.Items.Insert(0, cbText.Text);
    cbText.ItemIndex := 0;
  end;
  if cbDirectory.Text <> '' then
  begin
    i := cbDirectory.Items.IndexOf(cbDirectory.Text);
    if i <> - 1 then
      cbDirectory.Items.Move(i, 0)
    else
      cbDirectory.Items.Insert(0, cbDirectory.Text);
    cbDirectory.ItemIndex := 0;
  end;
end;

procedure TfrmGrepSearch.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  cbText.SaveMRU;
  cbDirectory.SaveMRU;
end;

end.


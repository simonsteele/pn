{***************************************************************
 *
 * Unit Name: newtool
 * Purpose  : Form used for configuring tools and other items
 * 			  throughout Programmers Notepad.
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *	      agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}

unit newtool;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BrowseDr, ExtCtrls, AgOpenDialog, hotkey;

type
  TfrmNewToolLink = class(TForm)
    lblCaption: TLabel;
    lblCommand: TLabel;
    txtCaption: TEdit;
    txtCommand: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    btnChoose: TButton;
    Label3: TLabel;
    txtParameters: TEdit;
    lblDir: TLabel;
    txtDir: TEdit;
    btnDir: TButton;
    dlgBrowse: TdfsBrowseDirectoryDlg;
    panHint: TPanel;
    Label4: TLabel;
    Memo1: TMemo;
    chkAskParameters: TCheckBox;
    chkCaptureOutput: TCheckBox;
    Label5: TLabel;
    dlgChoose: TAgOpenDialog;
    btnClearShortcut: TButton;
    procedure btnChooseClick(Sender: TObject);
    procedure btnDirClick(Sender: TObject);
    procedure txtCaptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearShortcutClick(Sender: TObject);
  private
  public
    { Public declarations }
    txtShortcut : tEchoHotKey;
    function Execute(TopPos, LeftPos, HeightInt, WidthInt : Integer) : Boolean; overload;
    function Execute(TopPos, LeftPos, HeightInt, WidthInt : Integer; ShowAdv : Boolean) : Boolean; overload;
    function ExecuteBrowser(TopPos, LeftPos, HeightInt, WidthInt : Integer) : Boolean;
    function ExecuteProject(TopPos, LeftPos, HeightInt, WidthInt : Integer) : Boolean;
  end;

var
  frmNewToolLink: TfrmNewToolLink;

implementation

uses options;

{$R *.DFM}

procedure TfrmNewToolLink.btnChooseClick(Sender: TObject);
begin
   If dlgChoose.Execute Then txtCommand.Text := dlgChoose.Filename;
end;

function TfrmNewToolLink.Execute(TopPos, LeftPos, HeightInt, WidthInt : Integer) : Boolean;
begin
   Self.Left := LeftPos + (WidthInt div 2) - (Self.Width div 2);
   Self.Top := TopPos + (HeightInt div 2) - (Self.Height div 2);
   Result := Showmodal = mrOk;
end;

function TfrmNewToolLink.Execute(TopPos, LeftPos, HeightInt,
  WidthInt: Integer; ShowAdv: Boolean): Boolean;
begin
   Self.Left := LeftPos + (WidthInt div 2) - (Self.Width div 2);
   Self.Top := TopPos + (HeightInt div 2) - (Self.Height div 2);
   btnDir.Visible := ShowAdv;
   lblDir.Visible := ShowAdv;
   txtDir.Visible := ShowAdv;
   Result := Showmodal = mrOk;
end;

procedure TfrmNewToolLink.btnDirClick(Sender: TObject);
begin
   If dlgBrowse.Execute Then txtDir.Text := dlgBrowse.Selection;
end;

procedure TfrmNewToolLink.txtCaptionChange(Sender: TObject);
begin
  Caption := tEdit(Sender).Text;
end;

procedure TfrmNewToolLink.FormCreate(Sender: TObject);
begin
  txtShortcut := tEchoHotKey.Create(Self);
  txtShortcut.Parent := Self;
  txtShortcut.Top := txtParameters.Top + (txtParameters.Top - txtDir.Top);
  txtShortcut.Left := txtDir.Left;
  txtShortcut.Height := txtDir.Height;
  txtShortcut.Width := round( (233 / 265) * txtParameters.Width);
end;

procedure TfrmNewToolLink.FormDestroy(Sender: TObject);
begin
  Freeandnil(txtShortcut);
end;

procedure TfrmNewToolLink.btnClearShortcutClick(Sender: TObject);
begin
  txtShortcut.Text := '';
end;

function TfrmNewToolLink.ExecuteBrowser(TopPos, LeftPos, HeightInt,
  WidthInt: Integer): Boolean;
begin
   Height := 200;
   lblCaption.Caption := 'Name:';
   Self.Left := LeftPos + (WidthInt div 2) - (Self.Width div 2);
   Self.Top := TopPos + (HeightInt div 2) - (Self.Height div 2);
   Result := Showmodal = mrOk;
end;

function TfrmNewToolLink.ExecuteProject(TopPos, LeftPos, HeightInt,
  WidthInt: Integer): Boolean;
begin
   Caption := 'New Project...';
   Height := 109;
   lblCaption.Top := 8;
   lblCaption.Alignment := taLeftJustify;
   lblCaption.AutoSize := True;
   lblCaption.Caption := 'Name your new project:';
   txtCaption.Top := txtCommand.Top;
   txtCaption.Left := 40;
   txtCaption.Width := txtCaption.Width + 20;
   // Make sure we don't see half of the control...
   txtDir.Visible := False;
   lblDir.Visible := False;
   btnDir.Visible := False;
   lblCommand.Visible := False;
   txtCommand.Visible := False;
   btnChoose.Visible := False;
   Self.Left := LeftPos + (WidthInt div 2) - (Self.Width div 2);
   Self.Top := TopPos + (HeightInt div 2) - (Self.Height div 2);
   Result := Showmodal = mrOk;
end;

end.

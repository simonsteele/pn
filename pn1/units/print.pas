{**********************************************************************
 *
 * Unit Name: Print
 *
 * Purpose  : PN Print Form
 *
 * Author   : Simon Steele
 *
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 *
 * History  : 29/05/1999 Fixed the setting of TSM settings for printing
 *                       and fixed writing and reading print Settings
 *                       from the registry.
 *********************************************************************}

unit print;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SyntaxEd, RXSpin, RXCombos, Registry;

type
  TfrmPrint = class(TForm)
    GroupBox1: TGroupBox;
    FontComboBox1: TFontComboBox;
    RxSpinEdit1: TRxSpinEdit;
    GroupBox2: TGroupBox;
    cblineno: TCheckBox;
    cbfilename: TCheckBox;
    cbdatetime: TCheckBox;
    cbwordwrap: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbpageno: TCheckBox;
    btnPrintSetup: TButton;
    chkHideBGColours: TCheckBox;
    chkColour: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPrintSetupClick(Sender: TObject);
  private
    { Private declarations }
    procedure SaveSettings;
    procedure LoadSettings;
  public
    { Public declarations }
  end;

var
  frmPrint: TfrmPrint;

const
  PrintKey = 'Software\Echo Software\PN\Printing';

implementation

{$R *.DFM}

uses Main, Editor, pntypes;

procedure TfrmPrint.btnOKClick(Sender: TObject);
var FEditor  : TfrmClient;
    FFont    : TFont;
    FOptions : TSyntaxMemoOptions;
    Fmt      : TFormatEntry;
    oldBG    : tColor;
begin
   Screen.Cursor := crHourglass;
   oldBG := clWhite;
   Try
   FEditor := frmMain.GetCurrentEditor;
   if Assigned(FEditor) then
     With FEditor do
     Begin
        if Mode <> emHex then
        Begin
           synMDI.Lines.BeginUpdate;
           FFont := TFont.Create;
           Try
              FFont.Assign(synMDI.Font);
              FOptions := synMDI.Options;
              synMDI.Font.Name := FontComboBox1.FontName;
              synMDI.Font.Size := trunc(RXSpinEdit1.Value);
              if cblineno.checked then
                synMDI.Options := synMDI.Options + [smoPrintLinenos] else
                synMDI.Options := synMDI.Options - [smoPrintLinenos];
              if cbfilename.checked then
                synMDI.Options := synMDI.Options + [smoPrintFilename] else
                synMDI.Options := synMDI.Options - [smoPrintFilename];
              if cbdatetime.checked then
                synMDI.Options := synMDI.Options + [smoPrintDate] else
                synMDI.Options := synMDI.Options - [smoPrintDate];
              if cbwordwrap.checked then
                synMDI.Options := synMDI.Options + [smoWordwrap] else
                synMDI.Options := synMDI.Options - [smoWordwrap];
              if cbpageno.checked then
                synMDI.Options := synMDI.Options + [smoPrintPageNos] else
                synMDI.Options := synMDI.Options - [smoPrintPageNos];
              // Do we actually want a colour print?
              synMDI.MonoPrint := not chkColour.Checked;
              // Stop printing black backgrounds!!!
              if chkHideBGColours.checked then
              begin
                Fmt := synMDI.UI_Styles.FormatTable[0];
                oldBG := Fmt.fme_Background;
                Fmt.fme_background := clWhite;
                synMDI.UI_Styles.FormatTable[0] := Fmt;
              end;
              synMDI.Print;
           finally
              synMDI.Font.Assign(FFont);
              if chkHideBGColours.Checked then
              begin
                Fmt := synMDI.UI_Styles.FormatTable[0];
                Fmt.fme_background := oldBG;
                synMDI.UI_Styles.FormatTable[0] := Fmt;
              end;
              FFont.Free;
              synMDI.Options := FOptions;
              synMDI.Lines.EndUpdate;
           End;
        end else
        begin
           FFont := tFont.Create;
           FFont.Name := FontComboBox1.FontName;
           FFont.Size := trunc(RXSpinEdit1.Value);
           HexPrint(FFont);
        end;
     End;
   finally
      Screen.Cursor := crDefault;
      Close;
   End;
end;

procedure TfrmPrint.SaveSettings;
var Settings: tRegistry;
Begin
  Settings := TRegistry.Create;
  Settings.OpenKey(PrintKey, True);
  Try
     With Settings do
     Begin
        WriteString( 'FontName', FontComboBox1.FontName);
        WriteInteger('FontSize', trunc(RxspinEdit1.Value));
        WriteBool(   'LineNo',   cbLineno.Checked);
        WriteBool(   'Filename', cbFilename.Checked);
        WriteBool(   'DateTime', cbDatetime.Checked);
        WriteBool(   'Wordwrap', cbWordWrap.Checked);
        WriteBool(   'PageNos',  cbPageno.Checked);
        WriteBool(   'HideBG',   chkHideBGColours.Checked);
        WriteBool(   'Colour',   chkColour.Checked);
     End;
  finally
     Settings.CloseKey;
     Settings.Free;
  End;
end;

procedure TfrmPrint.LoadSettings;
var Settings : TRegistry;
    FEditor  : TfrmClient;
    n        : integer;
Begin
   FEditor := frmMain.GetCurrentEditor;
   if Assigned(FEditor) then
   Begin
      FontComboBox1.FontName := FEditor.synMDI.Font.Name;
      RxSpinEdit1.Value      := Abs(Feditor.synMDI.Font.Size);
   End;
   Settings := TRegistry.Create;
   Settings.OpenKey(PrintKey, True);
   Try
      With Settings do
      Begin
         FontComboBox1.FontName := ReadString('FontName');
         RxspinEdit1.Value      := ReadInteger('FontSize');
         cbLineno.Checked       := ReadBool('LineNo');
         cbFilename.Checked     := ReadBool('Filename');
         cbDatetime.Checked     := ReadBool('DateTime');
         cbWordWrap.Checked     := ReadBool('Wordwrap');
         cbPageno.Checked       := ReadBool('PageNos');
         chkColour.Checked      := ReadBool('Colour');
         chkHideBGColours.Checked := ReadBool('HideBG');
      End;
   except
     chkHideBGColours.Checked := True;
     chkColour.Checked := True;
     cbPageno.Checked := False;
     chkHideBGColours.Checked := False;
     cbWordWrap.Checked := True;
     cbDateTime.Checked := False;
     cbLineNo.Checked := False;
     FontComboBox1.FontName := 'Courier New';
     RxspinEdit1.Value := 10;
   End;
   if fEditor.Mode = emHex then
   begin
      For n := 0 to GroupBox2.ControlCount - 1 do
         if GroupBox2.Controls[n] is tCheckBox then tCheckBox(GroupBox2.Controls[n]).Enabled := False;
   end;
   Settings.Free;
End;


procedure TfrmPrint.FormCreate(Sender: TObject);
begin
   LoadSettings;
end;

procedure TfrmPrint.FormDestroy(Sender: TObject);
begin
   SaveSettings;
end;

procedure TfrmPrint.btnPrintSetupClick(Sender: TObject);
begin
   frmMain.PrinterSetupDialog1.Execute;
end;

end.

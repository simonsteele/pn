{***************************************************************
 *
 * Unit Name: options
 * Purpose  : Programmers Notepad Options
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}
unit options;

interface

{$DEFINE plugin}
{$DEFINE parsers}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,Registry, ExtCtrls, Menus, CheckLst,
  Buttons, useful, ImgList, pntypes, UrlLabel, EnhListView, synParse, IniFiles,
  BrowseDr, ColorPickerButton, hotkey, ActnList, commctrl, FontComboBox;

Type TConfigInfo=class(TComponent)
  private
    FFontName: String;
    FFontSize: Integer;
    FWordWrap: Boolean;
    FShowChange: Boolean;
  public
    procedure LoadSettings;
    procedure SaveSettings;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    property Fontname: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property ShowChange: Boolean read FShowChange write FShowChange;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
end;

type
  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    popTools: TPopupMenu;
    itmAdd: TMenuItem;
    itmEdit: TMenuItem;
    itmRename: TMenuItem;
    itmRemove: TMenuItem;
    N1: TMenuItem;
    itmMoveUp: TMenuItem;
    itmMoveDown: TMenuItem;
    itmAlphaSort: TMenuItem;
    imgTick: TImage;
    imgRename: TImage;
    imgCancel: TImage;
    imgMinus: TImage;
    notOptions: TNotebook;
    Image1: TImage;
    Label1: TLabel;
    Bevel6: TBevel;
    Label26: TLabel;
    Image11: TImage;
    fcbFont: TFontComboBox;
    Label27: TLabel;
    Bevel7: TBevel;
    Label28: TLabel;
    cbWordwrap: TCheckBox;
    cbShowChange: TCheckBox;
    chkMaximiseFiles: TCheckBox;
    Image12: TImage;
    Label5: TLabel;
    Image3: TImage;
    Image14: TImage;
    Label29: TLabel;
    Bevel8: TBevel;
    Label30: TLabel;
    lstTypes: TListView;
    btnTypeMoveUp: TSpeedButton;
    btnTypeMoveDown: TSpeedButton;
    btnTypeRemove: TBitBtn;
    btnTypeEdit: TBitBtn;
    btnTypeAdd: TBitBtn;
    txtType: TEdit;
    Label32: TLabel;
    Label31: TLabel;
    txtDesc: TEdit;
    cmbParser: TComboBox;
    Label33: TLabel;
    chkUnixSave: TCheckBox;
    Label6: TLabel;
    Image4: TImage;
    chkAutoExec: TCheckBox;
    chkConfig: TCheckBox;
    chkWinIni: TCheckBox;
    chkSysIni: TCheckBox;
    chkProt: TCheckBox;
    Label7: TLabel;
    Label10: TLabel;
    Image5: TImage;
    chkUseProxy: TCheckBox;
    Bevel1: TBevel;
    Label11: TLabel;
    Image6: TImage;
    Label12: TLabel;
    txtProxyHost: TEdit;
    txtProxyPort: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Bevel3: TBevel;
    Label19: TLabel;
    Image8: TImage;
    Label20: TLabel;
    lstPlugins: TListView;
    UrlLabel1: TUrlLabel;
    btnAboutPlugin: TButton;
    btnConfigurePlugin: TButton;
    Label34: TLabel;
    Image13: TImage;
    treOptions: TTreeView;
    Bevel9: TBevel;
    chkAllowPluginStartup: TCheckBox;
    AllowPluginShutdown: TCheckBox;
    Bevel10: TBevel;
    Label35: TLabel;
    Bevel12: TBevel;
    Label37: TLabel;
    chkOpenAsHex: TCheckBox;
    lstAssoc: TdfsEnhListView;
    chkAssocOpenWith: TRadioButton;
    chkAssocOpen: TRadioButton;
    chkAssocNone: TRadioButton;
    Label8: TLabel;
    Bevel13: TBevel;
    Label9: TLabel;
    Label38: TLabel;
    Image15: TImage;
    Label39: TLabel;
    Bevel14: TBevel;
    lstSchemes: TListView;
    urlSchemes: TUrlLabel;
    chkSaveSchemeSettings: TCheckBox;
    btnConfigureScheme: TButton;
    btnResetScheme: TButton;
    Label40: TLabel;
    txtMRUMax: TEdit;
    Label41: TLabel;
    btnResetAllSchemes: TButton;
    Label42: TLabel;
    Image16: TImage;
    Label43: TLabel;
    Bevel15: TBevel;
    lstNew: TListView;
    Label44: TLabel;
    txtMenu: TEdit;
    Label45: TLabel;
    cmbParser2: TComboBox;
    btnNewAdd: TBitBtn;
    btnNewEdit: TBitBtn;
    btnNewRemove: TBitBtn;
    Label46: TLabel;
    txtTitle: TEdit;
    btnNewMoveDown: TSpeedButton;
    btnNewMoveUp: TSpeedButton;
    radMyDocuments: TRadioButton;
    radLastUsed: TRadioButton;
    radCustom: TRadioButton;
    btnChooseDir: TSpeedButton;
    Bevel16: TBevel;
    Label48: TLabel;
    Image17: TImage;
    Label49: TLabel;
    txtStartDir: TEdit;
    dlgBrowse: TdfsBrowseDirectoryDlg;
    btnSendTo: TBitBtn;
    chkShowClock: TCheckBox;
    txtClockFormat: TEdit;
    Image18: TImage;
    Label47: TLabel;
    Label51: TLabel;
    Bevel18: TBevel;
    lstFolders: TListView;
    btnFolderAdd: TBitBtn;
    btnFolderEdit: TBitBtn;
    btnFolderRemove: TBitBtn;
    btnFolderMoveUp: TSpeedButton;
    btnFolderMoveDown: TSpeedButton;
    Label52: TLabel;
    Bevel19: TBevel;
    btnDesktopAdd: TButton;
    Label53: TLabel;
    Bevel20: TBevel;
    Label54: TLabel;
    Image19: TImage;
    cmbSchemes: TComboBox;
    lstProgs: TListView;
    btnToolAdd: TButton;
    btnToolEdit: TButton;
    btnToolRemove: TButton;
    btnToolMoveUp: TSpeedButton;
    btnToolMoveDown: TSpeedButton;
    Image20: TImage;
    Label55: TLabel;
    Label56: TLabel;
    Bevel11: TBevel;
    chkSaveDFMsAsText: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Bevel21: TBevel;
    chkClearUndo: TCheckBox;
    Label4: TLabel;
    Image2: TImage;
    Label57: TLabel;
    Image21: TImage;
    Label58: TLabel;
    Bevel23: TBevel;
    Label59: TLabel;
    chkQSReturn: TRadioButton;
    chkQSLeave: TRadioButton;
    chkQSKeepHist: TCheckBox;
    btnSendToAdd: TButton;
    btnQLAdd: TButton;
    Label60: TLabel;
    Image22: TImage;
    Bevel24: TBevel;
    Label61: TLabel;
    lstBrowsers: TListView;
    btnBrowserConfigure: TButton;
    btnBrowserMoveUp: TSpeedButton;
    btnBrowserMoveDown: TSpeedButton;
    btnBrowserAdd: TButton;
    btnBrowserRemove: TButton;
    Label50: TLabel;
    chkLaunchURLs: TCheckBox;
    radDoubleClick: TRadioButton;
    radSingleClick: TRadioButton;
    Bevel17: TBevel;
    Label62: TLabel;
    Bevel25: TBevel;
    chkMultiLineTabs: TCheckBox;
    chkShowTabs: TCheckBox;
    chkShowHighlight: TCheckBox;
    chkHighlightActive: TCheckBox;
    cmbHighlightActiveColor: TColorPickerButton;
    chkHighlightChange: TCheckBox;
    cmbUnderlineColor: TColorPickerButton;
    btnCreateBat: TButton;
    btnIconChoose: TButton;
    imgIcon: TImage;
    ilsAssoc: TImageList;
    btnAddAssoc: TBitBtn;
    Label15: TLabel;
    Image7: TImage;
    Label16: TLabel;
    Bevel2: TBevel;
    Label17: TLabel;
    panWarnOpenAgain: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Image9: TImage;
    Label18: TLabel;
    Label21: TLabel;
    Bevel4: TBevel;
    lstActionCats: TListView;
    lstActionComms: TListView;
    Label22: TLabel;
    Label23: TLabel;
    lblNewShortcut: TLabel;
    lblActionDesc: TLabel;
    btnAssign: TButton;
    Label24: TLabel;
    cmbKeyboard: TComboBox;
    chkOpenWholeProject: TCheckBox;
    chkDeleteFileCheck: TCheckBox;
    RadioButton4: TRadioButton;
    txtFontSize: TEdit;
    updFontSize: TUpDown;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itmAlphaSortClick(Sender: TObject);
    procedure btnAddAssocClick(Sender: TObject);
    procedure chkUseProxyClick(Sender: TObject);
    procedure txtProxyPortKeyPress(Sender: TObject; var Key: Char);
    procedure txtProxyPortKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstTypesClick(Sender: TObject);
    procedure btnTypeEditClick(Sender: TObject);
    procedure btnTypeRemoveClick(Sender: TObject);
    procedure btnTypeAddClick(Sender: TObject);
    procedure btnTypeMoveUpClick(Sender: TObject);
    procedure lstTypesDblClick(Sender: TObject);
    procedure lstPluginsClick(Sender: TObject);
    procedure btnConfigurePluginClick(Sender: TObject);
    procedure btnAboutPluginClick(Sender: TObject);
    procedure lstAssocClick(Sender: TObject);
    procedure lstAssocDrawItem(Control: TWinControl; var ACanvas: TCanvas;
      Index: Integer; ARect: TRect; State: TOwnerDrawState;
      var DefaultDrawing, FullRowSelect: Boolean);
    procedure chkAssocNoneClick(Sender: TObject);
    procedure btnConfigureSchemeClick(Sender: TObject);
    procedure btnResetSchemeClick(Sender: TObject);
    procedure btnResetAllSchemesClick(Sender: TObject);
    procedure lstNewClick(Sender: TObject);
    procedure btnNewAddClick(Sender: TObject);
    procedure btnNewEditClick(Sender: TObject);
    procedure btnNewRemoveClick(Sender: TObject);
    procedure btnNewMoveUpClick(Sender: TObject);
    procedure radCustomClick(Sender: TObject);
    procedure btnChooseDirClick(Sender: TObject);
    procedure btnSendToClick(Sender: TObject);
    procedure chkShowClockClick(Sender: TObject);
    procedure txtClockFormatKeyPress(Sender: TObject; var Key: Char);
    procedure txtClockFormatKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkLaunchURLsClick(Sender: TObject);
    procedure btnFolderAddClick(Sender: TObject);
    procedure btnFolderRemoveClick(Sender: TObject);
    procedure btnFolderEditClick(Sender: TObject);
    procedure lstFoldersClick(Sender: TObject);
    procedure btnFolderMoveUpClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnDesktopAddClick(Sender: TObject);
    procedure treOptionsAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure btnToolMoveUpClick(Sender: TObject);
    procedure lstProgsClick(Sender: TObject);
    procedure btnToolRemoveClick(Sender: TObject);
    procedure cmbSchemesChange(Sender: TObject);
    procedure btnToolAddClick(Sender: TObject);
    procedure btnToolEditClick(Sender: TObject);
    procedure treOptionsChange(Sender: TObject; Node: TTreeNode);
    procedure lstBrowsersCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lstBrowsersClick(Sender: TObject);
    procedure btnBrowserAddClick(Sender: TObject);
    procedure btnBrowserConfigureClick(Sender: TObject);
    procedure btnBrowserRemoveClick(Sender: TObject);
    procedure lstBrowsersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure chkHighlightActiveClick(Sender: TObject);
    procedure cmbHighlightActiveColorClick(Sender: TObject);
    procedure treOptionsExpanded(Sender: TObject; Node: TTreeNode);
    procedure btnCreateBatClick(Sender: TObject);
    procedure btnIconChooseClick(Sender: TObject);
    procedure lstProgsDblClick(Sender: TObject);
    procedure lstActionCatsClick(Sender: TObject);
    procedure lstActionCommsClick(Sender: TObject);
    procedure btnAssignClick(Sender: TObject);
    procedure btnQLAddClick(Sender: TObject);
  private
    { Private declarations }
    _BSOverride : Boolean;
    PIs         : tStringList;
    CurrScheme  : String;
    txtHotKey   : tEchoHotKey;
    Actions     : TStringList;
    Mappings    : TStringList;
    procedure PropagateTypes;
    procedure WriteTypes;
    procedure PropagateAssoc;
    procedure SaveAssoc;
    procedure SetAssoc;
    procedure PropagateSchemes;
    procedure PropagateNew;
    procedure SaveNew;
    procedure WriteFolders;
    procedure PropagateFolders;
    procedure LoadTools(scheme: string);
    procedure SaveTools(Scheme: String);
    procedure PropagateBrowsers;
    procedure WriteBrowsers;
    procedure PropagateKeyboard;
    procedure OnLoadKeyPresetsChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;
  Config: TConfigInfo=nil;
  MoveMade : Boolean;

const
  UnderlineCol : tColor = clbtnFace;

implementation

uses newtool, Main, genfuncs, pndefs;

{$R *.DFM}

Constructor TConfigInfo.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FFontName:='Courier New';
  FFontSize:=10;
  FWordWrap:=False;
  FShowChange:=true;
  LoadSettings;
End;

Destructor TConfigInfo.Destroy;
Begin
  SaveSettings;
  inherited Destroy;
End;

procedure TConfigInfo.LoadSettings;
var RegIni : TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\Echo Software\PN');
  Try
     With RegIni do
     Begin
        FFontName   := ReadString('Editor', 'FontName', FFontName);
        FFontSize   := ReadInteger('Editor', 'FontSize', FFontSize);
        FWordWrap   := ReadBool('Editor', 'WordWrap', FWordWrap);
        FShowChange := ReadBool('Editor', 'ShowChange', FShowChange);
     End;
  finally
     RegIni.Free;
  End;
End;

procedure TConfigInfo.SaveSettings;
var RegIni : TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\Echo Software\PN');
  Try
  With RegIni do
     Begin
        WriteString('Editor','FontName',FFontName);
        WriteInteger('Editor','FontSize',FFontSize);
        WriteBool('Editor','WordWrap',FWordWrap);
        WriteBool('Editor','ShowChange',FShowChange);
     End;
  finally
     RegIni.Free;
  End;
End;

procedure TfrmOptions.btnOKClick(Sender: TObject);
var
  reg    : tRegistry;
  n      : Integer;
begin
   Screen.Cursor:=crHourglass;
   With Config do
   Begin
      FontName:=fcbFont.Font.Name{.FontName};
      FontSize:=trunc(updFontSize.Position);
      ShowChange:=cbShowChange.Checked;
   End;
   If CurrScheme <> '' then SaveTools(CurrScheme);
   frmMain.ReloadTools;
   frmMain.actWordWrap.Checked := cbWordWrap.Checked;
   frmMain.actClock.Checked    := chkShowClock.Checked;
   frmMain.ClockFormat         := txtClockFormat.Text;
   frmMain.SetClock(frmMain.actClock.Checked);
   frmMain.DoURLs              := chkLaunchURLs.Checked;
   frmMain.LoseUndo            := chkClearUndo.Checked;
   frmMain.QSKeepFocus         := chkQSLeave.Checked;
   frmMain.tabs.MultiLine      := chkMultiLineTabs.Checked;
   frmMain.HighlightActive     := chkHighlightActive.Checked;
   frmMain.HighlightChange     := chkHighlightChange.Checked;
   frmMain.HighlightColor      := cmbHighlightActiveColor.SelectionColor;
   frmMain.ModifiedColor       := cmbUnderlineColor.SelectionColor;
   frmMain.tabs.Repaint;
   for n := 0 to panWarnOpenAgain.ControlCount - 1 do
   begin
     with panWarnOpenAgain do
       if TRadioButton(Controls[n]).Checked then
         frmMain._wnOpenFileAgain := TRadioButton(Controls[n]).Tag;
   end;
   frmMain.SetSetting('MultiLineTabs', RootKey, chkMultiLineTabs.Checked);
   frmMain.SetSetting('WarnOpenProj', RootKey, chkOpenWholeProject.Checked);
   frmMain.SetSetting('WarnDeleteFile', RootKey, chkDeleteFileCheck.Checked);
   // Set File Associations
   SetAssoc;
   SystemParametersInfo(SPI_SETICONS, 0, nil, SPIF_SENDCHANGE);
   {SysEdit Settings}
   frmMain.SEFiles := [];
   If chkAutoExec.Checked then frmMain.SEFiles := frmMain.SEFiles + [seAutoExec];
   If chkConfig.Checked Then frmMain.SEFiles := frmMain.SEFiles + [seConfig];
   If chkWinIni.Checked Then frmMain.SEFiles := frmMain.SEFiles + [seWinIni];
   If chkSysIni.Checked Then frmMain.SEFiles := frmMain.SEFiles + [seSysIni];
   If chkProt.Checked   Then frmMain.SEFiles := frmMain.SEFiles + [seProt];
   frmMain.actTabs.Checked := chkShowTabs.Checked;
   frmMain.OpenFilesMaxed := chkMaximiseFiles.Checked;
   try
     frmMain.MRUFileList1.Maximum := strtoint(txtMRUMax.Text)
   except
     on EConvertError do
     begin
       MessageDlg('The maximum number of MRU numbers must be a whole number.'+#13+#10+'The maximum has been set to 5.', mtError, [mbOK], 0);
       frmMain.MRUFileList1.Maximum := 5;
     end;
   end;
   reg := tRegistry.Create;
   Try
     reg.openkey(rootkey, true);
     If radMyDocuments.Checked then reg.WriteInteger('StartDir', 0)
     else If radLastUsed.Checked then reg.WriteInteger('StartDir', 1)
     else begin
       reg.WriteInteger('StartDir', 2);
       reg.WriteString('CustomStartDir', txtStartDir.text);
     end;
     reg.WriteBool('SaveSchemeSettings', chkSaveSchemeSettings.Checked);
     reg.WriteBool('SaveDFMAsBinary', not chkSaveDFMsAsText.Checked);
     reg.WriteBool('SaveQSHistory', chkQSKeepHist.Checked);
     reg.CloseKey;
     // Internet Settings
     reg.OpenKey(netkey, true);
     reg.WriteBool('URLLaunchDouble', radDoubleClick.Checked);
     reg.WriteString('ProxyHost', txtProxyHost.Text);
     reg.WriteString('ProxyPort', txtProxyPort.Text);
     reg.WriteBool('UseProxy', chkUseProxy.Checked);
     reg.CloseKey;
     // Editor Settings
     reg.OpenKey(EditKey, True);
     reg.WriteBool('HighlightOnContext', chkShowHighlight.Checked);
   Finally
     reg.Free;
   end;
   frmMain.EnableButtons;
   WriteTypes;
   SaveNew;
   WriteFolders;
   WriteBrowsers;
   frmMain.UpdateDialogs;
   frmMain.UpdateEditors;
   frmMain.UpdateFolders;
   frmMain.UpdateBrowsers;
   Screen.Cursor:=crDefault;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
   With Config do
   Begin
     fcbFont.Font.Name:=FontName;
     updFontSize.Position:=FontSize;
     cbShowChange.Checked:=ShowChange;
   End;
   PIs := tStringList.Create;
   PIs.Clear;
   cmbParser.Enabled := False;
   txtDesc.Enabled := False;
   txtType.Enabled := False;
   btnTypeRemove.Enabled := False;
   btnTypeEdit.Enabled := False;

   // Create a new hotkey!
   txtHotKey := tEchoHotKey.Create(lblNewShortcut.Parent);
   txtHotKey.Parent := lblNewShortcut.Parent;
   txtHotKey.Left := lblNewShortcut.Left + lblNewShortcut.Width + 8;
   txtHotKey.Width := btnAssign.Left - txtHotKey.Left - 8;
   txtHotKey.Top := lblNewShortcut.Top;

   Actions := TStringList.Create;

end;

procedure TfrmOptions.PropagateKeyboard;
var
  n : Integer;
  sl : TStringList;
  li : TListItem;
  search : TSearchRec;
  keys   : TIniFile;
  s      : string;
begin
  Mappings := TStringList.Create;
  sl := TStringList.Create;
  cmbKeyboard.Items.Add('(None)');
  // Ok, first let's propagate the action category list...
  for n := 0 to frmMain.ActionList.ActionCount - 1 do
  begin
    with frmMain.ActionList do
    begin
      if sl.IndexOf(Actions[n].Category) < 0 then
      begin
        sl.Add(Actions[n].Category);
        li := lstActionCats.Items.Add;
        li.Caption := Actions[n].Category;
      end;
    end;
  end;
  sl.Free;
  // Now we'll build a list of any available key mapping files...
  if FindFirst(ExtractFilePath(ParamStr(0)) + '*.key', faAnyFile, search) = 0 then
  begin
    repeat
      // Load the key file and check if it is a key mappings file, then add
      // it's name to the key mappings combo box...
      keys := TIniFile.Create(ExtractFilePath(ParamStr(0)) + search.Name);
      try
        s := keys.ReadString('Mappings', 'Name', '');
        if s <> '' then
        begin
          cmbKeyboard.Items.Add(s);
          Mappings.Add(ExtractFilePath(ParamStr(0)) + search.Name);
        end;
      finally
        keys.Free;
      end;
    until FindNext(search) <> 0;
  end;
  FindClose(search);
  cmbKeyboard.ItemIndex := 0;
  cmbKeyboard.OnChange := OnLoadKeyPresetsChange;
  lstActionCats.AlphaSort;
  lstActionCommsClick(lstActionComms);
end;

procedure TfrmOptions.OnLoadKeyPresetsChange(Sender : TObject);
var i : Integer;
    sc  : string;
    ini : TIniFile;
    sct : TStringList;
begin
  // item 0 is the '(None)' item...
  if cmbKeyboard.ItemIndex = 0 then Exit;
  // Load a set of keyboard presets...
  ini := TIniFile.Create(Mappings[cmbKeyboard.ItemIndex-1]);
  sct := TStringList.Create;
  Try
    ini.ReadSections(sct);
    if sct.Count > 1 then
    begin
      // it's at least an attempt at a valid key mappings file...
      with frmMain.ActionList do
      begin
        for i := 0 to ActionCount -1 do
        begin
          sc := ini.ReadString(Actions[i].Category, Actions[i].Name, '');
          if '' <> sc then
            TAction(Actions[i]).ShortCut := TextToShortCut(sc)
          else
            TAction(Actions[i]).ShortCut := 0;
        end;
      end;
    end;
  Finally
    sct.Free;
    ini.Free;
  end;
  lstActionCommsClick(lstActionComms);
end;

procedure TfrmOptions.PropagateAssoc;
var sl  : tStringList;
    n   : integer;
    new : tListItem;
    ico : tIcon;
begin
   lstAssoc.Items.Clear;
   lstAssoc.RowSelect := True;
   sl := tStringList.Create;
   sl.Sorted := true;
   sl.Duplicates := dupIgnore;   
   sl.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'files.def');
   For n := 0 to sl.Count - 1 do
   begin
     new := lstAssoc.Items.Add;
     new.Caption := sl[n];
     new.SubItems.Add('0');
     new.SubItems.Add('0');
     new.SubItems[0] := BoolToStr(IsMyExt(sl[n]));
     new.SubItems[1] := BoolToStr(IsMyOpenWith(sl[n]));
     ico := tIcon.Create;
     ico.Handle := GetRegIcon(new.Caption, True);
     new.ImageIndex := ilsAssoc.addicon(ico);
     ico.Free;
   end;
   sl.Free;
end;

procedure TfrmOptions.SetAssoc;
var n : integer;
begin
   Try
      for n := 0 to lstAssoc.Items.Count - 1 do
      Begin
         if strtobool(lstAssoc.Items[n].SubItems[0]) then
         begin
            // Associate type with PN...
            RegisterWindowsExt(lstAssoc.Items[n].Caption);
            // Remove any PN Openwith entry...
            RemoveWindowsOW(lstassoc.Items[n].Caption);
         end else
         if strtobool(lstAssoc.Items[n].SubItems[1]) then
         begin
            // Restore original association...
            RestoreWindowsExt(lstAssoc.Items[n].Caption);
            // Add an OpenWith item...
            RegisterWindowsOW(lstAssoc.Items[n].Caption);
         end else
         begin
            RestoreWindowsExt(lstAssoc.Items[n].Caption);
            RemoveWindowsOW(lstAssoc.Items[n].Caption);
         end;
      End;
      RegisterApp;
   finally
   End;
end;

procedure TfrmOptions.SaveAssoc;
var n  : integer;
    sl : tStringList;
begin
   sl := tStringList.Create;
   For n := 0 to lstAssoc.Items.Count - 1 do
   begin
      sl.Add(lstAssoc.Items[n].Caption);
   end;
   sl.SaveToFile(ExtractFilePath(ParamStr(0)) + 'files.def');
   sl.Free;
   lstAssoc.Items.Clear;
end;

procedure TfrmOptions.PropagateNew;
var ini : tIniFile;
    num : integer;
    n   : integer;
    new : tListItem;
begin
  // Propagate the new menu options...
  ini := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'new.def');
  try
    num := ini.ReadInteger('New', 'Count', 0);
    For n := 0 to num - 1 do
    begin
      new := lstNew.Items.Add;
      new.Caption := ini.ReadString(inttostr(n), 'Name', 'New');
      new.SubItems.Add( ini.ReadString(inttostr(n), 'Title', 'new') );
      new.SubItems.Add( ini.ReadString(inttostr(n), 'Scheme', '0') );
    end;
  finally
    ini.free;
  end;
end;

procedure TfrmOptions.WriteFolders;
var tf : TextFile;
    n  : integer;
begin
  AssignFile(tf, ExtractFilePath(ParamStr(0)) + 'folders.def');
  Try
    Rewrite(tf);
    For n := 0 to lstFolders.Items.Count -1 do
      Writeln(tf, lstFolders.Items[n].Caption);
  Finally
    CloseFile(tf);
  End;
end;

procedure TfrmOptions.PropagateFolders;
var tf : TextFile;
    li : tListItem;
    s  : string;
begin
  If not fileexists(ExtractFilePath(ParamStr(0)) + 'folders.def') then exit;
  AssignFile(tf, ExtractFilePath(ParamStr(0)) + 'folders.def');
  Try
    Reset(tf);
    Repeat
      Readln(tf, s);
      If length(s) > 0 then
      begin
        li := lstFolders.Items.Add;
        li.Caption := s;
        li.ImageIndex := 0;
      end;
    Until EOF(tf);
  Finally
    CloseFile(tf);
  End;
end;

procedure TfrmOptions.SaveNew;
var ini : tIniFile;
    n   : integer;
begin
  // Save the new menu options...
  ini := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'new.def');
  Try
    ini.WriteInteger('New', 'Count', lstNew.Items.Count);
    For n := 0 to lstNew.Items.Count - 1 do
    begin
      ini.WriteString(inttostr(n), 'Name', lstNew.Items[n].Caption);
      ini.WriteString(inttostr(n), 'Title', lstNew.Items[n].SubItems[0]);
      ini.WriteString(inttostr(n), 'Scheme', lstNew.Items[n].SubItems[1]);
    end;
  Finally
    ini.Free;
  end;
end;

procedure TfrmOptions.PropagateSchemes;
var n : integer;
    l : tListItem;
    s : string;
begin
   For n := 0 to frmMain.Parsers.Count - 1 do
   begin
     s := tSyntaxMemoParser(frmMain.Parsers.TagItems(n)).UI_Styles.LangName;
     If s <> '' then
     begin
       // Add a list item to the schemes dialog.
       if not ((n > 0) and (s = 'Default')) then
       begin
         l := lstSchemes.Items.Add;
         l.Caption := s;
         If n > 0 then l.SubItems.Add(frmMain.Parsers.TagFiles(n) + '.*') else l.SubItems.Add('<na>');
         l.SubItems.Add(IntToStr(n));
         // Here we propagate the parsers combo box on the types page.
         cmbParser.Items.add(s);
         // Here we propagate the parsers combo box on the new menu page.
         cmbParser2.Items.add(s);
         // Here we propagate the parsers combo box on the tools page.
         cmbSchemes.Items.add(s);
         PIs.Add( inttostr( n ) );
       end;
     end;
   end;
end;

procedure TfrmOptions.PropagateBrowsers;
var li : TListItem;
    et : TIniFile;
    i  : Integer;
    n  : Integer;
    x  : Integer;

procedure CreateDefaultIE;
begin
  li := lstBrowsers.Items.Add;
  li.Caption := 'Built-In (IE)';
  li.ImageIndex := 43;
  li.SubItems.Add('0');
  li.Checked := True;
end;

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

procedure CreateDefaultMoz;
begin
  if not MozillaInstalled then Exit;
  li := lstBrowsers.Items.Add;
  li.Caption := 'Built-In (Mozilla)';
  li.ImageIndex := 44;
  li.SubItems.Add('1');
  li.Checked := True;
end;

begin
  et := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  Try
    i := et.ReadInteger('.Browsers', 'Number', 0);
    if i = 0 then begin CreateDefaultIE; CreateDefaultMoz; Exit; end;
    if et.ReadInteger('.Browsers', 'BrowserSystem', 1) = 1 then
    begin
      if MozillaInstalled then
        if MessageDlg('There may be a new browser available to you (Mozilla).'+#13+#10+'Do you wish to add it to your browser list?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          CreateDefaultMoz;
        end;
    end;
    for n := 0 to i - 1 do
    begin
      li := lstBrowsers.Items.Add;
      li.ImageIndex := 12;
      li.Caption := et.ReadString('.Browsers', 'Browser' + IntToStr(n), 'Built-In (IE)');
      case et.ReadBool('.Browsers', 'BuiltIn' + IntToStr(n), False) of
        True :
        begin
          li.Checked := True;
          x := et.ReadInteger('.Browsers', 'IbId' + IntToStr(n), 0);
          li.SubItems.Add(IntToStr(x));
          case x of
            0 : li.ImageIndex := 43;
            1 : li.ImageIndex := 44;
          end;
        end;
        False :
        begin
          li.Checked := False;
          li.SubItems.Add(CStringToString(et.ReadString('.Browsers', 'App' + IntToStr(n), '')));
          li.SubItems.Add(CStringToString(et.ReadString('.Browsers', 'Dir' + IntToStr(n), '')));
          li.SubItems.Add(CStringToString(et.ReadString('.Browsers', 'Par' + IntToStr(n), '')));
          li.SubItems.Add(et.ReadString('.Browsers', 'Keyb' + IntToStr(n), ''));
        end;
      end;
    end;
  Finally
    et.Free;
  end;
end;

procedure TfrmOptions.WriteBrowsers;
var et : TIniFile;
    n  : Integer;
    li : TListItem;
begin
  et := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  Try
    et.WriteInteger('.Browsers', 'Number', lstBrowsers.Items.Count);
    et.WriteInteger('.Browsers', 'BrowserSystem', BrowserVer);
    for n := 0 to lstBrowsers.Items.Count - 1 do
    begin
      li := TListItem(lstBrowsers.Items[n]);
      et.WriteString('.Browsers', 'Browser' + IntToStr(n), li.Caption);
      et.WriteBool('.Browsers', 'Cap' + IntToStr(n), False);
      et.WriteBool('.Browsers', 'Ask' + IntToStr(n), False);
      if (not li.Checked) then
      begin
        et.WriteBool('.Browsers', 'BuiltIn' + IntToStr(n), False);
        et.WriteString('.Browsers', 'App' + IntToStr(n), StringToCString(li.SubItems[0]));
        et.WriteString('.Browsers', 'Dir' + IntToStr(n), StringToCString(li.SubItems[1]));
        et.WriteString('.Browsers', 'Par' + IntToStr(n), StringToCString(li.SubItems[2]));
        et.WriteString('.Browsers', 'Keyb' + IntToStr(n), li.SubItems[3]);
      end else
      begin
        et.WriteBool('.Browsers', 'BuiltIn' + IntToStr(n), True);
        et.WriteInteger('.Browsers', 'IbId' + IntToStr(n), StrToInt(li.SubItems[0]));
      end;
    end;

  Finally
    et.Free;
  end;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
var
   n,
   i      : Integer;
   new    : tListItem;
   reg    : tRegistry;
   myd    : integer;
   pd     : PPluginData;
begin
   CurrScheme := '';
   // Set the Active Page...
   notOptions.ActivePage := 'General';
   // Set-up the Tools page combo-box...
   cmbSchemes.Items.Clear;
   cmbSchemes.Items.Add('Choose a scheme or menu to configure...');
   cmbSchemes.Items.Add('Tools Menu');
   cmbSchemes.ItemIndex := 0;
   // Load the file types page with data...
   Try PropagateTypes Except End;
   // Load the file associations page with data...
   Try PropagateAssoc Except End;
   // Load the schemes page with data...
   Try PropagateSchemes Except End;
   // Load the new page with data...
   Try PropagateNew Except End;
   // Load the Folders page with data...
   Try PropagateFolders Except End;
   // Load the Browsers information...
   try PropagateBrowsers except end;
   // Load the Keyboard Mappings information...
   try PropagateKeyboard except end;
   {SysEdit}
   chkAutoExec.Checked := seAutoExec in frmMain.SEFiles;
   chkConfig.Checked := seConfig in frmMain.SEFiles;
   chkWinIni.Checked := seWinIni in frmMain.SEFiles;
   chkSysIni.Checked := seSysIni in frmMain.SEFiles;
   chkProt.Checked := seProt in frmMain.SEFiles;
   chkShowTabs.Checked := frmMain.actTabs.Checked;
   cbWordWrap.Checked := frmMain.actWordWrap.Checked;
   chkShowClock.Checked := frmMain.actClock.Checked;
   txtClockFormat.Text := frmMain.ClockFormat;
   chkShowClockClick(self);
   chkLaunchURLs.Checked     := frmMain.DoURLs;
   chkLaunchURLsClick(Self);
   chkMaximiseFiles.Checked  := frmMain.OpenFilesMaxed;
   txtMRUMax.Text            := IntToStr(frmMain.MRUFileList1.Maximum);
   chkClearUndo.Checked      := frmMain.LoseUndo;
   chkQSReturn.Checked       := not frmMain.QSKeepFocus;
   chkQSLeave.Checked        := not chkQSReturn.Checked;
   chkMultiLineTabs.Checked  := frmMain.tabs.MultiLine;
   chkHighlightActive.Checked := frmMain.HighlightActive;
   chkHighlightChange.Checked := frmMain.HighlightChange;
   cmbHighlightActiveColor.SelectionColor := frmMain.HighlightColor;
   cmbUnderlineColor.SelectionColor := frmMain.ModifiedColor;
   chkHighlightActiveClick(chkHighlightActive);
   chkHighlightActiveClick(chkHighlightChange);
   for n := 0 to panWarnOpenAgain.ControlCount - 1 do
   begin
     with panWarnOpenAgain do
       if TRadioButton(Controls[n]).Tag = frmMain._wnOpenFileAgain then
         TRadioButton(Controls[n]).Checked := True;
   end;
   Reg := tRegistry.Create;
   Try
     Reg.OpenKey(rootkey, true);
     Try myd := Reg.ReadInteger('StartDir'); Except myd := 0; end;
     Case myd of
       0 : radMyDocuments.Checked := true;
       1 : radLastUsed.Checked := true;
       2 : begin
             radCustom.Checked := true;
             try
               txtStartDir.Text := reg.ReadString('CustomStartDir');
             except
               txtStartDir.Text := extractfilepath(paramstr(0));
             end;
           end;
     else
       radMyDocuments.Checked := true;
     end;
     Try chkSaveSchemeSettings.Checked := reg.ReadBool('SaveSchemeSettings'); except chkSaveSchemeSettings.Checked := true; end;
     Try chkSaveDFMsAsText.Checked := not reg.ReadBool('SaveDFMAsBinary'); except chkSaveDFMsAsText.Checked := False; end;
     try chkQSKeepHist.Checked := reg.ReadBool('SaveQSHistory'); except chkQSKeepHist.Checked := False; end;
     try chkOpenWholeProject.Checked := reg.ReadBool('WarnOpenProj'); except chkOpenWholeProject.Checked := True; end;
     try chkDeleteFileCheck.Checked := reg.ReadBool('WarnDeleteFile'); except chkDeleteFileCheck.Checked := True; end;
     Reg.CloseKey;
     Reg.OpenKey(netkey, true);
     Try radSingleClick.Checked := not (reg.ReadBool('URLLaunchDouble')); except radDoubleClick.Checked := true; end;
     Try
       chkUseProxy.Checked       := reg.ReadBool('UseProxy');
       txtProxyHost.Text         := reg.ReadString('ProxyHost');
       txtProxyPort.Text         := reg.ReadString('ProxyPort');
     except
       {Probably First Time Running}
       txtProxyHost.Text         := '';
       txtProxyPort.Text         := '8080';
       chkUseProxy.Checked       := False;
     end;
     reg.CloseKey;
     reg.OpenKey(EditKey, True);
     try chkShowHighlight.Checked := reg.ReadBool('HighlightOnContext'); except chkShowHighlight.Checked := True; end;
   Finally
     Reg.Free;
   End;
   chkUseProxyClick(Self);
   {$IFDEF plugin}
   for i := 0 To frmMain.Plugins.Count - 1 do
   begin
      new := lstPlugins.Items.Add;
      pd := frmMain.Plugins.Items[i];
      new.Caption := pd^.Name;
      new.SubItems.Add(pd^.FileName);
   end;
   {$ENDIF}
   for n := 0 to treOptions.Items.Count - 1 do
      treOptions.Items[n].Expand(False);
   // Select the 'General' node...
   tTreeNode(treOptions.TopItem).Selected := True;
end;

function MoveUp(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
var item2 : tListItem;
begin
   item2 := tListItem(lParam2);
   Result := 0;
   If not MoveMade Then
      If item2.Index = lParamSort Then
      Begin
         Result := 1;
         MoveMade := True;
      End Else Result := 0;
end;

function MoveDown(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
var item1 : tListItem;
begin
   item1 := tListItem(lParam1);
   Result := 0;
   If not MoveMade Then
      If item1.Index = lParamSort Then
      Begin
         Result := 1;
         MoveMade := True;
      End Else Result := 0;
end;

procedure TfrmOptions.itmAlphaSortClick(Sender: TObject);
begin
   lstProgs.CustomSort(nil, 0);
end;

procedure TfrmOptions.btnAddAssocClick(Sender: TObject);
var S : string;
    new : tListItem;
begin
   s := '';
   If InputQuery('New Extension', 'Enter the new extension...', s) then
   begin
      if Copy(s, 1, 1) <> '.' then s := '.' + s;
      s := RemoveSpaces(s);
      new := lstAssoc.Items.Add;
      new.Caption := s;
      new.SubItems.Add('1');
      new.SubItems.Add('0');
   end;
end;

procedure TfrmOptions.chkUseProxyClick(Sender: TObject);
begin
   case chkUseProxy.Checked of
      True  :
         Begin
            txtProxyHost.Color := clWindow;
            txtProxyPort.Color := clWindow;
            txtProxyHost.Enabled := True;
            txtProxyPort.Enabled := True;
         end;
      False :
         begin
            txtProxyHost.Color := clBtnFace;
            txtProxyPort.Color := clBtnFace;
            txtProxyHost.Enabled := False;
            txtProxyPort.Enabled := False;
         end;
   end;

end;

procedure TfrmOptions.txtProxyPortKeyPress(Sender: TObject; var Key: Char);
begin
   if (not _BSOverride) then
      if not (Key in ['0'..'9']) then Key := #0;
   _BSOverride := False;
end;

procedure TfrmOptions.txtProxyPortKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = VK_Back then _BSOverride := True;
end;

procedure TfrmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   SaveAssoc;
   PIs.Free;
   Actions.Free;
   Mappings.Free;
end;


procedure TfrmOptions.PropagateTypes;
var t        : TextFile;
    s        : string;
    ps       : string; {part of string}
    FileName : string;
    Part     : integer;
    ipos     : integer;
    new      : tListItem;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
      Unix   = 4;
      Hex    = 5;
      Ver    = 6;
begin
   // Load the file types into the dialog...
   new := nil;
   FileName := ExtractFilePath(ParamStr(0)) + 'types.def';
   if not fileexists(filename) then
      CreatePNFile(filename, strOpenTypes);
   Assignfile(t, FileName);
   Reset(t);
   repeat
      Readln(t, s)
   until (Length(s) > 0) or EOF(t);
   CloseFile(t);
   if s = '' then Exit;
   part := Ver;
   repeat
      iPos := Pos(SepChar, s);
      if (iPos = 0) and (Length(s) > 0) then
      begin
         ps := s;
         s := '';
      end else
         ps := Copy(s, 1, ipos - 1);
      s := Copy(S, ipos + 1, Length(s));
      case part of
         Ver  : begin
                  If copy(ps, 1,1) <> VerChar then
                  begin
                     // This is an old old definition file, we need to convert it.
                     ConvertTypes(filename);
                     // And now re-read the definition file string...
                     s := '';
                     Assignfile(t, filename);
                     Reset(t);
                     repeat
                       Readln(t, s);
                     until (length(s) > 0) or EOF(t);
                     System.Closefile(t);
                     part := Ver;
                  end
                  else if copy(ps, 2, 1) <> CurrFileVer then
                  begin
                     // And old version of the definition file.
                     // this shouldn't exist yet, so I'm not doing anything.
                     {!Todo Add version checking and updating code...}
                  end else
                  part := Desc;
                end;
         Desc : begin
                  new := lstTypes.Items.Add;
                  new.Caption := ps;
                  part := Files;
                end;
         Files : begin
                   new.SubItems.Add(ps);
                   part := Parser;
                 end;
         Parser : begin
                    new.SubItems.Add(ps);
                    part := Unix;
                  end;
         Unix   : begin
                    new.SubItems.Add(ps);
                    part := Hex;
                  end;
         Hex    : begin
                    new.SubItems.Add(ps);
                    part := Desc;
                  end;
      end;
   until Length(s) < 1;
end;

procedure TfrmOptions.WriteTypes;
var FileName : string;
    t        : TextFile;
    n        : integer;
    s        : string;
begin
   // Write the Type List to Disk...
   s := VerChar + CurrFileVer + sepChar;
   for n := 0 to lstTypes.Items.Count - 1 do
   begin
      if Length(s) > Length(VerChar + CurrFileVer + sepChar) then s := s + SepChar;
      s := s + lstTypes.Items[n].Caption + sepChar + lstTypes.Items[n].SubItems[0] + SepChar + lstTypes.Items[n].SubItems[1] + SepChar + lstTypes.Items[n].SubItems[2] + sepChar + lstTypes.Items[n].SubItems[3];
   end;
   FileName := ExtractFilePath(ParamStr(0)) + 'types.def';
   AssignFile(t, FileName);
   Rewrite(t);
   Write(t, s);
   CloseFile(t);
end;

procedure TfrmOptions.lstTypesClick(Sender: TObject);
begin
   if lstTypes.SelCount > 0 then
   begin
      txtType.Text := lstTypes.Selected.Caption;
      txtDesc.Text := lstTypes.Selected.SubItems[0];
      {$IFNDEF Parsers}
        cmbParser.ItemIndex := StrToInt(lstTypes.Selected.SubItems[1]);
      {$ELSE}
        cmbParser.ItemIndex := PIs.IndexOf(lstTypes.Selected.SubItems[1]);
      {$ENDIF}
      chkUnixSave.Checked := StrToInt(lstTypes.Selected.SubItems[2]) = 1;
      chkOpenAsHex.Checked := StrToInt(lstTypes.Selected.SubItems[3]) = 1;
      btnTypeEdit.Enabled := lstTypes.SelCount = 1;
      btnTypeRemove.Enabled := True;
      btnTypeMoveUp.Enabled := lstTypes.SelCount = 1;
      btnTypeMoveDown.Enabled := lstTypes.SelCount = 1;
   end else
   begin
      btnTypeEdit.Enabled := False;
      btnTypeRemove.Enabled := False;
      btnTypeMoveUp.Enabled := False;
      btnTypeMoveDown.Enabled := False;
      chkUnixSave.Checked := False;
      chkOpenAsHex.Checked := False;
      txtType.Text := '';
      txtDesc.Text := '';
      cmbParser.Text := '';
   end;
end;

procedure TfrmOptions.btnTypeEditClick(Sender: TObject);
begin
   case btnTypeEdit.Tag of
      0 : if lstTypes.SelCount = 1 then
          begin
             btnTypeEdit.Tag := 1; {Editing Caption}
             btnTypeEdit.Caption := '&Accept';
             btnTypeEdit.Glyph := imgTick.Picture.Bitmap;
             btnTypeEdit.HelpContext := 84;
             btnTypeRemove.Tag := 1; {Cancel Caption}
             btnTypeRemove.Caption := '&Back';
             btnTypeRemove.Glyph := imgCancel.Picture.Bitmap;
             btnTypeRemove.HelpContext := 86;
             txtType.Enabled := True;
             txtDesc.Enabled := True;
             chkUnixSave.Enabled := True;
             chkOpenAsHex.Enabled := True;
             cmbParser.Enabled := True;
             lstTypes.Enabled := False;
             btnTypeAdd.Enabled := False;
             btnTypeMoveUp.Enabled := False;
             btnTypeMoveDown.Enabled := False;
             btnOK.default := False;
             btnTypeEdit.default := True;
             btnCancel.cancel := False;
             btnTypeRemove.cancel := True;
             ActiveControl := txtType;
          end;
      1 : if lstTypes.SelCount = 1 then //Accept Clicked...
          begin
             lstTypes.Selected.Caption := txtType.Text;
             lstTypes.Selected.SubItems[0] := txtDesc.Text;
             {$ifndef parsers}
               lstTypes.Selected.SubItems[1] := IntToStr(cmbParser.ItemIndex);
             {$else}
               lstTypes.Selected.SubItems[1] := PIs[cmbParser.ItemIndex];
             {$endif}
             Case chkUnixSave.Checked of
                True : lstTypes.Selected.SubItems[2] := '1';
                False : lstTypes.Selected.SubItems[2] := '0';
             end;
             Case chkOpenAsHex.Checked of
                True : lstTypes.Selected.SubItems[3] := '1';
                False : lstTypes.Selected.SubItems[3] := '0';
             end;
             btnTypeEdit.Tag := 0;
             btnTypeEdit.Caption := '&Edit';
             btnTypeEdit.Glyph := imgRename.Picture.Bitmap;
             btnTypeEdit.HelpContext := 83;
             btnTypeRemove.Tag := 0;
             btnTypeRemove.Caption := '&Remove';
             btnTypeRemove.Glyph := imgMinus.Picture.Bitmap;
             btnTypeRemove.Tag := 85;
             txtType.Enabled := False;
             txtDesc.Enabled := False;
             chkUnixSave.Enabled := False;
             chkOpenAsHex.Enabled := False;
             cmbParser.Enabled := False;
             lstTypes.Enabled := True;
             btnTypeAdd.Enabled := True;
             btnTypeMoveUp.Enabled := True;
             btnTypeMoveDown.Enabled := True;
             btnTypeEdit.default := False;
             btnOk.default := True;
             btnTypeRemove.cancel := False;
             btnCancel.cancel := True;
          end;
   end;
end;

procedure TfrmOptions.btnTypeRemoveClick(Sender: TObject);
var n : integer;
begin
   case btnTypeRemove.Tag of
      0 : if lstTypes.SelCount > 0 then
          begin
             for n := lstTypes.Items.Count - 1 downto 0 do
             begin
                if lstTypes.Items[n].Selected then
                   if MessageDlg('Really remove "' + lstTypes.Items[n].Caption +  '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then lstTypes.Items.Delete(n);
             end;
          end;
      1 : begin // Cancel Clicked...
             btnTypeEdit.Tag := 0;
             btnTypeEdit.Caption := '&Edit';
             btnTypeEdit.Glyph := imgRename.Picture.Bitmap;
             btnTypeEdit.HelpContext := 83;
             btnTypeRemove.Tag := 0;
             btnTypeRemove.Caption := '&Remove';
             btnTypeRemove.Glyph := imgMinus.Picture.Bitmap;
             btnTypeRemove.HelpContext := 85;
             txtType.Enabled := False;
             txtDesc.Enabled := False;
             cmbParser.Enabled := False;
             chkUnixSave.Enabled := False;
             chkOpenAsHex.Enabled := False;
             lstTypes.Enabled := True;
             btnTypeAdd.Enabled := True;
             btnTypeMoveUp.Enabled := True;
             btnTypeMoveDown.Enabled := True;
             btnTypeEdit.default := False;
             btnOk.default := True;
             btnTypeRemove.cancel := False;
             btnCancel.cancel := True;
          end;
      2 : begin // Cancel clicked for new item...
             lstTypes.Selected.Delete; {delete unused new item}
             btnTypeEdit.Tag := 0;
             btnTypeEdit.Caption := '&Edit';
             btnTypeEdit.Glyph := imgRename.Picture.Bitmap;
             btnTypeEdit.HelpContext := 83;
             btnTypeRemove.Tag := 0;
             btnTypeRemove.Caption := '&Remove';
             btnTypeRemove.Glyph := imgMinus.Picture.Bitmap;
             btnTypeRemove.HelpContext := 85;
             txtType.Enabled := False;
             txtDesc.Enabled := False;
             chkUnixSave.Enabled := False;
             chkOpenAsHex.Enabled := False;
             cmbParser.Enabled := False;
             lstTypes.Enabled := True;
             btnTypeAdd.Enabled := True;
             btnTypeMoveUp.Enabled := True;
             btnTypeMoveDown.Enabled := True;
             btnTypeEdit.default := False;
             btnOk.default := True;
             btnTypeRemove.cancel := False;
             btnCancel.cancel := True;
          end;
   end;
end;

procedure TfrmOptions.btnTypeAddClick(Sender: TObject);
var n   : integer;
    new : tListItem;
begin
   txtType.Text := '';
   txtDesc.Text := '';
   chkUnixSave.Checked := False;
   cmbParser.ItemIndex := 0;
   txtType.Enabled := True;
   txtDesc.Enabled := True;
   chkUnixSave.Enabled := True;
   chkOpenAsHex.Enabled := True;
   cmbParser.Enabled := True;
   btnTypeEdit.Enabled := True;
   btnTypeRemove.Enabled := True;
   new := lstTypes.Items.Add;
   new.Caption := 'New Type';
   new.SubItems.Add('');
   new.SubItems.Add('0');
   new.SubItems.Add('0'); // Save in unix boolean.
   new.SubItems.Add('0'); // Open As Hex boolean.
   for n := 0 to lstTypes.Items.Count -1 do
      lstTypes.Items[n].Selected := False;
   new.MakeVisible(False);
   new.Selected := True;
   lstTypes.Enabled := False;
   btnTypeEdit.Tag := 1; {Editing Caption}
   btnTypeEdit.Caption := '&Accept';
   btnTypeEdit.Glyph := imgTick.Picture.Bitmap;
   btnTypeEdit.HelpContext := 84;
   btnTypeRemove.Tag := 2; {Cancel Edit Caption}
   btnTypeRemove.Caption := '&Back';
   btnTypeRemove.Glyph := imgCancel.Picture.Bitmap;
   btnTypeRemove.HelpContext := 86;
   btnTypeAdd.Enabled := False;
   btnTypeMoveUp.Enabled := False;
   btnTypeMoveDown.Enabled := False;
   btnOk.default := False;
   btnTypeEdit.default := True;
   btnCancel.cancel := False;
   btnTypeRemove.cancel := True;
   ActiveControl := txtType;
end;

var _sortmode : Boolean;

function SortUp(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
   if (tListItem(lParam1).Index = lParamSort - 1) and (tListItem(lParam2).Index = lParamSort) and _sortmode then
   begin
      Result := +1;
      _sortmode := False;
   end else
      Result := 0;
end;

function SortDown(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
   if _sortmode then
   begin
      if tListItem(lParam1).Index = lParamSort then
      begin
         Result := 1;
         _sortmode := False;
      end else Result := 0;
   end else Result := 0;
end;

procedure TfrmOptions.btnTypeMoveUpClick(Sender: TObject);
begin
   case TSpeedButton(Sender).Tag of
      1 : if lstTypes.SelCount = 1 then
          begin
             if lstTypes.Selected.Index <> 0 then
             begin
                _sortmode := True;
                lstTypes.CustomSort(SortUp, lstTypes.Selected.Index);
             end;
          end;
      2 : if lstTypes.SelCount = 1 then
          begin
             if lstTypes.Selected.Index <> lstTypes.Items.Count - 1 then
             begin
                _sortmode := True;
                lstTypes.CustomSort(SortDown, lstTypes.Selected.Index);
             end;
          end;
   end;
end;

procedure TfrmOptions.lstTypesDblClick(Sender: TObject);
begin
   btnTypeEdit.Click;
end;

procedure TfrmOptions.lstPluginsClick(Sender: TObject);
begin
   btnConfigurePlugin.Enabled := lstPlugins.SelCount = 1;
   btnAboutPlugin.Enabled := lstPlugins.SelCount = 1;
end;

procedure TfrmOptions.btnConfigurePluginClick(Sender: TObject);
var
   DLLHandle    : THandle;
   PluginInit   : tPluginInit;
   PluginMsgNum : TPlgMsgNum;
   PluginConfig : TIntResult;
   i            : integer;
begin
{$IFDEF plugin}
   if lstPlugins.SelCount <> 1 then Exit;
   DLLHandle := LoadLibrary(PChar(ExtractFileName(lstPlugins.Selected.SubItems[0])));
   if DLLHandle <> 0 then
   try
      @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
      i := PluginInit(Application.Handle);
      if i <> 0 then
      begin // Skip to finally, and Free the DLL.
         @PluginMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
         // Pass the DLL the main Message Number.
         // It shouldn't really be needed for the
         // configuration, but you never know.
         PluginMsgNum(frmMain.MsgNum);
         @PluginConfig := GetProcAddress(DLLHandle, 'Configure');
         PluginConfig;
      end;
   finally
      FreeLibrary(DLLHandle);
   end;
{$ENDIF}
end;

procedure TfrmOptions.btnAboutPluginClick(Sender: TObject);
var
   DLLHandle    : THandle;
   PluginInit   : tPluginInit;
   PluginMsgNum : TPlgMsgNum;
   PluginAbout  : TPlgProc;
   i            : integer;
begin
{$IFDEF plugin}
   DLLHandle := LoadLibrary(PChar(ExtractFileName(lstPlugins.Selected.SubItems[0])));
   if DLLHandle <> 0 then
   try
      @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
      i := PluginInit(Application.Handle);
      if i <> 0 then
      begin // Skip to finally, and Free the DLL.
         @PluginMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
         // Pass the DLL the main Message Number.
         // It shouldn't really be needed for the
         // about box, but you never know.
         PluginMsgNum(frmMain.MsgNum);
         // Invite the Plugin to show it's about stuff... we're not interested
         // in the result, so it's only a procedure call.
         @PluginAbout := GetProcAddress(DLLHandle, 'ShowAbout');
         PluginAbout;
      end;
   finally
      FreeLibrary(DLLHandle);
   end;
{$ENDIF}
end;

procedure TfrmOptions.lstAssocClick(Sender: TObject);
var icon : TIcon;
begin
   If assigned(lstAssoc.Selected) then
   begin
     chkAssocOpen.Checked := StrToBool(lstAssoc.Selected.SubItems[0]);
     chkAssocOpenWith.Checked := StrToBool(lstAssoc.Selected.SubItems[1]);
     If (not StrToBool(lstAssoc.Selected.SubItems[0])) and (not StrToBool(lstAssoc.Selected.SubItems[1])) then
        chkAssocNone.Checked := True;
     icon := TIcon.Create;
     icon.Handle := GetRegIcon(lstAssoc.Selected.Caption, False);
     imgIcon.Picture.Icon.Assign(icon);
     icon.Free;
     btnIconChoose.Enabled := chkAssocOpen.Checked;
   end;
end;

procedure TfrmOptions.lstAssocDrawItem(Control: TWinControl;
  var ACanvas: TCanvas; Index: Integer; ARect: TRect;
  State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: Boolean);
begin
   DefaultDrawing := True;
   FullRowSelect  := True;
   If StrToBool(lstAssoc.Items[Index].SubItems[0]) Then
      ACanvas.Font.Color := clRed else
   If StrToBool(lstAssoc.Items[Index].SubItems[1]) then
      ACanvas.Font.Color := clGreen;
end;

procedure TfrmOptions.chkAssocNoneClick(Sender: TObject);
begin
   If not Assigned(lstAssoc.Selected) then exit;

   Case tRadioButton(Sender).Tag of
      0 : begin
            lstAssoc.Selected.SubItems[0] := '0';
            lstAssoc.Selected.SubItems[1] := '0';
          end;
      1 : begin
            lstAssoc.Selected.SubItems[0] := '1';
            lstAssoc.Selected.SubItems[1] := '0';
            btnIconChoose.Enabled := True;
          end;
      2 : begin
            lstAssoc.Selected.SubItems[0] := '0';
            lstAssoc.Selected.SubItems[1] := '1';
          end;
   end;
end;

procedure TfrmOptions.btnConfigureSchemeClick(Sender: TObject);
var n : integer;
begin
  If Assigned(lstSchemes.Selected) then
  begin
    // Get the tag of the parser to be configured...
    n := strtoint( lstSchemes.Selected.SubItems[1] );
    if n = 0 then frmMain.Parsers.ModifyPlain else
      tSyntaxMemoParser( frmMain.Parsers.TagItems(n) ).ModifyProperties;
  end;
end;

procedure TfrmOptions.btnResetSchemeClick(Sender: TObject);
var s   : string;
    n   : integer;
    reg : tRegistry;
    fs  : tFileStream;
begin
  If Assigned(lstSchemes.Selected) then
  begin
    s := lstSchemes.Selected.Caption;
    Screen.Cursor := crAppStart;
    reg := tRegistry.Create;
    Try
      reg.OpenKey('Software\Echo Software\PN\Highlighting', True);
      reg.DeleteValue('Highlighting - ' + s);
      reg.DeleteValue('Key Assignments - ' + s);
      reg.CloseKey;
    Finally
      reg.Free;
    End;
    n := strtoint( lstSchemes.Selected.SubItems[1] );
    fs := tFileStream.Create(frmMain.Parsers.HomeDir + 'keys.def', fmOpenRead or fmShareCompat);
    fs.Seek(0,0);
    tSyntaxMemoParser( frmMain.Parsers.TagItems(n) ).UI_Styles.KeyController.LoadFromStream(fs);
    fs.Free;
    tSyntaxMemoParser( frmMain.Parsers.TagItems(n) ).StylesFromRegistry(True, '');
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmOptions.btnResetAllSchemesClick(Sender: TObject);
var n   : integer;
    i   : integer;
    s   : string;
    reg : tRegistry;
    fs  : tFileStream;
begin
  fs := nil;
  If MessageDlg('Are you sure you wish to reset all schemes?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  If lstSchemes.Items.Count > 0 then
  begin
    reg := tRegistry.Create;
    reg.OpenKey('Software\Echo Software\PN\Highlighting', True);
    Try
      Screen.Cursor := crAppStart;
      fs := tFileStream.Create(frmMain.Parsers.HomeDir + 'keys.def', fmOpenRead or fmShareCompat);
      for n := 0 to lstSchemes.items.count - 1 do
      begin
        s := lstSchemes.Items[n].Caption;
        reg.DeleteValue('Highlighting - ' + s);
        reg.DeleteValue('Key Assignments - ' + s);
        fs.Seek(0,0);
        i := strtoint( lstSchemes.Items[n].SubItems[1] );
        tSyntaxMemoParser( frmMain.Parsers.TagItems(i) ).UI_Styles.KeyController.LoadFromStream(fs);
        tSyntaxMemoParser( frmMain.Parsers.TagItems(i) ).StylesFromRegistry(True, '');
      end;
    Finally
      Screen.Cursor := crDefault;
      try fs.Free except end;
      reg.CloseKey;
      reg.Free;
    End;
  end;
end;

procedure TfrmOptions.lstNewClick(Sender: TObject);
begin
   if lstNew.SelCount > 0 then
   begin
      // Set options from thingy...
      txtMenu.Text := lstNew.Selected.Caption;
      txtTitle.Text := lstNew.Selected.SubItems[0];
      cmbParser2.ItemIndex := PIs.IndexOf(lstNew.Selected.SubItems[1]);

      btnNewEdit.Enabled := lstNew.SelCount = 1;
      btnNewRemove.Enabled := True;
      btnNewMoveUp.Enabled := lstNew.SelCount = 1;
      btnNewMoveDown.Enabled := lstNew.SelCount = 1;
   end else
   begin
      btnNewEdit.Enabled := False;
      btnNewRemove.Enabled := False;
      btnNewMoveUp.Enabled := False;
      btnNewMoveDown.Enabled := False;
      txtMenu.Text := '';
      txtTitle.Text := '';
      cmbParser2.Text := '';
   end;
end;

procedure TfrmOptions.btnNewAddClick(Sender: TObject);
var n   : integer;
    new : tListItem;
begin
   txtMenu.Text := '';
   txtMenu.Enabled := True;

   txtTitle.Text := '';
   txtTitle.Enabled := True;

   cmbParser2.ItemIndex := 0;
   cmbParser2.Enabled := True;

   new := lstNew.Items.Add;
   new.Caption := 'Text';
   new.SubItems.Add('new file');
   new.SubItems.Add('0');
   for n := 0 to lstNew.Items.Count -1 do
      lstNew.Items[n].Selected := False;
   new.MakeVisible(False);
   new.Selected := True;
   lstNew.Enabled := False;

   btnNewEdit.Tag := 1; {Editing Caption}
   btnNewEdit.Caption := '&Accept';
   btnNewEdit.Glyph := imgTick.Picture.Bitmap;
   btnNewEdit.Enabled := True;
   btnNewEdit.HelpContext := 60;

   btnNewRemove.Tag := 2; {Cancel Edit Caption}
   btnNewRemove.Caption := '&Back';
   btnNewRemove.Glyph := imgCancel.Picture.Bitmap;
   btnNewRemove.Enabled := True;
   btnNewRemove.HelpContext := 62;

   btnNewAdd.Enabled := False;

   btnNewMoveUp.Enabled := False;
   btnNewMoveDown.Enabled := False;

   btnOk.default := False;
   btnNewEdit.default := True;
   btnCancel.cancel := False;
   btnNewRemove.cancel := True;
   ActiveControl := txtMenu;
end;

procedure TfrmOptions.btnNewEditClick(Sender: TObject);
begin
   case btnNewEdit.Tag of
      0 : if lstNew.SelCount = 1 then
          begin
             btnNewEdit.Tag := 1; {Editing Caption}
             btnNewEdit.Caption := '&Accept';
             btnNewEdit.Glyph := imgTick.Picture.Bitmap;
             btnNewEdit.HelpContext := 60;

             btnNewRemove.Tag := 1; {Cancel Caption}
             btnNewRemove.Caption := '&Back';
             btnNewRemove.Glyph := imgCancel.Picture.Bitmap;
             btnNewRemove.HelpContext := 62;

             txtMenu.Enabled := True;
             txtTitle.Enabled := True;

             cmbParser2.Enabled := True;
             lstNew.Enabled := False;
             btnNewAdd.Enabled := False;

             btnNewMoveUp.Enabled := False;
             btnNewMoveDown.Enabled := False;

             btnOK.default := False;
             btnNewEdit.default := True;
             btnCancel.cancel := False;
             btnNewRemove.cancel := True;
             ActiveControl := txtMenu;
          end;
      1 : if lstNew.SelCount = 1 then //Accept Clicked...
          begin
             lstNew.Selected.Caption := txtMenu.Text;
             lstNew.Selected.SubItems[0] := txtTitle.Text;
             lstNew.Selected.SubItems[1] := PIs[cmbParser2.ItemIndex];
             btnNewEdit.Tag := 0;
             btnNewEdit.Caption := '&Edit';
             btnNewEdit.Glyph := imgRename.Picture.Bitmap;
             btnNewEdit.HelpContext := 59;
             btnNewRemove.Tag := 0;
             btnNewRemove.Caption := '&Remove';
             btnNewRemove.Glyph := imgMinus.Picture.Bitmap;
             btnNewRemove.HelpContext := 61;
             txtMenu.Enabled := False;
             txtTitle.Enabled := False;
             cmbParser2.Enabled := False;
             lstNew.Enabled := True;
             btnNewAdd.Enabled := True;
             btnNewMoveUp.Enabled := True;
             btnNewMoveDown.Enabled := True;
             btnNewEdit.default := False;
             btnOk.default := True;
             btnNewRemove.cancel := False;
             btnCancel.cancel := True;
          end;
   end;
end;

procedure TfrmOptions.btnNewRemoveClick(Sender: TObject);
var n : integer;
begin
   case btnNewRemove.Tag of
      0 : if lstNew.SelCount > 0 then
          begin
             for n := lstNew.Items.Count - 1 downto 0 do
             begin
                if lstNew.Items[n].Selected then
                   if MessageDlg('Really remove "' + lstNew.Items[n].Caption +  '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then lstNew.Items.Delete(n);
             end;
          end;
      1 : begin // Cancel Clicked...
             btnNewEdit.Tag := 0;
             btnNewEdit.Caption := '&Edit';
             btnNewEdit.Glyph := imgRename.Picture.Bitmap;
             btnNewEdit.HelpContext := 59;
             btnNewRemove.Tag := 0;
             btnNewRemove.Caption := '&Remove';
             btnNewRemove.Glyph := imgMinus.Picture.Bitmap;
             btnNewRemove.HelpContext := 61;
             txtMenu.Enabled := False;
             txtTitle.Enabled := False;
             cmbParser2.Enabled := False;
             lstNew.Enabled := True;
             btnNewAdd.Enabled := True;
             btnNewMoveUp.Enabled := True;
             btnNewMoveDown.Enabled := True;
             btnNewEdit.default := False;
             btnOk.default := True;
             btnNewRemove.cancel := False;
             btnCancel.cancel := True;
          end;
      2 : begin // Cancel clicked for new item...
             lstNew.Selected.Delete; {delete unused new item}
             btnNewEdit.Tag := 0;
             btnNewEdit.Caption := '&Edit';
             btnNewEdit.Glyph := imgRename.Picture.Bitmap;
             btnNewEdit.HelpContext := 59;
             btnNewRemove.Tag := 0;
             btnNewRemove.Caption := '&Remove';
             btnNewRemove.Glyph := imgMinus.Picture.Bitmap;
             btnNewRemove.HelpContext := 61;
             txtMenu.Enabled := False;
             txtTitle.Enabled := False;
             txtMenu.Text := '';
             txtTitle.Text := '';
             cmbParser2.Enabled := False;
             lstNew.Enabled := True;
             btnNewAdd.Enabled := True;
             btnNewMoveUp.Enabled := True;
             btnNewMoveDown.Enabled := True;
             btnNewEdit.default := False;
             btnOk.default := True;
             btnNewRemove.cancel := False;
             btnCancel.cancel := True;
          end;
   end;
end;

procedure TfrmOptions.btnNewMoveUpClick(Sender: TObject);
begin
   case TSpeedButton(Sender).Tag of
      1 : if lstNew.SelCount = 1 then
          begin
             if lstNew.Selected.Index <> 0 then
             begin
                _sortmode := True;
                lstNew.CustomSort(SortUp, lstNew.Selected.Index);
             end;
          end;
      2 : if lstNew.SelCount = 1 then
          begin
             if lstNew.Selected.Index <> lstNew.Items.Count - 1 then
             begin
                _sortmode := True;
                lstNew.CustomSort(SortDown, lstNew.Selected.Index);
             end;
          end;
   end;
end;

procedure TfrmOptions.radCustomClick(Sender: TObject);
begin
  txtStartDir.Enabled := radCustom.Checked;
end;

procedure TfrmOptions.btnChooseDirClick(Sender: TObject);
begin
  If dlgBrowse.Execute then txtStartDir.Text := dlgBrowse.Selection;
end;

procedure TfrmOptions.btnSendToClick(Sender: TObject);
begin
  CreateSendToLink(Handle);
end;

procedure TfrmOptions.chkShowClockClick(Sender: TObject);
begin
  txtClockFormat.Enabled := chkShowClock.Checked;
end;

procedure TfrmOptions.txtClockFormatKeyPress(Sender: TObject;
  var Key: Char);
begin
  If not _bsoverride then
     If not (key in ['h','m','s',':','-']) then Key := #0;
  _bsoverride := false;
end;

procedure TfrmOptions.txtClockFormatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key = vk_back then _bsoverride := true;
end;

procedure TfrmOptions.chkLaunchURLsClick(Sender: TObject);
begin
  radSingleClick.Enabled := chkLaunchURLs.Checked;
  radDoubleClick.Enabled := chkLaunchURLs.Checked;
end;

procedure TfrmOptions.btnFolderAddClick(Sender: TObject);
var li : tListItem;
begin
  If dlgBrowse.Execute then
  begin
    li := lstFolders.Items.Add;
    li.Caption := dlgBrowse.Selection;
    li.ImageIndex := 0;
  end;
end;

procedure TfrmOptions.btnFolderRemoveClick(Sender: TObject);
begin
  If lstFolders.SelCount > 0 then
  begin
    lstFolders.Selected.Delete;
  end;
end;

procedure TfrmOptions.btnFolderEditClick(Sender: TObject);
var s : string;
begin
  If lstFolders.SelCount > 0 then
  begin
    s := lstFolders.Selected.Caption;
    If InputQuery('PN', 'Folder Path:', s) then
    begin
      If (length(s) > 0) and (DirExists(s)) then
        lstFolders.Selected.Caption := s;
    end;
  end;
end;

procedure TfrmOptions.lstFoldersClick(Sender: TObject);
begin
  btnFolderRemove.Enabled := lstFolders.SelCount > 0;
  btnFolderEdit.Enabled := lstFolders.SelCount > 0;
  btnFolderMoveUp.Enabled := lstFolders.SelCount > 0;
  btnFolderMoveDown.Enabled := lstFolders.SelCount > 0;
end;

procedure TfrmOptions.btnFolderMoveUpClick(Sender: TObject);
begin
  case TSpeedButton(Sender).Tag of
     1 : if lstFolders.SelCount = 1 then
         begin
            if lstFolders.Selected.Index <> 0 then
            begin
               _sortmode := True;
               lstFolders.CustomSort(SortUp, lstFolders.Selected.Index);
            end;
         end;
     2 : if lstFolders.SelCount = 1 then
         begin
            if lstFolders.Selected.Index <> lstFolders.Items.Count - 1 then
            begin
               _sortmode := True;
               lstFolders.CustomSort(SortDown, lstFolders.Selected.Index);
            end;
         end;
  end;
end;

procedure TfrmOptions.BitBtn2Click(Sender: TObject);
begin
  CreateQuickLaunchLink(Handle);
end;

procedure TfrmOptions.btnDesktopAddClick(Sender: TObject);
begin
  CreateDesktopLink(Handle);
end;

procedure TfrmOptions.treOptionsAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var r : tRect;
begin
  If Node.ImageIndex <> -1 then
  begin
    If Stage <> cdPostPaint then exit;
    With TTreeView(Sender) do
    begin
      Canvas.Pen.Color := UnderLineCol;
      r := Node.DisplayRect(True);
      r.Top := r.Bottom - 2;
      r.Left := r.Left + 1;
      Canvas.PenPos := r.TopLeft;
      Canvas.LineTo(r.Right, r.Top);
    end;
  end
  else treOptions.Canvas.Font.Style := [];

end;

procedure TfrmOptions.btnToolMoveUpClick(Sender: TObject);
begin
   case TSpeedButton(Sender).Tag of
      1 : if lstProgs.SelCount = 1 then
          begin
             if lstProgs.Selected.Index <> 0 then
             begin
                _sortmode := True;
                lstProgs.CustomSort(SortUp, lstProgs.Selected.Index);
             end;
          end;
      2 : if lstProgs.SelCount = 1 then
          begin
             if lstProgs.Selected.Index <> lstProgs.Items.Count - 1 then
             begin
                _sortmode := True;
                lstProgs.CustomSort(SortDown, lstProgs.Selected.Index);
             end;
          end;
      3 : if lstBrowsers.SelCount = 1 then
          begin
            if lstBrowsers.Selected.Index <> 0 then
            begin
              _sortmode := True;
              lstBrowsers.CustomSort(SortUp, lstBrowsers.Selected.Index);
            end;
          end;
      4 : if lstBrowsers.SelCount = 1 then
          begin
            if lstBrowsers.Selected.Index <> lstBrowsers.Items.Count - 1 then
            begin
              _sortmode := True;
              lstBrowsers.CustomSort(SortDown, lstBrowsers.Selected.Index);
            end;
          end;
   end;
end;

procedure TfrmOptions.lstProgsClick(Sender: TObject);
begin
  btnToolMoveUp.Enabled   := lstProgs.SelCount = 1;
  btnToolMoveDown.Enabled := lstProgs.SelCount = 1;
  btnToolRemove.Enabled   := lstProgs.SelCount > 0;
  btnToolEdit.Enabled     := lstProgs.SelCount = 1;
  itmEdit.Enabled         := lstProgs.SelCount = 1;
  itmRemove.Enabled       := lstProgs.SelCount > 0;
  itmMoveUp.Enabled       := lstProgs.SelCount = 1;
  itmMoveDown.Enabled     := lstProgs.SelCount = 1;
  itmAlphaSort.Enabled    := lstProgs.Items.Count > 0;
end;

procedure TfrmOptions.btnToolRemoveClick(Sender: TObject);
var n : integer;
begin
  If lstProgs.SelCount > 0 then
  begin
    For n := lstProgs.Items.Count -1 downto 0 do
      If lstProgs.Items[n].Selected then lstProgs.Items.Delete(n);
  end;
  lstProgsClick(lstProgs);
end;

procedure TfrmOptions.LoadTools(scheme : string);
var li    : tListItem;
    tools : tIniFile;
    num   : integer;
    n     : integer;
    app   : string;
    dir   : string;
    parms : string;
begin
  lstProgs.Items.Clear;
  tools := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  try
    num := tools.ReadInteger(scheme, 'Number', 0);
    For n := 1 to num do
    begin
      li := lstProgs.Items.Add;
      li.Caption := tools.ReadString(scheme, 'Desc' + inttostr(n), 'Tool ' + inttostr(n));
      app        := CStringToString(tools.ReadString(scheme, 'App' + inttostr(n), 'error'));
      dir        := CStringToString(tools.ReadString(scheme, 'Dir' + inttostr(n), ''));
      parms      := CStringToString(tools.ReadString(scheme, 'Par' + inttostr(n), ''));
      li.SubItems.Add(app + ' ' + parms);
      li.SubItems.Add(app);
      li.SubItems.Add(dir);
      li.SubItems.Add(parms);
      li.SubItems.Add(BoolToStr(tools.ReadBool(scheme, 'Ask' + inttostr(n), False)));
      li.SubItems.Add(BoolToStr(tools.ReadBool(scheme, 'Cap' + inttostr(n), True)));
      li.SubItems.Add(tools.ReadString(scheme, 'Kyb' + IntToStr(n), ''));
    end;
  finally
    tools.Free;
  end;
end;

procedure TfrmOptions.SaveTools(Scheme : String);
var
  tools : tIniFile;
  n     : integer;
begin
  tools := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  try
    tools.WriteInteger(scheme, 'Number', lstProgs.Items.Count);
    For n := 0 to lstProgs.Items.Count - 1  do
    begin
      tools.WriteString(scheme, 'Desc' + inttostr(n+1), lstProgs.Items[n].Caption);
      tools.WriteString(scheme, 'App' + inttostr(n+1), StringToCString(lstProgs.Items[n].SubItems[1]));
      tools.WriteString(scheme, 'Dir' + inttostr(n+1), StringToCString(lstProgs.Items[n].SubItems[2]));
      tools.WriteString(scheme, 'Par' + inttostr(n+1), StringToCString(lstProgs.Items[n].SubItems[3]));
      tools.WriteBool(scheme, 'Ask' + inttostr(n+1), strtobool(lstProgs.Items[n].SubItems[4]));
      tools.WriteBool(scheme, 'Cap' + inttostr(n+1), strtobool(lstProgs.Items[n].SubItems[5]));
      tools.WriteString(Scheme, 'Kyb' + IntToStr(n+1), lstProgs.Items[n].SubItems[6]);
    end;
  finally
    tools.free;
  end;
end;

procedure TfrmOptions.cmbSchemesChange(Sender: TObject);
begin
  btnToolAdd.Enabled := cmbSchemes.ItemIndex <> 0;
  If CurrScheme <> '' then SaveTools(CurrScheme);
  If cmbSchemes.ItemIndex > 1 then
  begin
    LoadTools(cmbSchemes.Text);
    CurrScheme := cmbSchemes.Text;
  end else
  If cmbSchemes.ItemIndex = 1 then
  begin
    LoadTools('Menu');
    CurrScheme := 'Menu';
  end else
  begin
    CurrScheme := '';
    lstProgs.Items.Clear;
  end;
end;

procedure TfrmOptions.btnToolAddClick(Sender: TObject);
var f : TfrmNewToolLink;
    n : tListItem;
begin
  f := TfrmNewToolLink.Create(Self);
  Try
    f.chkCaptureOutput.Checked := True;
    If f.Execute(Top, Left, Height, Width, True) then
    begin
      n := lstProgs.Items.Add;
      n.Caption := f.txtCaption.Text;
      n.SubItems.Add(f.txtCommand.Text + ' ' + f.txtParameters.Text);
      n.SubItems.Add(f.txtCommand.Text);
      n.SubItems.Add(f.txtDir.Text);
      n.SubItems.Add(f.txtParameters.Text);
      n.SubItems.Add(BoolToStr(f.chkAskParameters.Checked));
      n.SubItems.Add(BoolToStr(f.chkCaptureOutput.Checked));
      n.SubItems.Add(f.txtShortcut.Text);
    end;
  Finally
    f.Free;
  end;
end;

procedure TfrmOptions.btnToolEditClick(Sender: TObject);
var f : TfrmNewToolLink;
begin
  f := TfrmNewToolLink.Create(Self);
  Try
    f.Caption := lstProgs.Selected.Caption;
    f.txtCaption.Text := lstProgs.Selected.Caption;
    f.txtCommand.Text := lstProgs.Selected.SubItems[1];
    f.txtDir.Text := lstProgs.Selected.SubItems[2];
    f.txtParameters.Text := lstProgs.Selected.SubItems[3];
    f.chkAskParameters.Checked := StrToBool(lstProgs.Selected.SubItems[4]);
    f.chkCaptureOutput.Checked := StrToBool(lstProgs.Selected.SubItems[5]);
    f.txtShortcut.Text := lstProgs.Selected.SubItems[6];
    If f.Execute(Top, Left, Height, Width, True) then
    begin
      lstProgs.Selected.SubItems.Clear;
      lstProgs.Selected.Caption := f.txtCaption.Text;;
      lstProgs.Selected.SubItems.Add(f.txtCommand.Text + ' ' + f.txtParameters.Text);
      lstProgs.Selected.SubItems.Add(f.txtCommand.Text);
      lstProgs.Selected.SubItems.Add(f.txtDir.Text);
      lstProgs.Selected.SubItems.Add(f.txtParameters.Text);
      lstProgs.Selected.SubItems.Add(BoolToStr(f.chkAskParameters.Checked));
      lstProgs.Selected.SubItems.Add(BoolToStr(f.chkCaptureOutput.Checked));
      lstProgs.Selected.SubItems.Add(f.txtShortcut.Text);
    end;
  Finally
    f.free;
  end;
end;

procedure TfrmOptions.treOptionsChange(Sender: TObject; Node: TTreeNode);
begin
   If not assigned(treOptions.Selected) then exit;
   If treOptions.Selected.Level = 0 then
   begin
      if (uppercase(treOptions.Selected.Text) = 'GENERAL')  or
         (uppercase(treOptions.Selected.Text) = 'TOOLS')    or
         (UpperCase(treOptions.Selected.Text) = 'FILES')    or
         (UpperCase(treOptions.Selected.Text) = 'INTERNET')
      then
         notOptions.ActivePage := treOptions.Selected.Text;
   end else
   begin
      notOptions.ActivePage := treOptions.Selected.Text;
   end;
end;

procedure TfrmOptions.lstBrowsersCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  With Sender.Canvas.Font do
  begin
    if Item.Index = 0 then Style := Style + [fsBold]
                      else Style := Style - [fsBold];
  end;
end;

procedure TfrmOptions.lstBrowsersClick(Sender: TObject);
begin
  btnBrowserMoveUp.Enabled := lstBrowsers.SelCount = 1;
  btnBrowserMoveDown.Enabled := lstBrowsers.SelCount = 1;
  btnBrowserConfigure.Enabled := (lstBrowsers.SelCount = 1) and (not lstBrowsers.Selected.Checked);
  btnBrowserRemove.Enabled := (lstBrowsers.SelCount = 1) and (not lstBrowsers.Selected.Checked);
end;

procedure TfrmOptions.btnBrowserAddClick(Sender: TObject);
var frmNT : tfrmNewToolLink;
    li    : TListItem;
begin
  frmNT := TfrmNewToolLink.Create(Self);
  Try
    frmNT.Caption := 'New Browser';
    if frmNT.ExecuteBrowser(Top, Left, Height, Width) then
    begin
      li := lstBrowsers.Items.Add;
      li.ImageIndex := 12;
      li.Caption := frmNT.txtCaption.Text;
      li.SubItems.Add(frmNT.txtCommand.Text);
      li.SubItems.Add(frmNT.txtDir.Text);
      if Pos('%f', frmNT.txtParameters.Text) = 0 then
        frmNT.txtParameters.Text := '%f ' + frmNT.txtParameters.Text;
      li.SubItems.Add(frmNT.txtParameters.Text);
      li.SubItems.Add(frmNT.txtShortcut.Text);
      li.Checked := False;
    end;
  Finally
    FreeAndNil(frmNT);
  end;
end;

procedure TfrmOptions.btnBrowserConfigureClick(Sender: TObject);
var frmNT : TfrmNewToolLink;
    li    : TListItem;
begin
  if lstBrowsers.SelCount <> 1 then Exit;
  frmNT := TfrmNewToolLink.Create(Self);
  li := TListItem(lstBrowsers.Selected);
  Try
    frmNT.txtCaption.Text := li.Caption;
    frmNT.txtCommand.Text := li.SubItems[0];
    frmNT.txtDir.Text := li.SubItems[1];
    frmNT.txtParameters.Text := li.SubItems[2];
    frmNT.txtShortcut.Text := li.SubItems[3];
    if frmNT.ExecuteBrowser(Top, Left, Height, Width) then
    begin
      li.Caption := frmNT.txtCaption.Text;
      li.SubItems.Clear;
      li.SubItems.Add(frmNT.txtCommand.Text);
      li.SubItems.Add(frmNT.txtDir.Text);
      if Pos('%f', frmNT.txtParameters.Text) = 0 then
        frmNT.txtParameters.Text := '%f ' + frmNT.txtParameters.Text;
      li.SubItems.Add(frmNT.txtParameters.Text);
      li.SubItems.Add(frmNT.txtShortcut.Text);
      li.Checked := False;
    end;
  Finally
    FreeAndNil(frmNT);
  end;
end;

procedure TfrmOptions.btnBrowserRemoveClick(Sender: TObject);
begin
  if lstBrowsers.SelCount = 1 then
  begin
    if not lstBrowsers.Selected.Checked then
      lstBrowsers.Selected.Delete;
  end;
end;

procedure TfrmOptions.lstBrowsersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  lstBrowsersClick(Sender);
end;

procedure TfrmOptions.chkHighlightActiveClick(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    0 : cmbHighlightActiveColor.Enabled := chkHighlightActive.Checked;
    1 : cmbUnderlineColor.Enabled := chkHighlightChange.Checked;
  end;
end;

procedure TfrmOptions.cmbHighlightActiveColorClick(Sender: TObject);
begin
  case tColorPickerButton(Sender).Tag of
    0 : cmbHighlightActiveColor.DroppedDown := True;
    1 : cmbUnderlineColor.DroppedDown := True;
  end;
end;

procedure TfrmOptions.treOptionsExpanded(Sender: TObject; Node: TTreeNode);
begin
  treOptions.Repaint;
end;

procedure TfrmOptions.btnCreateBatClick(Sender: TObject);
var s : string;
begin
  s := BatchFile(ExtractFilePath(ParamStr(0)));
  if s <> '' then
    MessageDlg('Batch file successfully created as:'+#13+#10+s, mtInformation, [mbOK], 0)
  else
    MessageDlg('Could not create batch file.', mtError, [mbOK], 0);
end;

procedure TfrmOptions.btnIconChooseClick(Sender: TObject);
var IconFile   : string;
    IconIndex  : Integer;
    NewSec     : string;
    Ext        : string;
    OldVal     : string;
    reguninst  : TIniFile;
    regentries : TIniFile;
    n          : Integer;
    desc       : string;
    ico        : TIcon;
const sec = 'PN_';
      ic = '%s,%d';
begin
  IconFile := ParamStr(0);
  Ext := lstAssoc.Selected.Caption;
  IconFile := GetRegIconFileName(Ext, IconIndex);
  if ChooseIcon(Application.Handle, IconFile, IconIndex) then
  begin
    // The user has selected a (possibly) new icon for this file.
    // We need to create a new registry section for this file-type alone,
    // and point the extension at it.
    NewSec := sec + Copy(Ext, 2, Length(Ext));
    Try
      CreateShellSection(NewSec, Format(ic, [IconFile, IconIndex]));
    Except
      MessageDlg('There was a problem registering this icon, please contact'+#13+#10+'pn-bugs@alternate.demon.co.uk with error code ChIc001.', mtError, [mbOK], 0);
      Exit;
    end;
    regentries := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'regentries.ini');
    Try
      desc := regentries.ReadString(Ext, 'Description', '');
    Finally
      regentries.Free;
    end;
    Try
      OldVal := CreateAssociation(Ext, desc, NewSec);
    Except
      MessageDlg('There was a problem registering this icon, please contact'+#13+#10+'pn-bugs@alternate.demon.co.uk with error code ChIc002.', mtError, [mbOK], 0);
      Exit;
    end;
    reguninst := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'reguninst.ini');
    Try
      n := reguninst.ReadInteger('Uninstall', 'Number', 0);
      Inc(n, 1);
      reguninst.WriteString('Uninstall', IntToStr(n), NewSec);
      reguninst.WriteInteger('Uninstall', 'Number', n);
      reguninst.WriteString(NewSec, 'Registry', NewSec);
      reguninst.WriteString(NewSec, 'FileType', Ext);
      reguninst.WriteString(NewSec, 'Original', OldVal);
    Finally
      reguninst.Free;
    end;
    Try
      ico := tIcon.Create;
      ico.Handle := GetRegIcon(lstAssoc.Selected.Caption, True);
      lstAssoc.Selected.ImageIndex := ilsAssoc.addicon(ico);
      ico.Handle := GetRegIcon(lstAssoc.Selected.Caption, False);
      imgIcon.Picture.Icon.Assign(ico);
      ico.Free;
    Except
      MessageDlg('There was a problem displaying the icon for this filetype, please contact'+#13#10+'pn-bugs@alternate.demon.co.uk with error code ChIc003.', mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmOptions.lstProgsDblClick(Sender: TObject);
begin
  if lstProgs.SelCount > 0 then
  begin
    btnToolEdit.Click;
  end;
end;

procedure TfrmOptions.lstActionCatsClick(Sender: TObject);
var
  i : Integer;
  cat : string;
  li  : TListItem;
begin
  lstActionComms.Items.Clear;
  Actions.Clear;

  if Assigned(lstActionCats.Selected) then
  begin
    with frmMain do
    begin
      cat := lstActionCats.Selected.Caption;
      for i := 0 to ActionList.ActionCount - 1 do
      begin
        if ActionList.Actions[i].Category = cat then
        begin
          li := lstActionComms.Items.Add;
          li.Caption := StringReplace(TAction(ActionList.Actions[i]).Caption, '&', '', [rfReplaceAll]);
          li.ImageIndex := TAction(ActionList.Actions[i]).ImageIndex;
          Actions.AddObject(ActionList.Actions[i].Name, ActionList.Actions[i]);
        end;
      end;
    end;
  end;

  lstActionCommsClick(Sender);
end;

procedure TfrmOptions.lstActionCommsClick(Sender: TObject);
var
  i : Integer;
begin
  if Assigned(lstActionComms.Selected) then
  begin
    btnAssign.Enabled := True;
    txtHotKey.Enabled := True;
    i := lstActionComms.Selected.Index;

    txtHotKey.HotKey := TAction(Actions.Objects[i]).ShortCut;
    lblActionDesc.Caption := GetLongHint(TAction(Actions.Objects[i]).Hint);
    if lblActionDesc.Caption = '' then lblActionDesc.Caption := '(none)';
  end else
  begin
    btnAssign.Enabled := False;
    txtHotkey.Enabled := False;
    lblActionDesc.Caption := '(none)';
    txtHotkey.Text := '';
  end;
end;

procedure TfrmOptions.btnAssignClick(Sender: TObject);
begin
  if Assigned(lstActionComms.Selected) then
  begin
    TAction(Actions.Objects[lstActionComms.Selected.Index]).ShortCut := txtHotKey.HotKey;
  end;
end;

procedure TfrmOptions.btnQLAddClick(Sender: TObject);
begin
  CreateQuickLaunchLink(Handle);
end;

end.

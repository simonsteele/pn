{***************************************************************
 *                    
 * Unit Name: main
 * Purpose  : SMDI MDI Parent
 * Author   : Simon Steele
 * History  : 18/04/1999 Start of SMDI Unit.
 *            19/04/1999 Continuation of conversion from old code
 *                       to 100% re-located code. Completed transferring
 *                       code - now to write extra code for tabs and
 *                       other necessary changes.
 *            24/04/1999 Fixed all of the Tabs and MDI related code. Also
 *                       added some new options due to the MDI system, and
 *                       changed the way the WordWrap toggle worked for
 *                       global and local toggle-ability.
 *            25/04/1999 A few minor fixes in relation to the Word-Wrap
 *                       toggle system.
 *            02/05/1999 Fixed bug preventing launching of multiple files
 *                       from explorer. Added basis for Internet Error
 *                       reporting engine...
 *            03/05/1999 Fixed editor context menu 'Close' item.
 *            08/05/1999 Added the loading and saving of file types from
 *                       the dialog.
 *            29/05/1999 Fixed the print dialog so that changes are
 *                       persistent, and so that setting the checkboxes
 *                       operates correctly. The font prints fine. Further
 *                       work done on Hex Editor integration with
 *                       correct button state updates and proper file
 *                       opening and saving. Printing and Print Previewing
 *                       hex now works.
 *            30/05/1999 Hex find added to the editor form. Also added Hex
 *                       viewing options, although they are not persistent.
 *                       customized many commands to allow Hex alternatives.
 *            31/05/1999 More dynamic menus related to the hex editor, and
 *                       fixed the Hex paste code. For some reason, the Open
 *                       files on start function died. After ages, found the
 *                       only way was to get a timer to open the files to make
 *                       sure that the MDI form had been created. Will ask in
 *                       a delphi forum to see if there is another way. Also
 *                       added Paste From File, Toolbar saving code, Save All,
 *                       some new keyboard shortcuts and a help file.
 *            03/06/1999 Fixed a bug in the last fix which caused dragging files
 *                       onto the icon of PN not to open the files properly...
 *            05/06/1999 Fixed the various file opening timing and drag drop bugs...
 *            06/06/1999 Continuing work on the filter system where any file type
 *                       can be recognised. Added procedures to set the filters of
 *                       the file dialogs to use all of the set types...
 *            13/06/1999 Started to implement Plugins now. These will be DLLs which
 *                       are dynamically loaded and must have certain procedure to be
 *                       recognised.
 *            14/06/1999 First plugin written, and begin testing with PN. A few problems
 *                       experienced with AVs and the such, but the biggest problem is
 *                       getting the string title to pass back and forth correctly. I hate
 *                       pChars.
 *            15/06/1999 Fixed the string thing with the plugins, and it now works ok. I needed
 *                       to pass a reference to the PChar, not just the PChar.
 *            18/06/1999 Finished implementing the Configure button for the Plugins
 *                       on the options form. Now time to start implementing the about
 *                       interface. I think we should provide a form in PN which allows
 *                       plugins not to need a special form for about... However, we
 *                       wont make this mandatory, as this would restrict our developers.
 *            01/07/1999 Moved the File types configuration tab on the options dialog,
 *                       and caused the list of associated types to be sorted. Maybe
 *                       change this to a drag <> list. Also added the Plain text parser
 *                       so that it can be configured. Added persistent visibility to the
 *                       toolbars, so that if you don't want toolbars, you don't 'ave to
 *                       'ave them... Added the CSS Parser...
 *            22/07/1999 Re-began work on plugin implementation after nice holiday :) Started
 *                       the plugin interface dll, and began to implement the interface. The
 *                       actual procedures called by the interface are housed in the plgiface
 *                       unit and are called by simple callbacks set-up on program load. This
 *                       all seems very quick on testing. Performed a simple test to prove that
 *                       the procedures were being called correctly. All ok. Next
 *                       is more work on the interface DLL. Then the test of a plugin, calling
 *                       the interface DLL.
 *            24/07/1999 Added more procedures and functions for the plugin interface to call,
 *                       and linked them in. Performed the first test of the plugin interface
 *                       system, and all works ok.
 *            25/07/1999 Mapped more procedures and functions to the plugin interface dll, and
 *                       tested with the test plugin - all seems ok at present. Updated the
 *                       CheckPlugins procedure to correctly call execute only if specified by
 *                       the plugin.
 *            26/07/1999 More procedures for the plugin interface.
 *            17/08/1999 Added the MRU Bookmarks Feature from bkmrkmru.pas.
 *            22/08/1999 Added the code to allow Plugins to have menuitems. Checked,
 *                       and it runs fine, with Workspace adding two menu Items to
 *                       the tools menu. Grooovy Baby!!!
 *            24/08/1999 Fixed the restore/maximise bug with MDI clients...
 *            27/08/1999 Added the Close and CloseAll plugin interfaces. Now Workspace
 *                       can open all of its files, and restore a workspace. Cool.
 *            08/09/1999 Started to add the "Find in Files" functionality starting
 *                       with the filefind unit.
 ****************************************************************}


{$DEFINE plugin}
{$DEFINE bookmark}

unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, ImgList, Menus, ActnList, WebFileInfo,
  MRUFList, editor, SyntaxEd, SynParse, Registry, ShellAPI, StdActns,
  pntypes, neterror, Logwiz, pndefs, SmtpProt, EchoSMTP, DistributorSMTP,
  HexEDitor, plgiface, TB97, TB97Tlwn, bkmrkmru, Buttons, filefind;

type
  TfrmMain = class(TForm)
    ActionList: TActionList;
    actOpen: TAction;
    actSave: TAction;
    actClose: TAction;
    actViewHex: TAction;
    actWebUpdate: TAction;
    actSysEdit: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Pascal2: TMenuItem;
    C2: TMenuItem;
    Java2: TMenuItem;
    HTML2: TMenuItem;
    SQL2: TMenuItem;
    Perl1: TMenuItem;
    INI3: TMenuItem;
    TExt2: TMenuItem;
    Open1: TMenuItem;
    N9: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    SaveasHTML1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    PrinterSetup1: TMenuItem;
    N5: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    Selectall1: TMenuItem;
    N3: TMenuItem;
    Wordwrap1: TMenuItem;
    CharacterCase1: TMenuItem;
    Uppercase1: TMenuItem;
    Lowercase1: TMenuItem;
    N7: TMenuItem;
    Properties1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Replace1: TMenuItem;
    Useregularexpressions1: TMenuItem;
    mnuView: TMenuItem;
    itmBookmark: TMenuItem;
    Bookmark01: TMenuItem;
    Bookmark11: TMenuItem;
    Bookmark21: TMenuItem;
    Bookmark31: TMenuItem;
    Bookmark41: TMenuItem;
    Bookmark51: TMenuItem;
    Bookmark52: TMenuItem;
    Bookmark71: TMenuItem;
    Bookmark81: TMenuItem;
    Bookmark91: TMenuItem;
    itmBookmark1: TMenuItem;
    Bookmark02: TMenuItem;
    Bookmark12: TMenuItem;
    Bookmark22: TMenuItem;
    Bookmark32: TMenuItem;
    Bookmark42: TMenuItem;
    Bookmark53: TMenuItem;
    Bookmark61: TMenuItem;
    Bookmark72: TMenuItem;
    Bookmark82: TMenuItem;
    Bookmark92: TMenuItem;
    itmNoneSet: TMenuItem;
    sepNor: TMenuItem;
    itmHighlight: TMenuItem;
    Pascal1: TMenuItem;
    C1: TMenuItem;
    Java1: TMenuItem;
    HTML1: TMenuItem;
    SQL1: TMenuItem;
    Perl3: TMenuItem;
    INI2: TMenuItem;
    Text1: TMenuItem;
    N12: TMenuItem;
    PreviewHTML1: TMenuItem;
    itmHexView: TMenuItem;
    Tools1: TMenuItem;
    HTMLTags1: TMenuItem;
    SelectTag1: TMenuItem;
    SelectEnclosingBlock1: TMenuItem;
    N10: TMenuItem;
    itmWebUpdate: TMenuItem;
    SysEdit1: TMenuItem;
    N13: TMenuItem;
    Preferences1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ilsDisabled: TImageList;
    ilsHot: TImageList;
    cbrMain: TControlBar;
    tbrMain: TToolBar;
    sbNew: TToolButton;
    OpenBtn: TToolButton;
    SaveBtn: TToolButton;
    sbClose: TToolButton;
    tbrEdit: TToolBar;
    CutBtn: TToolButton;
    CopyBtn: TToolButton;
    PasteBtn: TToolButton;
    ToolButton9: TToolButton;
    sbFind: TToolButton;
    ToolButton12: TToolButton;
    sbIndent: TToolButton;
    sbUnindent: TToolButton;
    ToolButton17: TToolButton;
    sbBookmark: TToolButton;
    sbSetBookmark: TToolButton;
    ToolButton20: TToolButton;
    sbWeb: TToolButton;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog1: TSaveDialog;
    MRUFileList1: TMRUFileList;
    PrinterSetupDialog1: TPrinterSetupDialog;
    wfInfo: TWebFileInfo;
    Timer1: TTimer;
    FontDialog1: TFontDialog;
    actNew: TAction;
    pmSyntaxMemo: TPopupMenu;
    Close3: TMenuItem;
    N4: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    pmTabControl: TPopupMenu;
    Close2: TMenuItem;
    N11: TMenuItem;
    itmTop: TMenuItem;
    itmBottom: TMenuItem;
    popBookmark: TPopupMenu;
    Bookmark03: TMenuItem;
    Bookmark13: TMenuItem;
    Bookmark23: TMenuItem;
    Bookmark33: TMenuItem;
    Bookmark43: TMenuItem;
    Bookmark54: TMenuItem;
    Bookmark62: TMenuItem;
    Bookmark73: TMenuItem;
    Bookmark83: TMenuItem;
    Bookmark93: TMenuItem;
    popSetBookmark: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    popNew: TPopupMenu;
    Pascal3: TMenuItem;
    C3: TMenuItem;
    Javascript1: TMenuItem;
    HTML3: TMenuItem;
    SQL3: TMenuItem;
    Perl2: TMenuItem;
    INI1: TMenuItem;
    Text3: TMenuItem;
    parPerl: TSyntaxMemoParser;
    parINI: TSyntaxMemoParser;
    SyntaxMemoParser1: TSyntaxMemoParser;
    SyntaxMemoParser2: TSyntaxMemoParser;
    SyntaxMemoParser3: TSyntaxMemoParser;
    SyntaxMemoParser4: TSyntaxMemoParser;
    SyntaxMemoParser5: TSyntaxMemoParser;
    panBack: TPanel;
    tabs: TTabControl;
    actTabs: TAction;
    actSaveAs: TAction;
    actExit: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actSelectAll: TAction;
    actDelete: TAction;
    actUndo: TAction;
    actPrint: TAction;
    actSaveAsHTML: TAction;
    actFind: TAction;
    actRegExp: TAction;
    actFindNext: TAction;
    actReplace: TAction;
    actProperties: TAction;
    actPrintSetup: TAction;
    actIndent: TAction;
    actUnindent: TAction;
    actAbout: TAction;
    actRedo: TAction;
    actWordWrap: TAction;
    actEditHTML: TAction;
    actUppercase: TAction;
    actLowercase: TAction;
    actSelectTag: TAction;
    actSelectEnclosing: TAction;
    actFont: TAction;
    actOptions: TAction;
    actPreviewHTML: TAction;
    Window1: TMenuItem;
    Cascade1: TMenuItem;
    Tile1: TMenuItem;
    actTileHorz: TWindowTileHorizontal;
    actTileVert: TWindowTileVertical;
    actCascade: TWindowCascade;
    actMinimizeAll: TWindowMinimizeAll;
    TileVertically1: TMenuItem;
    MinimizeAll1: TMenuItem;
    itmTabs: TMenuItem;
    sepNor1: TMenuItem;
    logMain: tLogWiz;
    dlgSaveError: TSaveDialog;
    smtp: TDistributorSMTP;
    actPrintPreview: TAction;
    itmPrintPreview: TMenuItem;
    actContents: TAction;
    Contents1: TMenuItem;
    N14: TMenuItem;
    actHexGoto: TAction;
    sepHex: TMenuItem;
    Goto1: TMenuItem;
    actJumpAmount: TAction;
    itmJump: TMenuItem;
    actJumpAmount1: TMenuItem;
    N15: TMenuItem;
    actJumpForwards: TAction;
    actJumpBackwards: TAction;
    JumpBackward1: TMenuItem;
    JumpForward1: TMenuItem;
    sepHex2: TMenuItem;
    itmLineSize: TMenuItem;
    act16BytesPerLine: TAction;
    act32BytesPerLine: TAction;
    act64BytesPerLine: TAction;
    N16BytesPerLine1: TMenuItem;
    N32BytesPerLine1: TMenuItem;
    N64BytesPerLine1: TMenuItem;
    act1BytesPerColumn: TAction;
    act2BytesPerColumn: TAction;
    act4BytesPerColumn: TAction;
    itmColumns: TMenuItem;
    N16BytesPerLine2: TMenuItem;
    N2BytesPerColumn1: TMenuItem;
    N4BytesPerColumn1: TMenuItem;
    actCaretFull: TAction;
    actCaretLeft: TAction;
    actCaretBottom: TAction;
    itmCaretStyle: TMenuItem;
    FullBlock1: TMenuItem;
    LeftLine1: TMenuItem;
    BottomLine1: TMenuItem;
    actHexOffset: TAction;
    actDecOffset: TAction;
    actNoOffset: TAction;
    itmOffset: TMenuItem;
    Hex1: TMenuItem;
    Dec1: TMenuItem;
    None1: TMenuItem;
    sepHex3: TMenuItem;
    itmAsciiToAnsi: TMenuItem;
    actAsciiToAnsi: TAction;
    actGrid: TAction;
    Grid1: TMenuItem;
    actShowMarkers: TAction;
    ShowMarkers1: TMenuItem;
    Timer2: TTimer;
    actSaveAll: TAction;
    SaveAll1: TMenuItem;
    actPasteFromFile: TAction;
    dlgPasteFrom: TOpenDialog;
    PasteFromFile1: TMenuItem;
    parPlain: TSyntaxMemoParser;
    popToolbar: TPopupMenu;
    pitFile: TMenuItem;
    pitEdit: TMenuItem;
    actTbrMain: TAction;
    actTbrEdit: TAction;
    Toolbars1: TMenuItem;
    File2: TMenuItem;
    Edit2: TMenuItem;
    parCSS: TSyntaxMemoParser;
    CSS1: TMenuItem;
    itmBookmarks: TMenuItem;
    parVB: TSyntaxMemoParser;
    VisualBasic1: TMenuItem;
    CSS2: TMenuItem;
    VisualBasic2: TMenuItem;
    CSS3: TMenuItem;
    VisualBasic3: TMenuItem;
    FloatBookmarks: TToolWindow97;
    panBookmarks: TPanel;
    lstBookmarks: TListView;
    Panel2: TPanel;
    btnAddBM: TSpeedButton;
    btnRemoveBM: TSpeedButton;
    dokRight: TDock97;
    dokLeft: TDock97;
    dokBottom: TDock97;
    FloatResults: TToolWindow97;
    lstResults: TListView;
    itmResults: TMenuItem;
    N6: TMenuItem;
    itmFileFind: TMenuItem;
    procedure actNewExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure tabsChange(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure itmHighlightClick(Sender: TObject);
    procedure SetSyntax(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actRegExpExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure pmTabControlPopup(Sender: TObject);
    procedure MRUFileList1MRUItemClick(Sender: TObject; AFilename: String);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintSetupExecute(Sender: TObject);
    procedure actIndentExecute(Sender: TObject);
    procedure actUnindentExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure NewFromMenu(Sender : TObject);
    procedure SetBookmark(Sender: TObject);
    procedure itmBookmark1Click(Sender: TObject);
    procedure GotoBookmark(Sender: TObject);
    procedure popBookmarkPopup(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveAsHTMLExecute(Sender: TObject);
    procedure actWordWrapExecute(Sender: TObject);
    procedure actEditHTMLExecute(Sender: TObject);
    procedure actUppercaseExecute(Sender: TObject);
    procedure actLowercaseExecute(Sender: TObject);
    procedure actSelectTagExecute(Sender: TObject);
    procedure actSelectEnclosingExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actPreviewHTMLExecute(Sender: TObject);
    procedure itmBookmarkClick(Sender: TObject);
    procedure popSetBookmarkPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure itmTopClick(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actViewHexExecute(Sender: TObject);
    procedure actWebUpdateExecute(Sender: TObject);
    procedure actSysEditExecute(Sender: TObject);
    function parINICustomParse(Sender: TObject; ParseID: Integer;
      IStream: TEdStream; var kLength, kValue: Integer): Boolean;
    procedure actTabsExecute(Sender: TObject);
    procedure smtpGetData(Sender: TObject; LineNum: Integer;
      MsgLine: PChar; MaxLen: Integer; var More: Boolean);
    procedure actPrintPreviewExecute(Sender: TObject);
    procedure actContentsExecute(Sender: TObject);
    procedure actHexGotoExecute(Sender: TObject);
    procedure actJumpAmountExecute(Sender: TObject);
    procedure itmJumpClick(Sender: TObject);
    procedure actJumpForwardsExecute(Sender: TObject);
    procedure actJumpBackwardsExecute(Sender: TObject);
    procedure act16BytesPerLineExecute(Sender: TObject);
    procedure itmLineSizeClick(Sender: TObject);
    procedure act1BytesPerColumnExecute(Sender: TObject);
    procedure actCaretFullExecute(Sender: TObject);
    procedure itmCaretStyleClick(Sender: TObject);
    procedure itmColumnsClick(Sender: TObject);
    procedure actHexOffsetExecute(Sender: TObject);
    procedure itmOffsetClick(Sender: TObject);
    procedure actAsciiToAnsiExecute(Sender: TObject);
    procedure actGridExecute(Sender: TObject);
    procedure mnuViewClick(Sender: TObject);
    procedure actShowMarkersExecute(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actPasteFromFileExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actTbrMainExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lstBookmarksResize(Sender: TObject);
    procedure actBMAddExecute(Sender: TObject);
    procedure actBMShowHideExecute(Sender: TObject);
    procedure btnBMRemoveClick(Sender: TObject);
    procedure lstBookmarksDblClick(Sender: TObject);
    procedure itmFileFindClick(Sender: TObject);
  private
    { Private declarations }
    FindInFiles      : tFindInFiles;
    Flag             : Integer;
    First            : Boolean;
    FirstMore        : Boolean;
    FLastFindText    : string;
    FLastFindOptions : longint;
    SetFiles         : Boolean;
    MsgBody          : TStringList;
    {$IFDEF plugin}
    PluginInit       : tPluginInit;
    PlgMsgNum        : tPlgMsgNum;
    PlgStrResult     : tStrResult;
    PluginQuitList   : TStringList;
    {$ENDIF}
    procedure Setup;
    procedure ToolLaunch(Sender: TObject);
    procedure NewType(Ext : String); overload;
    procedure NewType(Ext : Integer); overload;
    function GetFilterIndex(Ext: String): Integer;
    procedure Setpreview;
    procedure HandleHint(Sender: TObject);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure DoSetFilesFirst;
    procedure SelectHTMLTag(SelectEnclosingBlock: Boolean);
    procedure ChangeTabPos(Pos: integer);
    procedure SendErrorReport(Error: string);
    procedure SaveErrorReport(Error: string);
    function PrepareErrorReport(Error: string): string;
    procedure HexButtons;
    procedure SaveToolbars;
    procedure LoadToolbars(Settings: tRegistry);
    function GenSaveFilter: string;
    function GenOpenFilter: string;
    procedure UpdateTypes;
    {$IFDEF plugin}
    procedure CheckPlugins;
    {$ENDIF}
  protected
    procedure COPYDATA(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  public
    { Public declarations }
    OpenOnStart    : TStringList;
    fStarted       : Boolean;
    SEFiles        : tSysEditFiles;
    MsgNum         : Integer;
    SaveAsUnix     : Boolean;
    OpenExeAsHex   : Boolean;
    OpenFilesMaxed : Boolean;
    {Internet Variables}
    InternetReport : Boolean;
    AlwaysSend     : Boolean;
    UseProxy       : Boolean;
    ProxyHost      : string;
    ProxyPort      : string;
    MailHost       : string;
    MailPort       : string;
    UserEmail      : string;
    UserNameF       : string;
    TypeName       : TStringList;
    TypeIndex      : TStringList;
    TypeFilter     : TStringList;
    BookmarkMRU    : tBookmarkMRU;
    {$IFDEF plugin}
    PluginList       : tStringList;
    PluginFileList   : tStringList;
    PluginIF         : THandle;
    PluginMenuPlgs   : tStringList;
    PluginMenuProcs  : tStringList;
    PluginMenuItems  : tList;
    procedure MenuItemClick(Sender: TObject);
    {$ENDIF}
    procedure CloseFile;
    procedure CloseAll;
    procedure OpenFile(fname, ext: String);
    procedure EnableButtons;
    procedure DefaultHandler(Var Message); override;
    procedure DropFiles(var msg: TWmDropFiles); message wm_dropfiles;
    procedure ReloadTools;
    procedure UpdateStatusBar;
    function NewFilterIndex(FileName: String): Integer;
    function GetCurrentEditor: TfrmClient;
    procedure ShowText(const Text: String);
    procedure SetEditor;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure UpdateDialogs;
    function GetFileType(Ext: string): integer;
    procedure RefreshBookmarkList;
  end;

var
  frmMain: TfrmMain;

Const
  ProgTitle  = 'Programmers Notepad (PN)';
  newtxt     = '<new>';
  newpas     = '<new pascal>';
  newc       = '<new c>';
  newjav     = '<new java>';
  newhtm     = '<new html>';
  newsql     = '<new sql>';
  newpl      = '<new perl>';
  newini     = '<new ini>';
  newcss     = '<new css>';
  newvb      = '<new vb>';
  ToolOffSet = 7;
  SEKey      = 'Software\Echo Software\PN\SysEdit';
  RootKey    = 'Software\Echo Software\PN';
  NetKey     = 'Software\Echo Software\PN\Net';
  ToolKey    = 'Software\Echo Software\PN\Toolbars';
  ToolKey2   = 'Software\Echo Software\PN\Toolbars 2';
  ErrAddr    = 'pnerror@alternate.demon.co.uk';
  ErrSubj    = 'Automatic: PN Fatal Error Report';
  psText     = 0;
  psPascal   = 1;
  psC        = 2;
  psJava     = 3;
  psHTML     = 4;
  psSQL      = 5;
  psPerl     = 6;
  psINI      = 7;
  psCSS      = 8;
  psVB       = 9;

implementation

uses Useful, Clipbrd, preview, Options, genfuncs, welcome, About, print,
     TagEdit, pff;

const
  HTMLTagLetters     = ['a'..'z', 'A'..'Z', '!'];

type
  TXYRef      = record
                  X,
                  Y  : longint;
                end;
  TSelExtents = record
                  StartPos,
                  EndPos     : TXYRef;
                end;

{$R *.DFM}

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
   NewType(0);
end;

procedure TfrmMain.DefaultHandler(var Message);
var
   Buffer : Array[0..255] of Char;
   s : String;
begin
   Inherited DefaultHandler(Message);
   With tMessage(Message) Do
   If Msg = MsgNum Then
   Begin
      If GlobalGetAtomName(lParam, Buffer, 255) = 0 Then
         StrCopy(Buffer, 'Error!');
      s := StrPas(Buffer);
      If Length(s) > 0 Then
      Begin
//          ParmOptions(Copy(s, 2, 255))
//        else
          if fStarted then
          begin
            If Copy(s, 0, 1) <> '/' Then
               OpenFile(s, ExtractFileExt(s));
          end else
          begin
             If not Assigned(OpenOnStart) Then OpenOnStart := tStringList.Create;
             OpenOnStart.Add(s);
          end;
      End;
   end;
end;

procedure TfrmMain.Setup;
var Settings : tRegIniFile;
    Registry : tRegistry;
    n,
    i1,
    i2,
    i3,
    i        : Integer;
    m        : tMenuItem;
    ofs      : Integer;
begin
   if not FileExists(ExtractFilePath(ParamStr(0)) + 'files.def') then
      CreatePNFile(ExtractFilePath(ParamStr(0)) + 'files.def', strFileTypes);
   UpdateDialogs;
   If GetSize('Echo Software', 'PN', i1, i2) Then
   Begin
      SetBounds(Left, Top, i2, i1);
   End;
   frmMain.WindowState := GetStat('Echo Software', 'PN');
   If frmMain.WindowState = wsNormal then
   Begin
      GetPosition('Echo Software', 'PN', frmMain.Height, frmMain.Width,
                  i1, i2);
      SetBounds(i2, i1, Width, Height);
   End;

   Settings := TRegIniFile.Create('Software\Echo Software\PN');
   Try
{$IFDEF plugin}
      if Assigned(PluginMenuItems) then ofs := PluginMenuItems.Count else ofs := 0;
{$ELSE}
      ofs := 0;
{$ENDIF}
      With Settings Do
      Begin
         i := ReadInteger('Tools', 'CustomNumber', 0);
         If i > 0 Then
         Begin
            m := tMenuItem.Create(MainMenu1);
            m.Caption := '-';
            m.Tag := 19;
            MainMenu1.Items[4].Insert(ToolOffset + ofs, m);
            For n := 0 to i - 1 do
            Begin
               m := tMenuItem.Create(MainMenu1);
               m.Caption := ReadString('Tools', 'Desc' + inttostr(n), 'Error');
               m.Tag := n + 20;
               m.OnClick := ToolLaunch;
               MainMenu1.Items[4].Insert(ToolOffset + ofs + n, m);
            End;
         End;
      End;
   Finally
      Settings.Free;
   End;
   Registry := tRegistry.Create;
   Try
      Registry.OpenKey(SEKey, True);
      SEFiles := [];
      If Registry.ReadBool('AutoExec') Then
         SEFiles := SEFiles + [seAutoExec];
      If Registry.ReadBool('Config') Then
         SEFiles := SEFiles + [seConfig];
      If Registry.ReadBool('WinIni') Then
         SEFiles := SEFiles + [seWinIni];
      If Registry.ReadBool('SysIni') Then
         SEFiles := SEFiles + [seSysIni];
      If Registry.ReadBool('Prot') Then
         SEFiles := SEFiles + [seProt];
   Except
      SEFiles := [seAutoExec, seConfig, seWinIni, seSysIni, seProt];
   End;
   Registry.CloseKey;
   Registry.OpenKey(RootKey, True);
   try
      actTabs.Checked := Registry.ReadBool('ShowTabs');
      actWordWrap.Checked := Registry.ReadBool('WordWrap');
      i3 := Registry.ReadInteger('TabPos');
      SaveAsUnix := Registry.ReadBool('SaveAsUnix');
      OpenFilesMaxed := Registry.ReadBool('OpenMaxed');
   except
      {Probably First Time Running}
      actWordWrap.Checked := False;
      actTabs.Checked := True;
      i3 := 1;
      SaveAsUnix := False;
      OpenFilesMaxed := False;
   end;
   try OpenExeAsHex := Registry.ReadBool('ExeAsHex'); except OpenExeAsHex := True end;
   LoadToolbars(Registry);
   try Registry.CloseKey except end;
   Registry.OpenKey(NetKey, True);
   try
      InternetReport := Registry.ReadBool('SendError');
      AlwaysSend     := Registry.ReadBool('SendWithoutAsk');
      ProxyHost      := Registry.ReadString('ProxyHost');
      ProxyPort      := Registry.ReadString('ProxyPort');
      MailHost       := Registry.ReadString('MailHost');
      MailPort       := Registry.ReadString('MailPort');
      UseProxy       := Registry.ReadBool('UseProxy');
      UserNameF       := Registry.ReadString('UserName');
      UserEmail      := Registry.ReadString('UserEmail');
   except
      {Probably First Time Running}
      AlwaysSend := False;
      InternetReport := True;
      ProxyHost := '';
      ProxyPort := '8080';
      MailHost  := '';
      MailPort  := '25';
      UseProxy := False;
      UserNameF := '';
      UserEmail := '';
   end;
   Registry.CloseKey;
   Registry.Free;
   wfInfo.CurrentVersion := GetFileInformation(application.ExeName,
                                                    'FileVersion');
   LoadSettings;
   ChangeTabPos(i3);
   {$IFDEF plugin}
   InitPluginInterface(Application.Handle);
   CheckPlugins;
   {$ENDIF}
end;

procedure TfrmMain.ReloadTools;
var m        : tMenuItem;
    Settings : tRegIniFile;
    n,
    i        : Integer;
    ofs      : integer;
Begin
{1: Delete old Items}
   For n := MainMenu1.Items[4].Count - 1 downto 0 do
   Begin
      If (MainMenu1.Items[4].Items[n].Tag > 18) and (MainMenu1.Items[4].Items[n].Tag < 80) Then
         MainMenu1.Items[4].Delete(MainMenu1.Items[4].IndexOf(MainMenu1.Items[4].Items[n]));
   End;
{2: Re-Add Items}
{$IFDEF plugin}
   if Assigned(PluginMenuItems) then ofs := PluginMenuItems.Count else ofs := 0;
{$ELSE}
   ofs := 0;
{$ENDIF}
   Settings := TRegIniFile.Create('Software\Echo Software\PN');
   Try
      With Settings Do
      Begin
         i := ReadInteger('Tools', 'CustomNumber', 0);
         If i > 0 Then
         Begin
            m := tMenuItem.Create(MainMenu1);
            m.Caption := '-';
            m.Tag := 19;
            MainMenu1.Items[4].Insert(ToolOffset + ofs, m);
            For n := 0 to i - 1 do
            Begin
               m := tMenuItem.Create(MainMenu1);
               m.Caption := ReadString('Tools', 'Desc' + inttostr(n), 'Error');
               m.Tag := n + 20;
               m.OnClick := ToolLaunch;
               MainMenu1.Items[4].Insert(ToolOffset + ofs + n, m);
            End;
         End;
      End;
   Finally
      Settings.Free;
   End;
end;

procedure TfrmMain.ToolLaunch(Sender: TObject);
var Settings : tRegIniFile;
    p,
    s        : String;
    i        : Integer;
    Current  : tfrmClient;

function ParseIn(filename, instr : string) : string;
var s1, s2, ts : string;
begin
   s1 := copy(instr, 1, pos('%f', instr) - 1);
   s2 := copy(instr, pos('%f', instr) + 2, 255);
   ts := s1 + filename + s2;
   result := ts;
end;

begin
   Settings := tRegIniFile.Create('Software\Echo Software\PN');
   Try
      s := Settings.ReadString('Tools', 'Command' + inttostr(tMenuItem(Sender).tag - 20), 'Error');
      p := Settings.ReadString('Tools', 'Parm' + inttostr(tMenuItem(Sender).tag - 20), '');
      If s <> 'Error' Then
      Begin
         i := pos('%f', p);
         If i <> 0 Then
         Begin
            current := GetCurrentEditor;
            If current = nil then p := ParseIn('', p) Else
            Begin
               If Current.FileName <> newtxt Then
               If Current.FileName <> newpas Then
               If Current.FileName <> newc Then
               If Current.FileName <> newjav Then
               If Current.FileName <> newhtm Then
               If Current.FileName <> newsql Then
               If Current.FileName <> newpl then
               if Current.FileName <> newcss then
               If Current.FileName <> newini then
               if Current.FileName <> newvb then
               p := ParseIn(current.Filename, p)
               Else p := ParseIn('', p);
            End;
         End;
         ShellExecute(Handle, PChar('open'), PChar(s), PChar(p), PChar(ExtractFilePath(ParamStr(0))), SW_SHOW);
      End;
   Finally
      Settings.Free;
   End;
end;

procedure TfrmMain.NewType(ext : String);
var Editor : tfrmClient;
    tempstr : String;
Begin
   Editor := tfrmClient.Create(Self);
   Editor.Execute('', ext);
   tempstr := Editor.FileName;
   tabs.Tabs.AddObject(ExtractFileName(tempstr), Editor);
   tabs.TabIndex := tabs.Tabs.Count - 1;
   If actTabs.Checked Then
   Begin
      panBack.Visible := True;
      tabs.Visible := True;
   end;
   EnableButtons;
   SetEditor;
   SetPreview;
end;

procedure TfrmMain.NewType(Ext : Integer);
var Editor : tfrmClient;
    tempstr : String;
Begin
   Editor := tfrmClient.Create(Self);
   Editor.Execute(ext);
   tempstr := Editor.FileName;
   tabs.Tabs.AddObject(ExtractFileName(tempstr), Editor);
   tabs.TabIndex := tabs.Tabs.Count - 1;
   If actTabs.Checked Then
   Begin
      panBack.Visible := True;
      tabs.Visible := True;
   end;
   EnableButtons;
   SetEditor;
   SetPreview;
end;

procedure TfrmMain.OpenFile(fname,ext: String);
var Editor: TfrmClient;
Begin
  if fname <> '' then
    if not FileExists(fname) then
      Begin
         MessageDlg(fname + ' does not exist, it could not be opened',mtError,[mbOK],0);
         Exit;
      End;
  If ext = '' Then If fname <> '' Then
     ext := ExtractFileExt(fname);
  Application.ProcessMessages;
  Editor := TfrmClient.Create(Self);
  Editor.Execute(fname, Ext);
  if OpenFilesMaxed then Editor.WindowState := wsMaximized;
  fname := Editor.FileName;
  tabs.Tabs.AddObject(ExtractFileName(fname), Editor);
  tabs.TabIndex := tabs.Tabs.Count - 1;
  SetEditor;
  SetPreview;
  EnableButtons;
End;

function TfrmMain.NewFilterIndex(FileName : String) : Integer;
begin
   if (FileName = newhtm) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.htm')]) else
   if (FileName = newpl)  then Result := strtoint(TypeFilter[TypeName.IndexOf('*.pl')]) else
   if (FileName = newjav) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.jav')]) else
   if (FileName = newpas) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.pas')]) else
   if (FileName = newsql) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.sql')]) else
   if (FileName = newc)   then Result := strtoint(TypeFilter[TypeName.IndexOf('*.c')]) else
   if (FileName = newtxt) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.txt')]) else
   If (FileName = newini) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.ini')]) else
   if (FileName = newcss) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.css')]) else
   if (FileName = newvb) then Result := strtoint(TypeFilter[TypeName.IndexOf('*.vb')]) else
      Result := 11;
   Result := Result + 1;
end;

function TfrmMain.GetFilterIndex(Ext: String):Integer;
begin
  if Pos('.', Ext) = 0 then Ext := '.' + Ext;
  Ext := lowercase(Ext);
  Result := TypeName.IndexOf('*' + Ext);
  if Result = -1 then Result := 0;
  Result := strtoint(TypeFilter[Result]);
  Result := Result + 1;
end;



procedure TfrmMain.actSaveAsExecute(Sender: TObject);
var FEditor: TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  if not (FEditor.FileName = newtxt) and
     not (FEditor.FileName = newhtm) and
     not (FEditor.FileName = newpl)  and
     not (FEditor.FileName = newjav) and
     not (FEditor.FileName = newpas) and
     not (FEditor.FileName = newcss) and
     not (FEditor.FileName = newsql) and
     not (FEditor.FileName = newini) and
     not (FEditor.FileName = newvb) and
     not (FEditor.FileName = newc) Then
        SaveDialog1.FilterIndex := GetFilterIndex(ExtractFileExt(FEditor.FileName))
     Else
        SaveDialog1.FilterIndex := NewFilterIndex(FEditor.FileName);
  SaveDialog1.InitialDir  := ExtractFilePath(FEditor.FileName);
  if SaveDialog1.Execute then
  Begin
     With FEditor do
     Begin
        FileName := SaveDialog1.FileName;
        SaveFile;
        Tabs.Tabs[Tabs.TabIndex] := ExtractFileName(Filename);
        FEditor.SetParser(ExtractFileExt(FEditor.Filename));
     End;
  End;
  SaveDialog1.FilterIndex:=1;
  SetEditor;
  SetPreview;
end;

function TfrmMain.GetCurrentEditor: TfrmClient;
Begin
  Result := nil;
  if Assigned(frmMain.ActiveMDIChild) Then
     Result := TfrmClient(frmMain.ActiveMDIChild);
end;

Procedure TfrmMain.EnableButtons;
begin
  actPrintPreview.Visible := False;
  actHexGoto.Visible      := False;
  itmJump.Visible         := False;
  actUndo.Caption         := '&Undo';
  sepHex.Visible          := False;
  sepHex2.Visible         := False;
  sepHex3.Visible         := False;
  actGrid.Visible         := False;
  actShowMarkers.Visible  := False;
  itmLineSize.Visible     := False;
  itmColumns.Visible      := False;
  itmCaretStyle.Visible   := False;
  itmOffset.Visible       := False;
  itmAsciiToAnsi.Visible  := False;
  itmHighlight.Visible    := True;
  itmBookmark.Visible     := True;
  itmBookmark1.Visible    := True;
  sepNor1.Visible         := True;
  sepNor.Visible          := True;
  if Tabs.Tabs.Count < 1 then
    Begin
       sbBookmark.Enabled         := False;
       sbSetBookmark.Enabled      := False;
       btnAddBM.Enabled           := False;
       btnRemoveBM.Enabled        := False;
       lstBookmarks.Enabled       := False;
       lstBookmarks.Items.Clear;
       {TActions}
       actDelete.Enabled          := False;
       actSelectAll.Enabled       := False;
       actCopy.Enabled            := false;
       actPaste.Enabled           := False;
       actCut.Enabled             := False;
       actSave.Enabled            := False;
       actClose.Enabled           := False;
       actSaveAs.Enabled          := False;
       actSaveAll.Enabled         := False;
       actPrint.Enabled           := False;
       actSaveAsHTML.Enabled      := False;
       actFind.Enabled            := False;
       actFindNext.Enabled        := False;
       actReplace.Enabled         := False;
       actViewHex.Enabled         := False;
       actIndent.Enabled          := false;
       actUnindent.Enabled        := False;
       actUndo.Enabled            := False;
       actRedo.Enabled            := False;
       actWordWrap.Enabled        := False;
       actEditHTML.Enabled        := False;
       actUpperCase.Enabled       := False;
       actLowerCase.Enabled       := False;
       actSelectTag.Enabled       := False;
       actSelectEnclosing.Enabled := False;
       actPreviewHTML.Enabled     := False;
       actPrint.Enabled           := False;
       actProperties.Enabled      := False;
       actPasteFromFile.Enabled   := False;
       Exit;
    End;
  With GetCurrentEditor do
    Begin
       if Mode = emHex then
       begin
          HexButtons;
          Exit;
       end;
       sbBookmark.Enabled         := True;
       sbSetBookmark.Enabled      := True;
       btnAddBM.Enabled           := True;
       btnRemoveBM.Enabled        := True;
       lstBookmarks.Enabled       := True;
       sbFind.Enabled             := (synMDI.Lines.Count>0);
       {TActions}
       actProperties.Enabled      := True;
       actDelete.Enabled          := True;
       actSelectAll.Enabled       := True;
       actPrint.Enabled           := True;
       actPasteFromFile.Enabled   := True;
       actViewHex.Enabled         := True;
       actCopy.Enabled            := (synMDI.SelLength>0);
       actCut.Enabled             := (synMDI.SelLength>0);
       actPaste.Enabled           := Clipboard.HasFormat(CF_TEXT);
       actClose.Enabled           := True;
       actSave.Enabled            := (Modified);
       actSaveAs.Enabled          := True;
       actSaveAll.Enabled         := True;
       actPrint.Enabled           := True;
       actSaveAsHTML.Enabled      := True;
       actFind.Enabled            := (synMDI.Lines.Count > 0);
       actFindNext.Enabled        := (synMDI.Lines.Count > 0);
       actReplace.Enabled         := (synMDI.Lines.Count > 0);
       actIndent.Enabled          := (synMDI.SelLength > 0);
       actUnindent.Enabled        := (synMDI.SelLength > 0);
       actUndo.Enabled            := True;
       actRedo.Enabled            := True;
//       actWordWrap.Checked        := synMDI.WordWrap;
       actWordWrap.Enabled        := True;
       actEditHTML.Enabled        := (Parser = psHTML);
       actUpperCase.Enabled       := (synMDI.SelLength>0);
       actLowerCase.Enabled       := (synMDI.SelLength>0);
       actSelectTag.Enabled       := (Parser = psHTML);
       actSelectEnclosing.Enabled := (Parser = psHTML);
       actPreviewHTML.Enabled     := (Parser = psHTML);
       case actTabs.Checked of
          True  : begin
                     panBack.Visible := True;
                     tabs.Visible := True;
                  end;
          False : begin
                     panBack.Visible := False;
                     tabs.Visible := False;
                  end;
       end;
    end;
end;

procedure TfrmMain.CloseFile;
var FEditor : TfrmClient;
    ws      : tWindowState;
begin
  FEditor := GetCurrentEditor;
  if FEditor <> nil then
  begin
      ws := fEditor.WindowState;
      FEditor.Close;
  end;
  Application.ProcessMessages;
  if Tabs.Tabs.Count = 0 then
  begin
     tabs.Visible := False;
     panBack.Visible := False;
  end;
  TabsChange(Self);
  if ws = wsMaximized then GetCurrentEditor.WindowState := ws;
  EnableButtons;
end;

procedure TfrmMain.tabsChange(Sender: TObject);
begin
   if tabs.tabs.Count < 1 then Exit;
//   tfrmClient(tabs.tabs.Objects[tabs.tabindex]).WindowState := wsNormal;
   tfrmClient(tabs.tabs.Objects[tabs.tabindex]).BringToFront;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
   CloseFile;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
var
   FEditor: TfrmClient;
   pct : Integer;
   pPC : PChar;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  With FEditor do
  begin
    case Mode of
       emNormal : synMDI.CutToClipboard;
       emHex    : begin
                     with HexEditor do
                     begin
                        pCT := SelCount;
                        pPC := BufferFromFile ( Min ( SelStart , SelEnd ) , pCT );
                        SetCBText ( pPC , pCT );
                        FreeMem ( pPC , pCT );
                        DeleteSelection;
                     end;
                  end;
    end;
  end;
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
var
   FEditor : TfrmClient;
   pct : Integer;
   pPC : PChar;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  With FEditor do
  begin
    case Mode of
       emNormal : synMDI.CopyToClipboard;
       emHex    : begin
                     with HexEditor do
                     begin
                        pCT := SelCount;
                        pPC := BufferFromFile ( Min ( SelStart , SelEnd ) , pCT );
                        SetCBText ( pPC , pCT );
                        FreeMem ( pPC , pCT );
                     end;
                  end;
    end;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
var FEditor : TfrmClient;
    Sr      : string;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  With FEditor do
  begin
    case Mode of
       emNormal : synMDI.PasteFromClipboard;
       emHex    : begin
                     sr := Clipboard.AsText;
                     sr := sr+#0;
                     HexEditor.ReplaceSelection ( @sr[1] , Length(sr)-1);
                  end;
    end;
  end;
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then fEditor.SynMDI.SelectAll;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if FEditor = nil then exit;
   With FEditor do
   Begin
      if synMDI.SelLength=0 then exit;
      synMDI.Perform(SEM_REPLACESEL, euDRAW, longint(nil));
   end;
end;



procedure TfrmMain.Edit1Click(Sender: TObject);
begin
   EnableButtons;
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  With FEditor do
  Begin
    case Mode of
       emNormal : begin
                     PostMessage(synMDI.Handle,WM_UNDO,0,0);
                     synMDI.RemoveLineGlyph(15, FEditor.synMDI.CaretPos.Y);
                  end;
       emHex    : begin
                     if HexEditor.CanUndo then HexEditor.Undo;
                  end;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
   n : integer;
begin
  fStarted := false;
  Flag := 0;
  FirstMore := True;
  DragAcceptFiles(frmMain.handle, true);
  TypeName  := TStringList.Create;
  TypeIndex := TStringList.Create;
  TypeFilter := TStringList.Create;
  {$IFDEF plugin}
  PluginList      := tStringList.Create;
  PluginFileList  := tStringList.Create;
  PluginQuitList  := tStringList.Create;
  PluginMenuPlgs  := tStringList.Create;
  PluginMenuProcs := tStringList.Create;
  PluginMenuItems := tList.Create;
  {$ENDIF}
  BookmarkMRU := tBookmarkMRU.Initialise;
  Application.OnHint := HandleHint;
  Application.OnException := ExceptionHandler;
  logMain.GenFileName('pn', 'elg');
  logMain.AppName := 'PN';
  logMain.AppVersion := GetFileInformation(application.ExeName,
                                                    'FileVersion');
  Setup;
  Config := TConfigInfo.Create(nil);
  First := True;
  For n := 1 to ParamCount do
      If Length(ParamStr(n)) > 0 Then
      Begin
         If not Assigned(OpenOnStart) Then OpenOnStart := tStringList.Create;
         OpenOnStart.Add(ParamStr(n));
      end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  If First Then
  Begin
     First:=False;
     If SetFiles Then
     Begin
        RegisterApp;
        DoSetFilesFirst;
     End;
     Timer2.Enabled := True;
   End;
end;

Procedure TfrmMain.SetEditor;
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   UpdateStatusBar;
   if (not assigned(FEditor)) or (tabs.tabs.Count < 1) then
   Begin
      Application.Title := ProgTitle;
      Caption := ProgTitle;
      exit;
   End;
   FEditor.BringToFront;
   Caption := ProgTitle;
   Application.Title := 'PN - ' + ExtractFileName(FEditor.FileName);
   Tabs.Repaint;
end;

procedure TfrmMain.Setpreview;
var FEditor : TfrmClient;
Begin
   FEditor := GetCurrentEditor;
   if not assigned(FEditor) then exit;
   if (frmPreview <> nil) then
      if (FEditor.synMDI.ActiveParser = psHTML) then
      Begin
         frmPreview.Filename := FEditor.FileName;
         if not frmPreview.Visible then frmPreview.Show;
      End else
      Begin
         if frmPreview.Visible then frmPreview.Hide;
      End;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CloseAll;
  SaveSettings;
  Config.Free;
  Config:=nil;
end;

procedure TfrmMain.CloseAll;
Begin
   While Tabs.Tabs.Count > 0 do
   Begin
      TfrmClient(Tabs.Tabs.Objects[0]).Free;
      tabs.Tabs.Delete(0);
   end;
end;

procedure TfrmMain.HandleHint(Sender: TObject);
Begin
   StatusBar1.Panels[3].Text := GetLongHint(Application.Hint);
end;

procedure TfrmMain.itmHighlightClick(Sender: TObject);
var FEditor : TfrmClient;
    i       : Integer;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  for i:= 0 to TMenuItem(Sender).Count - 1 do
    if TMenuItem(Sender).Items[i].Tag = FEditor.Parser then
      TMenuItem(Sender).Items[i].Checked:=True;
end;

procedure TfrmMain.SetSyntax(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor:=GetCurrentEditor;
  if Assigned(FEditor) Then FEditor.SetParser(TMenuItem(Sender).Tag);
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   begin
      case FEditor.Mode of
        emNormal : begin
                      if actRegExp.Checked then
                         FEditor.synMDI.Perform(SEM_FINDTEXT, ft_REGEXPR, 0)
                      else
                          FEditor.synMDI.Perform(SEM_FINDTEXT, 0, 0);
                      FLastFindOptions := FEditor.synMDI.LastFindOptions;
                      FLastFindText    := FEditor.synMDI.LastFindText;
                   end;
        emHex    : FEditor.HexFind;
      end;
   end;
end;

procedure TfrmMain.actRegExpExecute(Sender: TObject);
begin
   actRegExp.Checked := not actRegExp.Checked;
end;

procedure TfrmMain.actFindNextExecute(Sender: TObject);
var FEditor  : TfrmClient;
    FNextLoc : longint;
    FSel     : TChRange;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   begin
      if fEditor.Mode = emHex then
      begin
         fEDitor.HexFindAgain;
         Exit;
      end;
      if FLastFindText = '' then FLastFindText := FEditor.synMDI.LastFindText;
      if FLastFindText = '' then exit;
      FEditor.synMDI.LastFindOptions := FLastFindOptions or ft_SILENT;
      if FEditor.synMDI.FindNext(FNextLoc) then
         if not actRegExp.Checked then
         begin
            FSel.chStart := FNextLoc;
            FSel.chEnd   := FNextLoc + length(FLastFindText);
            FEditor.synMDI.Perform(SEM_SELECTION, eaSET or euDRAW, longint(@FSel));
         end else
         begin
            FSel.chStart := FNextLoc;
            FSel.chEnd   := FEditor.synMDI.FoundRESections[FEditor.synMDI.FoundRENumSections];
            FEditor.synMDI.Perform(SEM_SELECTION, eaSET or euDRAW, longint(@FSel));
         end else
            ShowMessage('"' + FLastFindText + '" not found.');
   end;
end;

procedure TfrmMain.actReplaceExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   begin
      if actRegExp.Checked then
         FEditor.synMDI.Perform(SEM_REPLACETEXT, ft_REGEXPR, 0)
      else
         FEditor.synMDI.Perform(SEM_REPLACETEXT, 0, 0);
      FEditor.Modified := True;
      EnableButtons;
   end;
end;

procedure TfrmMain.SaveSettings;
var RegIni: TRegIniFile;
Begin
  RegIni := TRegIniFile.Create('Software\Echo Software\PN');
  Try
     RegIni.WriteBool('Firsttime', 'First', False);
     RegIni.WriteBool('Tabs', 'Position', tabs.TabPosition = tpTop);
     SyntaxMemoParser1.StylesToRegistry;
     SyntaxMemoParser2.StylesToRegistry;
     SyntaxMemoParser3.StylesToRegistry;
     SyntaxMemoParser4.StylesToRegistry;
     SyntaxMemoParser5.StylesToRegistry;
     parPerl.StylesToRegistry;
     parINI.StylesToRegistry;
     parPlain.StylesToRegistry;
     parCSS.StylesToRegistry;
     parVB.StylesToRegistry;
  finally
     RegIni.Free;
  End;
end;

procedure TfrmMain.LoadSettings;
var RegIni: TRegIniFile;
Begin
  RegIni := TRegIniFile.Create('Software\Echo Software\PN');
  Try
     SetFiles := RegIni.ReadBool('Firsttime', 'First', True);
     Case RegIni.ReadBool('Tabs', 'Position', True) of
        True  : tabs.TabPosition := tpTop;
        False : tabs.TabPosition := tpBottom;
     End;
     SyntaxMemoParser1.StylesFromRegistry(True, '');
     SyntaxMemoParser2.StylesFromRegistry(True, '');
     SyntaxMemoParser3.StylesFromRegistry(True, '');
     SyntaxMemoParser4.StylesFromRegistry(True, '');
     SyntaxMemoParser5.StylesFromRegistry(True, '');
     parPerl.StylesFromRegistry(True, '');
     parINI.StylesFromRegistry(True, '');
     parPlain.StylesFromRegistry(True, '');
     parCSS.StylesFromRegistry(True, '');
     parVB.StylesFromRegistry(True, '');
  finally
     RegIni.Free;
  End;
end;

procedure TfrmMain.DoSetFilesFirst;
var fm  : tfrmWelcome;
    Dlg : TfrmOptions;
    ass : Boolean;
Begin
  fm := tfrmWelcome.Create(Self);
  Try
     fm.ShowModal;
  finally
     ass := fm.chkAssociate.Checked;
     fm.Free;
  End;
  If ass Then
  Begin
     Dlg := tfrmOptions.Create(Self);
     Try
        Dlg.PageControl1.ActivePage := Dlg.tabFiles;
        Dlg.ShowModal;
     finally
        Dlg.Free;
     End;
  End;
end;

Procedure TfrmMain.UpdateStatusBar;
var FEditor : TfrmClient;
    i       : Integer;
Begin
   FEditor := GetCurrentEditor;
   if FEditor = nil then
   Begin
      for i:=0 to StatusBar1.Panels.Count-1 do
         StatusBar1.Panels[i].Text:='';
      Exit;
   End;
   With FEditor do
   Begin
      if Modified then
         StatusBar1.Panels[1].Text:='Modified'
      else
         StatusBar1.Panels[1].Text:='';
      StatusBar1.Panels[0].Text:=IntToStr(synMDI.CaretPos.X)+', '+IntToStr(synMDI.CaretPos.Y)+' : '+IntToStr(synMDI.Lines.Count);
      if FEditor.synMDI.InsertMode then
         StatusBar1.Panels[2].Text :=' Insert'
      else
         StatusBar1.Panels[2].Text :=' Overwrite';
   End;
end;

Procedure TfrmMain.ShowText(const Text: String);
Begin
  StatusBar1.Panels[3].Text := Text;
  Timer1.Enabled := True;
end;

procedure TfrmMain.actPropertiesExecute(Sender: TObject);
var FEditor: TfrmClient;
Begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then FEditor.synMDI.ModifyProperties;
end;

procedure TfrmMain.COPYDATA(var Message: TWMCopyData);
var CData : TCopyDataStruct;
    st    : PChar;
Begin
  CData := Message.CopyDataStruct^;
  st := CData.lpData;
  OpenFile(st, '');
  if Self.WindowState = wsMinimized then Self.WindowState := wsNormal;
  Self.Show;
  //ShowWindow(Self.Handle,SW_SHOWNORMAL);
  Perform(CM_ACTIVATE, 0, 0);
end;



procedure TfrmMain.pmTabControlPopup(Sender: TObject);
begin
  itmTop.Checked := tabs.TabPosition = tpTop;
  itmBottom.Checked := tabs.TabPosition = tpBottom;
end;

procedure TfrmMain.MRUFileList1MRUItemClick(Sender: TObject;
  AFilename: String);
var i : Integer;
begin
   for i := 0 to tabs.Tabs.Count-1 do
      Begin
         if CompareText(AFilename, TfrmClient(Tabs.Tabs.Objects[i]).FileName)=0 then
         Begin
            Tabs.TabIndex := i;
            Exit;
         End;
      End;
   OpenFile(AFileName,'');
end;

procedure TfrmMain.actPrintExecute(Sender: TObject);
var Dlg : TfrmPrint;
Begin
   Dlg := TfrmPrint.Create(Self);
   Try
      Dlg.ShowModal;
   finally
      Dlg.Free;
   end;
end;

procedure TfrmMain.actPrintSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfrmMain.actIndentExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then FEditor.synMDI.Perform(SEM_INDENT, 0, 0);
end;

procedure TfrmMain.actUnindentExecute(Sender: TObject);
var FEditor: TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then FEditor.synMDI.Perform(SEM_UNDENT, 0, 0);
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
var Dlg : TfrmAbout;
begin
   Dlg := TfrmAbout.Create(Self);
   Try
      Dlg.ShowModal;
   finally
      Dlg.Free;
   end;
end;

procedure TfrmMain.NewFromMenu(Sender: TObject);
begin
   NewType(TMenuItem(Sender).Tag);
end;

procedure TfrmMain.SetBookmark(Sender: TObject);
var FEditor : TfrmClient;
begin
  {//
   // Set bookmark at caret position
   //}
  FEditor := GetCurrentEditor;
  if Assigned(FEditor)
   then with FEditor.synMDI do
     SetBookmark((Sender as TMenuItem).Tag, CaretPos.Y, CaretPos.X);
end;

procedure TfrmMain.itmBookmark1Click(Sender: TObject);
var MenuItem  : TMenuItem;
    bmCol,
    bmRow     : longint;
    i         : Integer;
    FEditor   : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then
    Begin
       itmNoneSet.Visible := True;
       MenuItem := TMenuItem(Sender);
       for i := 0 to MenuItem.Count -2 do
       Begin
         MenuItem.Items[i].Visible := FEditor.synMDI.IsBookmarkSet(MenuItem.Items[i].Tag, bmRow, bmCol);
         If MenuItem.Items[i].Visible Then itmNoneSet.Visible := False;
       End;
    end;
end;

procedure TfrmMain.GotoBookmark(Sender: TObject);
var FEditor: TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then FEditor.synMDI.JumpToBookmark((Sender as TMenuItem).Tag);
end;

procedure TfrmMain.popBookmarkPopup(Sender: TObject);
var MenuItem : TPopupMenu;
    bmCol,
    bmRow    : longint;
    i        : Integer;
    FEditor  : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   Begin
      MenuItem:=TPopupMenu(Sender);
      for i:=0 to MenuItem.Items.Count-1 do
         MenuItem.Items[i].Visible := FEditor.synMDI.IsBookmarkSet(MenuItem.Items[i].Tag, bmRow, bmCol);
   end;
end;

procedure TfrmMain.actRedoExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then FEditor.synMDI.Perform(SEM_REDO, 0, 0);
end;



procedure TfrmMain.actSaveAsHTMLExecute(Sender: TObject);
var FEditor : TfrmClient;
    nfn     : string;
    oft     : TSaveFormat;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   begin
      oft := FEditor.synMDI.SaveFormat;
      with SaveDialog2 do
      begin
         if FEditor.Filename = 'New' then Filename := '' else
         begin
            nfn := ExtractFileName(FEditor.Filename);
            nfn := copy(nfn, 1, pos(ExtractFileExt(FEditor.Filename), nfn)-1);
            Filename := nfn;
         end;
         DefaultExt := 'htm';
         if Execute then
         begin
            FEditor.synMDI.SaveFormat := sfHTML;
            FEditor.synMDI.SaveToFile(Filename);
         end;
      end;
      FEditor.synMDI.SaveFormat := oft;
   end;
end;

procedure TfrmMain.actWordWrapExecute(Sender: TObject);
var FEditor : TfrmClient;
    n : integer;
begin
  for n := 0 to tabs.tabs.Count - 1 do
  begin
     FEditor := tfrmClient(tabs.tabs.Objects[n]);
     if Assigned(FEditor) then
        if FEditor.synMDI.Wordwrap = actWordWrap.Checked then
        Begin
           FEditor.synMDI.Wordwrap := not FEditor.synMDI.Wordwrap;
           FEditor.UpdateWordWrap;
        end;
  end;
  actWordWrap.Checked := not actWordWrap.Checked;
  EnableButtons;
end;

procedure TfrmMain.actEditHTMLExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   if TagEditDlg = nil then TagEditDlg := TTagEditDlg.Create(Self);
   with FEditor do
   begin
      if not TagEditDlg.EditHTMLTags(synMDI) then
          if TagEditDlg.ErrMsg <> '' then
             MessageDlg(TagEditDlg.ErrMsg, mtError,[mbOK],0);
   end;
end;

procedure TfrmMain.actUppercaseExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
   Begin
      SelText := Uppercase(SelText);
      AddLineGlyph(15,CaretPos.Y);
   end;
end;

procedure TfrmMain.actLowercaseExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
   Begin
      SelText := Lowercase(SelText);
      AddLineGlyph(15,CaretPos.Y);
   end;
end;

procedure TfrmMain.actSelectTagExecute(Sender: TObject);
var FEditor : TfrmClient;
    TagText : string;
begin
   SelectHTMLTag(false);
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
      if SelLength = 0 then ShowText('Cursor is not within a tag') else
      begin
         TagText := 'TAG: ' + StripNewLines(SelText);
         if length(TagText) > 50 then TagText := copy(TagText, 1, 50) + '...';
      end;
end;

procedure TfrmMain.actSelectEnclosingExecute(Sender: TObject);
var FEditor  : TfrmClient;
    i,
    orgStart : longint;
    TagName,
    TagText  : string;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
   begin
      orgStart := SelStart;
      SelectHTMLTag(true);
      if SelStart = orgStart then ShowText('No enclosing block') else
      begin
         TagText := StripNewLines(SelText);
         //
         // Pick up the tag name...
         //
         i := 1;
         TagName := '';
         while (i <= length(TagText)) and (not (TagText[i] in HTMLTagLetters)) do inc(i);
         while (i <= length(TagText)) and (TagText[i] in HTMLTagLetters) do
         begin
            TagName := TagName + TagText[i];
            inc(i);
         end;
      end;
   end;
end;

procedure TfrmMain.SelectHTMLTag(SelectEnclosingBlock: Boolean);
var FEditor      : TfrmClient;
    FLines       : TStrings;
    extPos,
    orgPos       : TSelExtents;
    i,
    MaxY,
    CachedLine   : longint;
    TagName,
    LineCache    : string;

    //
    // Referring to Lines[Y] in code yields slooooooow applications since the
    // actual text of the line has to be extracted from the edit control on
    // each access.
    // This function implements a cache of the Lines[] property for edit
    // controls, just remember to set CachedLine to -1 before using
    //
    function GetLine(aLine: longint): string;
    begin
      if aLine <> CachedLine
       then begin
         CachedLine := aLine;
         LineCache  := Uppercase(FLines[aLine]);
        end;
      result := LineCache;
    end;

    function PosDir(SubStr, txt: string; Start: longint; Dir: Boolean): longint;
    var i, j : longint;
    begin
      //
      // Locate SubStr in TXT starting from START. Dir = true to look forwards, false for back
      //
      result := 0;
      j := Start;
      while (j > 0) and (j <= length(txt)) do begin
        if Dir
         then i := 1
         else i := length(SubStr);
        while txt[j] = SubStr[i] do
         if Dir
          then
           if i = length(SubStr)
            then begin
              result := (j-i) + 1;
              exit;
             end
            else begin
              inc(i);
              inc(j);
              if j > length(txt) then exit;
             end
          else
           if i = 1
            then begin
              result := j;
              exit;
             end
            else begin
              dec(i);
              dec(j);
              if j = 0 then exit;
             end;
        if Dir
         then inc(j)
         else dec(j);
       end;
    end;



    //
    // Locate TXT in the control, starting at line SY.
    //
    // When looking, locate items either after (FORWARDS = true), or before (FORWARDS = false)
    //
    // Return line/offset of first character of located text.
    //
    // If TXT is not located then result.Y will be -1 ***REMEMBER TO CHECK***
    //
    function LocateToken(txt: string; Forwards: Boolean; sX, sY: longint): TXYRef;
    var FoundAt : longint;

    begin
      result.X := sX; result.Y := sY;
      txt      := Uppercase(txt);
      repeat
        FoundAt := PosDir(txt, GetLine(sY), sX, Forwards);
        if (FoundAt > 0)      // Text located
           and ( // Special case -- first line needs reference to direction search
                (sY <> result.Y)
                or
                ( (Forwards and (FoundAt >= sX)) or (FoundAt < sX) )
               )
         then begin
           //
           // Got it
           //
           result.X := FoundAt;
           result.Y := sY;
           exit;
          end;
        if Forwards
         then begin
           inc(sY);
           sX := 1;
          end
         else begin
           dec(sY);
           if sY >= 0 then sX := length(getLine(sY));
          end;
      until (sY < 0) or (sY > MaxY);
      //
      // Gone beyond text extents
      //
      result.Y := -1;
    end;

    //
    // Turn the current selection of text in the edit control into line/offset pairs
    //
    function SelectionToXY: TSelExtents;
    begin
      //
      // Get current selection as a pair of line/offset values
      //
      FEditor.synMDI.Perform(EM_GETSEL, longint(@result.StartPos.X), longint(@result.StartPos.Y));
      result.StartPos := TXYRef(Normalise(TChRange(result.StartPos)));
      result.EndPos.X := result.StartPos.X;
      //
      // Now translate both into Line/Offset values
      //
      result.StartPos.Y := FEditor.synMDI.Perform(EM_LINEFROMCHAR, result.StartPos.X, 0);
      dec(result.StartPos.X, FEditor.synMDI.Perform(EM_LINEINDEX, result.StartPos.Y, 0));
      //
      result.EndPos.Y := FEditor.synMDI.Perform(EM_LINEFROMCHAR, result.EndPos.X, 0);
      dec(result.EndPos.X, FEditor.synMDI.Perform(EM_LINEINDEX, result.EndPos.Y, 0));
      //
      // Now StartPos is the line/offset of the start of the current selection, and
      //     EndPos   is the line/offset of the end   of the current selection
    end;


begin
   //
   // HTML only - select enclosing tag
   //
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
      with FEditor do
      begin
         FLines       := synMDI.Lines;
         MaxY         := FLines.Count-1;
         CachedLine   := -1;
         orgPos       := SelectionToXY;
         repeat
            //
            // Now go back from start of selection looking for '<'
            //
            repeat
               extPos.EndPos   := LocateToken('>', false, orgPos.StartPos.X, orgPos.StartPos.Y);
               extPos.StartPos := LocateToken('<', false, orgPos.StartPos.X, orgPos.StartPos.Y);
               if extPos.StartPos.Y = -1 then exit;
               //
               // Did we find '>' before '<' ?
               //
               if (not SelectEnclosingBlock)
                  and
                  ( (extPos.EndPos.Y > extPos.StartPos.Y)
                  or
                  ( (extPos.EndPos.Y = extPos.StartPos.Y) and (extPos.EndPos.X > extPos.StartPos.X))
                ) then exit;
               //
               // Pick up the tag name...
               //
               TagName := '';
               i       := extPos.StartPos.X;
               with extPos.StartPos do
               repeat
                  inc(i);
                  if (i <= length(GetLine(Y)))
                     and
                     (getline(Y)[i] in HTMLTagLetters)
                     then TagName := TagName + getline(Y)[i]
                  else break;
               until false;
               if TagName = '' then
               begin
                  //
                  // We didn't find a tagname -- looking again before the '<' just located
                  //
                  orgPos.StartPos := extPos.StartPos;
                  dec(orgPos.StartPos.X);
                  if orgPos.StartPos.X = 0 then
                  begin
                     dec(orgPos.StartPos.Y);
                     if orgPos.StartPos.Y < 0 then exit;
                     orgPos.StartPos.X := length(getLine(orgPos.StartPos.Y));
                  end;
               end;
            until (Tagname <> '');
            //
            // Found '<TAG'
            //
            // What do we want to terminate it ?
            //   SelectEnclosingBlock = true  ===> need '</TAG' to be located
            //   SelectEnclosingBlock = false ===> need '>' to be located
            //
            // PROBLEM:  Need to watch out for '>' within string constant, use TokenAtCharPos function
            //           to track strings. Best placed in the LocateToken sub-function.
            //
            if SelectEnclosingBlock then TagName := '</' + TagName + '>'
            else TagName := '>';
            //
            // Go forward and look for the next '</'
            //
            extPos.EndPos := LocateToken(TagName, true, orgPos.EndPos.X, orgPos.EndPos.Y);
            if extPos.EndPos.Y <> -1 then break;
            //
            // Now we have found '<TAG.......> but no '</TAG' afterwards.
            //
            // Adjust start position to commence looking before '<TAG' and start again
            //
            orgPos.StartPos := extPos.StartPos;
            dec(orgPos.StartPos.X);
            if orgPos.StartPos.X = 0 then
            begin
               dec(orgPos.StartPos.Y);
               if orgPos.StartPos.Y < 0 then exit;
               orgPos.StartPos.X := length(getLine(orgPos.StartPos.Y));
            end;
         until false;
         //
         // We now have line/offsets for extent of tag, pass to editor for selection
         //
         synMDI.Perform(EM_SETSEL, synMDI.Perform(EM_LINEINDEX, extPos.StartPos.Y, 0) + extPos.StartPos.X - 1,
                                   synMDI.Perform(EM_LINEINDEX, extPos.EndPos.Y,   0) + extPos.EndPos.X
                                                                          + length(TagName) - 1);
      end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  StatusBar1.Panels[3].Text:='';
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
var Pref : TfrmOptions;
begin
  Pref := TfrmOptions.Create(Self);
  Try
     Pref.ShowModal;
  finally
     Pref.Free;
  end;
end;

procedure TfrmMain.actPreviewHTMLExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if not assigned(FEditor) then exit;
   { Because of partial boolean evaluation, this will probably only
    examine the first statement... }
   if (FEditor.FileName = newhtm) or
      (FEditor.FileName = newtxt) or
      (FEditor.FileName = newpl)  or
      (FEditor.FileName = newcss) or
      (FEditor.FileName = newjav) or
      (FEditor.FileName = newpas) or
      (FEditor.FileName = newsql) or
      (FEditor.FileNAme = newini) or
      (FEditor.FileName = newvb) or
      (FEditor.FileName = newc) Then
   Begin
      MessageDlg('You must save this file first', mtInformation, [mbOK], 0);
      Exit;
   End;
   if frmPreview = nil then
      frmPreview := TfrmPreview.Create(Self);
   frmPreview.Show;
   frmPreview.FileName:=FEditor.FileName;
end;

procedure TfrmMain.itmBookmarkClick(Sender: TObject);
var MenuItem : TMenuItem;
    bmCol,
    bmRow    : longint;
    i        : Integer;
    FEditor  : tfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   Begin
      MenuItem := TMenuItem(Sender);
      for i:=0 to MenuItem.Count - 1 do
        MenuItem.Items[i].Checked := FEditor.synMDI.IsBookmarkSet(MenuItem.Items[i].Tag, bmRow, bmCol);
   end;
end;

procedure TfrmMain.popSetBookmarkPopup(Sender: TObject);
var MenuItem : TPopupMenu;
    bmCol,
    bmRow    : longint;
    i        : Integer;
    FEditor  : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   Begin
      MenuItem:=TPopupMenu(Sender);
      for i:=0 to MenuItem.Items.Count-1 do
        MenuItem.Items[i].Checked := FEditor.synMDI.IsBookmarkSet(MenuItem.Items[i].Tag, bmRow, bmCol);
   end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Settings   : tRegistry;
    DLLHandle  : THandle;
    i          : integer;
    PluginQuit : tPlgProc;
begin
   {$IFDEF Plugin}
   for i := 0 to PluginQuitList.Count - 1 do
   begin
      DLLHandle := LoadLibrary(PChar(ExtractFileName(PluginQuitList[i])));
      if (DLLHandle <> 0) and (PluginIF <> 0) then
      begin
        // Initialise the Plugin...
        @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
        // Make sure we pass the handle of the plugin interface...
        PluginInit(PluginIF);
        // Pass the plugin the message number for atoms etc...
        @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
           PlgMsgNum(MsgNum);
        @PluginQuit := GetProcAddress(DLLHandle, 'QuitExecute');
           PluginQuit;
        FreeLibrary(DLLHandle);
      end;
   end;
   {$ENDIF}
   If frmMain.WindowState <> wsMaximized Then
   Begin
      SetPosition('Echo Software', 'PN', Top, Left);
      SetSize('Echo Software', 'PN', frmMain.Height, frmMain.Width);
   End;
   SetStat('Echo Software', 'PN', WindowState);
      {tSysEditFile = (seAutoExec, seConfig, seWinIni, seSysIni, seProt);}
   Settings := tRegistry.Create;
   Try
      Settings.OpenKey(SEKey, True);
      Settings.WriteBool('AutoExec', seAutoExec in SEFiles);
      Settings.WriteBool('Config', seConfig in SEFiles);
      Settings.WriteBool('WinIni', seWinIni in SEFiles);
      Settings.WriteBool('SysIni', seSysIni in SEFiles);
      Settings.WriteBool('Prot', seProt in SEFiles);
      Settings.CloseKey;
      Settings.OpenKey(RootKey, True);
      Settings.WriteBool('ShowTabs', actTabs.Checked);
      Settings.WriteBool('WordWrap', actWordWrap.Checked);
      Settings.WriteBool('SaveAsUnix', SaveAsUnix);
      Settings.WriteBool('ExeAsHex', OpenExeAsHex);
      Settings.WriteBool('OpenMaxed', OpenFilesMaxed);
      case tabs.Align of
        alTop    : Settings.WriteInteger('TabPos', 1);
        alBottom : Settings.WriteInteger('TabPos', 2);
      end;
      Settings.CloseKey;
      {Internet Settings}
      Settings.OpenKey(NetKey, True);
      Settings.WriteBool('SendError', InternetReport);
      Settings.WriteBool('SendWithoutAsk', AlwaysSend);
      Settings.WriteString('ProxyHost', ProxyHost);
      Settings.WriteString('ProxyPort', ProxyPort);
      Settings.WriteBool('UseProxy', UseProxy);
      Settings.WriteString('MailHost', MailHost);
      Settings.WriteString('MailPort', MailPort);
      Settings.WriteString('UserName', UserNameF);
      Settings.WriteString('UserEmail', UserEmail);
      Settings.CloseKey;
   Finally
      Settings.Free;
   end;
   SaveToolbars;
   if Assigned(MsgBody) then MsgBody.Free;
end;

procedure TfrmMain.WMChar(var Msg: TWMChar);
var Shift: TShiftState;
Begin
   inherited;
   Shift := KeyDataToShiftState(Msg.KeyData);
   if (Msg.CharCode = 8) and (ssCtrl in Shift) and (ssShift in Shift) and
     (Tabs.TabIndex>0) then
   Begin
      Tabs.TabIndex := Tabs.TabIndex-1;
      TabsChange(Tabs);
   End else
      if (Msg.CharCode=8) and (ssCtrl in Shift) and (Tabs.TabIndex < Tabs.Tabs.Count-1) then
      Begin
         Tabs.TabIndex := Tabs.TabIndex + 1;
         TabsChange(Tabs);
      end;
end;

procedure TfrmMain.itmTopClick(Sender: TObject);
Begin
   ChangeTabPos(tMenuItem(Sender).Tag)
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  if not (FEditor.FileName = newtxt) and
     not (FEditor.FileName = newhtm) and
     not (FEditor.FileName = newpl)  and
     not (FEditor.FileName = newcss) and
     not (FEditor.FileName = newjav) and
     not (FEditor.FileName = newpas) and
     not (FEditor.FileName = newini) and
     not (FEditor.FileName = newsql) and
     not (FEditor.FileName = newvb) and
     not (FEditor.FileName = newc) Then FEditor.SaveFile
     Else actSaveAsExecute(Sender);
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.FileName,'');
end;

procedure TfrmMain.actViewHexExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   if not assigned(FEditor) then exit;
   case fEditor.Mode of
      emHex    : fEditor.SetMode(emNormal);
      emNormal : fEditor.SetMode(emHex);
   end;
end;

procedure TfrmMain.actWebUpdateExecute(Sender: TObject);
begin
   if UseProxy then
   begin
      wfInfo.ProxyHost := ProxyHost;
      wfInfo.ProxyPort := strtoint(ProxyPort);
   end;
   wfInfo.CheckForUpdate;
end;

procedure TfrmMain.actSysEditExecute(Sender: TObject);
var dir  : String; //Windows Directory
begin
   SetLength(dir, MAX_PATH);
   GetWindowsDirectory(pChar(dir), MAX_PATH);
   SetLength(dir, StrLen(pChar(dir)));
   If seAutoExec in SEFiles Then
      If FileExists('c:\autoexec.bat') Then OpenFile('c:\autoexec.bat', '.bat');
   If seConfig in SEFiles Then
      If FileExists('c:\config.sys') Then OpenFile('c:\config.sys', '.sys');
   If seWinIni in SEFiles Then
      If FileExists(dir + '\win.ini') Then OpenFile(dir + '\win.ini', '.ini');
   If seSysIni in SEFiles Then
      If FileExists(dir + '\system.ini') Then OpenFile(dir + '\system.ini', '.ini');
   If seProt in SEFiles Then
      If FileExists(dir + '\protocol.ini') Then OpenFile(dir + '\protocol.ini', '.ini');
end;

function TfrmMain.parINICustomParse(Sender: TObject; ParseID: Integer;
  IStream: TEdStream; var kLength, kValue: Integer): Boolean;
var orgKL,
    sPos   : longint;
begin
  //
  // INI parser support functions...
  //
  result := true;
  sPos   := IStream.yypos;
  orgKL  := klength;
  case ParseID of
    1  :   //
           // Newline (without recognised follower)
           //
           // Scan until one of the following is located:
           //    \n      ------    Return entire line as default
           //    =       ------    Return as key name
           //
           with IStream do begin
             while (not yyeof) and (not (yychar in [#13, '='])) do begin
               yyadvance;
               inc(kLength);
              end;
             if yyeof
              then kValue := 0 else
             case yychar of
               #13       : if kLength > 1
                            then begin
                               kValue  := 8;
                               kLength := orgKL;
                               yypos   := sPos;
                             end
                            else kValue := 0;
               '='       : kValue := 3; // Key token value
             end;
           end;
  end;
end;

procedure TfrmMain.ChangeTabPos(Pos : integer);
begin
   Case Pos of
      1: Case Tabs.TabPosition of
            tpTop    : Begin
                          Tabs.Align := alTop;
                          panBack.Align := alTop;
                       end;
            tpBottom : Begin
                          Tabs.TabPosition := tpTop;
                          Tabs.Align := alTop;
                          panBack.Align := alTop;
                       end;
         End;
      2: Case Tabs.TabPosition of
            tpTop    : Begin
                          Tabs.TabPosition := tpBottom;
                          Tabs.Align := alBottom;
                          panBack.Align := alBottom;
                       end;
            tpBottom : Begin
                          Tabs.Align := alBottom;
                          panBack.Align := alBottom;
                       end;
         End;
   end;
end;

procedure TfrmMain.ExceptionHandler(Sender: TObject; E: Exception);
var nrform : TfrmNetError;
begin
   Application.OnException := nil;
   logMain.Add('exception', E.Message);
   case frmMain.InternetReport of
      True  : begin
                 nrform := TfrmNetError.Create(Self);
                 if nrform.Execute then
                 begin
                    case nrform.ButtonResult of
                       neSendNow    : SendErrorReport(E.Message);
                       neSendAlways : begin
                                         AlwaysSend := True;
                                         SendErrorReport(E.Message);
                                      end;
                       neDontSend   : MessageDlg('There was an internal error:'+#13#10#9+E.Message, mtError, [mbOK], 0);
                       neNeverSend  : InternetReport := False;
                       neSaveReport : SaveErrorReport(E.Message);
                    end;
                 end;
                 nrform.Free;
              end;
      False : MessageDlg('There was an internal error:'+#13#10#9+E.Message, mtError, [mbOK], 0);
   end;
   Application.OnException := ExceptionHandler;
end;

function TfrmMain.PrepareErrorReport(Error : string) : string;
var mstream : TMemoryStream;
    fstream : tFileStream;
    ErrText : string;
begin
  //Prepare an error report
  mstream := TMemoryStream.Create;
  fstream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'error.elg', fmCreate or fmShareCompat);
  Try
     try logMain.LogAsStream(mstream) except end;
     fstream.CopyFrom(mstream, 0);
  Finally
     fstream.Free;
     mstream.Free;
  end;
  ErrText :=
  '' + #13#10 +
  'Programmers Notepad Error Report' + #13#10 +
  '================================' + #13#10 +
  '' + #13#10 +
  'Name: ' + UserNameF + #13#10 +
  'E-Mail: ' + UserEmail + #13#10#13#10 +
  'Date: ' + FormatDateTime('ddd dd mmm yyyy', Now) + #13#10 +
  'Time: ' + FormatDateTime('hh:nn', Now) + #13#10 +
  '' + #13#10 +
  'Error Reported: ' + Error + #13#10 +
  '-------------------------------------------------------' + #13#10 +
  '                    END OF REPORT                      ' + #13#10 +
  '-------------------------------------------------------';
  Result := ErrText;
end;

procedure TfrmMain.SendErrorReport(Error : string);
var
   Filename : string;
begin
  //Send an error report
  if Length(MailHost) < 1 then Exit;
  FileName := ExtractFilePath(ParamStr(0)) + 'error.elg';
  if not Assigned(MsgBody) then MsgBody := TStringList.Create;
  MsgBody.Clear;
  MsgBody.Text := PrepareErrorReport(Error);
  smtp.EmailFiles.Add(FileName);
  smtp.FromName := UserEmail;
  smtp.HdrFrom := '"' + UserNameF + '" <' + UserEmail + '>';
  smtp.HdrTo := ErrAddr;
  smtp.RcptName.Add(ErrAddr);
  smtp.Host := MailHost;
  smtp.HdrSubject := ErrSubj;
  if RemoveSpaces(MailPort) <> '25' then smtp.Port := RemoveSpaces(MailPort);
  smtp.AutoSend;
end;

procedure TfrmMain.SaveErrorReport(Error : string);
var
   FileName : string;
   ErrText  : string;
   tf       : TextFile;
begin
  //Save an error report
  FileName := ExtractFilePath(ParamStr(0)) + 'error.elg';
  ErrText := PrepareErrorReport(Error);
  if dlgSaveError.Execute then
  begin
     AssignFile(tf, dlgSaveError.FileName);
     Rewrite(tf);
     Writeln(tf, ErrText);
     if not Assigned(Msgbody) then Msgbody := TStringList.Create;
     MsgBody.Clear;
     if FileExists(FileName) then MsgBody.LoadFromFile(FileName);
     Writeln(tf, MsgBody.Text);
     System.CloseFile(tf);
  end;
end;

procedure TfrmMain.actTabsExecute(Sender: TObject);
begin
   actTabs.Checked := not actTabs.Checked;
   EnableButtons;
end;

procedure TfrmMain.smtpGetData(Sender: TObject; LineNum: Integer;
  MsgLine: PChar; MaxLen: Integer; var More: Boolean);
var
    Len  : Integer;
begin
    if LineNum > MsgBody.Count then
        More := FALSE
    else begin
        Len := Length(MsgBody[LineNum - 1]);
        { Truncate the line if too long (should wrap to next line) }
        if Len >= MaxLen then
            StrPCopy(MsgLine, Copy(MsgBody[LineNum - 1], 1, MaxLen - 1))
        else
            StrPCopy(MsgLine, MsgBody[LineNum - 1]);
    end;
end;

procedure TfrmMain.HexButtons;
begin
   with GetCurrentEditor do
   Begin
   {Bookmarks Disabled}
       sbBookmark.Enabled         := False;
       sbSetBookmark.Enabled      := False;
       btnAddBM.Enabled           := False;
       btnRemoveBM.Enabled        := False;
       lstBookmarks.Enabled       := False;
       lstBookmarks.Items.Clear;
   {Find???}
       sbFind.Enabled             := HexEditor.DataSize > 0;
       {TActions}
       actProperties.Enabled      := False;
       actPasteFromFile.Enabled   := False;
       actDelete.Enabled          := False;
       actSelectAll.Enabled       := False;
       actSaveAll.Enabled         := True;
       actPrint.Enabled           := True;
       actViewHex.Enabled         := True;
       actCopy.Enabled            := (HexEditor.SelCount > 0);
       actCut.Enabled             := (HexEditor.SelCount > 0);
       actPaste.Enabled           := Clipboard.HasFormat(CF_TEXT);
       actClose.Enabled           := True;
       actSave.Enabled            := (HexEditor.Modified) and (not HexEditor.ReadOnly);
       actSaveAs.Enabled          := (not HexEditor.ReadOnly);
       actPrint.Enabled           := True;
{Save As HTML}
       actSaveAsHTML.Enabled      := False;
{Search and Replace}
       actFind.Enabled            := (HexEditor.DataSize > 0);
       actFindNext.Enabled        := (HexEditor.DataSize > 0);
       actReplace.Enabled         := False;
{Indent}
       actIndent.Enabled          := False;
       actUnindent.Enabled        := False;
{Undo and Redo}
       with actUndo do
       begin
          Enabled := HexEditor.CanUndo;
          Caption := '&Undo : ' + HexEditor.UndoDescription;
       end;
       actRedo.Enabled            := False;
{WordWrap}
       actWordWrap.Enabled        := False;
{HTML View}
       actEditHTML.Enabled        := False;
{Upper/Lower Case}
       actUpperCase.Enabled       := False;
       actLowerCase.Enabled       := False;
{HTML Tag Stuff}
       actSelectTag.Enabled       := False;
       actSelectEnclosing.Enabled := False;
       actPreviewHTML.Enabled     := False;
       actPrintPreview.Visible    := True;
{Hex Specific Stuff}
       sepHex.Visible             := True;
       sepHex2.Visible            := True;
       sepHex3.Visible            := True;
       actGrid.Visible            := True;
       itmAsciiToAnsi.Visible     := True;
       actAsciitoAnsi.Checked     := HexEditor.OEMTranslate;
       actHexGoto.Visible         := True;
       actHexGoto.Enabled         := (HexEditor.DataSize > 0);
       actShowMarkers.Visible     := True;
       itmJump.Visible            := True;
       itmLineSize.Visible        := True;
       itmColumns.Visible         := True;
       itmCaretStyle.Visible      := True;
       itmOffset.Visible          := True;
       actJumpForwards.Enabled    := (HexEditor.DataSize > 0);
       actJumpBackwards.Enabled   := (HexEditor.DataSize > 0);
       itmHighlight.Visible       := False;
       sepNor.Visible             := False;
       itmBookmark.Visible        := False;
       itmBookmark1.Visible       := False;
       sepNor1.Visible            := False;
       case actTabs.Checked of
          True  : begin
                     panBack.Visible := True;
                     tabs.Visible := True;
                  end;
          False : begin
                     panBack.Visible := False;
                     tabs.Visible := False;
                  end;
       end;
   end;
end;

procedure TfrmMain.actPrintPreviewExecute(Sender: TObject);
begin
   // Currently only applies to the Hex Editing Mode.
   with GetCurrentEditor do if Mode = emHex then HexPrintPreview;
end;

procedure TfrmMain.actContentsExecute(Sender: TObject);
begin
   Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TfrmMain.actHexGotoExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(FEditor) then
      FEditor.HexGoto;
end;

procedure TfrmMain.actJumpAmountExecute(Sender: TObject);
var FEditor : tFrmClient;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then FEditor.SetJumpOffset;
end;

procedure TfrmMain.itmJumpClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) then
   begin
     actJumpAmount.Caption := '&Amount : ' + IntToStr ( fEditor.JumpOffs );
     actJumpForwards.Enabled := FEditor.HexEditor.DataSize > 0;
     actJumpBackwards.Enabled := FEditor.HexEditor.DataSize > 0;
   end;
end;

procedure TfrmMain.actJumpForwardsExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) then
      fEditor.hexJump(True);
end;

procedure TfrmMain.actJumpBackwardsExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) then
      fEditor.hexJump(False);
end;

procedure TfrmMain.act16BytesPerLineExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
     fEditor := GetCurrentEditor;
     if Assigned(fEditor) then with fEditor do
        HexEditor.BytesPerLine := TAction(Sender).Tag;
end;

procedure TfrmMain.itmLineSizeClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   Case fEditor.HexEditor.BytesPerLine
     of
       16 : act16BytesPerLine.Checked := True;
       32 : act32BytesPerLine.Checked := True;
       64 : act64BytesPerLine.Checked := True;
     end;
end;

procedure TfrmMain.act1BytesPerColumnExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
     fEditor.HexEditor.BytesPerColumn := tAction(Sender).Tag;
end;

procedure TfrmMain.actCaretFullExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
     fEditor.HexEditor.CaretStyle := TCaretStyle(tAction(Sender).Tag);
end;

procedure TfrmMain.itmCaretStyleClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;

   case fEditor.HexEditor.CaretStyle of
     csFull       : actCaretFull.Checked := True;
     csLeftLine   : actCaretLeft.Checked := True;
     csBottomLine : actCaretBottom.Checked := True;
   end;
end;

procedure TfrmMain.itmColumnsClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   case fEditor.HexEditor.BytesPerColumn of
     1 : act1BytesPerColumn.Checked := True;
     2 : act2BytesPerColumn.Checked := True;
     4 : act4BytesPerColumn.Checked := True;
   end;
end;

procedure TfrmMain.actHexOffsetExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.OffsetDisplay := TOffsetDisplayStyle(TAction(Sender).Tag);
end;

procedure TfrmMain.itmOffsetClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
     case fEditor.HexEditor.OffsetDisplay of
       odHex       : actHexOffset.Checked := True;
       odDec       : actDecOffset.Checked := True;
       odNone      : actNoOffset.Checked  := True;
     end;
end;

procedure TfrmMain.actAsciiToAnsiExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.OEMTranslate := not fEditor.HexEditor.OemTranslate;
   actAsciiToAnsi.Checked := fEditor.HexEditor.OEMTranslate;
end;

procedure TfrmMain.actGridExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.GridLineWidth := 1 - fEditor.HexEditor.GridLineWidth;
end;

procedure TfrmMain.mnuViewClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   itmHighlight.Enabled := (GetCurrentEditor <> nil);
   itmBookmark.Enabled  := (GetCurrentEditor <> nil);
   itmBookmark1.Enabled := (GetCurrentEditor <> nil);
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   if fEditor.Mode = emHex then
   Begin
      actGrid.Checked := fEditor.HexEditor.GridLineWidth = 1;
      actShowMarkers.Checked := fEditor.HexEditor.ShowMarkerColumn;
   end;
   actViewHex.Checked := fEditor.Mode = emHex;
{$ifdef bookmark}
   itmBookmarks.Checked := FloatBookmarks.Visible;
{$endif}
   itmResults.Checked := FloatResults.Visible;
end;

procedure TfrmMain.actShowMarkersExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.ShowMarkerColumn := not fEditor.HexEditor.ShowMarkerColumn;
end;

procedure TfrmMain.Timer2Timer(Sender: TObject);
var n : integer;
begin
   Timer2.Enabled := False;
   fStarted := true;
   If Assigned(OpenOnStart) then
   Begin
      For n := 0 to OpenOnStart.Count - 1 do
      begin
         OpenFile(OpenOnStart[n], '');
      end;
      OpenOnStart.Free;
   end;
end;

procedure TfrmMain.SaveToolbars;
var Settings : TRegistry;
begin
   Settings := TRegistry.Create;
   Try
      Settings.OpenKey(Toolkey, True);
   // -----------------------------------------------------------------
      If tbrMain.Floating Then
      begin
         Settings.WriteInteger('MainTop', tbrMain.ClientOrigin.x);
         Settings.WriteInteger('MainLeft', tbrMain.ClientOrigin.y);
      end else
      begin
         Settings.WriteInteger('MainTop', tbrMain.top);
         Settings.WriteInteger('MainLeft', tbrMain.left);
      end;
      Settings.WriteInteger('MainWidth2', tbrMain.Width);
      Settings.WriteInteger('MainHeight', tbrMain.Height);
      Settings.WriteBool('MainVis', tbrMain.Visible);
      Settings.WriteBool('MainUnDocked', tbrMain.Floating);
   // -----------------------------------------------------------------
      If tbrEdit.Floating Then
      begin
         Settings.WriteInteger('EditTop', tbrEdit.ClientOrigin.x);
         Settings.WriteInteger('EditLeft', tbrEdit.ClientOrigin.y);
      end else
      begin
         Settings.WriteInteger('EditTop', tbrEdit.top);
         Settings.WriteInteger('EditLeft', tbrEdit.left);
      end;
      Settings.WriteInteger('EditWidth2', tbrEdit.Width);
      Settings.WriteInteger('EditHeight', tbrEdit.Height);
      Settings.WriteBool('EditVis', tbrEdit.Visible);
      Settings.WriteBool('EditUnDocked', tbrEdit.Floating);
   // -----------------------------------------------------------------
      Settings.CloseKey;
   finally
      Settings.Free;
   end;
   RegSaveToolbarPositions(Self, Toolkey2);
end;

procedure TfrmMain.LoadToolbars(Settings: tRegistry);
var pos      : tRect;
begin
   Try Settings.CloseKey Except End;
   Settings.OpenKey(ToolKey, true);
   try If Settings.ReadBool('MainUnDocked') Then
       Begin
          pos.Top := Settings.ReadInteger('MainTop');
          pos.Bottom := Settings.ReadInteger('MainHeight') + pos.Top;
          pos.Left := Settings.ReadInteger('MainLeft');
          pos.Right := Settings.ReadInteger('MainWidth2') + pos.Left;
          tbrMain.ManualFloat(pos);
       End Else
       Begin
          tbrMain.Top := Settings.ReadInteger('MainTop');
          tbrMain.Left := Settings.ReadInteger('MainLeft');
          tbrMain.Width := Settings.ReadInteger('MainWidth2');
       end;
       tbrMain.Visible := Settings.ReadBool('MainVis');
   Except End;
   try if Settings.ReadBool('EditUnDocked') Then
       Begin
          pos.Top := Settings.ReadInteger('EditTop');
          pos.Bottom := Settings.ReadInteger('EditHeight') + pos.Top;
          pos.Left := Settings.ReadInteger('EditLeft');
          pos.Right := Settings.ReadInteger('EditWidth2') + pos.Left;
          tbrEdit.ManualFloat(pos);
       End Else
       Begin
          tbrEdit.Top := Settings.ReadInteger('EditTop');
          tbrEdit.Left := Settings.ReadInteger('EditLeft');
          tbrEdit.Width := Settings.ReadInteger('EditWidth2');
       end;
       tbrEdit.Visible := Settings.ReadBool('EditVis');
   Except End;
   Settings.CloseKey;
   RegLoadToolbarPositions(Self, Toolkey2);
end;

procedure TfrmMain.actSaveAllExecute(Sender: TObject);
var n : integer;
begin
   for n := 0 to tabs.tabs.Count - 1 do
   Begin
      TfrmClient(Tabs.Tabs.Objects[n]).SaveFile;
   end;
end;

procedure TfrmMain.actPasteFromFileExecute(Sender: TObject);
var FEditor : tfrmClient;
    frmPFF  : tfrmPFF;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then
  Begin
     If dlgPasteFrom.Execute Then
     begin
       frmPFF := tfrmPFF.Create(Self);
       Try
          frmPFF.memPFF.Lines.Clear;
          frmPFF.memPFF.Lines.LoadFromFile(dlgPasteFrom.FileName);
          frmPFF.ShowModal;
          If frmPFF.ModalResult = mrOK Then
          FEditor.synMDI.SelText := frmPFF.memPFF.SelText;
       finally
          frmPFF.Free;
       end;
     End;
  end;
end;

procedure TfrmMain.DropFiles(var msg: TWmDropFiles);
var
   buf:array [0..256] of char;
   i:integer;
begin
   for i:=0 to DragQueryFile(msg.drop,$FFFFFFFF,nil,0)-1 do
   begin
      DragQueryFile(msg.drop,i,buf,255);
      OpenFile(buf, '');
   end;
   DragFinish(msg.drop);
   msg.result:=0;
end;

function TfrmMain.GenSaveFilter : string;
var t        : TextFile;
    s        : string;
    ps       : string; {part of string}
    FileName : string;
    Part     : integer;
    ipos     : integer;
    filter   : string;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
begin
   // Load the file types into the dialog...
   FileName := ExtractFilePath(ParamStr(0)) + 'types.def';
   if not fileexists(filename) then
      CreatePNFile(filename, strOpenTypes);
   Assignfile(t, FileName);
   Reset(t);
   repeat
      Readln(t, s)
   until (Length(s) > 0) or EOF(t);
   System.CloseFile(t);
   if s = '' then Exit;
   part := Desc;
   Filter := '';
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
         Desc : begin
                  if Length(Filter) > 0 then Filter := Filter + sepChar;
                  Filter := Filter + ps;
                  part := Files;
                end;
         Files : begin
                   Filter := Filter + SepChar + ps;
                   part := Parser;
                 end;
         Parser : begin
                    //new.SubItems.Add(ps);
                    part := Desc;
                  end;
      end;
   until Length(s) < 1;
   if Length(Filter) > 0 then Filter := Filter + '|';
   Filter := Filter + 'All Files (*.*)|*.*';
   Result := Filter;
end;

function TfrmMain.GenOpenFilter : string;
var t        : TextFile;
    s        : string;
    ps       : string; {part of string}
    FileName : string;
    Part     : integer;
    ipos     : integer;
    filter   : string;
    all      : string;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
begin
   // Load the file types into the dialog...
   FileName := ExtractFilePath(ParamStr(0)) + 'types.def';
   if not fileexists(filename) then
      CreatePNFile(filename, strOpenTypes);
   Assignfile(t, FileName);
   Reset(t);
   repeat
      Readln(t, s)
   until (Length(s) > 0) or EOF(t);
   System.CloseFile(t);
   if s = '' then Exit;
   part := Desc;
   Filter := '';
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
         Desc : begin
                  if Length(Filter) > 0 then Filter := Filter + sepChar;
                  Filter := Filter + ps;
                  part := Files;
                end;
         Files : begin
                   Filter := Filter + SepChar + ps;
                   if Length(all) > 0 then all := all + ';';
                   all := all + ps;
                   part := Parser;
                 end;
         Parser : begin
                    //new.SubItems.Add(ps);
                    part := Desc;
                  end;
      end;
   until Length(s) < 1;
   Filter := 'Recognised File Types|' + all + '|' + Filter;
   if Copy(Filter, Length(Filter), 1) <> '|' then
      if Length(Filter) > 0 then Filter := Filter + '|';
   Filter := Filter + 'All Files (*.*)|*.*';
   Result := Filter;
end;

procedure TfrmMain.UpdateTypes;
var t        : TextFile;
    s        : string;
    ps       : string; {part of string}
    FileName : string;
    Part     : integer;
    ipos     : integer;
    filter   : string;
    count    : integer;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
begin
   // Setup the File-Type Tables...
   TypeName.Clear;
   TypeIndex.Clear;
   TypeFilter.Clear;
   // Load the file types into the string
   FileName := ExtractFilePath(ParamStr(0)) + 'types.def';
   if not fileexists(filename) then
      CreatePNFile(filename, strOpenTypes);
   Assignfile(t, FileName);
   Reset(t);
   repeat
      Readln(t, s)
   until (Length(s) > 0) or EOF(t);
   System.CloseFile(t);
   if s = '' then Exit;
   part := Desc;
   Filter := '';
   Count := 0;
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
         Desc : begin
                  part := Files;
                end;
         Files : begin
                   repeat
                      ipos := Pos(';', ps);
                      if ipos = 0 then ipos := Length(ps) + 1;
                      TypeName.Add(lowercase(Copy(ps, 1, ipos - 1)));
                      ps := Copy(ps, ipos + 1, Length(ps));
                   until Pos(';', ps) = 0;
                   if Length(ps) > 0 then TypeName.Add(ps);
                   part := Parser;
                 end;
         Parser : begin
                    part := Desc;
                    repeat
                       TypeIndex.Add(ps);
                       TypeFilter.Add(IntToStr(Count));
                    until TypeIndex.Count >= TypeName.Count;
                    Inc(Count);
                  end;
      end;
   until Length(s) < 1;
end;

procedure TfrmMain.UpdateDialogs;
begin
   if not FileExists(ExtractFilePath(ParamStr(0)) + 'types.def') then
      CreatePNFile(ExtractFilePath(ParamStr(0)) + 'types.def', strOpenTypes);
   OpenDialog1.Filter := GenOpenFilter;
   SaveDialog1.Filter := GenSaveFilter;
   UpdateTypes;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var n : integer;
    mi : TMenuItem;
begin
   // Free the various lookup stringlists.
   TypeName.Free;
   TypeIndex.Free;
   TypeFilter.Free;
   {$IFDEF plugin}
   PluginList.Free;
   PluginFileList.Free;
   PluginQuitList.Free;
   PluginMenuPlgs.Free;
   PluginMenuProcs.Free;
   for n := PluginMenuItems.Count - 1 downto 0 do
   begin
      mi := TMenuItem(PluginMenuItems[n]);
      PluginMenuItems.Remove(mi);
      mi.Free;
   end;
   PluginMenuItems.Free;
   FreePluginInterface;
   {$ENDIF}
   BookmarkMRU.Shutdown;
end;

function TfrmMain.GetFileType(Ext : string) : integer;
var i : integer;
begin
   // Ensure Extension is in *.ext format:
   if Pos('.', Ext) = 0 then Ext := '.' + Ext;
   Ext := '*' + Ext;
   // Find position in first stringlist:
   i := TypeName.IndexOf(Ext);
   if i = -1 then
      Result := 0
   else
      Result := StrToInt(TypeIndex[i]);
end;

{$IFDEF plugin}
procedure TfrmMain.CheckPlugins;
var search : tSearchRec;
    DLLHandle : THandle;
    i         : Integer;
    s         : string;
    p         : pChar;
    ExOnStart : tBoolResult;
begin
    if PluginIF = 0 then Exit;
    if FindFirst(ExtractFilePath(ParamStr(0)) + '*.dll', faAnyFile, search) = 0 then
    begin
       repeat
          if lowercase(Copy(ExtractFileName(search.Name), 1, 2)) = 'pn' then
          begin
             DLLHandle := LoadLibrary(PChar(ExtractFileName(search.Name)));
             if DLLHandle <> 0 then
             begin
               // Initialise the Plugin...
               @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
               // Make sure we pass the handle of the plugin interface...
               PluginInit(PluginIF);
               // Pass the plugin the message number for atoms etc...
               @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
               PlgMsgNum(MsgNum);
               // Get the Plugin Name...
               @PlgStrResult := GetProcAddress(DLLHandle, 'GetPluginName');
               GetMem(p, 256);
               i := PlgStrResult(p);
               s := string(p);
               SetLength(s, i);
               FreeMem(p, 256);
               // Add the plugin to the list...
               PluginList.Add(s);
               PluginFileList.Add(ExtractFileName(Search.Name));
               // Call the plugins execute procedure, should
               // be in a case statement to call the correct one.
               @ExOnStart := GetProcAddress(DLLHandle, 'ExecuteOnStart');
               if ExOnStart then
               begin
                  @ExOnStart := GetProcAddress(DLLHandle, 'StartExecute');
                  ExOnStart;
               end;
               // Check whether to execute on quitting.
               @ExOnStart := GetProcAddress(DLLHandle, 'ExecuteOnQuit');
               if ExOnStart then PluginQuitList.Add(ExtractFileName(Search.Name));
               // And finally free this plugin.
               FreeLibrary(DLLHandle);
             end;
          end;
       until FindNext(search) <> 0;
    end;
    FindClose(search);
end;
{$ENDIF}

procedure TfrmMain.actTbrMainExecute(Sender: TObject);
begin
   case tAction(Sender).Tag of
      0 : tbrMain.Visible := not tbrMain.Visible;
      1 : tbrEdit.Visible := not tbrEdit.Visible;
   end;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
   actTbrMain.Checked := tbrMain.Visible;
   actTbrEdit.Checked := tbrEdit.Visible;
end;

procedure TfrmMain.lstBookmarksResize(Sender: TObject);
begin
   lstBookmarks.Columns[0].Width := lstBookmarks.Width - 23;
end;

procedure TfrmMain.actBMAddExecute(Sender: TObject);
var
   fEditor : tfrmClient;
   s       : string;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   s := 'New Bookmark';
   if InputQuery('Custom Bookmark', 'Enter a bookmark name:', s) then
   begin
      fEditor.synMDI.AddCustomBookmark(s, fEditor.synMDI.SelStart);
   end;
   RefreshBookmarkList;
end;

procedure tfrmMain.RefreshBookmarkList;
var fEditor : tfrmClient;
    n       : integer;
    li      : tListItem;
begin
   lstBookmarks.Items.Clear;
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) then
   begin
      with fEditor do
      begin
         for n := 0 to synMDI.CustomBookmarkList.Count - 1 do
         begin
           li := frmMain.lstBookmarks.Items.Add;
           li.Caption := synMDI.CustomBookmarkList[n];
           li.ImageIndex := 15;
         end; // for n := 0 to...
      end; // With fEditor do...
   end else
      lstBookmarks.Items.Clear;
end;

procedure TfrmMain.actBMShowHideExecute(Sender: TObject);
begin
   case TMenuItem(Sender).Tag of
      0 : case tMenuItem(Sender).Checked of
             False : FloatBookmarks.Visible := True;
             True  : FloatBookmarks.Visible := False;
          end;
      1 : case TMenuItem(Sender).Checked of
             False : FloatResults.Visible := True;
             True  : FloatResults.Visible := False;
          end;
   end;
end;

procedure TfrmMain.btnBMRemoveClick(Sender: TObject);
var
   fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) and (lstBookmarks.SelCount > 0) then
   begin
      fEditor.synMDI.RemoveCustomBookmark(lstBookmarks.Selected.Caption);
   end;
   RefreshBookmarkList;
end;

procedure TfrmMain.lstBookmarksDblClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if Assigned(fEditor) and (lstBookmarks.SelCount > 0) then
   begin
      fEditor.synMDI.GotoCustomBookmark(lstBookmarks.Selected.Caption);
   end;
end;

{$IFDEF plugin}
procedure TfrmMain.MenuItemClick(Sender : TObject);
type tEvProc = procedure(Sender : TObject); stdcall;
var Plugin : Integer;
    PlgFile : string;
    PlgProc : string;
    DLLHandle : THandle;
    EventProc : tEvProc;
begin
// Run a procedure in a plugin due to a menuitem click.
   if PluginIF = 0 then Exit;
   Plugin := TMenuItem(Sender).Tag;
   PlgFile := PluginMenuPlgs[Plugin - 80];
   PlgProc := PluginMenuProcs[Plugin - 80];
   DLLHandle := LoadLibrary(PChar(PlgFile + '.dll'));
   if DLLHandle <> 0 then
   try
      // Initialise the Plugin...
      @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
      // Make sure we pass the handle of the plugin interface...
      PluginInit(PluginIF);
      // Pass the plugin the message number for atoms etc...
      @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
      PlgMsgNum(MsgNum);
      @EventProc := GetProcAddress(DLLHandle, PChar(PlgProc));
      EventProc(Sender);
   finally
      FreeLibrary(DLLHandle);
   end;
end;
{$ENDIF}

procedure TfrmMain.itmFileFindClick(Sender: TObject);
begin
   if Assigned(FindInFiles) then Exit;
   FindInFiles := tFindInFiles.Setup(True);
   FindInFiles.Pattern := 'set';
   FindInFiles.Resume;
end;

end.

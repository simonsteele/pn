{***************************************************************
 *
 * Unit Name: main
 * Purpose  : SMDI MDI Parent
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele.
 * History  : 19/11/2000 Having released Beta 14, begin work on Beta 15. Removed
 *                       bloated stringlist based plugin management code and
 *                       transferred to a record-based linked-list. Also changed
 *                       the plugin menu-item management code to simply use a
 *                       descendant of TMenuItem instead of two stringlists.
 *            24/11/2000 Back to Beta 14 again, because we have a couple of
 *                       problems. We'll release with the additional changes as
 *                       Beta 14a. Added some exception traps to plugin calls,
 *                       and changed a couple of interfacy bits in the options
 *                       dialog. Also moved transparency stuff for w2k into this
 *                       program, not using trans.dll any more...
 *            04/12/2000 Added optional warning on re-opening of files. If paste
 *                       is selected with no open files, a new file is opened
 *                       and the contents of the clipboard pasted in.
 *            02/01/2001 Re-Began work on Beta 15 with a bug fix for tool launches.
 *                       if cancel is pressed on the parameters request box the
 *                       tool will now not be launched. Also set the stretch
 *                       property for the image in the about dialogue to try to
 *                       fix large font problems. Added an "Open Files" view
 *                       to the Helper.
 *            10/01/2001 More work on the helper and also added the expandfile
 *                       function to a couple of routines to deal with local
 *                       file names. Also began to add experimental Mozilla
 *                       support in the preview form.
 *            11/01/2001 Made the Mozilla support hot-switchable. This is much
 *                       nicer. Also added the Built-In (Mozilla) item to all of
 *                       the browser configuration.
 *            22/01/2001 Mozilla support doesn't seem to work today - mozilla
 *                       control not happy under my new PC atm. Fixed the grep
 *                       search dialog to remember which directory was last used.
 *                       Added a Find in Files button to the toolbar. Uppercase
 *                       and Lowercase now keep the text selection. Added an
 *                       experimental VisualStudioMenus procedure to move the
 *                       menus around a bit to make it more like Visual Studio.
 *            09/02/2001 Mozilla support works again today! I think it just needs
 *                       a reboot after the path settings are changed. More clean
 *                       up work, and work on key-mappings configuration...
 *                       Added most of the support (interface and setting) for
 *                       keyboard configuration. Just need load and save now, and
 *                       some preset files.
 *            10/02/2001 No more changes need to be done to the VisualStudioMenus
 *                       function - it's only search stuff that's different. Key
 *                       shortcuts now save and load to the registry... Also
 *                       added keyboard mappings importing, and a Visual Studio
 *                       mappings file.
 *            11/02/2001 Fixed CheckAlreadyOpen to correctly identify two
 *                       different files with the same filename part. Added file
 *                       changed notification to PN. Moved startup file opening
 *                       code to Application Activate.
 *            08/03/2001 Fixed "Close All" crashes. Added Open All in Project.
 *                       Added images to projects context menu. Far better error
 *                       handling on open and save files - GetLastError aware.
 *            09/03/2001 Text Clips can now be loaded from the schemes directory
 *                       as well as the clips dir. Added an icon to the editor
 *                       window - just looks a bit nicer. Added toolbar masking
 *                       popups to the helper windows which aren't projects ones.
 *            01/04/2001 Added Mozilla Control Detection to the Options dialog
 *                       using Reg checks. Added a file browser to the helper.
 *                       Added another crash prevention method to editor. 
 *            29/04/2001 Unsaved text is now transfered between normal and
 *                       hex editing modes. Added File Delete. Moved All Files
 *                       to second item in open filter. Added %d constant
 *                       evaluation for tools.
 *            02/08/2001 Did stuff to make file opening from outside work. Hope.
 *            23/10/2001 Ok, so it looks like I've not worked on this for ages.
 *                       I'm sure I have so never mind... Made some modifications
 *                       to the file opening code in the editor window which
 *                       makes the edit control select the parser before loading
 *                       text, much better that way. Also added TabsToSpaces and
 *                       SpacesToTabs items to the Edit menu. These work using
 *                       regexp replace all functionality.
 *            19/01/2002 Time to finish off PN1 methinks. Fixed the geticons.inc
 *                       source to ignore Microsofts IconHandler system
 *                       where DefaultIcon is set to %1. I can't be bothered to
 *                       write the COM code to get an icon from an iconhandler.
 *                       It would also appear that the options dialog for
 *                       TSyntaxMemo is incompatible with the Visual Styles
 *                       stuff in Windows XP. Therefore, it is not recommended
 *                       to use visual styles with PN1.
 *            24/01/2002 Fixed Cut and Copy with the hex editor, they were
 *                       dropping the end character.
 *            09/05/2002 Fixed the file opening from the command line stuff which
 *                       should now allow PN to act as a complete notepad
 *                       replacement. Also fixed file age checking to check that
 *                       the file exists first. Also stopped PN from crashing on
 *                       reverting a file that has been deleted, and made sure that
 *                       the pn window is shown before asking the user if they want
 *                       to reload a changed file.
 **************************************************************}

{$DEFINE plugin}
{$DEFINE bookmark}
{$DEFINE parsers}
{$DEFINE results}

unit main;

{ TODO -oSimon -cScheme Functionality : Allow reset of colour schemes... }
{ TODO -oSimon -cVisual : Clear StatusBar when all files closed }

// Enable this define to write a .key file in the Programmers Notepad directory
// with the currently used keyboard shortcuts in it on program close.
{.$DEFINE WRITEKEYSHORTCUTS}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, ImgList, Menus, ActnList, WebFileInfo,
  MRUFList, editor, SyntaxEd, SynParse, Registry, ShellAPI, StdActns,
  pntypes, Logwiz, pndefs, HexEditor, plgiface, TB97, TB97Tlwn, bkmrkmru,
  parserrepos, shlobj, TB97Tlbr, TB97Ctls, EnhCBox, INIFiles,
  StdCtrls, AgOpenDialog, Buttons, Options, GrepResultsDlg,
  FindDlg, ReplDlg, helper, hh, hh_funcs;

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
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    PrinterSetup1: TMenuItem;
    N5: TMenuItem;
    Exit1: TMenuItem;
    mnuEdit: TMenuItem;
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
    mnuSearch: TMenuItem;
    itmFind: TMenuItem;
    itmFindNext: TMenuItem;
    itmReplace: TMenuItem;
    itmUseRegex: TMenuItem;
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
    N12: TMenuItem;
    itmPreviewHTML: TMenuItem;
    itmHexView: TMenuItem;
    Tools1: TMenuItem;
    SelectTag1: TMenuItem;
    SelectEnclosingBlock1: TMenuItem;
    N10: TMenuItem;
    itmWebUpdate: TMenuItem;
    SysEdit1: TMenuItem;
    sepExecute: TMenuItem;
    Preferences1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ilsHot: TImageList;
    StatusBar1: TStatusBar;
    PrinterSetupDialog1: TPrinterSetupDialog;
    wfInfo: TWebFileInfo;
    Timer1: TTimer;
    FontDialog1: TFontDialog;
    actNew: TAction;
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
    panBack: TPanel;
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
    actPrintPreview: TAction;
    itmPrintPreview: TMenuItem;
    actContents: TAction;
    Contents1: TMenuItem;
    N14: TMenuItem;
    actHexGoto: TAction;
    sepHex: TMenuItem;
    itmHexGoto: TMenuItem;
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
    actGrid: TAction;
    Grid1: TMenuItem;
    actShowMarkers: TAction;
    ShowMarkers1: TMenuItem;
    tmrStartup: TTimer;
    actSaveAll: TAction;
    SaveAll1: TMenuItem;
    actPasteFromFile: TAction;
    PasteFromFile1: TMenuItem;
    popToolbar: TPopupMenu;
    pitFile: TMenuItem;
    pitEdit: TMenuItem;
    actTbrMain: TAction;
    actTbrEdit: TAction;
    Toolbars1: TMenuItem;
    File2: TMenuItem;
    Edit2: TMenuItem;
    itmBookmarks: TMenuItem;
    FloatBookmarks: TToolWindow97;
    panBookmarks: TPanel;
    lstBookmarks: TListView;
    dokRight: TDock97;
    dokLeft: TDock97;
    dokBottom: TDock97;
    itmResults: TMenuItem;
    sepFIF: TMenuItem;
    itmFileFind: TMenuItem;
    MRUFileList1: TdfsMRUFileList;
    Octal1: TMenuItem;
    itmHexTranslation: TMenuItem;
    ANSI1: TMenuItem;
    Dos81: TMenuItem;
    Ascii1: TMenuItem;
    Mac1: TMenuItem;
    EBCDIC1: TMenuItem;
    actAnsiTranslate: TAction;
    actAsciiTranslate: TAction;
    actDosTranslate: TAction;
    actMacTranslate: TAction;
    actEBCDICTranslate: TAction;
    actOctalOffset: TAction;
    popResults: TPopupMenu;
    ClearSearchResults1: TMenuItem;
    N9: TMenuItem;
    Bugger1: TMenuItem;
    tbrMain: TToolbar97;
    btnNew: TToolbarButton97;
    ToolbarButton972: TToolbarButton97;
    ToolbarButton973: TToolbarButton97;
    ToolbarButton974: TToolbarButton97;
    tbrEdit: TToolbar97;
    ToolbarButton975: TToolbarButton97;
    ToolbarButton976: TToolbarButton97;
    ToolbarButton977: TToolbarButton97;
    ToolbarSep971: TToolbarSep97;
    ToolbarButton978: TToolbarButton97;
    ToolbarSep972: TToolbarSep97;
    ToolbarButton979: TToolbarButton97;
    ToolbarButton9710: TToolbarButton97;
    ToolbarSep973: TToolbarSep97;
    btnGotoBookmark: TToolbarButton97;
    btnSetBookmark: TToolbarButton97;
    ToolbarSep974: TToolbarSep97;
    ToolbarButton9713: TToolbarButton97;
    dokTop: TDock97;
    ClockTimer: TTimer;
    itmClock: TMenuItem;
    actClock: TAction;
    ToolbarButton971: TToolbarButton97;
    ToolbarSep975: TToolbarSep97;
    tbrFolders: TToolbar97;
    cmbFolders: TImgComboBox;
    Folders1: TMenuItem;
    actTbrFolders: TAction;
    Folders2: TMenuItem;
    actReverseCase: TAction;
    N4: TMenuItem;
    ReverseCase1: TMenuItem;
    actExecute: TAction;
    popExecute: TPopupMenu;
    actPrintMargin: TAction;
    PrintMargin1: TMenuItem;
    actJumpToLine: TAction;
    sepJump: TMenuItem;
    itmGotoLine: TMenuItem;
    actBookmarkWindow: TAction;
    N8: TMenuItem;
    itmExecute: TMenuItem;
    itmNoneSet2: TMenuItem;
    N13: TMenuItem;
    actCloseAll: TAction;
    CloseAll1: TMenuItem;
    actSaveWithFormatting: TAction;
    SavewithFormatting1: TMenuItem;
    actSetDefaultBookmark: TAction;
    actJumpDefaultBookmark: TAction;
    Indentation1: TMenuItem;
    Indent1: TMenuItem;
    Unindent1: TMenuItem;
    OpenDialog1: TAgOpenDialog;
    SaveDialog2: TAgSaveDialog;
    SaveDialog1: TAgSaveDialog;
    dlgPasteFrom: TAgOpenDialog;
    dlgSaveError: TAgSaveDialog;
    tbrSearch: TToolbar97;
    cmbSearch: TEnhComboBox;
    itmQuickSearch: TMenuItem;
    actTbrSearch: TAction;
    QuickSearch1: TMenuItem;
    actQuickSearchSelect: TAction;
    Tabs1: TMenuItem;
    ToolbarButton9711: TToolbarButton97;
    ToolbarSep976: TToolbarSep97;
    ToolbarSep977: TToolbarSep97;
    ToolbarSep978: TToolbarSep97;
    ToolbarButton9712: TToolbarButton97;
    ToolbarButton9714: TToolbarButton97;
    ToolbarButton9715: TToolbarButton97;
    ToolbarSep979: TToolbarSep97;
    actFindInFiles: TAction;
    FloatFIF: TToolWindow97;
    panFIFContainer: TPanel;
    alsWin2k: TActionList;
    actw2kTransparentBookmarks: TAction;
    actBMAdd: TAction;
    actBMRemove: TAction;
    panBMToolbar: TPanel;
    ToolbarButton9716: TToolbarButton97;
    ToolbarButton9717: TToolbarButton97;
    btnBookmarksTransparent: TToolbarButton97;
    actw2kTransparentFindInFiles: TAction;
    actFIFResults: TAction;
    actRevert: TAction;
    Revert1: TMenuItem;
    popBrowser: TPopupMenu;
    tabs: TTabControl;
    FloatHelper: TToolWindow97;
    panPrjContainer: TPanel;
    Helper1: TMenuItem;
    actHelper: TAction;
    actIndex: TAction;
    Index1: TMenuItem;
    actHelpSearch: TAction;
    Search2: TMenuItem;
    actWhatIs: TAction;
    btnContextHelp: TToolbarButton97;
    ToolbarSep9710: TToolbarSep97;
    ToolbarButton9718: TToolbarButton97;
    actQuickFindInFiles: TAction;
    sepVS1: TMenuItem;
    sepVS2: TMenuItem;
    actDeleteFile: TAction;
    N6: TMenuItem;
    DeleteFile1: TMenuItem;
    actViewAsHTML: TAction;
    ViewasHTML1: TMenuItem;
    TabsandSpaces1: TMenuItem;
    itmTabsToSpaces: TMenuItem;
    itmSpacesToTabs: TMenuItem;
    actTabsToSpaces: TAction;
    actSpacesToTabs: TAction;
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
    procedure mnuEditClick(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure actWordWrapExecute(Sender: TObject);
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
    procedure actGridExecute(Sender: TObject);
    procedure mnuViewClick(Sender: TObject);
    procedure actShowMarkersExecute(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
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
    procedure actAnsiTranslateExecute(Sender: TObject);
    procedure itmHexTranslationClick(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure ClockTimerTimer(Sender: TObject);
    procedure actClockExecute(Sender: TObject);
    procedure cmbFoldersChange(Sender: TObject);
    procedure actReverseCaseExecute(Sender: TObject);
    procedure actPrintMarginExecute(Sender: TObject);
    procedure actJumpToLineExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure actSaveWithFormattingExecute(Sender: TObject);
    procedure actSetDefaultBookmarkExecute(Sender: TObject);
    procedure actJumpDefaultBookmarkExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure cmbSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbSearchEnter(Sender: TObject);
    procedure actQuickSearchSelectExecute(Sender: TObject);
    procedure cmbSearchExit(Sender: TObject);
    procedure cmbSearchKeyPress(Sender: TObject; var Key: Char);
    procedure actFindInFilesExecute(Sender: TObject);
    procedure actw2kTransparentBookmarksExecute(Sender: TObject);
    procedure FloatBookmarksDockChanged(Sender: TObject);
    procedure actFIFResultsExecute(Sender: TObject);
    procedure actRevertExecute(Sender: TObject);
    procedure tabsResize(Sender: TObject);
    procedure tabsDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure actHelperExecute(Sender: TObject);
    procedure actHelpSearchExecute(Sender: TObject);
    procedure actIndexExecute(Sender: TObject);
    procedure actWhatIsExecute(Sender: TObject);
    procedure actQuickFindInFilesExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure FloatHelperRecreated(Sender: TObject);
    procedure actViewAsHTMLExecute(Sender: TObject);
    procedure actTabsToSpacesExecute(Sender: TObject);
    procedure actSpacesToTabsExecute(Sender: TObject);
  private
    { Private declarations }
    Flag             : Integer;
    First            : Boolean;
    FirstMore        : Boolean;
    FLastFindText    : string;
    FLastFindOptions : longint;
    SetFiles         : Boolean;
    savefiltermax    : Integer;
    ClockShowing     : boolean;
    _enteroverride   : Boolean;
    ////////////////////////////////////////////////////////////////////////////
    // Plugin Related
    ////////////////////////////////////////////////////////////////////////////
    {$IFDEF plugin}
    PluginInit       : tPluginInit;
    PlgMsgNum        : tPlgMsgNum;
    PlgStrResult     : tStrResult;
    {$ENDIF}
    mHHelp           : THookHelpSystem;
    fTempFiles       : TStringList;
    procedure Setup;
    procedure ToolLaunch(Sender: TObject);
    function NewType(Ext : String) : TfrmClient; overload;
    function NewType(Ext : Integer; iCaption : string) : TfrmClient; overload;
    function GetFilterIndex(Ext: String): Integer;
    procedure Setpreview;
    procedure HandleHint(Sender: TObject);
    procedure DoSetFilesFirst;
    procedure SelectHTMLTag(SelectEnclosingBlock: Boolean);
    procedure ChangeTabPos(Pos: integer);
    procedure HexButtons;
    procedure SaveToolbars;
    procedure LoadToolbars;
    function GenSaveFilter: string;
    function GenOpenFilter: string;
    procedure UpdateTypes;
    procedure AddHighlightMenuItem(Sender : tObject; ftag : integer);
    procedure UpdateNew;
    procedure AddNewItem(iCaption: string; iTag: Integer;
      iOnClick: tNotifyEvent; nShortcut : Boolean);
    procedure SetStartDir(which : integer; sDir : string);
    {$IFDEF plugin}
    procedure CheckPlugins;
    {$ENDIF}
    procedure ToolExecute(Sender: tObject);
    procedure RunIt(App, Parms, Dir : string; AskParms, Capture : Boolean); overload;
    procedure RunIt(App, Parms, Dir : string; AskParms : Boolean); overload;
    function ParseInData(DataType: tParseDataType; var input: string): boolean;
    procedure ExecuteTool(Indx: Integer);
    procedure Execute(Scheme : string; Indx : Integer);
    procedure ConvertTools;
    procedure LoadTools;
    function DoParam(parm : string) : Boolean;
    procedure MyFindText(fEditor: tSyntaxMemo; fFindText: string);
    procedure BrowserMenuHandler(Sender: TObject);
    procedure PreviewWithIE;
    procedure FormFocus(Sender: TObject);
    procedure FormLostFocus(Sender: TObject);
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure PreviewWithMozilla;
    procedure VisualStudioMenus;
    procedure SaveKeyShortcuts;
    procedure LoadKeyShortcuts;
    procedure ExpandConstants(var str: string);
    function GetPreviewFileName(ed: TfrmClient): string;
    procedure ProcessCommandLine;
  protected
    procedure COPYDATA(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMHelp(var Message : TWMHelp); message WM_HELP;
  public
    { Public declarations }
    ////////////////////////////////////////////////////////////////////////////
    // Warnings
    ////////////////////////////////////////////////////////////////////////////
    _wnOpenFileAgain : Integer;
    ////////////////////////////////////////////////////////////////////////////
    // Other Variables
    ////////////////////////////////////////////////////////////////////////////
    _doingcloseall   : Boolean;
    ClockFormat     : string;
    OpenOnStart     : TStringList;
    fStarted        : Boolean;
    SEFiles         : tSysEditFiles;
    MsgNum          : Integer;
    OpenFilesMaxed  : Boolean;
    TypeName        : TStringList;
    TypeIndex       : TStringList;
    TypeFilter      : TStringList;
    TypeUnix        : TStringList;
    TypeHex         : TStringList;
    NewTitles       : TStringList;
    NewParsers      : TStringList;
    BookmarkMRU     : tBookmarkMRU;
    Parsers         : tParsers;
    DoURLS          : Boolean;
    LoseUndo        : Boolean;
    Plugins         : TPlugins;
    PluginMenuItems : tList;
    Opt             : tfrmOptions;
    Grep            : tfrmGrepResults;
    frmHelper       : TfrmHelper;
    Win2k           : Boolean;
    TransAvail      : Boolean;
    Fnd             : TfrmFind;
    Rep             : TfrmReplace;
    QSKeepFocus     : Boolean;
    HighlightActive : Boolean;
    HighlightChange : Boolean;
    HighlightColor  : tColor;
    ModifiedColor   : TColor;
    LastUsed        : string;
    function CheckAlreadyOpen(fname: string): Boolean;
    procedure MenuItemClick(Sender: TObject);
    procedure CloseFile;
    procedure CloseAll;
    procedure OpenFile(fname, ext: String);
    procedure OpenFilePos(fname, ext : string; cPos: TPoint);
    procedure EnableButtons;
    procedure DefaultHandler(Var Message); override;
    procedure DropFiles(var msg: TWmDropFiles); message wm_dropfiles;
    procedure ReloadTools;
    procedure UpdateStatusBar;
    procedure UpdateToolsFile(IFile : tIniFile);
    function NewFilterIndex(FileName: String): Integer;
    function GetCurrentEditor: TfrmClient;
    procedure ShowText(const Text: String);
    procedure SetEditor;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure UpdateDialogs;
    function GetFileType(Ext: string): integer;
    procedure RefreshBookmarkList;
    function GetSaveAsUnix(Ext: string): boolean;
    function SchemePath : string;
    function GetOpenAsHex(Ext: string): boolean;
    function Add_NewItem(iCaption, iTitle : string; iScheme : integer) : boolean;
    function Add_NewType(iNewType : tNewType; iScheme : integer) : boolean;
    function Add_NewAss(iAss: string): boolean;
    procedure SetStatus(Text: string);
    procedure SetClock(onoff : boolean);
    procedure URL(urlstr : string);
    function GetSetting(fname, key : string; default : boolean) : Boolean; overload;
    function GetSetting(fname, key : string; default : string) : string; overload;
    procedure SetSetting(fname, key: string; Value: Boolean);
    procedure SetHighlightMenu(Sender: TObject);
    procedure UpdateEditors;
    function AppHandle: tHandle;
    procedure UpdateFolders;
    procedure NewFromTemplate(Scheme: integer; Filename: string);
    procedure SaveAs(FEditor: tfrmClient);
    procedure LoadSchemeTools(scheme: string);
    procedure ClearSchemeTools;
    procedure SetResults(res : string);
    procedure AddResult(res : string);
    procedure ClearResults;
    procedure UpdateBrowsers;
  end;

var
  frmMain: TfrmMain;

Const
  ProgTitle  = 'Programmers Notepad (PN)';
  MainTitle  = 'PN';
  ToolOffSet = 7;
  StatPanel  = 3;
  TimePanel  = 4;
  ToolsMenu  = 4;
  ToolsVers  = 2;
  BrowserVer = 2;
  SEKey      = 'Software\Echo Software\PN\SysEdit';
  RootKey    = 'Software\Echo Software\PN';
  NetKey     = 'Software\Echo Software\PN\Net';
  ToolKey    = 'Software\Echo Software\PN\Toolbars';
  ToolKey2   = 'Software\Echo Software\PN\Toolbars 2';
  EditKey    = 'Software\Echo Software\PN\Editor';
  QSKey      = 'Software\Echo Software\PN\MRU\QuickSearch';
  GrepKey    = 'Software\Echo Software\PN\Grep';
  KeybKey    = 'Software\Echo Software\PN\Keyboard';
  newstr     = '<new>';

implementation

uses Useful, Clipbrd, preview, genfuncs, welcome, About, print,
     pff, execute;

const
  HTMLTagLetters     = ['a'..'z', 'A'..'Z', '!'];
  OPENAGAIN = 0;
  REVERT = 1;
  JUSTOPEN = 2;
  FRONT = 3;

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
   NewType(0, '<new>');
end;

procedure TfrmMain.DefaultHandler(var Message);
var
   Buffer : Array[0..255] of Char;
   s : String;
begin
   Inherited DefaultHandler(Message);
   With tMessage(Message) Do
   If integer(Msg) = MsgNum Then
   Begin
      OutputDebugString('Doing DefaultHandler');
      //ShowMessage('Argh!');
      If GlobalGetAtomName(lParam, Buffer, 255) = 0 Then
         StrCopy(Buffer, 'Error!');
      s := StrPas(Buffer);
      If Length(s) > 0 Then
      Begin
          if fStarted then
          begin
            OutputDebugString('fStarted was true.');
            If Copy(s, 0, 1) <> '/' Then
            Begin
               if Pos('\', s) = 0 then
                  s := ExpandFileName(s);
               OpenFile(s, ExtractFileExt(s));
            end else
              DoParam(s);
          end else
          begin
             OutputDebugString('fStarted was false.');
             If not Assigned(OpenOnStart) Then OpenOnStart := tStringList.Create;
             if Pos('\', s) = 0 then
                s := ExpandFileName(s);
             if OpenOnStart.IndexOf(s) < 0 then
                OpenOnStart.Add(s);
          end;
      End;
   end;
end;

function UsingWinNT: Boolean;
{ Returns True if system is running any version of Windows NT. Never returns
  True on Windows 95 or 3.1. }
begin
  {$IFNDEF WIN32}
  Result := GetWinFlags and $4000{WF_WINNT} <> 0;
  {$ELSE}
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  {$ENDIF}
end;

procedure TfrmMain.Setup;
var
  Registry : tRegistry;
  i1,
  i2,
  i3       : Integer;
  dschemes : string;
  dir      : integer;
  sdir     : string;
begin
  if not FileExists(ExtractFilePath(ParamStr(0)) + 'files.def') then
    CreatePNFile(ExtractFilePath(ParamStr(0)) + 'files.def', strFileTypes);
  {Are we running Win2k?}
  Win2k := UsingWinNT and (Lo(GetVersion) >= 5);
  TransAvail := Win2k;
  for i1 := 0 to alsWin2k.ActionCount - 1 do
  begin
    TAction(alsWin2k.Actions[i1]).Visible := TransAvail;
  end;
  UpdateDialogs;
  UpdateBrowsers;
  sdir := '';
  dir := 0;
  i3 := 1;
  // Set up the Search combo box.
  cmbSearch.Items.Clear;
  cmbSearch.Text := 'Search...';
  cmbSearch.Tag := 1;
  // Set up the schemes directory - if it doesn't exist, then Doh!
  dschemes := extractfilepath(ParamStr(0)) + 'Schemes\';
  If not DirExists(dschemes) then CreateDir(dschemes);
  Parsers := tParsers.Create(Self);
  Parsers.HomeDir := dschemes;
  Parsers.Refresh;
  SetStatus( inttostr(Parsers.Count) + ' Schemes Loaded...');
  // Set the form positioning...
  If GetSize('Echo Software', 'PN', i1, i2) Then
  Begin
    SetBounds(Left, Top, i2, i1);
  end;
  WindowState := GetStat('Echo Software', 'PN');
  If WindowState = wsNormal then
  Begin
    GetPosition('Echo Software', 'PN', Height, Width,
                i1, i2);
    SetBounds(i2, i1, Width, Height);
  End;
  ConvertTools;
  LoadTools;
  LoadSchemeTools('');
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
    // Write the app location to the registry, for setup programs to find...
    Registry.WriteString('Location', ExtractFilePath(ParamStr(0)));
    try actTabs.Checked := Registry.ReadBool('ShowTabs'); except actTabs.Checked := True; end;
    try actWordWrap.Checked := Registry.ReadBool('WordWrap'); except actWordWrap.Checked := False; end;
    try i3 := Registry.ReadInteger('TabPos'); except i3 := 1; end;
    try OpenFilesMaxed := Registry.ReadBool('OpenMaxed'); except OpenFilesMaxed := False; end;
    try MRUFileList1.Maximum := Registry.ReadInteger('MRUMax'); except MRUFileList1.Maximum := 5; end;
    try dir := Registry.ReadInteger('StartDir'); except dir := 0; end;
    try ClockFormat := Registry.ReadString('ClockFormat'); except ClockFormat := 'hh:mm'; end;
    try actClock.Checked := Registry.ReadBool('ShowClock'); except actClock.Checked := false; end;
    try actPrintMargin.Checked := Registry.ReadBool('PrintMargin'); except actPrintMargin.Checked := true; end;
    try actRegExp.Checked := Registry.ReadBool('UseRegExp'); except actRegExp.Checked := False; end;
    try LoseUndo := Registry.ReadBool('ClearUndo'); except LoseUndo := True; end;
    try QSKeepFocus := Registry.ReadBool('QSKeepFocus'); except QSKeepFocus := False; end;
    try tabs.MultiLine := Registry.ReadBool('MultiLineTabs'); except tabs.MultiLine := False; end;
    try
      HighlightActive := Registry.ReadBool('HighlightActive');
      HighlightColor := Registry.ReadInteger('HighlightColor');
    except
      HighlightActive := True;
      HighlightColor := clBlue;
    end;
    try HighlightChange := Registry.ReadBool('HighlightChange'); except HighlightChange := True; end;
    try ModifiedColor := Registry.ReadInteger('ModifiedColor'); except ModifiedColor := clRed; end;
    try _wnOpenFileAgain := Registry.ReadInteger('WarnOpenAgain'); except _wnOpenFileAgain := OPENAGAIN; end;
    try SetFiles := Registry.ReadBool('First'); except SetFiles := True; end;
    try
      if Registry.ValueExists('VSMenus') then
      begin
        if Registry.ReadBool('VSMenus') then
          VisualStudioMenus;
      end;
    except
    end;
    If ClockFormat = '' then ClockFormat := 'hh:mm';
    SetClock(actClock.Checked);
    ClockTimerTimer(Self);
    If dir = 2 then
      try sdir := Registry.ReadString('CustomStartDir'); except sdir := extractfilepath(paramstr(0)); end
    else
    If dir = 1 then
      try sdir := Registry.ReadString('LastDir'); except sdir := extractfilepath(paramstr(0)); end;
  except
  end;
  LoadToolbars;
  LoadKeyShortcuts;
  FloatFIF.Visible := False;
  If FloatHelper.Visible then
  begin
    actHelperExecute(Self);
  end;
  try Registry.CloseKey except end;
  Registry.OpenKey(QSKey, True);
  if Registry.ValueExists('MRU') then
  begin
   for i1 := 1 to Registry.ReadInteger('MRU') do
   begin
     cmbSearch.Items.Add(lowercase(Registry.ReadString('mrui' + IntToStr(i1))));
   end;
  end;
  try Registry.CloseKey except end;
  Registry.OpenKey(NetKey, True);
  try DoURLs := Registry.ReadBool('LaunchURLs'); except DoURLs := True; end;
  Registry.CloseKey;
  Registry.Free;
  wfInfo.CurrentVersion := GetFileInformation(ParamStr(0), 'FileVersion');

  ChangeTabPos(i3);

  SetStartDir(dir, sdir);

  {$IFDEF plugin}
  CheckPlugins;
  {$ENDIF}

  UpdateFolders;
end;

procedure TfrmMain.ConvertTools;
var ri : tRegIniFile;
    ini : tIniFile;
    s   : ShortString;
    n   : integer;
begin
  ri := tRegIniFile.Create('Software\Echo Software\PN');
  ini := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  try
    for n := 1 to ri.ReadInteger('Tools', 'CustomNumber', 0) do
    begin
      s := IntToStr(n);
      ini.WriteString('Menu', 'Desc' + s,
                      ri.ReadString('Tools', 'Desc' + s, ''));
      ini.WriteString('Menu', 'App' + s,
                      StringToCString(ri.ReadString('Tools', 'Command' + s, 'Error')));
      ini.WriteString('Menu', 'Par' + s,
                      StringToCString(ri.ReadString('Tools', 'Parm' + s, '')));
      ini.WriteString('Menu', 'Dir' + s, '');
      ini.WriteBool('Menu', 'Ask' + s, False);
      ini.WriteBool('Menu', 'Cap' + s, False);
      ini.WriteString('Menu', 'Kyb' + s, '');
      ini.WriteInteger('Menu', 'Number', n);
    end;
    ri.WriteInteger('Tools', 'CustomNumber', 0);
  finally
    ri.Free;
    ini.Free;
  end;
end;


procedure TfrmMain.LoadTools;
var Settings : tIniFile;
    ofs : integer;
    n   : integer;
    m   : TMenuItem;
    i   : integer;
    k   : string;
begin
  ofs := 0;
  {Get tools position offset by checking for plugin items.}
  with MainMenu1.Items[ToolsMenu] do
    for n := 0 to Count - 1 do
      if Items[n].Tag >= 80 then inc(ofs);
  Settings := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  Try
    if Settings.ReadInteger('Version', 'FileVersion', 1) <> ToolsVers then
       UpdateToolsFile(Settings);
    With Settings Do
    Begin
      i := ReadInteger('Menu', 'Number', 0);
      If i > 0 Then
      Begin
        For n := 1 to i do
        Begin
          m := tMenuItem.Create(MainMenu1);
          m.Caption := ReadString('Menu', 'Desc' + inttostr(n), 'Error');
          m.Tag := n + 20;
          k := ReadString('Menu', 'Kyb' + IntToStr(n), '');
          if (k <> '') and (Length(k) > 0) then
            m.ShortCut := TextToShortCut(k);
          m.OnClick := ToolLaunch;
          MainMenu1.Items[ToolsMenu].Insert(ToolOffset + ofs + n-1, m);
        End;
        m := tMenuItem.Create(MainMenu1);
        m.Caption := '-';
        m.Tag := 19;
        MainMenu1.Items[ToolsMenu].Insert(ToolOffset + ofs + i, m);
      End;
    End;
  Finally
    Settings.Free;
  End;
end;

procedure TfrmMain.ReloadTools;
var
  n : integer;
Begin
  with MainMenu1.Items[ToolsMenu] do
    For n := Count - 1 downto 0 do
      If (Items[n].Tag > 18) and (Items[n].Tag < 80) Then
        Delete(Items[n].MenuIndex);
  LoadTools;
  if Assigned(frmMain.ActiveMDIChild) then
    LoadSchemeTools(TSyntaxMemoParser(TfrmClient(frmMain.ActiveMDIChild).synMDI.Parser1).UI_Styles.LangName)
  else
    LoadSchemeTools('');
end;

procedure TfrmMain.ToolLaunch(Sender: TObject);
begin
   Execute('Menu', tMenuItem(Sender).tag - 20);
end;

function TfrmMain.NewType(ext : string) : TfrmClient;
var Editor : tfrmClient;
    tempstr : String;
Begin
   Editor := tfrmClient.Create(Self);
   Result := Editor;
   if OpenFilesMaxed then Editor.WindowState := wsMaximized;
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

function TfrmMain.NewType(Ext : Integer; iCaption : string) : TfrmClient;
var Editor : tfrmClient;
    tempstr : String;
Begin
   Editor := tfrmClient.Create(Self);
   Result := Editor;
   if OpenFilesMaxed then Editor.WindowState := wsMaximized;
   Editor.Execute(ext, iCaption);
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

procedure TfrmMain.NewFromTemplate(Scheme : integer; Filename : string);
var Editor : tfrmClient;
    ts     : string;
begin
  Editor := tfrmClient.Create(Self);
  Editor.Template := True;
  if OpenFilesMaxed then Editor.WindowState := wsMaximized;
  Editor.Execute(filename, ExtractFileExt(filename));
  Editor.Filename := newstr;
  Editor.Caption := newstr;
  Editor.synMDI.Modified := false;
  Editor.SetParser(Scheme);
  ts := Editor.Filename;
  tabs.Tabs.AddObject(ExtractFileName(ts), Editor);
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

function TfrmMain.CheckAlreadyOpen(fname : string) : Boolean;
var n : integer;
    //s1 : string;
    //s2 : string;
begin
  {
    OPENAGAIN = 0;
    REVERT = 1;
    JUSTOPEN = 2;
  }
  result := false;
  for n := 0 to frmMain.MDIChildCount - 1 do
  begin
    //s1 := uppercase(ExtractShortPathName(tfrmClient(frmMain.MDIChildren[n]).FileName));
    //s2 := uppercase(ExtractShortPathName(fname));
    if {s1} uppercase(ExtractShortPathName(tfrmClient(frmMain.MDIChildren[n]).FileName)) = {s2} uppercase(ExtractShortPathName(fname)) then result := true;
    If result then
    begin
      If _wnOpenFileAgain = OPENAGAIN then
      begin
        If MessageDlg('You are trying to open another copy of this file! Continue?', mtWarning, [mbYes, mbNo], 0) = mrYes then Result := False;
      end
      else if _wnOpenFileAgain = FRONT then
      begin
        TfrmClient(MDIChildren[n]).BringToFront;
      end
      else if _wnOpenFileAgain = REVERT then
      begin
        TfrmClient(MDIChildren[n]).synMDI.LoadFromFile(fname);
        TfrmClient(MDIChildren[n]).BringToFront;
      end else
        Result := False; //JUSTOPEN
      Break;
    end;
  end;
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
  If CheckAlreadyOpen(fname) then Exit;
  If ext = '' Then If fname <> '' Then
     ext := ExtractFileExt(fname);
  //!todo this line below is probably why we can't do this in an OnActivate or
  //an OnCreate event!!!
  //Application.ProcessMessages;
  Editor := TfrmClient.Create(Self);
  if OpenFilesMaxed then Editor.WindowState := wsMaximized;
  Editor.Execute(fname, Ext);
  fname := Editor.FileName;
  tabs.Tabs.AddObject(ExtractFileName(fname), Editor);
  tabs.TabIndex := tabs.Tabs.Count - 1;
  SetEditor;
  SetPreview;
  EnableButtons;
  Editor.synMDI.Font.Name := Config.Fontname;
  Editor.synMDI.Font.Size := Config.FontSize;
End;

procedure TfrmMain.OpenFilePos(fname, ext : string; cPos: TPoint);
var Editor: TfrmClient;
Begin
  if fname <> '' then
    if not FileExists(fname) then
      Begin
         MessageDlg(fname + ' does not exist, it could not be opened',mtError,[mbOK],0);
         Exit;
      End;
  If CheckAlreadyOpen(fname) then Exit;
  If ext = '' Then If fname <> '' Then
     ext := ExtractFileExt(fname);
  //!todo this line below is probably why we can't do this in an OnActivate or
  //an OnCreate event!!!
  //Application.ProcessMessages;
  Editor := TfrmClient.Create(Self);
  if OpenFilesMaxed then Editor.WindowState := wsMaximized;
  Editor.Execute(fname, Ext);
  fname := Editor.FileName;
  tabs.Tabs.AddObject(ExtractFileName(fname), Editor);
  tabs.TabIndex := tabs.Tabs.Count - 1;
  SetEditor;
  SetPreview;
  EnableButtons;
  Editor.synMDI.Font.Name := Config.Fontname;
  Editor.synMDI.Font.Size := Config.FontSize;
  Editor.synMDI.CaretPosition.OffsetLocation := cPos;
end;

function TfrmMain.NewFilterIndex(FileName : String) : Integer;
var ps      : integer;
begin
   // Position in list of newtitles...
   ps := NewTitles.IndexOf(filename);
   // Get the parser used for that file...
   ps := strtoint(NewParsers[ps]);
   If ps <> 0 then
   // Get the index of the first item mapped to that parser...
     Result := StrToInt(TypeFilter[TypeIndex.IndexOf(inttostr(ps))])
   else
     Result := savefiltermax;
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
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  SaveAs(FEditor);
end;

procedure TfrmMain.SaveAs(FEditor : tfrmClient);
var
  Index   : Integer;
begin
  if FEditor = nil then exit;
  if pos('>', FEditor.Filename) = 0 Then
        SaveDialog1.FilterIndex := GetFilterIndex(ExtractFileExt(FEditor.FileName))
     Else
        SaveDialog1.FilterIndex := NewFilterIndex(FEditor.FileName);
  SaveDialog1.InitialDir := ExtractFilePath(FEditor.FileName);
  if SaveDialog1.Execute then
  Begin
     With FEditor do
     Begin
        FileName := SaveDialog1.FileName;
        SaveFile;
        Tabs.Tabs[Tabs.TabIndex] := ExtractFileName(Filename);
        Index := GetFileType(ExtractFileExt(FEditor.Filename));
        FEditor.SetParser(Index);
     End;
     SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.Filename);
     LastUsed := SaveDialog1.InitialDir;
     SaveDialog1.Filename := '';
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
var n : Integer;
begin
  if _doingcloseall then Exit;
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
  itmHexTranslation.Visible := False;
  itmHighlight.Visible    := True;
  itmBookmark.Visible     := True;
  itmBookmark1.Visible    := True;
  actJumpToLine.Visible   := True;
  sepJump.Visible         := True;
  sepNor1.Visible         := True;
  sepNor.Visible          := True;
  actPrintMargin.Visible  := True;
  actPaste.Enabled        := Clipboard.HasFormat(CF_TEXT);
  
  //actQuickFindInFiles.Enabled := cmbSearch.Tag = 0;
  if Tabs.Tabs.Count < 1 then
  Begin
     btnGotoBookmark.Enabled    := False;
     actJumpDefaultBookmark.Enabled := False;
     btnSetBookmark.Enabled     := False;
     actSetDefaultBookmark.Enabled := False;
     actBMAdd.Enabled           := False;
     actBMRemove.Enabled        := False;
     lstBookmarks.Enabled       := False;
     lstBookmarks.Items.Clear;
     {TActions}
     actExecute.Enabled         := False;
     actRevert.Enabled          := False;
     actJumpToLine.Enabled      := False;
     actDelete.Enabled          := False;
     actDeleteFile.Enabled      := False;
     actSelectAll.Enabled       := False;
     actCopy.Enabled            := false;
     actCut.Enabled             := False;
     actSave.Enabled            := False;
     actClose.Enabled           := False;
     actCloseAll.Enabled        := False;
     actSaveAs.Enabled          := False;
     actSaveAll.Enabled         := False;
     actPrint.Enabled           := False;
     actSaveWithFormatting.Enabled := False;
     actFind.Enabled            := False;
     actFindNext.Enabled        := False;
     actReplace.Enabled         := False;
     actViewHex.Enabled         := False;
     actIndent.Enabled          := false;
     actUnindent.Enabled        := False;
     actUndo.Enabled            := False;
     actRedo.Enabled            := False;
     actWordWrap.Enabled        := False;
     actUpperCase.Enabled       := False;
     actLowerCase.Enabled       := False;
     actReverseCase.Enabled     := False;
     actSelectTag.Enabled       := False;
     actSelectEnclosing.Enabled := False;
     actPreviewHTML.Enabled     := False;
     itmPreviewHTML.Enabled     := False;
     actPrint.Enabled           := False;
     actProperties.Enabled      := False;
     actPasteFromFile.Enabled   := False;
     actViewAsHTML.Enabled      := False;
     actTabsToSpaces.Enabled    := False;
     actSpacesToTabs.Enabled    := False;
     Exit;
  End;
  With GetCurrentEditor do
  Begin
    if Mode = emHex then
    begin
      HexButtons;
      Exit;
    end;
    btnGotoBookmark.Enabled    := True;
    btnSetBookmark.Enabled     := True;
    actJumpDefaultBookmark.Enabled := True;
    actSetDefaultBookmark.Enabled := True;
    actBMAdd.Enabled           := True;
    actBMRemove.Enabled        := True;
    lstBookmarks.Enabled       := True;
    actFind.Enabled            := (synMDI.Lines.Count>0);
    {TActions}
    actExecute.Enabled         := True;
    actRevert.Enabled          := (not IsNewFile);
    actJumpToLine.Enabled      := True;
    actProperties.Enabled      := True;
    actDelete.Enabled          := True;
    actDeleteFile.Enabled      := (not IsNewFile);
    actSelectAll.Enabled       := True;
    actPrint.Enabled           := True;
    actPasteFromFile.Enabled   := True;
    actViewHex.Enabled         := True;
    actCopy.Enabled            := (synMDI.SelLength>0);
    actCut.Enabled             := (synMDI.SelLength>0);
    actClose.Enabled           := True;
    actCloseAll.Enabled        := True;
    actSave.Enabled            := (Modified);
    actSaveAs.Enabled          := True;
    actSaveAll.Enabled := False;
    for n := 0 to tabs.tabs.Count - 1 do
    Begin
    if TfrmClient(Tabs.Tabs.Objects[n]).Modified then
      actSaveAll.Enabled := True;
    end;
    actPrint.Enabled           := True;
    actSaveWithFormatting.Enabled := True;
    actFind.Enabled            := (synMDI.Lines.Count > 0);
    actFindNext.Enabled        := (synMDI.Lines.Count > 0);
    actReplace.Enabled         := (synMDI.Lines.Count > 0);
    actIndent.Enabled          := (synMDI.SelLength > 0);
    actUnindent.Enabled        := (synMDI.SelLength > 0);
    actUndo.Enabled            := True;
    actRedo.Enabled            := True;
    actWordWrap.Enabled        := True;
    actUpperCase.Enabled       := (synMDI.SelLength>0);
    actLowerCase.Enabled       := (synMDI.SelLength>0);
    actReverseCase.Enabled     := (synMDI.SelLength>0);
    actSelectTag.Enabled       := (Parser = parsers.HTML);
    actSelectEnclosing.Enabled := (Parser = parsers.HTML);
    actPreviewHTML.Enabled     := (Parser = parsers.HTML);
    itmPreviewHTML.Enabled     := (Parser = Parsers.HTML);
    actViewAsHTML.Enabled      := True;
    actTabsToSpaces.Enabled    := True;
    actSpacesToTabs.Enabled    := True;
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
  ws := wsNormal;
  if FEditor <> nil then
  begin
    ws := fEditor.WindowState;
    FEditor.Close;
  end;
  // Why is this line here?
  Application.ProcessMessages;
  if Tabs.Tabs.Count = 0 then
  begin
    tabs.Visible := False;
    panBack.Visible := False;
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
    StatusBar1.Panels[2].Text := '';
  end;
  TabsChange(Self);
  if (GetCurrentEditor <> nil) then
    if ws = wsMaximized then GetCurrentEditor.WindowState := ws;
  EnableButtons;
end;

procedure TfrmMain.tabsChange(Sender: TObject);
begin
   if tabs.tabs.Count < 1 then Exit;
   tfrmClient(tabs.tabs.Objects[tabs.tabindex]).BringToFront;
   if actHelper.Checked then
   begin
     frmHelper.UpdateCurrentSelection(tabs.TabIndex);
   end;
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
       emNormal : if GetFocus = synMDI.Handle then
                      synMDI.CutToClipboard
                    else
                      SendMessage(GetFocus, WM_CUT, 0, 0);
       emHex    : begin
                    if GetFocus <> HexEditor.Handle then
                      SendMessage(GetFocus, WM_CUT, 0, 0)
                    else
                      with HexEditor do
                      begin
                        pCT := SelCount;
                        pPC := BufferFromFile ( Min ( SelStart , SelEnd ) , pCT );
                        SetCBText ( pPC , pCT+1 );
                        FreeMem ( pPC , pCT+1 );
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
       emNormal : if GetFocus = synMDI.Handle then
                      synMDI.CopyToClipboard
                    else
                      SendMessage(GetFocus, WM_COPY, 0, 0);
       emHex    : begin
                     if GetFocus <> HexEditor.Handle then
                       SendMessage(GetFocus, WM_COPY, 0, 0)
                     else
                       with HexEditor do
                       begin
                          pCT := SelCount;
                          pPC := BufferFromFile ( Min ( SelStart , SelEnd ) , pCT );
                          SetCBText ( pPC , pCT+1 );
                          FreeMem ( pPC , pCT+1 );
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
  if FEditor = nil then
  begin
    // If there's no file open, paste into a new file!
    NewType(0, '<new>');
    FEditor := GetCurrentEditor;
    FEditor.synMDI.PasteFromClipboard;
    exit;
  end else
  With FEditor do
  begin
    case Mode of
       emNormal : begin
                    if GetFocus = synMDI.Handle then
                      synMDI.PasteFromClipboard
                    else
                      SendMessage(GetFocus, WM_PASTE, 0, 0);
                  end;
       emHex    : begin
                     if GetFocus = HexEditor.Handle then
                     begin
                       sr := Clipboard.AsText;
                       sr := sr+#0;
                       HexEditor.ReplaceSelection ( @sr[1] , Length(sr)-1);
                     end else
                       SendMessage(GetFocus, WM_PASTE, 0, 0);
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

procedure TfrmMain.mnuEditClick(Sender: TObject);
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

{ Function to parse the command line in a more sensible manner - we only bother
 with quoted groups when quotes are present on the command line, else we
 assume one big filename. }
procedure TfrmMain.ProcessCommandLine;
var
  p : PChar;
  n : Integer;
begin
  p := CmdLine;

  // Skip the application name...
  if (p^ = '"') then
  begin
    Inc(p);
    while (p^ <> #0) and (p^ <> '"') do
      Inc(p);

    if(p^ = '"') then
      Inc(p);
  end;

  while (p^ <> #0) and (p^ <= ' ') do
    Inc(p);

  if (p^ <> #0) and (strlen(p) > 0) then
  begin
    if not Assigned(OpenOnStart) then
      OpenOnStart := TStringList.Create;

    if (p^ = '/') then
    begin
      // We have a command-line option
      if not DoParam(String(p)) then
      begin
        Application.ShowMainForm := False;
        Application.Terminate;
      end;
    end else
    begin
      if (strpos(p, '"') = nil) then
      begin
        // No quotes, treat as one filename...
        OpenOnStart.Add(String(p));
      end else
        For n := 1 to ParamCount do
        begin
          OpenOnStart.Add(ParamStr(n));
        end;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmHelper := nil;
  fStarted := false;
  _doingcloseall := False;
  Flag := 0;
  savefiltermax := 0;
  ClockFormat := 'hh:mm';
  FirstMore := True;
  DragAcceptFiles(frmMain.handle, true);
  TypeName  := TStringList.Create;
  TypeIndex := TStringList.Create;
  TypeFilter := TStringList.Create;
  TypeUnix := tStringlist.Create;
  TypeHex   := tStringlist.Create;
  NewTitles := tStringList.Create;
  NewParsers := tStringList.Create;
  fTempFiles := TStringList.Create;
  {$IFDEF plugin}
  Plugins := TPlugins.Create;
  PluginMenuItems := tList.Create;
  {$ENDIF}
  BookmarkMRU := tBookmarkMRU.Initialise;
  Application.OnHint := HandleHint;
  Application.OnException := ExceptionHandler;
  Application.OnActivate := FormFocus;
  Application.OnDeactivate := FormLostFocus;
  mHHelp := THookHelpSystem.Create(ExtractFilePath(ParamStr(0)) + 'pn.chm', '', htHHAPI);
  //mHHelp.HelpCallback2 := F1HelpEvent;
  logMain.GenFileName('pn', 'elg');
  logMain.AppName := 'PN';
  logMain.AppVersion := GetFileInformation(application.ExeName, 'FileVersion');
  First := True;
  //MessageBox(0, CmdLine, 'Programmers Notepad', MB_OK);
  ProcessCommandLine;
  (*For n := 1 to ParamCount do
      If (Length(ParamStr(n)) > 0) and (Copy(ParamStr(n), 1, 1) <> '/') Then
      Begin
         If not Assigned(OpenOnStart) Then OpenOnStart := tStringList.Create;
         OpenOnStart.Add(ParamStr(n));
      end else
      if (Copy(ParamStr(n), 1, 1) = '/') then
      begin
        if not DoParam(ParamStr(n)) then
        begin
          Application.ShowMainForm := False;
          Application.Terminate;
        end;
      end;*)
  Setup;
  Config := TConfigInfo.Create(nil);
end;

function TfrmMain.DoParam(parm : string) : Boolean;
var dir    : String; //Windows Directory
    Atom   : TAtom;
    Buffer : Array[0..255] of Char;

procedure AtomOpenFile(str : string);
begin
  Atom := 0;
  Atom := GlobalAddAtom(StrPCopy(Buffer, str));
  SendMessage(HWND_BROADCAST, MsgNum, Application.Handle, Atom);
  GlobalDeleteAtom(Atom);
end;

begin
  Result := True;
  if LowerCase(parm) = '/reguninstall' then
  begin
    UninstallAssoc;
    Result := False;
  end else
  if LowerCase(parm) = '/sysedit' then
  begin
    SetLength(dir, MAX_PATH);
    GetWindowsDirectory(pChar(dir), MAX_PATH);
    SetLength(dir, StrLen(pChar(dir)));
    If seAutoExec in SEFiles Then
      If FileExists('c:\autoexec.bat') Then AtomOpenFile('c:\autoexec.bat');
    If seConfig in SEFiles Then
      If FileExists('c:\config.sys') Then AtomOpenFile('c:\config.sys');
    If seWinIni in SEFiles Then
      If FileExists(dir + '\win.ini') Then AtomOpenFile(dir + '\win.ini');
    If seSysIni in SEFiles Then
      If FileExists(dir + '\system.ini') Then AtomOpenFile(dir + '\system.ini');
    If seProt in SEFiles Then
      If FileExists(dir + '\protocol.ini') Then AtomOpenFile(dir + '\protocol.ini');
  end else Exit;
end;

Procedure TfrmMain.SetEditor;
var FEditor : TfrmClient;
begin
   FEditor := GetCurrentEditor;
   UpdateStatusBar;
   if (not assigned(FEditor)) or (tabs.tabs.Count < 1) then
   Begin
      Application.Title := MainTitle;
      Caption := ProgTitle;
      exit;
   End;
   FEditor.BringToFront;
   Caption := ProgTitle;
   Application.Title := MainTitle + ' - ' + ExtractFileName(FEditor.FileName);
   Tabs.Repaint;
end;

procedure TfrmMain.Setpreview;
var FEditor : TfrmClient;
Begin
   FEditor := GetCurrentEditor;
   if not assigned(FEditor) then exit;
   if (frmPreview <> nil) then
      if (FEditor.synMDI.ActiveParser = parsers.HTML) then
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
  Config.Free;
  Config:=nil;
  mHHelp.Free;
  HHCloseAll;     //Close help before shutdown or big trouble
end;

procedure TfrmMain.CloseAll;
var n : Integer;
Begin
   _doingcloseall := True;
   for n := tabs.Tabs.Count - 1 downto 0 do
   Begin
      TfrmClient(Tabs.Tabs.Objects[n]).Close;
   end;
   if tabs.Tabs.Count = 0 then
   begin
     tabs.Visible := False;
     panBack.Visible := False;
     StatusBar1.Panels[0].Text := '';
     StatusBar1.Panels[1].Text := '';
     StatusBar1.Panels[2].Text := '';
   end;
   _doingcloseall := False;
   EnableButtons;
end;

procedure TfrmMain.HandleHint(Sender: TObject);
Begin
   StatusBar1.Panels[StatPanel].Text := GetLongHint(Application.Hint);
end;

procedure TfrmMain.SetSyntax(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor:=GetCurrentEditor;
  if Assigned(FEditor) Then FEditor.SetParser(TMenuItem(Sender).Tag);
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
var
  FEditor : TfrmClient;
  FindSeed : string;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then
  begin
    case FEditor.Mode of
      emNormal :
      begin
        if not Assigned(Fnd) then Fnd := TfrmFind.Create(Self);
        FindSeed := FEditor.synMDI.SelText;
        // If more than one line is selected then don't try and find the selection...
        if pos(#13, FindSeed) > 0 then FindSeed := '';
        // Set the Regular Expressions Check-Box
        Fnd.RegExBox.Checked := actRegExp.Checked;
        // Invoke the Find Text dialog
        Fnd.Execute(FindSeed, 0, FEditor.synMDI);
        FLastFindOptions := FEditor.synMDI.LastFindOptions;
        FLastFindText    := FEditor.synMDI.LastFindText;
       end;
      emHex : FEditor.HexFind;
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
         end;
   end;
end;

procedure TfrmMain.actReplaceExecute(Sender: TObject);
var
  FEditor : TfrmClient;
  FindSeed : string;
begin
   FEditor := GetCurrentEditor;
   if Assigned(FEditor) then
   begin
      if not Assigned(Rep) then Rep := TfrmReplace.Create(Self);
      FindSeed := FEditor.synMDI.SelText;
      // If more than one line is selected then don't try and find the selection...
      if pos(#13, FindSeed) > 0 then FindSeed := '';
      // Set the Regular Expressions Check-Box
      Rep.RegExBox.Checked := actRegExp.Checked;
      // Invoke the Find Text dialog
      Rep.Execute(FindSeed, 0, FEditor.synMDI);
      EnableButtons;
   end;
end;

procedure TfrmMain.DoSetFilesFirst;
var fm  : tfrmWelcome;
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
     Opt := tfrmOptions.Create(Self);
     Try
        Opt.ShowModal;
     finally
        FreeAndNil(Opt);
     End;
  End;
end;

Procedure TfrmMain.UpdateStatusBar;
var FEditor : TfrmClient;
    i       : Integer;
Begin
   if _doingcloseall then Exit;
   FEditor := GetCurrentEditor;
   if FEditor = nil then
   Begin
      for i:=0 to StatusBar1.Panels.Count-1 do
         StatusBar1.Panels[i].Text:='';
      If ClockShowing then ClockTimerTimer(Self);
      Exit;
   End;
   With FEditor do
   Begin
      if Modified then
         StatusBar1.Panels[1].Text:='Modified'
      else
         StatusBar1.Panels[1].Text:='';
      StatusBar1.Panels[0].Text:='[' + IntToStr(synMDI.CaretPos.X)+', '+IntToStr(synMDI.CaretPos.Y)+']  :  '+IntToStr(synMDI.Lines.Count);
      if FEditor.synMDI.InsertMode then
         StatusBar1.Panels[2].Text :=' Insert'
      else
         StatusBar1.Panels[2].Text :=' Overwrite';
   End;
end;

Procedure TfrmMain.ShowText(const Text: String);
Begin
  StatusBar1.Panels[StatPanel].Text := Text;
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
    s     : string;
Begin
  CData := Message.CopyDataStruct^;
  st := CData.lpData;
  s := string(st);
  if Pos('\', s) = 0 then
       s := ExpandFileName(s);
  OpenFile(s, '');
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
   NewType(TMenuItem(Sender).Tag, NewTitles[tMenuItem(Sender).MenuIndex]);
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
   itmNoneSet2.Visible := True;
   itmNoneSet2.default := True;
   SetStatus('We''re Here!');
   if Assigned(FEditor) then
   Begin
      MenuItem:=TPopupMenu(Sender);
      for i:=0 to MenuItem.Items.Count-2 do
      begin
         MenuItem.Items[i].Visible := FEditor.synMDI.IsBookmarkSet(MenuItem.Items[i].Tag, bmRow, bmCol);
         If MenuItem.Items[i].Visible Then
         begin
           itmNoneSet2.Visible := False;
           itmNoneSet2.default := False;
         end;
      end;

   end;
end;

procedure TfrmMain.actRedoExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then FEditor.synMDI.Perform(SEM_REDO, 0, 0);
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

procedure TfrmMain.actUppercaseExecute(Sender: TObject);
var FEditor : TfrmClient;
    oss : Integer;
    osl : Integer;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
   Begin
      oss := SelStart;
      osl := SelLength;
      SelText := Uppercase(SelText);
      SelStart := oss;
      SelLength := osl;
      AddLineGlyph(15,CaretPos.Y);
   end;
end;

procedure TfrmMain.actLowercaseExecute(Sender: TObject);
var FEditor : TfrmClient;
    oss : Integer;
    osl : Integer;
begin
   FEditor := GetCurrentEditor;
   if not Assigned(FEditor) then exit;
   with FEditor.synMDI do
   Begin
      oss := SelStart;
      osl := SelLength;
      SelText := Lowercase(SelText);
      SelStart := oss;
      SelLength := osl;
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
  StatusBar1.Panels[StatPanel].Text:='';
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  Opt := TfrmOptions.Create(Self);
  Try
     Opt.ShowModal;
  finally
     FreeAndNil(Opt);
  end;
end;

procedure TfrmMain.PreviewWithIE;
var FEditor : TfrmClient;
    s       : string;
begin
  FEditor := GetCurrentEditor;
  s := GetPreviewFileName(FEditor);
  if frmPreview = nil then
     frmPreview := TfrmPreview.Create(Self);
  frmPreview.Execute(0, s);
end;

procedure TfrmMain.PreviewWithMozilla;
var FEditor : TfrmClient;
    s       : string;
begin
  FEditor := GetCurrentEditor;
  s := GetPreviewFileName(FEditor);
  if frmPreview = nil then
    frmPreview := TfrmPreview.Create(Self);
  frmPreview.Execute(1, s);
end;

procedure TfrmMain.actPreviewHTMLExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not assigned(FEditor) then Exit;
  if TAction(Sender).Tag < 10 then
  begin
    case TAction(Sender).Tag of
      0 : PreviewWithIE;
      1 : PreviewWithMozilla;
    end;
  end else
  begin
    Execute('.Browsers', TMenuItem(Sender).Tag - 10);
  end;
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
    n,
    i          : integer;
    PluginQuit : tPlgProc;
    b          : Boolean;
    pi         : PPluginData;
begin
   {$IFDEF Plugin}
   for i := 0 to Plugins.Count - 1 do
   begin
      pi := Plugins.Items[i];
      if pi^.OnQuit then
      begin
        DLLHandle := LoadLibrary(PChar(pi^.FileName));
        if (DLLHandle <> 0) then
        begin
          // Initialise the Plugin...
          @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
          // Make sure we pass the handle of the plugin interface...
          PluginInit(Application.Handle);
          // Pass the plugin the message number for atoms etc...
          @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
          PlgMsgNum(MsgNum);
          @PluginQuit := GetProcAddress(DLLHandle, 'QuitExecute');
          PluginQuit;
          FreeLibrary(DLLHandle);
        end;
      end;
   end;
   {$ENDIF}
   If frmMain.WindowState <> wsMaximized Then
   Begin
      SetPosition('Echo Software', 'PN', Top, Left);
      SetSize('Echo Software', 'PN', frmMain.Height, frmMain.Width);
   End;
   SetStat('Echo Software', 'PN', WindowState);
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
      Settings.WriteBool('HighlightActive', HighlightActive);
      Settings.WriteBool('HighlightChange', HighlightChange);
      Settings.WriteInteger('HighlightColor', HighlightColor);
      Settings.WriteInteger('ModifiedColor', ModifiedColor);
      Settings.WriteInteger('WarnOpenAgain', _wnOpenFileAgain);
      Settings.WriteBool('WordWrap', actWordWrap.Checked);
      Settings.WriteBool('OpenMaxed', OpenFilesMaxed);
      Settings.WriteBool('ClearUndo', LoseUndo);
      Settings.WriteString('LastDir', LastUsed);
      Settings.WriteInteger('MRUMax', MRUFileList1.Maximum);
      case tabs.Align of
        alTop    : Settings.WriteInteger('TabPos', 1);
        alBottom : Settings.WriteInteger('TabPos', 2);
      end;
      Settings.WriteBool('ShowClock', actClock.Checked);
      Settings.WriteString('ClockFormat', ClockFormat);
      Settings.WriteBool('PrintMargin', actPrintMargin.Checked);
      Settings.WriteBool('UseRegExp', actRegExp.Checked);
      Settings.WriteBool('QSKeepFocus', QSKeepFocus);
      if SetFiles then
        Settings.WriteBool('First', false);
      if Settings.ValueExists('SaveQSHistory') then
        b := Settings.ReadBool('SaveQSHistory') else
        b := False;
      Settings.CloseKey;
      {Internet Settings}
      Settings.OpenKey(NetKey, True);
      Settings.WriteBool('LaunchURLs', DoURLs);
      Settings.CloseKey;
      Settings.OpenKey(QSKey, True);
      if b then
      begin
        if cmbSearch.Items.Count <= 10 then n := cmbSearch.Items.Count else n := 10;
        for i := 0 to n - 1 do
        begin
          Settings.WriteString('mrui' + IntToStr(i+1), cmbSearch.Items[i]);
        end;
        Settings.WriteInteger('MRU', n);
      end else
        Settings.WriteInteger('MRU', 0);
      Settings.CloseKey;
   Finally
      Settings.Free;
   end;
   // Save out toolbar configuration
   SaveToolbars;
   // Save out keyboard configuration
   SaveKeyShortcuts;
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
  if pos('>', FEditor.Filename) = 0 Then FEditor.SaveFile
     Else actSaveAsExecute(Sender);
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
var n : Integer;
begin
  if OpenDialog1.Execute then
  begin
    for n := 0 to OpenDialog1.Files.Count -1  do
    begin
      OpenFile(OpenDialog1.Files[n],'');
    end;
    OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
    LastUsed := OpenDialog1.InitialDir;
    OpenDialog1.FileName := '';
  end;
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
   if GetSetting('UseProxy', NetKey, False) then
   begin
      wfInfo.ProxyHost := GetSetting('ProxyHost', NetKey, '');
      wfInfo.ProxyPort := StrToInt(GetSetting('ProxyPort', NetKey, '8080'));
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
begin
   Application.OnException := nil;
   Try logMain.Add('exception', E.Message); Except End;
   Case MessageDlg('There was an internal error:'+#9+E.Message +#13#10+
              'Would you like to attempt to save all files?', mtError, [mbYes, mbNo], 0) of
     mrYes :
       Try
         actSaveAllExecute(actSaveAll);
       Except
         MessageDlg('There was a problem saving all files. Please close down PN and re-start.', mtError, [mbOK], 0);
       End;
     mrNo : ;
   end;
   Application.OnException := ExceptionHandler;
end;

procedure TfrmMain.actTabsExecute(Sender: TObject);
begin
   actTabs.Checked := not actTabs.Checked;
   EnableButtons;
end;

procedure TfrmMain.HexButtons;
begin
   with GetCurrentEditor do
   Begin
   {Bookmarks Disabled}
       btnGotoBookmark.Enabled         := False;
       btnSetBookmark.Enabled      := False;
       actSetDefaultBookmark.Enabled := False;
       actJumpDefaultBookmark.Enabled := False;
       actBMAdd.Enabled           := False;
       actBMRemove.Enabled        := False;
       lstBookmarks.Enabled       := False;
       lstBookmarks.Items.Clear;
   {Find???}
       actFind.Enabled             := HexEditor.DataSize > 0;
       {TActions}
       actProperties.Enabled      := False;
       actPasteFromFile.Enabled   := False;
       actDelete.Enabled          := False;
       actDeleteFile.Enabled      := not IsNewFile;
       actSelectAll.Enabled       := False;
       actSaveAll.Enabled         := True;
       actPrint.Enabled           := True;
       actViewHex.Enabled         := True;
       actCopy.Enabled            := (HexEditor.SelCount > 0);
       actCut.Enabled             := (HexEditor.SelCount > 0);
       actPaste.Enabled           := Clipboard.HasFormat(CF_TEXT);
       actClose.Enabled           := True;
       actSave.Enabled            := (HexEditor.Modified) and (not HexEditor.ReadOnlyFile);
       actSaveAs.Enabled          := (not HexEditor.ReadOnlyFile);
       actPrint.Enabled           := True;
{Save With Formatting}
       actSaveWithFormatting.Enabled := False;
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
{Upper/Lower Case}
       actUpperCase.Enabled       := False;
       actLowerCase.Enabled       := False;
       actReverseCase.Enabled     := False;
{HTML Tag Stuff}
       actSelectTag.Enabled       := False;
       actSelectEnclosing.Enabled := False;
       actPreviewHTML.Enabled     := False;
       itmPreviewHTML.Enabled     := False;
       actViewAsHTML.Enabled      := False;
{Other Stuff}
       actTabsToSpaces.Enabled    := False;
       actSpacesToTabs.Enabled    := False;
{Hex Specific Stuff}
       actPrintPreview.Visible    := True;
       sepHex.Visible             := True;
       sepHex2.Visible            := True;
       sepHex3.Visible            := True;
       actGrid.Visible            := True;
       itmHexTranslation.Visible  := True;
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
       actJumpToLine.Visible      := False;
       sepJump.Visible            := False;
       actPrintMargin.Visible     := False;
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
   HtmlHelp(0, PChar(mHHelp.ChmFile), HH_DISPLAY_TOC, 0);
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
       odOctal     : actOctalOffset.Checked := True;
       odNone      : actNoOffset.Checked  := True;
     end;
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
   fEditor := GetCurrentEditor;
   itmHighlight.Enabled := (fEditor <> nil);
   itmBookmark.Enabled  := (fEditor <> nil);
   itmBookmark1.Enabled := (fEditor <> nil);
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
   SetHighlightMenu(itmHighlight);
end;

procedure TfrmMain.AddHighlightMenuItem(Sender : tObject; ftag : integer);
var s : string;
    mi : tMenuItem;
    FEditor : tfrmClient;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;
  s := tSyntaxMemoParser( parsers.tagitems(ftag) ).UI_Styles.LangName;
  If s <> '' then
  begin
    mi := tMenuItem.Create(Self);
    mi.Caption := s;
    mi.Tag := ftag;
    mi.Checked := ftag = FEditor.Parser;
    mi.OnClick := SetSyntax;
    tMenuItem(Sender).Add(mi);
  end;
end;

procedure TfrmMain.SetHighlightMenu(Sender : TObject);
var i : integer;
    FEditor : tfrmClient;
begin
  FEditor := GetCurrentEditor;
  if FEditor = nil then exit;

  // Delete the old items...
  for i := tMenuItem(Sender).Count - 1 downto 0 do
     tMenuItem(Sender).Delete(i);

  for i := 1 to parsers.MaxTag do
  begin
    AddHighlightMenuItem(Sender, i);
  end;
  AddHighlightMenuItem(Sender, 0);
end;

procedure TfrmMain.actShowMarkersExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.ShowMarkerColumn := not fEditor.HexEditor.ShowMarkerColumn;
end;

procedure TfrmMain.tmrStartupTimer(Sender: TObject);
begin
  FormFocus(Sender);
end;

procedure TfrmMain.SaveToolbars;
begin
   // Call Toolbar97s SaveToolbars thingy...
   RegSaveToolbarPositions(Self, Toolkey2);
end;

procedure TfrmMain.LoadToolbars;
begin
   // Call Toolbar97s LoadToolbars thingy...
   RegLoadToolbarPositions(Self, Toolkey2);
end;

procedure TfrmMain.actSaveAllExecute(Sender: TObject);
var n : integer;
    FEditor : TfrmClient;
begin
   for n := 0 to tabs.tabs.Count - 1 do
   Begin
     FEditor := TfrmClient(tabs.Tabs.Objects[n]);
     if pos('>', FEditor.Filename) = 0 Then
       FEditor.SaveFile
     Else
       SaveAs(FEditor);
   end;
   EnableButtons;
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
      Unix   = 4;
      Hex    = 5;
      Ver    = 6;
begin
   savefiltermax := 0;
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
   part := Ver;
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
                  inc(savefiltermax);
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
                    part := Unix;
                  end;
         Unix   : begin
                    part := Hex;
                  end;
         Hex    : begin
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
      Unix   = 4;
      Hex    = 5;
      Ver    = 6;
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
   part := Ver;
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
                     StatusBar1.Panels[3].Text := 'Converted old types file...';
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
                    part := Unix;
                  end;
         Unix   : begin
                    part := Hex;
                  end;
         Hex    : begin
                    part := Desc;
                  end;
      end;
   until Length(s) < 1;
   Filter := 'Recognised File Types|' + all + '|All Files (*.*)|*.*|' + Filter;
   if Copy(Filter, Length(Filter), 1) <> '|' then
      if Length(Filter) > 0 then Filter := Filter + '|';
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
    IndStr   : string;
    UnxStr   : string;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
      Unix   = 4;
      Hex    = 5;
      Ver    = 6;
begin
   // Setup the File-Type Tables...
   TypeName.Clear;
   TypeIndex.Clear;
   TypeFilter.Clear;
   TypeUnix.Clear;
   TypeHex.Clear;
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
   part := Ver;
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
                    part := Unix;
                    IndStr := ps;
                  end;
         Unix   : begin
                    part := Hex;
                    UnxStr := ps;
                  end;
         Hex    : begin
                    part := Desc;
                    repeat
                       TypeIndex.Add(IndStr);
                       TypeFilter.Add(IntToStr(Count));
                       TypeUnix.Add(UnxStr);
                       TypeHex.Add(ps);
                    until TypeIndex.Count >= TypeName.Count;
                    Inc(Count);
                  end;
      end;
   until Length(s) < 1;
end;

procedure TfrmMain.AddNewItem(iCaption : string; iTag : Integer; iOnClick : tNotifyEvent; NShortcut : boolean);
var
  new : tMenuItem;
  ne2 : tMenuItem;
begin
  ne2 := tMenuItem.Create(Self);
  new := tMenuItem.Create(Self);
  new.Caption := iCaption;
  ne2.caption := iCaption;
  new.OnClick := iOnClick;
  ne2.OnClick := iOnClick;
  new.Tag := iTag;
  ne2.Tag := iTag;
  If nShortcut then new.ShortCut := Shortcut(78, [ssCtrl]);
  File1.Items[0].Add(new);
  popNew.Items.Add(ne2);
end;

procedure TfrmMain.UpdateNew;
var ini : tIniFile;
    num : integer;
    n   : integer;
    s   : string;
begin
  // Clear the menus...
  for n := File1.Items[0].Count -1 downto 0 do
    File1.Items[0].Delete(0);
  for n := popNew.Items.Count -1 downto 0 do
    popNew.Items.Delete(0);
  NewTitles.Clear;
  NewParsers.Clear;
  // Propagate the new menu options...
  ini := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'new.def');
  try
    num := ini.ReadInteger('New', 'Count', 0);
    // Add the Plain Text item...
    If num > 0 then
    begin
      NewTitles.Add('<new>');
      NewParsers.Add('0');
      AddNewItem('Plain &Text', 0, NewFromMenu, true);
      AddNewItem('-', 0, nil, false);
      NewTitles.Add(' ');
      NewParsers.Add('0');
      For n := 0 to num - 1 do
      begin
        s   := ini.ReadString(inttostr(n), 'Name', 'New');
        NewTitles.Add( '<' + ini.ReadString(inttostr(n), 'Title', 'new') + '>' );
        NewParsers.Add(ini.ReadString(inttostr(n), 'Scheme', '0'));
        AddNewItem(s, ini.ReadInteger(inttostr(n), 'Scheme', 0), NewFromMenu, false);
      end;
    end else
    begin
      NewTitles.Add('<new>');
      NewParsers.Add('0');
      New1.OnClick := NewFromMenu;
      New1.Hint := 'New Text File';
      New1.Tag := 0;
      btnNew.DropdownCombo := False;
      btnNew.DropdownArrow := False;
    end;
  finally
    ini.free;
  end;
end;

procedure TfrmMain.UpdateDialogs;
begin
   SetStatus('Reading Type Configuration...');
   if not FileExists(ExtractFilePath(ParamStr(0)) + 'types.def') then
      CreatePNFile(ExtractFilePath(ParamStr(0)) + 'types.def', strOpenTypes);
   OpenDialog1.Filter := GenOpenFilter;
   SaveDialog1.Filter := GenSaveFilter;
   SetStatus('Setting Types...');
   UpdateTypes;
   SetStatus('Building Menus...');
   UpdateNew;
   SetStatus('');
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var n : integer;
    mi : TPluginMI;
begin
   // Free the various lookup stringlists.
   if Assigned(Grep) then FreeAndNil(Grep);
   if Assigned(frmHelper) then
   begin
     frmHelper.Close;
     FreeAndNil(frmHelper);
   end;

   for n := 0 to fTempFiles.Count -1 do
   begin
     DeleteFile(fTempFiles[n]);
   end;

   Try
     TypeName.Free;
     TypeIndex.Free;
     TypeFilter.Free;
     TypeUnix.Free;
     TypeHex.Free;
     NewTitles.Free;
     NewParsers.Free;
     fTempFiles.Free;
   Except
     logMain.Add('shutdown', 'Exception freeing stringlists');
     logMain.DumpCache;
     logMain.FinishLogging;
   End;
   For n := File1.Items[0].Count - 1 downto 0 do
     Try File1.Items[0].Delete(n) Except End;
   For n := popNew.Items.Count - 1 downto 0 do
     Try popNew.Items.Delete(n) Except End;
   {$IFDEF plugin}
   Try
     for n := PluginMenuItems.Count - 1 downto 0 do
     begin
        mi := TPluginMI(PluginMenuItems[n]);
        PluginMenuItems.Remove(mi);
        FreeAndNil(mi);
     end;
     PluginMenuItems.Free;
   Except
     logMain.Add('shutdown', 'Exception freeing plugin interface elements');
     logMain.DumpCache;
     logMain.FinishLogging;
   end;
   {$ENDIF}
   if Assigned(fnd) then FreeAndNil(fnd);
   if Assigned(rep) then FreeAndNil(rep);
   try
     BookmarkMRU.Shutdown
   except
     logMain.Add('shutdown', 'Exception freeing bookmark interface');
     logMain.DumpCache;
     logMain.FinishLogging;
   end;
   // Free the Parser Repository
   try
     Parsers.Free;
   except
     logMain.Add('shutdown', 'Exception freeing scheme repository');
     logMain.DumpCache;
     logMain.FinishLogging;
   end;
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

function TfrmMain.GetSaveAsUnix(Ext : string) : boolean;
var i : integer;
begin
   // Ensure Extension is in *.ext format:
   if Pos('.', Ext) = 0 then Ext := '.' + Ext;
   Ext := '*' + Ext;
   // Find position in first stringlist:
   i := TypeName.IndexOf(Ext);
   if i = -1 then
      Result := False
   else
      Result := StrToBool(TypeUnix[i]);
end;

function TfrmMain.GetOpenAsHex(Ext : string) : boolean;
var i : integer;
begin
   // Ensure Extension is in *.ext format:
   if Pos('.', Ext) = 0 then Ext := '.' + Ext;
   Ext := '*' + Ext;
   // Find position in first stringlist:
   i := TypeName.IndexOf(Ext);
   if i = -1 then
      Result := False
   else
      Result := StrToBool(TypeHex[i]);
end;

{$IFDEF plugin}
procedure TfrmMain.CheckPlugins;
var search : tSearchRec;
    DLLHandle : THandle;
    i         : Integer;
    s         : string;
    p         : pChar;
    ExOnStart : tBoolResult;
    ExWhen    : TPlgFlags;
    CallFlags : Longint;
    data      : TPluginData;

function CheckFlag(Flags, Flag : Longint) : Boolean;
begin
  Result := (Flag and Flags) = Flag;
end;

begin
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
               PluginInit(Application.Handle);
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
               // Store the name of the plugin...
               data.Name := s;
               // And it's filename...
               data.FileName := ExtractFileName(search.Name);
               // Now we find out where the plugin should be called...
               @ExWhen := GetProcAddress(DLLHandle, 'CallRequests');
               Try
                 CallFlags := ExWhen;
                 If CheckFlag(CallFlags, plgOnStartup) then
                 begin
                   @ExOnStart := GetProcAddress(DLLHandle, 'StartExecute');
                   Try
                     ExOnStart;
                   Except
                   end;
                 end;
                 If CheckFlag(CallFlags, plgOnShutdown) then
                   data.OnQuit := True else data.OnQuit := False;
                 If CheckFlag(CallFlags, plgOnFocus) then
                   data.OnGotFocus := True else data.OnGotFocus := False;
                 If CheckFlag(CallFlags, plgOnLostFocus) then
                   data.OnLostFocus := True else data.OnLostFocus := False;
               Except
                 MessageDlg('Programmers Notepad could not retrieve information from plugin:'+#13+#10+s+#13+#10+'This is most likely because the plugin interface has not been updated to '+#13+#10+'specification 2.', mtWarning, [mbOK], 0);
               end;
               Plugins.Add(data);
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
      2 : tbrFolders.Visible := not tbrFolders.Visible;
      3 : tbrSearch.Visible := not tbrSearch.Visible;
   end;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actTbrMain.Checked := tbrMain.Visible;
  actTbrEdit.Checked := tbrEdit.Visible;
  actTbrFolders.Checked := tbrFolders.Visible;
  actTbrSearch.Checked := tbrSearch.Visible;
  actFIFResults.Checked := FloatFIF.Visible;
  actHelper.Checked := FloatHelper.Visible;
  if frmMain.ActiveMDIChild <> nil then
    actPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
  if actHelper.Checked then
  begin
    frmHelper.UpdateCurrent;
  end;
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
var PlgFile : string;
    PlgProc : string;
    DLLHandle : THandle;
    EventProc : tEvProc;
    MyMI      : TPluginMI;
begin
// Run a procedure in a plugin due to a menuitem click.
   MyMI := TPluginMI(Sender);
   PlgFile := MyMI.plugin;
   PlgProc := MyMI.evname;
   DLLHandle := LoadLibrary(PChar(PlgFile + '.dll'));
   if DLLHandle <> 0 then
   try
      // Initialise the Plugin...
      @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
      // Make sure we pass the handle of the plugin interface...
      PluginInit(Application.Handle);
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

procedure TfrmMain.actAnsiTranslateExecute(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
   fEditor.HexEditor.Translation := TTranslationType(TAction(Sender).Tag);
end;

procedure TfrmMain.itmHexTranslationClick(Sender: TObject);
var fEditor : tfrmClient;
begin
   fEditor := GetCurrentEditor;
   if not Assigned(fEditor) then Exit;
     case fEditor.HexEditor.Translation of
       ttAnsi      : actAnsiTranslate.Checked := True;
       ttAscii     : actAsciiTranslate.Checked := True;
       ttEBCDIC    : actEBCDICTranslate.Checked  := True;
       ttMac       : actMacTranslate.Checked := True;
       ttDos8      : actDosTranslate.Checked := True;
     end;
end;

function TfrmMain.SchemePath: string;
begin
   // Return the scheme directory, for now the app dir.
   Result := extractfilepath(ParamStr(0)) + 'Schemes\';
   If not DirExists(Result) then CreateDir(Result);
end;

procedure TfrmMain.SetStatus(Text : string);
begin
  StatusBar1.Panels[StatPanel].Text := Text;
end;

procedure TfrmMain.SetStartDir(which: integer; sDir: string);
var
  path           : lpstr;
  PIDL           : pItemIDList;
  s              : string;
  reg            : TRegistry;
begin
  Case which of
    0 : begin
          path := StrAlloc(MAX_PATH);
          SHGetSpecialFolderLocation(Handle, CSIDL_PERSONAL, PIDL);
          If SHGetPathFromIDList(PIDL, Path) then
          begin
            s := path;
          end;
          StrDispose(Path);
        end;
    1 :
      begin
        reg := TRegistry.Create;
        try
          reg.OpenKey(RootKey, True);
          if reg.ValueExists('LastDir') then
            try
              s := reg.ReadString('LastDir');
            except
            end;
        finally
          reg.Free;
        end;
        if removespaces(s) = '' then s := sDir;
      end;
    2 : s := sDir;
  end;
  SaveDialog1.InitialDir  := s;
  SaveDialog2.InitialDir  := s;
  OpenDialog1.InitialDir  := s;
  dlgPasteFrom.InitialDir := s;
  LastUsed := s;
end;

function TfrmMain.Add_NewItem(iCaption, iTitle: string;
  iScheme: integer): boolean;
var
  nini : tIniFile;
  n    : integer;
begin
  nini := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'new.def');
  Try
    n := nini.ReadInteger('New', 'Count', 0);
    nini.WriteInteger('New', 'Count', n + 1);
    nini.WriteString(inttostr(n), 'Name', iCaption);
    nini.WriteString(inttostr(n), 'Title', iTitle);
    nini.WriteInteger(inttostr(n), 'Scheme', iScheme);
  Finally
    nini.free;
  End;
  result := true;
end;

function TfrmMain.Add_NewType(iNewType: tNewType;
  iScheme: integer): boolean;
var
  outs : string;
  ins  : string;
  types : Textfile;
begin
  outs := sepChar + iNewType.TypeDesc + sepChar + iNewType.TypeFilter + sepChar + inttostr(ischeme) + sepChar + BoolToStr(iNewType.TypeUnix) + sepChar + BoolToStr(iNewType.TypeHex);
  Assignfile(types, ExtractFilePath(ParamStr(0)) + 'types.def');
  Reset(types);
  Try
    Readln(types, ins);
    outs := ins + outs;
    System.CloseFile(types);
    AssignFile(types, ExtractFilePath(ParamStr(0)) + 'types.def');
    Rewrite(types);
    Write(types, outs);
  Finally
    System.CloseFile(types);
  end;
  result := true;
end;

function TfrmMain.Add_NewAss(iAss : string) : boolean;
var
  files : TextFile;
begin
  AssignFile(files, ExtractFilePath(ParamSTr(0)) + 'files.def');
  Append(files);
  Try
    Writeln(files, iAss);
  Finally
    System.CloseFile(files);
  end;
  result := true;
end;

procedure TfrmMain.StatusBar1Resize(Sender: TObject);
begin
   StatusBar1.Panels[StatPanel].Width := StatusBar1.Width - 350;
end;

procedure TfrmMain.SetClock(onoff: boolean);
var np : tStatusPanel;
begin
  If pos('s', ClockFormat) <> 0 then
  begin
    ClockTimer.Interval := 800
  end else
  begin
    ClockTimer.Interval := 10000;
  end;
  Case onoff of
    true : begin
             If not ClockShowing then
             begin
               np := StatusBar1.Panels.Add;
               np.Width := 60;
               np.Alignment := taCenter;
               np := StatusBar1.Panels.Add;
               np.Width := 5;
               np.Bevel := pbNone;
             end;
             ClockShowing := True;
             ClockTimer.Enabled := True;
             ClockTimerTimer(Self);
           end;
    false: begin
             If ClockShowing then
             begin
               StatusBar1.Panels.Delete(TimePanel);
               StatusBar1.Panels.Delete(TimePanel);
             end;
             ClockShowing := False;
             ClockTimer.Enabled := False;
           end;
  end;
end;

procedure TfrmMain.ClockTimerTimer(Sender: TObject);
var s : string;
    n : integer;
begin
  s := ClockFormat;
  For n := 1 to length(s) do if s[n] = 'm' then s[n] := 'n';
  If StatusBar1.Panels.Count = TimePanel + 2 then
  begin
    StatusBar1.Panels[TimePanel].Text := FormatDateTime(s, now);
  end;
end;

procedure TfrmMain.actClockExecute(Sender: TObject);
begin
  actClock.Checked := not actClock.Checked;
  SetClock(actClock.Checked);
end;

procedure TfrmMain.URL(urlstr: string);
begin
   ShellExecute(GetDesktopWindow(), 'open', PChar(urlstr), nil, nil, SW_SHOWNORMAL);
end;

function TfrmMain.GetSetting(fname, key : string; default : boolean) : Boolean;
var MyReg : tRegistry;
begin
  MyReg := tRegistry.Create;
  Try
    With MyReg do
    begin
      OpenKey(key, true);
      If ValueExists(fname) then
        Result := ReadBool(fname)
      else
        Result := default;
      CloseKey;
    end;
  Finally
    MyReg.Free;
  end;
end;

procedure TfrmMain.SetSetting(fname, key : string; Value : Boolean);
var MyReg : TRegistry;
begin
  MyReg := tRegistry.Create;
  Try
    With MyReg do
    begin
      OpenKey(key, true);
      WriteBool(fname, value);
      CloseKey;
    end;
  Finally
    MyReg.Free;
  end;
end;

function TfrmMain.GetSetting(fname, key : string; default : string) : string;
var MyReg : TRegistry;
begin
  MyReg := tRegistry.Create;
  Try
    With MyReg do
    begin
      OpenKey(key, true);
      If ValueExists(fname) then
        Result := ReadString(fname)
      else
        Result := default;
      CloseKey;
    end;
  Finally
    MyReg.Free;
  end;
end;

procedure TfrmMain.UpdateFolders;
var tf : TextFile;
    s  : string;
begin
  cmbFolders.Items.Clear;
  cmbFolders.AddItem('My Folders', nil, 29, 0, 29, -1);
  cmbFolders.AddItem('Default', nil, 30, 0, 30, -1);
  If FileExists(ExtractFilePath(ParamSTr(0)) + 'folders.def') then
  begin
    Assignfile(tf, ExtractFilePath(ParamSTr(0)) + 'folders.def');
    Try
      Reset(tf);
      Repeat
        Readln(tf, s);
        If length(s) > 0 then
        begin
          cmbFolders.AddItem(s, nil, 31, 1, 31, -1);
        end;
      Until EOF(tf);
    Finally
      System.CloseFile(tf);
    End;
  end;
  cmbFolders.ItemIndex := 0;
end;

procedure TfrmMain.UpdateEditors;
var n : integer;
begin
  For n := 0 to MDIChildCount - 1 do
    PostMessage(tfrmClient(MDIChildren[n]).Handle,WM_UPDATESETTINGS,0,0);
end;

function TfrmMain.AppHandle : tHandle;
begin
  Result := Application.Handle;
end;

procedure TfrmMain.cmbFoldersChange(Sender: TObject);
var i : integer;
    registry : tRegistry;
    sdir     : string;
    dir      : integer;
begin
  i := cmbFolders.ItemIndex;
  If cmbFolders.IndentLevel[i] = 0 then
  begin
    Registry := tRegistry.Create;
    Registry.OpenKey(rootkey, true);
    try
      try dir := Registry.ReadInteger('StartDir'); except dir := 0; end;
      If dir = 2 then
        try sdir := Registry.ReadString('CustomStartDir'); except sdir := extractfilepath(paramstr(0)); end
      else
      If dir = 1 then
        try sdir := Registry.ReadString('LastDir'); except sdir := extractfilepath(paramstr(0)); end;
    finally
      Registry.Free;
    end;
  end else
  begin
    dir := 2;
    sdir := cmbFolders.Items[i];
  end;
  SetStartDir(dir, sdir);
  cmbFolders.ItemIndex := 0;
  If frmMain.ActiveMDIChild <> nil then frmMain.ActiveMDIChild.SetFocus;
end;

procedure TfrmMain.actReverseCaseExecute(Sender: TObject);
var FEditor : tfrmClient;
begin
  If Assigned(frmMain.ActiveMDIChild) then
  begin
    FEditor := GetCurrentEditor;
    With FEditor.synMDI do
    begin
      If SelLength > 0 then SelText := ReverseCase(SelText);
    end;
  end;
end;

procedure TfrmMain.actPrintMarginExecute(Sender: TObject);
begin
  actPrintMargin.Checked := not actPrintMargin.Checked;
  UpdateEditors;
end;

procedure ecCopyMenuItem(m1, m2 : TMenuItem);
begin
  m2.Caption := m1.Caption;
  m2.OnClick := m1.OnClick;
  m2.Hint := m1.Hint;
  m2.Tag := m1.Tag;
  m2.ImageIndex := m1.ImageIndex;
  m2.ShortCut := m1.ShortCut;
  m2.default := m1.default;
end;

procedure TfrmMain.ClearSchemeTools;
var n : Integer;
    mi : TMenuItem;
    mp : TMenuItem;

begin
  For n := popExecute.Items.Count - 1 downto 0 do
    popExecute.Items.Delete(n);
  for n := itmExecute.Count - 1 downto 0 do
    itmExecute.Delete(n);
  mi := TMenuItem.Create(Self);
  mi.Caption := 'No Tools Available';
  mi.default := True;
  mp := TMenuItem.Create(Self);
  ecCopyMenuItem(mi, mp);
  popExecute.Items.Add(mp);
  itmExecute.Add(mi);
end;

procedure TfrmMain.LoadSchemeTools(scheme : string);
var mi    : tMenuItem;
    mp    : TMenuItem;
    tools : tIniFile;
    num   : integer;
    n     : integer;
    k     : string;

begin
  For n := popExecute.Items.Count - 1 downto 0 do
    popExecute.Items.Delete(n);
  for n := itmExecute.Count - 1 downto 0 do
    itmExecute.Delete(n);
  If not assigned(frmMain.ActiveMDIChild) then
  begin
    // Show no files loaded item.
    mi := TMenuItem.Create(itmExecute);
    mi.Caption := 'No Files Open';
    mi.default := True;
    mp := TMenuItem.Create(popExecute);
    ecCopyMenuItem(mi, mp);
    popExecute.Items.Add(mp);
    itmExecute.Add(mi);
    exit;
  end;
  if scheme = '' then
  begin
    mi := TMenuItem.Create(itmExecute);
    mi.Caption := 'No Tools Defined';
    mi.default := True;
    mp := TMenuItem.Create(popExecute);
    ecCopyMenuItem(mi, mp);
    popExecute.Items.Add(mp);
    itmExecute.Add(mi);
    Exit;
  end;
  tools := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  try
    num := tools.ReadInteger(scheme, 'Number', 0);
    For n := 1 to num do
    begin
      mi := tMenuItem.Create(itmExecute);
      mi.Caption := tools.ReadString(scheme, 'Desc' + inttostr(n), 'Tool ' + inttostr(n));
      mi.Tag := n;
      k := tools.ReadString(scheme, 'Kyb' + IntToStr(n), '');
      if (k <> '') then
        mi.ShortCut := TextToShortCut(k);
      if n = 1 then mi.ImageIndex := 32 else mi.ImageIndex := -1;
      mi.OnClick := ToolExecute;
      mp := TMenuItem.Create(popExecute);
      ecCopyMenuItem(mi, mp);
      popExecute.Items.Add(mp);
      itmExecute.Add(mi);
    end;
    if num = 0 then
    begin
      mi := TMenuItem.Create(itmExecute);
      mi.Caption := 'No Tools Defined';
      mi.default := True;
      mp := TMenuItem.Create(popExecute);
      ecCopyMenuItem(mi, mp);
      popExecute.Items.Add(mp);
      itmExecute.Add(mi);
    end;
  finally
    tools.Free;
  end;
end;

function TfrmMain.ParseInData(DataType : tParseDataType; var input : string) : boolean;
var
  p       : string;
  s       : string;
  FEditor : tfrmClient;
begin
  Result := True;
  p := input;
  Case DataType of
    pdtFilename :
      begin
        FEditor := GetCurrentEditor;
        If FEditor = nil then p := ParseIn('', p, '%f') Else
        Begin
           If pos('>', FEditor.FileName) = 0 then
             p := ParseIn(FEditor.Filename, p, '%f')
           Else
           Begin
             // Prompt user to save their file.
             If MessageDlg('You have not saved this file, and therefore cannot use it with this tool.'+
             #13+#10+'Do you wish to save the file and continue?', mtWarning, [mbYes, mbNo], 0) = mrYes then
               FEditor.SaveFile
             else
               Exit;
             If (pos('>', FEditor.Filename) = 0) and (Length(FEditor.Filename) > 0) then p := ParseIn(FEditor.filename, p, '%f') else
               p := ParseIn('', p, '%f');
           End;
        End;
      end; {Begin pdtFilename}
    pdtExtraParms :
      begin
        s := p;
        If InputQuery('Programmers Notepad', 'Tool Parameters:', s) then
          p := s
        else
          Result := False;
      end;
    pdtColumn :
      begin
        FEditor := GetCurrentEditor;
        if FEditor = nil then
          p := ParseIn('', p, '%c')
        Else
          p := ParseIn(IntToStr(FEditor.synMDI.CaretPos.x), p, '%c');
      end;
    pdtLine :
      begin
        FEditor := GetCurrentEditor;
        if FEditor = nil then
          p := ParseIn('', p, '%l')
        Else
          p := ParseIn(IntToStr(FEditor.synMDI.CaretPos.y), p, '%l');
      end;
    pdtBuildFile :
      begin
        if Assigned(frmHelper) then
          p := ParseIn(frmHelper.GetBuildFile, p, '%b')
        else
          p := ParseIn('', p, '%b');
      end;
    pdtFolder :
      begin
        FEditor := GetCurrentEditor;
        If FEditor = nil then p := ParseIn('', p, '%d') Else
        Begin
           If pos('>', FEditor.FileName) = 0 then
             p := ParseIn(ExtractFilePath(FEditor.FileName), p, '%d')
           Else
           Begin
             // Prompt user to save their file.
             If MessageDlg('You have not saved this file, and therefore cannot use it with this tool.'+
             #13+#10+'Do you wish to save the file and continue?', mtWarning, [mbYes, mbNo], 0) = mrYes then
               FEditor.SaveFile
             else
               Exit;
             If (pos('>', FEditor.Filename) = 0) and (Length(FEditor.Filename) > 0) then p := ParseIn(ExtractFilePath(FEditor.FileName), p, '%d') else
               p := ParseIn('', p, '%d');
           End;
        End;
      end; {Begin pdtFolder}

  end; {case}
  input := p;
end;

procedure TfrmMain.ExpandConstants(var str : string);
begin
  If pos('%f', str) <> 0 Then
    ParseInData(pdtFilename, str);
  if Pos('%l', str) <> 0 then
    ParseInData(pdtLine, str);
  if Pos('%c', str) <> 0 then
    ParseInData(pdtColumn, str);
  if Pos('%b', str) <> 0 then
    ParseInData(pdtBuildFile, str);
  if Pos('%d', str) <> 0 then
    ParseInData(pdtFolder, str);
end;

procedure TfrmMain.RunIt(App, Parms, Dir : string; AskParms, Capture : Boolean);
var msg     : tStringList;
    FEditor : tfrmClient;
    cmd     : string;
    par     : string;
    doit    : Boolean;
    fdr     : string;
begin
  // Run App, and Capture Output...
  doit := True;
  msg := tStringList.Create;
  If Assigned(frmMain.ActiveMDIChild) then
  begin
    cmd := App;

    par := Parms;
    fdr := Dir;

    ExpandConstants(cmd);
    ExpandConstants(par);
    ExpandConstants(fdr);

    cmd := cmd + ' ' + par;
    If AskParms then
       doit := ParseInData(pdtExtraParms, par);
    if doit then
    begin
      RunProgram(cmd, fdr, msg);
      FEditor := GetCurrentEditor;
      FEditor.ShowResults(True);
      FEditor.SetResults(msg);
    end;
  end else
    SetStatus('Note: Tool not launched because no files are open...');
  msg.Free;
end;

procedure TfrmMain.RunIt(App, Parms, Dir : string; AskParms : Boolean);
var
  cmd : string;
  par : string;
  doit : Boolean;
  fdr  : string;
begin
  doit := True;
  If Assigned(frmMain.ActiveMDIChild) then
  begin
    cmd := App;
    par := Parms;
    fdr := Dir;

    ExpandConstants(par);
    ExpandConstants(fdr);
    ExpandConstants(cmd);

    if Length(fdr) < 1 then fdr := ExtractFilePath(ParamStr(0));

    If AskParms then doit := ParseInData(pdtExtraParms, par);
    if doit then ShellExecute(Handle, PChar('open'), PChar(cmd), PChar(par), PChar(fdr), SW_SHOW);
  end else
    SetStatus('Note: Tool not launched because no files are open...');
end;

procedure TfrmMain.Execute(scheme : string; indx : integer);
var tools : tIniFile;
    Index : ShortString;
begin
  Index := inttostr(indx);
  tools := tIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  try
    if tools.ReadInteger(Scheme, 'Number', 0) < StrToInt(Index) then Exit;
    If tools.ValueExists(scheme, 'App' + index) then
    Case tools.ReadBool(scheme, 'Cap' + index, False) of
      True :
          RunIt(
            CStringToString(tools.ReadString(scheme, 'App' + index, '')),
            CStringToString(tools.ReadString(scheme, 'Par' + index, '')),
            CStringToString(tools.ReadString(scheme, 'Dir' + index, '')),
            tools.ReadBool(scheme, 'Ask' + index, False),
            True // Set Capture to True, calls overloaded procedure.
          );
      False :
          RunIt(
            CStringToString(tools.ReadString(scheme, 'App' + index, '')),
            CStringToString(tools.ReadString(scheme, 'Par' + index, '')),
            CStringToString(tools.ReadString(scheme, 'Dir' + index, '')),
            tools.ReadBool(scheme, 'Ask' + index, False)
          );
    end;
  finally
    tools.free;
  end;
end;

procedure TfrmMain.ExecuteTool(Indx : Integer);
var
  scheme : string;
begin
  scheme := tSyntaxMemoParser(tfrmclient(frmMain.ActiveMDIChild).synMDI.Parser1).UI_Styles.LangName;
  Execute(Scheme, Indx);
end;

procedure TfrmMain.ToolExecute(Sender : tObject);
begin
  ExecuteTool(tMenuItem(sender).tag);
end;

procedure TfrmMain.actJumpToLineExecute(Sender: TObject);
var s : string;
    FEditor : tfrmClient;
begin
  if not Assigned(frmMain.ActiveMDIChild) then Exit;
  FEditor := GetCurrentEditor;
  s := '';
  if InputQuery('PN', 'Jump to which line number?', s) then
  begin
    FEditor.JumpToLineNumber(GetNumbers(s), False);
  end;
end;

procedure TfrmMain.actCloseAllExecute(Sender: TObject);
begin
  CloseAll;
end;

procedure TfrmMain.actSaveWithFormattingExecute(Sender: TObject);
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
      if Pos('<', FEditor.FileName) <> 0 then Filename := '' else
      begin
        nfn := ExtractFileName(FEditor.Filename);
        nfn := copy(nfn, 1, pos(ExtractFileExt(FEditor.Filename), nfn)-1);
        Filename := nfn;
      end;
      DefaultExt := 'html';
      if Execute then
      begin
        case SaveDialog2.FilterIndex of
          1 : FEditor.synMDI.SaveFormat := sfHTML;
          2 : FEditor.synMDI.SaveFormat := sfRTF;
        end;
        FEditor.synMDI.SaveToFile(Filename);
      end;
    end;
    FEditor.synMDI.SaveFormat := oft;
  end;
end;

procedure TfrmMain.actSetDefaultBookmarkExecute(Sender: TObject);
var
  n       : integer;
  cpos,
  lpos    : Longint;
  FEditor : tfrmClient;
  _notset : Boolean;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  _notset := True;
  for n := 0 to 9 do
  begin
    if (not fEditor.synMDI.IsBookmarkSet(n, lpos, cpos)) and _notset then
    begin
      fEditor.synMDI.SetBookmark(n, fEditor.synMDI.CaretPos.y, fEditor.synMDI.CaretPos.x);
      _notset := False;
    end;
  end;
end;

procedure TfrmMain.actJumpDefaultBookmarkExecute(Sender: TObject);
var
  n       : integer;
  cpos,
  lpos    : Longint;
  FEditor : tfrmClient;
  _notset : Boolean;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  _notset := True;
  for n := 0 to 9 do
  begin
    if (fEditor.synMDI.IsBookmarkSet(n, lpos, cpos)) and _notset then
    begin
      fEditor.synMDI.JumpToBookmark(n);
      _notset := False;    
    end;
  end;
end;

procedure TfrmMain.actExecuteExecute(Sender: TObject);
begin
  ExecuteTool(1);
end;

procedure TfrmMain.cmbSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  FEditor : TfrmClient;
begin
   _enteroverride := False;
   if Key = vk_return then
   begin
     // Do Search...
     FEditor := GetCurrentEditor;
     if not Assigned(fEditor) then Exit;
     FEditor.synMDI.HideSelection := False;
     MyFindText(FEditor.synMDI, cmbSearch.Text);
     _enteroverride := True;
     if not QSKeepFocus then Windows.SetFocus(FEditor.synMDI.Handle);
   end else
   if Key = vk_escape then
   begin
     FEditor := GetCurrentEditor;
     if not Assigned(fEditor) then Exit;
     _enteroverride := True;
     Windows.SetFocus(FEditor.synMDI.Handle);
   end;
end;

procedure TfrmMain.MyFindText(fEditor : tSyntaxMemo; fFindText : string);
var FoundAt : Longint;
    FStartPoint        : longint;
    FOriginalSelection : TChRange;
    FFindOpt           : Longint;
begin
  FEditor.Perform(SEM_SELECTION, eaGET, longint(@FOriginalSelection));
  FOriginalSelection := Normalise(FOriginalSelection);
  FStartPoint := FOriginalSelection.chStart;
  FFindOpt := 0;
  if actRegExp.Checked then FFindOpt := FFindOpt or ft_REGEXPR;

  with FEditor do
  begin
    if (not actRegExp.Checked) then
      if (Lowercase(SelText) = Lowercase(fFindText)) then
        FStartPoint := FStartPoint + 1
      else
        if FStartPoint <> 0 then FStartPoint := FStartPoint - 1;
    FEditor.Perform(EM_SETSEL, FStartPoint, FStartPoint);
    // Set my find variables...
    FLastFindOptions := FFindOpt;
    FLastFindText    := fFindText;
    FoundAt := 0;
    if cmbSearch.Items.IndexOf(LowerCase(fFindText)) = -1 then cmbSearch.Items.Insert(0, LowerCase(fFindText));
    if FindText(fFindText, FoundAt, FFindOpt or ft_SILENT) then
    begin
      if sizeof(TSM_char) = sizeof(WideChar) then
        if actRegExp.Checked then
          Perform(EM_SETSEL, CaretPosition.ByteOffsetFromCharOffset[FoundRESections[0]], CaretPosition.ByteOffsetFromCharOffset[FoundRESections[FoundRENumSections]])
        else
          Perform(EM_SETSEL, CaretPosition.ByteOffsetFromCharOffset[FoundAt],            CaretPosition.ByteOffsetFromCharOffset[FoundAt + length(fFindText)])
      else
        if actRegExp.Checked then
          Perform(EM_SETSEL, FoundRESections[0], FoundRESections[FoundRENumSections])
        else
          Perform(EM_SETSEL, FoundAt,            FoundAt + length(fFindText));
    Perform(EM_SCROLLCARET,0,0);
    SetFocus;
    end
    else ShowMessage(format('"%s" not found', [fFindText]));
  end;
end;

procedure TfrmMain.cmbSearchEnter(Sender: TObject);
begin
  if cmbSearch.Tag = 1 then cmbSearch.Text := '';
  cmbSearch.Tag := 0;
end;

procedure TfrmMain.actQuickSearchSelectExecute(Sender: TObject);
var fEditor : TfrmClient;
begin
  fEditor := GetCurrentEditor;
  if fEditor = nil then Exit;
  if not tbrSearch.Visible then Exit;
  if (GetFocus = fEditor.synMDI.Handle) or (GetFocus = fEditor.Handle) then
  begin
    Windows.SetFocus(cmbSearch.Handle);
  end else
  if ActiveControl = cmbSearch then
  begin
    Windows.SetFocus(fEditor.synMDI.Handle);
  end;
end;

procedure TfrmMain.cmbSearchExit(Sender: TObject);
var
  FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  FEditor.synMDI.HideSelection := True;
end;

procedure TfrmMain.cmbSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if _enteroverride then Key := #0;
  _enteroverride := False;
end;

procedure TfrmMain.SetResults(res : string);
var
  FEditor : TfrmClient;
  sl      : TStringList;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  sl := TStringList.Create;
  sl.Text := res;
  FEditor.ShowResults(True);
  FEditor.SetResults(sl);
  FreeAndNil(sl);
end;

procedure TfrmMain.AddResult(res : string);
var
  FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  FEditor.ShowResults(True);
  FEditor.AddResult(res);
end;

procedure TfrmMain.ClearResults;
var
  FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  FEditor.ClearResults;
end;

procedure TfrmMain.UpdateToolsFile(IFile : tIniFile);
var n  : Integer;
    sl : TStringList;
    i  : Integer;
    sc : Integer;

procedure ConvertValue(section, vName : string);
begin
  IFile.WriteString(section, vName,
                    StringToCString(
                      IFile.ReadString(section, vName, '')
                    )
  );
end;

begin
  sl := TStringList.Create;
  Try
    IFile.ReadSections(sl);
    for n := 0 to sl.Count - 1 do
    begin
      sc := IFile.ReadInteger(sl[n], 'Number', 0);
      for i := 1 to sc do
      begin
        ConvertValue(sl[n], 'App' + IntToStr(i));
        ConvertValue(sl[n], 'Dir' + IntToStr(i));
        ConvertValue(sl[n], 'Par' + IntToStr(i));
      end;
    end;
  Finally
    sl.Free;
  end;
  IFile.WriteInteger('Version', 'FileVersion', ToolsVers);
end;

procedure TfrmMain.actFindInFilesExecute(Sender: TObject);
begin
  if not Assigned(Grep) then
  begin
    Grep := tfrmGrepResults.Create(Self);
    panFIFContainer.InsertControl(Grep);
    Grep.BorderStyle := bsNone;
    Grep.Parent := panFIFContainer;
    Grep.Align := alClient;
  end;
  Grep.Execute(False);
end;

procedure TfrmMain.actw2kTransparentBookmarksExecute(Sender: TObject);

  procedure ToggleTransparency(toggle : Boolean; MyHandle : THandle);
  begin
    case toggle of
      False : SetLayeredWindowAttributes(MyHandle);
      True  : RemoveLayeredWindowAttributes(MyHandle);
    end;
  end;

begin
  if TransAvail then
  begin
    case TAction(Sender).Tag of
      0 : ToggleTransparency(TAction(Sender).Checked, FloatBookmarks.Handle);
      1 : ToggleTransparency(TAction(Sender).Checked, FloatFIF.Handle);
    end;
    TAction(Sender).Checked := not TAction(Sender).Checked;
  end;
end;

procedure TfrmMain.FloatBookmarksDockChanged(Sender: TObject);
begin
  actw2kTransparentBookmarks.Enabled := not FloatBookmarks.Docked;
  actw2kTransparentFindInFiles.Enabled := not FloatFIF.Docked;
end;

procedure TfrmMain.actFIFResultsExecute(Sender: TObject);
begin
  if not Assigned(Grep) then
  begin
    Grep := tfrmGrepResults.Create(Self);
    panFIFContainer.InsertControl(Grep);
    Grep.BorderStyle := bsNone;
    Grep.Parent := panFIFContainer;
    Grep.Align := alClient;
    Grep.MakeVisible;
  end;
  FloatFIF.Visible := not TAction(Sender).Checked;
end;

procedure TfrmMain.actRevertExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(fEditor) then Exit;
  if not FEditor.IsNewFile then
  begin
    if MessageDlg('Are you sure that you wish to revert to the last saved version of this file?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    begin
      FEditor.Revert;
    end;
  end;
end;

procedure TfrmMain.UpdateBrowsers;
var
  et  : TIniFile;
  n,
  i   : Integer;
  mi  : TMenuItem;
  mi2 : TMenuItem;

procedure CreateDefault;
begin
  mi := TMenuItem.Create(Self);
  mi.ImageIndex := 12;
  mi.Caption := 'Built-In (IE)';
  mi.OnClick := BrowserMenuHandler;
  mi.default := True;
  mi.Tag := 0;
  mi2 := TMenuItem.Create(Self);
  ecCopyMenuItem(mi, mi2);
  popBrowser.Items.Add(mi);
  itmPreviewHTML.Add(mi2);

  mi := TMenuItem.Create(Self);
  mi.ImageIndex := 12;
  mi.Caption := 'Built-In (Mozilla)';
  mi.OnClick := BrowserMenuHandler;
  mi.Default := False;
  mi.Tag := 1;
  mi2 := TMenuItem.Create(Self);
  ecCopyMenuItem(mi, mi2);
  popBrowser.Items.Add(mi);
  itmPreviewHTML.Add(mi2);
end;

begin
  popBrowser.Items.Clear;
  itmPreviewHTML.Clear;
  et := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'exttools.ini');
  Try
    i := et.ReadInteger('.Browsers', 'Number', 0);
    if i = 0 then begin CreateDefault; Exit; end;
    for n := 0 to i - 1 do
    begin
      mi := TMenuItem.Create(Self);
      mi.ImageIndex := 12;
      mi.Caption := et.ReadString('.Browsers', 'Browser' + IntToStr(n), 'Built-In (IE)');
      mi.OnClick := BrowserMenuHandler;
      case et.ReadBool('.Browsers', 'BuiltIn' + IntToStr(n), False) of
        True :
        begin
          mi.Tag := et.ReadInteger('.Browsers', 'IbId' + IntToStr(n), 0);
        end;
        False :
        begin
          mi.Tag := 10 + n;
          mi.ShortCut := TextToShortCut(et.ReadString('.Browsers', 'Keyb' + IntToStr(n), ''));
        end;
      end;
      if n = 0 then
      begin
        mi.default := True;
        actPreviewHTML.Tag := mi.Tag;
      end;
      mi2 := TMenuItem.Create(Self);
      ecCopyMenuItem(mi, mi2);
      popBrowser.Items.Add(mi);
      itmPreviewHTML.Add(mi2);
    end;
  Finally
    et.Free;
  end;
end;

procedure TfrmMain.BrowserMenuHandler(Sender : TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not assigned(FEditor) then exit;

  {if pos('>', FEditor.Filename) <> 0 Then
  Begin
    MessageDlg('You must save this file first.', mtInformation, [mbOK], 0);
    Exit;
  End;
  }
  
  if TMenuItem(Sender).Tag < 10 then
  begin
    case TMenuItem(Sender).Tag of
      0 : PreviewWithIE;
      1 : PreviewWithMozilla;
    end;
  end else
  begin
    Execute('.Browsers', TMenuItem(Sender).Tag - 10);
  end;
end;

procedure TfrmMain.tabsResize(Sender: TObject);
begin
  tabs.TabHeight := tabs.Canvas.TextHeight('WWWHEIGHT') + 6;
  panBack.Height := tabs.Height;
  tabs.Height := tabs.TabHeight +
                     (tabs.TabHeight *
                      Pred(tabs.RowCount) + 6);
end;

procedure TfrmMain.tabsDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var s : string;
    w : Integer;
    h : Integer;
    l : Integer;
    t : Integer;
    e : TfrmClient;
begin
  // Paint out the little white line at the bottom.
  Control.Canvas.FillRect(Rect);
  // Get the tab caption.
  s := tabs.Tabs[TabIndex];
  e := TfrmClient(tabs.Tabs.Objects[TabIndex]);
  // Should we highlight the tab text?
  if HighlightActive then
  begin
    case Active of
      True  : Control.Canvas.Font.Color := HighlightColor;
      False : Control.Canvas.Font.Color := clMenuText;
    end;
  end else
    Control.Canvas.Font.Color := clMenuText;
  // Work out where to draw the text.
  w := Control.Canvas.TextWidth(s);
  h := Control.Canvas.TextHeight('W');
  l := Rect.Left + ((Rect.Right - Rect.Left) div 2) - (w div 2);
  t := Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (h div 2);
  // Now draw it.
  Control.Canvas.TextOut(l, t, s);
  // Draw modified underline?
  if HighlightChange then
  begin
    if e.Modified then
    begin
      t := t + h + 1;
      Control.Canvas.MoveTo(l - 1, t);
      Control.Canvas.Pen.Color := ModifiedColor;
      Control.Canvas.LineTo(l + w + 1, t);
    end;
  end;
end;

procedure TfrmMain.FormFocus(Sender: TObject);
var
  DLLHandle   : THandle;
  i           : integer;
  PluginFocus : tPlgProc;
  pi          : PPluginData;
  FEditor     : TfrmClient;

  procedure CheckStartup;
  var n : integer;
      ow : Integer;
  begin
     // Let's try putting this code in the activate section!
     ow := _wnOpenFileAgain;
     //tmrStartup.Enabled := False;
     fStarted := true;
     If Assigned(OpenOnStart) then
     Begin
        For n := 0 to OpenOnStart.Count - 1 do
        begin
           OpenFile(OpenOnStart[n], '');
        end;
        FreeAndNil(OpenOnStart);
     end;
     _wnOpenFileAgain := ow;
  end;

begin
  if (not fStarted) and (IsWindow(ClientHandle)) then
     CheckStartup;

  //Check to see if files have been changed since we last had focus!
  if MDIChildCount > 0 then
  begin
    FEditor := GetCurrentEditor;
    if Assigned(FEditor) then
    begin
      FEditor.CheckAge;
    end;
  end;

  // Update whether we can paste from the clipboard or not!
  actPaste.Enabled := Clipboard.HasFormat(CF_TEXT);

  // Is this the first time the application has been run?
  If First Then
  Begin
    First:=False;
    If SetFiles Then
    Begin
      RegisterApp;
      DoSetFilesFirst;
    End;
    //tmrStartup.Enabled := True;
  End;

  // Plugins can be called whenever PN is activated...
  {$IFDEF Plugin}
  for i := 0 to Plugins.Count - 1 do
  begin
    pi := Plugins.Items[i];
    if pi^.OnGotFocus then
    begin
      DLLHandle := LoadLibrary(PChar(pi^.Filename));
      if (DLLHandle <> 0) then
      begin
        // Initialise the Plugin...
        @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
        // Make sure we pass the handle of the plugin interface...
        PluginInit(Application.Handle);
        // Pass the plugin the message number for atoms etc...
        @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
        PlgMsgNum(MsgNum);
        @PluginFocus := GetProcAddress(DLLHandle, 'OnFocus');
        PluginFocus;
        FreeLibrary(DLLHandle);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TfrmMain.FormLostFocus(Sender : TObject);
var
  DLLHandle   : THandle;
  i           : integer;
  PluginFocus : TPlgProc;
  pi          : PPluginData;
begin
  {$IFDEF Plugin}
  for i := 0 to Plugins.Count - 1 do
  begin
    pi := Plugins.Items[i];
    if pi^.OnLostFocus then
    begin
      DLLHandle := LoadLibrary(PChar(pi^.FileName));
      if (DLLHandle <> 0) then
      begin
        // Initialise the Plugin...
        @PluginInit := GetProcAddress(DLLHandle, 'Initialise');
        // Make sure we pass the handle of the plugin interface...
        PluginInit(Application.Handle);
        // Pass the plugin the message number for atoms etc...
        @PlgMsgNum := GetProcAddress(DLLHandle, 'MessageNum');
        PlgMsgNum(MsgNum);
        @PluginFocus := GetProcAddress(DLLHandle, 'OnLostFocus');
        PluginFocus;
        FreeLibrary(DLLHandle);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TfrmMain.actHelperExecute(Sender: TObject);
begin
  if not Assigned(frmHelper) then
  begin
    frmHelper := TfrmHelper.Create(Self);
    panPrjContainer.InsertControl(frmHelper);
    frmHelper.BorderStyle := bsNone;
    frmHelper.Parent := panPrjContainer;
    frmHelper.Align := alClient;
  end;
  if Sender <> frmMain then
  begin
    Case actHelper.Checked of
      False :
        begin
          frmHelper.MakeVisible;
          FloatHelper.Visible := True;
        end;
      True :
        FloatHelper.Visible := False;
    end;
  end else
  begin
    frmHelper.MakeVisible;
    FloatHelper.Visible := True;
  end;
end;

procedure TfrmMain.actHelpSearchExecute(Sender: TObject);
var q: THHFtsQuery;
begin
  q.cbStruct         := sizeof(q);      // Sizeof structure in bytes.
  q.fUniCodeStrings  := FALSE;          // TRUE if all strings are unicode.
  q.pszSearchQuery   := nil;            // String containing the search query.
  q.iProximity       := HH_FTS_DEFAULT_PROXIMITY;    // Word proximity - only one option
  q.fStemmedSearch   := FALSE;         // TRUE for StemmedSearch only.
  q.fTitleOnly       := FALSE;         // TRUE for Title search only.
  q.fExecute         := FALSE;         // TRUE to initiate the search.
  q.pszWindow        := nil;             // Window to display in

  HtmlHelp(0, PChar(mHHelp.ChmFile), HH_DISPLAY_SEARCH, DWORD(@q));
end;

procedure TfrmMain.actIndexExecute(Sender: TObject);
begin
  HtmlHelp(0, pChar(mHHelp.ChmFile), HH_DISPLAY_INDEX, 0);
end;

procedure TfrmMain.actWhatIsExecute(Sender: TObject);
begin
  sendmessage(self.handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
end;

procedure TfrmMain.WMSysCommand(var Message: TWMSysCommand);
var
  Msg : TMsg;
  HelpInfo : THelpInfo;
  W : HWND;
begin
  if Message.CmdType and $FFF0 <> SC_CONTEXTHELP then
  begin
    inherited;
    Exit;
  end;
  SetCapture (Handle);
  SetCursor (LoadCursor(0, IDC_HELP));
  while GetCapture = Handle do
  begin
    case Integer(GetMessage(Msg, 0, 0, 0)) of
      -1 : Break; {GetMessage Failed}
       0 : begin
             // Repost WM_QUIT messages
             PostQuitMessage (Msg.wParam);
             Break;
           end;
    end;
    case Msg.message of
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
      begin
        ReleaseCapture;
        {Redraw the form's non-client area to pop the help button back up...}
        SetWindowPos (Handle, 0, 0, 0,0,0, SWP_FRAMECHANGED or
           SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
        if Msg.message = WM_LBUTTONDOWN then
        begin
          W := WindowFromPoint(Msg.pt);
          if (W <> 0) and (GetWindowThreadProcessId(W, nil) = GetCurrentThreadId) then
          begin
            FillChar(HelpInfo, SizeOf(HelpInfo), 0);
            HelpInfo.cbSize := SizeOf(HelpInfo);
            HelpInfo.iContextType := HELPINFO_WINDOW;
            HelpInfo.hItemHandle := W;
            HelpInfo.MousePos := Msg.pt;
            SendMessage (Handle, WM_HELP, 0, lParam(@HelpInfo));
            Exit;
          end;
        end;
        Break;
      end;
    else
      TranslateMessage (Msg);
      DispatchMessage (Msg);
    end;
  end;
  // In case loop exited without releasing capture...
  if GetCapture = Handle then ReleaseCapture;
end;

procedure TfrmMain.WMHelp(var Message: TWMHelp);
  function GetMenuHelpContext(Menu : TMenu) : Integer;
  begin
    Result := 0;
    if Menu = nil then Exit;
    Result := Menu.GetHelpContext(Message.HelpInfo.iCtrlId, True);
    if Result = 0 then
      Result := Menu.GetHelpContext(Message.HelpInfo.hItemHandle, False);
  end;

var
  Control: TWinControl;
  Control2 : TControl; //added
  ContextID : Integer;
  Pt : TSmallPoint;
begin
  if csDesigning in ComponentState then
    inherited
  else
  begin
    Pt.x := 0;
    with Message.HelpInfo^ do
    begin
      if iContextType = HELPINFO_WINDOW then
      begin
        // changes begin
        Control := FindControl(hItemHandle);
        ContextID := 0;
        // special TB97 Handler
        if Assigned(Control) then
        begin
          Control2 := Control.ControlAtPos(Control.ScreenToClient(MousePos), True, False);
          if Control2 is TToolbarButton97 then
          begin
            ContextID := TToolbarButton97(Control2).HelpContext;
            Pt := PointToSmallPoint(Control2.ClientToScreen(Point(0, 0)));
          end;
        end;
        while (ContextID = 0) and (Control <> nil) do
        begin
          ContextID := Control.HelpContext;
          Control := Control.Parent;
        end;
        if ContextID = 0 then Exit;
        if Pt.x = 0 then
          Pt := PointToSmallPoint(Control.ClientToScreen(Point(0, 0)));
        // changes end...
      end
      else
      begin
        ContextID := GetMenuHelpContext(Menu);
        if ContextID = 0 then
          ContextID := GetMenuHelpContext(PopupMenu);
        Pt := PointToSmallPoint(ClientToScreen(Point(0, 0)));
      end;
    end;
    if (biHelp in BorderIcons) then
    begin
      Application.HelpCommand(HELP_SETPOPUP_POS, Longint(Pt));
      Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID);
    end
    else
      Application.HelpContext(ContextID);
  end;
end;

procedure TfrmMain.actQuickFindInFilesExecute(Sender: TObject);
begin
  if not Assigned(Grep) then
  begin
    Grep := tfrmGrepResults.Create(Self);
    panFIFContainer.InsertControl(Grep);
    Grep.BorderStyle := bsNone;
    Grep.Parent := panFIFContainer;
    Grep.Align := alClient;
  end;

  if cmbSearch.Text <> 'Search...' then
    Grep.Execute(False, cmbSearch.Text)
  else
    Grep.Execute(False, '');
end;

procedure TfrmMain.VisualStudioMenus;
  procedure MoveToEdit(inspoint : Integer; item : TMenuItem);
  begin
    mnuSearch.Remove(item);
    mnuEdit.Insert(inspoint, item);
  end;
var edinspt : Integer;
begin
  // Try and make PN look a bit like Visual Studio in the Menu department...

  // Firstly, move all the search items to their rough VS equivalents...
  edinspt := 10;
  sepVS1.Visible := True;
  MoveToEdit(edinspt, itmFind);
  Inc(edinspt);
  MoveToEdit(edinspt, itmFindNext);
  Inc(edinspt);
  MoveToEdit(edinspt, itmReplace);
  Inc(edinspt);
  MoveToEdit(edinspt, itmUseRegex);
  Inc(edinspt);
  MoveToEdit(edinspt, sepJump);
  Inc(edinspt);
  MoveToEdit(edinspt, itmGotoLine);
  Inc(edinspt);
  MoveToEdit(edinspt, sepFIF);
  Inc(edinspt);
  MoveToEdit(edinspt, sepHex);
  Inc(edinspt);
  MoveToEdit(edinspt, itmHexGoto);
  Inc(edinspt);
  MoveToEdit(edinspt, itmJump);
  Inc(edinspt);
  MoveToEdit(edinspt, itmFileFind);
  sepVS2.Visible := True;
  mnuSearch.Visible := False;
end;

// This function writes all of the keyboard shortcuts of the actions in
// the ActionList control of frmMain to the registry.
procedure TfrmMain.SaveKeyShortcuts;
var i : Integer;
    r : TRegistry;
    {$IFDEF WRITEKEYSHORTCUTS}
    n : TIniFile;
    {$ENDIF}
begin
  //KeybKey
  r := TRegistry.Create;
  {$IFDEF WRITEKEYSHORTCUTS}
  n := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'current.key');
  {$ENDIF}
  if r.OpenKey(KeybKey, True) then
  begin
    try
      for i := 0 to ActionList.ActionCount - 1 do
      begin
        r.WriteInteger(ActionList.Actions[i].Name, TAction(ActionList.Actions[i]).ShortCut);
        {$IFDEF WRITEKEYSHORTCUTS}
        n.WriteString(ActionList.Actions[i].Category, ActionList.Actions[i].Name, ShortCutToText(TAction(ActionList.Actions[i]).ShortCut));
        {$ENDIF}
      end;
    finally
      r.CloseKey;
    end;
  end;
  r.Free;
  {$IFDEF WRITEKEYSHORTCUTS}
  n.Free;
  {$ENDIF}
end;

procedure TfrmMain.LoadKeyShortcuts;
var i : Integer;
    r : TRegistry;
begin
  r := TRegistry.Create;
  if r.OpenKey(KeybKey, True) then
  begin
    try
      for i := 0 to ActionList.ActionCount - 1 do
      begin
        try
          TAction(ActionList.Actions[i]).ShortCut := Word(r.ReadInteger(ActionList.Actions[i].Name));
        except
          on ERegistryException do;
        end;
      end;
    finally
      r.CloseKey;
    end;
  end;
  r.Free;
end;

procedure TfrmMain.actDeleteFileExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor;
  if not Assigned(FEditor) then Exit;
  if GetSetting('WarnDeleteFile', RootKey, True) then
    if MessageDlg('Are you sure you wish to delete this file?'+#13+#10+FEditor.Filename, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
      Exit;
  if FEditor.IsNewFile then CloseFile else
  begin
    DeleteFile(FEditor.FileName);
    FEditor.Modified := False;
    CloseFile;
  end;
end;

procedure TfrmMain.FloatHelperRecreated(Sender: TObject);
begin
  if frmHelper <> nil then
  begin
    try frmHelper.ResetBrowseFolders(nil) except end;
  end;
end;

procedure TfrmMain.actViewAsHTMLExecute(Sender: TObject);
var
   FEditor : TfrmClient;
   FResEd  : TfrmClient;
   mems    : TMemoryStream;
   osf     : TSaveFormat;
begin
  FEditor := GetCurrentEditor;
  if Assigned(FEditor) then
  begin
    if FEditor.Mode <> emNormal then Exit;
    mems := TMemoryStream.Create;
    osf := FEditor.synMDI.SaveFormat;
    try
      FEditor.synMDI.SaveFormat := sfHTML;
      FEditor.synMDI.SaveToStream(mems);
      mems.Seek(0,0);
      FResEd := NewType(Parsers.HTML, '<new html>');
      FResEd.synMDI.LoadFromStream(mems);
      FResEd.Modified := False;
    finally
      FEditor.synMDI.SaveFormat := osf;
      mems.Free;
    end;
  end;
end;

function TfrmMain.GetPreviewFileName(ed : TfrmClient) : string;
var
   tp       : PChar;
   temppath : string;
begin
  if ed.IsNewFile then
  begin
    GetMem(tp, MAX_PATH + 1);
    GetTempPath(MAX_PATH, tp);
    temppath := tp;
    FreeMem(tp, MAX_PATH + 1);
    result := GenTempFile('pn', temppath, 0);
    fTempFiles.Add(Result);
    ed.synMDI.SaveToFile(Result);
  end else
  if ed.Modified then
  begin
    temppath := ExtractFilePath(ed.FileName);
    result := GenTempFile('pn', temppath, 0);
    fTempFiles.Add(Result);
    ed.synMDI.SaveToFile(Result);
  end else
    Result := ed.FileName;
end;

procedure TfrmMain.actTabsToSpacesExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor();
  if not Assigned(FEditor) then
    Exit;
  FEditor.TabsToSpaces;
end;

procedure TfrmMain.actSpacesToTabsExecute(Sender: TObject);
var FEditor : TfrmClient;
begin
  FEditor := GetCurrentEditor();
  if not Assigned(FEditor) then
    Exit;
  FEditor.SpacesToTabs;
end;

end.

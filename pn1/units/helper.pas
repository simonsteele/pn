{*******************************************************************************
 *
 * Unit Name: helper
 * Purpose  : Floating Helper Form... Projects & Text-Clips...
 * Author   : Simon Steele - Echo Software
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			      agreement at www.pnotepad.org/press/psidx.html.
 * History  : 22/09/2000 - Creation - Notebook and Tabs plus Projects tree.
 *            23/09/2000 - Decided on .INI format for project files. Easily
 *                         edited by humans, and easy to read-in write-out. Also
 *                         it's easy to modify the format and keep backwards
 *                         compatibility. Imported loads of code from the MTQEdit
 *                         projects helper. Needs modifications so it works with
 *                         more than one file type...
 *            24/09/2000 - Removed the Notebook, and replaced with multiple
 *                         panels. This prevents a redraw problem. Only remaining
 *                         interface problem is when showing docked on creation,
 *                         the tab control always appears half-covered...
 *            12/10/2000 - Unfortunately, MTQEdit used a hard-coded set of clip
 *                         files whereas PN must have an unlimited custom number
 *                         of files. List of available clip files stored in a
 *                         stringlist after unsuccessful attempt at using pointers
 *                         to pChars. Found nowhere to store the pointer with the
 *                         combo box item.
 *            02/01/2001 - Removed the tabs for switching the view and just left
 *                         the large title in place. Also added a current files
 *                         view.
 *            01/04/2001 - Added the file browse helper (hopefully). Success!
 *            11/04/2001 - Added full-size toggle to helper window menu.
 *            10/04/2003 - Woo! Just under two years later. Fixed a Setup bug
 *                         where the projects directory could be set to the
 *                         clips directory. Also fixed the renaming current file
 *                         problem. SF Bug: #704375.
 *
 ******************************************************************************}


unit helper;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, ExtCtrls, Menus, IniFiles, AgOpenDialog, ImgList, Registry,
  Shellapi, StdCtrls, EnhCBox, ActnList, TB97Ctls, BrowseDr, CommCtrl;

type
  TfrmHelper = class(TForm)
    popProjects: TPopupMenu;
    dlgAddFiles: TAgOpenDialog;
    ilsFiles: TImageList;
    panProjects: TPanel;
    tbrProjects: TToolBar;
    Projects: TTreeView;
    panTextClips: TPanel;
    cmbPick: TImgComboBox;
    lstClips: TListBox;
    ilsClips: TImageList;
    panBack: TPanel;
    panFront: TPanel;
    ActionList1: TActionList;
    actShowBigTitle: TAction;
    ToolbarButton971: TToolbarButton97;
    popTabs: TPopupMenu;
    itmProjects: TMenuItem;
    itmTextClips: TMenuItem;
    ToolButton1: TToolButton;
    ilsToolbar: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    actAddProject: TAction;
    actAddFiles: TAction;
    actExpand: TAction;
    actContract: TAction;
    panCurrent: TPanel;
    lstCurrent: TListView;
    itmCurrentFiles: TMenuItem;
    popHelper: TPopupMenu;
    Help1: TMenuItem;
    panBrowser: TPanel;
    tbrBrowse: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    treBrowse: TTreeView;
    dlgBrowse: TdfsBrowseDirectoryDlg;
    itmBrowser: TMenuItem;
    actChooseRoot: TAction;
    actRefreshBrowser: TAction;
    itmFullSize: TMenuItem;
    N1: TMenuItem;
    ToolButton10: TToolButton;
    actFullSize: TAction;
    N2: TMenuItem;
    FullSize1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ProjectsClick(Sender: TObject);
    procedure popProjectsPopup(Sender: TObject);
    procedure ProjectsEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure ProjectsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstClipsDblClick(Sender: TObject);
    procedure cmbPickChange(Sender: TObject);
    procedure popTabsPopup(Sender: TObject);
    procedure itmProjectsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actAddProjectExecute(Sender: TObject);
    procedure actAddFilesExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
    procedure actContractExecute(Sender: TObject);
    procedure lstCurrentResize(Sender: TObject);
    procedure lstCurrentClick(Sender: TObject);
    procedure treBrowseCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure actChooseRootExecute(Sender: TObject);
    procedure treBrowseExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure treBrowseClick(Sender: TObject);
    procedure actRefreshBrowserExecute(Sender: TObject);
    procedure treBrowseCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure treBrowseCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure treBrowseCollapsed(Sender: TObject; Node: TTreeNode);
    procedure treBrowseExpanded(Sender: TObject; Node: TTreeNode);
    procedure popHelperPopup(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure actFullSizeExecute(Sender: TObject);
    procedure Help1Click(Sender: TObject);
  private
    { Private declarations }
    ext_known,
    ProjList,
    ProjFiles,
    ProjNameList  : TStringList;
    fList         : TStringList; {Clips List}
    cList         : TStringList;
    ProjDir       : String;
    ClipDir       : String;
    SchemeDir     : string;
    CurrentItem   : String;
    CurrentParent : String;
    CurrentNode   : TTreeNode;
    BuildProject  : TTreeNode;
    CurrentPage   : Integer;
    _NoBrowseRoot : Boolean;
    _DisableClick : Boolean;
    sBrowseRoot   : string;
    function Parse(FileName: String; var ProjCompileTarget : String): String;
    procedure GetProjects;
    function KillProjects: Boolean;
    procedure AddFiles(CallType: Integer);
    procedure DeleteProject(Sender: tObject);
    function GetProjIndex(Name: String): Integer;
    procedure NewProject(Sender: TObject);
    procedure prjAddCFile(Style: Integer);
    procedure prjAddCurrent(Sender: tObject);
    procedure prjAddFiles(Sender: TObject);
    procedure prjAddFiles2(Sender: tObject);
    procedure prjDeleteFile(Sender: tObject);
    procedure prjRemoveFile(Sender: tObject);
    procedure RenameProject(Sender: tObject);
    procedure Setup;
    function GetIconIndex(filename: string): Integer;
    procedure Shutdown;
    procedure FindClipFiles;
    function GetClipName(fn: string): string;
    procedure LoadClips(Index: Integer);
    procedure FreeClipList;
    function InsIndent(S: String; I: Integer): String;
    function GetClipImage(Dir, fn : string) : Integer;
    function AddClipImage(fn: string): Integer;
    procedure PageChange(Sender: TObject);
    procedure OpenEntireProject(Sender: TObject);
    procedure PropagateBrowser;
    procedure LoadDir(sDir: string; tnRoot: TTreeNode);
    procedure ClearBrowser(tr : TTreeNode);
    procedure prjBuildFile(Sender: tObject);
    procedure SetBuildProject(Sender: TObject);
  public
    { Public declarations }
    procedure UpdateCurrent;
    procedure UpdateCurrentSelection(Index: integer);
    procedure MakeVisible;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure LoadProjects;
    procedure ResetBrowseFolders(tr : TTreeNode);
    function GetBuildFile: string;
  end;

var
  frmHelper: TfrmHelper;

implementation

{$R *.DFM}

{ TfrmHelper }

uses Main, useful, editor, newtool;

{$include geticons.inc}

procedure TfrmHelper.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := frmMain.panPrjContainer.Handle;
  Params.Style := WS_CHILD or WS_CLIPSIBLINGS;
  Params.X := 0;
  Params.Y := 0;
  Params.Width := frmMain.panPrjContainer.ClientWidth;
  Params.Height := frmMain.panPrjContainer.ClientHeight;
end;

procedure TfrmHelper.MakeVisible;
begin
  Visible := True;
  FormResize(Self);
end;

procedure TfrmHelper.FormResize(Sender: TObject);
begin
  SetBounds(0,0, frmMain.panPrjContainer.Width, frmMain.panPrjContainer.Height);
  Projects.Invalidate;
  lstCurrent.Invalidate;
  tbrProjects.Invalidate;
  //treBrowse.Invalidate;
  tbrBrowse.Invalidate;
end;

procedure TfrmHelper.Setup;
var locpath : string;
    reg     : TRegistry;
    i       : Integer;
    s       : string;
begin
  locpath := ExtractFilePath(ParamStr(0));
  reg := TRegistry.Create;
  Try
    reg.OpenKey(RootKey, True);
    if reg.ValueExists('FullSizeHelper') then
      frmMain.FloatHelper.FullSize := reg.ReadBool('FullSizeHelper') else
      frmMain.FloatHelper.FullSize := True; // Default for first install.
    // Find out where the projects are stored...
    If reg.ValueExists('ProjectsFolder') then
      ProjDir := reg.ReadString('ProjectsFolder')
    else
      ProjDir := locpath + 'Projects\';
    // Find out where the Clips are stored...
    If reg.ValueExists('ClipsFolder') then
      ClipDir := reg.ReadString('ClipFolder')
    else
      ClipDir := locpath + 'Clips\';
    if reg.ValueExists('SchemesFolder') then
      SchemeDir := reg.ReadString('SchemesFolder')
    else
      SchemeDir := locpath + 'Schemes\';
    // Get the last used helper tab...
    If reg.ValueExists('LastHelperTab') then
    begin
      CurrentPage := reg.ReadInteger('LastHelperTab');
      PageChange(Self);
    end;
    If reg.ValueExists('LastClipIndex') then
      i := reg.ReadInteger('LastClipIndex')
    else
      i := -1;
    // Get the last used browse directory..
    if reg.ValueExists('LastBrowseDir') then
      s := reg.ReadString('LastBrowseDir')
    else
      s := '';
  Finally
    reg.Free;
  end;
  treBrowse.ShowRoot := False;
  If not (length(ProjDir) > 0) Then ProjDir := locpath + 'Projects\';
  If not (copy(ProjDir, Length(ProjDir), 1) = '\') Then
    ProjDir := ProjDir + '\';
  If not (Length(ClipDir) > 0) then ClipDir := locpath + 'Clips\';
  If not (Copy(ClipDir, Length(ClipDir), 1) = '\') Then
    ClipDir := ClipDir + '\';
  if not (Length(SchemeDir) > 0) then SchemeDir := locpath + 'Schemes\';
  If not (Copy(SchemeDir, Length(SchemeDir), 1) = '\') Then
    SchemeDir := SchemeDir + '\';
  If not DirExists(ProjDir) Then
  Begin
    If (not DirExists(locpath + 'Projects')) Then
      MkDir(locpath + 'Projects');
    ProjDir := locpath + 'Projects\';
    { TODO -oSimon -cFirst-Time Functionality : Make a default project to create from here. }
    {If not FileExists(locpath + 'Projects\template.mep') Then
    CreateDefProj(locpath + 'Projects\template.mep', locpath);}
  End;
  If not DirExists(ClipDir) Then
  Begin
    If (not DirExists(locpath + 'Clips')) Then
      MkDir(locpath + 'Clips');
    ClipDir := locpath + 'Clips\';
  End;
  if not DirExists(SchemeDir) Then
  begin
    if (not DirExists(locpath + 'Schemes')) Then
      MkDir(locpath + 'Schemes');
    SchemeDir := locpath + 'Schemes\';
  end;
  LoadProjects;
  FindClipFiles;
  if (i > -1) and (i < cmbPick.Items.Count) then cmbPick.ItemIndex := i;
  If (cmbPick.ItemIndex < 0) and (cmbPick.Items.Count > 0) then cmbPick.ItemIndex := 0;
  If cmbPick.Items.Count > 0 then
    LoadClips(cmbPick.ItemIndex);
  UpdateCurrent;
  if DirExists(s) then
  begin
    sBrowseRoot := s;
    _NoBrowseRoot := False;
    PropagateBrowser;
  end;
end;

procedure TfrmHelper.Shutdown;
var reg : TRegistry;
begin
  reg := TRegistry.Create;
  Try
    reg.OpenKey(RootKey, True);
    reg.WriteInteger('LastHelperTab', CurrentPage);
    reg.WriteInteger('LastClipIndex', cmbPick.ItemIndex);
    reg.WriteString('LastBrowseDir', sBrowseRoot);
    reg.WriteBool('FullSizeHelper', frmMain.FloatHelper.FullSize);
  Finally
    reg.Free;
  End;
end;

procedure TfrmHelper.PageChange(Sender: TObject);
var n : Integer;
begin
  // First hide *all* panels, then show the right one!
  for n := 0 to Self.ControlCount - 1 do
  begin
    if Self.Controls[n] is TPanel then
       if (Self.Controls[n] <> panBack) and (Self.Controls[n] <> panFront) then
          Self.Controls[n].Visible := False;
  end;
  Case CurrentPage of
    0 :
    begin
      panProjects.Visible := True;
      panFront.Caption := 'Projects';
    end;
    1 :
    begin
      panTextClips.Visible := True;
      panFront.Caption :='Text Clips';
    end;
    2 :
    begin
      panCurrent.Visible := True;
      panFront.Caption := 'Open Files';
    end;
    3 :
    begin
      panBrowser.Visible := True;
      panFront.Caption := 'Browser';
    end;
  end;
end;

function TfrmHelper.GetIconIndex(filename : string) : Integer;
var
  {for icon finding}
  ext   : string;
  Icon  : tIcon;
  small : THandle;
  p     : Integer;
begin
  Result := 2;
  If not assigned(ext_known) Then ext_known := tStringList.Create;
  if (pos('.',filename)<>0) then
  begin
      //ext := lowercase(copy(item.caption, posn('.', item.caption,-1),
      //                                               length(item.caption)));
      ext := ExtractFileExt(filename);
      if ext_known.find(ext,p) then
      begin
         result := integer(ext_known.objects[p]);
      end else
      begin
         small := GetIconIndirect(ext, ShellSmall);
         if small<>0 then
         begin
           ext_known.add(ext);
           icon := Ticon.create;
           icon.handle := small;
           p := ilsFiles.addicon(icon);
           icon.free;
           ext_known.objects[ext_known.count-1] := TObject(p);
           result := p;
         end;
      end;
     {end Get System Image}
  end;
end;

Procedure TfrmHelper.LoadProjects;
var TempNode, RootNode : tTreeNode;
    n,
    o,
    ImgIndex : Integer;
    Pt       : PChar;
    ts       : String;
    PCT      : String;
begin { SetTBRPos }
   If not assigned(ProjList) Then ProjList := TStringList.Create;
   If not assigned(ProjNameList) Then ProjNameList := TStringList.Create;
   { TODO -oSimon -cVisual : Allow configuration of Font here... }
   Projects.Font.Name := 'MS Sans Serif';
   Projects.Font.Size := 8;
   Projects.Items.BeginUpdate;
   {Read in Projects}
   GetProjects;
   RootNode := Projects.Items.AddChildFirst(Nil, 'Projects');
   RootNode.ImageIndex := 0;
   RootNode.SelectedIndex := 0;
   ProjNameList.Clear;
   For n := 0 to ProjList.Count - 1 do
   Begin { For n :=.. }
      ProjNameList.Add(Parse(ProjList[n], PCT));
      RootNode := Projects.Items.AddChild(Nil, ProjNameList[n]);
      RootNode.ImageIndex := 1;
      RootNode.SelectedIndex := 1;

      // Add support for project compile targets...
      if Length(PCT) = 0 then
        RootNode.Data := nil
      else begin
        GetMem(Pt, Length(PCT)+1);
        StrPCopy(Pt, PCT);
        RootNode.Data := Pt;
      end;

      For o := 0 to ProjFiles.Count -1 do
      Begin { For o :=.. }
         ts := ExtractFileName(ProjFiles[o]);
         TempNode := Projects.Items.AddChild(RootNode, ts);
         ImgIndex := GetIconIndex(ts);
         TempNode.ImageIndex := ImgIndex;
         TempNode.SelectedIndex := ImgIndex;
         GetMem(Pt, 256);
         StrPCopy(Pt, ProjFiles[o]);
         TempNode.Data := Pt;
      End; { For o :=.. }
   End; { For n :=.. }
   Projects.Items.EndUpdate;
   { - bit of a hash, Make sure the Projects is redrawn}
    Application.ProcessMessages;
End;

procedure TfrmHelper.GetProjects;
var
   SRes : integer;
   SRec : TSearchRec;
begin { GetProjects }
  If not Assigned(ProjList) Then ProjList := tStringList.Create;
  ProjList.Clear;
  SRes := FindFirst(ProjDir + '*.pnp',(faAnyFile-faDirectory), SRec);
  repeat
    if SRes = 0 then
    begin
      ProjList.Add(SRec.Name);
      SRes := FindNext(SRec);
    end;
  until SRes <> 0;
  FindClose(SRec);
end;

function TfrmHelper.Parse(FileName : String; var ProjCompileTarget : String) : String;
var Proj      : tIniFile;
    CurrName,
    ChkVer    : String;
    CompileTarget : String;
    ErrorStat : tStringList;
    i,
    Files     : integer;
    ret       : Word;
begin { Parse }
   ChkVer := '-1';
   ErrorStat := tStringList.Create;
   ErrorStat.Clear;
   If not assigned(ProjFiles) Then ProjFiles := tStringList.Create;
   ProjFiles.Clear;
   ProjCompileTarget := '';
   {$I-}
   Proj := tIniFile.Create(ProjDir + FileName);
   {$I+}
   If IOResult <> 0 Then
   Begin
      frmMain.logMain.Add('projparseerror', Format('IO Error: %d Opening File: %s',
                                           [IOResult, FileName]));
      ret := MessageDlg('There was an error opening the selected ' +
                        'project file.',mtError,[mbOK,mbRetry],17);
      Case ret of
        mrRetry : Begin
                    {$I-}
                    Proj := tIniFile.Create(ProjDir + FileName);
                    {$I+}
                    If IOResult <> 0 Then
                    Begin
                       MessageDLG('The selected project still cannot be opened.',
                                  mtError, [mbOK], 17);
                       ErrorStat.Free;
                       Exit;
                    End;
                  End;
        mrOK : Begin
                  ErrorStat.Free;
                  Exit;
               End;
      end;
   end;

   CurrName := Proj.ReadString('Project', 'Name', 'error-1-1');
   If CurrName = 'error-1-1' Then
     ErrorStat.Add('No project name defined');

   ChkVer := Proj.ReadString('Project', 'FileVersion', '-1');
   If ChkVer = '-1' Then
     ErrorStat.Add('Project file version not found');

   CompileTarget := Proj.ReadString('Project', 'CompileTarget', '');
   if CompileTarget <> '' then
     ProjCompileTarget := CompileTarget;

   {Insert Code here to deal with changes to the file-format}
   Files := Proj.ReadInteger('Files', 'NumFiles', -1);
   If Files = -1 Then
     ErrorStat.Add('Invalid number of project files');

   For i := 0 to Files -1 do
   Begin { For i :=.. }
      ProjFiles.Add(Proj.ReadString('Files','File' + inttostr(i), 'error-2-2'));
      If ProjFiles[i] = 'error-2-2' Then ErrorStat.Add('Error in a filename');
   End; { For i :=.. }

   Proj.Free;

   If ErrorStat.Count > 0 Then
   For i := 0 to ErrorStat.Count - 1 Do
   Begin
      MessageDlg('Error: ' + ErrorStat[i] + ' in Project: ' + FileName,mtError,
                                                                      [mbOK],0);
      frmMain.logMain.Add('projparseerror', ErrorStat[i] + ' in Project: ' + FileName);
   End;
   ErrorStat.Free;

   Parse := CurrName;
end;

function tfrmHelper.KillProjects : Boolean;
var n  : integer;
    pt : pChar;
begin
  Projects.Items.BeginUpdate;
  For n := 0 to Projects.Items.Count - 1 do
  Begin { For n :=.. }
     If Projects.Items[n].Parent <> nil then
     begin
        pt := Projects.Items[n].data;
        FreeMem(pt, 256);
        Projects.Items[n].data := nil;
     end else
     if Projects.Items[n].Data <> nil then
     begin
       FreeMem(Projects.Items[n].data);
       Projects.Items[n].data := nil;
     end;
  end; { For n :=.. }
  Projects.Items.Clear;
  Projects.Items.EndUpdate;
  Result := True;
end;

procedure TfrmHelper.ProjectsClick(Sender: TObject);
var ChangeNode : tTreeNode;
    pTemp      : pChar;
    sTemp      : String;
    n          : Integer;
begin
  frmMain.StatusBar1.Panels[statPanel].Text := '';
  ChangeNode := Projects.Selected;
  If ChangeNode = Projects.TopItem Then
     Projects.ReadOnly := True Else
     Projects.ReadOnly := False;
  If Assigned(ChangeNode) Then
     If ChangeNode.Parent <> nil Then
     Begin
        pTemp := ChangeNode.Data;
        sTemp := StrPas(pTemp);
        for n := 0 to frmMain.MDIChildCount - 1 do
        begin
          If uppercase(tfrmClient(frmMain.MDIChildren[n]).Filename) = uppercase(sTemp) then
          begin
            frmMain.MDIChildren[n].BringToFront;
            Break;
          end;
        end;
        frmMain.StatusBar1.Panels[statPanel].Text := sTemp;
     End;
  actAddFiles.Enabled := Assigned(ChangeNode) and (Projects.ReadOnly = False);
end;

procedure TfrmHelper.SetBuildProject(Sender : TObject);
var
  ChangeNode : tTreeNode;
begin
  ChangeNode := CurrentNode;
  if ChangeNode.Parent = nil then
  begin
    BuildProject := ChangeNode;
  end;
end;

procedure TfrmHelper.OpenEntireProject(Sender : TObject);
var ChangeNode : tTreeNode;
    lcount : Integer;
    ReturnResult : Word;
    pt : PChar;
begin { Open Entire Project }
  if frmMain.GetSetting('WarnOpenProj', RootKey, True) then
  begin
    ReturnResult := MessageDlg('Are you sure you wish to open this entire project?',
                               mtConfirmation,[mbYes,mbNo],0);
    case ReturnResult of
      mrYes : {Carry On};
      mrNo : Exit;
    end; { case ReturnResult of }
  end;
  ChangeNode := CurrentNode;
  If ChangeNode.Parent = nil Then
  Begin
    Projects.Items.Beginupdate;
    For lcount := 0 to Projects.Items.Count - 1 do
      If not (Projects.Items[lcount].Parent = nil) Then
      If Projects.Items[lcount].Parent = ChangeNode Then
      Begin
        Pt := Projects.Items[lcount].data;
        frmMain.OpenFile(pt, '');
      end;
    Projects.Items.EndUpdate;
  end;
end;

procedure TfrmHelper.popProjectsPopup(Sender: TObject);
var n          : Integer;
    ChangeNode : tTreeNode;
    mi         : Array[0..9] of tMenuItem;
    target     : String;
    current    : String;
begin { popProjectsPopup }
   for n := popProjects.Items.Count - 1 downto 0 do
      popProjects.Items.Delete(0);
   For n := 0 to 9 do
      mi[n] := tMenuItem.Create(popProjects);
   ChangeNode := Projects.Selected;
   CurrentItem := ChangeNode.Text;
   If ChangeNode.Parent <> nil Then
      CurrentParent := ChangeNode.Parent.Text;
   CurrentNode := Projects.Selected;
   If (ChangeNode.Parent = nil) And (ChangeNode.Text = 'Projects') Then
                             Begin
                                mi[0].Caption := 'New Project';
                                mi[0].OnClick := NewProject;
                                mi[0].ImageIndex := 0;
                                popProjects.Items.Add(mi[0]);
                             End Else
                             If ChangeNode.Parent = nil Then
                             Begin
                                mi[0].Caption := 'Open Project';
                                mi[0].OnClick := OpenEntireProject;
                                mi[0].ImageIndex := 30;
                                popProjects.Items.Add(mi[0]);
                                if ChangeNode.Data <> nil then
                                begin
                                  mi[8].Caption := 'Set Build Project';
                                  mi[8].OnClick := SetBuildProject;
                                  mi[8].ImageIndex := 32;
                                  if BuildProject = ChangeNode then
                                    mi[8].Checked := true;
                                  popProjects.Items.Add(mi[8]);
                                end;
                                mi[1].Caption := '-';
                                popProjects.Items.Add(mi[1]);
                                mi[2].Caption := 'New Project';
                                mi[2].OnClick := NewProject;
                                mi[2].ImageIndex := 0;
                                popProjects.Items.Add(mi[2]);
                                mi[3].Caption := 'Delete Project';
                                mi[3].OnClick := DeleteProject;
                                popProjects.Items.Add(mi[3]);
                                mi[4].Caption := 'Rename Project';
                                mi[4].OnClick := RenameProject;
                                mi[4].ImageIndex := 36;
                                popProjects.Items.Add(mi[4]);
                                mi[5].Caption := '-';
                                popProjects.Items.Add(mi[5]);
                                mi[6].Caption := 'Add Files';
                                mi[6].ImageIndex := 13;
                                mi[6].OnClick := prjAddFiles;
                                popProjects.Items.Add(mi[6]);
                                mi[7].Caption := 'Add Current File';
                                mi[7].OnClick := prjAddCurrent;
                                popProjects.Items.Add(mi[7]);
                             End Else
                             Begin
                                mi[0].Caption := 'Remove File';
                                mi[0].OnClick := prjRemoveFile;
                                mi[0].ImageIndex := 14;
                                popProjects.Items.Add(mi[0]);
                                mi[1].Caption := 'Delete File';
                                mi[1].OnClick := prjDeleteFile;
                                popProjects.Items.Add(mi[1]);

                                mi[2].Caption := 'Set Build File';
                                mi[2].OnClick := prjBuildFile;
                                mi[2].ImageIndex := 32;
                                if ChangeNode.Parent.Data <> nil then
                                begin
                                  target := PChar(ChangeNode.Parent.Data);
                                  current := PChar(ChangeNode.Data);
                                  if target = current then
                                    mi[2].Checked := true;
                                end;
                                popProjects.Items.Add(mi[2]);

                                mi[3].Caption := '-';
                                popProjects.Items.Add(mi[3]);
                                mi[4].Caption := 'Add Files';
                                mi[4].OnClick := prjAddFiles2;
                                mi[4].ImageIndex := 13;
                                popProjects.Items.Add(mi[4]);
                                mi[5].Caption := 'Add Current File';
                                mi[5].OnClick := prjAddCurrent;
                                popProjects.Items.Add(mi[5]);
                             End;
end;

procedure TfrmHelper.ProjectsEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
var ChangeNode : tTreeNode;
    SelItem, nItems, n : Integer;
    prIni : tIniFile;
    itemData : PChar;
    orgFile, fstr, newpath : String;
    rFile : File;
Begin { ProjectsEdited }
   //ChangeNode := Projects.Selected;
   ChangeNode := CurrentNode; // Might fix that big bad bug...
   If ChangeNode.Parent = Nil then
   begin
     {Rename Projects}
     SelItem := getprojindex(ChangeNode.Text);
     prIni := tIniFile.Create(ProjDir + ProjList[selitem]);
     prIni.WriteString('Project', 'Name', S);
     prIni.Free;
     projNameList[selitem] := S;
   end else
   begin
     {Rename for individual files}
     SelItem := getprojindex(ChangeNode.Parent.Text);
     prIni := tIniFile.Create(ProjDir + ProjList[selitem]);
     nItems := prIni.ReadInteger('Files', 'NumFiles', -1);
     If nItems = -1 Then Exit; {!NYI!}{Error handling Please!!!}
     itemData := ChangeNode.Data;
     orgfile := StrPas(itemData);
     For n := 0 to nItems -1 do
     begin { For n :=.. }
       fstr := prIni.ReadString('Files', 'File' + inttostr(n), 'error');
       if fstr = orgfile Then
       begin
         newpath := ExtractFilePath(orgfile) + S;
         prIni.WriteString('files', 'File' + inttostr(n), newpath);
         StrPCopy(itemData, newpath);
         AssignFile(rfile, orgfile);
         Rename(rfile, newpath);
         Break;
       end;
     end; { For n :=.. }
     prIni.Free;
   end;
end;

function TfrmHelper.GetProjIndex(Name : String) : Integer;
var curindex, tempint : integer;
begin
   tempint := -1;
   For curindex := 0 to ProjNameList.Count - 1 do
          If ProjNameList[curindex] = name then tempint := curindex;
   getprojindex := tempint;
end;

procedure tfrmHelper.NewProject(Sender: TObject);
var NewProject : tTreeNode;
    NewINI : tIniFile;
    prjname : string;
    frmNewProject : TfrmNewToolLink;
begin { NewProject }
   frmNewProject := TfrmNewToolLink.Create(Self);
   If frmNewProject.ExecuteProject(frmMain.Top, frmMain.Left, frmMain.Height, frmMain.Width) then
   begin
     NewProject := Projects.Items.AddChild(nil, 'New Project');
     NewProject.ImageIndex := 1;
     NewProject.SelectedIndex := 1;
     prjname := frmNewProject.txtCaption.text;
     NewProject.Text := prjname;
     NewINI := tINIFile.Create(ProjDir + copy(prjname, 1, 20 ) + '.pnp');
     NewINI.WriteString('Project', 'Name', prjName);
     NewINI.WriteString('Project', 'FileVersion', '1');
     NewINI.WriteInteger('Files', 'NumFiles', 0);
     NewINI.Free;
     ProjList.Add(copy(prjname, 1, 20) + '.pnp');
     ProjNameList.Add(prjname);
   end;
end;

procedure tfrmHelper.prjRemoveFile(Sender : tObject);
var ChangeNode                  : tTreeNode;
    SelProj, numFiles, index, n : Integer;
    ProjFile, filename          : String;
    ProjIni                     : tIniFile;
    prFiles                     : tStringList;
    pFilename                   : pChar;
begin
   prFiles := tStringList.Create;
   ChangeNode := CurrentNode;
   SelProj := GetProjIndex(currentparent);
   ProjFile := ProjList[SelProj];
   ProjIni := tIniFile.Create(ProjDir + ProjFile);
   numFiles := ProjIni.ReadInteger('Files', 'NumFiles', 1);
   For n := 0 to numFiles - 1 do
      prFiles.Add(ProjIni.ReadString('Files', 'File' + inttostr(n), 'error'));
   pFilename := ChangeNode.Data;
   filename := StrPas(pFilename);
   index := prFiles.IndexOf(filename);
   prFiles.Delete(index);
   ProjIni.EraseSection('Files');
   ProjIni.WriteInteger('Files', 'NumFiles', prFiles.Count);
   For n := 0 to prFiles.Count - 1 do
      ProjIni.WriteString('Files', 'File' + inttostr(n), prFiles[n]);
   ProjIni.Free;
   CurrentNode.data := nil;
   Freemem(pFilename, 256);
   ChangeNode.Delete;
   prFiles.Free;
end;

procedure tfrmHelper.prjBuildFile(Sender : tObject);
var
  ChangeNode : tTreeNode;
  ProjNode   : tTreeNode;
  ptr        : PChar;
  target     : string;
  SelProj    : Integer;
  ProjFile   : string;
  ProjIni    : tIniFile;
begin
  ChangeNode := CurrentNode;
  ProjNode := ChangeNode.Parent;

  SelProj := GetProjIndex(currentparent);
  ProjFile := ProjList[SelProj];

  ptr := ChangeNode.Data;
  target := ptr;

  {$I-}
  ProjIni := tIniFile.Create(ProjDir + ProjFile);
  {$I+}
  try
    ProjIni.WriteString('Project', 'CompileTarget', target);
  finally
    ProjIni.Free;
  end;

  if (ProjNode.Data <> nil) then
    FreeMem(ProjNode.Data);

  GetMem(ptr, Length(target)+1);
  StrPCopy(ptr, target);
  ProjNode.Data := ptr;
end;

procedure tfrmHelper.prjDeleteFile(Sender : tObject);
var ChangeNode                  : tTreeNode;
    SelProj, numFiles, index, n : Integer;
    ProjFile, filename          : String;
    ProjIni                     : tIniFile;
    prFiles                     : tStringList;
    pFilename                   : pChar;
    fFile                       : File;
    ret                         : Word;
begin
   ret := MessageDlg('Are you sure you want to delete this file?',
                     mtConfirmation,[mbYes,mbNo],0);
   Case ret of
      mrYes : ;
      mrNo : Exit;
   End;
   prFiles := tStringList.Create;
   ChangeNode := CurrentNode;
   SelProj := GetProjIndex(currentparent);
   ProjFile := ProjList[SelProj];
   ProjIni := tIniFile.Create(ProjDir + ProjFile);
   numFiles := ProjIni.ReadInteger('Files', 'NumFiles', 1);
   For n := 0 to numFiles - 1 do
      prFiles.Add(ProjIni.ReadString('Files', 'File' + inttostr(n), 'error'));
   pFilename := ChangeNode.Data;
   filename := StrPas(pFilename);
   index := prFiles.IndexOf(filename);
   prFiles.Delete(index);
   ProjIni.EraseSection('Files');
   ProjIni.WriteInteger('Files', 'NumFiles', prFiles.Count);
   For n := 0 to prFiles.Count - 1 do
      ProjIni.WriteString('Files', 'File' + inttostr(n), prFiles[n]);
   ProjIni.Free;
   CurrentNode.data := nil;
   Freemem(pFilename, 256);
   ChangeNode.Delete;
   {$I-}
     AssignFile(fFile, filename);
     Erase(fFile);
   {$I+}
    If IOResult <> 0 Then
    begin
      frmMain.logMain.Add('delerr', 'Could not delete project file: ' + filename);
      MessageDlg('Could not delete file: ' + filename ,mtError,[mbOK],16);
    end;
   prFiles.Free;
end;

procedure tfrmHelper.prjAddFiles(Sender: TObject);
begin
   AddFiles(0); {Add from a folder}
end;

procedure tfrmHelper.AddFiles(CallType : Integer);
var ChangeNode, NewFile : tTreeNode;
    o, SelItem, new, ImgIndex : Integer;
    fileini : tIniFile;
    pt : pChar;
    ts, ci : String;
    dummy : String;
begin
   case CallType of
     0 : Begin
            ChangeNode := CurrentNode;
            ci := currentitem;
         End;
     1 : Begin
            ChangeNode := CurrentNode.Parent;
            ci := currentparent;
         End;
     Else ChangeNode := CurrentNode;
   End;
   If ChangeNode.Parent = nil Then
    Begin
      SelItem := getprojindex(ci);
      Parse(ProjList[SelItem], dummy);
      dlgAddFiles.Filter := frmMain.OpenDialog1.Filter;
      If dlgAddFiles.Execute Then
      Begin
        o := ProjFiles.Count;
        fileini := tIniFile.Create(ProjDir + ProjList[SelItem]);
        If dlgAddFiles.Files.Count > 0 Then
           For new := 0 to dlgAddFiles.Files.Count -1 do
           begin { For new.. }
             ts := ExtractFileName(dlgAddFiles.Files[new]);
// Change back to CurrentNode if this doesn't work :)
             NewFile := projects.items.addchild(ChangeNode, ts);
             ImgIndex := GetIconIndex(ts);
             NewFile.ImageIndex := ImgIndex;
             NewFile.SelectedIndex := ImgIndex;
             GetMem(Pt, 256);
             StrPCopy(Pt, dlgAddFiles.Files[new]);
             NewFile.Data := Pt;
             fileini.WriteString('Files', 'File' + inttostr(o),
                                                        dlgAddFiles.Files[new]);
             o := o + 1;
           end; { For new.. }
        fileini.WriteInteger('Files', 'NumFiles', o);
        fileini.Free;
      End;
    End Else Application.ProcessMessages;
end;

procedure tfrmHelper.prjAddFiles2(Sender : tObject);
begin
   AddFiles(1); {add from an item}
end;

procedure tfrmHelper.prjAddCFile(Style : Integer); {Add Current File}
var ChangeNode,
    NewFile     : tTreeNode;
    SelItem,
    ImgIndex    : Integer;
    fileini     : tIniFile;
    pt          : pChar;
    ts,
    ci          : String;
    fEditor     : tfrmClient;
    dummy       : String;
begin
   Case Style of
     0 : Begin
            ChangeNode := CurrentNode;
            ci := currentitem;
         End;
     1 : Begin
            ChangeNode := CurrentNode.Parent;
            ci := currentparent;
         End;
   Else ChangeNode := CurrentNode;
   End;
   fEditor := frmMain.GetCurrentEditor;
   If not Assigned(fEditor) Then Exit;
   If (pos('>', FEditor.Filename) <> 0) or (fEditor.Filename = '') Then Exit;
   SelItem := getprojindex(ci);
   Parse(ProjList[SelItem], dummy);
   fileini := tIniFile.Create(ProjDir + ProjList[SelItem]);
   ts := ExtractFileName(fEditor.Filename);
   NewFile := projects.items.addchild(ChangeNode, ts);
   ImgIndex := GetIconIndex(ts);
   NewFile.ImageIndex := ImgIndex;
   NewFile.SelectedIndex := ImgIndex;
   GetMem(Pt, 256);
   StrPCopy(Pt, fEditor.Filename);
   NewFile.Data := Pt;
   fileini.WriteString('Files', 'File' + inttostr(ProjFiles.Count),
                                              fEditor.Filename);
   fileini.WriteInteger('Files', 'NumFiles', ProjFiles.Count + 1);
   fileini.Free;
end;

procedure tfrmHelper.prjAddCurrent(Sender : tObject);
begin
   If CurrentNode.Parent = nil Then
      prjAddCFile(0) {add from folder}
   Else
      prjAddCFile(1); {add from item}
end;

procedure tfrmHelper.RenameProject(Sender : tObject);
begin
   CurrentNode.EditText;
end;

procedure tfrmHelper.DeleteProject(Sender : tObject);
var ChangeNode : tTreeNode;
    SelItem, lcount : Integer;
    fileini : File;
    ReturnResult : Word;
    pt : PChar;
begin { DeleteProject }
   ReturnResult := MessageDlg('Are you sure you wish to delete this project?',
                                                 mtConfirmation,[mbYes,mbNo],0);
   case ReturnResult of
        mrYes : {Carry On};
        mrNo : Exit;
  end; { case ReturnResult of }
  ChangeNode := CurrentNode;
  If ChangeNode.Parent = nil Then
  Begin
    if BuildProject = ChangeNode then
      BuildProject := nil;
    Projects.Items.Beginupdate;
    {Here we have to De-Allocate any Pointers made}
    For lcount := 0 to Projects.Items.Count - 1 do
       If not (Projects.Items[lcount].Parent = nil) Then
         If Projects.Items[lcount].Parent = ChangeNode Then
         Begin
           Pt := Projects.Items[lcount].data;
           Projects.Items[lcount].data := nil;
           Freemem(Pt, 256);
         end;

    if (ChangeNode.Data <> nil) Then
    begin
     FreeMem(ChangeNode.Data);
     ChangeNode.Data := nil;
    end;

    Projects.Items.EndUpdate;
    SelItem := getprojindex(currentitem);
    frmMain.logMain.Add('projects', 'Deleting Project: ' + currentitem);
    AssignFile(fileini, ProjDir + ProjList[SelItem]);
    Erase(fileini);
    CurrentNode.DeleteChildren;
    CurrentNode.Delete;
    ProjList.Delete(SelItem);
    ProjNameList.Delete(SelItem);
  End;
end;

procedure TfrmHelper.ProjectsDblClick(Sender: TObject);
var ChangeNode : tTreeNode;
    pTemp : PChar;
begin { ProjDblClick }
   ChangeNode := Projects.Selected;
   pTemp := ChangeNode.Data;
   If ChangeNode.Parent = nil Then Application.ProcessMessages Else
       Begin
         frmMain.OpenFile(pTemp, '');
       End;
end;

procedure TfrmHelper.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Shutdown;
  KillProjects;
  FreeClipList;
  If assigned(ProjFiles) Then ProjFiles.Free;
  If assigned(ProjList) Then ProjList.Free;
  If assigned(ProjNameList) Then ProjNameList.Free;
  If assigned(fList) Then fList.Free;
  If Assigned(cList) Then FreeAndNil(cList);
end;

procedure TfrmHelper.FormCreate(Sender: TObject);
begin
   Projects.Align := alClient;
   panProjects.Align := alClient;
   panTextClips.Align := alClient;
   lstCurrent.Align := alClient;
   panCurrent.Align := alClient;
   cmbPick.Align := alTop;
   lstClips.Align := alClient;
   treBrowse.Align := alClient;
   panBrowser.Align := alClient;
   tbrBrowse.Align := alBottom;
   _NoBrowseRoot := True;
   BuildProject := nil;

   ProjList := tStringList.Create;
   ProjFiles := tStringList.Create;
   ProjNameList := tStringList.Create;
   fList := tStringList.Create;
   cList := TStringList.Create;
   Setup;
end;

function TfrmHelper.InsIndent(S : String; I : Integer) : String;
var n    : integer;
    outs : String;
    skip : Boolean;

function space(num : integer) : String;
var spaces : string;
    ns     : integer;
begin
   For ns := 1 to num do
   begin
      spaces := spaces + ' ';
   end;
   Result := Spaces;
end;

begin
skip := false;
   For n := 0 to length(S) do
   begin
      If not Skip then
      Case s[n] of
         #13 : begin
                 skip := true;
                 outs := outs + #13#10 + space(I);
               end;
         #10..#12, #14..#255 : outs := outs + s[n];
      end else
      Skip := False;
   end;
   Result := outs;
end;

procedure TfrmHelper.lstClipsDblClick(Sender: TObject);
var r,
    s,
    oldsel  : String;
    n, olds : integer;
    addold  : boolean;
    fEditor : tfrmClient;
    cPos    : tPoint;
begin
   addold := false;
   If lstClips.SelCount <> 0 Then
      If frmMain.tabs.TabIndex <> -1 Then
      Begin
         fEditor := tfrmClient(frmMain.ActiveMDIChild);
         If fEditor.synMDI.SelLength > 0 Then
         begin
            oldsel := fEditor.synMDI.SelText;
            addold := true;
         end;
         s := CStringToString(fList[lstClips.ItemIndex]);
         cPos := fEditor.synMDI.CaretPos;
         If cPos.x > 1 Then
            s := InsIndent(s, cPos.x - 1);
         n := pos('|', s);
         r := s;
         olds := fEditor.synMDI.SelStart;
         If n <> 0 Then
            s := copy(r, 1, n-1) + copy(r, n+1, length(s));
         fEditor.synMDI.SelText := s;
         If n <> 0 Then
          fEditor.synMDI.SelStart :=
             olds + n - 1;
          fEditor.synMDI.SelStart := fEditor.synMDI.SelStart - (fEditor.synMDI.CaretPos.y - cPos.y);
         olds := fEditor.synMDI.SelStart;
         If addold Then
         begin
            fEditor.synMDI.SelText := oldsel;
            fEditor.synMDI.SelStart := olds;
            fEditor.synMDI.SelLength := length(oldsel);
         end;
         fEditor.BringToFront;
         fEditor.SetFocus;
         Windows.SetFocus(FEditor.synMDI.Handle)
      end;
end;

function TfrmHelper.GetClipName(fn : string) : string;
var t : TextFile;
    s : string;
begin
  AssignFile(t, fn);
  Try
    Reset(t);
    Repeat
      Readln(t, s);
    Until (s <> '') or Eof(t);
    CloseFile(t);
    If UpperCase(Copy(RemovePrepending(s), 1, 6)) = '!NAME=' then
      s := copy(RemovePrepending(s), 7, length(s)) else
    begin
      s := ExtractFileName(fn);
      s := Copy(s, 1, LEngth(s) - Length(ExtractFileExt(s)));
    end;
    Result := s;
  Except
    Result := ExtractFileName(fn);
  end;
end;

function TfrmHelper.AddClipImage(fn : string) : Integer;
var
  bmp : TBitmap;
  msk : TBitmap;
begin
  bmp := TBitmap.Create;
  msk := TBitmap.Create;
  Try
    bmp.LoadFromFile(fn + '.bmp');
    If FileExists(fn + '.mbmp') then
      msk.LoadFromFile(fn + '.mbmp')
    else
    begin
      msk.Assign(bmp);
      msk.Mask(clWhite);
    end;
    result := ilsClips.Add(bmp, msk);
  Finally
    msk.Free;
    bmp.Free;
  End;
end;

function TfrmHelper.GetClipImage(Dir, fn : string) : Integer;
var t : TextFile;
    s : string;
    q : string;
begin
  If FileExists(fn + '.bmp') then
  begin
    Result := AddClipImage(fn);
  end else
  begin
    AssignFile(t, fn + '.pnclip');
    Try
      Reset(t);
      Repeat
        Readln(t, s);
        q := UpperCase(copy(RemovePrepending(s), 1, 6));
      Until (q = '!TEXT=') or (q = '!IMAGE') or Eof(t);
      CloseFile(t);
    except
      q := '';
    end;
    If q <> '!IMAGE' then
      Result := 0
    else
    begin
      s := copy(s, 8, length(s));
      s := copy(s, 1, length(s) - length(ExtractFileExt(s)));
      Result := AddClipImage(Dir + s);
    end;
  end;
end;

procedure TfrmHelper.FindClipFiles;
var
   SRes : integer;
   SRec : TSearchRec;
   s    : string;
   i    : Integer;

procedure AddClipFile(Dir, FileName : string);
begin
  s := ExtractFileName(FileName);
  s := Copy(s, 1, Length(s) - Length(ExtractFileExt(s)));
  i := GetClipImage(Dir, Dir + s);
  s := GetClipName(Dir + FileName);
  cList.Add(Dir + FileName);
  cmbPick.AddItem(s, nil, i, 0, i, -1);
end;

begin { GetClipFiles }
  cmbPick.Items.Clear;
  cList.Clear;
  SRes := FindFirst(ClipDir + '*.pnclip',(faAnyFile-faDirectory), SRec);
  repeat
    if SRes = 0 then
    begin
      AddClipFile(ClipDir, SRec.Name);
      SRes := FindNext(SRec);
    end;
  until SRes <> 0;
  FindClose(SRec);
  // Now check in the schemes directory...
  SRes := FindFirst(SchemeDir + '*.pnclip',(faAnyFile-faDirectory),SRec);
  repeat
    if SRes = 0 then
    begin
      AddClipFile(SchemeDir, SRec.Name);
      SRes := FindNext(SRec);
    end;
  until SRes <> 0;
  FindClose(SRec);

end;

procedure TfrmHelper.FreeClipList;
begin
  cList.Clear;
  cmbPick.Items.Clear;
end;

procedure TfrmHelper.LoadClips(Index : Integer);
var TempList, Fl : tStringList;
    fFile : TextFile;
    s, r : String;
    n : Integer;
    InSection : Boolean;
    fn  : string;
begin
   lstClips.Items.Clear;
   If not Assigned(fList) Then fList := tStringList.Create;
   fList.Clear;
   lstClips.Items.BeginUpdate;
   fl := tStringList.Create;
   TempList := tStringList.Create;
   InSection := False;
   s := '';
   r := '';
   fn := cList[Index];
    {$I-}
    AssignFile(fFile, fn);
    Reset(fFile);
    Repeat
       Readln(fFile, s);
       fl.Add(s);
    Until EOF(fFile);
    CloseFile(fFile);
    {$I+}
    For n := 0 to fl.Count - 1 do
    Begin
       If not (UpperCase(Copy(RemovePrepending(Fl[n]), 1, 5)) = '!NAME') and
          not (UpperCase(Copy(RemovePrepending(Fl[n]), 1, 6)) = '!IMAGE') then
       begin
         If not InSection Then
         Begin
            TempList.Clear;
            InSection := True;
         End;
         If ucase(copy(fl[n], 1, 5)) = '!TEXT' Then
         Begin
            s := fl[n];
            s := (copy(s, 7, 255));
         End;
         r := fl[n];
         If (copy(r, 1, 1) <> '!') and (uppercase(copy(r, 1, 5)) <> '!TEXT') Then
         TempList.Add(r);
         If (copy(r, 1, 1) = '!') and (uppercase(copy(r, 1, 5)) <> '!TEXT') Then
         Begin
            InSection := False;
            fList.Add(StringToCString(TempList.text));
            lstClips.Items.Add(s);
         End;
       end;
    End;
    TempList.Free;
    fl.Free;
    lstClips.Items.EndUpdate;
end;



procedure TfrmHelper.cmbPickChange(Sender: TObject);
begin
  LoadClips(cmbPick.ItemIndex);
end;

procedure TfrmHelper.popTabsPopup(Sender: TObject);
begin
  itmProjects.Checked := CurrentPage = 0;
  itmTextClips.Checked := CurrentPage = 1;
  itmCurrentFiles.Checked := CurrentPage = 2;
  itmBrowser.Checked := CurrentPage = 3;
  actFullSize.Checked := frmMain.FloatHelper.FullSize;
end;

procedure TfrmHelper.itmProjectsClick(Sender: TObject);
begin
  CurrentPage := TMenuItem(Sender).Tag;
  PageChange(Self);
end;

procedure TfrmHelper.FormShow(Sender: TObject);
begin
   panProjects.Invalidate;
   panTextClips.Invalidate;
end;

procedure TfrmHelper.actAddProjectExecute(Sender: TObject);
begin
   NewProject(Sender);
end;

procedure TfrmHelper.actAddFilesExecute(Sender: TObject);
var ChangeNode : tTreeNode;
begin
  ChangeNode := Projects.Selected;
  if ChangeNode = nil then Exit;
  CurrentItem := ChangeNode.Text;
  If ChangeNode.Parent <> nil Then
    CurrentParent := ChangeNode.Parent.Text;
  CurrentNode := Projects.Selected;
  if (Projects.Selected.Text <> 'Projects') then
  begin
    if (Projects.Selected.Parent = nil) then
      AddFiles(0)
    else
      AddFiles(1); //Add from a file
  end;
end;

procedure TfrmHelper.actExpandExecute(Sender: TObject);
var n : Integer;
begin
   for n := 0 to Projects.Items.Count - 1 do
      Projects.Items[n].Expand(False);
end;

procedure TfrmHelper.actContractExecute(Sender: TObject);
var n : Integer;
begin
   for n := 0 to Projects.Items.Count - 1 do
      Projects.Items[n].Collapse(False);
end;

procedure TfrmHelper.lstCurrentResize(Sender: TObject);
begin
  lstCurrent.Columns[0].Width := lstCurrent.Width - 24;
end;

procedure TfrmHelper.UpdateCurrent;
var n : Integer;
    i : TListItem;
begin
  if frmMain.tabs.Tabs.Count <> lstCurrent.Items.Count then
  begin
    lstCurrent.Items.BeginUpdate;
    lstCurrent.Items.Clear;
    for n := 0 to frmMain.tabs.Tabs.Count - 1 do
    begin
      i := lstCurrent.Items.Add;
      i.Caption := frmMain.tabs.Tabs[n];
      i.SubItems.Add(TfrmClient(frmMain.tabs.Tabs.Objects[n]).FileName);
      i.SubItems.Add(IntToStr(n));
      i.Selected := n = frmMain.tabs.TabIndex;
    end;
    lstCurrent.Items.EndUpdate;
  end;
end;

procedure TfrmHelper.UpdateCurrentSelection(Index : integer);
var i : Integer;
begin
  for i := 0 to lstCurrent.Items.Count - 1 do
  begin
    lstCurrent.Items[i].Selected :=
                (StrToInt(lstCurrent.Items[i].SubItems[1]) = Index)
  end;
end;

procedure TfrmHelper.lstCurrentClick(Sender: TObject);
begin
  if lstCurrent.SelCount > 0 then
  begin
    frmMain.tabs.tabindex := strtoint(lstCurrent.Selected.SubItems[1]);
    frmMain.tabsChange(Self);
  end;
end;

procedure TfrmHelper.treBrowseCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
const
  noroot : string = 'Select a Start Point';
var
  x,y : Integer;
  tw : Integer;
  th : Integer;
begin
  if _NoBrowseRoot then
  begin
    tw := Sender.Canvas.TextWidth(noroot);
    th := Sender.Canvas.TextHeight(noroot);
    x := ARect.Left + ( ((ARect.Right - ARect.Left) div 2) - (tw div 2));
    y := ARect.Top + ( ((ARect.Bottom - ARect.Top) div 2) - (th div 2));
    //Sender.Canvas.FillRect(Sender.Canvas.ClipRect);
    Sender.Canvas.TextOut(x, y, noroot);
    DefaultDraw := False;
  end else
    DefaultDraw := True;
end;

procedure TfrmHelper.actChooseRootExecute(Sender: TObject);
begin
  if dlgBrowse.Execute then
  begin
    sBrowseRoot := dlgBrowse.Selection;
    _NoBrowseRoot := False;
    PropagateBrowser;
    treBrowse.Repaint;
  end;
end;

procedure TfrmHelper.ClearBrowser(tr : TTreeNode);
var
  nexttn : TTreeNode;
  tn : TTreeNode;
  p : PChar;
  i : Integer;
begin
  if tr = nil then
    tn := treBrowse.Items.GetFirstNode
  else
    tn := tr.getFirstChild;

  while not (tn = nil) do
  begin
    if tn.HasChildren and (tn.Count > 0) then
      ClearBrowser(tn);
    if tn.Data <> nil then
    begin
      p := tn.Data;
      i := strlen(p);
      FreeMem(tn.Data, i+1);
    end;
    nexttn := tn.getNextSibling;
    tn.Delete;
    tn := nexttn;
  end;
end;

procedure TfrmHelper.ResetBrowseFolders(tr : TTreeNode);
var tn : TTreeNode;
begin
  if tr = nil then
    tn := treBrowse.Items.GetFirstNode
  else
    tn := tr.getFirstChild;

  while not (tn = nil) do
  begin
    if tn.ImageIndex = 1 then
      tn.HasChildren := True;
    if tn.HasChildren and (tn.Count > 0) then
      ResetBrowseFolders(tn);
    tn := tn.getNextSibling;
  end;
end;

procedure TfrmHelper.LoadDir(sDir : string; tnRoot : TTreeNode);
var
  search : TSearchRec;
  tn     : TTreeNode;
  s      : string;
  p      : PChar;
  ignore : Boolean;
begin
  treBrowse.Cursor := crAppStart;
  try
    tn := nil;
    if (sDir[Length(sDir)] <> '\') and (sDir[Length(sDir)] <> '/') then
      sDir := sDir + '\';
    if not DirExists(sDir) then
    begin
      _NoBrowseRoot := True;
      Exit;
    end;
    if FindFirst(sDir + '*', faAnyFile or faDirectory, search) = 0 then
    begin
      Repeat
        ignore := False;
        if (search.Attr and faDirectory) = faDirectory then
        begin
          // It's a directory...
          if (search.Name <> '.') and (search.Name <> '..') then
          begin
            s := search.Name;
            tn := treBrowse.Items.AddChild(tnRoot, s);
            tn.HasChildren := True;
            tn.ImageIndex := 1;
          end else ignore := True;
        end else
        begin
          s := search.Name;
          tn := treBrowse.Items.AddChild(tnRoot, s);
          tn.ImageIndex := GetIconIndex(search.Name);
        end;
        if not ignore then
        begin
          tn.SelectedIndex := tn.ImageIndex;
          s := sDir + search.Name;
          GetMem(p, Length(s) + 1);
          StrPCopy(p, s);
          tn.Data := p;
        end;
      until FindNext(search) <> 0;
    end;
    FindClose(search);
    if tnRoot <> nil then
      tnRoot.AlphaSort
    else
      treBrowse.AlphaSort;
  Finally
    treBrowse.Cursor := crDefault;
  end;
end;

function LastDir(sDir : string) : string;
var
  i : Integer;
  res : string;
begin
  if (sDir[Length(sDir)] = '\') or (sDir[Length(sDir)] = '/') then
    sDir := Copy(sDir, 1, Length(sDir)-1);
  for i := Length(sDir) downto 1 do
  begin
    if (sDir[i] = '\') or (sDir[i] = '/') then
    begin
      res := Copy(sDir, i+1, Length(sDir));
      Break;
    end;
  end;
  Result := res;
end;

procedure TfrmHelper.PropagateBrowser;
var
  tn : TTreeNode;
  s : string;
begin
  if sBrowseRoot = '' then Exit;
  if not DirExists(sBrowseRoot) then
  begin
    MessageDlg('Directory '+ sBrowseRoot +' does not exist.', mtInformation, [mbOK], 0);
    Exit;
  end;
  treBrowse.Items.BeginUpdate;
  try
    ClearBrowser(nil);
    s := LastDir(sBrowseRoot);
    tn := treBrowse.Items.AddFirst(nil, s);
    tn.ImageIndex := 1;
    tn.SelectedIndex := 1;
    tn.Data := nil;
    LoadDir(sBrowseRoot, tn);
    tn.Expand(False);
  finally
    treBrowse.Items.EndUpdate;
    _DisableClick := False;
  end;
end;

procedure TfrmHelper.treBrowseExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  Node.Selected := True; // Stop us from loading some random file on expand.
  if (Node.ImageIndex = 1) and (Node.Count = 0) then
  begin
    treBrowse.Items.BeginUpdate;
    try
      LoadDir(PChar(Node.Data), Node);
    finally
      treBrowse.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmHelper.treBrowseClick(Sender: TObject);
var
  tn : TTreeNode;
begin
  if not _DisableClick then
  begin
    tn := treBrowse.Selected;
    if tn <> nil then
      if tn.ImageIndex >= 2 then
      begin
        //if (not frmMain.CheckAlreadyOpen(PChar(tn.Data))) then
          frmMain.OpenFile(PChar(tn.Data), '');
      end else if tn.ImageIndex = 1 then
        if not tn.Expanded then tn.Expand(False)
        else tn.Collapse(False);
  end;
  _DisableClick := False;
end;

procedure TfrmHelper.actRefreshBrowserExecute(Sender: TObject);
begin
  PropagateBrowser;
end;

procedure TfrmHelper.treBrowseCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
var s1, s2 : string;
begin
  if (Node1.ImageIndex = 1) and (Node2.ImageIndex > 1) then
  begin
    Compare := -1;
    Exit;
  end else
  if (Node1.ImageIndex > 1) and (Node2.ImageIndex = 1) then
  begin
    Compare := 1;
    Exit;
  end else
  begin
    s1 := LowerCase(PChar(Node1.Data));
    s2 := LowerCase(PChar(Node2.Data));
    if s1 > s2 then Compare := 1
    else if s2 > s1 then Compare := -1
    else Compare := 0;
  end;
end;

procedure TfrmHelper.treBrowseCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  if Node.Level = 0 then
  begin
    AllowCollapse := False;
    Exit;
  end;
end;

procedure TfrmHelper.treBrowseCollapsed(Sender: TObject; Node: TTreeNode);
begin
  _DisableClick := True;
end;

procedure TfrmHelper.treBrowseExpanded(Sender: TObject; Node: TTreeNode);
begin
  _DisableClick := True;
end;

procedure TfrmHelper.popHelperPopup(Sender: TObject);
begin
  actFullSize.Checked := frmMain.FloatHelper.FullSize;
end;

procedure TfrmHelper.ToolButton10Click(Sender: TObject);
begin
  ResetBrowseFolders(nil);
end;

procedure TfrmHelper.actFullSizeExecute(Sender: TObject);
begin
  frmMain.FloatHelper.FullSize := not frmMain.FloatHelper.FullSize;
  treBrowse.Refresh;
end;

procedure TfrmHelper.Help1Click(Sender: TObject);
begin
  sendmessage(frmMain.handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
end;

function TfrmHelper.GetBuildFile : string;
begin
  if (BuildProject <> nil) then
  begin
    Result := PChar(BuildProject.Data);
  end else
    Result := '';
end;

end.

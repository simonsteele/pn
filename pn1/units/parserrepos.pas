{***************************************************************
 *
 * Unit Name: parserrepos
 * Purpose  : Repository for tSyntaxMemo Parsers
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 * History  : 20/10/1999 Created the object (made from a form).
 *                       added an intelligent index management
 *                       system.
 *          : 06/11/1999 Added registry settings updating for all
 *                       parsers and changed the key bindings for
 *                       all current parsers.
 *          : 28/02/2000 Added a parser to look for line numbers in
 *                       results output.
 *          : 02/03/2000 Fixed a bug in aforementioned parser.
 *          : 03/05/2000 Modified the keyboard shortcuts for the
 *                       default scripts (default & results).
 *
 ****************************************************************}


unit parserrepos;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SyntaxEd, SynParse, IniFiles, Registry, SYM_TSMOpt,
  SYMOptPE;

type
  TParsers = class(TForm)
    parPlain: TSyntaxMemoParser;
    parResults: TSyntaxMemoParser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure ClearList;
    procedure SetTags;
    function INICustomParse(Sender: TObject; ParseID: Integer;
      IStream: TEdStream; var kLength, kValue: Integer): Boolean;
    procedure PerformNewScheme(filebase : string; iScheme : integer);
    procedure OnPlainDlgInitialize(Sender: TObject);
  public
    { Public declarations }
    HomeDir  : string;
    list     : tList;
    filelist : tStringList;
    MaxTag   : integer;
    HTML     : integer;
    procedure Refresh;
    function Count : integer;
    function Items(index : integer) : tSyntaxMemoParser;
    function TagItems(index : integer) : tSyntaxMemoParser;
    function TagFiles(index : integer) : string;
    procedure ModifyPlain;
  end;

var
  Parsers: TParsers;

implementation

{$R *.DFM}

uses main, pntypes;

const RootKey    = 'Software\Echo Software\PN';

procedure TParsers.FormCreate(Sender: TObject);
begin
   list := tList.Create;
   filelist := tStringList.Create;
   parPlain.StylesFromRegistry(true, '');
end;

procedure TParsers.Refresh;
var
   search : tSearchRec;
   tp     : tSyntaxMemoParser;
   fs     : tFileStream;
   sdir,
   sname,      
   sext   : string;
begin
  ClearList;
  if FindFirst(HomeDir + '*.sch', faAnyFile, search) = 0 then
  begin
    repeat
      tp := tSyntaxMemoParser.Create(Self);
      sdir := HomeDir;                                  // The directory
      sname := extractFileName(search.name);            // The filename
      sext := extractFileExt(search.name);              // The extension
      sname := copy(sname, 1, length(sname) - length(sext)); // fname w/o ext.
      filelist.Add(sname);
      fs := tFileStream.Create(sdir + sname + '.scp', fmOpenRead or fmShareCompat);
      fs.seek(0,0);
      tp.RegistryKey := 'Software\Echo Software\PN\Highlighting';
      tp.UseRegistry := true;
      tp.SetFromStream(fs);
      If fileexists(sdir + sname + '.sct') then
        tp.UI_Styles.Templates.LoadFromFile(sdir + sname + '.sct');
      If fileexists(sdir + sname + '.sac') then
        tp.UI_Styles.AutoReplace.LoadFromFile(sdir + sname + '.sac');
      If tp.UI_Styles.LangName = 'INI' then tp.OnCustomParse := INICustomParse;
      tp.StylesFromRegistry(True, '');
      fs.free;
      list.Add(tp);
    until FindNext(search) <> 0;
  end;
  FindClose(search);
  parPlain.StylesFromRegistry(True, '');
  If fileexists(sdir + 'plain.sct') then
    parPlain.UI_Styles.Templates.LoadFromFile(sdir + 'plain.sct');
  If fileexists(sdir + 'plain.sac') then
    parPlain.UI_Styles.AutoReplace.LoadFromFile(sdir + 'plain.sac');
  parResults.StylesFromRegistry(True, '');
  SetTags;
end;

procedure TParsers.ClearList;
var n  : integer;
    r  : tRegistry;
    SaveSettings : Boolean;
begin
   r := tRegistry.Create;
   try
     r.OpenKey(rootkey, True);
     try SaveSettings := r.ReadBool('SaveSchemeSettings') except SaveSettings := true; end;
   finally
     r.Free;
   end;
   parPlain.StylesToRegistry;
   try
     If (not parPlain.UI_Styles.Templates.IsEmpty) and SaveSettings then
       parPlain.UI_Styles.Templates.SaveToFile(HomeDir+'plain.sct');
     If (not parPlain.UI_Styles.AutoReplace.IsEmpty) and SaveSettings then
       parPlain.UI_Styles.AutoReplace.SaveToFile(HomeDir+'plain.sac');

     for n := list.count - 1 downto 0 do
     begin
        If (not tSyntaxMemoParser(list.Items[n]).UI_Styles.Templates.IsEmpty) and SaveSettings then
          tSyntaxMemoParser(list.Items[n]).UI_Styles.Templates.SaveToFile(
            HomeDir + filelist[n] + '.sct');
        If (not tSyntaxMemoParser(list.Items[n]).UI_Styles.AutoReplace.IsEmpty) and SaveSettings then
          tSyntaxMemoParser(list.Items[n]).UI_Styles.AutoReplace.SaveToFile(
            HomeDir + filelist[n] + '.sac');
        tSyntaxMemoParser(list.Items[n]).StylesToRegistry;
        tSyntaxMemoParser(list.Items[n]).Free;
     end;
   except
   end;
   filelist.Clear;
   parResults.StylesToRegistry;
end;

function TParsers.Count: integer;
begin
  // Add 1 for the plain text parser...
  Result := list.Count + 1;
end;

procedure TParsers.SetTags;
var Ini : tIniFile;
    s   : string;
    i   : integer;
    n   : integer;
    ix  : integer;
    ixt : integer;
    sl  : tStringList;
begin
  MaxTag := 0;
  HTML := -1;
  Ini := tIniFile.Create(HomeDir + 'schemes.dat');
  try
    For n := 0 to list.Count - 1 do
    begin
       s := tSyntaxMemoParser(list.Items[n]).UI_Styles.LangName;
       i := Ini.ReadInteger('Indexes', s, -1);
       If i <> -1 then
       begin
          tSyntaxMemoParser(list.Items[n]).Tag := i;
          If i > MaxTag then MaxTag := i;
          If uppercase(s) = 'HTML' then HTML := i;
       end else
       begin
          // We're going to use a loop to find the maximum assigned tag value.
          // This value will be stored in I.
          sl := tStringList.Create;
          try
            Ini.ReadSection('Indexes', sl);
            i := 0;
            for ix := 0 to sl.count - 1 do
            begin
               ixt := Ini.ReadInteger('Indexes', sl[ix], 0);
               If ixt > i then i := ixt;
            end;
          finally
            sl.Free;
          end;
          // Increment I to give the new index.
          inc(i);
          PerformNewScheme(filelist[n], i);
          If i > MaxTag then MaxTag := i;
          tSyntaxMemoParser(list.Items[n]).Tag := i;
          If uppercase(s) = 'HTML' then HTML := i;
          Ini.WriteInteger('Indexes', s, i);
       end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TParsers.PerformNewScheme(filebase : string; iScheme : integer);
var
  ini      : tIniFile;
  newass,
  newdesc,
  newtit   : string;
  i        : integer;
  n        : integer;
  newtype  : tNewType;
begin
  // Ask if the user wishes to add new types...
  ini := tIniFile.Create(HomeDir + filebase + '.sch');
  try
    // Read in New Item...
    newdesc := ini.ReadString('Scheme', 'Name', 'New');
    newdesc := ini.ReadString('New', 'Description', newdesc);
    newtit := ini.ReadString('New', 'WindowTitle', 'new ' + newdesc);
    If MessageDlg('Would you like to add:'+#13+#10+''+#13+#10+newdesc+#13+#10+''+#13+#10+'to the New menu?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      frmMain.Add_NewItem(newdesc, newtit, iScheme);
    // Read in File Types...
    n := ini.ReadInteger('Files', 'Types', 0);
    For i := 1 to n do
    begin
      newtype.TypeDesc := ini.ReadString('Files', 'Type' + inttostr(i) + 'Desc', 'Text Files (*.txt)');
      newtype.TypeFilter := ini.ReadString('Files', 'Type' + inttostr(i) + 'Filter', '*.txt');
      newtype.TypeHex := ini.ReadBool('Files', 'Type' + inttostr(i) + 'Hex', False);
      newtype.TypeUnix := ini.ReadBool('Files', 'Type' + inttostr(i) + 'Unix', False);
      If MessageDlg('Would you like to add the following type:'+#13+#10+''+#13+#10+newtype.TypeDesc, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        frmMain.Add_NewType(newtype, iScheme);
    end;
    n := ini.ReadInteger('Files', 'Assoc', 0);
    For i := 1 to n do
    begin
      newass := ini.ReadString('Files', 'Assoc' + inttostr(i), '.txt');
      frmMain.Add_NewAss(newass);
    end;
    frmMain.UpdateDialogs;
  finally
    ini.free;
  end;
end;

function TParsers.Items(index : integer): tSyntaxMemoParser;
begin
   If index <> 0 then
   begin
      Result := tSyntaxMemoParser(list.Items[index - 1]);
   end else
      Result := parPlain;
end;

function TParsers.TagItems(index : integer) : tSyntaxMemoParser;
var found : boolean;
    n     : integer;
begin
  Result := parPlain;
  found := false;
  If index <> 0 then
  begin
    for n := 0 to list.count - 1 do
    begin
      If tSyntaxMemoParser(list.Items[n]).Tag = index then
      begin
         found := true;
         Result := tSyntaxMemoParser(list.Items[n]);
      end;
    end;
    If not found then Result := parPlain;
  end else
    Result := parPlain;
end;

function TParsers.INICustomParse(Sender: TObject; ParseID: Integer;
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

procedure TParsers.FormDestroy(Sender: TObject);
begin
   ClearList;
   list.free;
   filelist.free;
end;

function TParsers.TagFiles(index: integer): string;
var found : boolean;
    n     : integer;
begin
  Result := '';
  found := false;
  If index <> 0 then
  begin
    for n := 0 to list.count - 1 do
    begin
      If tSyntaxMemoParser(list.Items[n]).Tag = index then
      begin
         found := true;
         Result := filelist[n];
      end;
    end;
    If not found then Result := '';
  end else
    Result := '';
end;

procedure TParsers.OnPlainDlgInitialize(Sender : TObject);
var n : Integer;
begin
  with Sender as TSYMOptionsDlgWrapper do
  begin
    with Settings do
    begin
      for n := 0 to list.Count - 1 do
        AddAvailableParser(TSyntaxMemoParser(list[n]));
    end;
  end;
end;

procedure TParsers.ModifyPlain;
var tsm : TSyntaxMemo;
begin
  tsm := TSyntaxMemo.Create(Self);
  tsm.Parser1 := parPlain;
  tsm.OnDlgInitialize := OnPlainDlgInitialize;
  tsm.ModifyPropertiesEx;
  FreeAndNil(tsm);
end;

end.

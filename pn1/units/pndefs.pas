{***************************************************************
 *
 * Unit Name: pndefs
 * Purpose  : Various Definitions and functions...
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}
unit pndefs;

interface

uses SysUtils;

function CreatePNFile(filename : string; Text : pChar) : Boolean;
function StripNewLines(aString: string): string;
procedure ConvertTypes(filename : string);

const strFileTypes : PChar = ('.txt');
   strOpenTypes : PChar = ('%2|Text files (*.txt)|*.txt|0|0|0|LOG files (*.log)|*.log|0|0|0|Executable Files (*.exe, *.com, *.dll)|*.exe;*.com;*.dll|0|0|0');
   sepChar = '|';
   verChar = '%';
   CurrFileVer = '2';

implementation

function CreatePNFile(filename : string; Text : pChar) : Boolean;
var F : TextFile;
begin
   {$I-}
   AssignFile(F, filename);
   Rewrite(F);
   Write(F, Text);
   CloseFile(F);
   If IOResult <> 0 Then Result := False
                    Else Result := True;
   {$I+}
end;

function StripNewLines(aString: string): string;
var i : longint;
begin
   result := '';
   i      := 1;
   while i <= length(aString) do
   begin
      if aString[i] = #13 then result := result + ' ' else
      if aString[i] <> #10 then result := result + aString[i];
      inc(i);
   end;
end;

procedure ConvertTypes(filename : string);
var t        : TextFile;
    s        : string;
    ps       : string; {part of string}
    Part     : integer;
    ipos     : integer;
    OutStr   : string;
const Desc   = 1;
      Files  = 2;
      Parser = 3;
      Unix   = 4;
begin
   // This assumes that it is being passed one of the old style type definition
   // files. We'll set the status on the main form to indicate this as well...
   OutStr := VerChar + CurrFileVer;
   if not fileexists(filename) then
   begin
      CreatePNFile(filename, strOpenTypes);
      exit;
   end;
   Assignfile(t, FileName);
   Reset(t);
   repeat
      Readln(t, s)
   until (Length(s) > 0) or EOF(t);
   CloseFile(t);
   if s = '' then Exit;
   part := Desc;
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
                  OutStr := OutStr + SepChar + ps;
                  part := Files;
                end;
         Files : begin
                   OutStr := OutStr + SepChar + ps;
                   part := Parser;
                 end;
         Parser : begin
                    OutStr := OutStr + SepChar + ps + SepChar + '0' + SepChar + '0';
                    part := Desc;
                  end;
      end;
   until Length(s) < 1;
   Assignfile(t, filename);
   Rewrite(t);
   Write(t, OutStr);
   CloseFile(t);
end;

end.

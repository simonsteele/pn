{$D-}
{******************************************************************************}
{                                useful.pas                                    }
{******************************************************************************}
{ the Useful unit by Echo Software                                             }
{ Version 1.1.8.4   (9/03/00)                                                  }
{ Copyright: This Source Code is Copyright © 1998-2000 Echo					           }
{            Software and Simon Steele.	   	 		   						                 }
{                                                                              }
{ Changes:                                                                     }
{ ========                                                                     }
{    1.1.8.4: Added GetNumbers function.                                       }
{    1.1.8.3: Added the ParseIn function.                                      }
{    1.1.8.2: Removed outdated get functions.                                  }
{    1.1.8.1: Added the ReverseCase Function                                   }
{    1.1.8.0: Added the GetTempFile Function                                   }
{    1.1.7.9: Added the RemovePrepending Function                              }
{    1.1.7.8: Added the DirExists Function                                     }
{    1.1.7.7: Fixed the C-Style Functions to included excluded characters      }
{    1.1.7.6: Added the first C-Style String Functions                         }
{    1.1.7.5: Added RemoveTrailing. This removes trailing space characters     }
{             from a string.                                                   }
{                                                                              }
{    1.1.7.4: Added BoolToStr and StrToBool. This uses '1' or '0' to store     }
{             boolean values. This is useful for Stringlists and tStrings.     }
{                                                                              }
{ Credits:                                                                     }
{ ========                                                                     }
{   GetFileInformation : David Symons (ds@pnotepad.org)                        }
{   All Others         : Marc Griffin and Simon Steele, Echo Software.         }
{                                                                              }
{ Contact:                                                                     }
{ ========                                                                     }
{    Echo Software : info@pnotepad.org                                         }
{    Simon Steele  : ss@pnotepad.org                                           }
{    Marc Griffin  : mg@pnotepad.org                                           }
{                                                                              }
{******************************************************************************}
{                          Funtions and Procedures                             }
{******************************************************************************}
{                                                                              }
{  1: GetFileInformation = Used for About Box Style Stuff, Interface with      }
{                          VersionInfo.                                        }
{  5: GetPosition = Used for getting the position for the main form of your    }
{                   application.                                               }
{  6: SetPosition = Used for setting the above.                                }
{  7: SetSize = Used for setting the size of the main form of your application.}
{  8: SetStat = Used for setting the WindowState of your application in the    }
{               registry.                                                      }
{  9: GetSize = Used for getting the size of the main form of your application }
{               as stored.                                                     }
{  10: GetStat = Used for getting the WindowState of the application as stored }
{                in the registry.                                              }
{  11: UCase = Returns the UpperCase version of any string passed.             }
{  12: StrEllipse = Returns a filename concatenated with a '...'.              }
{  13: BoolToStr = Returns a String from a Boolean.                            }
{  14: StrToBool = Returns a Boolean from a String.                            }
{  15: RemoveTrailing = Removes trailing space characters from a string.       }
{  16: RemovePrepending = Removes prepending space from a string.              }
{  17: RemoveSpace = Removes spaces and tabs from a string                     }
{  18: StringToCString = Converts a Standard string into a C-Style one.        }
{  19: CStringToString = Vice Versa Above...                                   }
{  20: DirExists = Returns True if a Directory Exists. Saves using FileCtrls.  }
{  21: GenTempFile = Returns a filename for a temporary file.                  }
{  22: ReverseCase = Reverse the case of the input string character by         }
{      character.                                                              }
{  23: ParseIn = Parse in a piece of data, in place of a token.                }
{******************************************************************************}

unit useful;

interface

uses Windows, Dialogs, Controls, Registry, Forms, Printers, SysUtils;

function GetFileInformation( const FileName, Value : String ): String;
         {Usage:
          productname.caption:=GetFileInformation(application.ExeName,
                                                    'ProductName');
          version.caption:=GetFileInformation(application.ExeName,
                                                    'FileVersion');
          copyright.caption:=GetFileInformation(application.ExeName,
                                                    'LegalCopyright');
          comments.caption:=  GetFileInformation(application.exename,
                                                    'Comments');
          description.Caption := GetfileInformation(application.exename,
                                                    'FileDescription');}
procedure GetPosition(Company, AppName : String; Height, Width : integer;
                      var Top, Left : integer);
          {Usage:
           GetPosition(frmMain.Height, frmMain.Width, 'MindWeb', 'Fast-HTML',
           Top, Left); Sets the Top and Left from Stored Settings. Needs to be
           performed in FormCreate, and will not work in FormShow. Application
           must not be Position poScreenCentered.}
procedure SetPosition(Company, AppName : String; Top, Left : Integer);
          {Usage:
           SetPosition('MindWeb', 'Fast-HTML', Top, Left); Stores Form
           Position}
procedure SetSize(Company, AppName : String; Height, Width : Integer);
          {Usage:
           SetSize('MindWeb', 'Fast-HTML', frmMain.Height, frmMain.Width);
           Stores the Width and Height of frmMain.}
procedure SetStat(Company, AppName : String; State : tWindowState);
          {Usage:
           SetStat('MindWeb', 'Fast-HTML', frmMain.WindowState);
           Stores whether the form is maximised or not, minimised is
           taken as normal.}
function GetSize(Company, AppName : String;
                  var Height, Width : Integer) : Boolean;
         {Usage:
          If GetSize('MindWeb', 'Fast-HTML', heightvar, widthvar) Then
              Begin
                frmMain.Height := heightvar;
                frmMain.Width := widthvar;
              end;}
function GetStat(Company, AppName : String) : tWindowState;
         {Usage:
            frmMain.WindowState := GetStat('MindWeb', 'Fast-HTML');
            If frmMain.WindowState = wsMaximized Then frmMain.Maximise}
procedure InitializePrinter(Printer : tPrinter);
          {Usage:
           InitializePrinter(Printer);}
procedure GetPrinterResolution(Printer : tPrinter; var X_Dpi, Y_Dpi : Integer);
          {Usage:
           GetPrinterResolution(Printer, X, Y);}
procedure AbortPrintDoc(Printer : TPrinter);
          {Usage:
           AbortPrintDoc(Printer);}

function UCase(Input : STring) : String;
         {Usage:
          ShowMessage(UCase(String));
          Used to Uppercase all characters in a string.}

Function StrEllipse(FileName : String; Limit : Integer) : String;
         {Usage:
          ShorterString := StrEllipse(FileName, Length);}

Function BoolToStr(Value : Boolean) : String;
         {Usage String := BoolToStr(Boolean);}

Function StrToBool(Value : String) : Boolean;
         {Usage Boolean := StrToBool(String);
          Only the first character is examined for a '1' or '0'.}

Function RemoveTrailing(source : string) : string;
         {Usage String := RemoveTrailing(String_with_trailing_spaces);}

function RemovePrepending(source : string) : string;
         {Usage String := RemovePrepending(OldString);}

Function RemoveSpaces(source : string) : string;
         {Usage String := RemoveSpaces(OldString);}

Function StringToCString(source : string) : string;
         {Usage c-style-string := StringToCString(Normal-Style-String);}

Function CStringToString(source : string) : string;
         {Usage Normal-Style-String := CStringToString(C-Style-String);}

function DirExists(const Name: string): Boolean;
         {Usage If DirExists(Directory) Then DoThis;}

function GenTempFile(id, dir : string; idn : integer) : String;
         {Returns a Windows Generated Temporary Filename}

Function UsefulVersion : String;
         {Returns a string containing the useful library version}

function ReverseCase(input : string) : string;
         {Returns a string with the case reversed}

function ParseIn(data, instr, token : string) : string;
         {Parses data into instr where token is found}

function GetNumbers(input : string) : integer;
         {Returns a number from a string, extracting all alpha chars}

const usefulver = '1.1.8.4';

implementation

function UsefulVersion : String;
begin
   Result := usefulver;
end;

{$WARNINGS OFF}
function GetFileInformation( const FileName, Value : String ): String;
var
  dwHandle, dwVersionSize   : DWORD;
  strLangCharSetInfoString  : String;
  pcBuffer                  : PChar;
  pTemp                     : Pointer;
begin
   // US: strLangCharSetInfoString := '\StringFileInfo\040904E4\' + Value;
   // UK:
   strLangCharSetInfoString := '\StringFileInfo\080904E4\' + Value;
   // get version information values
   dwVersionSize := GetFileVersionInfoSize( PChar( FileName ),
                                            dwHandle );
   // PChar( FileName ) = pointer to filename string
   // dwHandle = pointer to variable to receive zero
   // if GetFileVersionInfoSize is successful
   if dwVersionSize <> 0 then
   begin
      try
         GetMem( pcBuffer, dwVersionSize );
         if GetFileVersionInfo( PChar( FileName ),
                                dwHandle,
                                dwVersionSize,      // Buffer Size
                                pcBuffer ) then     // buffer for info.
   // pcBuffer =    pBlock     - address of buffer for version resource
   // pChar( strLangCharSetInfoString ) =
   //               lpSubBlock - address of value to retrieve
   // pTemp =       lplpBuffer - address of buffer for version pointer
   // dwVersionSize = puLen    - address of version-value length buffer

            if VerQueryValue( pcBuffer,
                              PChar( strLangCharSetInfoString ),
                              pTemp,
                              dwVersionSize ) then

               Result := PChar( pTemp );
      finally
         FreeMem( pcBuffer );
      end; // try
   end;// if dwVersionSize
end; // GetFileInformation
{$WARNINGS ON}

{ The form position must be set to poDesigned for this to work, and it cannot  }
{ be used from inside FormShow, or FormCreate. Don't know why...               }
procedure GetPosition(Company, AppName : String; Height, Width : integer;
                                                       var Top, Left : integer);
var Settings : tRegistry;
    RootKey : String;
begin {GetPosition}
{Initialize Variables}
Top := -1;
Left := -1;
   RootKey := 'Software\' + Company + '\' + AppName;      {Setup Registry Path }
   Settings := tRegistry.Create;                          {Open the Registry   }
   Settings.OpenKey(RootKey, True);                       {Open the Key        }
     Try Top := Settings.ReadInteger('Top') Except End;   {Trap Exceptions     }
     Try Left := Settings.ReadInteger('Left') Except End; {while reading values}
   Settings.Free;                                         {Close the Registry  }
{ Now check to see if values were found. If they weren't, then generate some   }
{ to center the window on the screen.                                          }
   If Top = -1 Then
        Top := Screen.Height div 2 - (Height div 2);
   If Left = -1 Then
        Left := Screen.Width div 2 - (Width div 2);
   If Top > Screen.Height then
     Top := 0;
   //If Top + Height > Screen.Height - 25 then
     //Repeat Top := Top - 100 until (Top + Height) < (Screen.Height - 25);
   If Left > Screen.Width then
     Left := 0;
   //If Left + Width > Screen.Width - 25 then
     //Repeat Left := Left - 25 until Left < (Screen.Width - 25);
end; {GetPosition}

{ The form position must be set to poDesigned for this to work, and it cannot  }
{ be used from inside FormShow, or FormCreate. Don't know why...               }
function GetSize(Company, AppName : String;
                                         var Height, Width : integer) : Boolean;
var Settings : tRegistry;
    Rootkey : String;
begin {GetSize}
{Initialize Variables}
Height := -1;
Width := -1;
   RootKey := 'Software\' + Company + '\' + AppName;
   Settings := tRegistry.Create;                          {Open the Registry   }
   Settings.OpenKey(RootKey, True);                       {Open the Key        }
     Try Height := Settings.ReadInteger('Height') Except End;
     Try Width := Settings.ReadInteger('Width') Except End;
   Settings.Free;                                         {Close the Registry  }
{Check to see if values were retrieved. Else use developed position...}
   If (Height = -1) or (Width = -1) Then
        GetSize := False Else
        GetSize := True;
End;

function GetStat(Company, AppName : String) : tWindowState;
var Settings : tRegistry;
    Rootkey : String;
    WinState : Integer;
begin {GetStat}
{Initialize Variables}
WinState := -1;
  RootKey := 'Software\' + Company + '\' + AppName;
   Settings := tRegistry.Create;                          {Open the Registry   }
   Settings.OpenKey(RootKey, True);                       {Open the Key        }
    Try WinState := Settings.ReadInteger('WinState') Except End;
   Settings.Free;                                         {Close the Registry  }
  Case WinState of
     -1 : GetStat := wsNormal;
      1 : GetStat := wsNormal;
      2 : GetStat := wsMaximized;
  Else
      GetStat := wsNormal;
  End;
End; {GetStat}

procedure SetPosition(Company, AppName : String; Top, Left : Integer);
var Settings : tRegistry;
    Rootkey : String;
begin {SetPosition}
   RootKey := 'Software\' + Company + '\' + AppName;
   Settings := tRegistry.Create;
   Settings.OpenKey(RootKey, True);
   Settings.WriteInteger('Top', Top);
   Settings.WriteInteger('Left', Left);
   Settings.Free;
end; {SetPosition}

procedure SetSize(Company, AppName : String; Height, Width : Integer);
var Settings : tRegistry;
    Rootkey : String;
begin {SetSize}
   Rootkey := 'Software\' + Company + '\' + AppName;
   Settings := tRegistry.Create;
   Settings.OpenKey(RootKey, True);
   Settings.WriteInteger('Width', Width);
   Settings.WriteInteger('Height', Height);
   Settings.Free;
end; {SetSize}

procedure SetStat(Company, AppName : String; State : tWindowState);
var Settings : tRegistry;
    Rootkey : String;
    winState : integer;
begin {SetStat}
   Rootkey := 'Software\' + Company + '\' + AppName;
   Settings := tRegistry.Create;
   Settings.OpenKey(RootKey, True);
   winState := 1;
   Case State of
        wsNormal : winState := 1;
        wsMaximized : winState := 2;
        wsMinimized : winState := 1;
   End;
   Settings.WriteInteger('WinState', winState);
   Settings.Free;
end; {SetStat}

{ call InitializePrinter before setting a printer font, to ensure
  that Delphi knows the correct printer resolution.}
{$HINTS OFF}
procedure InitializePrinter(Printer : tPrinter);
var
  Junk : THandle;
begin {InitializePrinter}
  Junk := Printer.Canvas.Handle;
end; {InitializePrinter}
{$HINTS ON}

{Get the printer resolution in pixels per inch, which might be different in
 the X and Y directions. Note that LogPixelsX and LogPixelsY are not necessarily
 accurate. A "logical" inch is not always a physical inch. Thus, HorzRes,
 HorzSize, VertRes, and VertSize are used.}
procedure GetPrinterResolution(Printer : tPrinter; var X_Dpi, Y_Dpi : Integer);
begin {GetPrinterResolution}
  {HorzRes / HorzSize gives pixels per millimeter.
           Convert to Pixels per inch.}
  X_Dpi := MulDiv(GetDeviceCaps(Printer.Handle, HorzRes), 254,
                  GetDeviceCaps(Printer.Handle, HorzSize) * 10);
  Y_Dpi := MulDiv(GetDeviceCaps(Printer.Handle, VertRes), 254,
                  GetDeviceCaps(Printer.Handle, VertSize) * 10);
end; {GetPrinterResolution}

{ Abort a print job. A print process dialog box can call this procedure if
 the user clicks a cancel button.}
procedure AbortPrintDoc(Printer : TPrinter);
begin {AbortPrintDoc}
  Printer.Abort;
{$ifndef WIN32}
  AbortDoc(Printer.Handle);
  Printer.EndDoc;
{$endif}
end; {AbortPrintDoc}

function UCase(Input : String) : String;
var retstr : String;
    count : byte;
begin {UCase}
{Initialize Variables}
   retstr := '';
   for count := 1 to Length(Input) do
      retstr := retstr + UpCase(Input[count]);
   UCase := retstr;
end; {UCase}

{ This one isn't perfect, as it is based on a character limit rather than a    }
{ canvas length. Because fonts are generally not fixed-width, allow some       }
{ lee-way when deciding on a Limit.                                            }
{ Also try changing the line:                                                  }
{     If Not (TempInt > Limit - 15)                                            }
{ to a value other than 15 for a different effect.                             }
Function StrEllipse(FileName : String; Limit : Integer) : String;
var TempInt : Integer;
    TempStr : String;
Begin {StrEllipse}
   TempInt := Length(ExtractFileName(FileName));
   If Not (TempInt > Limit - 15) Then
   TempStr := Copy(FileName, 1, Limit - TempInt - 4);
   TempStr := TempStr + '...\' + ExtractFileName(FileName);
   StrEllipse := TempStr;
End; {StrEllipse}

Function BoolToStr(Value : Boolean) : String;
begin {BoolToStr}
{1 := True, 0 := False}
   case Value of
      True : Result := '1';
      False : Result := '0';
   end;
end; {BoolToStr}

Function StrToBool(Value : String) : Boolean;
begin {StrToBool}
{1 := True, 0 := False}
   If copy(value, 1, 1) = '1' Then Result := True Else Result := False;
end; {StrToBool}

{Remove Trailing Spaces from a String}
function RemoveTrailing(source : string) : string;
var work : string;
begin
If length(source) = 0 Then
begin
   removetrailing := '';
   exit;
end;
work := '';
work := source;
   repeat
      If work[length(work)] = ' ' Then
         work := copy(work, 1, length(work) - 1);
   until work[length(work)] <> ' ';
   result := work;
end;

{Remove prepending spaces from a string}
function RemovePrepending(source : string) : string;
var work      : string;
    StartFlag : Boolean;
    n         : Integer;
begin
StartFlag := False;
   If length(source) = 0 Then
   begin
      RemovePrepending := '';
      exit;
   end;
   work := '';
   For n := 1 to length(source) do
   Begin
      If not StartFlag Then
      Begin
         If (source[n] <> #32) and (source[n] <> #9) Then
         Begin
            work := work + source[n];
            Startflag := true;
         end;
      End else work := work + source[n];
   End;
   Result := work;
end;

{Remove all spaces and tabs from a string}
Function RemoveSpaces(source : string) : string;
var n    : integer;
    work : string;
begin
   work := '';
   For n := 1 to Length(source) do
      Case source[n] of
         #32, #9   : ;
         #0..#8,
         #10..#31,
         #33..#255 : work := work + source[n];
      end;
   Result := work;
end;

{Convert a normal multi-line string, to a C-Style one with \n etc...}
Function StringToCString(source : string) : string;
var s : string;
    c : string;
    n : integer;
begin
   s := source;
   For n := 1 to Length(s) do
   begin
      Case s[n] of
         #13 : c := c + '\n';
         #92 : c := c + '\\';
         #34 : c := c + '\q';
         #32..#33, #35..#91, #93..#255 : c := c + s[n];
      end;
   end;
   result := c;
end; {StringToCString}

{Convert a C-Style string to a normal multiline string}
{Work in progress, currently only parses '\\' and '\n'}
Function CStringToString(source : string) : string;
var c : string; {C String}
    s : string; {Normal String}
    n : integer;
    skip : boolean;
begin
skip := false;
c := source;
   For n := 1 to Length(c) do
   begin
      If not skip then
         Case c[n] of
            #92 : Case c[n+1] of
                     #92 : begin
                               s := s + '\';
                               skip := True;
                           end;
                     #113 : begin
                              s := s + '"';
                              skip := True;
                            end;
                     #110 : begin
                               s := s + #13 + #10;
                               skip := true;
                            end;
                     #10..#91, #93..#109, #111..#112, #114..#255 : ;
                  end;
            #10..#91, #93..#255 : s := s + c[n];
         end
      else skip := false;
   end;
   result := s;
end; {CStringToString}

function DirExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function GenTempFile(id, dir : string; idn : integer) : String;
var path,
    ret,
    ident : pChar;
    s     : String;
begin
   s := ExtractFilePath(ParamStr(0));
   GetMem(path, MAX_PATH);
   GetMem(ident, 5);
   GetMem(ret, MAX_PATH);
   StrPCopy(path, dir);
   StrPCopy(ident, copy(id, 1, 3));
   GetTempFileName(path,
                   ident,
                   idn,
                   ret);
   S := strPas(ret);
   Result := S;
   FreeMem(path, MAX_PATH);
   FreeMem(ident, 5);
   FreeMem(ret, MAX_PATH);
end;

function ReverseCase(input : string) : string;
var
   Pos : Integer;
   output : string;
begin
   for Pos := 1 to Length(input) do
   begin
      if (Ord(input[Pos]) in [65..90]) or (Ord(input[Pos]) in [97..122]) then
      begin
         if (Ord(input[Pos]) in [65..90]) then
            output := output + chr(Ord(input[Pos]) + 32)
         else
            output := output + chr(Ord(input[Pos]) - 32);
      end else
         output := output + input[Pos];
   end;
   Result := output;
end;

function ParseIn(data, instr, token : string) : string;
var s1, s2, ts : string;
begin
  If pos(token, instr) = 0 then exit;
  ts := instr;
  Repeat
    s1 := copy(ts, 1, pos(token, ts) - 1);
    s2 := copy(ts, pos(token, ts) + 2, length(ts));
    ts := s1 + data + s2;
  Until Pos(token, ts) = 0;
  result := ts;
end;

function GetNumbers(input : string) : integer;
var
  r : string;
  n : integer;
begin
  r := '';
  for n := 1 to Length(input) do
    if input[n] in ['0'..'9'] then r := r + input[n];
  try
    Result := strtoint(r);
  except
    Result := 0;
  end;
end;

end.

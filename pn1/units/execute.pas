{***************************************************************
 *
 * Unit Name: execute
 * Purpose  : Code to execute an external program, and capture
 * 			  the output. Does not work on 16-bit programs at
 *			  the moment. Not sure why.
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}

unit execute;

interface

uses Forms, SysUtils, Windows, Classes, Useful;

{.$DEFINE OLD}

procedure RunProgram( Parser, dirpath : string; msg : tstrings );

implementation

{$IFDEF OLD}
procedure RunProgram( Parser, dirpath : string; msg : tstrings );
const
     LENBUFF = 255;
var
   tmp: string;
   hReadPipe, hWritePipe: THandle;
   sa : TSecurityAttributes;
   si : TStartupInfo;
   pi : TProcessInformation;
   lpBuffer    : array[0..LENBUFF] of char;
   //BytesRead   : integer;
   //BytesToRead : integer;
   BytesRead2  : cardinal;
   BytesToRead2 : cardinal;
   rb          : boolean;
   Buffer      : string;
   BufPos      : integer;
   FoundNewLine: boolean;
   output_line : string;

begin
     If not assigned(msg) then exit;
     If length(dirpath) > 0 then
       If dirExists(dirpath) then
         Try ChDir(dirpath); Except End;
     sa.nLength              := sizeof( sa );
     sa.lpSecurityDescriptor := nil;
     sa.bInheritHandle       := True;

     if not CreatePipe( hReadPipe, hWritePipe, @sa, 0 ) then
     begin
        tmp := IntToStr(GetLastError());
        msg.add('Error creating pipe: ' + tmp);
        exit;
     end;

     FillChar( si, sizeof(si), 0 );
     si.cb          := sizeof( si );
     si.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
     si.wShowWindow := SW_HIDE;
     si.hStdInput   := 0;
     si.hStdOutput  := hWritePipe;
     si.hStdError   := hWritePipe;

     if not CreateProcess( nil, pChar( parser ), nil, nil, true, 0, nil, nil, si, pi ) then
     begin
        tmp := IntToStr( GetLastError() );
        msg.add('Error executing sub-program: ' + tmp);
        CloseHandle( hReadPipe );
        CloseHandle( hWritePipe );
        exit;
     end;
     CloseHandle( hWritePipe );

     BytesToRead2 := LENBUFF;
     BytesRead2 := 0;
     Buffer := '';
     While (True) do begin

           lpBuffer := '';
           rb := ReadFile( hReadPipe, lpBuffer, BytesToRead2, BytesRead2, nil );
           if ( not rb ) then
              if ( length( Buffer ) = 0 ) then break;

           Buffer := Buffer + lpBuffer;

           foundNewLine := False;
           BufPos := Pos( #13, Buffer);

           If ( BufPos > 0 ) then begin
              foundNewLine := True;
              output_line := Copy( Buffer, 1, BufPos-1 );

              // Here you have the Line by Line of the output
              Msg.Add( output_line );

              // shift remainder of buffer down
              Buffer := Copy( Buffer, BufPos+2, LENBUFF );
              BytesToRead2   := LENBUFF - Length( Buffer );

           end else begin
              BytesToRead2   := LENBUFF;
              Msg.Add( Buffer );
              Buffer := '';
           end;

           if not foundNewLine then begin
              if ( BytesRead2 = LENBUFF ) then
                 {Application.MessageBox( pChar('Line to Long: inc buffer size'), 'Error', IDOK )}
              else
                 BytesToRead2   := LENBUFF - Length( Buffer );
           end;

     end;
     WaitForSingleObject( pi.hProcess, 10000 );
     CloseHandle( pi.hProcess );
     CloseHandle( hReadPipe );
end;
{$ELSE}

procedure RunProgram(Parser, dirpath : string; msg : tstrings);
var
  tsi: TStartupInfo;
  tpi: TProcessInformation;
  nRead: DWORD;
  aBuf: Array[0..101] of char;
  sa: TSecurityAttributes;
  hOutputReadTmp, hOutputRead, hOutputWrite, hInputWriteTmp, hInputRead,
  hInputWrite, hErrorWrite: THandle;
  FOutput: String;
  Dir : PChar;
begin
  FOutput := '';

  sa.nLength              := SizeOf(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle       := True;

  CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0);
  DuplicateHandle(GetCurrentProcess(), hOutputWrite, GetCurrentProcess(),
    @hErrorWrite, 0, true, DUPLICATE_SAME_ACCESS);
  CreatePipe(hInputRead, hInputWriteTmp, @sa, 0);

  // Create new output read handle and the input write handle. Set
  // the inheritance properties to FALSE. Otherwise, the child inherits
  // the these handles; resulting in non-closeable handles to the pipes
  // being created.
  DuplicateHandle(GetCurrentProcess(), hOutputReadTmp,  GetCurrentProcess(),
    @hOutputRead,  0, false, DUPLICATE_SAME_ACCESS);
  DuplicateHandle(GetCurrentProcess(), hInputWriteTmp, GetCurrentProcess(),
    @hInputWrite, 0, false, DUPLICATE_SAME_ACCESS);
  CloseHandle(hOutputReadTmp);
  CloseHandle(hInputWriteTmp);

  FillChar(tsi, SizeOf(TStartupInfo), 0);
  tsi.cb         := SizeOf(TStartupInfo);
  tsi.dwFlags    := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  tsi.hStdInput  := hInputRead;
  tsi.hStdOutput := hOutputWrite;
  tsi.hStdError  := hErrorWrite;

  if (dirpath <> '') then
    Dir := PChar(dirpath)
  else
    Dir := nil;

  if not CreateProcess(nil, PChar(Parser), @sa, @sa, true, 0, nil, Dir,
    tsi, tpi) then
  begin
    FOutput := IntToStr( GetLastError() );
    msg.add('Error executing sub-program: ' + FOutput);
    CloseHandle( hOutputWrite );
    CloseHandle( hInputRead );
    CloseHandle( hErrorWrite );
    exit;
  end;

  CloseHandle(hOutputWrite);
  CloseHandle(hInputRead );
  CloseHandle(hErrorWrite);
  Application.ProcessMessages;

  repeat
     if (not ReadFile(hOutputRead, aBuf, 16, nRead, nil)) or (nRead = 0) then
     begin
        if GetLastError = ERROR_BROKEN_PIPE then
          Break
        else begin
          msg.Add('Pipe read error, could not execute file');
          break;
        end;
     end;
     aBuf[nRead] := #0;
     FOutput := FOutput + PChar(@aBuf[0]);
     Application.ProcessMessages;
  until False;

  msg.Text := FOutput;
  //GetExitCodeProcess(tpi.hProcess, nRead) = True;
end;

{$ENDIF}

end.

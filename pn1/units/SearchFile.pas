{***************************************************************
 *
 * Unit Name   : GrepResultsDlg
 * Purpose     : Grep Search Result Dialog
 * Copyright   : This Source Code is taken from GExperts, the excellent
 * 			     Delphi/C++Builder add-on available from GExperts.org.
 *				 Please see the file gexpertslicense.html for the license.
 *				 Any modifications from the original are copyright Echo
 *				 Software.
 ****************************************************************}

unit SearchFile;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

{$IFDEF VER125}
  {$D-}  //! StH: This works around a bug in C++Builder 4.0
{$ENDIF VER125}

{$IFDEF VER110}
  {$D-} //! StH: Work around a compiler bug in C++Builder 3.0
{$ENDIF VER110}

uses
  Windows, SysUtils, Classes, Dialogs;

type

  TSearchOption = (soCaseSensitive, soWholeWord, soRegEx);

  TSearchOptions = set of TSearchOption;

  TFoundEvent = procedure(Sender: TObject; LineNo: Integer; Line: string; SPos, FEditReaderPos: Integer) of object;

  TSearcher = class(TObject)
  private
    FSearchStream: TStream;
    FEof: Boolean;
    FSearchBuffer: PChar;
    BLine: PChar; //? What is that
    FBufferSearchPos: Integer;
    FBufferDataCount: Integer;
    FLineNo: Integer;
    FBufSize: Integer;
    FFileName: string;
    FNoComments: Boolean;
    FCurlyCommentActive: Boolean;
    FStarCommentActive: Boolean;
    FOnFound: TFoundEvent;
    FOnStartSearch: TNotifyEvent;
    FPattern: PChar;
    FSearchOptions: TSearchOptions;
    LoCase: function(const Ch: Char): Char;
    procedure Reset;
    procedure FillBuffer;
    //function GetLineCount: Integer;
    procedure SetBufSize(New: Integer);
    procedure PatternMatch;
    function GetANSICompatible: Boolean;
    procedure SetANSICompatible(const Value: Boolean);
  protected
    procedure SetFileName(const Value: string);
    procedure SearchForm;
    procedure FreeObjects;
    procedure DoSearch;
  public
    constructor Create(const SearchFileName: string);
    destructor Destroy; override;
    procedure Execute;
    procedure SetPattern(const Source: string);
    property Pattern: PChar read FPattern;
  published
    property BufSize: Integer read FBufSize write SetBufSize;
    property SearchOptions: TSearchOptions read FSearchOptions write FSearchOptions;
    property FileName: string read FFileName write SetFileName;
    property NoComments: Boolean read FNoComments write FNoComments;
    property ANSICompatible: Boolean read GetANSICompatible write SetANSICompatible;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property OnStartSearch: TNotifyEvent read FOnStartSearch write FOnStartSearch;
  end;

const
  opCHAR = 1;
  opBOL = 2;
  opEOL = 3;
  opANY = 4;
  opCLASS = 5;
  opNCLASS = 6;
  opSTAR = 7;
  opPLUS = 8;
  opMINUS = 9;
  opALPHA = 10;
  opDIGIT = 11;
  opNALPHA = 12;
  opPUNCT = 13;
  opRANGE = 14;
  opENDPAT = 15;

  LastPatternChar = Char(opENDPAT);

implementation


const
  GrepPatternSize = 512;

//! StH: Optimize this, perhaps some BASM
(*
function ANSILoCase(const Ch: Char): Char;
var
  ConversionChar: PChar;
begin
  ConversionChar := PChar(MakeLong(Ord(Ch), 0));
  CharLower(ConversionChar);
  Result := Chr(LoWord(ConversionChar));
end;
*)

function ANSILoCase(const Ch: Char): Char;
var
  w: Word;
begin
  w := MakeWord(Ord(Ch), 0);
  CharLower(PChar(@w));
  Result := Char(Lo(w));
end;

function ASCIILoCase(const Ch: Char): Char;
begin
  if Ch in ['A'..'Z'] then
    Result := Char(Ord(Ch) + 32)
  else
    Result := Ch;
end;

constructor TSearcher.Create(const SearchFileName: string);
begin
  inherited Create;

  FBufSize := 2048;
  FSearchStream := TMemoryStream.Create;
{
  // everything is automatically initialized to
  // these values
  FEof := False;
  FLineNo := 0;
  FNoComments := False;
  FCurlyCommentActive := False;
  FStarCommentActive := False;
  FBufferSearchPos := 0;
  FBufferDataCount := 0;
  FSearchBuffer := nil;
  FName := '';
}
  BLine := StrAlloc(1024);
  FPattern := StrAlloc(GrepPatternSize);
  if SearchFileName <> '' then
    SetFileName(SearchFileName);
  LoCase := ASCIILoCase;
end;

destructor TSearcher.Destroy;
begin
  FSearchStream.Free;
  FSearchStream := nil;

  StrDispose(FSearchBuffer);
  FSearchBuffer := nil;

  StrDispose(BLine);
  BLine := nil;

  StrDispose(FPattern);
  FPattern := nil;

  inherited Destroy;
end;

procedure TSearcher.SearchForm;
var
  FormStream: TStream;
  origFormat: TStreamOriginalFormat;
begin
    FreeObjects;
    if FileExists(FFileName) then
    begin
      FormStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      try
        FSearchStream := TMemoryStream.Create;
        ObjectResourceToText(FormStream, FSearchStream, origFormat);
      finally
        FormStream.Free;
      end;
    end;

    if FSearchStream <> nil then
    begin
      Reset;
      DoSearch;
    end;
end;

procedure TSearcher.FreeObjects;
begin
  if FFileName <> '' then
  begin
    FSearchStream.Free;
    FSearchStream := nil;
  end;
end;

procedure TSearcher.SetFileName(const Value: string);
begin
//  FreeObjects;
  FFileName := Value;
  if FFileName <> '' then
    Reset;
  TMemoryStream(FSearchStream).LoadFromFile(FFileName);
end;

function TSearcher.GetANSICompatible: Boolean;
begin
  if @LoCase = @ANSILoCase then
    Result := True
  else
    Result := False;
end;

procedure TSearcher.SetANSICompatible(const Value: Boolean);
begin
  if Value then
    LoCase := ANSILoCase
  else
    LoCase := ASCIILoCase;
end;

procedure TSearcher.Reset;
resourcestring
  SSearcherReset = 'Reset exception:'#13#10;
begin
    if FFileName = '' then
      Exit;

    FBufferSearchPos := 0;
    FBufferDataCount := 0;
    FLineNo := 0;
    FEof := False;
    FCurlyCommentActive := False;
    FStarCommentActive := False;
    FSearchStream.Position := 0;
end;

procedure TSearcher.FillBuffer;
var
  AmountOfBytesToRead: Integer;
  SkippedCharactersCount: Integer;
  LineEndScanner: PChar;
begin
  if FSearchBuffer = nil then
    FSearchBuffer := StrAlloc(FBufSize);
  FSearchBuffer[0] := #0;

  // Read at most (FBufSize - 1) bytes
  AmountOfBytesToRead := FBufSize - 1;

  FBufferDataCount := FSearchStream.Read(FSearchBuffer^, AmountOfBytesToRead);

  FEof := (FBufferDataCount = 0);

  // Reset buffer position to zero
  FBufferSearchPos := 0;

  // If we filled our buffer completely, there is a chance that
  // the last line was read only partially.
  // Since our search algorithm is line-based,
  // skip back to the end of the last completely read line.
  if FBufferDataCount = AmountOfBytesToRead then
  begin
    // Get pointer on last character of read data
    LineEndScanner := FSearchBuffer + FBufferDataCount - 1;
    // We have not skipped any characters yet
    SkippedCharactersCount := 0;
    // While we still have data in the buffer,
    // do scan for a line break as characterised
    // by a #13#10 or #10#13 or a single #10.
    // Which sequence exactly we hit is not important,
    // we just need to find and line terminating
    // sequence.
    while FBufferDataCount > 0 do
    begin
      if LineEndScanner^ = #10 then
      begin
        FSearchStream.Seek(-SkippedCharactersCount, soFromCurrent);

        // Done with finding last complete line
        Break;
      end;

      Inc(SkippedCharactersCount);
      Dec(FBufferDataCount);
      Dec(LineEndScanner);
    end;

    // With FBufferPos = 0 we have scanned back in our
    // buffer and not found any line break; this means
    // that we cannot employ our pattern matcher on a
    // complete line -> Internal Error.
    if FBufferDataCount = 0 then
    begin
      { TODO -oStefan -cIssue: Complete error handling for the case where
                               a single line exceeds FBufSize-1 characters }
      raise Exception.Create('Grep: Internal line length error.  Binary file?'); //! FIXME
    end;
  end;


  // Cut off everything beyond the line break
  // Assert(FBufferDataCount >= 0);
  FSearchBuffer[FBufferDataCount] := #0;
end;

procedure TSearcher.Execute;
var
  UpperFileExt: string;
begin
  UpperFileExt := UpperCase(ExtractFileExt(FFileName));

  if UpperFileExt = '.DFM' then
  begin
    SearchForm;
  end
  else DoSearch;
end;

procedure TSearcher.DoSearch;
var
  i: Integer;
  t: Integer;
  LPos: Integer;
  UseChar: Boolean;
begin
  if FFileName = '' then
    Exit;
  if Assigned(FOnStartSearch) then
    FOnStartSearch(Self);
  LPos := 0;
  while not FEof do
  begin
    { read new data in }
    if (FBufferSearchPos >= FBufferDataCount) or (FBufferDataCount = 0) then
      FillBuffer;
    if FEof then Exit;
    for i := FBufferSearchPos to FBufferDataCount - 1 do
    begin
      UseChar := False;
      case FSearchBuffer[i] of
        #0:
          begin
            FBufferSearchPos := FBufferDataCount + 1;
            Break;
          end;
        #10:
          begin
            FBufferSearchPos := i + 1;
            Break;
          end;
        #13:
          begin
            FBufferSearchPos := i + 1;
            if FSearchBuffer[FBufferSearchPos] = #10 then Inc(FBufferSearchPos);
            Break;
          end;
        // TODO -oStefan -cC++Builder: C(++) comments are a major problem here;
        '(':
          if FNoComments and not FCurlyCommentActive and (FSearchBuffer[i + 1] = '*') then
            FStarCommentActive := True
          else
            if not (FCurlyCommentActive or FStarCommentActive) then
              UseChar := True;
        ')':
          if (i > 1) and (FNoComments) and not FCurlyCommentActive and (FSearchBuffer[i - 1] = '*') then
            FStarCommentActive := False
          else
            if not (FCurlyCommentActive or FStarCommentActive) then
              UseChar := True;
        '/':
          if FNoComments then
          begin
            if not (FCurlyCommentActive or FStarCommentActive) then
              if FSearchBuffer[i + 1] = '/' then
              begin
                t := i;
                while (t <= FBufferDataCount - 1) and not (FSearchBuffer[t] in [#0, #13]) do
                  Inc(t);
                FBufferSearchPos := t + 1;
                if (t < FBufferDataCount - 1) and (FSearchBuffer[FBufferSearchPos] = #10) then
                  Inc(FBufferSearchPos);
                BLine[LPos] := #0;
                Inc(LPos);
                Break;
              end
              else
                if not (FCurlyCommentActive or FStarCommentActive) then
                  UseChar := True;
          end
          else
            UseChar := True;
        '{':
          if FNoComments and not FStarCommentActive then
            FCurlyCommentActive := True
          else
            if not (FCurlyCommentActive or FStarCommentActive) then
              UseChar := True;
        '}':
          if FNoComments and not FStarCommentActive then
            FCurlyCommentActive := False
          else
            if not (FCurlyCommentActive or FStarCommentActive) then
              UseChar := true;
      else
        if not (FCurlyCommentActive or FStarCommentActive) then
          UseChar := True;
      end;
      if UseChar then
      begin
        if not (soCaseSensitive in SearchOptions) then
          BLine[LPos] := LoCase(FSearchBuffer[i])
        else
          BLine[LPos] := FSearchBuffer[i];
        Inc(LPos);
        if LPos >= 1023 then //! StH: 1023 somehow related to the 1024 StrAlloc of BLine? What for?
          Exit; { Binary not text file }
      end;
    end;
    if FSearchBuffer[i] <> #0 then Inc(FLineNo);
    BLine[LPos] := #0;
    if BLine[0] <> #0 then PatternMatch;
    LPos := 0;
    if FBufferSearchPos < i then FBufferSearchPos := i;
  end;
end;

procedure TSearcher.SetBufSize(New: Integer);
begin
  if (FSearchBuffer = nil) and (New <> FBufSize) then
    FBufSize := New;
end;

(*
function TSearcher.GetLineCount: Integer;
begin
  if FMode = mmModule then
    Result := EditIntF.LinesInBuffer
  else
    Result := -1;
end;
*)

procedure TSearcher.SetPattern(const Source: string);
var
  PatternCharIndex: Integer;
  SourceCharIndex: Integer;

  procedure Store(Ch: Char);
  begin
    Assert(PatternCharIndex < GrepPatternSize, 'Buffer overrun!');
    if not (soCaseSensitive in SearchOptions) then
      FPattern[PatternCharIndex] := LoCase(Ch)
    else
      FPattern[PatternCharIndex] := Ch;
    Inc(PatternCharIndex);
  end;

  procedure cclass;
  resourcestring
    SClassNotTerminated = 'Class at %d did not terminate properly';
  var
    cstart: Integer;
  begin
    cstart := SourceCharIndex;
    Inc(SourceCharIndex);
    if Source[SourceCharIndex] = '^' then
      Store(Char(opNCLASS))
    else
      Store(Char(opCLASS));

    { changed: 10/22 1998 by dg, more info: see bottom }
    while (SourceCharIndex <= Length(Source)) and (Source[SourceCharIndex] <> ']') do
    begin
      if (Source[SourceCharIndex] = '-') and
        (SourceCharIndex - cstart > 1) and
        (Source[SourceCharIndex + 1] <> ']') and
        (SourceCharIndex < Length(Source)) then
      begin
        Dec(PatternCharIndex, 2);
        Store(Char(opRANGE));
        Store(Source[SourceCharIndex - 1]);
        Store(Source[SourceCharIndex + 1]);
        Inc(SourceCharIndex, 2);
      end
      else
      begin
        Store(Source[SourceCharIndex]);
        Inc(SourceCharIndex);
      end;
    end;

    if (Source[SourceCharIndex] <> ']') or (SourceCharIndex > Length(Source)) then
      raise Exception.CreateFmt(SClassNotTerminated, [cstart]);

    Inc(SourceCharIndex); { To push past close bracket }
  end;

resourcestring
  SPatternTooLong = 'Grep pattern too long. (> 500 characters)';
  SInvalidGrepSearchCriteria = 'Character immediately following: at %d is not a valid grep search criteria';
  SSenselessEscape = 'Escape character ("\") without a following character does not make sense';
begin
  //! Warning: this does not properly protect against pattern overruns
  // A better solution needs to be found for this, possibly by sacrificing
  // a bit of performance for a test in the pattern storage code where a
  // new Assert has been introduced.
  if Length(Source) > 500 then
    raise Exception.Create(SPatternTooLong);

  try
    SourceCharIndex := 1;
    PatternCharIndex := 0;
    while SourceCharIndex <= Length(Source) do
    begin
      if not (soRegEx in SearchOptions) then
      begin
        Store(Char(opCHAR));
        Store(Source[SourceCharIndex]);
        Inc(SourceCharIndex);
      end
      else
      begin
(*
      if (Source[SourceCharIndex]='*') or (Source[SourceCharIndex]='+') or (Source[SourceCharIndex]='-') then
      if (SourceCharIndex=1) or
         ((PatternCharIndex>1) and (  (PBuf[lp-1]=char(opBOL)) or
                        (PBuf[PatternCharIndex-1]=char(opEOL)) or
                        (PBuf[PatternCharIndex-1]=char(opSTAR)) or
                        (PBuf[PatternCharIndex-1]=char(opPLUS)) or
                        (PBuf[PatternCharIndex-1]=char(opMINUS)))) then
         begin
         Store(Char(opENDPAT));
         Raise Exception.Create('Bad pattern at character '+intToStr(SourceCharIndex));
         end;
*)
        case Source[SourceCharIndex] of
          '^':
            begin
              Store(Char(opBOL));
              Inc(SourceCharIndex);
            end;

          '$':
            begin
              Store(Char(opEOL));
              Inc(SourceCharIndex);
            end;

          '.':
            begin
              Store(Char(opANY));
              Inc(SourceCharIndex);
            end;

          '[':
            cclass;

          ':':
            begin
              if SourceCharIndex < Length(Source) then
              begin
                case UpCase(Source[SourceCharIndex + 1]) of
                  'A': Store(Char(opALPHA));
                  'D': Store(Char(opDIGIT));
                  'N': Store(Char(opNALPHA));
                  ' ': Store(Char(opPUNCT));
                else
                  Store(Char(opENDPAT));
                  //! ????? Store followed by Exception?
                  raise Exception.CreateFmt(SInvalidGrepSearchCriteria, [SourceCharIndex]);
                end;
                Inc(SourceCharIndex, 2);
              end
              else
              begin
                Store(Char(opCHAR));
                Store(Source[SourceCharIndex]);
                Inc(SourceCharIndex);
              end;
            end;

          '\':
            begin { changed: 10/22 1998 by dg, more info: see bottom }
              if SourceCharIndex >= Length(Source) then
                raise Exception.Create(SSenselessEscape);

              Store(Char(opCHAR));
              Store(Source[SourceCharIndex + 1]);
              Inc(SourceCharIndex, 2);
            end;
        else
          Store(Char(opCHAR));
          Store(Source[SourceCharIndex]);
          Inc(SourceCharIndex);
        end; // case
      end;
    end;
  finally
    Store(Char(opENDPAT));
    Store(#0);
  end;
end;

procedure TSearcher.PatternMatch;
var
  l, p: Integer; // line and pattern pointers
//e: Integer;     // End for STAR and PLUS match
  op: Char; // Pattern operation
//n: Integer;     // Class Counter
//are: String;    // Start of STAR match
  linepos: Integer;

  procedure IsFound;
  var
    S: Integer;
    E: Integer;
  begin
    { TODO -oStefan -cIssue : Note that this algorithm will identify  "GX_GExperts" as a word for "GExperts" }
    if soWholeWord in SearchOptions then
    begin
      S := linepos - 2;
      E := l;
      if (S > 0) and IsCharAlpha(BLine[S]) then
        Exit;
      if (BLine[E] <> #0) and IsCharAlpha(BLine[E]) then
        Exit;
    end;
    if Assigned(FOnFound) then
      FOnFound(Self, FLineNo, BLine, linepos, l);
  end;

begin
  if FPattern[0] = Char(opENDPAT) then
    Exit;
  linepos := 0;

  { Don't bother pattern matching if first search is opCHAR, just go to first match directly }
  { Results in about a 5% to 10% speed increase }
  if (FPattern[0] = Char(opCHAR)) and not (soCaseSensitive in SearchOptions) then
    while (FPattern[1] <> BLine[linepos]) and (BLine[linepos] <> #0) do
      Inc(Linepos);

  while BLine[linepos] <> #0 do
  begin
    l := linepos;
    p := 0;
    op := FPattern[p];
    while op <> Char(opENDPAT) do
    begin
      case Ord(op) of
        opCHAR:
          begin
            if not (Bline[l] = FPattern[p + 1]) then
              Break;
            Inc(p, 2);
          end;

        opBOL:
          begin
            Inc(p);
          end;

        opEOL:
          begin
            if BLine[l] in [#0, #10, #13] then
              Inc(p)
            else
              Break;
          end;

        opANY:
          begin
            if BLine[l] in [#0, #10, #13] then
              Break;
            Inc(p);
          end;

        opCLASS:
          begin
            Inc(p);
            { Compare letters to find a match }
            while (FPattern[p] > LastPatternChar) and (FPattern[p] <> Bline[l]) do
              Inc(p);
            { Was a match found? }
            if FPattern[p] <= LastPatternChar then
              Break;
            { move pattern pointer to next opcode }
            while FPattern[p] > LastPatternChar do
              Inc(p);
          end;

        opNCLASS:
          begin
            Inc(p);
            { Compare letters to find a match }
            while (FPattern[p] > LastPatternChar) and (FPattern[p] <> Bline[l]) do
              Inc(p);
            if FPattern[p] > LastPatternChar then
              Break;
          end;

        opALPHA:
          begin
            if not IsCharAlpha(BLine[l]) then
              Break;
            Inc(p);
          end;

        opDIGIT:
          begin
            if not (BLine[l] in ['0'..'9']) then
              Break;
            Inc(p);
          end;

        opNALPHA:
          begin
            //! StH: Is the second part of the clause correct?
            // This appears to be a test for alphanumerics - if it is, then
            // the clause is incorrect and should be
            //        if IsCharAlphaNumeric(BLine[l]) then
            if IsCharAlpha(BLine[l]) or ((BLine[l] < '0') or (BLine[l] > '9')) then
              Inc(p)
            else
              Break;
          end;

        opPUNCT:
          begin
            if (BLine[l] = ' ') or (BLine[l] > #64) then
              Break;
            Inc(p);
          end;

        opRANGE:
          begin
            if (BLine[l] < FPattern[p + 1]) or (BLine[l] > FPattern[p + 2]) then
              Break;
            Inc(p, 3);
          end;
      else
        Inc(p);
      end; { case }

      if (op = Char(opBOL)) and not (BLine[l] in [#9, #32]) then
        Exit; { Means that we did not match at start }

      op := FPattern[p];
      Inc(l);
    end; { while op <> opENDPAT }
    Inc(LinePos);
    if op = Char(opENDPAT) then
      IsFound;
  end; { while BLine[LinePos] <> #0 }
end;

{
change notes:

changed made by dg (dgerhard@bigfoot.com) @ 22/10 1998:

1) '\'-behaviour: changed to standard grep style
new meaning: any character following '\' is treated as a normal character (= is being quoted).
Only exception: inside classes (see next remark).
examples:
 search for a '[': -> searchstring = '\[',
 search for a '\' -> searchstring = '\\',
 search for '\\' -> searchstring = '\\\\'.

2) removed '\'-treatment in classes ([...]) because it didn't work anyway and excape-functionality inside
of classes is very seldomly useful.
btw.: other grep implementations dont's treat '\' as escape-character in classes, too :-)
}

end.



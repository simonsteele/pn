{***************************************************************
 *
 * Unit Name: HotKey
 * Purpose  : Edit component for inputting keyboard shortcuts.
 * Author   : David Brock & Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Portions Copyright David
 *			      Brock. Please read the license agreement at
 *			      www.pnotepad.org/press/psidx.html.
 *
 ****************************************************************}

unit HotKey;

interface

uses stdctrls, classes, messages, menus, windows;

type
  tEchoHotKey = class(TCustomEdit)
  private
    FShortCut: TShortCut;
    procedure SetShortCut(const Value: TShortcut);
    procedure UpdateDisplay;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   KeyDown(var Key: Word; Shift: TShiftState); override;
    property    HotKey : TShortcut read FShortCut write SetShortCut;
  end;

implementation

constructor tEchoHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := true;
end;

procedure tEchoHotKey.UpdateDisplay;
begin
  if 0 <> FShortCut then
     Text := ShortCutToText(FShortCut)
  else
     Text := '(None)';
end;

procedure tEchoHotKey.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if vk_back <> Key then
     SetShortCut(Shortcut(Key, Shift))
  else
     //SetShortCut(TextToShortCut(''));
     SetShortCut(0);
end;

procedure tEchoHotKey.SetShortCut(const Value: TShortcut);
begin
  FShortCut := Value;
  UpdateDisplay;
end;

procedure tEchoHotKey.WndProc(var Message: TMessage);
begin
  with Message do
   case Msg of
     WM_GETDLGCODE :
        begin
          inherited WndProc(Message);
          result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
          exit;
        end;
     else inherited WndProc(Message);
   end;
end;

end.
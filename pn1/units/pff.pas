 unit pff;
{This unit has no procedures or functions because it is a modal form... All of
 the code is processed in the main form which waits for this form to close.}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmPFF = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    memPFF: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end; { TfrmPFF = class(TForm) }

var
  frmPFF: TfrmPFF;

implementation

{$R *.DFM}

end.

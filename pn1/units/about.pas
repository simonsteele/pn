{***************************************************************
 *
 * Unit Name: about
 * Purpose  : About Window
 * Author   : Simon Steele
 * Copyright: This Source Code is Copyright © 1998-2000 Echo
 *            Software and Simon Steele. Please read the license 
 *			  agreement at www.pnotepad.org/press/psidx.html.
 **************************************************************}

unit about;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, UrlLabel, ComCtrls;

type
  TfrmAbout = class(TForm)
    ProgramIcon: TImage;
    Version: TLabel;
    Build: TLabel;
    copyright: TLabel;
    UrlLabel2: TUrlLabel;
    Label3: TLabel;
    Label2: TLabel;
    UrlLabel1: TUrlLabel;
    OKButton: TButton;
    Image1: TImage;
    Label1: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.DFM}

uses useful;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
   self.Icon := application.icon;
   version.caption := GetFileInformation(application.ExeName, 'ProductVersion');
   build.caption := 'Build: ' + GetFileInformation(application.ExeName, 'FileVersion');
   copyright.caption := GetFileInformation(application.ExeName, 'LegalCopyright');
end;

end.


unit FrameForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmFrame = class(TFrame)
    lblScore: TLabel;
    lblBall1: TLabel;
    bvl1: TBevel;
    Bevel1: TBevel;
    lblBall2: TLabel;
    lblBall3: TLabel;
    shp: TShape;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.

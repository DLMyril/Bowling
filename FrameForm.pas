unit FrameForm; // contains the individual frame display

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
    lblTotal: TLabel;
    Bevel2: TBevel;
  private
    { Private declarations }
  public
    procedure Populate(AScore, ATotal: integer; b1, b2, b3: string);
    procedure Blank;
  end;

implementation

{$R *.dfm}

{ TfmFrame }

procedure TfmFrame.Blank;
begin
  lblScore.Caption := '';
  lblBall1.Caption := '';
  lblBall2.Caption := '';
  lblBall3.Caption := '';
  lblTotal.Caption := '';
end;

procedure TfmFrame.Populate(AScore, ATotal: integer; b1, b2, b3: string);
begin
  lblScore.Caption := IntToStr(AScore);
  lblTotal.Caption := IntToStr(ATotal + AScore);
  lblBall1.Caption := b1;
  lblBall2.Caption := b2;
  lblBall3.Caption := b3;
end;

end.

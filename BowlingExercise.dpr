program BowlingExercise;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  BowlingInt in 'BowlingInt.pas',
  GameUnit in 'GameUnit.pas',
  BowlFrameUnit in 'BowlFrameUnit.pas',
  BowlError in 'BowlError.pas',
  FrameForm in 'FrameForm.pas' {fmFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Bowling Exercise';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

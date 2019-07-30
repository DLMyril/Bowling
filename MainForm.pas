unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, System.Actions, Vcl.ActnList,
  FrameForm, Vcl.ExtCtrls, Generics.Collections,
  BowlingInt, GameUnit;

type
  TfrmMain = class(TForm)
    grd: TGridPanel;
    fmFrame1: TfmFrame;
    fmFrame2: TfmFrame;
    fmFrame3: TfmFrame;
    fmFrame4: TfmFrame;
    fmFrame5: TfmFrame;
    fmFrame6: TfmFrame;
    fmFrame7: TfmFrame;
    fmFrame8: TfmFrame;
    fmFrame9: TfmFrame;
    fmFrame10: TfmFrame;
    al: TActionList;
    mnu: TMainMenu;
    Button1: TButton;
    acStart: TAction;
    acRoll: TAction;
    acScore: TAction;
    Button2: TButton;
    Button3: TButton;
    miSetup: TMenuItem;
    miBowl: TMenuItem;
    miScore: TMenuItem;
    edtRoll: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acRollExecute(Sender: TObject);
    procedure acScoreExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
  private
    Game: TGame;
    BowlLine: TBowlLine;
    FrameArray: TList<TfmFrame>;
    function FrameCharacters(AFrame: IBowlFrame): string;
  public
    procedure AppException(Sender: TObject; E: Exception);
    procedure PopulateLine(ABowlLine: TBowlLine);
    procedure PopulateFrame(ABowlLine: TBowlLine; AFrameIdx: integer);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  BowlError;

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.acRollExecute(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.Text := edtRoll.Text;
    for i := 0 to (sl.Count - 1) do begin
      Game.Roll(StrToIntDef(sl[i], 0));
    end;
  finally
    sl.Free;
  end;
end;

procedure TfrmMain.acScoreExecute(Sender: TObject);
begin
  BowlLine := Game.ScoreByFrame;
  PopulateLine(BowlLine);
end;

procedure TfrmMain.acStartExecute(Sender: TObject);
begin
  Game.Start;
  BowlLine.Clear;
end;

procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  if E is EBowlException then begin
    ShowMessage(E.Message); // prep for any particular error handling regarding EBowlException
  end else begin
    Application.ShowException(E);
    Application.Terminate;
  end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  dt: TDateTime;
  y, m, d, h, n, s, e: word;
begin
  Application.OnException := AppException;
  Game := TGame.Create;
  Game.Start;
  BowlLine := TBowlLine.Create;
  FrameArray := TList<TfmFrame>.Create;
  FrameArray.Add(fmFrame1);
  FrameArray.Add(fmFrame2);
  FrameArray.Add(fmFrame3);
  FrameArray.Add(fmFrame4);
  FrameArray.Add(fmFrame5);
  FrameArray.Add(fmFrame6);
  FrameArray.Add(fmFrame7);
  FrameArray.Add(fmFrame8);
  FrameArray.Add(fmFrame9);
  FrameArray.Add(fmFrame10);
  for i := 1 to System.ParamCount do begin // check command line flags
    if (copy(Uppercase(System.ParamStr(i)), 1, 4) = 'log=') then begin // log file name
      LogFileName := copy(System.ParamStr(i), 5, length(System.ParamStr(i)));
    end;
  end;
  if LogFileName = '' then begin
    dt := now;
    DecodeDate(dt, y, m, d);
    DecodeTime(dt, h, n, s, e);
    LogFileName := Format('Log%d%d%d%d%d%d.txt', [y, m, d, h, n, s]);
  end;
  LogFileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + LogFileName;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FrameArray.Clear;
end;

function TfrmMain.FrameCharacters(AFrame: IBowlFrame): string;  // 2 character score string in display order for regular frames
begin
  case AFrame.BowlFrameType(False) of // uses a standard frame
    frameOpen: Result := IntToStr(AFrame.Roll[1]) + IntToStr(AFrame.Roll[2]);
    frameSpare: Result := IntToStr(AFrame.Roll[1]) + '/';
    frameStrike: Result := ' X';
  else
    Result := '  ';
  end;
end;

procedure TfrmMain.PopulateFrame(ABowlLine: TBowlLine; AFrameIdx: integer);
var
  BallDisplay: string;
begin
  FrameArray[AFrameIdx].lblScore.Caption := IntToStr(ABowlLine.Items[AFrameIdx].CurrentScore(AFrameIdx = 9));
  BallDisplay := FrameCharacters(ABowlLine.Items[AFrameIdx]);
  FrameArray[AFrameIdx].lblBall1.Caption := BallDisplay[1];
  FrameArray[AFrameIdx].lblBall2.Caption := BallDisplay[2];
  FrameArray[AFrameIdx].lblBall3.Caption := '';
end;

procedure TfrmMain.PopulateLine(ABowlLine: TBowlLine);
var
  i: integer;
  BallDisplay: string;
begin
  for i := 0 to 8 do begin
    PopulateFrame(ABowlLine, i);
  end;
  FrameArray[9].lblScore.Caption := IntToStr(ABowlLine.Items[9].CurrentScore(True));
  BallDisplay := FrameCharacters(ABowlLine.Items[9]);
  if (ABowlLine.Items[9].BowlFrameType(True) = frameStrike) then begin
    FrameArray[9].lblBall1.Caption := 'X';
    if (ABowlLine.Items[9].Roll[2] < 10) then
      FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[2])
    else
      FrameArray[9].lblBall1.Caption := 'X';
    if (ABowlLine.Items[9].Roll[3] < 10) then
      FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[3])
    else
      FrameArray[9].lblBall1.Caption := 'X';
  end else if (ABowlLine.Items[9].BowlFrameType(True) = frameSpare) then begin
    FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[1]);
    FrameArray[9].lblBall2.Caption := '/';
    FrameArray[9].lblBall3.Caption := IntToStr(ABowlLine.Items[9].Roll[3]);
  end else begin
    FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[1]);
    FrameArray[9].lblBall2.Caption := IntToStr(ABowlLine.Items[9].Roll[2]);
    FrameArray[9].lblBall3.Caption := '';
  end;
end;

end.

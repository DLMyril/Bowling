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
    acStart: TAction;
    acRoll: TAction;
    acScore: TAction;
    btnRoll: TButton;
    miSetup: TMenuItem;
    miBowl: TMenuItem;
    miScore: TMenuItem;
    edtRoll: TEdit;
    ttlTotalScore: TLabel;
    lblTotalScore: TLabel;
    acExit: TAction;
    miExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acRollExecute(Sender: TObject);
    procedure acScoreExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
    procedure edtRollKeyPress(Sender: TObject; var Key: Char);
    procedure acExitExecute(Sender: TObject);
  private
    Game: TGame;
    BowlLine: TBowlLine;
    FrameArray: TList<TfmFrame>;
    function FrameCharacters(AFrame: IBowlFrame): string;
    procedure Display;
  public
    procedure AppException(Sender: TObject; E: Exception);
    procedure PopulateLine(ABowlLine: TBowlLine);
    procedure PopulateFrame(var ATotal: integer; ABowlLine: TBowlLine; AFrameIdx: integer);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  BowlError;

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acRollExecute(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.DelimitedText := edtRoll.Text;
    for i := 0 to (sl.Count - 1) do begin
      Game.Roll(StrToIntDef(sl[i], 0));
    end;
  finally
    sl.Free;
  end;
  Display;
end;

procedure TfrmMain.acScoreExecute(Sender: TObject);
begin
  Display;
end;

procedure TfrmMain.acStartExecute(Sender: TObject);
begin
  Game.Start;
  Display;
end;

procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  if E is EBowlException then begin
    ShowMessage(E.Message); // prep for any particular error handling regarding EBowlException
    AddLogEntry(E.Message);
    Display;
  end else if E is EInOutError then begin
    Application.ShowException(E);
  end else begin
    Application.ShowException(E);
    AddLogEntry(E.Message);
//    Application.Terminate;
  end;

end;

procedure TfrmMain.Display;
begin
  BowlLine := Game.ScoreByFrame;
  PopulateLine(BowlLine);
  lblTotalScore.Caption := IntToStr(Game.TotalScore);
end;

procedure TfrmMain.edtRollKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    acRollExecute(Sender);
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

  BowlLine.Clear;
  PopulateLine(BowlLine);
  lblTotalScore.Caption := '0';

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

function TfrmMain.FrameCharacters(AFrame: IBowlFrame): string;  // 2 character score string in display order for regular frames; not used for last frame
var
  FrameType: TBowlFrameType;
  WorkStr: string;
begin
  FrameType := AFrame.BowlFrameType(0); // Doesn't matter what frame in this case.
  case FrameType of // uses a standard frame
    frameIncomplete,
    frameOpen: begin
      if (AFrame.Roll[1] >= 0) then
        WorkStr := IntToStr(AFrame.Roll[1])
      else
        WorkStr := ' ';
      if (AFrame.Roll[2] >= 0) then
        Result := WorkStr + IntToStr(AFrame.Roll[2])
      else
        Result := WorkStr + ' ';
    end;
    frameSpare: Result := IntToStr(AFrame.Roll[1]) + '/';
    frameStrike: Result := ' X';
  else
    Result := '  ';
  end;
end;

procedure TfrmMain.PopulateFrame(var ATotal: integer; ABowlLine: TBowlLine; AFrameIdx: integer);
var
  BallDisplay: string;
  Score: integer;
begin
  if (AFrameIdx < ABowlLine.Count) then begin
    BallDisplay := FrameCharacters(ABowlLine.Items[AFrameIdx]);
    Score := ABowlLine.Items[AFrameIdx].CurrentScore(AFrameIdx + 1);
    FrameArray[AFrameIdx].Populate(Score, ATotal, BallDisplay[2], BallDisplay[1], '');
    if (ABowlLine.Items[AFrameIdx].BowlFrameType(AFrameIdx + 1) <> frameIncomplete) then // add in total of completed frames
      ATotal := ATotal + Score;
  end else begin
    FrameArray[AFrameIdx].Blank;
  end;
end;

procedure TfrmMain.PopulateLine(ABowlLine: TBowlLine);
var
  i: integer;
  BallDisplay: string;
  Total: integer; // this is not the totalscore from the IGame, but the score totalled by the GUI
begin
  Total := 0;
  for i := 0 to 8 do begin
    PopulateFrame(Total, ABowlLine, i);
  end;
  if ABowlLine.Count = 10 then begin // last frame needs to exist
    i := ABowlLine.Items[9].CurrentScore(10);
    FrameArray[9].lblScore.Caption := IntToStr(i);
    Total := Total + i; // normal frames handle this in PopulateFrame
    FrameArray[9].lblTotal.Caption := IntToStr(Total);
    BallDisplay := FrameCharacters(ABowlLine.Items[9]);
    if (ABowlLine.Items[9].BowlFrameType(10) = frameStrike) then begin
      FrameArray[9].lblBall1.Caption := 'X';
      if (ABowlLine.Items[9].Roll[2] < 10) then
        FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[2])
      else
        FrameArray[9].lblBall1.Caption := 'X';
      if (ABowlLine.Items[9].Roll[3] < 10) then
        FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[3])
      else
        FrameArray[9].lblBall1.Caption := 'X';
    end else if (ABowlLine.Items[9].BowlFrameType(10) = frameSpare) then begin
      FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[1]);
      FrameArray[9].lblBall2.Caption := '/';
      FrameArray[9].lblBall3.Caption := IntToStr(ABowlLine.Items[9].Roll[3]);
    end else begin
      FrameArray[9].lblBall1.Caption := IntToStr(ABowlLine.Items[9].Roll[1]);
      FrameArray[9].lblBall2.Caption := IntToStr(ABowlLine.Items[9].Roll[2]);
      FrameArray[9].lblBall3.Caption := '';
    end;
  end else begin
    FrameArray[9].Blank;
  end;
end;

end.

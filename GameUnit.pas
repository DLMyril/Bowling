unit GameUnit;

interface

uses BowlingInt;

type
  TGame = class(TInterfacedObject, IGame)
  private
    FLine: TBowlLine;
    function GetScoreByFrame: TBowlLine;
  public
    procedure Start;
    procedure Roll(ARoll: integer);
    property ScoreByFrame: TBowlLine read GetScoreByFrame;
    function TotalScore: integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,
  BowlError,
  BowlFrameUnit,
  Forms;

{ TGame }

constructor TGame.Create;
begin
  inherited;
  FLine := TBowlLine.Create;
end;

destructor TGame.Destroy;
begin
  if assigned(FLine) then
    FLine.Free;
  inherited;
end;

function TGame.GetScoreByFrame: TBowlLine;
begin
  Result := FLine;
end;

procedure TGame.Roll(ARoll: integer);
var
  Current: IBowlFrame;
  Status: TBowlFrameType;
  LastFrame: boolean;
begin
  LastFrame := (FLine.Count = 10);
  Status := FLine[FLine.Count - 1].BowlFrameType(LastFrame);
  if (FLine.Count = 10) and (Status <> frameIncomplete) then begin // last frame with completed status can't be added to
    raise EBowlException.Create('Trying to add a frame to a line that already has 10 frames.');
  end else begin
    // Set Current to the proper frame
    if (Status = frameIncomplete) or (Current.Roll[1] = -1) then begin // using incomplete frame, second condition shouldn't happen, but...
      Current := FLine[FLine.Count - 1];
    end else begin
      Current := TBowlFrame.Create; // use new frame and put into the line
      FLine[FLine.Count - 1].LinkNextFrame(Current);
      FLine.Add(Current);
    end;
  end;
  Current.AddRoll(ARoll);
end;

procedure TGame.Start;
begin
  FLine.Clear;
end;

function TGame.TotalScore: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to (FLine.Count - 1) do
    Result := Result + FLine[i].CurrentScore(i = (FLine.Count - 1));
end;

end.

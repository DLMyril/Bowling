unit FrameUnit;

interface

uses
  Generics.Collections,
  BowlingInt;

type
  TBowlFrame = class(TInterfacedObject, IBowlFrame)
  private
    fCurrentScore: integer;
    fRoll: array[1..3] of integer;
    fBowlFrameType: TBowlFrameType;
    function GetRoll(AnIdx: integer): integer;
    procedure SetRoll(AnIdx: integer; ARoll: integer);
    function RollRangeCheck(AnIdx: integer): boolean;
    function CurrentScoreCheck(ACurrentScore: integer): boolean;
    function RollCheck(ARoll: integer): boolean;
  public
    function GetCurrentScore: integer;
    procedure SetCurrentScore(ACurrentScore: integer);
    function IsFull(IsLast: boolean): boolean;
    function RollValue(AnIdx: integer): integer;
    function BowlFrameType: TBowlFrameType;
    property Roll[idx: integer]: integer read GetRoll write SetRoll;
    property CurrentScore: integer read GetCurrentScore write SetCurrentScore;
    constructor Create;
  end;


implementation

uses
  SysUtils, BowlError;

{ TFrame }

constructor TBowlFrame.Create;
begin
  inherited;
  fCurrentScore := 0;
  fRoll[1] := -1; fRoll[2] := -1; fRoll[3] := -1;
  fBowlFrameType := frameIncomplete;
end;

function TBowlFrame.CurrentScoreCheck(ACurrentScore: integer): boolean;
begin
  Result := (ACurrentScore >= 0) and (ACurrentScore <= 300);
  if not Result then
    raise EBowlException.Create('Assigning a current score of ' + IntToStr(ACurrentScore) +
                                ' is out of range.  (Scores can be between 0 and 300)');
end;

function TBowlFrame.BowlFrameType: TBowlFrameType;
begin
  Result := fBowlFrameType;
end;

function TBowlFrame.GetCurrentScore: integer;
begin
  result := fCurrentScore;
end;

function TBowlFrame.GetRoll(AnIdx: integer): integer;
begin
  if RollRangeCheck(AnIdx) then
    result := fRoll[AnIdx]
  else
    result := -1;
end;

function TBowl7Frame.IsFull(IsLast: boolean): boolean;
begin
  case fBowlFrameType of
     frameOpen,
     frameSpare: Result := (fRoll[1] > -1) and (fRoll[2] > -1);
     frameStrike: Result := (fRoll[1] > -1) and (fRoll[2] > -1) and (fRoll[3] > -1);
  else
    Result := False;
  end;
end;

function TBowlFrame.RollCheck(ARoll: integer): boolean;
begin
  result := (ARoll >= 0) and (ARoll <= 10);
  if not result then
    raise EBowlException.Create('A roll of ' + IntToStr(ARoll) +
                                ' is out of range.  (It must be between 0 and 10 to be valid)');
end;

function TBowlFrame.RollRangeCheck(AnIdx: integer): boolean;
begin
  result := (AnIdx >= 1) and (AnIdx <= 3);
  if not result then
    raise EBowlException.Create('The index ' + IntToStr(AnIdx) +
                                ' to the roll in a frame is out of range.  (It must be between 1 and 3 to be valid)');
end;

function TBowlFrame.RollValue(AnIdx: integer): integer;  // Used for totalling score only.
begin
  if RollRangeCheck(AnIdx) then begin
    Result := GetRoll(AnIdx);
    if Result = -1 then
      Result := 0;
  end else
    Result := 0;
end;

procedure TBowlFrame.SetCurrentScore(ACurrentScore: integer);
begin
  if CurrentScoreCheck(ACurrentScore) then
    fCurrentScore := ACurrentScore;
end;

procedure TBowlFrame.SetRoll(AnIdx, ARoll: integer);
begin
  if RollRangeCheck(AnIdx) and RollCheck(ARoll) then
    fRoll[AnIdx] := ARoll;
end;

end.

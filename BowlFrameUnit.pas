unit BowlFrameUnit;

interface

uses
  Generics.Collections,
  BowlingInt;

type
  TBowlFrame = class(TInterfacedObject, IBowlFrame)
  private
    fNextFrame: IBowlFrame;
    fRoll: array[1..3] of integer;
    function GetRoll(AnIdx: integer): integer;
    function RollRangeCheck(AnIdx: integer): boolean;
    function RollCheck(ARoll: integer): boolean;
  public
    procedure LinkNextFrame(ANextFrame: IBowlFrame = nil);
    function NextFrame: IBowlFrame;
    function SecondNextFrame: IBowlFrame;
    function BowlFrameType(WhichFrame: integer): TBowlFrameType;
    function AddRoll(ARoll: integer): boolean; // add a roll to the next available position in the frame, or return false;
    function CurrentScore(WhichFrame: integer): integer;
    constructor Create;
    property Roll[idx: integer]: integer read GetRoll;
  end;


implementation

uses
  SysUtils, BowlError;

{ TFrame }

constructor TBowlFrame.Create;
begin
  inherited;
  fRoll[1] := -1;
  fRoll[2] := -1;
  fRoll[3] := -1;
  fNextFrame := nil;
end;

function TBowlFrame.CurrentScore(WhichFrame: integer): integer;
var
  temp: integer;
begin
  Result := 0;
  case BowlFrameType(WhichFrame) of
    frameIncomplete,
    frameOpen: begin
      if Roll[1] <> -1 then Result := Roll[1];
      if Roll[2] <> -1 then Result := Result + Roll[2];
    end;
    frameSpare: begin
      Result := 10;
      if (WhichFrame = 10) then begin
        if (Roll[3] > -1) then begin
          Result := Result + Roll[3];
        end;
      end else begin
        if assigned(fNextFrame) then begin
          if (fNextFrame.Roll[1] > -1) then begin
            Result := Result + fNextFrame.Roll[1];
          end;
        end;
      end;
    end;
    frameStrike: begin
      Result := 10;
      if (WhichFrame = 9) then begin
        if assigned(fNextFrame) then begin
          temp := fNextFrame.Roll[1];
          if (temp > -1) then begin
            Result := Result + temp;
          end;
          temp := fNextFrame.Roll[2];
          if (temp > -1) then begin
            Result := Result + temp;
          end;
        end;
      end else if (WhichFrame = 10) then begin
        if (Roll[2] > -1) then begin
          Result := Result + Roll[2];
        end;
        if (Roll[3] > -1) then begin
          Result := Result + Roll[3];
        end;
      end else begin
        if assigned(fNextFrame) then begin
          temp := fNextFrame.Roll[1];
          if (temp > -1) then begin
            Result := Result + temp;
          end;
          if (temp = 10) then begin // move to the next one
            if (SecondNextFrame <> nil) then begin
              Result := Result + SecondNextFrame.Roll[1];
            end;
          end else begin
            temp := fNextFrame.Roll[2];
            if (temp > -1) then begin
              Result := Result + temp;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TBowlFrame.BowlFrameType(WhichFrame: integer): TBowlFrameType;
begin
  Result := frameIncomplete;
  if (WhichFrame = 10) then begin
    if (fRoll[2] = -1) then begin // it's incomplete no matter what
      Result := frameIncomplete;
    end else if (fRoll[3] <> -1) then begin // it's a spare or strike
      if Roll[1] = 10 then begin
        Result := frameStrike;
      end else begin
        Result := frameSpare;
      end;
    end else if ((fRoll[1] + fRoll[2])< 10) then begin // it's an open frame
      Result := frameOpen;
    end;
  end else begin
    if (fRoll[1] = 10) then begin
      Result := frameStrike;
    end else if ((fRoll[1] + fRoll[2]) = 10) then begin
      Result := frameSpare;
    end else begin
      if (fRoll[2] = -1) then begin
        Result := frameIncomplete;
      end else begin
        Result := frameOpen;
      end;
    end;
  end;
end;

function TBowlFrame.GetRoll(AnIdx: integer): integer;
begin
  if RollRangeCheck(AnIdx) then
    Result := fRoll[AnIdx]
  else
    Result := -1;
end;

procedure TBowlFrame.LinkNextFrame(ANextFrame: IBowlFrame);
begin
  if not assigned(fNextFrame) and assigned(ANextFrame) then
    fNextFrame := ANextFrame;
end;

function TBowlFrame.NextFrame: IBowlFrame;
begin
  NextFrame := fNextFrame;
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

function TBowlFrame.SecondNextFrame: IBowlFrame;
begin
  Result := nil;
  if assigned(fNextFrame) then
    Result := fNextFrame.NextFrame;
end;

function TBowlFrame.AddRoll(ARoll: integer): boolean; // add a roll to the next available position in the frame, or return false;
begin
  Result := RollCheck(ARoll);
  if Result then begin
    if (fRoll[1] = -1) then begin
      fRoll[1] := ARoll;
    end else if (fRoll[2] = -1) then begin
      if (NextFrame = nil) then begin // last frame
        if (fRoll[1] = 10) then begin
          fRoll[2] := ARoll;
        end else begin
          if((fRoll[1] + ARoll) > 10) then begin
            raise EBowlException.Create('Trying to add a ball roll of ' + IntToStr(ARoll) +
                                        ' would result in more than 10 pins in this frame.');
          end else begin
            fRoll[2] := ARoll;
          end;
        end;
      end else begin // all other frames
        if((fRoll[1] + ARoll) > 10) then begin
          raise EBowlException.Create('Trying to add a ball roll of ' + IntToStr(ARoll) +
                                      ' would result in more than 10 pins in this frame.');
        end else begin
          fRoll[2] := ARoll;
        end;
      end;
    end else if (fRoll[3] = -1) and (NextFrame = nil) then begin
      if (fRoll[1] = 10) or ((fRoll[1] + fRoll[2]) >= 10) then begin // spare or strike allows the third roll to be entered
        fRoll[3] := ARoll;
      end else begin
        if (NextFrame = nil) then begin
          raise EBowlException.Create('Trying to add a ball roll of ' + IntToStr(ARoll) +
                                      ' past the allowed number of rolls.  The tenth frame is full.');
        end else begin
          raise EBowlException.Create('Trying to add a ball roll of ' + IntToStr(ARoll) +  // only for unit testing, shouldn't reach this one.
                                      ' past the allowed number of rolls.');
        end;
      end;
    end else begin
      raise EBowlException.Create('Trying to add a ball roll of ' + IntToStr(ARoll) +
                                  ' past the allowed number of rolls.');
    end;
  end;
end;

end.

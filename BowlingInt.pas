unit BowlingInt;

interface

uses
  Generics.Collections;  // apparently this version doesn't have spring.collections

type
  TBowlFrameType = (frameIncomplete, frameOpen, frameSpare, frameStrike);
  IBowlFrame = interface
  ['{642BCAEE-5189-495C-9334-76C12225B149}']
    function BowlFrameType(WhichFrame: integer): TBowlFrameType; // returns the type of frame: incomplete or final state
    procedure LinkNextFrame(NextFrame: IBowlFrame = nil);
    function NextFrame: IBowlFrame;
    function SecondNextFrame: IBowlFrame;
    function CurrentScore(WhichFrame: integer): integer; // return the current score of the frame, dependent on which position the frame is in
    function AddRoll(ARoll: integer): boolean; // add a roll to the next available position in the frame, or return false; ARoll should be between 0 and 10;
    function GetRoll(AnIdx: integer): integer; // return the roll (index should be 1..3)
    property Roll[idx: integer]: integer read GetRoll; // property for retrieving the roll value (idx should be 1..3)
  end;

  TBowlLine = TList<IBowlFrame>;

  IGame = interface
    ['{A7E5EF17-F469-49E6-9C88-C8CD2C118781}']
    procedure Start;
    procedure Roll(ARoll: integer); // only way to get data in
    function GetScoreByFrame: TBowlLine;
    property ScoreByFrame: TBowlLine read GetScoreByFrame;
    function TotalScore: integer;
  end;

implementation

end.


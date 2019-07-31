unit BowlingInt; // unit containing the interfaces and data structures for the app

interface

uses
  Generics.Collections;  // apparently this version of Delphi doesn't have spring.collections

type
  // Represents the state the frame is in at any given time
  TBowlFrameType = (frameIncomplete,  // frame with incomplete information
                    frameOpen,        // frame with no spares or strikes
                    frameSpare,       // frame with scores totalling 10 in 2 throws
                    frameStrike);     // frame with a 10 score for the first throw

  // Frame structure allowing access to a single frame
  IBowlFrame = interface
  ['{642BCAEE-5189-495C-9334-76C12225B149}']
    function BowlFrameType(WhichFrame: integer): TBowlFrameType;  // returns the type of frame: incomplete or final state
    procedure LinkNextFrame(NextFrame: IBowlFrame = nil);         // register (link) new frames with their previous ones
    function NextFrame: IBowlFrame;                               // used to access the next frame in the series for scoring purposes
    function SecondNextFrame: IBowlFrame;                         // used to access two frames in the series for scoring purposes (2 strikes in a row)
    function CurrentScore(WhichFrame: integer): integer;          // return the current score of the frame, dependent on which position the frame is in
    function AddRoll(ARoll: integer): boolean;                    // add a roll to the next available position in the frame, or return false; ARoll should be between 0 and 10;
    function GetRoll(AnIdx: integer): integer;                    // return the roll (index should be 1..3)
    property Roll[idx: integer]: integer read GetRoll;            // property for retrieving the roll value (idx should be 1..3)
  end;

  // List containing the ten frames, last element in the list is the current frame being populated
  TBowlLine = TList<IBowlFrame>;

  // Interface used to manage the game from the users perspective
  IGame = interface
    ['{A7E5EF17-F469-49E6-9C88-C8CD2C118781}']
    procedure Start;                                        // initializes the data, erasing old score data
    procedure Roll(ARoll: integer);                         // only way to get a thrown ball score into the system, expected to be entered in sequential order
    function GetScoreByFrame: TBowlLine;                    // retrieves the scoring structure for the ScoreByFrame property
    property ScoreByFrame: TBowlLine read GetScoreByFrame;  // read only property to get the scoring structure
    function TotalScore: integer;                           // returns the total score of all the entered ball throws up to the currently entered one
  end;

implementation

end.


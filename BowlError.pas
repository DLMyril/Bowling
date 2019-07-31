unit BowlError; // Contains the exception class and log file handling.
                // Also stores some of the test data used during unit and integration testing

interface

uses SysUtils;

type
  EBowlException = class(Exception); // exception class used for the bowling exercise

var
  LogFileName: string; // global containing the requested log file name; used to indicate what file logs will appear in.  Should be initialized by whatever is implementing the IGame interface

procedure AddLogEntry(Msg: string); // Add an entry to the log file

implementation

uses
  Classes;

procedure AddLogEntry(Msg: string);
var
  T: Text;
begin
  if not(FileExists(LogFileName)) then begin
    AssignFile(T, LogFileName);
    Rewrite(T);
  end else begin
    AssignFile(T, LogFileName);
    Append(T);
  end;
  Writeln(T, Msg);
  CloseFile(T);
end;

initialization
  LogFileName := '';

// Test cases
// Throw these into the edit box to test features

// Check all strikes              10 10 10 10 10 10 10 10 10 10 10 10
// Check the gutters              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
// Check too many entries         1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
// Check over 10 in frame         1 2 3 4 5 6
// Check all spares               5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
// Check strike, spare, open      1 1 10 7 3 2 8
// Prep for 10th frame testing    4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
// Check for 10th strike display  4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 10 2 9

end.

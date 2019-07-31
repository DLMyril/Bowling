unit BowlError;

interface

uses SysUtils;

type
  EBowlException = class(Exception);

var
  LogFileName: string;

procedure AddLogEntry(Msg: string);

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

// Check all strikes          10 10 10 10 10 10 10 10 10 10 10 10
// Check the gutters          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
// Check too many entries     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
// Check over 10 in frame     1 2 3 4 5 6
// Check all spares           5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
// Check strike, spare, open  1 1 10 7 3 2 8

end.

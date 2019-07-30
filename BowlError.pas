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

end.

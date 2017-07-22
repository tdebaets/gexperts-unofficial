unit OneD4AllUnit;

interface

procedure Run;

implementation

uses
  Dialogs,
  Windows, Messages, SysUtils;

{ Search all available top-level windows on the Desktop for a Delphi 3 (!)
  application handle }
function EnumWindowsProc(WinHandle: THandle; FoundWindow: PHandle): BOOL; stdcall;
var
  Buffer: array[0..50] of Char;
begin
  Result := True;
  GetClassName(WinHandle, Buffer, SizeOf(Buffer)-1);
  if StrIComp('TApplication', Buffer) = 0 then
  begin
    GetWindowText(WinHandle, Buffer, SizeOf(Buffer)-1);
    if StrIComp('Delphi 3', Buffer) = 0 then
    begin
      FoundWindow^ := WinHandle;
      Result := False;

      // MessageBox(0, 'Found Instance', 'OneD4All', MB_OK);
    end;
  end;
end;

procedure SendFileNameToExistingInstance(const FileName: string);
var
  DelphiApplicationWindow: THandle;
  CopyData: TCopyDataStruct;
  DataToPass: AnsiString;
begin
  ShowMessage(FileName);

  DelphiApplicationWindow := 0;

  EnumWindows(@EnumWindowsProc, Integer(@DelphiApplicationWindow));

  if DelphiApplicationWindow <> 0 then
  begin
    // We pass the fully qualified file name; better safe than sorry...
    DataToPass := ExpandFileName(FileName);

    CopyData.cbData := Length(DataToPass) + 1;
    CopyData.lpData := PChar(DataToPass);
    CopyData.dwData := 0;

    // MessageBox(0, PChar('Sending data to window ' + IntToHex(DelphiApplicationWindow, 8)), PChar(DataToPass), mb_ok);

    SendMessage(DelphiApplicationWindow, WM_COPYDATA, 0, Integer(@CopyData));
  end;
end;


procedure Run;
begin
  // Question is: could we run into a situation where we
  // would want to pass params 2+ to a running instance
  // of Delphi?
  if ParamCount >= 1 then
    SendFileNameToExistingInstance(ParamStr(1));
end;

end.

unit GX_HookDebugString;

{$I GX_CondDefine.inc}

{
From Stefan Hoffmeister:

The Delphi 3-only expert "Send OutputDebugString to GExperts" might be
incompatible with IDE add-ins that also hook the debugger kernel. One
example of such an add-in is Numega's BoundsChecker for Delphi IDE
integration.

Symptoms of failure are that either

- the GExperts OutputDebugString expert
or
- the (commercial) IDE add-in

do not work as expected, in particular they do not show any features
that depend on the debugger kernel.

The only reliable work-around for this problem is to disable either
the "Send OutputDebugString to GExperts" expert and then restart
Delphi 3 or to remove the IDE add-in that fails.

If the (commercial) IDE add-in is working correctly, it might be
possible to get BOTH experts to work simultaneously. In order to
achieve this, make sure that the GExperts DLL is the FIRST expert DLL
to be loaded; alter the Delphi 3 "Experts" registry entries to do
this.

If the (commercial) IDE add-in, e.g. Numega's BoundsChecker for Delphi
IDE integration, does not work in this setup, then this is a
deficiency of the other add-in.
}

{
  Additional notes by Stefan Hoffmeister:

  This is a slightly bug-fixed and enhanced version
  of Erik S. Johansen's code.

  This expert will work with Delphi 3.0 (any version,
  any package - Standard/Professional/Client-Server).
  No other version of Delphi and no version of C++
  Builder is and will be supported.

  The expert works on any operating system: NT 4.0,
  Windows 98 and Windows 95.

  It still contains a problem that debugger chaining is not
  possible and that this source code will break any chain,
  most notably BoundsChecker IDE integration - but this will
  NOT be fixed in the near future.
}

(*
Hook OutputDebugString from processes debugged by Delphi.
Tested with Delphi 3.0 C/S, might work with D2 too.

When a DLL containing this unit is loaded into Delphi's address space, as a
package or as an expert, the WaitForDebugEvent call in DFWDBK32.DLL is
redirected to the WaitDbgEvent function declared here. Whenever Delphi calls
WaitForDebugEvent, the return values will be examined, and if the debug event
indicates a debug string, this string is retrieved from the debugged process
and passed to the OutStr procedure.

Currently OutStr simply allocates a console and uses WriteLn to display the
string contents. Any desired functionality could be added here, but be aware
that OutStr will never get called in the primary thread context and can't
directly pass the string to e.g. a form.

Take into consideration that DebugStrings often contain CR/LF when implementing
any display methods.

You may use this code whatever way you wish, but I'd like recognition of my name
on any products you create which incorporates the code. This code is provided
as is, with no guarantees of any kind. Use at your own risk.

March 27, 1998

Erik S. Johansen
*)

interface

uses
  Classes, GX_Experts, ExptIntf, ToolIntf, Registry;

type
  THookDebugExpert = class(TGX_Expert)
  private
    FIsHooked: Boolean;
  protected
    procedure SetActive(New: Boolean); override;
    function IconFileName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

uses
{.$IFOPT D+}
  GX_DbugIntf,
{.$ENDIF D+}
  SysUtils, Windows, Dialogs, GX_GExperts, GX_ConfigurationInfo, GX_GenFunc;

const
  IMAGE_DIRECTORY_ENTRY_EXPORT = 0; { Export Directory }
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1; { Import Directory }
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2; { Resource Directory }
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; { Exception Directory }
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4; { Security Directory }
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; { Base Relocation Table }
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6; { Debug Directory }
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7; { Description String }
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8; { Machine Value (MIPS GP) }
  IMAGE_DIRECTORY_ENTRY_TLS   = 9; { TLS Directory }
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10; { Load Configuration Directory }
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11; { Bound Import Directory in headers }
  IMAGE_DIRECTORY_ENTRY_IAT   = 12; { Import Address Table }

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;


type
  { Image format }
  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record
    e_magic: Word; // Magic number
    e_cblp: Word; // Bytes on last page of file
    e_cp: Word; // Pages in file
    e_crlc: Word; // Relocations
    e_cparhdr: Word; // Size of header in paragraphs
    e_minalloc: Word; // Minimum extra paragraphs needed
    e_maxalloc: Word; // Maximum extra paragraphs needed
    e_ss: Word; // Initial (relative) SS value
    e_sp: Word; // Initial SP value
    e_csum: Word; // Checksum
    e_ip: Word; // Initial IP value
    e_cs: Word; // Initial (relative) CS value
    e_lfarlc: Word; // File address of relocation table
    e_ovno: Word; // Overlay number
    e_res: array [ 0..3 ] of Word; // Reserved words
    e_oemid: Word; // OEM identifier (for e_oeminfo)
    e_oeminfo: Word; // OEM information; e_oemid specific
    e_res2: array [ 0..9 ] of Word; // Reserved words
    e_lfanew: Longint; // File address of new exe header
  end;


  PImageImportByName = ^TImageImportByName;
  TImageImportByName =
    packed record
    Hint: Word;
    Name: array [0..0] of Byte;
  end;

  PImageThunkData = ^TImageThunkData;
  TImageThunkData = packed record
    case Integer of
      0: (ForwarderString: PByte);
      1: (_Function: PDWORD);
      2: (Ordinal: DWORD);
      3: (AddressOfData: PImageImportByName);
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    Union: record
      case Integer of
        0: (
             Characteristics: DWORD; // 0 for terminating null import descriptor
           );
        1: (
             OriginalFirstThunk: PImageThunkData; // RVA to original unbound IAT
           );
    end;

    TimeDateStamp: DWORD; // 0 if not bound,
                          // -1 if bound, and real date\time stamp
                          //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                          // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWORD; // -1 if no forwarders
    Name: DWORD;
    FirstThunk: PImageThunkData; // RVA to IAT (if bound this IAT has actual addresses)
  end;



// Global variables

type
  TWaitForDebugEvent = function(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD): BOOL; stdcall;

var
  PreviousWaitForDebugEventCall: TWaitForDebugEvent = nil;
  OrigProc: Pointer = nil;
  pWaitDbgEventThunk: PImageThunkData = nil;
  hDebugger: Integer;
  ProcHandle: Integer = 0; // this does need to be global
  HookLoadDllEvent: Boolean = False;

procedure LoadDebugSettings;
begin
  Assert(ConfigInfo <> nil);
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    HookLoadDllEvent := ReadBool('HookDebugString', 'HookLoadDllEvent', False);
  finally
    Free;
  end;
end;

procedure OutStr(const Msg: string);
begin
  SendDebugEx(Msg, mtInformation);
end;

procedure OutException(const E: Exception);
begin
  OutStr('OutputDebugString hook:'#13+
         'Exception ' + E.ClassName + ': ' + E.Message);
end;

function WaitDbgEvent(var lpde: TDebugEvent; dwTimeout: DWORD): BOOL; stdcall;
var
  Buf: Pointer;
  BytesRead: Cardinal;
  Path: WideString;
  Filename: String;
begin
  Assert(@PreviousWaitForDebugEventCall <> nil);

  Result := PreviousWaitForDebugEventCall(lpde, dwTimeOut);
  if Result then
  begin
    try
      case lpde.dwDebugEventCode of
        CREATE_PROCESS_DEBUG_EVENT:
          begin
            LoadDebugSettings;
            ProcHandle := lpde.CreateProcessInfo.hProcess;
          end;

        EXIT_PROCESS_DEBUG_EVENT:
          ProcHandle := 0;

        LOAD_DLL_DEBUG_EVENT:
          // Workaround for access violations in bordbk40.dll when initiating
          // debugging in Delphi 4 (most likely caused by Symantec Endpoint
          // Protection software). These access violations appear to be
          // intermittent and don't always occur immediately, but as soon as they
          // start occurring they persist until the next reboot. They always
          // occur on LOAD_DLL_DEBUG_EVENT of msctf.dll, so as a workaround, we
          // process such events ourselves and prevent these events from being
          // seen by bordbk40.
          if HookLoadDllEvent and (lpde.LoadDll.hFile <> 0) then
          begin
            {$IFOPT D+}SendDebug('LOAD_DLL_DEBUG_EVENT: ' + IntToStr(lpde.LoadDll.hFile)); {$ENDIF}
            if GetPathFromHandle(lpde.LoadDll.hFile, Path) then begin
              Filename := ExtractFileName(Path);
              if CompareText(Filename, 'msctf.dll') = 0 then begin
                {$IFOPT D+}SendDebug('Closing handle to module ' + Filename); {$ENDIF}
                // Keep the event, but zero out hFile - this appears to suffice
                // to prevent the access violations.
                CloseHandle(lpde.LoadDll.hFile);
                lpde.LoadDll.hFile := 0;
              end;
            end;
          end;

        OUTPUT_DEBUG_STRING_EVENT:
          begin
            GetMem(Buf, lpde.DebugString.nDebugStringLength + 1);
            try
              FillChar(Buf^, lpde.DebugString.nDebugStringLength + 1, 0);
              if ReadProcessMemory(ProcHandle, lpde.DebugString.lpDebugStringData,
                                    Buf, lpde.DebugString.nDebugStringLength, BytesRead) then
              begin
                if lpde.DebugString.fUnicode <> 0 then
                  OutStr(WideCharToString(Buf))
                else
                  OutStr(StrPas(Buf));
              end
              else
              begin
                OutStr('OutputDebugString hook: ReadProcessMemory failed.'#13+
                       'Error ' + IntToStr(GetLastError)+': ' + SysErrorMessage(GetLastError));
              end;
            finally
              FreeMem(Buf);
            end;
          end;
      end;

    except
      on E: Exception do
      begin
        OutException(E);
        { swallow }
      end;
    end;
  end;
end;

function HookFunction(hLib: HMODULE; pFuncLibName, pFuncName: PChar;
    pNewAddr: Pointer; var pOldAddr: Pointer;
    var pThunk: PImageThunkData): Boolean;
var
  OldProt: Integer;
  pImage: PImageNtHeaders;
  pImpDesc: PImageImportDescriptor;
  pName: PChar;
  pOrigProc: Pointer;
begin
  Result := True;
  try
    pImage := PImageNtHeaders(Cardinal(PImageDosHeader(hLib)^.E_lfanew) + hLib);
    pImpDesc := Pointer(pImage.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress + hLib);
    pOrigProc := GetProcAddress(GetModuleHandle(pFuncLibName), pFuncName);
    while pImpDesc.FirstThunk <> nil do
    begin
      pName := Pointer(hLib + pImpDesc.Name);
      if CompareText(StrPas(pName), pFuncLibName) = 0 then
      begin
        pThunk := Pointer(hLib + Cardinal(pImpDesc.FirstThunk));
        while pThunk.Ordinal <> 0 do
        begin
          if pThunk.AddressOfData = pOrigProc then
          begin
            Win32Check(VirtualProtect(@pThunk.AddressOfData, SizeOf(Pointer), PAGE_EXECUTE_READWRITE, @OldProt));
            try
              pOldAddr := pThunk.AddressOfData;
              pThunk.AddressOfData := pNewAddr;
            finally
              Win32Check(VirtualProtect(@pThunk.AddressOfData, SizeOf(Pointer), OldProt, @OldProt));
            end;
            Exit;
          end;
          Inc(pThunk);
        end;
      end;
      Inc(pImpDesc);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      OutException(E);
      { swallow }
    end;
  end;
end;

procedure UnhookFunction(var pThunk: PImageThunkData; pOldAddr: Pointer);
var
  OldProt: Integer;
begin
  if pThunk <> nil then
  begin
    Win32Check(VirtualProtect(@pThunk.AddressOfData, SizeOf(Pointer), PAGE_EXECUTE_READWRITE, @OldProt));
    try
      pThunk.AddressOfData := pOldAddr;
    finally
      Win32Check(VirtualProtect(@pThunk.AddressOfData, SizeOf(Pointer), OldProt, @OldProt));
    end;
    pThunk := nil;
  end;
end;

function GetBorlandSharedFilesDir: String;
const
  BorlandSharedKey = 'Software\Borland\Borland Shared';
  SharedFilesDirValue = 'SharedFilesDir';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    // Under NT5+, we can't open HKLM in r/w mode (supported in D4+)
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFDEF GX_VER125_up}
    if Reg.OpenKeyReadOnly(BorlandSharedKey) then
    {$ELSE not GX_VER125_up}
    if Reg.OpenKey(BorlandSharedKey, False) then
    {$ENDIF not GX_VER125_up}
    begin
      Result := Reg.ReadString(SharedFilesDirValue);
      Assert(Length(Result) > 0, 'Bad shared files dir detected');
      if Result[Length(Result)] = '\' then
        Delete(Result, 1, 1)
    end
    else
      Result := '';
  finally
    Reg.Free;
  end;
end;

procedure Init;
const
  {$IFDEF GX_VER120_up}
  // added for Delphi 4
  DebuggerDllName = 'Debugger\bordbk40.dll';
  {$ELSE}
  DebuggerDllName = 'DFWDBK32.DLL';
  {$ENDIF GX_VER120_up}
  KernelName = 'KERNEL32.DLL';
var
  DebuggerDllPath: String;
begin
  IsMultiThread := True;
  {$IFDEF GX_VER120_up}
  // added for Delphi 4
  DebuggerDllPath := GetBorlandSharedFilesDir;
  if DebuggerDllPath = '' then
    Exit;
  DebuggerDllPath := DebuggerDllPath + '\' + DebuggerDllName;
  {$ELSE}
  DebuggerDllPath := DebuggerDllName;
  {$ENDIF GX_VER120_up}
  {$IFOPT D+}SendDebug('Debugger DLL Path: '+DebuggerDllPath);{$ENDIF}
  hDebugger := LoadLibrary(PChar(DebuggerDllPath));
  if hDebugger <> 0 then
  begin
    HookFunction(hDebugger, KernelName, 'WaitForDebugEvent', @WaitDbgEvent,
        @PreviousWaitForDebugEventCall, pWaitDbgEventThunk);
  end;
end;

procedure Finish;
begin
  UnhookFunction(pWaitDbgEventThunk, @PreviousWaitForDebugEventCall);
  // not really required, but it is cleaner
  if hDebugger <> 0 then
  begin
    Win32Check(FreeLibrary(hDebugger));
    hDebugger := 0;
  end;
end;


{ THookDebugExpert }

constructor THookDebugExpert.Create;
begin
  inherited Create;

  HasConfigOptions := False;
  HasMenuItem := False;
  DefaultActive := False;

  FIsHooked := False;
end;

destructor THookDebugExpert.Destroy;
begin
  try
    if Active then
      Finish;
  except
    on E: Exception do
    begin
      // nothing
    end;
  end;

  inherited Destroy;
end;

procedure THookDebuGExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);

  if New <> FIsHooked then
  begin
    if New then
      Init
    else
      Finish;

    FIsHooked := New;
  end;
end;

function THookDebugExpert.GetMenuCaption: string;
begin
  Result := ''; // this expert has no associated menu item
end;

function THookDebugExpert.GetMenuName: string;
begin
  Result := '';
end;

function THookDebugExpert.GetMenuMask: string;
begin
  Result := '';
end;

function THookDebugExpert.GetName: string;
begin
  Result := 'Hook_Debug';
end;

function THookDebugExpert.GetDisplayName: string;
resourcestring
  SDebugDisplayName = 'Send OutputDebugString To GExperts';
begin
  Result := SDebugDisplayName;
end;

procedure THookDebugExpert.Click(Sender: TObject);
begin
  { nothing }
end;

function THookDebugExpert.IconFileName: string;
begin
  Result := 'Hook';
end;

initialization
  RegisterGX_Expert(THookDebugExpert);

end.

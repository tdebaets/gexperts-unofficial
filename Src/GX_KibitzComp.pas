unit GX_KibitzComp;

{$I GX_CondDefine.inc}

interface

uses
  Classes;

procedure GetKibitzSymbols(EditControl: TObject; XPos, YPos: Integer; Strings: TStrings);

function KibitzEnabled: Boolean;

implementation

{$UNDEF GxSupportsKibitzing}

  {$IFDEF GX_Delphi}
    {$IFDEF GX_VER120_up}
      {$IFDEF GX_UseNativeToolsApi}
        {$DEFINE GxSupportsKibitzing}
      {$ENDIF GX_UseNativeToolsApi}
    {$ENDIF GX_VER120_up}
  {$ENDIF GX_Delphi}

{$IFDEF GxSupportsKibitzing}
uses
  ToolsApi, Windows, SysUtils, GX_VerDepConst;

type
  PObject = ^TObject;
  TSymbols = packed array[0..(MaxInt div SizeOf(Integer))-1] of Integer;
  PSymbols = ^TSymbols;
  TUnknowns = packed array [0..(MaxInt div SizeOf(Byte))-1] of Byte;
  PUnknowns = ^TUnknowns;
  TKibitzResult = packed array[0..81] of Integer;

const
  // dphideXX.bpl
  // Codcmplt.TCodeCompletionManger.GetKibitzInfo(XPos, YPos: Integer; var KibitzResult: TKibitzResult);
  GetKibitzInfoName = '@Codcmplt@TCodeCompletionManager@GetKibitzInfo$qqriir22Comtypes@TKibitzResult';
  // Codcmplt.CodeCompletionManager: TCodeCompletionManager;
  CodeCompletionManagerName = '@Codcmplt@CodeCompletionManager';
  // dccXX.dll
  // KibitzGetValidSymbols(var KibitzResult: TKibitzResult; Symbols: PSymbols; Unknowns: PUnknowns; SymbolCount: Integer): Integer; stdcall;
  KibitzGetValidSymbolsName = 'KibitzGetValidSymbols';
  // corideXX.bpl
  // Comdebug.CompGetSymbolText(Symbol: PSymbols; var S: string; Unknown: Word); stdcall;
  CompGetSymbolTextName = '@Comdebug@CompGetSymbolText$qqsp16Comtypes@TSymbolr17System@AnsiStringus';

type
  TGetKibitzInfoProc = procedure(Self: TObject; XPos, YPos: Integer; var KibitzResult: TKibitzResult); register;

  TKibitzGetValidSymbolsProc = function(var KibitzResult: TKibitzResult; Symbols: PSymbols;
                                         Unknowns: PUnknowns; SymbolCount: Integer): Integer; stdcall;

  TCompGetSymbolTextProc = procedure(Symbol: Integer {Comtypes::TSymbol*};
                                     var S: string; Unknown: Word); stdcall;

var
  GetKibitzInfo: TGetKibitzInfoProc;
  KibitzGetValidSymbols: TKibitzGetValidSymbolsProc;
  CompGetSymbolText: TCompGetSymbolTextProc;

  CodeCompletionManager: PObject;

// XPos starts a 0; YPos at 1.
procedure GetKibitzSymbols(EditControl: TObject; XPos, YPos: Integer; Strings: TStrings);
var
  KibitzResult: TKibitzResult;
  SymbolCount: Integer;
  Unknowns: PUnknowns;
  Symbols: PSymbols;
  I: Integer;
  S: string;
  OldEditControl: TObject;
  MagicEditControlHolder: PObject;
begin
  // Exit if kibitzing is not enabled, or the IDE is debugging.
  if (not KibitzEnabled) or
     ((BorlandIDEServices as IOTADebuggerServices).ProcessCount <> 0) then
    Exit;

  // Save the old edit control (I don't know if it is necessary but it won't do harm).
  MagicEditControlHolder := PObject(PChar(CodeCompletionManager^) + 4);

  OldEditControl := MagicEditControlHolder^;
  MagicEditControlHolder^ := EditControl;
  try
    // Get general kibitzinfo.
    GetKibitzInfo(CodeCompletionManager^, XPos, YPos, KibitzResult);
    case Byte(KibitzResult[0]) of
      $0B,
      $08,
      $09: Exit;
    else
      // Get valid symbol count.
      SymbolCount := KibitzGetValidSymbols(KibitzResult, nil, nil, MaxInt);
      // Allocate memory for symbols.
      GetMem(Symbols, SymbolCount * 4);
      try
        GetMem(Unknowns, SymbolCount);
        try
          // Get symbols.
          KibitzGetValidSymbols(KibitzResult, Symbols, Unknowns, SymbolCount);
          // Expand string list to the needed capacity
          // so that it does not have to be resized
          // while adding symbols in the loop below.
          Strings.Capacity := (Strings.Capacity - Strings.Count) + SymbolCount;

          Strings.BeginUpdate;
          try
            for I := 0 to SymbolCount-1 do
            begin
              // Get the name of the symbol.
              CompGetSymbolText(Symbols^[I], S, 2);
              // Add the retrieved string to the list.
              Strings.Add(S);
            end;
          finally
            Strings.EndUpdate;
          end;
        finally
          FreeMem(Unknowns);
        end;
      finally
        FreeMem(Symbols);
      end;
    end;

  finally
    // Restore the old edit control
    MagicEditControlHolder^ := OldEditControl;
  end;
end;

var
  PrivateKibitzEnabled: Boolean { = False };

function KibitzEnabled: Boolean;
begin
  Result := PrivateKibitzEnabled;
end;

var
  CorIdeModule: HModule;
  DphIdeModule: HModule;
  dccModule: HModule;

function Initialize: Boolean;
begin
  Result := False;

  {$IFDEF GX_UseNativeToolsApi}
  if BorlandIdeServices = nil then
    Exit;
  {$ELSE GX_UseNativeToolsApi}
  if ToolServices = nil then
    Exit;
  {$ENDIF GX_UseNativeToolsApi}

  dphideModule := LoadPackage(DphIdeLibName);
  if dphideModule = 0 then
    Exit;

  CodeCompletionManager := GetProcAddress(DphIdeModule, CodeCompletionManagerName);
  if not Assigned(CodeCompletionManager) then
    Exit;

  GetKibitzInfo := GetProcAddress(dphideModule, GetKibitzInfoName);
  if not Assigned(GetKibitzInfo) then
    Exit;

  dccModule := LoadLibrary(dccLibName);
  KibitzGetValidSymbols := GetProcAddress(dccModule, KibitzGetValidSymbolsName);
  if not Assigned(KibitzGetValidSymbols) then
    Exit;

  corideModule := LoadPackage(CorIdeLibName);
  if corideModule = 0 then
    Exit;

  CompGetSymbolText := GetProcAddress(corideModule, CompGetSymbolTextName);
  if not Assigned(CompGetSymbolText) then
    Exit;

  // If everything succeeded set KibitzEnabled to True.
  PrivateKibitzEnabled := True;
  Result := KibitzEnabled;
end;

procedure Finalize;
begin
  if CorIdeModule <> 0 then
  begin
    UnLoadPackage(CorIdeModule);
    CorIdeModule := 0;
  end;

  if dccModule <> 0 then
  begin
    FreeLibrary(dccModule);
    dccModule := 0;
  end;

  if DphIdeModule <> 0 then
  begin
    UnLoadPackage(DphIdeModule);
    dphideModule := 0;
  end;
end;

initialization

  if not Initialize then
    Finalize;

finalization

  Finalize;

{$ELSE GxSupportsKibitzing} // C++Builder or Delphi 3.
procedure GetKibitzSymbols(EditControl: TObject; XPos, YPos: Integer; Strings: TStrings);
begin
  // Empty; we do not support neither Delphi 3 or earlier nor C++Builder.
end;

function KibitzEnabled: Boolean;
begin
  Result := False;
end;

{$ENDIF GxSupportsKibitzing}

end.

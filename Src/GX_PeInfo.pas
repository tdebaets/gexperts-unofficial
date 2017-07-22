unit GX_PeInfo;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{ TODO -oAnyone -cCleanup : PE Header characteristics should be shown without leading FFFF? }
{ TODO -oAnyone -cCleanup : Header records should use Word, etc instead of Smallint, etc. }

uses
  Classes, Windows, SysUtils;

type
  DosHeader = record
    e_magic: SmallInt; // Magic number
    e_cblp: SmallInt; // Bytes on last page of file
    e_cp: SmallInt; // Pages in file
    e_crcl: SmallInt; // Relocations
    e_cparhdr: SmallInt; // Size of header in paragraphs
    e_minalloc: SmallInt; // Minimum extra paragraphs needed
    e_maxalloc: SmallInt; // Maximum extra paragraphs needed
    e_ss: SmallInt; // Initial (relative) SS value
    e_sp: SmallInt; // Initial SP value
    e_csum: SmallInt; // Checksum
    e_ip: SmallInt; // Initial IP value
    e_cs: SmallInt; // Initial (relative) CS value
    e_lfarclc: SmallInt; // File address of relocation table
    e_ovno: SmallInt; // Overlay number
    e_res: array[1..4] of SmallInt; // Reserved words
    e_oemid: SmallInt; // OEM identifier (for e_oeminfo)
    e_oeminfo: SmallInt; // OEM information; e_oemid specific
    e_res2: array[1..10] of SmallInt; // Reserved words
    e_lfanew: Integer; // File address of new exe header
  end;

  PEImgHeader = record
    Machine: SmallInt;
    NumberofSections: SmallInt;
    TimeDateStamp: Integer;
    PointerToSymboltable: Integer;
    NumberofSymbols: Integer;
    SizeOfOptionalHeader: SmallInt;
    Characteristics: SmallInt;
  end;

  Image_Data_Directory = record
    VirtualAddress: Integer;
    Size: Integer;
  end;

  Thunk_Data = record
    AddressOfData: Integer;
  end;

  Import_Function = record
    Ordinal: SmallInt;
    Name: array[0..255] of Char;
  end;

  PEOptionalHeader = record
    Magic: SmallInt;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Integer;
    SizeofInitializedData: Integer;
    SizeofUninitializedData: Integer;
    AddressofEntryPoint: Integer;
    BaseofCode: Integer;
    BaseofData: Integer;
    ImageBase: Integer;
    SectionAlignment: Integer;
    FileAlignment: Integer;
    MajorOperatingSystemVersion: SmallInt;
    MinorOperatingSystemVersion: SmallInt;
    MajorImageVersion: SmallInt;
    MinorImageVersion: SmallInt;
    MajorSubsystemVersion: SmallInt;
    MinorSubsystemVersion: SmallInt;
    Win32Version: Integer;
    SizeofImage: Integer;
    SizeofHeaders: Integer;
    CheckSum: Integer;
    Subsystem: SmallInt;
    DLLCharacteristics: SmallInt;
    SizeofStackReserve: Integer;
    SizeofStackCommit: Integer;
    SizeofHeapReserve: Integer;
    SizeofHeapCommit: Integer;
    LoaderFlags: Integer;
    NumberofRVAandSizes: Integer;
    DataDirectories: array[0..15] of Image_Data_Directory;
  end;

  ImageSectionHeader = record
    Name: array[0..7] of Char;
    VirtualSize: Integer;
    VirtualAddress: Integer;
    SizeofRawData: Integer;
    PointerToRawData: Integer;
    PointerToRelocations: Integer;
    PointerToLineNumbers: Integer;
    NumberofRelocations: SmallInt;
    NumberofLineNumbers: SmallInt;
    Characteristics: Integer;
  end;

  PEImportDescriptors = record
    Characteristics: Integer;
    TimeDateStamp: Integer;
    ForwarderChain: Integer;
    Name: Integer;
    FirstThunk: Integer;
  end;

  PEExportImage = record
    Characteristics: Integer;
    TimeDateStamp: Integer;
    MajorVersion: SmallInt;
    MinorVersion: SmallInt;
    Name: Integer;
    Base: Integer;
    NumberofFunctions: Integer;
    NumberofNames: Integer;
    AddressOfFunctions: Integer;
    AddressofNames: Integer;
    AddressofNameOrdinals: Integer;
  end;

  TImportExportItem = class(TCollectionItem)
  private
    FFunctionName: string;
    FOrdinal: Integer;
  public
    property FunctionName: string read FFunctionName write FFunctionName;
    property Ordinal: Integer read FOrdinal write FOrdinal;
  end;

  TImportExport = class(TCollection)
  private
    FPEName: string;
    function GetItem(Index: Integer): TImportExportItem;
    procedure SetItem(Index: Integer; Value: TImportExportItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TImportExportItem;
    property PEName: string read FPEName write FPEName;
    property Items[Index: Integer]: TImportExportItem read GetItem write SetItem; default;
  end;

  TNumberType = (ntDecimal, ntHex);

  TPEFileInfo = class(TObject)
  private
    FBase: Integer;
    FStream: TFileStream;
    FFileName: string;
    FIsPE: Boolean;
    FIsMSDos: Boolean;
    FImportList: TStringList;
    FExportList: TStringList;
    FMSDOSHeader: TStringList;
    FPEHeaderList: TStringList;
    FPEOptionalHeaderList: TStringList;
    FSectionStart: Integer;
    FDosHeader: DosHeader;
    FPEHeader: PEImgHeader;
    FPEOptionalHeader: PEOptionalHeader;
    FNumberType: TNumberType;
    procedure ReadPEFileFormat;
    procedure ReadImportList;
    function GetEnclosingSectionHeader(rva: Integer; var SectionHeader: ImageSectionHeader): Boolean;
    procedure GetImportFunctions(Import: PEImportDescriptors; Delta: Integer; ImpExp: TImportExport);
    procedure ReadExportList;
    function GetCPUType: string;
    procedure FillMSDOSHeader;
    procedure FillPEOptionalHeader;
    procedure FillPEHeader;
    function GetExportList: TStrings;
    function GetImportList: TStrings;
    function GetMSDOSHeader: TStrings;
    function GetPEHeaderList: TStrings;
    function GetPEOptionalHeaderList: TStrings;
  protected
  public
    function IntToNum(n: Integer): string;
    constructor Create(FName: string; NType: TNumberType);
    destructor Destroy; override;
  published
    property FileName: string read FFileName;
    property CPUType: string read GetCPUType;
    property NumberType: TNumberType read FNumberType write FNumberType;
    property IsPE: Boolean read FIsPE;
    property IsMSDos: Boolean read FIsMSDos;
    property ImportList: TStrings read GetImportList;
    property ExportList: TStrings read GetExportList;
    property MSDOSHeader: TStrings read GetMSDOSHeader;
    property PEHeaderList: TStrings read GetPEHeaderList;
    property PEOptionalHeaderList: TStrings read GetPEOptionalHeaderList;
  end;

const
  IMAGE_DIRECTORY_ENTRY_EXPORT = 0;
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1; // Import Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2; // Resource Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; // Exception Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4; // Security Directory
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; // Base Relocation Table
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6; // Debug Directory
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7; // Description String
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8; // Machine Value (MIPS GP)
  IMAGE_DIRECTORY_ENTRY_TLS = 9; // TLS Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10; // Load Configuration Directory

implementation

function TImportExport.GetItem(Index: Integer): TImportExportItem;
begin
  Result := TImportExportItem(inherited GetItem(Index));
end;

procedure TImportExport.SetItem(Index: Integer; Value: TImportExportItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TImportExport.Update(Item: TCollectionItem);
begin
   {Nothing for now}
end;

function TImportExport.Add: TImportExportItem;
begin
  Result := TImportExportItem(inherited Add);
end;

constructor TPEFileInfo.Create(FName: string; NType: TNumberType);
begin
  NumberType := NType;
  FImportList := TStringList.Create;
  FExportList := TStringList.Create;
  FMSDOSHeader := TStringList.Create;
  FPEHEaderList := TStringList.Create;
  FPEOptionalHeaderList := TStringList.Create;

  FFileName := FName;
  FIsPE := False;
  FIsMSDos := False;
  ReadPEFileFormat;
end;

destructor TPEFileInfo.Destroy;
var
  i: Integer;
begin
  FMSDOSHeader.Free;
  FMSDOSHeader := nil;

  FPEHeaderList.Free;
  FPEHeaderList := nil;

  FPEOptionalHeaderList.Free;
  FPEOptionalHeaderList := nil;

  FExportList.Free;
  FExportList := nil;

  for i := 0 to FImportList.Count-1 do
    TImportExport(FImportList.Objects[i]).Free;

  FImportList.Free;
  FImportList := nil;

  inherited Destroy;
end;

function TPEFileInfo.IntToNum(n: Integer): string;

  function IntToHex(IntVal: Integer): AnsiString;  // added Stefan
  const
    HexTransform: array[$0..$F] of Char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    Width = 16;
  var
    IntBytes: array[0..SizeOf(Integer)-1] of Byte absolute IntVal;
    I: Integer;
    SIndex: Integer;
  begin
    SetLength(Result, Width);
    FillChar(PChar(Result)^, Width, '0');
    SIndex := Length(Result);
    for i := 0 to SizeOf(Integer)-1 do
    begin
      Result[SIndex] := HexTransform[IntBytes[i] and $0F];
      Dec(SIndex);
      Result[SIndex] := HexTransform[(IntBytes[i] shr 4) and $0F];
      Dec(SIndex);
    end;
  end;

const // Duplicated here for use outside of GExperts
  GXHexPrefix = {$IFDEF GX_BCB}'0x'{$ELSE}'$'{$ENDIF};
begin
  try
    case FNumberType of
      ntDecimal: Result := IntToStr(n);
      {$IFDEF LEADING0}
      ntHex: Result := IntToHex(n);
      {$ELSE}
      ntHex: Result := Format(GXHexPrefix+'%x', [n]);
      {$ENDIF LEADING0}
    end;
  except
    on E: EConvertError do
      Result := '';
  end;
end;

procedure TPEFileInfo.ReadPEFileFormat;
resourcestring
  SNotPeFile = 'Not a PE File';
var
  PE: array[0..3] of Char;
begin
  FStream := TFileStream.Create(Filename, fmOpenRead + fmShareDenyNone);
  try
    FStream.Read(FDosHeader, 64);

    FIsMSDos := (FDosHeader.e_magic = $5A4D);

    FStream.Position := FDosHeader.e_lfanew;
    FStream.Read(PE, 4);
    if (PE[0] = 'P') and (PE[1] = 'E') and (PE[2] = #0) and (PE[3] = #0) then
      FIsPE := True;
    FillMSDosHeader;
    if not FIsPE then
    begin
      FPEHEaderList.Add(SNotPeFile);
      Exit;
    end;
    FStream.Read(FPEHeader, 20);
    FStream.Read(FPEOptionalHeader, SizeOf(FPEOptionalHeader));
    FBase := FStream.Position;
    FSectionStart := FStream.Position;
    ReadImportList;
    ReadExportList;
    FillPEOptionalHeader;
    FillPEHeader;
  finally
    FStream.Free;
    FStream := nil;
  end;
end;

procedure TPEFileInfo.ReadImportList;

  function GetName(L: Integer): string;
  var
    SPos: Integer;
    Buf: array[0..1024] of Char;
  begin
    SPos := FStream.Position;
    FStream.Position := L;
    FStream.Read(Buf, 1024);
    Result := StrPas(Buf);
    FStream.Position := SPos;
  end;

var
  Import: PEImportDescriptors;
  delta, p: Integer;
  SectionHeader: ImageSectionHeader;
  Name: string;
  ImpExp: TImportExport;
begin
  FImportList.Sorted := False;
  FImportList.Clear;
  if not GetEnclosingSectionHeader(FPEOptionalHeader.DataDirectories[1].VirtualAddress, SectionHeader) then
    exit;
  delta := SectionHeader.VirtualAddress - SectionHeader.PointerToRawData;
  FStream.Position := FPEOptionalHeader.DataDirectories[1].VirtualAddress - delta;
  FStream.Read(Import, Sizeof(Import));
  while (Import.Name <> 0) and (FStream.Position < FStream.Size) do
  begin
    Name := GetName(Import.Name - delta);
    if FImportList.Indexof(Name) < 0 then
    begin
      ImpExp := TImportExport.Create(TImportExportItem);
      ImpExp.PEName := Name;
      FImportList.AddObject(Name, ImpExp);
    end;
    p := FImportList.Indexof(Name);
    GetImportFunctions(Import, Delta, TImportExport(FImportList.Objects[p]));
    FStream.Read(Import, Sizeof(Import));
  end;
  FImportList.Sorted := True;
end;

procedure TPEFileInfo.GetImportFunctions(Import: PEImportDescriptors; Delta: Integer; ImpExp: TImportExport);
var
  Thunk: Integer;
  SPos, p: Integer;
  ThunkData: Thunk_Data;
  ImpF: Import_Function;
begin
  SPos := FStream.Position;
  try
    if Import.Characteristics = 0 then
      Thunk := Import.FirstThunk - delta
    else
      Thunk := Import.Characteristics - delta;
    FStream.Position := Thunk;
    FStream.Read(ThunkData, Sizeof(ThunkData));
    while (FStream.Position < FStream.Size) and (ThunkData.AddressofData <> 0) do
    begin
      p := FStream.Position;
      if ThunkData.AddressofData < 0 then
      begin {Imported by Ordinal}
        FStream.Position := Thunk;
        with ImpExp.Add do
        begin
          Ordinal := Word(ThunkData.AddressofData);
          FunctionName := '';
        end;
      end
      else
      begin
        FStream.Position := ThunkData.AddressofData - delta;
        FStream.Read(ImpF, Sizeof(ImpF));
        with ImpExp.Add do
        begin
          Ordinal := ImpF.Ordinal;
          FunctionName := StrPas(ImpF.Name);
        end;
      end;
      FStream.Position := p;
      FStream.Read(ThunkData, SizeOf(ThunkData));
    end;
  finally
    FStream.Position := SPos;
  end;
end;

function TPEFileInfo.GetEnclosingSectionHeader(rva: Integer; var SectionHeader: ImageSectionHeader): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FPEHeader.NumberOfSections-1 do  // fix Stefan
  begin
    FStream.Position := FSectionStart + Sizeof(SectionHeader) * i;
    FStream.Read(SectionHeader, Sizeof(SectionHeader));
    if (rva >= SectionHeader.VirtualAddress) and
       (rva < SectionHeader.VirtualAddress + SectionHeader.VirtualSize) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TPEFileInfo.ReadExportList;  // enhanced Stefan
var
  SectionHeader: ImageSectionHeader;
  ExportAddress: Integer;
  NameAddress: Integer;
  Buffer: array[0..255] of Char;
  j: Integer;
  MaxExports: Longint;
  Ordinal: Word;
  ExportVA: DWORD;
  ExportInfo: PEExportImage;
  Delta: Integer;
begin
  // Utils.StartPerformanceCount; // StH
  FExportList.Clear;
  with FPEOptionalHeader.DataDirectories[0] do
    ExportVA := VirtualAddress;
  if not GetEnclosingSectionHeader(ExportVA, SectionHeader) then
    Exit;
  Delta := SectionHeader.VirtualAddress - SectionHeader.PointerToRawData;
  FStream.Position := Longint(ExportVA) - Delta;
  FStream.Read(ExportInfo, SizeOf(ExportInfo));
  if ExportInfo.Characteristics <> 0 then
    Exit;
  FStream.Position := Longint(ExportInfo.Name) - Delta;
  with ExportInfo do
  begin
    MaxExports := NumberOfFunctions;
    if NumberOfNames < MaxExports then
      MaxExports := NumberOfNames;
  end;
  FExportList.Sorted := False;
  try
    FExportList.Capacity := MaxExports;
    FStream.Position := LongInt(ExportInfo.AddressOfNameOrdinals) - Delta;
    for j := 0 to MaxExports-1 do
    begin
      FStream.ReadBuffer(Ordinal, SizeOf(Ordinal));
      FExportList.Add(#9 + IntToNum(Ordinal + ExportInfo.Base) + #9);
    end;
    // Now read the names themselves
    for j := 0 to MaxExports-1 do
    begin
      FStream.Position := (LongInt(ExportInfo.AddressOfNames) - Delta)
                          + (j * SizeOf(NameAddress));
      FStream.ReadBuffer(NameAddress, SizeOf(NameAddress));
      FStream.Position := NameAddress - Delta;
      FStream.Read(Buffer, SizeOf(Buffer));
      FExportList[j] := StrPas(Buffer) + FExportList[j];
    end;
    // And finally read the function associated with each export
    FStream.Position := LongInt(ExportInfo.AddressOfFunctions) - Delta;
    for j := 0 to MaxExports-1 do
    begin
      FStream.ReadBuffer(ExportAddress, SizeOf(ExportAddress));
      FExportList[j] := FExportList[j] + IntToNum(ExportAddress);
    end;
  finally
    FExportList.Sorted := True;
  end;
  //ShowMessage(IntToStr(Utils.LastPerformanceCount)); // StH
end;

function TPEFileInfo.GetCPUType: string;
resourcestring
  SIntel_80386 = 'Intel 80386';
  SIntel_80486 = 'Intel 80486';
  SIntel_Pentium = 'Intel Pentium';
  SMIPS_R3000_BigEndian = 'MIPS R3000 (Big Endian)?';
  SMIPS_Mark_I = 'MIPS Mark I (R2000, R3000)';
  SMIPS_Mark_II = 'MIPS Mark II (R6000)';
  SMIPS_Mark_III = 'MIPS Mark III (R4000)';
  SMIPS_R10000 = 'MIPS R10000';
  SDEC_Alpha_XP = 'DEC Alpha XP';
  SPower_PC = 'Power PC';
  SMotorola_68000 = 'Motorola 68000';
  SPA_RISC = 'PA RISC';
  SUnknown_CPU = 'Unknown CPU';
begin
  if not isPE then
  begin
    Result := '';
    Exit;
  end;
  case FPEHeader.Machine of
    $14C: Result := SIntel_80386;
    $14D: Result := SIntel_80486;
    $14E: Result := SIntel_Pentium;
    $160: Result := SMIPS_R3000_BigEndian;
    $162: Result := SMIPS_Mark_I;
    $163: Result := SMIPS_Mark_II;
    $166: Result := SMIPS_Mark_III;
    $168: Result := SMIPS_R10000;
    $184: Result := SDEC_Alpha_XP;
    $1F0: Result := SPower_PC;
    $268: Result := SMotorola_68000;
    $290: Result := SPA_RISC;
  else
    Result := SUnknown_CPU;
  end;
end;

procedure TPEFileInfo.FillMSDOSHeader;
resourcestring
  SMagicNumber = 'Magic number';
  SBytesOnLastPage = 'Bytes on last page of file';
  SPagesInFile = 'Pages in file';
  SRelocations = 'Relocations';
  SSizeOfHeaderInParagraphs = 'Size of header in paragraphs';
  SMinimumExtraParagraphs = 'Minimum extra paragraphs';
  SMaximumExtraParagraphs = 'Maximum extra paragraphs';
  SInitialSS = 'Initial (relative) SS value';
  SInitialSP = 'Initial SP value';
  SChecksum = 'Checksum';
  SInitialIP = 'Initial IP value';
  SInitialCS = 'Initial (relative) CS value';
  SFileAddressRelocation = 'File address of relocation table';
  SOverlayNumber = 'Overlay number';
  // SReservedWords = 'Reserved words';
  SOemIdentifier = 'OEM identifier';
  SOemInformation = 'OEM information';
  SPeHeaderAddress = 'PE header address';
begin
  FMSDOSHeader.Clear;
  with FMSDOSHeader, FDOSHeader do
  begin
    Add(SMagicNumber + #9 + IntToNum(e_magic));
    Add(SBytesOnLastPage + #9 + IntToNum(e_cblp));
    Add(SPagesInFile + #9 + IntToNum(e_cp));
    Add(SRelocations + #9 + IntToNum(e_crcl));
    Add(SSizeOfHeaderInParagraphs + #9 + IntToNum(e_cparhdr));
    Add(SMinimumExtraParagraphs + #9 + IntToNum(e_minAlloc));
    Add(SMaximumExtraParagraphs + #9 + IntToNum(e_maxAlloc));
    Add(SInitialSS + #9 + IntToNum(e_ss));
    Add(SInitialSP + #9 + IntToNum(e_sp));
    Add(SChecksum + #9 + IntToNum(e_csum));
    Add(SInitialIP + #9 + IntToNum(e_ip));
    Add(SInitialCS + #9 + IntToNum(e_cs));
    Add(SFileAddressRelocation + #9 + IntToNum(e_lfarclc));
    Add(SOverlayNumber + #9 + IntToNum(e_ovno));
    // Add(SReserved words #9 + IntToNum(e_res));
    Add(SOemIdentifier + #9 + IntToNum(e_oemid));
    Add(SOemInformation + #9 + IntToNum(e_oemInfo));
    Add(SPeHeaderAddress + #9 + IntToNum(e_lfanew));
  end;
end;

procedure TPEFileInfo.FillPEHeader;
resourcestring
  SMachine = 'Machine';
  SNumberSections = 'Number of sections';
  STimeDate = 'Time/Date stamp';
  SAddressSymbolTable = 'Address of symbol table';
  SNumberSymbols = 'Number of symbols';
  SSizeOptionalHeader = 'Size of optional header';
  SCharacteristics = 'Characteristics';
begin
  FPEHeaderList.Clear;
  with FPEHeaderList, FPEHEader do
  begin
    Add(SMachine + #9 + CPUType);
    Add(SNumberSections + #9 + IntToNum(NumberofSections));
    Add(STimeDate + #9 + IntToNum(TimeDateStamp));
    Add(SAddressSymbolTable + #9 + IntToNum(PointerToSymboltable));
    Add(SNumberSymbols + #9 + IntToNum(NumberofSymbols));
    Add(SSizeOptionalHeader + #9 + IntToNum(SizeOfOptionalHeader));
    Add(SCharacteristics + #9 + IntToNum(Characteristics));
  end;
end;

procedure TPEFileInfo.FillPEOptionalHeader;
resourcestring
  SMagic = 'Magic';
  SMajorLinker = 'Major linker version';
  SMinorLinker = 'Minor linker version';
  SSizeCode = 'Size of Code';
  SInitializedData = 'Size of initialized data';
  SUninitializedData = 'Size of uninitialized data';
  SAddressEntryPoint = 'Address of entry point';
  SBaseCode = 'Base of code';
  SBaseData = 'Base of data';
  SImageBase = 'Image base';
  SSectionAlignment = 'Section alignment';
  SFileAlignment = 'File alignment';
  SMajorOs = 'Major OS version';
  SMinorOs = 'Minor OS version';
  SMajorImage = 'Major image version';
  SMinorImage = 'Minor image version';
  SMajorSubsystem = 'Major subsystem version';
  SMinorSubsystem = 'Minor subsystem version';
  SWin32Version = 'Win32 Version';
  SSizeImage = 'Size of image';
  SSizeHeaders = 'Size of headers';
  SCrc = 'CRC Checksum';

  SSub = 'Subsystem';
  SSubNative = 'Native, no subsystem required';
  SSubWinGui = 'Windows GUI subsystem required';
  SSubWinConsole = 'Windows console subsystem required';
  SSubOs2Console = 'OS/2 console subsystem required';
  SSubPosix = 'POSIX console subsystem required';

  SDll = 'DLL Characteristics';
  SDllPerProcessInit = 'Per-process library initialization';
  SDllPerProcessTermination = 'Per-process library termination';
  SDllPerThreadInit = 'Per-thread library initialization';
  SDllPerThreadTermination = 'Per-thread library termination';

  SStackReserve = 'Size of stack reserve';
  SStackCommit = 'Size of stack commit';
  SHeapReserve = 'Size of heap reserve';
  SHeapCommit = 'Size of heap commit';

  SLoaderFlags = 'Loader Flags';
  SLoaderFlagsBP = 'Invoke a breakpoint before starting process';
  SLoaderFlagsDBG = 'Invoke a debugger after process has been loaded';
  SLoaderFlagsNone = 'No flags set';

  SNumDataEntries = 'Number of entries in data directory';

begin
  FPEOptionalHEaderList.Clear;
  with FPEOptionalHeaderList, FPEOptionalHeader do
  begin
    Add(SMagic + #9 + IntToNum(Magic));
    Add(SMajorLinker + #9 + IntToNum(MajorLinkerVersion));
    Add(SMinorLinker + #9 + IntToNum(MinorLinkerVersion));
    Add(SSizeCode + #9 + IntToNum(SizeofCode));
    Add(SInitializedData + #9 + IntToNum(SizeofInitializedData));
    Add(SUninitializedData + #9 + IntToNum(SizeofUninitializedData));
    Add(SAddressEntryPoint + #9 + IntToNum(AddressofEntryPoint));
    Add(SBaseCode + #9 + IntToNum(BaseofCode));
    Add(SBaseData + #9 + IntToNum(baseofdata));
    Add(SImageBase + #9 + IntToNum(imagebase));
    Add(SSectionAlignment + #9 + IntToNum(sectionalignment));
    Add(SFileAlignment + #9 + IntToNum(filealignment));
    Add(sMajorOS + #9 + IntToNum(MajorOperatingSystemVersion));
    Add(SMinorOS + #9 + IntToNum(MinorOperatingSystemVersion));
    Add(SMajorImage + #9 + IntToNum(MajorImageVersion));
    Add(SMinorImage + #9 + IntToNum(MinorImageVersion));
    Add(SMajorSubsystem + #9 + IntToNum(MajorSubsystemVersion));
    Add(SMinorSubsystem + #9 + IntToNum(MinorSubsystemVersion));
    Add(SWin32Version + #9 + IntToNum(Win32Version));
    Add(SSizeImage + #9 + IntToNum(SizeofImage));
    Add(SSizeHeaders + #9 + IntToNum(SizeofHeaders));
    Add(SCRC + #9 + IntToNum(Checksum));
    case Subsystem of
      1: Add(SSub + #9 + SSubNative);
      2: Add(SSub + #9 + SSubWinGUI);
      3: Add(SSub + #9 + SSubWinConsole);
      5: Add(SSub + #9 + SSubOS2Console);
      7: Add(SSub + #9 + SSubPOSIX);
    end;
    if ((DLLCharacteristics and $0001) > 0) then
      Add(SDll + #9 + SDllPerProcessInit);
    if ((DLLCharacteristics and $0002) > 0) then
      Add(SDll + #9 + SDllPerProcessTermination);
    if ((DLLCharacteristics and $0004) > 0) then
      Add(SDll + #9 + SDllPerThreadInit);
    if ((DLLCharacteristics and $0008) > 0) then
      Add(SDll + #9 + SDllPerThreadTermination);
    Add(SStackReserve + #9 + IntToNum(SizeofStackReserve));
    Add(SStackCommit + #9 + IntToNum(SizeofStackCommit));
    Add(SHeapReserve + #9 + IntToNum(SizeofHeapReserve));
    Add(SHeapCommit + #9 + IntToNum(SizeofHeapCommit));
    case LoaderFlags of
      1: Add(SLoaderFlags + #9 + SLoaderFlagsBP);
      2: Add(SLoaderFlags + #9 + SLoaderFlagsDBG);
    else
      Add(SLoaderFlags + #9 + SLoaderFlagsNone);
    end;
    Add(SNumDataEntries + #9 + IntToNum(NumberofRVAandSizes));
  end;
end;

function TPEFileInfo.GetExportList: TStrings;
begin
  Result := FExportList;
end;

function TPEFileInfo.GetImportList: TStrings;
begin
  Result := FImportList;
end;

function TPEFileInfo.GetMSDOSHeader: TStrings;
begin
  Result := FMSDOSHeader;
end;

function TPEFileInfo.GetPEHeaderList: TStrings;
begin
  Result := FPEHEaderList;
end;

function TPEFileInfo.GetPEOptionalHeaderList: TStrings;
begin
  Result := FPEOptionalHeaderList;
end;

end.


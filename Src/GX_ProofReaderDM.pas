{ Original Author: Alex Petrov - Alex@krona.obninsk.ru }
{ Modified 30 January 1999 by Francois Sorrentino for localized keyboard support }

unit GX_ProofReaderDM;

{$I GX_CondDefine.inc}

{$IFNDEF GX_NOBDE}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  GX_BaseModuleNotifiers,
  Windows, Classes, Forms,
  {$IFDEF GX_VER140_up}
  Variants,
  {$ENDIF GX_VER140_up}
  DBTables, DB;

const
  wrAnywhere  = 0;
  wrWordBegin = 1;
  wrWordEnd   = 2;
  wrWholeWord = 3;

resourcestring
  swrAnywhere  = 'Anywhere';
  swrWordBegin = 'At the begining of a word';
  swrWordEnd   = 'At the end of a word';
  swrWholeWord = 'Whole word only';

type
  TReplacementSource = (rtPasSrc, rtCPPSrc, rtPreproc, rtSQLSrc, rtAssembler, rtString, rtComment);

const
  ReplacementSourceText: array[TReplacementSource] of string = (
    'Pascal source code',
    'C++ source code',
    'C pre-processor',
    'SQL source code',
    'Inline assembler',
    'Strings',
    'Comments'
  );


type
  TCorrectionKind = (ckAutoCorrection, ckWord);

  TCorrectionItem = class(TObject)
  public
    CorrectionKind: TCorrectionKind;
    SourceLanguage: TReplacementSource;
    OriginalText: string;
    InfoString: string;
    Time: TDateTime;
  end;

  TCorrectionHistory = class(TObject)
  private
    Items: TList;
    MaxHistory: Integer;
    function GetCount: Integer;
  public
    property Count: Integer read GetCount;
    function GetItem(Index: Integer): TCorrectionItem;
    procedure Add(Item: TCorrectionItem);
    constructor Create;
    destructor Destroy; override;
  end;

  TReplacementItem = class(TObject)
  private
    Typed: string;
    Where: Shortint;
    Replace: string;
  public
    constructor Create(const TypedString: string; ReplaceWhere: ShortInt; const ReplaceWithString: string);
  end;

  TdmProofReader = class(TDataModule)
    Database: TDatabase;
    Session: TSession;
    tReplacement: TTable;
    tReplacementLanguage: TSmallintField;
    tReplacementTyped: TStringField;
    tReplacementWhereReplace: TSmallintField;
    dsReplacement: TDataSource;
    tDictionary: TTable;
    tDictionaryLanguage: TSmallintField;
    tDictionaryWord: TStringField;
    dsDictionary: TDataSource;
    tReplacementReplace: TStringField;
    procedure tReplacementTypedSetText(Sender: TField; const Text: string);
    procedure tReplacementWhereReplaceGetText(Sender: TField;
      var Text: string; DisplayText: Boolean);
    procedure tReplacementWhereReplaceSetText(Sender: TField; const Text: string);
    procedure tReplacementNewRecord(DataSet: TDataSet);
    procedure tReplacementBeforePost(DataSet: TDataSet);
  private
    FIsInstalled: Boolean;
    FModuleChangeNotifiers: TBaseProjectNotifier;
    // Option settings for the proofreader
    FReplacerActive: Boolean;
    FDictionaryActive: Boolean;
    FCompilerActive: Boolean;
    FDictionaryCaseDiffer: Boolean;
    FOtherLetter: Boolean;
    FNearbyLetter: Boolean;
    FNoLetter: Boolean;
    FMoreLetter: Boolean;
    FMixedLetter: Boolean;
    FNoFirstOther: Boolean;
    FBeepOnReplace: Boolean;
    //
    slReplace: array[TReplacementSource] of TStringList;
    slDictionary: array[TReplacementSource] of TStringList;
    slKibitz: TStringList;
    procedure ReverseString(var s: string);
    function IsWord(const s: string): Boolean;
    procedure LoadReplacement;
    procedure LoadDictionary;
    function FindReplacement(Zone: TReplacementSource; TypedString: string): TReplacementItem;
    function FindStringList(StringList: TStringList; SortedList: Boolean; const Word: string): string;
    function FindDictionary(Zone: TReplacementSource; const Word: string): string;
  private
    FHistory: TCorrectionHistory;
    FDefaultLanguage: TReplacementSource;
  public
    property History: TCorrectionHistory read FHistory write FHistory;
    property DefaultLanguage: TReplacementSource read FDefaultLanguage write FDefaultLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure Initialize;
    procedure Finalize;
    //
    procedure Reload;
    //
    procedure SaveSettings;
    procedure LoadSettings;
    property ReplacerActive: Boolean read FReplacerActive write FReplacerActive;
    property DictionaryActive: Boolean read FDictionaryActive write FDictionaryActive;
    property CompilerActive: Boolean read FCompilerActive write FCompilerActive;
    property DictionaryCaseDiffer: Boolean read FDictionaryCaseDiffer write FDictionaryCaseDiffer;
    property OtherLetter: Boolean read FOtherLetter write FOtherLetter;
    property NearbyLetter: Boolean read FNearbyLetter write FNearbyLetter;
    property NoLetter: Boolean read FNoLetter write FNoLetter;
    property MoreLetter: Boolean read FMoreLetter write FMoreLetter;
    property MixedLetter: Boolean read FMixedLetter write FMixedLetter;
    property NoFirstOther: Boolean read FNoFirstOther write FNoFirstOther;
    property BeepOnReplace: Boolean read FBeepOnReplace write FBeepOnReplace;
    property IsInstalled: Boolean read FIsInstalled;
  end;

implementation

uses
  SysUtils, Registry, Dialogs,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  ToolIntf, ExptIntf, EditIntf,
  GX_GenFunc, GX_GExperts, GX_ConfigurationInfo, GX_KibitzComp;

{$R *.DFM}

{$IFDEF VER110}
// C++Builder 3.0 does not "like" initialised
// sets above a given size, so we declare
// variables and initialise them in the
// initialization section of this unit.
var
  shsSource: set of Byte;
  AlphaNum: set of Char;
{$ELSE}
const
  shsSource = [atWhiteSpace, atReservedWord, atIdentifier, atSymbol, atNumber,
    atFloat, atOctal, atHex, atCharacter, atIllegal, SyntaxOff, 37]; //37??
  AlphaNum = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
{$ENDIF VER110}



// This method allows for case sensitive sorting;
// the existing routine in CLASSES.PAS is case-INsensitive
procedure SortStringList(AStringList: TStringList);

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, S: string;
  begin
    repeat
      I := L;
      J := R;
      P := AStringList.Strings[(L + R) shr 1];
      repeat
        while AnsiCompareStr(AStringList.Strings[I], P) < 0 do Inc(I);
        while AnsiCompareStr(AStringList.Strings[J], P) > 0 do Dec(J);
        if I <= J then
        begin
          S := AStringList.Strings[J];
          AStringList.Strings[J] := AStringList.Strings[I];
          AStringList.Strings[I] := S;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if AStringList.Count > 1 then
    QuickSort(0, AStringList.Count - 1);
end;

type
  TAutoTypeWriterNotifier = class(TBaseModuleNotifier)
  private
    // When the proofreader corrects text it will
    // cause another editor notification message
    // to be sent. This might result in re-entrancy.
    // The FModifyingSelf Boolean flag prevents the
    // re-entrancy problem by exiting if True.
    FModifyingSelf: Boolean;
    //
    FIsKnownSourceCode: Boolean;
    //
    FPreviousEditorPosition: TEditPos;
    procedure AppendHistory(CorrectionKind: TCorrectionKind; SourceLanguage: TReplacementSource;
                            const InfoString, OriginalText: string);
    procedure AppendAutoCorrectHistory(SourceLanguage: TReplacementSource;
                                       const FromString, ToString: string);
    procedure AppendWordHistory(SourceLanguage: TReplacementSource;
                                const FromString, ToString: string);
    procedure AppendKibitzHistory(SourceLanguage: TReplacementSource;
                                  const FromString, ToString: string);
    procedure BeepOnDemand;
    function GetReplacementSource(Element: Integer; var Source: TReplacementSource): Boolean;
    function HaveValidPreviousPos: Boolean;
  private
    FSyntaxHighlighter: TSyntaxHighlighter;
  protected
    procedure DoEditorModified; override;
  public
    constructor Create(ProjectNotifier: TBaseProjectNotifier; const ModuleFileName: string); override;
    destructor Destroy; override;
    property PreviousEditorPosition: TEditPos read FPreviousEditorPosition write FPreviousEditorPosition;
    property SyntaxHighlighter: TSyntaxHighlighter read FSyntaxHighlighter;
  end;

type
  TProofreaderProjectNotifier = class(TBaseProjectNotifier)
  public
    procedure AttachToLoadedModules;
  end;

procedure TProofreaderProjectNotifier.AttachToLoadedModules;
var
  i: Integer;
  AModuleNotifier: TBaseModuleNotifier;
  AProjectNotifier: TBaseProjectNotifier;
begin
  // This method attaches to already loaded modules
  // for the case where the proofreader expert is
  // activated "late" at runtime as oppposed to "early"
  // creation when the expert DLL loads (and initializes)

  Assert(GExpertsInst <> nil, 'GExpertsInst is nil in AttachToLoadedModules');
  AProjectNotifier := GExpertsInst.ProjectNotifier;

  Assert(AProjectNotifier <> nil, 'AProjectNotifier is nil in AttachToLoadedModules');
  for i := 0 to AProjectNotifier.ModuleNotifierCount-1 do
  begin
    AModuleNotifier := AProjectNotifier.ModuleNotifiers[i];

    { TODO -oStefan -cIssue:
        We do not protect against attaching to
        the same module more than once here. Very
        probably we don't have to do it, but check this
        assumption (and perhaps make it explicit
        in code at runtime) }
    Assert(AModuleNotifier <> nil, 'AModuleNotifier is nil in AttachToLoadedModules');
    Self.InstallModuleNotifier(AModuleNotifier.FileName);
  end;
end;

{ TReplacementItem }

constructor TReplacementItem.Create(const TypedString: string; ReplaceWhere: ShortInt;
  const ReplaceWithString: string);
begin
  inherited Create;

  Typed := TypedString;
  Where := ReplaceWhere;
  Replace := ReplaceWithString;
end;

{ TdmProofReader }

var
  PrivateProofReader: TdmProofReader = nil; // Singleton instance of data module

function dmProofReader: TdmProofReader;
begin
  Result := PrivateProofReader;
  Assert(Result <> nil, 'PrivateProofReader is nil in dmProofReader');
end;

constructor TdmProofReader.Create(AOwner: TComponent);
var
  i: Integer;
  j: TReplacementSource;
  DatabaseSet: TDBDataSet;
begin
  inherited Create(AOwner);

  Assert(PrivateProofReader = nil, 'PrivateProofreader is not nil in constructor');
  PrivateProofReader := Self;

  Database.Connected := False;
  Randomize;
  Session.SessionName := 'ACSession' + IntToHex(Random(32768), 4);
  Database.SessionName := Session.SessionName;
  Database.DatabaseName := 'ACData' + IntToHex(Random(32768), 4);
  Database.Params.Values['PATH'] := ConfigInfo.ConfigPath;
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TDBDataSet then
    begin
      DatabaseSet := TDBDataSet(Components[i]);
      DatabaseSet.SessionName := Session.SessionName;
      DatabaseSet.DatabaseName := Database.DatabaseName;
    end;
  end;
  // Initialize default settings for correcting
  FReplacerActive := True;
  // Dictionary based correction is off by default
  DictionaryActive := False;
  CompilerActive := False;
  FDictionaryCaseDiffer := True;
  FOtherLetter := True;
  FNoLetter := True;
  FMoreLetter := True;
  FMixedLetter := True;
  FNoFirstOther := True;
  FBeepOnReplace := True;

  // Restore settings (from registry)
  LoadSettings;

  for j := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    slReplace[j] := TStringList.Create;
    slDictionary[j] := TStringList.Create;
  end;
  slKibitz := TStringList.Create;
  FHistory := TCorrectionHistory.Create;
end;

destructor TdmProofReader.Destroy;
var
  i: TReplacementSource;
begin
  Assert(PrivateProofReader <> nil, 'PrivateProofReader is nil in dectructor');
  PrivateProofReader := nil;

  if FIsInstalled then
    Finalize;

  slKibitz.Free;
  slKibitz := nil;
  for i := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    slReplace[i].Free;
    slReplace[i] := nil;

    slDictionary[i].Free;
    slDictionary[i] := nil;
  end;
  FHistory.Free;
  FHistory := nil;

  inherited Destroy;
end;

procedure TdmProofReader.Initialize;
{$IFOPT D+}
resourcestring
  SReplacementTableOpen = 'Error opening replacement table:'#13#10;
  SOpenTableOpen = 'Error opening dictionary table:'#13#10;
{$ENDIF D+}
begin
  {$IFOPT D+}SendDebug('TdmProofReader.Initialize starting');{$ENDIF}
  Assert(not FIsInstalled, 'ProofReader is already installed in Initialize');

  if ReplacerActive then
  try
    tReplacement.Open;
    LoadReplacement;
  except
    on E: Exception do
    begin
      // Create the table here if it doesn't exist?
      {$IFOPT D+}ProcessExceptionMsg(E, SReplacementTableOpen);{$ENDIF D+}
      ReplacerActive := False;
    end;
  end;

  if DictionaryActive then
  try
    tDictionary.Open;
    LoadDictionary;
  except
    on E: Exception do
    begin
      // Create the table here if it doesn't exist?
      {$IFOPT D+}ProcessExceptionMsg(E, SOpenTableOpen);{$ENDIF D+}
      DictionaryActive := False;
    end;
  end;

  Assert(ToolServices <> nil);
  Assert(FModuleChangeNotifiers = nil);

  FModuleChangeNotifiers := TProofreaderProjectNotifier.Create(TAutoTypeWriterNotifier);
  (FModuleChangeNotifiers as TProofreaderProjectNotifier).AttachToLoadedModules;

  ToolServices.AddNotifier(FModuleChangeNotifiers);

  {$IFOPT D+}SendDebug('TdmProofReader.Initialize completed');{$ENDIF}

  FIsInstalled := True;
end;

procedure TdmProofReader.Finalize;
begin
  Assert(FIsInstalled, 'Not installed in TdmProofReader.Finalize');

  try
    tReplacement.Close;
    tDictionary.Close;
  except
    on E: Exception do
    begin
      ProcessException(E);
      {$IFOPT D+}SendDebug('ProofReaderFinalize: '+ E.Message);{$ENDIF}
    end;
  end;

  if FModuleChangeNotifiers <> nil then
  begin
    Assert(ToolServices <> nil);
    ToolServices.RemoveNotifier(FModuleChangeNotifiers);

    // Freeing this project notifier will (implicitly)
    // detach all owned, current module notifiers
    // and free them, too.
    FModuleChangeNotifiers.Free;
    FModuleChangeNotifiers := nil;
  end;

  FIsInstalled := False;
end;

procedure TdmProofReader.Reload;
begin
  {$IFOPT D+}SendDebug('Reloading ProofReader tables');{$ENDIF}
  if ReplacerActive and tReplacement.Active then
    LoadReplacement;

  if DictionaryActive and tDictionary.Active then
    LoadDictionary;
end;

const
  ConfigurationKey = '\GExperts\CodeProofreader';

procedure TdmProofReader.SaveSettings;
begin
  // do not localize any of the below items
  with TRegistry.Create do
  try
    OpenKey(ConfigInfo.RegKey + ConfigurationKey, True);

    WriteBool('ReplacerActive', FReplacerActive);
    WriteBool('DictionaryActive', FDictionaryActive);
  {$IFNDEF GX_BCB}
    // C++Builder does not have compiler support
    // for proof-reading.
    WriteBool('CompilerActive', FCompilerActive);
  {$ENDIF GX_BCB}
    WriteBool('DictionaryCaseDiffer', FDictionaryCaseDiffer);
    WriteBool('OtherLetter', FOtherLetter);
    WriteBool('NearbyLetter', FNearbyLetter);
    WriteBool('NoLetter', FNoLetter);
    WriteBool('MoreLetter', FMoreLetter);
    WriteBool('MixedLetter', FMixedLetter);
    WriteBool('NoFirstOther', FNoFirstOther);
    WriteBool('BeepOnReplace', FBeepOnReplace);
  finally
    Free;
  end;
end;

procedure TdmProofReader.LoadSettings;
var
  Registry: TRegistry;

  procedure ReadObsoleteOptions;
  type
    {$ALIGN ON}
    TObsoleteOptions = record
      ReplacerActive: Boolean;
      DictionaryActive: Boolean;
      DictionaryCaseDiffer: Boolean;
      OtherLetter: Boolean;
      NoLetter: Boolean;
      MoreLetter: Boolean;
      MixedLetter: Boolean;
      NoFirstOther: Boolean;
      BeepOnReplace: Boolean;
    end;
    {$ALIGN OFF}
  var
    ObsoleteOptions: TObsoleteOptions;
  begin
    {$IFOPT D+}SendDebug('Reading obsolete ProofReader Options');{$ENDIF}
    FillChar(ObsoleteOptions, SizeOf(ObsoleteOptions), 0);

    Assert(Registry <> nil);
    Registry.ReadBinaryData('Options', ObsoleteOptions,
        Min(Registry.GetDataSize('Options'), SizeOf(ObsoleteOptions)));
    FReplacerActive := ObsoleteOptions.ReplacerActive;
    FDictionaryActive := ObsoleteOptions.DictionaryActive;
    FDictionaryCaseDiffer := ObsoleteOptions.DictionaryCaseDiffer;
    FOtherLetter := ObsoleteOptions.OtherLetter;
    FNoLetter := ObsoleteOptions.NoLetter;
    FMoreLetter := ObsoleteOptions.MoreLetter;
    FMixedLetter := ObsoleteOptions.MixedLetter;
    FNoFirstOther := ObsoleteOptions.NoFirstOther;
    FBeepOnReplace := ObsoleteOptions.BeepOnReplace;
  end;

begin
  {$IFOPT D+}SendDebug('Loading ProofReader settings');{$ENDIF}
  // do not localize any of the below items
  Registry := TRegistry.Create;
  with Registry do
  try
    OpenKey(ConfigInfo.RegKey + ConfigurationKey, True);

    if ValueExists('Options') then
    begin
      // We have saved options in the obsolete (packed binary format)
      // style. Read these options into the current settings, then
      // save new-style "safe" options, finally delete the old
      // binary packed options.
      ReadObsoleteOptions;
      SaveSettings;
      DeleteValue('Options');
    end
    else
    begin
      if ValueExists('ReplacerActive') then
        FReplacerActive := ReadBool('ReplacerActive');

      if ValueExists('DictionaryActive') then
        FDictionaryActive := ReadBool('DictionaryActive');

    {$IFNDEF GX_BCB}
      // C++Builder does not have compiler support
      // for proof-reading.
      if ValueExists('CompilerActive') then
        FCompilerActive := ReadBool('CompilerActive');
    {$ENDIF GX_BCB}

      if ValueExists('DictionaryCaseDiffer') then
        FDictionaryCaseDiffer := ReadBool('DictionaryCaseDiffer');

      if ValueExists('OtherLetter') then
        FOtherLetter := ReadBool('OtherLetter');

      if ValueExists('NearbyLetter') then
        FNearbyLetter := ReadBool('NearbyLetter');

      if ValueExists('NoLetter') then
        FNoLetter := ReadBool('NoLetter');

      if ValueExists('MoreLetter') then
        FMoreLetter := ReadBool('MoreLetter');

      if ValueExists('MixedLetter') then
        FMixedLetter := ReadBool('MixedLetter');

      if ValueExists('NoFirstOther') then
        FNoFirstOther := ReadBool('NoFirstOther');

      if ValueExists('BeepOnReplace') then
        FBeepOnReplace := ReadBool('BeepOnReplace');
    end;
  finally
    Free;
  end;
  {$IFOPT D+}SendDebug('Loaded ProofReader settings');{$ENDIF}
end;

procedure TdmProofReader.ReverseString(var s: string);
var
  Counter: Integer;
  n: Integer;
  c: Char;
  Temp: Integer;
begin
  n := Length(s);
  // This will work fine for strings of odd length;
  // middle char does not need swapping
  for Counter := 1 to (n div 2) do
  begin
    c := s[Counter];

    Temp := n - Counter + 1;
    s[Counter] := s[Temp];
    s[Temp] := c;
  end;
end;

procedure TdmProofReader.LoadReplacement;
var
  i: TReplacementSource;
  ReplaceString: string;
  TypedString: string;
  AReplacement: TReplacementItem;
begin
  for i := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    Assert(slReplace[i] <> nil, 'slReplace is nil: ' + IntToStr(Ord(i)));
    slReplace[i].Clear;
  end;

  with tReplacement do
  begin
    DisableControls;
    try
      Filtered := False;
      First;
      if ReplacerActive then
        while not EOF do
        begin
          if Trim(tReplacementTyped.AsString) <> '' then
          begin
            TypedString := tReplacementTyped.AsString;
            ReverseString(TypedString);
            ReplaceString := tReplacementReplace.AsString;

            AReplacement := TReplacementItem.Create(TypedString, tReplacementWhereReplace.AsInteger, ReplaceString);
            slReplace[TReplacementSource(tReplacementLanguage.AsInteger)].AddObject(TypedString, AReplacement);
          end;
          Next;
        end;
    finally
      Close;
      EnableControls;
      for i := Low(TReplacementSource) to High(TReplacementSource) do
        slReplace[i].Sort;
    end;
  end;
end;

function TdmProofReader.FindReplacement(Zone: TReplacementSource; TypedString: string): TReplacementItem;
var
  a: Integer;
  b: Integer;
  m: Integer;
begin
  Result := nil;
  if TypedString = '' then Exit;

  a := 0;
  b := slReplace[Zone].Count - 1;
  ReverseString(TypedString);
  if b >= 0 then
  begin
    if AnsiCompareText(slReplace[Zone].Strings[b], Copy(TypedString, 1, Length(slReplace[Zone].Strings[b]))) = 0 then
    begin
      Result := slReplace[Zone].Objects[b] as TReplacementItem;
      Exit;
    end
    else
    begin
      //! StH: Consider moving this to a local procedure
      while True do
      begin
        m := (a + b) div 2;
        Result := slReplace[Zone].Objects[m] as TReplacementItem;
        if AnsiCompareText(Result.Typed, TypedString) = 0 then
          Exit
        else
        begin
          if AnsiCompareText(Result.Typed, TypedString) > 0 then
          begin
            if b = m then
            begin
              if AnsiCompareText(Copy(TypedString, 1, Length(Result.Typed)), Result.Typed) <> 0 then
                Result := nil;
              Exit;
            end
            else
              b := m;
          end
          else
          begin
            if a = m then
            begin
              if AnsiCompareText(Copy(TypedString, 1, Length(Result.Typed)), Result.Typed) <> 0 then
                Result := nil;
              Exit;
            end
            else
              a := m;
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmProofReader.LoadDictionary;
var
  i: TReplacementSource;
  DictionaryWordString: string;
begin
  for i := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    Assert(slDictionary[i] <> nil, 'slDictionary is nil: ' + IntToStr(Ord(i)));
    slDictionary[i].Clear;
  end;

  with tDictionary do
  try
    DisableControls;
    Filtered := False;
    First;
    if DictionaryActive then
      while not EOF do
      begin
        DictionaryWordString := tDictionaryWord.AsString;
        if Trim(DictionaryWordString) <> '' then
          slDictionary[TReplacementSource(tDictionaryLanguage.AsInteger)].Add(DictionaryWordString);
        Next;
      end;
  finally
    Close;
    EnableControls;

    for i := Low(TReplacementSource) to High(TReplacementSource) do
      SortStringList(slDictionary[i]);
  end;
end;

(*
// These are obtained dynamically now
const
  KeyboardChars: string =
    '`1234567890-=\' +
    'qwertyuiop[]' +
    'asdfghjkl;''' +
    'zxcvbnm,./';

const
  ShiftKeyboardChars: string =
    '~!@#$%^&*()_+|' +
    'QWERTYUIOP{}' +
    'ASDFGHJKL:"' +
    'ZXCVBNM<>?';
*)

// FS: get local keyboard Layout
var
  KeysWithDistance1: array[1..50] of set of char;
  KeyboardChars, ShiftKeyboardChars, AltGrKeyboardChars: ShortString;
  BarrierChars: set of char;
  Barriers, NextChars: string[6]; // FS: added

//Python
(*var
  KeysWithDistance1: array[#0..#$7F] of set of char = (
  {0X}  [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [],
  {1X}  [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [],
  {2X}  [], [], [], [], [], [], [], [], [], [], [], [],
        {,}['m', 'k', 'l', '.'],
        {-}['0', '=', 'p', '['],
        {.} [',', 'l', ';', '/'],
        {/} ['.', ';', ''''],
  {3X}
        {0}['9', '-', 'o', 'p'],
        {1}['`', '2', 'q'],
        {2}['1', '3', 'q', 'w'],
        {3}['2', '4', 'w', 'e'],
        {4}['3', '5', 'e', 'r'],
        {5}['4', '6', 'r', 't'],
        {6}['5', '7', 't', 'y'],
        {7}['6', '8', 'y', 'u'],
        {8}['7', '9', 'u', 'i'],
        {9}['8', '0', 'i', 'o'], [],
        {;}['p', '[', 'l', '''', '.', '/'], [],
        {=}['-', '\', '[', ']'], [], [],
  {4X}  [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [],
  {5X}  [], [], [], [], [], [], [], [], [], [], [],
        {[}['-', '=', 'p', ']', ';', ''''],
        {\}['=', ']'],
        {]}['=', '\', '[', ''''], [], [],
  {6X}  {'}['[', ']', ';', '/'],
        {a}['q', 'w', 's', 'z'],
        {b}['v', 'g', 'h', 'n'],
        {c}['x', 'd', 'f', 'v'],
        {d}['e', 'r', 's', 'f', 'x', 'c'],
        {e}['3', '4', 'w', 'r', 's', 'd'],
        {f}['r', 't', 'd', 'g', 'c', 'v'],
        {g}['t', 'y', 'f', 'h', 'v', 'b'],
        {h}['y', 'u', 'g', 'j', 'b', 'n'],
        {i}['8', '9', 'u', 'o', 'j', 'k'],
        {j}['u', 'i', 'h', 'k', 'n', 'm'],
        {k}['i', 'o', 'j', 'l', 'm', ','],
        {l}['o', 'p', 'k', ';', ',', '.'],
        {m}['n', 'j', 'k', ','],
        {n}['b', 'h', 'j', 'm'],
        {o}['9', '0', 'i', 'p', 'k', 'l'],
  {7X}
        {p}['0', '-', 'o', '[', 'l', ';'],
        {q}['1', '2', 'w', 'a'],
        {r}['4', '5', 'e', 't', 'd', 'f'],
        {s}['w', 'e', 'a', 'd', 'z', 'x'],
        {t}['5', '6', 'r', 'y', 'f', 'g'],
        {u}['7', '8', 'y', 'i', 'h', 'j'],
        {v}['c', 'f', 'g', 'b'],
        {w}['2', '3', 'q', 'e', 'a', 's'],
        {x}['z', 's', 'd', 'c'],
        {y}['6', '7', 't', 'u', 'g', 'h'],
        {z}['a', 's', 'x'],
        [], [], [],
        {~}['1'], []
    );
//    ('qsz', 'vghn', 'xdfv', 'sefc', ';

// do not localize any of the below items
(*
const
  KeysWithDistance1: array[1..47] of set of char = (
  {~}['1'],
  {1}['`', '2', 'q'],
  {2}['1', '3', 'q', 'w'],
  {3}['2', '4', 'w', 'e'],
  {4}['3', '5', 'e', 'r'],
  {5}['4', '6', 'r', 't'],
  {6}['5', '7', 't', 'y'],
  {7}['6', '8', 'y', 'u'],
  {8}['7', '9', 'u', 'i'],
  {9}['8', '0', 'i', 'o'],
  {0}['9', '-', 'o', 'p'],
  {-}['0', '=', 'p', '['],
  {=}['-', '\', '[', ']'],
  {\}['=', ']'],

  {q}['1', '2', 'w', 'a'],
  {w}['2', '3', 'q', 'e', 'a', 's'],
  {e}['3', '4', 'w', 'r', 's', 'd'],
  {r}['4', '5', 'e', 't', 'd', 'f'],
  {t}['5', '6', 'r', 'y', 'f', 'g'],
  {y}['6', '7', 't', 'u', 'g', 'h'],
  {u}['7', '8', 'y', 'i', 'h', 'j'],
  {i}['8', '9', 'u', 'o', 'j', 'k'],
  {o}['9', '0', 'i', 'p', 'k', 'l'],
  {p}['0', '-', 'o', '[', 'l', ';'],
  {[}['-', '=', 'p', ']', ';', ''''],
  {]}['=', '\', '[', ''''],

  {a}['q', 'w', 's', 'z'],
  {s}['w', 'e', 'a', 'd', 'z', 'x'],
  {d}['e', 'r', 's', 'f', 'x', 'c'],
  {f}['r', 't', 'd', 'g', 'c', 'v'],
  {g}['t', 'y', 'f', 'h', 'v', 'b'],
  {h}['y', 'u', 'g', 'j', 'b', 'n'],
  {j}['u', 'i', 'h', 'k', 'n', 'm'],
  {k}['i', 'o', 'j', 'l', 'm', ','],
  {l}['o', 'p', 'k', ';', ',', '.'],
  {;}['p', '[', 'l', '''', '.', '/'],
  {'}['[', ']', ';', '/'],

  {z}['a', 's', 'x'],
  {x}['z', 's', 'd', 'c'],
  {c}['x', 'd', 'f', 'v'],
  {v}['c', 'f', 'g', 'b'],
  {b}['v', 'g', 'h', 'n'],
  {n}['b', 'h', 'j', 'm'],
  {m}['n', 'j', 'k', ','],
  {,}['m', 'k', 'l', '.'],
  {.}[',', 'l', ';', '/'],
  {/}['.', ';', '''']
    );

const
  BarrierChars = ['m', 'l', 'p', 'M', 'L', 'P'];
*)

{$WARNINGS OFF}
procedure InitKeySets;

  procedure IncludeInSet(iKeySet: Integer; Key: Char);
  begin
    if Key <> ' ' then
      KeysWithDistance1[iKeySet] := KeysWithDistance1[iKeySet] + [Key];
  end;

type
  KeyState = (ksNormal, ksShift, ksAltGr);
var
  KeyBoardLayout: array[KeyState] of array[1..4] of string[15];
  i, j, Row, Column: Smallint;
  aKeyState: Keystate;
begin
  Column := 0;
  // get layout as 4 rows of 11..13 keys in 3 states
  // keyboard state could be Normal, Shift or AltGr (CTRL+ALT)
  FillChar(KeyBoardLayout, SizeOf(KeyBoardLayout), #32);
  for aKeyState := ksNormal to ksAltGr do
  begin
    KeyBoardLayout[aKeyState][1][0] := #13;
    KeyBoardLayout[aKeyState][2][0] := #12;
    KeyBoardLayout[aKeyState][3][0] := #12;
    KeyBoardLayout[aKeyState][4][0] := #11;
  end;

  for i := 33 to 254 do
  begin
    j := VkKeyScan(Chr(i)); // char to virtual key
    if Lo(j) > 0 then
    begin
      case Hi(j) of
        0: aKeystate := ksNormal;
        1: aKeystate := ksShift;
        6: aKeystate := ksAltGr; // (CTRL+ALT)
      else
        Continue; // ignore other shift states
      end;

      begin
        j := MapVirtualKey(lo(j), 0); // virtual key code to scan code
        Row := 0;
        case Lo(j) of
          1..13:
            begin
              Row := 1;
              Column := j;
            end;
          16..27:
            begin
              Row := 2;
              Column := j - 15;
            end;
          30..40:
            begin
              Row := 3;
              Column := j - 29;
            end;
          44..53:
            begin
              Row := 4;
              Column := j - 42;
            end;
          41:
            begin
              Row := 1;
              Column := 1;
            end;
          43:
            begin
              Row := 3;
              Column := 12;
            end;
          86:
            begin
              Row := 4;
              Column := 1;
            end;
        end;
        if (Row > 0) and (KeyBoardLayout[aKeyState][Row][Column] = #32) then
          KeyBoardLayout[aKeyState][Row][Column] := Char(i);
      end;
    end;
  end;

  KeyboardChars :=
    KeyBoardLayout[ksNormal][1] +
    KeyBoardLayout[ksNormal][2] +
    KeyBoardLayout[ksNormal][3] +
    KeyBoardLayout[ksNormal][4];

  ShiftKeyboardChars :=
    KeyBoardLayout[ksShift][1] +
    KeyBoardLayout[ksShift][2] +
    KeyBoardLayout[ksShift][3] +
    KeyBoardLayout[ksShift][4];

  AltGrKeyboardChars :=
    KeyBoardLayout[ksAltgr][1] +
    KeyBoardLayout[ksAltgr][2] +
    KeyBoardLayout[ksAltgr][3] +
    KeyBoardLayout[ksAltgr][4];

  // Get keys with distance 1
  for i := 1 to Length(KeyboardChars) do
  begin
    case i of

      1:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][2]);
        end;

      2..13:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i - 1]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 1]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 1]);
          if i > 2 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 2]);
        end;

      14..25:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 2 - 13]);
          if i > 14 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i + 1 - 13]);
          if i > 14 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 13]);
        end;

      26..37:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i + 1 - 25]);
          if i > 26 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 1 - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i + 1 - 25]);
          if i > 26 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][4][i - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][4][i + 1 - 25]);
        end;

      38..48:
        begin
          if i > 38 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 38]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 37]);
          if i > 38 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][4][i - 1 - 37]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][4][i + 1 - 37]);
        end;
    end;
  end;

  // Get BarrierChars
  BarrierChars := [];
  Barriers := '';
  NextChars := '';
  for Row := 2 to 4 do
  begin
    for aKeyState := ksNormal to ksShift do
    begin
      Column := Length(KeyBoardLayout[aKeyState][Row]);
      while (Column > 0) and not (KeyBoardLayout[aKeyState][Row][Column] in ['a'..'z', 'A'..'Z']) do
        Dec(Column);
      if (Column > 0) and (Column < Length(KeyBoardLayout[aKeyState][Row])) then
      begin
        BarrierChars := BarrierChars + [KeyBoardLayout[aKeyState][Row][Column]];
        Barriers := Barriers + KeyBoardLayout[aKeyState][Row][Column];
        NextChars := NextChars + KeyBoardLayout[aKeyState][Row][Column + 1];
      end;
    end;
  end;
end;
{$WARNINGS ON}

function TdmProofReader.FindStringList(StringList: TStringList; SortedList: Boolean; const Word: string): string;
var
  MaxErrs: Integer;
  ResString: string absolute Result;
  s: string;

  procedure CheckWord(const InDict: string);

    function ErrCount(CurErrLevel: Integer; const InDict, Typed: string): Integer;

      function CharsNearOnKeyBoard(a, b: Char): Boolean;
      var
        p: ShortInt;
        q: ShortInt;
      begin
        p := Pos(a, KeyboardChars);
        if p = 0 then
          p := Pos(a, ShiftKeyboardChars);
        if p = 0 then
          p := Pos(a, AltGrKeyboardChars); // FS: added
        q := Pos(b, KeyboardChars);
        if q = 0 then
          q := Pos(b, ShiftKeyboardChars);
        if q = 0 then
          q := Pos(b, AltGrKeyboardChars); // FS: added

        Result := (p > 0) and (q > 0) and
          (KeyboardChars[q] in KeysWithDistance1[p]);
      end;

    var
      DifferCharIndex: Integer;
      nInDict, nTyped, minN: Integer;
      ResultCandidate: Integer;
    begin

      // Return if the InDict and Typed are the same the CurErrLevel
      if InDict = Typed then
      begin
        Result := CurErrLevel;
        Exit;
      end;

      if CurErrLevel > MaxErrs then
      begin
        Result := 1000;
        Exit;
      end;

      nInDict := Length(InDict);
      nTyped := Length(Typed);
      minN := Min(nInDict, nTyped);

      // Skip words that have too many additional characters
      if abs(nInDict - nTyped) * 10 + CurErrLevel > MaxErrs then
      begin
        Result := 1000;
        Exit;
      end;

      DifferCharIndex := 1;
      while (DifferCharIndex <= minN) and
            (InDict[DifferCharIndex] = Typed[DifferCharIndex]) do
      begin
        Inc(DifferCharIndex);
      end;
      // The chars are now the same untile DifferCharIndex

      // If InDict or Typed has run out of letters.
      if DifferCharIndex = minN then
      begin
        // If the moreletters are allowed and there are still letters in Typed
        if (nTyped <> DifferCharIndex) and MoreLetter then
          // Return for every addional letter 20 more error levels.
          Result := (nTyped - DifferCharIndex) * 20 + CurErrLevel
        // If there are still letters in InDict and missing letters are allowed.
        else if (nInDict <> DifferCharIndex) and NoLetter then
          // Return for every addional letter 20 more error levels.
          Result := (nInDict - DifferCharIndex) * 20 + CurErrLevel
        else
          Result := 1000;

        Exit;
      end;

      Result := 1000;

      // If other letters are allowed and the chars are near on the keyboard.
      if OtherLetter and ((not NearbyLetter) or (CharsNearOnKeyBoard(Typed[DifferCharIndex], InDict[DifferCharIndex]))) then
      begin
        // The Result is the errorcount +14 found in the resulting string.
        Result := ErrCount(CurErrLevel + 14,
            Copy(InDict, DifferCharIndex +1, nInDict - DifferCharIndex),
            Copy(Typed, DifferCharIndex +1, nTyped - DifferCharIndex));
      end;

      if NoLetter then
      begin
        // Delete one character from the InDict string and get resulting
        // error count + 14.
        ResultCandidate := ErrCount(CurErrLevel + 14,
          Copy(InDict, DifferCharIndex + 1, nInDict - DifferCharIndex),
          Copy(Typed, DifferCharIndex, nTyped - DifferCharIndex +1));

        if ResultCandidate < Result then
          Result := ResultCandidate;
      end;

      if MoreLetter then
      begin
        ResultCandidate := ErrCount(CurErrLevel + 16,
          Copy(InDict, DifferCharIndex, nInDict - DifferCharIndex +1),
          Copy(Typed, DifferCharIndex + 1, nTyped - DifferCharIndex));

        if ResultCandidate < Result then
          Result := ResultCandidate;
      end;

      if MixedLetter and (nInDict > DifferCharIndex) and
         (nTyped > DifferCharIndex) and
         (Typed[DifferCharIndex] = InDict[DifferCharIndex + 1]) and
         (Typed[DifferCharIndex + 1] = InDict[DifferCharIndex]) then
      begin
        ResultCandidate := ErrCount(CurErrLevel + 13,
          Copy(InDict, DifferCharIndex + 2, nInDict - DifferCharIndex - 1),
          Copy(Typed, DifferCharIndex + 2, nTyped - DifferCharIndex - 1));

        if ResultCandidate < Result then
          Result := ResultCandidate;
      end;
    end;

  var
    Err: Integer;
  begin
    if NoFirstOther and (AnsiCompareText(InDict[1], s[1]) <> 0) then
      Exit;

    if DictionaryCaseDiffer then
      Err := ErrCount(0, AnsiLowerCase(InDict), s)
    else
      Err := ErrCount(0, InDict, s);

    // Possibly save the errorcount as the max error count
    if Err <= MaxErrs then
    begin
      if (Err = MaxErrs) and (ResString <> InDict) then
        ResString := '@'
      else
      begin
        MaxErrs := Err;
        ResString := InDict;
      end;
    end;
  end;

  function FindInStringList(sl: TStringList; CaseSensitive: Boolean): Boolean;

    function BinaryCaseSensitiveSearch: Boolean;
    var
      i: Integer;
      a: Integer;
      b: Integer;
      m: Integer;
    begin
      Result := False;
      a := 0;
      b := sl.Count - 1;
      if b >= 0 then
      begin
        if AnsiCompareStr(sl[b], Word) = 0 then
        begin
          Result := True;
          Exit;
        end
        else
          while True do
          begin
            m := (a + b) div 2;
            i := AnsiCompareStr(sl[m], Word);
            if i = 0 then
            begin
              Result := True;
              Exit;
            end
            else
            begin
              if i > 0 then
              begin
                if b = m then
                  Exit
                else
                  b := m
              end
              else
              begin
                if a = m then
                  Exit
                else
                  a := m;
              end;
            end;
          end;
      end;
    end;

    function LinearCaseSensitiveSearch: Boolean;
    var
      i: Integer;
    begin
      for i := 0 to sl.Count - 1 do
      begin
        if AnsiCompareStr(sl[i], Word) = 0 then
        begin
          Result := True;
          Exit;
        end
      end;
      Result := False;
    end;

    function LinearCaseInSensitiveSearch: Boolean;
    var
      i: Integer;
    begin
      for i := 0 to sl.Count - 1 do
      begin
        if AnsiCompareText(sl[i], Word) = 0 then
        begin
          Result := True;
          Exit;
        end
      end;
      Result := False;
    end;

  begin
    if CaseSensitive then // String list is sorted as case sensitive - binary search
    begin
      if SortedList then
        Result := BinaryCaseSensitiveSearch
      else
        Result := LinearCaseSensitiveSearch;
    end
    else
      Result := LinearCaseInSensitiveSearch;
  end;

var
  i: Integer;
begin
  if (Trim(Word) = '') or (SortedList and (FindInStringList(StringList, DictionaryCaseDiffer))) then
    Exit;

  Result := '';
  MaxErrs := Length(Word) + 10;

  if DictionaryCaseDiffer then
    s := AnsiLowerCase(Word)
  else
    s := Word;

  for i := 0 to StringList.Count - 1 do
  begin
    CheckWord(StringList[i]);
    if Result = Word then
    begin
      Result := '';
      Exit;
    end;
  end;

  if Result = '@' then
  begin
    {$IFOPT D+}SendDebug(Format('Ambiguity: %s --> %s or %s', [Word, Result, s])); {$ENDIF D+}
    Result := '';
  end;
end;

function TdmProofReader.FindDictionary(Zone: TReplacementSource; const Word: string): string;
begin
  Result := FindStringList(slDictionary[Zone], True, Word);
end;

function TdmProofReader.IsWord(const s: string): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 1 to Length(s) do
    if not (s[i] in AlphaNum) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TdmProofReader.tReplacementTypedSetText(Sender: TField; const Text: string);
var
  TempStr: string;
  f: TField;
begin
  Assert(Sender <> nil);

  TempStr := Trim(Text);
  if TempStr = '' then
    Sender.Value := NULL
  else
    Sender.AsString := AnsiLowerCase(TempStr);

  try
    f := Sender.DataSet.FieldByName('WhereReplace'); // do not localize
    if (f <> nil) and f.IsNull then
    begin
      if IsWord(TempStr) then
        f.AsInteger := wrWholeWord
      else
        f.AsInteger := wrAnywhere;
    end;
  except
    on E: Exception do
    begin
      ProcessException(E);
    end;
  end;
end;

procedure TdmProofReader.tReplacementWhereReplaceGetText(
  Sender: TField; var Text: string; DisplayText: Boolean);
begin
  Assert(Sender <> nil);

  if Sender.IsNull then
    Text := ''
  else
    case Sender.AsInteger of
      wrAnywhere:
        Text := swrAnywhere;
      wrWordBegin:
        Text := swrWordBegin;
      wrWordEnd:
        Text := swrWordEnd;
      wrWholeWord:
        Text := swrWholeWord;
    else
      Text := '';
    end;
end;

procedure TdmProofReader.tReplacementWhereReplaceSetText(
  Sender: TField; const Text: string);
begin
  Assert(Sender <> nil);

  if AnsiCompareText(Text, swrAnywhere) = 0 then
    Sender.AsInteger := wrAnywhere
  else
    if AnsiCompareText(Text, swrWordBegin) = 0 then
      Sender.AsInteger := wrWordBegin
    else
      if AnsiCompareText(Text, swrWordEnd) = 0 then
        Sender.AsInteger := wrWordEnd
      else
        if AnsiCompareText(Text, swrWholeWord) = 0 then
          Sender.AsInteger := wrWholeWord
        else
          raise Exception.Create('Invalid field value');
end;

procedure TdmProofReader.tReplacementNewRecord(DataSet: TDataSet);
begin
   // do not localize
  DataSet.FieldByName('Language').Value := DefaultLanguage;
end;

procedure TdmProofReader.tReplacementBeforePost(DataSet: TDataSet);
var
  s: string;
begin
  s := DataSet.FieldByName('Typed').AsString; // do not localize
  if (not IsWord(s)) and
     (not (DataSet.FieldByName('WhereReplace').AsInteger in [wrAnywhere])) then // do not localize
  begin
    raise Exception.CreateFmt('"%s" is not a word', [s]);
  end;
end;

{=========================  Main procedure of Code proof-reader  ============================}

{ TAutoTypeWriterNotifier}

constructor TAutoTypeWriterNotifier.Create(ProjectNotifier: TBaseProjectNotifier; const ModuleFileName: string);
var
  EditorInterface: TIEditorInterface;
begin
  inherited Create(ProjectNotifier, ModuleFileName);

  with FModuleInterface do
  begin
    FSyntaxHighlighter := shNone;
    EditorInterface := GetEditorInterface;
    if EditorInterface <> nil then
    begin
      try
        FSyntaxHighlighter := EditorInterface.SetSyntaxHighlighter(shQuery);
      finally
        EditorInterface.Release;
      end;
    end
    else
    begin
      {$IFOPT D+}SendDebugEx(Format('Proofreader: Could not get EditorInterface for', [FFileName]), mtWarning);{$ENDIF}
    end;

    {$IFOPT D+}SendDebug(Format('Module = %s, Syntax = %d', [FFileName, Ord(FSyntaxHighlighter)]));{$ENDIF}
    if FSyntaxHighlighter in [shPascal, shSQL{$IFDEF GX_VER110_up}, shC{$ENDIF GX_VER110_up}] then
      FIsKnownSourceCode := True;
  end;
end;

destructor TAutoTypeWriterNotifier.Destroy;
begin
  FIsKnownSourceCode := False;

  inherited Destroy;
end;


function TAutoTypeWriterNotifier.GetReplacementSource(Element: Integer;
  var Source: TReplacementSource): Boolean;
begin
  Result := True;

  if (SyntaxHighlighter = shPascal) and (Element in shsSource) then
    Source := rtPasSrc
  else
  {$IFDEF GX_VER120_up}
  if (SyntaxHighlighter = shC) and (Element in shsSource) then
    Source := rtCPPSrc
  else if (SyntaxHighlighter = shC) and (Element in [atPreproc]) then
    Source := rtPreproc
  else
  {$ENDIF GX_VER120_up}
  if (SyntaxHighlighter = shSQL) and (Element in shsSource) then
    Source := rtSQLSrc
  else if Element in [atAssembler] then
    Source := rtAssembler
  else if Element in [atString] then
    Source := rtString
  else if Element in [atComment] then
    Source := rtComment
  else
    Result := False
end;

function TAutoTypeWriterNotifier.HaveValidPreviousPos: Boolean;
begin
  Result := (FPreviousEditorPosition.Col <> 0) and
            (FPreviousEditorPosition.Line <> 0);
end;

procedure TAutoTypeWriterNotifier.AppendHistory(CorrectionKind: TCorrectionKind;
  SourceLanguage: TReplacementSource; const InfoString, OriginalText: string);
var
  Correction: TCorrectionItem;
begin
  try
    Correction := TCorrectionItem.Create;
    try
      Correction.CorrectionKind := CorrectionKind;
      Correction.SourceLanguage := SourceLanguage;
      Correction.OriginalText := OriginalText;
      Correction.InfoString := InfoString;
      Correction.Time := Now;
    except
      on E: Exception do
      begin
        Correction.Free;
      end;
    end;
    dmProofReader.FHistory.Add(Correction);
  except
    on E: Exception do
    begin
      // nothing
    end;
  end;
end;

procedure TAutoTypeWriterNotifier.AppendAutoCorrectHistory(SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckAutoCorrection,
    SourceLanguage,
    Format('AutoCorrect: "%s" --> "%s"', [FromString, ToString]),
    FromString);
end;

procedure TAutoTypeWriterNotifier.AppendWordHistory(SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckWord,
    SourceLanguage,
    Format('Dictionary Word: "%s" --> "%s"', [FromString, ToString]),
    FromString);
end;

procedure TAutoTypeWriterNotifier.AppendKibitzHistory(SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckWord,
    SourceLanguage,
    Format('Compiler Correct: "%s" --> "%s"', [FromString, ToString]),
    FromString);
end;

procedure TAutoTypeWriterNotifier.BeepOnDemand;
begin
  if dmProofReader.BeepOnReplace then
    MessageBeep(MB_OK);
end;

function GetBufferBeforeCursor(EditorInterface: TIEditorInterface; Pos: Longint): string;
const
  MaxBufRead = 1024;
  ReadSize = 256;
var
  NotifyBuf: array[0..MaxBufRead] of Char;
begin
  with EditorInterface.CreateReader do
  try
    FillChar(NotifyBuf, SizeOf(NotifyBuf), 0);
    NotifyBuf[0] := ' ';
    if Pos > ReadSize + 1 then
      GetText(Pos - (ReadSize + 1), @NotifyBuf[0], ReadSize + 1)
    else
      GetText(0, @NotifyBuf[1], Pos);
    Result := StrPas(NotifyBuf);
  finally
    Release; // the reader
  end;
end;

//! StH: Consolidate with NeedsReplacementEnd
function NeedsReplacementBegin(ReplaceItem: TReplacementItem; const ReplaceString: string): Boolean;
var
  PartialReplaceString: string;
begin
  if ReplaceItem = nil then
  begin
    Result := False;
    Exit;
  end;

  PartialReplaceString := Copy(ReplaceString,
    Length(ReplaceString) - Length(ReplaceItem.Replace) + 1,
    Length(ReplaceItem.Replace));

  Result := (AnsiCompareStr(ReplaceItem.Replace, PartialReplaceString) <> 0);

  Result := Result and
    ((ReplaceItem.Where = wrAnywhere) or
    (ReplaceItem.Where = wrWordBegin) and not
    (ReplaceString[Length(ReplaceString) - Length(ReplaceItem.Typed)] in AlphaNum));
end;

function NeedsReplacementEnd(ReplaceItem: TReplacementItem; const ReplaceString: string): Boolean;
var
  PartialReplaceString: string;
begin
  if ReplaceItem = nil then
  begin
    Result := False;
    Exit;
  end;

  PartialReplaceString := Copy(ReplaceString,
    Length(ReplaceString) - Length(ReplaceItem.Replace) + 1,
    Length(ReplaceItem.Replace));

  Result := (AnsiCompareStr(ReplaceItem.Replace, PartialReplaceString) <> 0);

  Result := Result and
    ((ReplaceItem.Where = wrWordEnd) or
    (ReplaceItem.Where = wrWholeWord) and not
    (ReplaceString[Length(ReplaceString) - Length(ReplaceItem.Typed)] in AlphaNum));
end;

{ Ok, we have a pesky problem with the editor. If someone is replacing
  a bit too much with the Replace feature and at the same time uses the
  confirmation dialog, then the editor kernel isn't in a fancy mood
  (it AVs if we touch it with the OTA).
  Thus we simply try to detect that confirmation dialog and panic
  out if we find it. }
function IsReplaceConfirmDialogOnScreen: Boolean;
const
  ConfirmDialogClassName = 'TMessageForm';
var
  AForm: TForm;
  i: Integer;
begin
  Result := False;

  Assert(Assigned(Screen));
  // Count in reverse, since it is usually last in the list anyway
  for i := Screen.FormCount - 1 downto 0 do
  begin
    AForm := Screen.Forms[i];

    Assert(Assigned(AForm));
    if AForm.ClassName = ConfirmDialogClassName then
    begin
      // Make sure it lives in the main VCL module (package or not)
      if FindClassHInstance(AForm.ClassType) = FindClassHInstance(TObject) then
      begin
        // Now some weak heuristics (don't localize the component names).
        if Assigned(AForm.FindComponent('Yes')) and
           Assigned(AForm.FindComponent('No')) and
           Assigned(AForm.FindComponent('Cancel')) and
           Assigned(AForm.FindComponent('All')) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TAutoTypeWriterNotifier.DoEditorModified;

  function CharPosIsEqual(a, b: TCharPos): Boolean;
  begin
    Result := (a.Line = b.Line) and (a.CharIndex = b.CharIndex);
  end;

  function LastCharsAreCRLF(const Str: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    i := Length(Str);
    while (i > 0) and (Str[i] in [#9, #32]) do
      Dec(i);
    if i > 1 then
      Result := (Str[i] = #10) and (Str[i - 1] = #13);
    //{$IFOPT D+}SendDebug('LastChars: '+IntToStr(Ord(Str[i - 1]))+ ' '+IntToStr(Ord(Str[i])));{$ENDIF}
  end;

  // Test whether the last attribute before the current one
  // is part of another one passed in InElement - or something else.
  // This is only called if GetAttributeAtPos returns
  // atWhiteSpace - it is not clear, then, whether
  // the caret is inside another element.
  // Currently, this code is used for atComment and atString,
  // see below.
  { TODO -oStefan -cFeature:
    Reconsider this code so that we don't call it twice - perhaps
    something generic that returns atWhiteSpace if not within
    atComment, atString - else atComment, atString.

    Perhaps "ReconsiderWhiteSpaceIdentification" ?
  }
  function GenericTestForWhiteSpaceInElement(const InElement: Integer;
    EV: TIEditView; EditorPosition: TEditPos): Boolean;
  var
    Element: Integer;
    LineFlag: Integer;
  begin
    Element := atWhiteSpace;

    // Strange enough, but this algorithm
    // seems to be serving each situation,
    // even the case where we react the
    // very beginning of the line (col = 0)?
    // The expected case would have been to
    // climb the lines up and check further
    // from the end towards the beginning,
    // but that somehow is not necessary??
    Dec(EditorPosition.Col);
    while EditorPosition.Col >= 0 do
    begin
      EV.GetAttributeAtPos(EditorPosition, Element, LineFlag);
      if Element <> atWhiteSpace then
        Break;
      Dec(EditorPosition.Col);
    end;

    Result := (Element = InElement);
  end;

  function TestForWhiteSpaceInComment(EV: TIEditView; EditorPosition: TEditPos): Boolean;
  begin
    Result := GenericTestForWhiteSpaceInElement(atComment, EV, EditorPosition);
  end;

  function TestForWhiteSpaceInString(EV: TIEditView; EditorPosition: TEditPos): Boolean;
  begin
    Result := GenericTestForWhiteSpaceInElement(atString, EV, EditorPosition);
  end;

var
  c: string;
  s: string;
  d: string;

var
  EI: TIEditorInterface;
  EditView: TIEditView;
  PrevPos: TEditPos;
  EPos: TEditPos;
  CPos: TCharPos;
  Element: Integer;
  LineFlag: Integer;
  RepTable: TReplacementSource;
  EditorPos: Longint;
  ReplaceItem: TReplacementItem;
  CurrentView: Integer;
  n: Integer;
  CursorOffset: Integer;
  EditorForm: TCustomForm;
  EditorComponent: TComponent;
begin
  inherited DoEditorModified;

  // Main work - Module was changed
  try
    if not dmProofReader.ReplacerActive and not dmProofReader.DictionaryActive and
       not dmProofReader.CompilerActive then
    begin
      Exit;
    end;

   if FModifyingSelf then
      Exit;

    if IsReplaceConfirmDialogOnScreen then
      Exit;

    // Don't correct when the current form isn't an IDE edit window
    // This prevents some unwanted corrections when other experts,
    // the form designer, or the IDE make changes
    EditorForm := Screen.ActiveCustomForm;
    if ((EditorForm = nil) or (not (Pos('EditWindow_', EditorForm.Name) = 1)) or (not (EditorForm.ClassName = 'TEditWindow'))) then
      Exit;

    EI := FModuleInterface.GetEditorInterface;
    Assert(EI <> nil, 'EditorInterface is nil');
    try
      FModifyingSelf := True;
      // Editor change notifiers may fire if the IDE auto-creates
      // some code "hidden". Since we only want to react on user
      // input, we exit if there is no view at all
      if not (EI.GetViewCount > 0) then
        Exit;

      if not CharPosIsEqual(EI.BlockStart, EI.BlockAfter) then
        Exit;

      CurrentView := GetCurrentEditView(EI);
      Assert(CurrentView >= 0, 'No EditViews available');

      EditView := EI.GetView(CurrentView);
      try
        EPos := EditView.CursorPos;

        PrevPos := EPos;

        EditView.ConvertPos(True, EPos, CPos);
        EditorPos := EditView.CharPosToPos(CPos);

        // s - Editor buffer content before cursor.
        s := GetBufferBeforeCursor(EI, EditorPos);

        if not HaveValidPreviousPos then
        begin
          FPreviousEditorPosition := EPos;
          // Exit;
        end;

        // This is set incorrectly when pressing enter after a comment
        // It reports the previous character as Pascal/C++ Source
        // So we just exit when we are at the end of a line
        // !!FixMe!!

        if LastCharsAreCRLF(s) then
          Exit
        else
          EditView.GetAttributeAtPos(PrevPos, Element, LineFlag);

        if Element = atWhiteSpace then
          if TestForWhiteSpaceInComment(EditView, PrevPos) then
            Element := atComment
          else
          if TestForWhiteSpaceInString(EditView, PrevPos) then
            Element := atString;

        if not GetReplacementSource(Element, RepTable) then
          Exit;
      finally
        EditView.Release;
      end;

      // Check the replacement table for matches not requiring a
      // whole word match ("AutoCorrect" functionality)
      if dmProofReader.ReplacerActive then
      begin
        // Checking s for needing replacement
        ReplaceItem := dmProofReader.FindReplacement(RepTable, s);
        if NeedsReplacementBegin(ReplaceItem, s) then
        begin
          d := ReplaceItem.Replace;

          CursorOffset := Pos('|', d);
          if CursorOffset > 0 then
          begin
            Delete(d, CursorOffset, 1);
            CursorOffset := CursorOffset - Length(d) - 1;
          end;

          with EI.CreateUndoableWriter do
          try
            CopyTo(EditorPos - Length(ReplaceItem.Typed));
            DeleteTo(EditorPos);
            Insert(PChar(d));
          finally
            Release;
          end;

          n := Length(d);
          Assert(EI.GetViewCount > 0);
          CurrentView := GetCurrentEditView(EI);
          Assert(CurrentView >= 0);

          with EI.GetView(CurrentView) do
          try
            if CPos.CharIndex > 0 then
              Inc(EPos.Col, n - Length(ReplaceItem.Typed) + CursorOffset);
            CursorPos := EPos;
          finally
            Release;
          end;

          BeepOnDemand;

          Delete(s, 1, Length(s) - Length(ReplaceItem.Typed));
          AppendAutoCorrectHistory(RepTable, s, d);

          Exit;
        end;
      end;

      Assert(Length(s) > 0);

      // The rest of the replacement tests replace whole words only
      // and can't possibly match if the last char is alphanumeric
      n := Length(s);
      if s[n] in AlphaNum then
        Exit;
(*

StH:
 FIXME! The original code breaks down with auto-indent = on, since auto-indent
 automatically adds white-space into the editor buffer.

      if LastCharsAreCRLF(s) then
        while (n > 0) and (s[n] = #32) do
          Dec(n);
      if (n > 1) and (s[n] = #10) and (s[n - 1] = #13) then
        Dec(n);
*)
      c := Copy(s, n, Length(s));
      Delete(s, n, Length(s));
      if (Length(s) = 0) or not (s[Length(s)] in AlphaNum) then
        Exit;

      if dmProofReader.ReplacerActive then
      begin
        ReplaceItem := dmProofReader.FindReplacement(RepTable, s);
        if NeedsReplacementEnd(ReplaceItem, s) then
        begin
          d := ReplaceItem.Replace;

          CursorOffset := Pos('|', d);
          if CursorOffset > 0 then
          begin
            Delete(d, CursorOffset, 1);
            // -2 is for the Deleted char and the non-alpha word delimiter
            CursorOffset := CursorOffset - Length(d) - 2; // Offset is negative
          end;

          // Single line
          with EI.CreateUndoableWriter do
          try
            CopyTo(EditorPos - Length(ReplaceItem.Typed) - Length(c));
            DeleteTo(EditorPos);
            Insert(PChar(d + c));
          finally
            Release;
          end;

          n := Length(d);

          Assert(EI.GetViewCount > 0);

          CurrentView := GetCurrentEditView(EI);
          Assert(CurrentView >= 0);

          with EI.GetView(CurrentView) do
          try
            if CPos.CharIndex > 0 then
              Inc(EPos.Col, n - Length(ReplaceItem.Typed) + CursorOffset);
            CursorPos := EPos;
          finally
            Release;
          end;

          BeepOnDemand;

          Delete(s, 1, Length(s) - Length(ReplaceItem.Typed));
          AppendAutoCorrectHistory(RepTable, s, ReplaceItem.Replace);

          Exit;
        end;
      end;

      n := Length(s);
      while (n > 0) and (s[n] in AlphaNum) do
        Dec(n);
      if n = 0 then
        Exit; // May be only a part of the word

      Delete(s, 1, n); // s - the word, c - symbols after word

      if dmProofReader.CompilerActive and
         (RepTable in [rtPasSrc, rtCPPSrc]) then
      begin
        dmProofReader.slKibitz.Clear;
        EditorComponent := EditorForm.FindComponent('Editor');
        Assert(Assigned(EditorComponent), 'No edit control found on ' + EditorForm.Name);
        GetKibitzSymbols(EditorComponent, CPos.CharIndex - Length(c),
                         CPos.Line, dmProofReader.slKibitz);
        d := dmProofReader.FindStringList(dmProofReader.slKibitz, False, s);
        if d <> '' then
        begin
          with EI.CreateUndoableWriter do
          try
            CopyTo(EditorPos - Length(s) - Length(c));
            DeleteTo(EditorPos);
            Insert(PChar(d + c));
          finally
            Release;
          end;

          BeepOnDemand;

          Assert(EI.GetViewCount > 0);

          CurrentView := GetCurrentEditView(EI);

          Assert(CurrentView >= 0);

          with EI.GetView(CurrentView) do
          try
            if CPos.CharIndex > 0 then
              Inc(EPos.Col, Length(d) - Length(s));
            CursorPos := EPos;
          finally
            Release;
          end;

          AppendKibitzHistory(RepTable, s, d);

          Exit;
        end;
      end;

      if dmProofReader.DictionaryActive then
      begin
        d := dmProofReader.FindDictionary(RepTable, s);
        if d <> '' then
        begin
          with EI.CreateUndoableWriter do
          try
            CopyTo(EditorPos - Length(s) - Length(c));
            DeleteTo(EditorPos);
            Insert(PChar(d + c));
          finally
            Release;
          end;

          BeepOnDemand;

          Assert(EI.GetViewCount > 0);

          CurrentView := GetCurrentEditView(EI);
          Assert(CurrentView >= 0);

          with EI.GetView(CurrentView) do
          try
            if CPos.CharIndex > 0 then
              Inc(EPos.Col, Length(d) - Length(s));
            CursorPos := EPos;
          finally
            Release;
          end;

          AppendWordHistory(RepTable, s, d);

          Exit;
        end;
      end;

    finally
      FModifyingSelf := False;
      EI.Release;
    end;

  except
    on E: Exception do
    begin
      ProcessException(E);
      {$IFOPT D+}SendDebug('Proof reader: ' + E.Message); {$ENDIF}
      { swallow }
    end;
  end;
end;

{ TCorrectionHistory }

procedure TCorrectionHistory.Add(Item: TCorrectionItem);
begin
  if Item = nil then
    Exit;

  if Count = MaxHistory then
  begin
    Assert(Count > 0);
    TCorrectionItem(Items[Count - 1]).Free;
    Items.Delete(Count - 1);
  end;
  Items.Insert(0, Item);
end;

constructor TCorrectionHistory.Create;
begin
  inherited Create;

  Items := TList.Create;
  MaxHistory := 25;
end;

destructor TCorrectionHistory.Destroy;
var
  i: Integer;
begin
  if Items <> nil then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      Assert(TObject(Items[i]) is TCorrectionItem);
      TCorrectionItem(Items[i]).Free;
    end;
    Items.Free;
    Items := nil;
  end;

  inherited Destroy;
end;

function TCorrectionHistory.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TCorrectionHistory.GetItem(Index: Integer): TCorrectionItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < Items.Count) then
    Result := TCorrectionItem(Items[Index]);
end;

initialization
{$IFDEF VER110}
  // C++Builder 3.0 does not "like" initialised
  // sets above a given size. At the top of this
  // unit we therefore declared variables and
  // initialise them here.
  shsSource := [atWhiteSpace, atReservedWord, atIdentifier, atSymbol, atNumber,
    atFloat, atOctal, atHex, atCharacter, atIllegal, SyntaxOff, 37];
  AlphaNum := ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
{$ENDIF VER110}

  InitKeySets;

{$ELSE GX_NOBDE}
interface implementation
{$ENDIF GX_NOBDE}

end.


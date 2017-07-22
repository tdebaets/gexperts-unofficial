unit GX_EditReader;

{$I GX_CondDefine.Inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is **not** fully compatible with C++Builder yet,
    //! see comment below for comment reader

{$UNDEF GX_WorkAroundFullBufferSelectBug}

{$IFDEF GX_VER120_up}
  {$DEFINE GX_WorkAroundFullBufferSelectBug}
{$ENDIF GX_VER120_up}

interface

uses
  Classes, EditIntf, ExptIntf, ToolIntf, Dialogs, Windows;

type
  TModuleMode = (mmModule, mmFile);

type
  TEditReader = class(TObject)
  private
    FSourceInterfaceAllocated: Boolean;
    FModuleNotifier: TIModuleNotifier;
    FEditIntf: TIEditorInterface;
    FEditRead: TIEditReader;
    FModIntf: TIModuleInterface;

    FEof: Boolean;
    Buf: PChar;
    BLine: PChar;
    EPos: Integer;
    BSPos: Integer;
    BEPos: Integer;
    FLineNo: Integer;
    FBufSize: Integer;
    FFileName: string;
    FMode: TModuleMode;
    FNoComments: Boolean;
    FUCase: Boolean;
    FNoTabs: Boolean;
    CurlyCommentActive: Boolean;
    StarCommentActive: Boolean;
    CppStarCommentActive: Boolean;

    FSyntaxHighlighter: TSyntaxHighlighter;

    SFile: TStream;

    procedure AllocateFileData;
    function GetLine: string;
    procedure FillBuffer;
    function GetLineCount: Integer;
    procedure SetBufSize(New: Integer);
    function GetModuleSyntaxFromExtension(const FileName: string): TSyntaxHighlighter;
    function GetModuleSyntaxHighlighter: TSyntaxHighlighter;
    function GetEditViewCount: Integer;
    procedure InternalGotoLine(Line: Integer; Offset: Boolean);
  protected
    procedure SetFileName(const Value: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure FreeFileData;
    procedure Reset;
    procedure GotoLine(L: Integer);
    procedure GotoOffsetLine(L: Integer);
    procedure ShowSource;
    procedure ShowForm;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToStreamFromPos(Stream: TStream);
    function GetBlock: string;
    function GetCurrentPos: TEditPos;
    function GetCurrentBufferPos: Integer;
    function GetPrecedingCharacters(Count: Integer): string;
    property EditViewCount: Integer read GetEditViewCount;
  published
    property BufSize: Integer read FBufSize write SetBufSize;
    property Eof: Boolean read FEof;
    property FileName: string read FFileName write SetFileName;
    property Line: string read GetLine;
    property LineCount: Integer read GetLineCount;
    property LineNo: Integer read FLineNo;
    property Mode: TModuleMode read FMode;
    property NoComments: Boolean read FNoComments write FNoComments;
    property NoTabs: Boolean read FNoTabs write FNoTabs;
    property UCase: Boolean read FUCase write FUCase;
    property SyntaxHighlighter: TSyntaxHighlighter read FSyntaxHighlighter;
  end;

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc, SysUtils;

type
  TModuleFreeNotifier = class(TIModuleNotifier)
  private
    FOwner: TEditReader;
  public
    constructor Create(Owner: TEditReader);
    destructor Destroy; override;

    procedure Notify(NotifyCode: TNotifyCode); override;
    {$IFDEF GX_VER140_up}
    procedure ComponentRenamed(const AComponent: TComponent;
      const OldName, NewName: string); override;
    {$ELSE not GX_VER140_up}
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
    {$ENDIF}
  end;

{ TModuleFreeNotifier }

constructor TModuleFreeNotifier.Create(Owner: TEditReader);
begin
  inherited Create;

  FOwner := Owner;
end;

destructor TModuleFreeNotifier.Destroy;
begin
  Assert(FOwner <> nil);
  FOwner.FModuleNotifier := nil;

  inherited Destroy;
end;

{$IFDEF GX_VER140_up}
procedure TModuleFreeNotifier.ComponentRenamed(const AComponent: TComponent;
  const OldName, NewName: string);
{$ELSE not GX_VER140_up}
procedure TModuleFreeNotifier.ComponentRenamed(ComponentHandle: Pointer;
  const OldName, NewName: string);
{$ENDIF}
begin
  // Nothing
end;

procedure TModuleFreeNotifier.Notify(NotifyCode: TNotifyCode);
begin
  case NotifyCode of

    ncModuleDeleted:
      begin
        Assert(FOwner <> nil);
        FOwner.FreeFileData;
      end;

    ncModuleRenamed:
      begin
        //! StH: FIXME
        // we might want to handle this and
        // change the file name we store in
        // the edit reader
        {$IFOPT D+} SendDebugEx('Warning: Module '+FOwner.FileName+' was just renamed with an edit reader open!', mtWarning); {$ENDIF}
      end;

  else
    // nothing
  end;
end;

resourcestring
  SNoEditReader = 'FEditRead: No editor reader interface (you have found a bug!)';

constructor TEditReader.Create(const FileName: string);
begin
  inherited Create;

  FBufSize := 2048;
  FMode := mmModule;
  BLine := StrAlloc(1024);
  if FileName <> '' then
    SetFileName(FileName);
end;

procedure TEditReader.FreeFileData;
begin
  // Use the FreeFileData to release the references
  // to the internal editor buffer or the external
  // file on disk in order not to block the file
  // or track the editor (which may disappear) for
  // possibly extended periods of time.

  // Calls to edit reader will always (re-)allocate
  // references again by calling the "AllocateFileData"
  // method, so calling "FreeFileData" essentially comes
  // for free, only reducing the length a reference
  // is held to an entity.

  SFile.Free;
  SFile := nil;

  FEditRead.Free;
  FEditRead := nil;

  FEditIntf.Free;
  FEditIntf := nil;

  if FModIntf <> nil then
    FModIntf.RemoveNotifier(FModuleNotifier);

  FModuleNotifier.Free;
  FModuleNotifier := nil;

  //! EB: Crash here on shutdown sometimes?  Delphi has freed the ModuleInterface?
  FModIntf.Free;
  FModIntf := nil;

  FSourceInterfaceAllocated := False;
end;

destructor TEditReader.Destroy;
begin
  FreeFileData;

  StrDispose(Buf);
  Buf := nil;

  StrDispose(BLine);
  BLine := nil;

  inherited Destroy;
end;

function TEditReader.GetEditViewCount: Integer;
begin
  AllocateFileData;
  Assert(FEditIntf <> nil);
  Result := FEditIntf.GetViewCount;
end;

procedure TEditReader.AllocateFileData;
resourcestring
  SFileDoesNotExist = 'File %s does not exist';
  SNoEditorInterface = 'FEditRead: No editor interface';
  SNoModuleNotifier = 'TEditReader: Could not get module notifier';

  procedure AllocateFromDisk;
  begin
    if not FileExists(FFileName) then
      raise Exception.CreateFmt(SFileDoesNotExist, [FFileName]);

    FMode := mmFile;
    SFile := TFileStream.Create(FFileName, fmOpenRead + fmShareDenyWrite);
  end;

begin
  if FSourceInterfaceAllocated then
    Exit;

  if ToolServices = nil then
  begin
    AllocateFromDisk;
    Exit;
  end;

  // Get module interface
  Assert(FModIntf = nil);
  {$IFOPT D+} SendDebug('Getting module interface for ' + FFileName); {$ENDIF}
  FModIntf := ToolServices.GetModuleInterface(FFileName);
  if FModIntf = nil then
  begin
    {$IFOPT D+} SendDebug('Module does not exist in the IDE - loading from disk'); {$ENDIF}
    AllocateFromDisk;
  end
  else
  begin
    FMode := mmModule;

    // Leak: {$IFOPT D+} SendDebug('Actually got module interface for ' + FModIntf.GetEditorInterface.FileName); {$ENDIF}
    Assert(FModuleNotifier = nil);
    // Allocate notifier for module
    FModuleNotifier := TModuleFreeNotifier.Create(Self);
    if FModuleNotifier = nil then
    begin
      FModIntf.Free;
      FModIntf := nil;

      raise Exception.Create(SNoModuleNotifier);
    end;
    FModIntf.AddNotifier(FModuleNotifier);

    // Get Editor Interface
    Assert(FEditIntf = nil);

    {$IFDEF GX_BCB}
    if IsH(FFileName) then
      FEditIntf := FModIntf.GetAuxEditorInterface;
    if FEditIntf = nil then
    {$ENDIF GX_BCB}
    FEditIntf := FModIntf.GetEditorInterface;
    {$IFOPT D+} SendDebug('Got FEditIntf'); {$ENDIF}
    if FEditIntf = nil then
    begin
      FModIntf.RemoveNotifier(FModuleNotifier);
      FModuleNotifier.Free;
      FModuleNotifier := nil;

      FModIntf.Free;
      FModIntf := nil;

      // Should we do this instead?
      //FreeFileData;

      // Sometimes causes "Instance of TEditClass has a dangling reference count of 3"
      // Happens in BCB5 when trying to focus a .h when the .dfm is being vewed as text
      // Maybe fixed in 1.0?
      raise Exception.Create(SNoEditorInterface);
    end;

    // Get Reader interface }
    Assert(FEditRead = nil);
    FEditRead := FEditIntf.CreateReader;
    if FEditRead = nil then
    begin
      FModIntf.RemoveNotifier(FModuleNotifier);
      FModuleNotifier.Free;
      FModuleNotifier := nil;

      FModIntf.Free;
      FModIntf := nil;

      FEditIntf.Free;
      FEditIntf := nil;

      raise Exception.Create(SNoEditReader);
    end;
  end;

  FSourceInterfaceAllocated := True;
end;

procedure TEditReader.SetFileName(const Value: string);
resourcestring
  SFileDoesNotExist = 'File %s does not exist';
  SNoEditorInterface = 'FEditRead: No editor interface';

begin
  if CompareText(Value, FFileName) = 0 then
    Exit;

  FreeFileData;

  // Assigning an empty string clears allocation
  if Value = '' then
    Exit;

  FFileName := Value;

  AllocateFileData;

  if FMode = mmFile then
    FSyntaxHighlighter := GetModuleSyntaxFromExtension(FFileName)
  else
    FSyntaxHighlighter := GetModuleSyntaxHighlighter;

  FreeFileData;

  Reset;
end;

procedure TEditReader.SetBufSize(New: Integer);
begin
  if (Buf = nil) and (New <> FBufSize) then
    FBufSize := New;
end;

procedure TEditReader.ShowSource;
begin
  AllocateFileData;
  Assert(FModIntf <> nil);

  if FMode = mmModule then
    FModIntf.ShowSource;
end;

procedure TEditReader.ShowForm;
begin
  AllocateFileData;
  Assert(FModIntf <> nil);

  if FMode = mmModule then
    FModIntf.ShowForm;
end;

procedure TEditReader.GotoLine(L: Integer);
begin
  InternalGotoLine(L, False);
end;

procedure TEditReader.GotoOffsetLine(L: Integer);
begin
  InternalGotoLine(L, True);
end;

procedure TEditReader.InternalGotoLine(Line: Integer; Offset: Boolean);
var
  EditView: TIEditView;
  EditPos: TEditPos;
  S: Integer;
  CurrentView: Integer;
  ViewCount: Integer;
begin
  AllocateFileData;

  //{$IFOPT D+} SendDebug('LineCount ' + IntToStr(LineCount)); {$ENDIF}
  if Line > LineCount then Exit;
  if Line < 1 then
    Line := 1;

  Assert(FModIntf <> nil);
  // We don't try to focus .h files if the module is already the active one
  // Using ShowSource opens the .cpp and obscures the .h
  {$IFDEF GX_BCB} if not (IsH(FFileName) and (ExtractPureFileName(ToolServices.GetCurrentFile) = ExtractPureFileName(FFileName))) then {$ENDIF GX_BCB}
  if not FModIntf.ShowSource then
    Exit;

  Assert(FEditIntf <> nil);
  ViewCount := FEditIntf.GetViewCount;

  // We don't know how to focus a .h file except "Opening" it
  {$IFDEF GX_BCB}
  if IsH(FFileName) then
  begin
    ToolServices.OpenFile(FFileName);
    // This edit interface should still be valid afer "opening"
    ViewCount := FEditIntf.GetViewCount;
  end;
  {$ENDIF GX_BCB}

  if ViewCount < 1 then
    Exit;

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  if EditView <> nil then
  try
    EditPos.Col := 1;
    EditPos.Line := Line;
    if Offset then
    begin
      EditView.CursorPos := EditPos;
      S := Line - (EditView.ViewSize.cy div 2);
      if S < 1 then S := 1;
      EditPos.Line := S;
      EditView.TopPos := EditPos;
    end
    else
    begin
      EditView.TopPos := EditPos;
      EditView.CursorPos := EditPos;
      // As above, we don't try to set focus to .h files, since it doesn't work (if this necessary?)
      {$IFDEF GX_BCB}
      if not (IsH(FFileName) and (ExtractPureFileName(ToolServices.GetCurrentFile) = ExtractPureFileName(FFileName))) then
      {$ENDIF GX_BCB}
      FModIntf.ShowSource;
    end;
  finally
    EditView.Free;
  end;
end;

procedure TEditReader.FillBuffer;
var
  i: Integer;
begin
  if Buf = nil then
    Buf := StrAlloc(BufSize);
  Buf[0] := #0;

  AllocateFileData;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);
    BEPos := SFile.Read(Buf^, BufSize - 1);
  end
  else
  begin
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);
    BEPos := FEditRead.GetText(EPos, Buf, BufSize - 1);
  end;

  BSPos := 0; // Reset internal buffer position to zero
  FEof := (BEPos = 0);
  if BEPos = BufSize - 1 then
  begin
    Assert((Mode <> mmFile) or (SFile <> nil));
    for i := BEPos downto 1 do
    begin
      if Buf[i] = #10 then
      begin
        if Mode = mmFile then
          SFile.Position := SFile.Position - (BEPos - i) + 1;
        BEPos := i;
        EPos := EPos + i + 1;
        Buf[i + 1] := #0;
        Break;
      end;
    end;
  end
  else
    EPos := EPos + BEPos + 1;
end;

function TEditReader.GetLine: string;

    function BlockCommentActive: Boolean;
    begin
      if SyntaxHighlighter = shPascal then
        Result := CurlyCommentActive or StarCommentActive
      else
        Result := CppStarCommentActive;
    end;

var
  i: Integer;
  t: Integer;
  LPos: Integer;
  AddChar: Boolean;
begin
  //{$IFOPT D+} SendDebug('TEditReader.GetLine'); {$ENDIF}
  LPos := 0;
  Result := '';
  // Read new data
  if (BSPos >= BEPos) or (BEPos = 0) then
    FillBuffer;
  if FEof then
  begin
    {$IFDEF GX_WorkAroundFullBufferSelectBug}
      Inc(FLineNo);
    {$ENDIF GX_WorkAroundFullBufferSelectBug}
    Exit;
  end;

  for i := BSPos to BEPos - 1 do
  begin
    AddChar := False;
    case Buf[i] of
      '/':
        begin
          case SyntaxHighlighter of

            shPascal:
              begin
                // Pascal comment --> //
                if FNoComments and not BlockCommentActive then
                begin
                  if Buf[i + 1] = '/' then
                  begin
                    t := i;
                    while (t <= BEPos - 1) and (Buf[t] <> #13) and (Buf[t] <> #0) do
                      Inc(t);
                    BSPos := t + 1;
                    if t < BEPos - 1 then
                      if Buf[BSPos] = #10 then
                        Inc(BSPos);
                    BLine[LPos] := #0;
                    Inc(LPos);
                    Break;
                  end
                  else
                    if not BlockCommentActive then AddChar := True;
                end
                else
                  if not BlockCommentActive then AddChar := True;
              end;

          {$IFDEF GX_VER110_up}
            shC:
              begin
                // C and C++ style comments --> /**/   AND  //
                if FNoComments and not BlockCommentActive then
                begin
                  if Buf[i + 1] = '/' then
                        begin
                          t := i;
                          while (t <= BEPos - 1) and (Buf[t] <> #13) and (Buf[t] <> #0) do
                            Inc(t);
                          BSPos := t + 1;
                          if t < BEPos - 1 then
                            if Buf[BSPos] = #10 then
                              Inc(BSPos);
                          BLine[LPos] := #0;
                          Inc(LPos);
                          Break;
                  end
                  else
                        begin
                    if FNoComments and (Buf[i + 1] = '*') then
                            CppStarCommentActive := True
                          else
                      if not BlockCommentActive then
                      AddChar := True;
                  end
                    end
                    else
                  if not BlockCommentActive then AddChar := True;
                  end;
          {$ENDIF GX_VER110_up}
          else
            AddChar := True;
          end;
        end;

      '{':
        begin
          if SyntaxHighlighter = shPascal then
          begin
            if FNoComments and not StarCommentActive then
              CurlyCommentActive := True
            else
              if not BlockCommentActive then
                AddChar := True;
          end
          else
            AddChar := True;
        end;

      '}':
        begin
          if SyntaxHighlighter = shPascal then
          begin
            if FNoComments and not StarCommentActive then
              CurlyCommentActive := False
            else
              if not BlockCommentActive then
                AddChar := True;
          end
          else
            AddChar := True;
        end;

      '(':
        begin
          if SyntaxHighlighter = shPascal then
          begin
            if FNoComments and (Buf[i + 1] = '*') and not CurlyCommentActive then
              StarCommentActive := True
            else
              if not BlockCommentActive then
                AddChar := True;
          end
          else
            AddChar := True;
        end;

      ')':
        begin
          if SyntaxHighlighter = shPascal then
          begin
            if (i > 1) and (FNoComments) and (Buf[i - 1] = '*') and not CurlyCommentActive then
              StarCommentActive := False
            else
              if not BlockCommentActive then
                AddChar := True;
          end
          else
            AddChar := True;
        end;

      {$IFDEF GX_VER110_up}
      '*':
        begin
          if SyntaxHighlighter = shC then
          begin
            if (i > 1) and (FNoComments) and (Buf[i + 1] = '/') and CppStarCommentActive then
              CppStarCommentActive := False
            else
              if not BlockCommentActive then
                AddChar := True;
          end
          else
            AddChar := True;
        end;
      {$ENDIF GX_VER110_up}

      #13:
        begin
          BSPos := i + 1;
          if Buf[BSPos] = #10 then Inc(BSPos);
          Break;
        end;

      #10:
        begin
          BSPos := i + 1;
          Break;
        end;

      #0:
        begin
          BSPos := BEPos + 1;
          Break;
        end;

    else // case
      if not BlockCommentActive then
        AddChar := True;
    end; // case

    if AddChar then
    begin
      BLine[LPos] := Buf[i];
      if UCase then
        CharUpperBuff(PChar(Integer(BLine) + LPos), 1);
      if NoTabs and (BLine[LPos] = #9) then
        BLine[LPos] := #32;
      Inc(LPos);
    end;
  end;
  if BSPos < i then
    BSPos := i;
  if LPos <> 0 then
  begin
    BLine[LPos] := #0;
    Result := StrPas(BLine);
  end;
  if Buf[i] <> #0 then
    Inc(FLineNo);
end;

function TEditReader.GetLineCount: Integer;
begin
  if FMode = mmModule then
  begin
    AllocateFileData;
    Assert(FEditIntf <> nil);

    Result := FEditIntf.LinesInBuffer;
  end
  else
    Result := -1;
end;

procedure TEditReader.Reset;
begin
  {$IFOPT D+} SendDebug('In Reset'); {$ENDIF}
  EPos := 0;
  BSPos := 0;
  BEPos := 0;
  FLineNo := 0;
  FEof := False;
  CurlyCommentActive := False;
  StarCommentActive := False;
  CppStarCommentActive := False;
  if FMode = mmFile then
  begin
    // We do not need to allocate file data
    // in order to set the stream position
    if SFile <> nil then
      SFile.Position := 0;
  end;
  {$IFOPT D+} SendDebug('Out of Reset'); {$ENDIF}
end;

procedure TEditReader.SaveToStream(Stream: TStream);
var
  Pos: Integer;
  Size: Integer;
const
  TheEnd: Char = #0; // Leave typed constant as is - needed for streaming code
begin
  Assert(Stream <> nil);

  Reset;

  AllocateFileData;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);

    SFile.Position := 0;
    Stream.CopyFrom(SFile, SFile.Size);
    Stream.Write(TheEnd, 1);
  end
  else
  begin
    Pos := 0;
    if Buf = nil then
      Buf := StrAlloc(BufSize + 1);
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);
    // Delphi 4+ sometimes returns -1 here, for an unknown reason
    Size := FEditRead.GetText(Pos, Buf, BufSize);
    if Size = -1 then
    begin
      FreeFileData;
      AllocateFileData;
      Size := FEditRead.GetText(Pos, Buf, BufSize);
    end;
    if Size > 0 then
    begin
      Pos := Pos + Size;
      while Size = BufSize do
      begin
        Stream.Write(Buf^, Size);
        Size := FEditRead.GetText(Pos, Buf, BufSize);
        Pos := Pos + Size;
      end;
      Stream.Write(Buf^, Size);
    end;
    Stream.Write(TheEnd, 1);
  end;
end;

function TEditReader.GetCurrentBufferPos: Integer;
var
  EditorPos: TEditPos;
  CharPos: TCharPos;
  EditView: TIEditView;
  CurrentView: Integer;
begin
  AllocateFileData;

  Assert(FEditIntf <> nil);

  Result := -1; //! Reactivated
  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  if EditView <> nil then
  try
    EditorPos := EditView.CursorPos;
    EditView.ConvertPos(True, EditorPos, CharPos);
    Result := EditView.CharPosToPos(CharPos);
  finally
    EditView.Free;
  end;
end;

procedure TEditReader.SaveToStreamFromPos(Stream: TStream);
var
  Pos: Integer;
  Size: Integer;
begin
  AllocateFileData;

  Reset;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);
    SFile.Position := 0;
    Stream.CopyFrom(SFile, SFile.Size);
  end
  else
  begin
    Pos := GetCurrentBufferPos;
    if Buf = nil then
      Buf := StrAlloc(BufSize);
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);
    Size := FEditRead.GetText(Pos, Buf, BufSize);
    if Size > 0 then
    begin
      Pos := Pos + Size;
      while Size = BufSize do
      begin
        Stream.Write(Buf^, Size);
        Size := FEditRead.GetText(Pos, Buf, BufSize);
        Pos := Pos + Size;
      end;
      Stream.Write(Buf^, Size);
    end;
  end;
end;

function TEditReader.GetBlock: string;

  procedure GetBlock;
  var
    TempString: string;
    L: Integer;
    SPos: Integer;
    EPos: Integer;
  begin
    Result := '';
    L := 0;

    AllocateFileData;

    Assert(FEditRead <> nil);
    Assert(FEditIntf <> nil);

    with FEditRead, FEditIntf do
    begin
      while L < BlockStart.Line - 1 do
      begin
        Self.Line;
        L := Self.LineNo;
      end;
      //GotoLine(BlockStart.Line);
      while L <= BlockAfter.Line - 1 do
      begin
        TempString := Self.Line;
        L := LineNo;
        SPos := 1;
        EPos := Length(TempString);
        if (L = BlockStart.Line) and (BlockType <> btLine) then
          SPos := BlockStart.CharIndex + 1;
        if (L = BlockAfter.Line) and (BlockType <> btLine) then
        begin
          case BlockType of
            btInclusive:    EPos := BlockAfter.CharIndex + 1;
            btNonInclusive: EPos := BlockAfter.CharIndex;
          end;
        end;
        Result := Result + Copy(TempString, SPos, EPos - SPos + 1);
        if L < BlockAfter.Line then
          Result := Result + #13#10;
      end;
    end;
  end;

  procedure GetColumnBlock;
  begin
    // not implemented
  end;

begin
  Result := '';
  if FFileName = '' then Exit;
  if FMode <> mmModule then Exit;

  AllocateFileData;
  Assert(FEditIntf <> nil);

  if (FEditIntf.BlockStart.CharIndex = FEditIntf.BlockAfter.CharIndex) and
     (FEditIntf.BlockStart.Line = FEditIntf.BlockAfter.Line) then
  begin
    Exit;
  end;

  if FEditIntf.BlockType = btColumn then
    GetColumnBlock
  else
    GetBlock;
end;

function TEditReader.GetCurrentPos: TEditPos;
var
  EditView: TIEditView;
  CurrentView: Integer;
begin
  AllocateFileData;
  Assert(FEditIntf <> nil);

  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  if EditView <> nil then
  try
    Result := EditView.GetPos(0);
  finally
    EditView.Free;
  end;
end;

function TEditReader.GetModuleSyntaxHighlighter: TSyntaxHighlighter;
begin
  AllocateFileData;
  Assert(FEditIntf <> nil);

  Result := FEditIntf.SetSyntaxHighlighter(shQuery);
end;

function TEditReader.GetModuleSyntaxFromExtension(const FileName: string): TSyntaxHighlighter;
var
  FileExt: string;
begin
  Result := shNone;

  FileExt := ExtractUpperFileExt(FileName);
  if (FileExt = '.PAS') or (FileExt = '.DPR') or (FileExt = '.DPK') then
    Result := shPascal
{$IFNDEF VER100}
  else
  if (FileExt = '.CPP') or (FileExt = '.HPP') or (FileExt = '.C') or (FileExt = '.H') or
     (FileExt = '.BPK') or (FileExt = '.HXX') or (FileExt = '.CXX') then
  begin
    Result := shC;
  end;
{$ENDIF VER100}
end;

function TEditReader.GetPrecedingCharacters(Count: Integer): string;
var
  CurrentView: Integer;
  EditView: TIEditView;

  CurrentCharPos: TCharPos;
  CurrentEditPos: TEditPos;
  Position: Longint;
begin
  Assert(Count >= 0);
  AllocateFileData;
  Assert(FEditIntf <> nil);

  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  try
    Assert(EditView <> nil);
    CurrentEditPos := EditView.GetPos(0);
    EditView.ConvertPos(True, CurrentEditPos, CurrentCharPos);
    Position := EditView.CharPosToPos(CurrentCharPos);

    // If there do not exist at least Count characters
    // before the cursor, we can not retrieve them
    if Position < Count then
    begin
      Count := Position;
      Position := 0;
    end
    else
      Position := Position - Count;

    SetLength(Result, Count);

    Assert(FEditRead <> nil);
    Count := FEditRead.GetText(Position, PChar(Result), Count);
    SetLength(Result, Count);
  finally
    EditView.Free;
  end;
end;

end.


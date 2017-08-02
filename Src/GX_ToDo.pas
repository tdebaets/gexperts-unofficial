unit GX_ToDo;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is compatible with C++Builder
//!   Note, though, that non-Pascal source files will not be parsed

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Buttons, GX_EditReader, mPasLex, ToolIntf, ExptIntf,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  EditIntf, GX_Experts, Registry, StdCtrls, Menus, GX_IdeDock;

const
  UM_RESIZECOLS = WM_USER + 523;

type
  // TODO -oAnyone -CFeature : Add tpDone (completed) type and display in grey
  TToDoPriority = (tpHigh, tpMed, tpLow);

  TTokenInfo = class(TObject)
  private
    FToken: string;
    FPriority: TToDoPriority;
  public
    property Token: string read FToken write FToken;
    property Priority: TToDoPriority read FPriority write FPriority;
  end;

  TTokenList = class(TStringList)
  private
    procedure LoadFromRegistry(const KeyName: string);
    procedure SaveToRegistry(const KeyName: string);
    procedure AddToken(Token: string; Priority: TToDoPriority);
  public
    destructor Destroy; override;
  end;

  TDirList = class(TStringList)
    procedure LoadFromRegistry(const KeyName: string);
    procedure SaveToRegistry(const KeyName: string);
  end;

  TToDoInfo = class(TObject)
  private
    NumericPriority: Cardinal;
    Owner: string;
    ToDoClass: string;
    //
    Priority: TToDoPriority;
    Raw: string;
    Display: string;
    FileName: string;
    LineNo: Integer;
  end;

  TToDoNotifier = class;

  TToDoScanType = (tstProject, tstOpenFiles, tstDirectory);

  TToDoExpert = class;

  TfmToDo = class(TfmIdeDockForm)
    pnlToolbar: TPanel;
    StatusBar: TStatusBar;
    ilToDo: TImageList;
    lvToDo: TListView;
    btnCancel: TButton;
    sbRefresh: TSpeedButton;
    sbGoto: TSpeedButton;
    sbPrint: TSpeedButton;
    sbConfigure: TSpeedButton;
    sbHelp: TSpeedButton;
    Popup: TPopupMenu;
    mitGoto: TMenuItem;
    mitRefresh: TMenuItem;
    mitPrint: TMenuItem;
    mitConfiguration: TMenuItem;
    mitCopyToClipboard: TMenuItem;
    procedure sbRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sbGotoClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure lvToDoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvToDoColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvToDoEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lvToDoCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure sbConfigureClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure mitCopyToClipboardClick(Sender: TObject);
  private
    First: Boolean;
    // Sorted list of all filenames to scan (in uppercase)
    // The objects contain the filenames in original case (as string)
    FQueuedFileList: TStringList;
    FDataList: TList;
    EditRead: TEditReader;
    Notifier: TToDoNotifier;
    FSortAscending: Boolean;
    FColumnIndex: Integer;
    function GetSelectedItem: TToDoInfo;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
    procedure EnqueueFile(Filename: string);
    procedure ClearQueuedFiles;
    procedure EnqueueProjectUses;
  protected
    procedure EnumerateFilesByDirectory;
    procedure SaveSettings;
    procedure LoadSettings;
  public
    procedure LoadFile(FileName: string);
    procedure Clear;
  end;

  TToDoNotifier = class(TIAddInNotifier)
  private
    FToDoForm: TfmToDo;
  public
    constructor Create(Owner: TfmToDo);
    destructor Destroy; override;

    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
  end;

  TToDoExpert = class(TGX_EnhExpert)
  private
    FScanType: TToDoScanType;
    FScanProjUses: Boolean;
    FDirToScan: string;
    FRecurseDirScan: Boolean;
    FTokenList: TTokenList;
    FShowTokens: Boolean;
    FAddMessage: Boolean;
    FHideOnGoto: Boolean;
    FDirList: TDirList;
    function GetDirList: TStrings;
    procedure SetDirList(New: TStrings);
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Configure; override;
    property DirList: TStrings read GetDirList write SetDirList;
  end;

var
  fmToDo: TfmToDo;
  ToDoExpert: TToDoExpert;

implementation

{$R *.DFM}
{$R ToDo.res}

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_GExperts, GX_ConfigurationInfo, GX_GenFunc,
  {$IFDEF GX_UseNativeToolsApi}
  ToolsAPI, GX_OtaUtils,
  {$ENDIF GX_UseNativeToolsApi}
  GX_ToDoOptions, ClipBrd, TypInfo;

resourcestring
  SParsingError = 'A parsing error occurred in file %s.';

{ TTokenList }

procedure TTokenList.LoadFromRegistry(const KeyName: string);
resourcestring
  SWhiteSpaceWarning = 'GExperts found the "%s" To Do List token with leading and/or trailing spaces.'#13 +
    #13 +
    'Use of such tokens is deprecated; please correct your list of tokens ' +
    'in the To Do List configuration dialog to take this into account.';

  SLeadingDollarWarning = 'GExperts found the "%s" To Do List token with a leading $ character.'#13 +
    #13 +
    'Use of such tokens is no longer allowed.'#13 +
    #13 +
    'Please correct your list of tokens in the To Do List configuration dialog ' +
    'as soon as possible to take this into account.';
var
  i: Integer;
  TokenInfo: TTokenInfo;
  TempTokenText: string;
begin
  // do not localize any of the following
  with TRegIniFile.Create(KeyName) do
  try
    ReadSection('Tokens', Self);
    for i := 0 to Count - 1 do
    begin
      // Sanity checks of tokens
      TempTokenText := Self[i];
      if TempTokenText <> Trim(TempTokenText) then
      begin
        MessageDlg(Format(SWhiteSpaceWarning, [TempTokenText]), mtWarning, [mbOK], 0);
        TempTokenText := Trim(TempTokenText);
      end;

      if (Length(TempTokenText) > 0) and (TempTokenText[1] = '$') then
      begin
        MessageDlg(Format(SLeadingDollarWarning, [TempTokenText]), mtWarning, [mbOK], 0);
      end;

      TokenInfo := TTokenInfo.Create;
      TokenInfo.Token := Self[i];
      TokenInfo.Priority := TToDoPriority(ReadInteger('Tokens', TokenInfo.Token, 1));
      Objects[i] := TokenInfo;
    end;
    if Count = 0 then
    begin
      // No tokens found, create a default list of tokens
      AddToken('#ToDo1', tpHigh);
      AddToken('#ToDo2', tpMed);
      AddToken('#ToDo3', tpLow);
    end;
  finally
    Free;
  end;
end;

procedure TTokenList.SaveToRegistry(const KeyName: string);
var
  i: Integer;
begin
  // do not localize any of the below items
  with TRegIniFile.Create(KeyName) do
  try
    EraseSection('Tokens');
    for i := 0 to Count - 1 do
      WriteInteger('Tokens', Self[i], Ord(TTokenInfo(Objects[i]).Priority));
  finally
    Free;
  end;
end;

procedure TTokenList.AddToken(Token: string; Priority: TToDoPriority);
var
  TokenInfo: TTokenInfo;
begin
  TokenInfo := TTokenInfo.Create;
  TokenInfo.Token := Token;
  TokenInfo.Priority := Priority;
  AddObject(Token, TokenInfo);
end;

destructor TTokenList.Destroy;
var
  i: Integer;
begin
  //{$IFOPT D+} SendDebug('TTokenList.Destroy'); {$ENDIF}
  for i := Count - 1 downto 0 do
    Objects[i].Free;
  inherited Destroy;
end;

{ TToDoNotifier }

constructor TToDoNotifier.Create(Owner: TfmToDo);
begin
  inherited Create;
  FToDoForm := Owner;
end;

destructor TToDoNotifier.Destroy;
begin
  {$IFOPT D+}SendDebug('TToDoNotifier.Destroy'); {$ENDIF}
  FToDoForm := nil;
  inherited Destroy;
end;

procedure TToDoNotifier.EventNotification(NotifyCode: TEventNotification;
  var Cancel: Boolean);
begin
  // nothing (avoids abstract warnings)
end;

procedure TToDoNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of

    fnProjectOpened:
      begin
        FToDoForm.Clear;
        if FToDoForm.Visible then
          FToDoForm.sbRefreshClick(FToDoForm.sbRefresh)
        else
          FToDoForm.First := True;
      end;

    fnProjectClosing:
      begin
        FToDoForm.Clear;
      end;

    fnFileOpened:
      begin
        //! StH: Check whether this is a DESIRABLE thing
        //! This will notify of any file loading, not only
        //! when adding to the project file...
        //if FToDoForm.FScanType = tstOpenFiles then
        //  FToDoForm.LoadFile(FileName);
        //  FToDoForm.First := True;
      end;
  end;
end;

{#todo1 make sure this works}

procedure TfmToDo.Clear;
var
  i: Integer;
begin
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
      TToDoInfo(FDataList.Items[i]).Free;
    FDataList.Clear;
  end;
  lvToDo.Items.Clear;
end;

function EnumUnits(Param: Pointer; const FileName, Unitname, FormName: string): Boolean; stdcall;
var
  Form: TfmToDo;
begin
  Result := True;
  try
    if not IsKnownSourceFile(FileName) then
      Exit;
    Form := TfmToDo(Param);
    if (ToDoExpert.fScanType = tstOpenFiles) and
      (not ToolServices.IsFileOpen(FileName)) then
    begin
      Exit;
    end;
    Form.EnqueueFile(FileName);
  except
    on E: Exception do
    begin
      ShowExceptionErrorMessage(E);
      Result := False;
    end;
  end;
end;

{#todo2 test this carefully}

procedure TfmToDo.sbRefreshClick(Sender: TObject);
var
  i: Integer;
  Filename: string;
  AListItem: TListItem;
begin
  Screen.Cursor := crHourglass;
  try
    Clear;
    ClearQueuedFiles;
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create('');
    try
      // Collect files to scan
      case ToDoExpert.FScanType of
        tstProject, tstOpenFiles:
          begin
          // scan project files
            ToolServices.EnumProjectUnits(EnumUnits, Pointer(Self));
            if (ToDoExpert.FScanType = tstProject) and ToDoExpert.FScanProjUses then
              EnqueueProjectUses;
          end;

        tstDirectory:
          begin
            // if expert is instructed to process files by directory
            if Trim(ToDoExpert.FDirToScan) <> '' then
              EnumerateFilesByDirectory;
          end;
      end;
      // Now actually scan the collected files
      lvToDo.Items.BeginUpdate;
      try
        for i := 0 to FQueuedFileList.Count - 1 do
        begin
          Filename := string(FQueuedFileList.Objects[i]);
          try
            LoadFile(Filename);
          except
            on E: Exception do
            begin
              ProcessException(E);
              MessageDlg(Format(SParsingError, [Filename]), mtError, [mbOK], 0);
            end;
          end;
        end;
      finally
        lvToDo.Items.EndUpdate;
      end;
    finally
      EditRead.Free;
      EditRead := nil;
    end;
  finally
    Screen.Cursor := crDefault;
    StatusBar.SimpleText := '';
    lvToDo.AlphaSort;
    if lvToDo.Items.Count > 0 then
    begin
      AListItem := lvToDo.Items[0];
      lvToDo.Selected := AListItem;
      lvToDo.ItemFocused := AListItem;
    end
    else
      sbGoto.Enabled := False;
{P    lvToDo.SetFocus;  // FIXME
    gives a Cannot focus a disabled or invisble window error, has something to
    do with IDE docking }
  end;
  sbPrint.Enabled := lvToDo.Items.Count > 0;
end;

{#todo3 another test}

procedure TfmToDo.EnqueueFile(Filename: string);
begin
  FQueuedFileList.AddObject(UpperCase(Filename), RefString(Filename));
end;

procedure TfmTodo.ClearQueuedFiles;
begin
  while FQueuedFileList.Count > 0 do
  begin
    ReleaseString(FQueuedFileList.Objects[0]);
    FQueuedFileList.Delete(0);
  end;
end;

procedure TfmToDo.EnqueueProjectUses;
var
  ProjectFilename: string;
  MS: TMemoryStream;
  Parser: TmwPasLex;
  nUses: Integer;
  UnitName, UnitFilename: string;
begin
  nUses := 0;
  
  ProjectFilename := ToolServices.GetProjectName;
  if Trim(ProjectFilename) = '' then
    Exit;

  // Based on code in GX_ProjDepend.LoadFileDepend
  Assert(EditRead <> nil);
  EditRead.FileName := ProjectFilename;
  MS := TMemoryStream.Create;
  try
    EditRead.SaveToStream(MS);
    Parser := TmwPasLex.Create;
    try
      Parser.Origin := MS.Memory;
      {$IFOPT D+}SendDebug('First: '+Parser.Token+'('+GetEnumName(TypeInfo(TTokenKind), Integer(Parser.TokenID))+')');{$ENDIF}
      while not (Parser.TokenID in [tkUnit, tkNull, tkLibrary, tkProgram]) do
      begin
        Parser.NextNoJunk;
        {$IFOPT D+}SendDebug('Looking for Unit, found: '+Parser.Token);{$ENDIF}
      end;
      {$IFOPT D+}SendDebug('L/P/U Found: '+Parser.Token);{$ENDIF}
      if Parser.TokenID in [tkUnit, tkLibrary, tkProgram] then
      begin
        Parser.NextNoJunk;
        {$IFOPT D+}SendDebug('ID is: '+Parser.Token+'('+GetEnumName(TypeInfo(TTokenKind), Integer(Parser.TokenID))+')');{$ENDIF}
        if Parser.TokenID <> tkIdentifier then
          Exit;
      end;
      while Parser.TokenID <> tkNull do
      begin
        if Parser.TokenID = tkUses then
        begin
          Inc(nUses);
          Parser.NextNoJunk;
          while (Parser.TokenID <> tkSemiColon) and (Parser.TokenID <> tkNull) do
          begin
            UnitName := '';
            if Parser.TokenID = tkIdentifier then
            begin
              {$IFOPT D+}SendDebug('Unit found: '+Parser.Token);{$ENDIF}
              UnitName := Parser.Token;
            end;
            Parser.NextNoJunk;
            if Parser.TokenID = tkIn then
            begin
              // Units with 'in' were already collected in EnumUnits, so ignore those
              UnitName := '';
              while not (Parser.TokenID in [tkSemiColon, tkComma, tkNull]) do
                Parser.NextNoJunk;
            end;
            if Trim(UnitName) <> '' then
            begin
              // TODO: there's probably a better way than to just append .pas
              UnitFilename := GxOtaFindPathToFile(UnitName + '.pas');
              {$IFOPT D+}SendDebug('Unit filename: '+UnitFilename);{$ENDIF}
              if (Trim(UnitFilename) <> '') and FileExists(UnitFilename) then
                EnqueueFile(UnitFilename);
            end;
          end;
        end;
        // Don't scan entire unit (for now)
        if nUses >= 2 then
          Break;
        Parser.NextNoJunk;
      end;
    finally
      Parser.Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TfmToDo.LoadFile(FileName: string);
var
  Parser: TmwPasLex;
  EStream: TMemoryStream;
  Buf: array[0..30] of Char;

  //! StH: what are SComment and EComment passed in for?
  //! If we want trimming, then a Boolean variable should
  //! be passed in, clearly saying so.

  procedure ParseComment(const SComment, EComment: string);
  var
    i, j, k, n, m: Integer;
    Info: TToDoInfo;
    TokenString: string;
    ParsingString: string;
    ColonString: string;
    OptionChar: Char;
    CListItem: TListItem;
  begin
    TokenString := Parser.Token;

    for i := 0 to ToDoExpert.FTokenList.Count - 1 do
    begin
      n := CaseInsensitivePos(ToDoExpert.FTokenList[i], TokenString);
      if n > 1 then
      begin
        // We found a token that looks like a TODO comment. Now
        // verify that it *is* one: either a white-space or the
        // comment token need to be right in front of the TODO item

        // Remove comment characters
        ParsingString := TokenString;
        Delete(ParsingString, 1, Length(SComment));
        // Remove white-space left and right
        ParsingString := Trim(ParsingString);

        if CaseInsensitivePos(ToDoExpert.FTokenList[i], ParsingString) <> 1 then
          Continue;

        // Token found in comment line
        Info := TToDoInfo.Create;

        // Remove token from string
        Delete(ParsingString, 1, Length(ToDoExpert.FTokenList[i]));
        ParsingString := TrimRight(ParsingString);

        // Identifiy numeric priority
        j := 0;
        while j < Length(ParsingString) do
        begin
          if not (ParsingString[j + 1] in ['0'..'9']) then
            Break;
          Inc(j);
        end;
        Info.NumericPriority := StrToIntDef(Copy(ParsingString, 1, j), 0);
        Delete(ParsingString, 1, j);
        ParsingString := TrimLeft(ParsingString);

        { zTODO -oTestCase: -cIssue <-- test case for colon }
        // Delete everything being with a possible trailing colon:
        j := Pos(':', ParsingString);
        if j > 0 then
        begin
          ColonString := Copy(ParsingString, j + 1, Length(ParsingString));
          Delete(ParsingString, j, Length(ParsingString));
        end
        else
        begin
          ParsingString := '';
          ColonString := ParsingString;
        end;

        { zTODO -cSomething -oTestCase: <-- test case for -o switch }
        { zTODO blah -cSomething -oTestCase: <-- this is a deliberately broken item }
        { zTODO -oTestCase -cTest -c switch: <-- test case for -c switch }
        { zTODO -oTestCase -cTest -c switch: <-- another test case for -c switch }
        { zTODO -oTestCase -cTest -zWhoops -c switch: <-- -z switch }
        { zTODO -oTestCase -cTest -z-z-z-z -------------- -c switch: <-- try to break it }
        { zTODO -oTestCase -cTe-st : <-- hyphen test }
        { zTODO -oTestCase
            -cMultiline
            <-- Multiline test }
        // Identify owner of TODO item (-o)
        // Identify class of TODO item (-c)
        OptionChar := #0; // initialize to make compiler happy - redundant
        while Pos('-', ParsingString) > 0 do
        begin
          if Length(ParsingString) > 1 then
          begin
            OptionChar := UpCase(ParsingString[2]);
            Delete(ParsingString, 1, 2);
          end
          else
            Break;

          // find char immediately preceding the next option switch
          j := Pos('-', ParsingString) - 1;
          if j > 0 then
            k := j
          else
            k := Length(ParsingString);

          case OptionChar of
            'O':
              begin
                Info.Owner := Trim(Copy(ParsingString, 1, k));
              end;

            'C':
              begin
                Info.ToDoClass := Trim(Copy(ParsingString, 1, k));
              end;
          end;

          // Delete everything up to, but not including the
          // next option switch
          Delete(ParsingString, 1, j);
        end;

        Info.Raw := TokenString;
        Info.Priority := TTokenInfo(ToDoExpert.FTokenList.Objects[i]).Priority;
        if not ToDoExpert.FShowTokens then
          n := n + Length(ToDoExpert.FTokenList[i]);
        if EComment <> '' then // trim of end-comment token.
          m := CaseInsensitivePos(EComment, TokenString) - 1
        else
          m := Length(TokenString);
        if m < 1 then
          m := Length(TokenString);
        // Prevent multi-line to do notes
        j := Pos(#13, TokenString);
        k := Pos(#10, TokenString);
        if j = 0 then j := 999999;
        if k = 0 then k := 999999;
        m := Min(m, Min(j, k));
        Info.Display := Trim(Copy(TokenString, n, (m - n) + 1));
        // +1 necessary to match IDE's line numbering
        Info.LineNo := Parser.LineNumber + 1;
        Info.Filename := Filename;
        FDataList.Add(Info);
        CListItem := lvToDo.Items.Add;
        ClistItem.SubItems.Add(Info.Display);
        ClistItem.SubItems.Add(ExtractFileName(Info.FileName));
        ClistItem.SubItems.Add(IntToStr(Info.LineNo));
        CListItem.Data := Info;
        CListItem.ImageIndex := Ord(Info.Priority);
        {$IFDEF GX_UseNativeToolsApi}
        // This only works in Delphi 4.0+ and C++Builder 4.0+
        if ToDoExpert.FAddMessage then
          with BorlandIdeServices as IOTAMessageServices do
            AddToolMessage(Info.FileName, Info.Display, ToDoExpert.FTokenList[i], Info.LineNo, 1);
        {$ENDIF GX_UseNativeToolsApi}
        Break; // Comment line parsed, stop searching for more To Do items in that line
      end;
    end;
  end;

var
  IsCPPModule: Boolean;

begin
  StatusBar.SimpleText := ExtractFileName(Filename);
  StatusBar.Repaint;
  {$IFOPT D+}SendDebug('Loading: ' + FileName); {$ENDIF}
  if not IsKnownSourceFile(FileName) then
    Exit;
  IsCPPModule := IsCppSourceModule(FileName);

  Assert(EditRead <> nil);
  EditRead.FileName := FileName;
  EStream := TMemoryStream.Create;
  try
    EditRead.SaveToStream(EStream);
    EStream.Position := EStream.Size;
    FillChar(Buf, 23, 0);
    EStream.WriteBuffer(Buf, 23);
    EStream.Position := 0;
    Parser := TmwPasLex.Create;
    try
      Parser.Origin := EStream.Memory;
      while Parser.TokenID <> tkNull do
      begin
        { TODO -oStefan -cIssue: This needs to be fixed for multiline comments;
          the strategy ought to be to read the complete comment and only then
          start parsing. Also it would be better to move deleting of the comment
          tokens out of the parser }
        case Parser.TokenID of
          tkBorComment: if not IsCPPModule then ParseComment('{', '}');
          tkAnsiComment: if not IsCPPModule then ParseComment('(*', '*)');
          // Slash comments in CPP files should work if they are not in a {}
          tkSlashesComment: ParseComment('//', '');
        end;
        Parser.Next;
      end;
    finally
      Parser.Free;
    end;
  finally
    EStream.Free;
    Update;
  end;
end;

{#todo1 yet another test}

procedure TfmToDo.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  if Notifier <> nil then
  begin
    ToolServices.RemoveNotifier(Notifier);
    Notifier.Free;
    Notifier := nil;
  end;
  Clear;
  ClearQueuedFiles;
  FQueuedFileList.Free;
  FQueuedFileList := nil;
  FDataList.Free;
  FDataList := nil;
  fmToDo := nil;
  inherited;
end;

{#todo5 and yet another test}

procedure TfmToDo.FormCreate(Sender: TObject);
begin
  FDataList := TList.Create;
  ilToDo.ResourceLoad(rtBitmap, 'GX_TODO', clFuchsia);
  First := True;
  FQueuedFileList := TStringList.Create;
  FQueuedFileList.Duplicates := dupIgnore;
  FQueuedFileList.Sorted := True;
  Notifier := TToDoNotifier.Create(Self);
  ToolServices.AddNotifier(Notifier);
  FColumnIndex := -1;
  FSortAscending := True;
  LoadSettings;
  inherited;
end;

{#todo6 finally, one more test}

procedure TfmToDo.FormActivate(Sender: TObject);
begin
  if First then
  begin
    First := False;
    sbRefreshClick(sbRefresh);
  end;
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

function TfmToDo.GetSelectedItem: TToDoInfo;
begin
  Result := nil;
  if lvToDo.Visible then
    if lvToDo.Selected <> nil then
      Result := TToDoInfo(lvToDo.Selected.Data);
end;

procedure TfmToDo.sbGotoClick(Sender: TObject);
var
  Parser: TmwPasLex;
  MemStream: TMemoryStream;
  SelectedItem: TToDoInfo;
  ClosestLineMatch: Integer;
  LineMatchDifference: Integer;
begin
  ClosestLineMatch := -1;
  LineMatchDifference := MaxInt;
  SelectedItem := GetSelectedItem;
  if SelectedItem = nil then Exit;
  if not ToolServices.IsFileOpen(SelectedItem.Filename) then
    ToolServices.OpenFile(SelectedItem.FileName);

  Screen.Cursor := crHourglass;
  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  EditRead := TEditReader.Create(SelectedItem.FileName);
  try
    MemStream := TMemoryStream.Create;
    try
      EditRead.SaveToStream(MemStream);
      Parser := TmwPasLex.Create;
      try
        Parser.Origin := MemStream.Memory;
        while Parser.TokenID <> tkNull do
        begin
          if Parser.TokenID in [tkBorComment, tkAnsiComment, tkSlashesComment] then
            if CompareText(Parser.Token, SelectedItem.Raw) = 0 then
            begin
              // Look for a matching todo comment with the smallest absolute distance
              // from the line where we found the original comment when last scanning
              if (ClosestLineMatch = -1) or (Abs(SelectedItem.LineNo - Parser.LineNumber + 1) <= LineMatchDifference) then
              begin
                ClosestLineMatch := Parser.LineNumber;
                LineMatchDifference := Abs(SelectedItem.LineNo - ClosestLineMatch);
              end;
            end;
          Parser.Next;
          Application.ProcessMessages;
        end;
      finally
        if LineMatchDifference > -1 then
          EditRead.GotoLine(ClosestLineMatch + 1);
        Parser.Free;
      end;
    finally
      MemStream.Free;
    end;
  finally
    EditRead.Free;
    EditRead := nil;
    Screen.Cursor := crDefault;
  end;
  if ToDoExpert.FHideOnGoto then
    Self.Hide;
end;

procedure TfmToDo.lvToDoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  sbGoto.Enabled := (Item <> nil);
end;

procedure TfmToDo.EnumerateFilesByDirectory;
var
  FileMaskList: TStringList;

  procedure DirScan(Dir: string);
  var
    Search: TSearchRec;
    Result: Integer;
    i: Integer;
  begin
    //{$IFOPT D+}SendDebug('DirSearch on:' +Dir+' Mask: '+Mask);{$ENDIF}
    Dir := AddSlash(Dir);

    // First do sub-directories if option is selected
    if ToDoExpert.FRecurseDirScan then
    begin
      Result := FindFirst(Dir + '*.*', faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) <> 0 then
          begin
            if (Search.Name <> '.') and (Search.Name <> '..') then
              DirScan(Dir + Search.Name);
          end;
          Result := FindNext(Search);
        end;
      finally
        FindClose(Search);
      end;
    end;

    for i := 0 to FileMaskList.Count - 1 do
    begin
      Result := FindFirst(Dir + Trim(FileMaskList.Strings[i]), faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) = 0 then
          begin
            EnqueueFile(Dir + Search.Name);
            Application.ProcessMessages;
          end;
          Result := FindNext(Search);
        end;
      finally
        FindClose(Search);
      end;
    end;
  end;

var
  FileMask: string;
  i: Integer;
begin
  {$IFDEF GX_BCB}
  FileMask := '*.cpp;*.hpp;*.pas;*.h';
  {$ELSE}
  FileMask := '*.pas;*.dpr';
  {$ENDIF GX_BCB}

  for i := 1 to Length(FileMask) do
    if FileMask[i] in [';', ','] then
      FileMask[i] := #13;

  FileMaskList := TStringList.Create;
  try
    FileMaskList.Text := FileMask;

    DirScan(ToDoExpert.FDirToScan);
  finally
    FileMaskList.Free;
  end;
end;

procedure TfmToDo.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmToDo.sbPrintClick(Sender: TObject);
var
  RichEdit: TRichEdit;
  i: Integer;
const
  PriorityText: array[tpHigh..tpLow] of string =
    ('High', 'Normal', 'Low');
begin
  if lvToDo.Items.Count = 0 then
    Exit;
  //#TODO2 make this a nicely formatted page
  RichEdit := TRichEdit.Create(Self);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Self;
    RichEdit.Clear;
    RichEdit.Lines.Add('');
    with RichEdit.SelAttributes do
    begin
      Style := [fsBold];
      Size := 14;
    end;

    case ToDoExpert.FScanType of
      tstProject,
        tstOpenFiles:
        RichEdit.Lines.Add('To Do List for Project: ' + ToolServices.GetProjectName);

      tstDirectory:
        RichEdit.Lines.Add('To Do List for Directory: ' + ToDoExpert.FDirToScan);
    end;

    for i := 0 to FDataList.Count - 1 do
      with TToDoInfo(FDataList.Items[i]) do
      begin
        with RichEdit.SelAttributes do
        begin
        (*
          case Priority of
            tpHigh   : Style := [fsBold];
            tpMed : Style := [];
            tpLow    : Style := [fsItalic];
          end;
        *)
          Style := [];
          Size := 10;
        end;
        RichEdit.Lines.Add(ExtractFileName(Filename) + ' (' + IntToStr(LineNo) + ')' +
          #09 + PriorityText[Priority] + #9 + Display);
      end;
    RichEdit.Print('To Do Items');
  finally
    RichEdit.Free;
  end;
end;

procedure TfmToDo.lvToDoEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmToDo.lvToDoColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = FColumnIndex then
    FSortAscending := not FSortAscending
  else
    FColumnIndex := Column.Index;
  lvToDo.AlphaSort;
end;

procedure TfmToDo.lvToDoCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  case FColumnIndex of
    - 1: Compare := 0;
    0: Compare := Item1.ImageIndex - Item2.ImageIndex; // priority
    3: try
        // Odd bug workaround
        if ((Item1.SubItems.Count > 2) and (Item2.SubItems.Count > 2)) then
          Compare := StrToInt(Item1.SubItems[FColumnIndex - 1]) -
            StrToInt(Item2.SubItems[FColumnIndex - 1])
        else
          Compare := 0;
      except
        on E: Exception do
        begin
          Compare := AnsiCompareStr(Item1.SubItems[FColumnIndex - 1],
            Item2.SubItems[FColumnIndex - 1]);
        end;
      end;
  else
    Compare := AnsiCompareStr(Item1.SubItems[FColumnIndex - 1], Item2.SubItems[FColumnIndex - 1]);
  end;
  if not FSortAscending then Compare := -Compare;
end;

procedure TfmToDo.sbConfigureClick(Sender: TObject);
begin
  ToDoExpert.Configure;
end;

procedure TfmToDo.SaveSettings;
begin
  // Do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteBool('ToDo', 'SortAscending', FSortAscending);
    WriteInteger('ToDo', 'SortColumn', FColumnIndex);
    WriteInteger('ToDo', 'Height', Height);
    WriteInteger('ToDo', 'Width', Width);
  finally
    Free;
  end;
end;

procedure TfmToDo.LoadSettings;
begin
  // Do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    FSortAscending := ReadBool('ToDo', 'SortAscending', True);
    FColumnIndex := ReadInteger('ToDo', 'SortColumn', 0);
    Width := ReadInteger('ToDo', 'Width', Width);
    Height := ReadInteger('ToDo', 'Height', Height);
  finally
    Free;
  end;
end;

procedure TfmToDo.FormResize(Sender: TObject);
var
  NewWidth: Integer;
begin
  NewWidth :=
    lvToDo.ClientWidth
    - lvToDo.Columns[0].Width
    - lvToDo.Columns[2].Width
    - lvToDo.Columns[3].Width;

  if NewWidth < 10 then
    NewWidth := 10;

  lvToDo.Columns[1].Width := NewWidth;
end;

procedure TfmToDo.PopupPopup(Sender: TObject);
begin
  mitConfiguration.Enabled := sbConfigure.Enabled;
  mitPrint.Enabled := sbPrint.Enabled;
  mitRefresh.Enabled := sbRefresh.Enabled;
  mitGoto.Enabled := sbGoto.Enabled;
end;

procedure TfmToDo.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 24);
end;

procedure TfmToDo.mitCopyToClipboardClick(Sender: TObject);
var
  ClipText: TStringList;
  i: Integer;
begin
  ClipText := TStringList.Create;
  try
    for i := 0 to lvToDo.Items.Count - 1 do
    begin
      ClipText.Add(IntToStr(lvToDo.Items[i].ImageIndex) + #9 +
        lvToDo.Items[i].SubItems[0] + #9 +
        lvToDo.Items[i].SubItems[1] + #9 +
        lvToDo.Items[i].SubItems[2]);
    end;
    Clipboard.AsText := ClipText.Text;
  finally
    ClipText.Free;
  end;
end;

procedure TfmToDo.UMResizeCols(var Msg: TMessage);
begin
  FormResize(Self);
end;


{ TToDoExpert }

procedure TToDoExpert.Click(Sender: TObject);
begin
  if fmToDo = nil then
    fmToDo := TfmToDo.Create(nil);
  IdeDockManager.ShowForm(fmToDo);
end;

// This process needs to be cleaned up.  Why store the configuration
// settings in the expert and the form?  It only makes updating both
// copies harder.
procedure TToDoExpert.Configure;

  procedure AddString(Text: string; List: TStrings);
  begin
    if Trim(Text) = '' then Exit;
    if Length(Text) > 100 then Exit;
    if Text[Length(Text)] = '\' then
      Delete(Text, Length(Text), 1);
    if List.IndexOf(Text) >= 0 then
      List.Delete(List.IndexOf(Text));

    if List.Count = 0 then
      List.Add(Text)
    else
      List.Insert(0, Text);

    if List.Count > 20 then
      List.Delete(List.Count - 1);
  end;

var
  Dlg: TfmToDoOptions;
begin
  try
    Dlg := TfmToDoOptions.Create(nil);
    try
      Dlg.lstTokens.Items.Assign(FTokenList);
      Dlg.cbShowTokens.Checked := FShowTokens;
      Dlg.cbAddMessage.Checked := FAddMessage;
      Dlg.cbHideOnGoto.Checked := FHideOnGoto;
      Dlg.cboDirectory.Items.Assign(DirList);
      case FScanType of
        tstProject:
          Dlg.radScanProj.Checked := True;
        tstOpenFiles:
          Dlg.radScanOpen.Checked := True;
        tstDirectory:
          Dlg.radScanDir.Checked := True;
      end;
      Dlg.chkInclProjUses.Checked := FScanProjUses;
      Dlg.chkInclude.Checked := FRecurseDirScan;
      Dlg.cboDirectory.Text := FDirToScan;
      if Dlg.ShowModal = mrOK then
      begin
        // Add directory to DirList
        AddString(Dlg.cboDirectory.Text, DirList);
        FTokenList.Assign(Dlg.lstTokens.Items);
        FShowTokens := Dlg.cbShowTokens.Checked;
        FAddMessage := Dlg.cbAddMessage.Checked;
        FHideOnGoto := Dlg.cbHideOnGoto.Checked;
        if Dlg.radScanProj.Checked then
          FScanType := tstProject
        else if Dlg.radScanOpen.Checked then
          FScanType := tstOpenFiles
        else if Dlg.radScanDir.Checked then
          FScanType := tstDirectory;
        FScanProjUses := Dlg.chkInclProjUses.Checked;
        FRecurseDirScan := Dlg.chkInclude.Checked;
        FDirToScan := Dlg.cboDirectory.Text;
        SaveSettings;
      end;
    finally
      Dlg.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

constructor TToDoExpert.Create;
begin
  Assert(ToDoExpert = nil);
  inherited Create;
  ToDoExpert := Self;
  HasMenuItem := True;
  FTokenList := TTokenList.Create;
  FHideOnGoto := True;
  FScanType := tstProject;
  FDirList := TDirList.Create;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

destructor TToDoExpert.Destroy;
begin
  fmToDo.Free;

{P  if Active then  // FIXME
    IdeDockManager.UnRegisterDockableForm(fmToDo, 'fmToDo');
  Suppose the expert is just activated, then we are unregistering a not
  registered form, fortunately unregistering is only necessary to prevent
  that your form is suddenly created. }
  FDirList.Free;
  FDirList := nil;

  FTokenList.Free;
  FTokenList := nil;

  ToDoExpert := nil;
  inherited Destroy;
end;

function TToDoExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'To Do List';
begin
  Result := SDisplayName;
end;

function TToDoExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&To Do List';
begin
  Result := SMenuCaption;
end;

function TToDoExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TToDoExpert.GetMenuName: string;
begin
  Result := 'GX_ToDo';
end;

function TToDoExpert.GetName: string;
begin
  Result := 'ToDo_Expert';
end;

function TToDoExpert.IconFileName: string;
begin
  Result := 'ToDo';
end;

procedure TToDoExpert.LoadSettings;
begin
  inherited LoadSettings;
  // Do not localize
  FTokenList.LoadFromRegistry(ConfigInfo.RegKey + '\GExperts\ToDo');
  FDirList.LoadFromRegistry(ConfigInfo.RegKey + '\GExperts\ToDo');
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    FShowTokens := ReadBool('ToDo', 'ShowTokens', False);
    FAddMessage := ReadBool('ToDo', 'AddMessage', False);
    FHideOnGoto := ReadBool('ToDo', 'HideOnGoto', False);
    FScanType := TToDoScanType(ReadInteger('ToDo', 'ScanType', Ord(tstProject)));
    FScanProjUses := ReadBool('ToDo', 'ScanProjUses', False);
    fDirToScan := ReadString('ToDo', 'DirToScan', '');
    fRecurseDirScan := ReadBool('ToDo', 'RecurseDirScan', False);
    if Active then
      IdeDockManager.RegisterDockableForm(TfmToDo, fmToDo, 'fmToDo');
  finally
    Free;
  end;
end;

procedure TToDoExpert.SaveSettings;
begin
  inherited SaveSettings;
  // Do not localize
  FTokenList.SaveToRegistry(ConfigInfo.RegKey + '\GExperts\ToDo');
  FDirList.SaveToRegistry(ConfigInfo.RegKey + '\GExperts\ToDo');
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteBool('ToDo', 'ShowTokens', FShowTokens);
    WriteBool('ToDo', 'AddMessage', FAddMessage);
    WriteBool('ToDo', 'HideOnGoto', FHideOnGoto);
    WriteInteger('ToDo', 'ScanType', Ord(FScanType));
    WriteBool('ToDo', 'ScanProjUses', FScanProjUses);
    WriteString('ToDo', 'DirToScan', fDirToScan);
    WriteBool('ToDo', 'RecurseDirScan', fRecurseDirScan);
  finally
    Free;
  end;
end;

procedure TToDoExpert.SetDirList(New: TStrings);
begin
  FDirList.Assign(New);
end;

function TToDoExpert.GetDirList: TStrings;
begin
  Result := FDirList;
end;

procedure TToDoExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      fmToDo.Free;
      fmToDo := nil;
    end;
  end;
end;

{ TDirList }

procedure TDirList.LoadFromRegistry(const KeyName: string);
begin
  // do not localize any of the following
  with TRegIniFile.Create(KeyName) do
  try
    ReadSection('DirList', Self);
  finally
    Free;
  end;
end;

procedure TDirList.SaveToRegistry(const KeyName: string);
var
  i: Integer;
begin
  // Do not localize any of the below items
  with TRegIniFile.Create(KeyName) do
  try
    EraseSection('DirList');
    for i := 0 to Count - 1 do
      WriteString('DirList', Self[i], '');
  finally
    Free;
  end;
end;

initialization
  RegisterGX_Expert(TToDoExpert);

end.


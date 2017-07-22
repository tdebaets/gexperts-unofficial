unit GX_ProcedureList;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

{$IFDEF VER125}
{$D-} //! StH: This works around a bug in C++Builder 4.0
{$ENDIF VER125}

{$IFDEF VER110}
  {$D-} //! StH: Work around a compiler bug in C++Builder 3.0
{$ENDIF VER110}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ToolIntf, ExptIntf, EditIntf, Grids,
  Buttons, GX_EditReader, Registry,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  GX_Experts, Menus;

const
  UM_RESIZECOLS = WM_USER + 523;

type
  TProcInfo = class(TObject)
  public
    LineNo: Integer;
    Name: string;
    DisplayName: string;
    ProcedureType: string;
  end;

type
  TfmProcedureList = class(TForm)
    ilImages: TImageList;
    pnHolder: TPanel;
    lvProcs: TListView;
    StatusBar: TStatusBar;
    pnlHeader: TPanel;
    pnlToolbar: TPanel;
    sbGoto: TSpeedButton;
    sbHelp: TSpeedButton;
    sbStart: TSpeedButton;
    sbAny: TSpeedButton;
    sbFont: TSpeedButton;
    dlgProcFont: TFontDialog;
    sbCopy: TSpeedButton;
    pnlHeaderLeft: TPanel;
    lblMethods: TLabel;
    edtMethods: TEdit;
    pnlHeaderRight: TPanel;
    cbxObjects: TComboBox;
    lblObjects: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnGotoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvProcsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure sbHelpClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvProcsColumnClick(Sender: TObject; Column: TListColumn);
    procedure sbFontClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure edtMethodsChange(Sender: TObject);
    procedure edtMethodsKeyPress(Sender: TObject; var Key: Char);
    procedure edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxObjectsChange(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);
  protected
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
  private
    ProcCount: Integer;
    Loaded: Boolean;
    EditReader: TEditReader;
    Sort: Integer;
    ProcList: TStringList;
    NoUpdate: Boolean;
    GotoLine: Integer;
    FFileName: string;
    FObjectStrings: TStringList;
    FNoComments: Boolean;
    FEditViewCount: Integer;
    procedure QuickSort(L, R: Integer);
    function SetIndex(ProcName: string): Integer;
    procedure LoadProcs;
    procedure FillListBox;
    procedure ResizeCols;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
    function GetMethodName(const ProcName: string): string;
    procedure ClearObjectStrings;
    procedure LoadObjectCombobox;
  public
    procedure AddProcedure(const ProcedureName, ProcedureType: string; LineNo: Integer);
    procedure SaveSettings;
    procedure LoadSettings;
    property NoComments: Boolean read FNoComments;
    property EditViewCount: Integer read FEditViewCount;
    constructor CreateWithFileName(AOwner: TComponent; const FileName: string);
  end;

type
  TProcedureExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}
{$R ProcList.res}

uses
  GX_GenFunc, ClipBrd,
{$IFOPT D+}
  GX_DbugIntf, TypInfo,
{$ENDIF D+}
  mPasLex, GX_GExperts,
  GX_ConfigurationInfo;

{ Tell Delphi about exceptions. }

resourcestring
  SAllString  = '<All>';
  SNoneString = '<None>';

constructor TfmProcedureList.CreateWithFileName(AOwner: TComponent; const FileName: string);
begin
  FFileName := FileName;
  inherited Create(AOwner);
end;

procedure TfmProcedureList.FormCreate(Sender: TObject);
resourcestring
  SCouldNotLoadImages = 'Could not load images';
var
  FileExt: string;
begin
  FObjectStrings := TStringList.Create;
  FObjectStrings.Sorted := True;
  FObjectStrings.Duplicates := dupIgnore;
  ClearObjectStrings;
  Sort := 1;
  FileExt := ExtractUpperFileExt(FFileName);
  EditReader := TEditReader.Create(FFileName);
  try
    EditReader.BufSize := 30000;
    EditReader.NoTabs := True;
    if FileExt <> '.DPR' then // do not localize
      EditReader.NoComments := NoComments
    else
      EditReader.NoComments := True;
    FEditViewCount := EditReader.EditViewCount;
  finally
    EditReader.FreeFileData;
  end;

  Loaded := False;
  ProcList := TStringList.Create;
  NoUpdate := False;
  ilImages.ResourceLoad(rtBitmap, 'MFIND_STRIP', clTeal); // do not localize
  if ilImages.Count = 0 then
    raise Exception.Create(SCouldNotLoadImages);
  ProcCount := 0;
  GotoLine := -1;
  CenterForm(Self);
  LoadSettings;
  LoadProcs;
  FillListBox;
  sbGoto.Enabled := (lvProcs.Selected <> nil);
  Loaded := True;
end;

procedure TfmProcedureList.LoadProcs;
resourcestring
  SImplementationNotFound = 'implementation section not found (parser error?)';
  SUnknown = 'Unknown';
var
  Parser: TmwPasLex;

  function MoveToImplementation: Boolean;
  begin
    if IsDpr(FFileName) or (IsInc(FFileName)) then // do not localize
    begin
      Result := True;
      Exit;
    end;
    Result := False;
    while Parser.TokenID <> tkNull do
    begin
      if not (Parser.TokenID in [tkSpace, tkCRLF, tkSemicolon]) then
      //{$IFOPT D+}SendDebug('Type: '+GetEnumName(TypeInfo(TTokenKind), Integer(Parser.TokenID))+'  Token: '+Parser.Token);{$ENDIF}
      if Parser.TokenID = tkImplementation then
        Result := True;
      Parser.Next;
      if Result then
        Break;
    end;
  end;

  procedure FindProcs;

    function GetProperProcName(ProcType: TTokenKind; IsClass: Boolean): string;
    begin
      Result := SUnknown;
      if IsClass then
      begin
        if ProcType = tkFunction then
          Result := 'Class Func' // do not localize
        else
          if ProcType = tkProcedure then
            Result := 'Class Proc'; // do not localize
      end
      else
      begin
        case ProcType of
          // do not localize
          tkFunction: Result := 'Function';
          tkProcedure: Result := 'Procedure';
          tkConstructor: Result := 'Constructor';
          tkDestructor: Result := 'Destructor';
        end;
      end;
    end;

  var
    ProcLine: string;
    ProcType: TTokenKind;
    Line: Integer;
    ClassLast: Boolean;
    InParenthesis: Boolean;
    InTypeDeclaration: Boolean;
    FoundNonEmptyType: Boolean;
    IdentifierNeeded: Boolean;
  begin
    if not MoveToImplementation then
      raise Exception.Create(SImplementationNotFound);
    ClassLast := False;
    InParenthesis := False;
    InTypeDeclaration := False;
    FoundNonEmptyType := False;
    ProcList.Capacity := 200;
    ProcList.BeginUpdate;
    try
      while Parser.TokenID <> tkNull do
      begin
        if not InTypeDeclaration and
           (Parser.TokenID in [tkFunction, tkProcedure, tkConstructor, tkDestructor]) then
        begin
          IdentifierNeeded := True;
          ProcType := Parser.TokenID;
          Line := Parser.LineNumber;
          ProcLine := '';
          while not (Parser.TokenId in [tkNull]) do
          begin
            //{$IFOPT D+}SendDebug('Found Inner Token: '+ Parser.Token+ ' '+BTS(ClassLast));{$ENDIF}
            case Parser.TokenID of
              tkIdentifier, tkRegister:
                IdentifierNeeded := False;

              tkRoundOpen:
                begin
                  // Did we run into an identifier already?
                  // This prevents
                  //    AProcedure = procedure() of object
                  // from being recognised as a procedure
                  if IdentifierNeeded then
                    Break;
                  InParenthesis := True;
                end;

              tkRoundClose:
                InParenthesis := False;

            else
              // nothing
            end; // case

            if (not InParenthesis) and (Parser.TokenID = tkSemiColon) then
              Break;

            if not (Parser.TokenID in [tkCRLF, tkCRLFCo]) then
              ProcLine := ProcLine + Parser.Token;
            Parser.Next;
          end; // while
          if Parser.TokenID = tkSemicolon then
            ProcLine := ProcLine + ';';
          if ClassLast then
            ProcLine := 'class ' + ProcLine; // do not localize
          //{$IFOPT D+}SendDebug('FoundProc: ' + ProcLine);{$ENDIF}
          if not IdentifierNeeded then
            AddProcedure(ProcLine, GetProperProcName(ProcType, ClassLast), Line + 1);
        end;
        if (Parser.TokenID = tkClass) and Parser.IsClass then
        begin
          InTypeDeclaration := True;
          FoundNonEmptyType := False;
        end
        else 
        if InTypeDeclaration and
                (Parser.TokenID in [tkProcedure, tkFunction, tkProperty,
                                    tkPrivate, tkProtected, tkPublic, tkPublished]) then
        begin
          FoundNonEmptyType := True;
        end
        else 
        if InTypeDeclaration and
                ((Parser.TokenID = tkEnd) or
                 ((Parser.TokenID = tkSemiColon) and not FoundNonEmptyType)) then
        begin
          InTypeDeclaration := False;
        end;
        //{$IFOPT D+}SendDebug('Found Token: '+ Parser.Token+ ' '+BTS(ClassLast));{$ENDIF}
        ClassLast := (Parser.TokenID = tkClass);
        if ClassLast then
        begin
          Parser.NextNoJunk;
          //{$IFOPT D+}SendDebug('Found Class Token'+ ' '+BTS(ClassLast));{$ENDIF}
        end
        else
          Parser.Next;
      end;
    finally
      ProcList.EndUpdate;
    end;
  end;

resourcestring
  SParseStatistics = 'Procedures processed in %g seconds';
var
  ParseStartTime: DWORD;
  MemStream: TMemoryStream;
begin
  Parser := TmwPasLex.Create;
  try
    MemStream := TMemoryStream.Create;
    try
      // Since this edit reader is destroyed almost
      // immediately, do not call FreeFileData
      with TEditReader.Create(FFileName) do
      try
        SaveToStream(MemStream);
      finally
        Free;
      end;
      {$IFOPT D+}SendDebug('Starting Parse'); {$ENDIF}
      Parser.Origin := MemStream.Memory;
      Caption := Caption + ' - ' + ExtractFileName(FFileName);
      //Application.ProcessMessages;
      ParseStartTime := GetTickCount;
      Screen.Cursor := crHourGlass;
      try
        ClearObjectStrings;
        try
          FindProcs;
        finally
          LoadObjectCombobox;
        end;
        {$IFOPT D+}SendDebug('QuickSorting'); {$ENDIF}
        QuickSort(0, ProcList.Count - 1);
        ParseStartTime := GetTickCount - ParseStartTime;
      finally
        Screen.Cursor := crDefault;
      end;
      StatusBar.Panels[0].Text := Format(SParseStatistics, [ParseStartTime / 1000]);
      StatusBar.Panels[1].Text := Trim(IntToStr(lvProcs.Items.Count));
    finally
      MemStream.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TfmProcedureList.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  try
    FObjectStrings.Free;
    FObjectStrings := nil;
    if ProcList <> nil then
    begin
      for i := 0 to ProcList.Count - 1 do
        ProcList.Objects[i].Free;
      ProcList.Free;
      ProcList := nil;
    end;
    EditReader.Free;
    EditReader := nil;
    {$IFOPT D+}SendDebug('Procedure Window Freed'); {$ENDIF}
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmProcedureList.AddProcedure(const ProcedureName, ProcedureType: string; LineNo: Integer);

  function CleanWhiteSpace(Str: string): string;
  var
    i: Integer;
  begin
    Result := Trim(Str);
    for i := 1 to Length(Result) do
    begin
      if Result[i] = #9 then
        Result[i] := ' ';
    end;
    i := Pos('  ', Result);
    while i > 0 do
    begin
      Delete(Result, i, 1);
      i := Pos('  ', Result);
    end;
  end;

var
  TempStr: string;
  i: Integer;
  ProcedureInfo: TProcInfo;
begin
  // Some fixes from John Hansen <John_Hansen@tcsmgmt.com>
  ProcedureInfo := TProcInfo.Create;
  ProcedureInfo.LineNo := LineNo;
  ProcedureInfo.Name := CleanWhiteSpace(ProcedureName);
  TempStr := ProcedureInfo.Name;
  // Remove the class reserved word
  i := Pos('CLASS ', UpperCase(TempStr)); // do not localize
  if i = 1 then
    Delete(TempStr, 1, Length('CLASS ')); // do not localize
  // Remove 'function' or 'procedure'
  i := Pos(' ', TempStr);
  if i > 0 then
    TempStr := Copy(TempStr, i + 1, Length(TempStr))
  else
    TempStr := TempStr;
  // Remove the paramater list
  i := Pos('(', TempStr);
  if i > 0 then
    TempStr := Copy(TempStr, 1, i - 1);
  // Remove the function return type
  i := Pos(':', TempStr);
  if i > 0 then
    TempStr := Copy(TempStr, 1, i - 1);
  // Check for an implementation procedural type
  if Length(TempStr) = 0 then
  begin
    ProcedureInfo.Free;
    Exit;
  end;
  // Remove any trailing ';'
  if TempStr[Length(TempStr)] = ';' then
    Delete(TempStr, Length(TempStr), 1);
  TempStr := Trim(TempStr);
  ProcedureInfo.DisplayName := TempStr;
  ProcedureInfo.ProcedureType := ProcedureType;
  ProcList.AddObject(#9 + TempStr + #9 + ProcedureType + #9 + IntToStr(LineNo), ProcedureInfo);
  // Add to the object combobox
  if Pos('.', TempStr) = 0 then
    FObjectStrings.Add(SNoneString)
  else
    FObjectStrings.Add(Copy(TempStr, 1, Pos('.', TempStr) - 1));
end;

function TfmProcedureList.GetMethodName(const ProcName: string): string;
var
  CharPos: Integer;
  TempStr: string;
begin
  Result := ProcName;
  Delete(Result, 1, 1);

  CharPos := Pos(#9, Result);
  if CharPos <> 0 then
    Delete(Result, CharPos, Length(Result));

  CharPos := Pos(' ', Result);
  TempStr := Copy(Result, CharPos + 1, Length(Result));

  CharPos := Pos('.', TempStr);
  if CharPos = 0 then
    Result := TempStr
  else
    TempStr := Copy(TempStr, CharPos + 1, Length(TempStr));

  CharPos := Pos('(', TempStr);
  if CharPos = 0 then
    Result := TempStr
  else
    Result := Copy(TempStr, 1, CharPos - 1);

  Result := Trim(Result);
end;

function TfmProcedureList.SetIndex(ProcName: string): Integer;
begin
  ProcName := UpperCase(ProcName);
  if Pos('.', ProcName) <> 0 then
    Result := 0
  else
    Result := 1;
  if Pos('CONSTRUCTOR', ProcName) <> 0 then // do not localize
    Result := 2;
  if Pos('DESTRUCTOR', ProcName) <> 0 then // do not localize
    Result := 3;
end;

procedure TfmProcedureList.btnGotoClick(Sender: TObject);
var
  ProcInfo: TProcInfo;
begin
  if lvProcs.Selected <> nil then
  begin
    ProcInfo := lvProcs.Selected.Data;
    if ProcInfo <> nil then
    begin
      Assert(EditReader <> nil);
      EditReader.GotoLine(ProcInfo.LineNo);
      EditReader.ShowSource;
      EditReader.FreeFileData;
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfmProcedureList.lvProcsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  ProcInfo: TProcInfo;
begin
  ProcInfo := nil;
  if lvprocs.Selected <> nil then
    ProcInfo := lvProcs.Selected.Data;
  if ProcInfo <> nil then
  begin
    StatusBar.Panels[0].Text := ProcInfo.Name;
    StatusBar.Panels[1].Text := Format('%d/%d', [lvProcs.Selected.Index + 1, lvProcs.Items.Count]);
    sbGoto.Enabled := (lvProcs.Selected <> nil);
  end;
end;

procedure TfmProcedureList.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 4);
end;

procedure TfmProcedureList.sbCopyClick(Sender: TObject);
var
  i: Integer;
  Procs: TStringList;
  ProcInfo: TProcInfo;
begin
  Procs := TStringList.Create;
  try
    for i := 0 to lvProcs.Items.Count - 1 do
    begin
      ProcInfo := TProcInfo(lvProcs.Items[i].Data); //GetProcInfo(hlbProcs.Items[i]);
      if ProcInfo <> nil then
        Procs.Add(ProcInfo.Name);
    end;
  finally
    if (Procs <> nil) and (Procs.Count > 0) then
      Clipboard.AsText := Procs.Text;
    Procs.Free;
  end;
end;

procedure TfmProcedureList.FillListBox;
var
  i: Integer;
  ProcName: string;
  ListItem: TListItem;

  procedure AddListItem(ProcInfo: TProcInfo);
  begin
    ListItem := lvProcs.Items.Add;
    ListItem.Caption := '';
    ListItem.ImageIndex :=  SetIndex(ProcInfo.Name);
    ListItem.SubItems.Add(ProcInfo.DisplayName);
    ListItem.SubItems.Add(ProcInfo.ProcedureType);
    ListItem.SubItems.Add(IntToStr(ProcInfo.LineNo));
    ListItem.Data := ProcInfo;
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvProcs.Items.Count > 0 then
    begin
      lvProcs.Selected    := lvProcs.Items[0];
      lvProcs.ItemFocused := lvProcs.Selected;
    end;
  end;

begin
  lvProcs.Items.BeginUpdate;
  try
    lvProcs.Items.Clear;
    if (Length(edtMethods.Text) = 0) and (cbxObjects.Text = SAllString) then
    begin
      for i := 0 to ProcList.Count - 1 do
        AddListItem(TProcInfo(ProcList.Objects[i]));
      FocusAndSelectFirstItem;
      Exit;
    end;
    for i := 0 to ProcList.Count - 1 do
    begin
      ProcName := TProcInfo(ProcList.Objects[i]).Name;
      // is it the object we want?
        if cbxObjects.Text <> SAllString
        then if cbxObjects.Text = SNoneString then
      begin
        if Pos('.', ProcName) <> 0 then // does it have an object?
          Continue;
        if Length(edtMethods.Text) = 0 then // if nothing to match, add
        begin
          AddListItem(TProcInfo(ProcList.Objects[i]));
          Continue;
        end;
      end  // if/then
      else if Pos(cbxObjects.Text, ProcName) = 0 then
        Continue;
      if Length(edtMethods.Text) = 0 then
        AddListItem(TProcInfo(ProcList.Objects[i]))
      else if sbStart.Down and
        (CompareText(edtMethods.Text, Copy(GetMethodName(ProcName), 1, Length(edtMethods.Text))) = 0) then
      begin
        AddListItem(TProcInfo(ProcList.Objects[i]));
      end
      else if sbAny.Down and (Pos(UpperCase(edtMethods.Text), UpperCase(GetMethodName(ProcName))) <> 0) then
        AddListItem(TProcInfo(ProcList.Objects[i]));
    end;
    FocusAndSelectFirstItem;
  finally
    lvProcs.Items.EndUpdate;
  end;
  ResizeCols;
end;

procedure TfmProcedureList.sbFontClick(Sender: TObject);
begin
  dlgProcFont.Font.Assign(lvProcs.Font);
  if dlgProcFont.Execute then
    lvProcs.Font.Assign(dlgProcFont.Font);
end;

procedure TfmProcedureList.FormResize(Sender: TObject);
begin
  with StatusBar do
  begin
    if Width > 80 then
    begin
      Panels[1].Width := 80;
      Panels[0].Width := Width - 80;
    end;
  end;
end;

procedure TfmProcedureList.WMExitSizeMove(var Message: TMessage);
begin
  ResizeCols;
end;

// This is just a nasty hack to be sure the scroll bar is set right
// before playing with the column widths. We should fix this somehow.
procedure TfmProcedureList.ResizeCols;
begin
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

procedure TfmProcedureList.UMResizeCols(var Msg: TMessage);
begin
  Application.ProcessMessages;
  lvProcs.Columns[1].Width := Max(0, lvProcs.ClientWidth - lvProcs.Columns[2].Width
    - lvProcs.Columns[3].Width - lvProcs.Columns[0].Width);
end;

procedure TfmProcedureList.sbStartClick(Sender: TObject);
begin
  FillListBox;
end;

procedure TfmProcedureList.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('ProcViewer', 'SearchAll', sbAny.Down);
    RegIni.WriteInteger('ProcViewer', 'Left', Left);
    RegIni.WriteInteger('ProcViewer', 'Top', Top);
    RegIni.WriteInteger('ProcViewer', 'Width', Width);
    RegIni.WriteInteger('ProcViewer', 'Height', Height);
    RegIni.WriteInteger('ProcViewer', 'SortColumn', Sort);
    SaveFont(RegIni, 'ProcViewer', lvProcs.Font);
  finally
    RegIni.Free;
  end;
end;

procedure TfmProcedureList.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    //{$IFOPT D+}SendDebug('Loading SearchAll: ' + BTS(RegIni.ReadBool('ProcViewer', 'SearchAll', False))); {$ENDIF}
    if RegIni.ReadBool('ProcViewer', 'SearchAll', True) then
      sbAny.Down := True
    else
      sbStart.Down := True;
    //{$IFOPT D+}SendDebug('sbAny.Down: ' + BTS(sbAny.Down) + ' sbStart.Down: ' + BTS(sbStart.Down)); {$ENDIF}
    Left := RegIni.ReadInteger('ProcViewer', 'Left', Left);
    Top := RegIni.ReadInteger('ProcViewer', 'Top', Top);
    Width := RegIni.ReadInteger('ProcViewer', 'Width', Width);
    Height := RegIni.ReadInteger('ProcViewer', 'Height', Height);
    Sort := RegIni.ReadInteger('ProcViewer', 'SortColumn', Sort);
    LoadFont(RegIni, 'ProcViewer', lvProcs.Font);
  finally
    RegIni.Free;
    ResizeCols;
  end;
end;

procedure TfmProcedureList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TfmProcedureList.QuickSort(L, R: Integer);
resourcestring
  SInvalidIndex = 'Invalid index number';

  function GetValue(idx: Integer): string;
  var
    i: Integer;
    TabPos: Integer;
  begin
    if idx >= ProcList.Count then
      raise Exception.Create(SInvalidIndex);
    Result := ProcList.Strings[idx];
    for i := 0 to sort - 1 do
    begin
      TabPos := Pos(#9, Result);
      if TabPos > 0 then
        Delete(Result, 1, TabPos)
      else
        Exit;
    end;
    if Sort = 3 then
    begin
      for i := Length(Result) to 5 do
        Result := ' ' + Result;
    end;
  end;

var
  I, J: Integer;
  P: string;
begin
  if ProcList.Count = 0 then
    Exit;
  repeat
    I := L;
    J := R;
    P := GetValue((L + R) shr 1);
    repeat
      while AnsiCompareText(GetValue(I), P) < 0 do Inc(I);
      while AnsiCompareText(GetValue(J), P) > 0 do Dec(J);
      if I <= J then
      begin
        ProcList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TfmProcedureList.lvProcsColumnClick(Sender: TObject; Column: TListColumn);
var
  i: Integer;
begin
  i := Column.Index;
  if i <> 0 then
  begin
    Self.Cursor := crHourglass;
    try
      Sort := i;
      QuickSort(0, ProcList.Count - 1);
      FillListBox;
    finally
      Self.Cursor := crDefault;
    end;
  end;
end;

procedure TfmProcedureList.edtMethodsChange(Sender: TObject);
begin
  FillListBox;
end;

procedure TfmProcedureList.edtMethodsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: begin
           btnGotoClick(lvProcs);
           Key := #0;
         end;
    #27: begin
           Close;
           Key := #0;
         end;
  end;
end;

procedure TfmProcedureList.edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not ((Key = VK_F4) and (ssAlt in Shift)) then
  begin
    SendMessage(lvProcs.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmProcedureList.cbxObjectsChange(Sender: TObject);
begin
  FillListBox;
  ResizeCols;
end;

procedure TfmProcedureList.pnlHeaderResize(Sender: TObject);
begin
  pnlHeaderLeft.Width := (pnlHeader.ClientWidth div 2);
  edtMethods.Width := pnlHeaderLeft.ClientWidth - edtMethods.Left - 8;
  cbxObjects.Width := pnlHeaderRight.ClientWidth - cbxObjects.Left - 8;
end;

procedure TfmProcedureList.ClearObjectStrings;
begin
  FObjectStrings.Clear;
  FObjectStrings.Add(SAllString);
end;

procedure TfmProcedureList.LoadObjectCombobox;
begin
  cbxObjects.Items.Assign(FObjectStrings);
  cbxObjects.ItemIndex := cbxObjects.Items.IndexOf(SAllString);
end;

{ TProcedureExpert }

constructor TProcedureExpert.Create;
begin
  inherited Create;
  ShortCut := Menus.ShortCut(Word('G'), [ssCtrl]);
  HasConfigOptions := False;
  HasMenuItem := True;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

procedure TProcedureExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here
    end;
  end;
end;

function TProcedureExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Procedure List...';
begin
  Result := SMenuCaption;
end;

function TProcedureExpert.GetMenuName: string;
begin
  Result := 'GX_ProcList'; // do not localize
end;

function TProcedureExpert.GetMenuMask: string;
begin // Compatible with TLB/Interface files (though these shouldn't be edited)
  Result := '*.PAS;*.DPR;*.INC;*.DFM;*.TLB;*.OCX;*.OLB;*.DLL;*.EXE'; // do not localize
end;

function TProcedureExpert.GetName: string;
begin
  Result := 'Procedure_Viewer'; // do not localize
end;

function TProcedureExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Procedure List';
begin
  Result := SDisplayName;
end;

procedure TProcedureExpert.Click(Sender: TObject);

  function IsTypeLibrary(const FileName: string): Boolean;
  var
    FileExt: string;
  begin
    FileExt := ExtractUpperFileExt(FileName);
    Result := ((FileExt = '.TLB')
            or (FileExt = '.OLB')
            or (FileExt = '.OCX')
            or (FileExt = '.DLL')
            or (FileExt = '.EXE'));
  end;

var
  FileName: string;
resourcestring
  SPasOrDprOnly = 'This expert is for use in .PAS or .DPR files only';
begin
  Assert(ToolServices <> nil);
  FileName := ToolServices.GetCurrentFile;
  if IsDfm(FileName) and ToolServices.IsFileOpen(ChangeFileExt(FileName, '.pas')) then
    FileName := ChangeFileExt(FileName, '.pas');

  if not (IsDprOrPas(FileName) or IsTypeLibrary(FileName) or
          IsInc(FileName)) then
    MessageDlg(SPasOrDprOnly, mtError, [mbOK], 0)
  else
  begin
    try
      {$IFOPT D+}SendDebug('Procedure Expert Activated'); {$ENDIF}
      with TfmProcedureList.CreateWithFileName(nil, FileName) do
      try
        if ShowModal <> mrCancel then
          FocusEditWindow(EditViewCount, Filename);
      finally
        Free;
      end;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  end;
end;

function TProcedureExpert.IconFileName: string;
begin
  Result := 'ProcView';
end;

initialization
  RegisterGX_Expert(TProcedureExpert);
end.


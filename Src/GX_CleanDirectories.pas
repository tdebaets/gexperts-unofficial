unit GX_CleanDirectories;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ToolIntf, ExptIntf, GX_GExperts, GX_Experts, Registry, CheckLst;

type
  TCleanExpert = class;

  TfmCleanDirectories = class(TForm)
    gbxDirectories: TGroupBox;
    btnAdd: TButton;
    btnRemove: TButton;
    btnCancel: TButton;
    btnClean: TButton;
    laStatus: TLabel;
    btnHelp: TButton;
    gbxExtensions: TGroupBox;
    clbExtensions: TCheckListBox;
    btnAddExt: TButton;
    btnRemoveExt: TButton;
    chkReportErrors: TCheckBox;
    lCleaning: TLabel;
    clbDirs: TCheckListBox;
    laRecursingNote: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clbExtensionsClickCheck(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure clbDirsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
    procedure clbDirsClick(Sender: TObject);
    procedure btnAddExtClick(Sender: TObject);
    procedure btnRemoveExtClick(Sender: TObject);
    procedure clbDirsKeyPress(Sender: TObject; var Key: Char);
    procedure clbExtensionsClick(Sender: TObject);
    procedure clbExtensionsKeyPress(Sender: TObject; var Key: Char);
  private
    MaxWidth: Integer;
    CleanExpert: TCleanExpert;
    CleanExtList: TStringList;
  private
    FTotalBytesCleaned: Integer;
    FTotalFilesCleaned: Integer;

    procedure PerformCleaning;
    procedure CleanDirectory(const Directory: string; const Recursing: Boolean);
    procedure DeleteFoundFile(const FileName: string);
  public
    constructor CreateParametrized(OwningExpert: TCleanExpert);
  end;

  TCleanExpert = class(TGX_EnhExpert)
  private
    FReportErrors: Boolean;
    FExtensionList: TStrings;
    FCleanList: TStrings;
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
    property ExtensionList: TStrings read FExtensionList;
    property CleanList: TStrings read FCleanList;
  end;

implementation

{$R *.DFM}

uses
  GX_ConfigurationInfo, FileCtrl,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc;

resourcestring
  SCouldNotDelete = 'Could not delete %s'#13#13+
                    'Continue showing errors?';

// Tested, but needs cleanup, and to be moved to GX_GenFunc
function WildcardCompare(FileWildcard, FileName: string; IgnoreCase: Boolean): Boolean;
var
  NameWild, NameFile, ExtWild, ExtFile: string;
  DotPos: Integer;

  function WildCompare(var WildS, IstS: string): Boolean;
  var
    WildPos, FilePos, l, p: Integer;
  begin
    // Start at the first wildcard/filename character
    WildPos := 1;  // Wildcard position
    FilePos := 1;  // FileName position
    while (WildPos <= Length(WildS)) do
    begin
      // '*' matches any sequence of characters
      if WildS[WildPos] = '*' then
      begin
        // We've reached the end of the wildcard string with a * and are done
        if WildPos = Length(WildS) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          l := WildPos + 1;
          // Anything after a * in the wildcard must match literally
          while (l < Length(WildS)) and (WildS[l + 1] <> '*') do
            Inc(l);
          // Check for the literal match immediately after the current position
          p := Pos(Copy(WildS, WildPos + 1, l - WildPos), IstS);
          if p > 0 then
            FilePos := p - 1
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      // '?' matches any character - other characters must literally match
      else if (WildS[WildPos] <> '?') and ((Length(IstS) < WildPos) or
         (WildS[WIldPos] <> IstS[FilePos])) then
      begin
        Result := False;
        Exit;
      end;
      // Match is OK so far - check the next character
      Inc(WildPos);
      Inc(FilePos);
    end;
    Result := (FilePos > Length(IstS));
  end;

  // Get the position of the last period in the string
  function LastDotPos(s: string): Integer;
  var
    i: Integer;
  begin
    i := Length(s);
    while (i > 0) and (s[i] <> '.') do
       Dec(i);
    Result := i;
  end;

begin
  // Parse to find the extension and name base of filename and wildcard
  DotPos := LastDotPos(FileWildcard);
  if DotPos = 0 then
  begin
    // Assume .* if an extension is missing
    NameWild := FileWildcard;
    ExtWild := '*';
  end
  else
  begin
    NameWild := Copy(FileWildcard, 1, DotPos - 1);
    ExtWild := Copy(FileWildcard, DotPos + 1, 255);
  end;
  // We could probably modify this to use ExtractFileExt, etc.
  DotPos := LastDotPos(FileName);
  if DotPos = 0 then
    DotPos := Length(FileName) + 1;
  NameFile := Copy(FileName, 1, DotPos - 1);
  ExtFile := Copy(FileName, DotPos + 1, 255);
  // Case insensitive check
  if IgnoreCase then
  begin
    NameWild := UpperCase(NameWild);
    NameFile := UpperCase(NameFile);
    ExtWild  := UpperCase(ExtWild);
    ExtFile  := UpperCase(ExtFile);
  end;
  // Both the extension and the filename must match
  Result := WildCompare(NameWild, NameFile) and WildCompare(ExtWild, ExtFile);
end;

function ListFiles(Param: Pointer; const FileName, UnitName, FormName: string): Boolean; stdcall;
var
  Strings: TStrings;
  TempPathString: string;
begin
  Result := True;
  try
    Strings := TfmCleanDirectories(Param).clbDirs.Items;

    TempPathString := LowerCase(ExtractFilePath(FileName));
    if Strings.IndexOf(TempPathString) < 0 then
      Strings.Add(TempPathString);
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

{ TfmCleanDirectories }

constructor TfmCleanDirectories.CreateParametrized(OwningExpert: TCleanExpert);
begin
  CleanExpert := OwningExpert;

  inherited Create(nil);
end;

procedure TfmCleanDirectories.FormCreate(Sender: TObject);
const
  CRLF = #13+#10;
const // We will never localize these strings.
  SDefaultCleanExts =
    {$IFDEF GX_VER130_up}
    '.~bpg'+CRLF+'.~cpp'+CRLF+'.~dfm'+CRLF+'.~dpk'+CRLF+
    '.~dsk'+CRLF+'.~h'  +CRLF+'.~hpp'+CRLF+'.~pas'+CRLF+
    {$ELSE}
    '.~bp' +CRLF+'.~cp' +CRLF+'.~df' +CRLF+'.~dp' +CRLF+
    '.~ds' +CRLF+'.~h'  +CRLF+'.~hp' +CRLF+'.~pa' +CRLF+
    {$ENDIF GX_VER130_up}
    '.bak' +CRLF+'.cfg' +CRLF+'.csm' +CRLF+'.dcu' +CRLF+
    '.dof' +CRLF+'.dsk' +CRLF+'.fts' +CRLF+'.gid' +CRLF+
    '.il*' +CRLF+'.kwf' +CRLF+'.md'  +CRLF+'.obj' +CRLF+
    '.tds' +CRLF+'.tmp' +CRLF+'.$*'  +CRLF+'.~*';
var
  i, j: Integer;
begin
  laStatus.Caption := '';

  MaxWidth := clbDirs.Width;

  if CleanExpert.ExtensionList.Count > 0 then
    clbExtensions.Items.Assign(CleanExpert.ExtensionList)
  else
    clbExtensions.Items.Text := SDefaultCleanExts;

  CleanExtList := TStringList.Create;
  CleanExtList.Assign(CleanExpert.CleanList);

  for i := CleanExtList.Count - 1 downto 0 do
  begin
    j := clbExtensions.Items.IndexOf(CleanExtList[i]);
    if j >= 0 then
      clbExtensions.Checked[j] := True
    else
      CleanExtList.Delete(i);
  end;

  ToolServices.EnumProjectUnits(ListFiles, Pointer(Self));
end;

procedure TfmCleanDirectories.FormDestroy(Sender: TObject);
begin
  CleanExpert.CleanList.Assign(CleanExtList);
  CleanExpert.ExtensionList.Assign(clbExtensions.Items);

  CleanExtList.Free;
  CleanExtList := nil;
end;

procedure TfmCleanDirectories.clbExtensionsClickCheck(Sender: TObject);
begin
  with Sender as TCheckListBox do
  begin
    if Checked[ItemIndex] then
      CleanExtList.Add(Items[ItemIndex])
    else
      CleanExtList.Delete(CleanExtList.IndexOf(Items[ItemIndex]));
  end;
end;

procedure TfmCleanDirectories.btnAddClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := '';
  if GetDir(Self, Temp) then
  begin
    Temp := AddSlash(Temp);
    clbDirs.Items.Add(Temp);
  end;
end;

procedure TfmCleanDirectories.btnRemoveClick(Sender: TObject);
var
  i: Integer;
  OldIndex: Integer;
begin
  i := 0;
  OldIndex := clbDirs.ItemIndex;
  // Multiselect isn't published/implemented in Delphi 5
  while i <= clbDirs.Items.Count - 1 do
  begin
    if clbDirs.Selected[i] then
      clbDirs.Items.Delete(i)
    else
      Inc(i);
  end;
  if (OldIndex > -1) and (clbDirs.Items.Count > 0) then
    clbDirs.ItemIndex := Min(OldIndex, clbDirs.Items.Count - 1);
  btnRemove.Enabled := (clbDirs.ItemIndex > -1) and (clbDirs.Items.Count > 0);
end;

procedure TfmCleanDirectories.btnAddExtClick(Sender: TObject);
resourcestring
  SAddNewExtension = 'Add file extension';
  SAddNewText = 'Enter the file extension to be cleaned:';
var
  NewExt: string;
  Idx: Integer;
begin
  if InputQuery(SAddNewExtension, SAddNewText, NewExt) then
  begin
    if NewExt[1] = '*' then
      Delete(NewExt, 1, 1);
    if not (NewExt[1] = '.') then
      NewExt := '.' + NewExt;
    Idx := clbExtensions.Items.Add(NewExt);
    clbExtensions.Checked[Idx] := True;
    // JCH 02/17/1999 - Fixes exception when clicking a new extension
    CleanExtList.Add(NewExt);
  end;
end;

procedure TfmCleanDirectories.btnRemoveExtClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := clbExtensions.ItemIndex;
  if i < 0 then
    Exit;

  j := CleanExtList.IndexOf(clbExtensions.Items[i]);
  if j > -1 then
    CleanExtList.Delete(j);
  clbExtensions.Checked[i] := False;
  clbExtensions.Items.Delete(i);

  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1) and (clbExtensions.Items.Count > 0);
end;

procedure TfmCleanDirectories.btnCleanClick(Sender: TObject);
begin
  try
    PerformCleaning;
  finally
    CleanExpert.SaveSettings;

    ModalResult := mrOK;
  end;
end;

procedure TfmCleanDirectories.clbDirsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  TempString: string;
begin
  TempString := clbDirs.Items[Index];
  if clbDirs.Canvas.TextWidth(TempString) > MaxWidth then
  begin
    MaxWidth := clbDirs.Canvas.TextWidth(TempString);
    SendMessage(clbDirs.Handle, LB_SETHORIZONTALEXTENT, MaxWidth, 0);
    clbDirs.Invalidate;
  end;
  Rect.Right := Rect.Left + MaxWidth;
  clbDirs.Canvas.FillRect(Rect);
  // New style controls with DT_PATH_ELLIPSIS do not work out properly (buggy) - play it safe.
  DrawText(clbDirs.Canvas.Handle, PChar(TempString), -1, Rect, DT_LEFT);
end;

procedure TfmCleanDirectories.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 14);
end;

procedure TfmCleanDirectories.clbDirsClick(Sender: TObject);
begin
  btnRemove.Enabled := (clbDirs.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbDirsKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemove.Enabled := (clbDirs.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbExtensionsClick(Sender: TObject);
begin
  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1);
end;

procedure TfmCleanDirectories.clbExtensionsKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemoveExt.Enabled := (clbExtensions.ItemIndex > -1);
end;

procedure TfmCleanDirectories.DeleteFoundFile(const FileName: string);
{.$DEFINE SimulateDeleting}
{$IFNDEF SimulateDeleting}
var
  TempFileSize: Integer;
{$ENDIF SimulateDeleting}
begin
  {$IFOPT D+}SendDebug('Deleting file: ' + FileName);{$ENDIF}
{$IFNDEF SimulateDeleting}
  TempFileSize := GX_GenFunc.GetFileSize(FileName);
  if DeleteFile(FileName) then
  begin
    Inc(FTotalFilesCleaned);
    FTotalBytesCleaned := FTotalBytesCleaned + TempFileSize;
  end
  else
  begin
    if chkReportErrors.Checked then
    begin
      chkReportErrors.Checked := (MessageDlg(Format(SCouldNotDelete,
                                             [FileName]), mtError, [mbYes, mbNo], 0) = mrYes);
    end;
  end;
{$ENDIF SimulateDeleting}
end;

procedure TfmCleanDirectories.CleanDirectory(const Directory: string; const Recursing: Boolean);
var
  SearchRec: TSearchRec;
  SearchAttr: Integer;
  FindResult: Integer;

  i: Integer;
begin
  // We explicitly do not search for r/o files since we cannot
  // delete them anyway; alternatively do search r/o files and have
  // an error reported?
  SearchAttr := faHidden or faSysFile or faArchive;

  if Recursing then
    SearchAttr := SearchAttr or faDirectory;

  FindResult := FindFirst(Directory + '*.*', SearchAttr, SearchRec);
  try
    while FindResult = 0 do
    begin
      // Do not localize strings in the following expression.
      // Note: this test includes the "recursing" test as we
      // will only find faDirectory if we are recursing.
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory <> 0) then
        begin
          // Recurse into sub-directories.
          SearchRec.Name := AddSlash(SearchRec.Name);
          CleanDirectory(Directory + SearchRec.Name, Recursing);
        end
        else
        begin
          // Delete files with matching extension(s).
          for i := 0 to CleanExtList.Count - 1 do
          begin
            //if CompareText(FoundFileExt, CleanExtList.Strings[i]) = 0 then
            if WildcardCompare('*'+CleanExtList.Strings[i], SearchRec.Name, True) then
            begin
              DeleteFoundFile(Directory + SearchRec.Name);
              Break;
            end;
          end;
        end;
      end;

      FindResult := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TfmCleanDirectories.PerformCleaning;
resourcestring
  SCleaningComplete = 'Cleaning complete. %d files were deleted.'#13#10+
                      '%s bytes of storage space were recovered.';
  SOneCleaningComplete = 'Cleaning complete. %d file was deleted.'#13#10+
                      '%s bytes of storage space were recovered.';
var
  i: Integer;
  ConfirmMessage: string;
begin
  FTotalBytesCleaned := 0;
  FTotalFilesCleaned := 0;

  lCleaning.Visible := True;
  lCleaning.Repaint;

  Self.Cursor := crHourglass;
  try
    for i := 0 to clbDirs.Items.Count - 1 do
    begin
      laStatus.Caption := MinimizeName(clbDirs.Items[i], laStatus.Canvas, laStatus.Width);
      laStatus.Repaint;

      // If a directory is checked, then the user wants to recurse into that
      // directory and clean all items there, too.
      // Ascertain that we have a trailing slash for each directory item here,
      // since doing it directly in the libtbox clears the checked state in D3.
      CleanDirectory(AddSlash(clbDirs.Items[i]), clbDirs.Checked[i]);
    end;
  finally
    Self.Cursor := crDefault;
    if FTotalFilesCleaned = 1 then
      ConfirmMessage := SOneCleaningComplete
    else
      ConfirmMessage := SCleaningComplete;
    MessageDlg(Format(ConfirmMessage,
                      [FTotalFilesCleaned,
                       FormatFloat('#,;;0', FTotalBytesCleaned)]),
               mtInformation, [mbOK], 0);

    lCleaning.Visible := False;

    // This saving is also done on destruction, but duplicating it here
    // preserves changes even if Delphi crashes before we are destroyed.
    CleanExpert.CleanList.Assign(CleanExtList);
    CleanExpert.ExtensionList.Assign(clbExtensions.Items);
  end;
end;

{ TCleanExpert }

constructor TCleanExpert.Create;
begin
  inherited Create;

  FCleanList := TStringList.Create;
  FExtensionList := TStringList.Create;

  HasConfigOptions := False;
end;

destructor TCleanExpert.Destroy;
begin
  SaveSettings;

  FCleanList.Free;
  FCleanList := nil;

  FExtensionList.Free;
  FExtensionList := nil;

  inherited Destroy;
end;

function TCleanExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Clea&n Directories...';
begin
  Result := SMenuCaption;
end;

function TCleanExpert.GetMenuName: string;
begin
  Result := 'GX_Clean';
end;

function TCleanExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TCleanExpert.GetName: string;
begin
  Result := 'Clean_Directory';
end;

function TCleanExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Clean Directories';
begin
  Result := SDisplayName;
end;

procedure TCleanExpert.Click(Sender: TObject);
begin
  with TfmCleanDirectories.CreateParametrized(Self) do
  try
    chkReportErrors.Checked := FReportErrors;
    ShowModal;
    FReportErrors := chkReportErrors.Checked;
  finally
    Free;
  end;
end;

procedure TCleanExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;
  // Do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    ReadStringList(RegIni, CleanList, 'CleanList\Delete', 'CleanExt');
    ReadStringList(RegIni, ExtensionList, 'CleanList\Extensions', 'AvailableExt');
    FReportErrors := RegIni.ReadBool('CleanList', 'ReportError', True);
  finally
    RegIni.Free;
  end;
end;

procedure TCleanExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;
  // Do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('CleanList', 'ReportError', FReportErrors);
    WriteStringList(RegIni, ExtensionList, 'CleanList\Extensions', 'AvailableExt');
    WriteStringList(RegIni, CleanList, 'CleanList\Delete', 'CleanExt');
  finally
    RegIni.Free;
  end;
end;

function TCleanExpert.IconFileName: string;
begin
  Result := 'Clean';
end;

procedure TCleanExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Clean Directories is a modal expert
    end;
  end;
end;

initialization
  RegisterGX_Expert(TCleanExpert);
end.


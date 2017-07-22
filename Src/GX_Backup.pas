unit GX_Backup;

{$I GX_CondDefine.inc}

{$IFDEF VCLZIP}
interface

{.$DEFINE TestCodeForStressing}
(*
  Project backup might occasionally cause a page fault.
  This apparently only happens in rare cases when the option to parse
  the source files is checked.  Sometimes there is an AV in borlndmm.dll.
  Heap corruption?  Buffer Overrun?  Unknown origin, but duplicated.
*)

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GX_Progress,
  VCLUnZip, VCLZip, // Errors here mean VCLZip is missing: see GX_CondDefine.inc
  ToolIntf, ExptIntf,
{$IFDEF GX_VER120_up}
  ToolsApi,
{$ENDIF GX_VER120_up}
  GX_EditReader, GX_Experts;

type
  TBackupExpert = class;

  TBackupType = (btFile, btDir);

  TBackupScope = (bsActiveProject, bsProjectGroup);

  TfmBackup = class(TForm)
    gbxFiles: TGroupBox;
    lbFiles: TListBox;
    btnAdd: TButton;
    btnRemove: TButton;
    btnCancel: TButton;
    btnBackup: TButton;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    btnHelp: TButton;
    btnOptions: TButton;
    btnRemoveWithWildcard: TButton;
    btnAddWithWildcard: TButton;
    procedure btnBackupClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFilesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure VCLZipTotalPercentDone(Sender: TObject; Percent: Integer);
    procedure VCLZipOnSkipFile(Sender: TObject; Reason: TSkipReason;
      FName: string; FileIndex: Integer; var Retry: Boolean);
    procedure VCLZipNoSuchFile(Sender: TObject; FName: String);
    procedure FormShow(Sender: TObject);
    procedure btnAddWithWildcardClick(Sender: TObject);
    procedure lbFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FListboxEntryMaxWidth: Integer;
    FDoAbortCollectingFiles: Boolean;
    FHaveCollectedFiles: Boolean;
    FZipEncrypted: Boolean;
    FProgressForm: TfmProgress;
    FCurrentBackupScope: TBackupScope;
    FBackupExpert: TBackupExpert;
    FLibraryPath: TStringList;
    FFilesFoundNowhere: TStringList;
    VCLZip: TVCLZip;
    procedure LoadIdeLibraryPath;
    procedure LocateFileOnPathAndAdd(FilesNotFound: TStrings);
  {$IFDEF TestCodeForStressing}
    procedure WMBye(var Message: TMessage); message WM_USER + 20;
  {$ENDIF TestCodeForStressing}
  {$IFDEF GX_VER120_up}
    procedure DoCollectFilesFromSingleProject(IProject: IOTAProject);
  {$ENDIF GX_VER120_up}
    procedure DoCollectFiles;
    procedure DoAddWithWildcards(WildcardString: string);
    procedure AddBackupFile(FileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CollectFilesForBackup;
  end;

  TBackupExpert = class(TGX_EnhExpert)
  private
    FFollowLibraryPath: Boolean;
    FBackupInc: Boolean;
    FBackupType: TBackupType;
    FBackupDir: string;
    FIncludeDir: Boolean;
    FBackupScope: TBackupScope;
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
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;

    property BackupDir: string read FBackupDir;
    property BackupScope: TBackupScope read FBackupScope;
    property BackupType: TBackupType read FBackupType;
    property DoBackupIncludedFiles: Boolean read FBackupInc;
    property DoIncludeDirInfoInZip: Boolean read FIncludeDir;
    property FollowLibraryPath: Boolean read FFollowLibraryPath;
  end;

implementation

{$R *.DFM}

uses
  FileCtrl,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc, GX_Substitute,
  GX_BackupConfig, GX_BackupOptions,
  GX_GExperts, Registry, GX_ConfigurationInfo;

const
  ItemSeparatorChar = '|';
  MaximumParsedImplementationLines = 20;

procedure ScanForIncludesAndAdd(const FileName: string; FoundFileList: TStrings; NotFoundFileList: TStrings);
var
  p: Integer;
  Match: Integer;
  EditRead: TEditReader;
  StartedCountingLines: Boolean;
  ImplementationLinesParsed: Cardinal;
  TempString: string;
  StoreFileName: string;
  TempFileName: string;
const
  BInc: array[0..4] of string =
    ('{$I ', '{$INCLUDE ', '{$RESOURCE ', '{$R ', '{#BACKUP ');  // do not localize
begin
  // FIXME
  //! StH: IMO this code/logic is flawed.
  //! 1.) --- void --- [Only $I and $R are detected, not $INCLUDE and $RESOURCE]
  //! 2.) This counting below is confusing - what for?
  //!     (it speeds up parsing by counting only a few lines past the implementation)
  //! 3.) The routine is by far too large
  //! 4.) Anything that even remotely looks like an include will be added; even if commented out
  //! 5.) Really interesting stuff will not be found - units on the path, for instance,
  //!     since we just don't scan for units *at all*.

  //! --> Conclusion: rewrite this routine here from scratch

  StartedCountingLines := False;
  ImplementationLinesParsed := 0;

  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  EditRead := TEditReader.Create(FileName);
  try
    EditRead.UCase := True;
    while not EditRead.Eof do
    begin
      //{$IFOPT D+}SendDebug('Reading Next Line');{$ENDIF}
      TempString := Trim(EditRead.Line);
      //{$IFOPT D+}SendDebug('Read: '+TempString);{$ENDIF}

      //! StH: FIXME
      { TODO -oStefan -cIssue : This assumes that "implementation" is the first word on a line }
      if TempString = 'IMPLEMENTATION' then  // do not localize
        StartedCountingLines := True;

      // Scan for include directive
      Match := Low(BInc)-1;
      p := -1;
      while (Match < High(BInc)) and (p <= 0) do
      begin
        Inc(Match);
        p := Pos(BInc[Match], TempString);
      end;

      //{$IFOPT D+}SendDebug('Searched for include matches');{$ENDIF}
      while p > 0 do
      begin
        if p + Length(BInc[Match]) > Length(TempString) then
        begin
          TempString := '';
          Break;
        end;

        //{$IFOPT D+}SendDebug('TempString: '+TempString);{$ENDIF}
        TempString := Copy(TempString, p + Length(BInc[Match]), Length(TempString));
        p := Pos('}', TempString);
        if p = 0 then
          Break;

        //{$IFOPT D+}SendDebug('Found closing } char');{$ENDIF}
        StoreFileName := UpperCase(Trim(Copy(TempString, 1, p - 1)));
        { TODO -oStefan -cIssue: leaving out *.RES might be a bad idea? }
        if (StoreFileName <> '*.RES') and
           (StoreFileName <> '*.DFM') then // we add these elsewhere already
        begin
          if FileExists(StoreFileName) then
          begin
            if FoundFileList.IndexOf(StoreFileName) < 0 then
              FoundFileList.Add(ExpandFileName(StoreFileName));
          end
          else
          begin
            TempFileName := ExtractFilePath(FileName) + StoreFileName;
            if FileExists(TempFileName) then
            begin
              if FoundFileList.IndexOf(TempFileName) < 0 then
                FoundFileList.Add(TempFileName);
            end
            else
            begin
              if StoreFileName <> '' then
              begin
                StoreFileName := StoreFileName + ItemSeparatorChar + FileName;
                if NotFoundFileList.IndexOf(StoreFileName) < 0 then
                  NotFoundFileList.Add(StoreFileName);
              end;
            end;
          end;
        end;
        if p + 1 < Length(TempString) then
          TempString := Copy(TempString, p + 1, Length(TempString));

        //{$IFOPT D+}SendDebug('Midway');{$ENDIF}

        // Scan for include directive
        Match := Low(BInc)-1;
        p := -1;
        while (Match < High(BInc)) and (p <= 0) do
        begin
          Inc(Match);
          p := Pos(BInc[Match], TempString);
        end;
      end;

      if StartedCountingLines then
      begin
        Inc(ImplementationLinesParsed);
        if ImplementationLinesParsed > MaximumParsedImplementationLines then
          Break;
      end;
      Application.ProcessMessages;
      //{$IFOPT D+}SendDebug('Processed Messages');{$ENDIF}
    end;

  finally
    //{$IFOPT D+}SendDebug('Freeing Edit Reader');{$ENDIF}
    EditRead.Free;
    //{$IFOPT D+}SendDebug('Freed Edit Reader');{$ENDIF}
  end;
end;

function ListFiles(Param: Pointer; const FileName, UnitName, FormName: string): Boolean; stdcall;
var
  BackupForm: TfmBackup absolute Param;
  TempString: string;
begin
  {$IFOPT D+}SendDebug('Enumerating File: '+FileName+' Form: '+FormName+' Unit: '+UnitName);{$ENDIF}

  // by default continue searching
  Result := True;

  // do not backup the .DOF file
  if ExtractUpperFileExt(FileName) = '.DOF' then  // do not localize
    Exit;

  try
    //{$IFOPT D+}SendDebug('Adding: '+FileName);{$ENDIF}
    Assert(BackupForm <> nil);
    BackupForm.AddBackupFile(FileName);

    TempString := ChangeFileExt(FileName, '.dfm');  // do not localize
    if FileExists(TempString) then
      BackupForm.AddBackupFile(TempString);

    if BackupForm.FBackupExpert.DoBackupIncludedFiles and IsDprOrPas(FileName) then
    begin
      {$IFOPT D+}SendDebug('Scanning: '+FileName);{$ENDIF}
      ScanForIncludesAndAdd(FileName, BackupForm.lbFiles.Items, BackupForm.FFilesFoundNowhere);
    end;

    Result := not BackupForm.FDoAbortCollectingFiles;

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

constructor TfmBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLibraryPath := TStringList.Create;
  FLibraryPath.Duplicates := dupIgnore;

  LoadIdeLibraryPath;

  FFilesFoundNowhere := TStringList.Create;
end;

destructor TfmBackup.Destroy;
begin
  FFilesFoundNowhere.Free;
  FFilesFoundNowhere := nil;

  FLibraryPath.Free;
  FLibraryPath := nil;

  inherited Destroy;
end;

procedure TfmBackup.LoadIdeLibraryPath;
const
  LibrarySection = '\Library';
{$IFDEF VER100}
  LibraryPathValue = 'SearchPath';
{$ELSE}
  LibraryPathValue = 'Search Path';
{$ENDIF VER100}

{$IFDEF GX_BCB}
  IdeBase = '$(BCB)';
{$ELSE}
  {$IFDEF GX_VER120_up}
    IdeBase = '$(DELPHI)';
  {$ELSE}
    // IdeBase = ''; <-- not required
  {$ENDIF GX_VER120_up}
{$ENDIF GX_BCB}

var
  BasePath: string;

  procedure AddPathItemToList(PathItem: string);
  {$IFDEF GX_VER110_up}
  var
    IdeBasePos: Integer;
  {$ENDIF GX_VER110_up}
  begin
    PathItem := UpperCase(PathItem);

    // Expand the folder name if necessary $(DELPHI); $(BCB)
    {$IFDEF GX_VER110_up}
      IdeBasePos := Pos(IdeBase, PathItem);
      if IdeBasePos > 0 then
      begin
        Delete(PathItem, IdeBasePos, Length(IdeBase));
        Insert(BasePath, PathItem, IdeBasePos);
      end;
    {$ENDIF GX_VER110_up}

    // Make sure that every item has a trailing slash.
    // This simplifies processing when we scan for files.
    PathItem := AddSlash(PathItem);

    // verify that the directory exists
    if DirectoryExists(PathItem) then
    begin
      FLibraryPath.Add(PathItem);
    end;
  end;

var
  PathString: string;
  PathItem: string;
  CPos: Integer;
begin
  with TRegistry.Create do
  try
    if OpenKey(GxGetIdeBaseRegistryKey + LibrarySection, False) then
    begin
      PathString := ReadString(LibraryPathValue);
      CloseKey;
    end;
  finally
    Free;
  end;
  BasePath := GxGetIdeInstallationDirectory;

  CPos := Pos(';', PathString);
  while CPos > 0 do
  begin
    PathItem := Trim(Copy(PathString, 1, CPos-1));
    if PathItem <> '' then
      AddPathItemToList(PathItem);

    Delete(PathString, 1, CPos);
    CPos := Pos(';', PathString);
  end;
  PathString := Trim(PathString);
  if PathString <> '' then
    AddPathItemToList(PathItem);
end;

procedure TfmBackup.LocateFileOnPathAndAdd(FilesNotFound: TStrings);

  procedure SplitUpEntry(const Entry: string; var IncludedFile, RefererFile: string);
  var
    SeparatorPos: Integer;
  begin
    SeparatorPos := Pos(ItemSeparatorChar, Entry);
    if SeparatorPos > 0 then
    begin
      IncludedFile := Entry;
      Delete(IncludedFile, SeparatorPos, Length(Entry));

      RefererFile := Copy(Entry, SeparatorPos+1, Length(Entry));
    end
    else
    begin
      IncludedFile := Entry;
      RefererFile := '';
    end;
  end;

var
  i, j: Integer;
  IncludedFile: string;
  RefererFile: string;
  FileLocation: string;
begin
  // FilesNotFound is a list of files that have not been found, combined
  // with the filename which contained the reference to that file
  // Both parts are separated by the ItemSeparatorChar character ('|')
  // This routine scans the library path for the presence of files;
  // if a file is found, it is removed from the list of files not found
  // and adds it to the list(box) of files to be backed up.
  // Finally the list of *really* not found files is beautified.

  {$IFOPT D+}
    if FilesNotFound.Count > 0 then
      SendDebug('+++ Files not found:');
    for i := 0 to FilesNotFound.Count-1 do
      SendDebug(PChar(FilesNotFound[i]));
  {$ENDIF D+}

  // Scan each directory on the library path whether
  // it contains any of the missing files.
  // Scan one directory completely before progressing
  // to the next to give the operating system's file cache
  // an easier job of reading the directory structure;
  // this should perform better than iterating over all files
  // and for each file trying to find the containing directory.
  for i := 0 to FLibraryPath.Count-1 do
  begin
    j := FilesNotFound.Count-1;
    while j >= 0 do
    begin
      SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
      FileLocation := FLibraryPath[i] + IncludedFile;
      if FileExists(FileLocation) then
      begin
        FilesNotFound.Delete(j);
        if lbFiles.Items.IndexOf(FileLocation) < 0 then
          lbFiles.Items.Add(FileLocation);
      end;
      Dec(j);
    end;
  end;

  // Finally post-process the list of *really* not found files
  // and give the list a pretty format now
  for j := 0 to FilesNotFound.Count-1 do
  begin
    SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
    FilesNotFound[j] := Format('%s (%s)', [IncludedFile, RefererFile]);
  end;
end;

{$IFNDEF GX_VER120_up}

procedure TfmBackup.DoCollectFiles;
begin
  ToolServices.SaveProject;
  //{$IFOPT D+}SendDebug('Enumerating project units for backup');{$ENDIF}
  ToolServices.EnumProjectUnits(ListFiles, Pointer(Self));
  {$IFOPT D+}SendDebug('Done enumerating project units for backup');{$ENDIF}
end;

{$ELSE}

procedure TfmBackup.DoCollectFiles;
var
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  // First find the currently active project group
  IProjectGroup := GetProjectGroup;
  if IProjectGroup = nil then
    Exit;

  if FCurrentBackupScope = bsActiveProject then
    DoCollectFilesFromSingleProject(IProjectGroup.GetActiveProject)
  else
  begin
    if FileExists(GetProjectGroupFileName) then
      AddBackupFile(GetProjectGroupFileName);
    for i := 0 to IProjectGroup.ProjectCount-1 do
      DoCollectFilesFromSingleProject(IProjectGroup.Projects[i]);
  end;
end;

procedure TfmBackup.DoCollectFilesFromSingleProject(IProject: IOTAProject);
var
  i: Integer;
  IModuleInfo: IOTAModuleInfo;
  IEditor: IOTAEditor;
  FileName: string;
begin
  if IProject = nil then
    Exit;

  // gather project files
  for i := 0 to IProject.GetModuleFileCount-1 do
  begin
    IEditor := IProject.GetModuleFileEditor(i);
    Assert(IEditor <> nil);

    FileName := IEditor.FileName;
    if FileName <> '' then
      ListFiles(Self, IEditor.FileName, '', '');
  end;

  for i := 0 to IProject.GetModuleCount-1 do
  begin
    IModuleInfo := IProject.GetModule(i);
    Assert(IModuleInfo <> nil);

    FileName := IModuleInfo.FileName;
    if FileName <> '' then
      ListFiles(Self, IModuleInfo.FileName, '', IModuleInfo.FormName);
  end;
end;

{$ENDIF GX_VER120_up}

procedure TfmBackup.CollectFilesForBackup;
resourcestring
  SFilesNotFound = 'The following included files could not be found for backup:'#13#13'%s';
begin
  try
    lbFiles.Clear;
    FFilesFoundNowhere.Clear;

    FDoAbortCollectingFiles := False;
    Screen.Cursor := crHourglass;
    lbFiles.Items.BeginUpdate;
    try
      DoCollectFiles;
    finally
      lbFiles.Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;

    if FBackupExpert.FBackupInc then
    begin
      // The "FFilesFoundNowhere" list now contains everything that could not be
      // found scanning the source text. Process now the path in order to
      // possibly find items there.
      // "LocateFileOnPathAndAdd" will remove the files that it finds;
      // after returning, the list will contain those files that *really*
      // could not be found, not even on the library path.
      if FBackupExpert.FollowLibraryPath then
        LocateFileOnPathAndAdd(FFilesFoundNowhere);
      {$IFDEF GX_VER120_up}
      FFilesFoundNowhere.Text := StringReplace(FFilesFoundNowhere.Text, '|', ' from ', [rfReplaceAll]);
      {$ENDIF GX_VER120_up}
      if FFilesFoundNowhere.Count > 0 then
        MessageDlg(Format(SFilesNotFound, [FFilesFoundNowhere.Text]), mtWarning, [mbOK], 0);
    end;

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnBackupClick(Sender: TObject);
resourcestring
  SFileExists = 'File %s already exists, do you want to overwrite this file?';
  SDirectoryDoesNotExist = 'Directory %s does not exist, do you want to create this directory?';
  SBackupComplete = 'The backup operation is complete.';
const
  ZipExtension = '.zip';
var
  ZipFileName: string;
  ZipFilePath: string;
  FileExt: string;
  i: Integer;
  CurrentIdeFolder: string;
begin
  try
    if FBackupExpert.BackupType = btFile then
    begin

    {$IFDEF TestCodeForStressing}
      ZipFileName := 'C:\temp\gx_bu.zip';
      DeleteFile(ZipFileName);
    {$ELSE}
      CurrentIdeFolder := GetCurrentDir;
      try
        if not dlgSave.Execute then
          Exit;
      finally
        SetCurrentDir(CurrentIdeFolder);
      end;

      ZipFileName := dlgSave.FileName;
    {$ENDIF TestCodeForStressing}

      if ExtractFileExt(ZipFileName) = '' then
        ZipFileName := ZipFileName + ZipExtension;
      if FileExists(ZipFileName) then
      begin
        if MessageDlg(Format(SFileExists, [ZipFileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;
        DeleteFile(ZipFileName);
      end;
    end
    else
    begin
      ZipFileName := ReplaceStrings(FBackupExpert.BackupDir, True);
      ZipFilePath := ExtractFilePath(ZipFileName);
      ZipFileName := ExtractFileName(ZipFileName);
      if not DirectoryExists(ZipFilePath) then
      begin
        if MessageDlg(Format(SDirectoryDoesNotExist, [ZipFilePath]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit
        else
          MkDir(ZipFilePath);
      end;
      FileExt := ExtractFileExt(ZipFileName);
      if Pos(FileExt, ZipFileName) <> 0 then
        Delete(ZipFileName, Pos(FileExt, ZipFileName), Length(FileExt));

      if FileExists(ZipFilePath + ZipFileName + ZipExtension) then
      begin
        i := 1;
        while i < 999 do
        begin
          if not FileExists(ZipFilePath + ZipFileName + IntToStr(i) + ZipExtension) then
          begin
            ZipFileName := ZipFileName + IntToStr(i) + ZipExtension;
            Break;
          end;
          Inc(i);
        end;
      end
      else
        ZipFileName := ZipFileName + ZipExtension;
    end;

    FProgressForm := TfmProgress.Create(nil);
    try
      FProgressForm.Progress.Max := 100;
      FProgressForm.Progress.Position := 0;
      Self.Enabled := False;
      FProgressForm.Show;
      with VCLZip do
      begin
        Application.ProcessMessages;
        DestDir := ZipFilePath;
        Zipname := ZipFilePath + ZipFileName;

        OnSkippingFile := VCLZipOnSkipFile;
        OnNoSuchFile := VCLZipNoSuchFile;
        Screen.Cursor := crHourglass;
        try
          FilesList.Assign(lbFiles.Items); { specify filenames }
          if not FZipEncrypted then
            Password := '';
          Zip;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
    finally
      FProgressForm.Free;
      Self.Enabled := True;
    end;
  {$IFNDEF TestCodeForStressing}
    MessageDlg(SBackupComplete, mtInformation, [mbOK], 0);
  {$ENDIF TestCodeForStressing}

    ModalResult := mrOK;

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnAddClick(Sender: TObject);
var
  i: Integer;
  CurrentIdeFolder: string;
begin
  try
    dlgOpen.Options := dlgOpen.Options + [ofAllowMultiSelect, ofFileMustExist] - [ofNoValidate];

    CurrentIdeFolder := GetCurrentDir;
    try
      if dlgOpen.Execute then
        for i := 0 to dlgOpen.Files.Count-1 do
          AddBackupFile(dlgOpen.Files[i]);
    finally
      SetCurrentDir(CurrentIdeFolder);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  try
    i := 0;
    while i <= lbFiles.Items.Count - 1 do
    begin
      if lbFiles.Selected[i] then
        lbFiles.Items.Delete(i)
      else
        Inc(i);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnCancelClick(Sender: TObject);
begin
  try
    Close;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.FormDestroy(Sender: TObject);
begin
  FDoAbortCollectingFiles := True;
end;

procedure TfmBackup.lbFilesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Str: string;
begin
  try
    Str := lbFiles.Items[Index];
    if lbFiles.Canvas.TextWidth(Str) > FListboxEntryMaxWidth then
    begin
      FListboxEntryMaxWidth := lbFiles.Canvas.TextWidth(Str);
      SendMessage(lbFiles.Handle, LB_SETHORIZONTALEXTENT, FListboxEntryMaxWidth, 0);
      Rect.Right := FListboxEntryMaxWidth;
    end;

    lbFiles.Canvas.FillRect(Rect);
    lbFiles.Canvas.TextRect(Rect, Rect.Left, Rect.Top + 1, Str);
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 10);
end;

procedure TfmBackup.FormActivate(Sender: TObject);
begin
  try
    if not FHaveCollectedFiles then
    begin
      VCLZip.StorePaths := FBackupExpert.DoIncludeDirInfoInZip;
      CollectFilesForBackup;
      FHaveCollectedFiles := True;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;

{$IFDEF TestCodeForStressing}
  PostMessage(Self.Handle, WM_USER + 20, 0, 0);
{$ENDIF TestCodeForStressing}
end;

{$IFDEF TestCodeForStressing}
procedure TfmBackup.WMBye(var Message: TMessage);
begin
  btnBackupClick(nil);
  ModalResult := mrOK;
end;
{$ENDIF TestCodeForStressing}

procedure TfmBackup.FormCreate(Sender: TObject);
begin
  try
    FZipEncrypted := False;
    FHaveCollectedFiles := False;
    VCLZip := TVCLZip.Create(Self);
    with VCLZip do
    begin
      Name := 'VCLZip';
      OnTotalPercentDone := VCLZipTotalPercentDone;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.btnOptionsClick(Sender: TObject);
var
  Dlg: TfmBackupOptions;
  OldBackupScope: TBackupScope;
begin
  try
    Dlg := TfmBackupOptions.Create(nil);
    try
      { TODO -oStefan -cIssue : Consistency problem: cbIncludeDir is already present in
                    GX_BackupOptions -> global and not specified as "default" there }
      Dlg.cbIncludeDir.Checked := VCLZip.StorePaths;
      Dlg.cbPassword.Checked := FZipEncrypted;
      Dlg.edPassword.Text := VCLZip.Password;

      OldBackupScope := FCurrentBackupScope;
      Dlg.rgScope.ItemIndex := Ord(FCurrentBackupScope);
      if Dlg.ShowModal = mrOK then
      begin
        VCLZip.StorePaths := Dlg.cbIncludeDir.Checked;
        VCLZip.Password := Dlg.edPassword.Text;
        FZipEncrypted := Dlg.cbPassword.Checked;
        FCurrentBackupScope := TBackupScope(Dlg.rgScope.ItemIndex);
      end;
    finally
      Dlg.Free;
    end;

    if OldBackupScope <> FCurrentBackupScope then
    begin
      FHaveCollectedFiles := False;
      CollectFilesForBackup;
      FHaveCollectedFiles := True;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.VCLZipTotalPercentDone(Sender: TObject; Percent: Integer);
begin
  FProgressForm.Progress.Position := Percent;
end;

procedure TfmBackup.FormShow(Sender: TObject);
begin
  FCurrentBackupScope := FBackupExpert.BackupScope;
end;

//************************ Backup Expert *********************

constructor TBackupExpert.Create;
begin
  inherited Create;
  FBackupType := btFile;
  FBackupDir := '%PROJECTDIR%\%PROJECTNAME%';  // do not localize
  FIncludeDir := True;
  FBackupScope := bsActiveProject;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

function TBackupExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Backup Project...';
begin
  Result := SMenuCaption;
end;

function TBackupExpert.GetMenuName: string;
begin
  Result := 'GX_Backup';
end;

function TBackupExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TBackupExpert.GetName: string;
begin
  Result := 'Backup_Project';
end;

function TBackupExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Backup Project';
begin
  Result := SDisplayName;
end;

procedure TBackupExpert.Click(Sender: TObject);
{$IFDEF TestCodeForStressing}
var
  i: Integer;
{$ENDIF TestCodeForStressing}
begin
{$IFDEF TestCodeForStressing}
if MessageDlg('Warning: lengthy stress testing! - press "No" to cancel NOW', mtWarning,[mbYes, mbNo], 0) = mrNo then
  Exit;
for i := 1 to 1000 do
{$ENDIF TestCodeForStressing}
  with TfmBackUp.Create(nil) do
  try
    FBackupExpert := Self;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBackupExpert.Configure;
var
  Dlg: TfmBackupConfig;
begin
  Dlg := TfmBackupConfig.Create(nil);
  try
    with Dlg do
    begin
      cbBackupInc.Checked := FBackupInc;
      cbIncludeDir.Checked := FIncludeDir;
      rbBackupAskForFile.Checked := (FBackupType = btFile);
      rbBackupToDirectory.Checked := (FBackupType = btDir);
      edBackupDir.Text := FBackupDir;
      rgDefaultScope.ItemIndex := Ord(FBackupScope);
      cbSearchOnLibraryPath.Checked := FFollowLibraryPath;
      if ShowModal = mrOK then
      begin
        if rbBackupAskForFile.Checked then
          FBackupType := btFile
        else
          FBackupType := btDir;
        FBackupDir := edBackupDir.Text;
        FBackupInc := cbBackupInc.Checked;
        FIncludeDir := cbIncludeDir.Checked;
        FBackupScope := TBackupScope(rgDefaultScope.ItemIndex);
        FFollowLibraryPath := cbSearchOnLibraryPath.Checked;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TBackupExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('Backup', 'Include', FBackupInc);
    RegIni.WriteInteger('Backup', 'Type', Ord(FBackupType));
    RegIni.WriteString('Backup', 'Directory', FBackupDir);
    RegIni.WriteBool('Backup', 'IncludeDir', FIncludeDir);
    RegIni.WriteInteger('Backup', 'BackupScope', Ord(FBackupScope));
    RegIni.WriteBool('Backup', 'FollowLibraryPath', FFollowLibraryPath);
  finally
    RegIni.Free;
  end;
end;

procedure TBackupExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FBackupInc := RegIni.ReadBool('Backup', 'Include', FBackupInc);
    FBackupType := TBackupType(RegIni.ReadInteger('Backup', 'Type', Ord(FBackupType)));
    FBackupDir := RegIni.ReadString('Backup', 'Directory', FBackupDir);
    FIncludeDir := RegIni.ReadBool('Backup', 'IncludeDir', FIncludeDir);
    FBackupScope := TBackupScope(RegIni.ReadInteger('Backup', 'BackupScope', Ord(FBackupScope)));
    FFollowLibraryPath := RegIni.ReadBool('Backup', 'FollowLibraryPath', FFollowLibraryPath);
  finally
    RegIni.Free;
  end;
end;

function TBackupExpert.IconFileName: string;
begin
  Result := 'Backup';
end;

procedure TfmBackup.DoAddWithWildcards(WildcardString: string);
resourcestring
  SNoFilesFound = 'Your wildcard selection "%s" did not match any files.';
  SFoundFilesListing = 'The following files matched your wildcard selection:'#13 +
                       #13+
                       '%s'#13;
  SFoundFilesCount = 'Your wildcard selection matched %d files.'#13;

  SConfirmAdding = 'Do you want to add all these files to the backup?';
const
  // Do not search for hidden (faHidden) and system (faSystem) files
  // Also disregard volume IDs and directories
  FileSearchAttributes = faReadOnly or faArchive;
var
  FilePath: string;
  SearchRec: TSearchRec;
  FindResult: Integer;

  FoundFiles: TStringList;

  DoAddFiles: Boolean;
  DuplicateItem: Boolean;
  UpperListBound: Integer;
  i, j: Integer;
begin
  // This methods gets a string passed in that may contain wildcards
  // in the file name / extension. This routine is not equipped to
  // handle wild cards in the the path.
  // We deliberately do not recurse into sub-directories, as this
  // may add by far too many items. Perhaps implement this later
  // with an additional check box.

  FoundFiles := TStringList.Create;
  try

    FilePath := AddSlash(ExtractFilePath(WildcardString));

    FindResult := FindFirst(WildcardString, FileSearchAttributes, SearchRec);
    try
      while FindResult = 0 do
      begin
        FoundFiles.Add(FilePath + SearchRec.Name);

        FindResult := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;

    case FoundFiles.Count of
      0:
        begin
          MessageDlg(SNoFilesFound, mtInformation, [mbOK], 0);
          DoAddFiles := False;
        end;

      1..10: // only list all files if we found <= 10 files (otherwise the dialog is too large)
        begin
          DoAddFiles := MessageDlg(Format(SFoundFilesListing, [FoundFiles.Text]) + SConfirmAdding,
                                   mtInformation,
                                   [mbYes, mbNo], 0) = mrYes;
        end;
    else
      DoAddFiles := MessageDlg(Format(SFoundFilesCount, [FoundFiles.Count]) + SConfirmAdding,
                               mtInformation,
                               [mbYes, mbNo], 0) = mrYes;
    end;

    if DoAddFiles then
    begin
      lbFiles.Items.BeginUpdate;
      try
        // We only ever want to check for duplicates in the
        // now current list of files to be backed up; what
        // we add is guaranteed to be unique.
        UpperListBound := lbFiles.Items.Count-1;

        for i := 0 to FoundFiles.Count-1 do
        begin
          DuplicateItem := False;

          for j := 0 to UpperListBound do
          begin
            DuplicateItem := (CompareText(lbFiles.Items[j], FoundFiles[i]) = 0);
            if DuplicateItem then
              Break;
          end;

          if not DuplicateItem then
            lbFiles.Items.Add(FoundFiles[i]);
        end;
      finally
        lbFiles.Items.EndUpdate;
      end;
    end;

  finally
    FoundFiles.Free;
  end;
end;

procedure TfmBackup.btnAddWithWildcardClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  try
    dlgOpen.Options := dlgOpen.Options - [ofAllowMultiSelect, ofFileMustExist] + [ofNoValidate];

    CurrentIdeFolder := GetCurrentDir;
    try
      if dlgOpen.Execute then
        DoAddWithWildcards(ExpandFileName(dlgOpen.FileName));
    finally
      SetCurrentDir(CurrentIdeFolder);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmBackup.lbFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (lbFiles.ItemIndex > -1) then
  begin
    lbFiles.Items.Delete(lbFiles.ItemIndex);
    Key := 0;
  end;
end;

procedure TBackupExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Backup Project is a modal expert
    end;
  end;
end;

procedure TfmBackup.VCLZipOnSkipFile(Sender: TObject; Reason: TSkipReason;
  FName: String; FileIndex: Integer; var Retry: Boolean);
resourcestring
  SCouldNotOpenFile = ' could not be opened to be zipped.';
begin
  Assert(FProgressForm <> nil);
  FProgressForm.FormStyle := fsNormal;
  try
    Retry := MessageDlg(FName + SCouldNotOpenFile, mtError, [mbIgnore, mbRetry], 0) = mrRetry;
  finally
    FProgressForm.FormStyle := fsStayOnTop;
  end;
end;

procedure TfmBackup.VCLZipNoSuchFile(Sender: TObject; FName: String);
resourcestring
  SCouldNotFindFile = ' could not be found to be zipped.';
begin
  Assert(FProgressForm <> nil);
  FProgressForm.FormStyle := fsNormal;
  try
    MessageDlg(FName + SCouldNotFindFile, mtError, [mbOK], 0);
  finally
    FProgressForm.FormStyle := fsStayOnTop;
  end;
end;

// Check for duplicates before adding files to the the backup list
// Prevents adding files twice when units are in two projects of a group
procedure TfmBackup.AddBackupFile(FileName: string);
var
  i: Integer;
begin
  for i := 0 to lbFiles.Items.Count - 1 do
    if (CompareText(FileName, lbFiles.Items[i]) = 0) then
      Exit;
  lbFiles.Items.Add(FileName);
end;

initialization
  RegisterGX_Expert(TBackupExpert);

{$ELSE VCLZIP}
interface implementation
{$ENDIF VCLZIP}

end.


unit GX_FavFiles;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, StdCtrls, Buttons, FileView, ExtCtrls,
  GX_FavUtil,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  ToolIntf, EditIntf, ExptIntf, GX_Experts, Registry, DropTarget, DropSource;

type
  TfmFavFiles = class(TForm)
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileNew: TMenuItem;
    mitNewFile: TMenuItem;
    mitNewFolder: TMenuItem;
    mitFileDelete: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitOptions: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    tvFolders: TTreeView;
    splTreeView: TSplitter;
    StatusBar: TStatusBar;
    imlFolders: TImageList;
    dlgGetFiles: TOpenDialog;
    mitFileSep1: TMenuItem;
    mitFileProperties: TMenuItem;
    pmuFolders: TPopupMenu;
    mitTreeNewFolder: TMenuItem;
    mitTreeDeleteFolder: TMenuItem;
    mitTreeSep1: TMenuItem;
    mitTreeProperties: TMenuItem;
    pmuFiles: TPopupMenu;
    mitFNewFile: TMenuItem;
    mitFDelete: TMenuItem;
    mitFExecute: TMenuItem;
    mitCSep2: TMenuItem;
    mitFProperties: TMenuItem;
    imlSystem: TImageList;
    mitOptionsOptions: TMenuItem;
    imGlyph: TImage;
    imlSysLarge: TImageList;
    mitFView: TMenuItem;
    mitViewLarge: TMenuItem;
    mitViewSmall: TMenuItem;
    mitViewList: TMenuItem;
    mitViewDetails: TMenuItem;
    mitCSep1: TMenuItem;
    pnlToolbar: TPanel;
    sbnNew: TSpeedButton;
    sbnDelete: TSpeedButton;
    sbnLevelUp: TSpeedButton;
    sbnProperty: TSpeedButton;
    sbnExpand: TSpeedButton;
    sbnContract: TSpeedButton;
    sbnHelp: TSpeedButton;
    pnlFiles: TPanel;
    ListView: TListView;
    splFileView: TSplitter;
    pnlFileView: TPanel;
    procedure mitFileNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mitNewFolderClick(Sender: TObject);
    procedure mitFileExitClick(Sender: TObject);
    procedure mitNewFileClick(Sender: TObject);
    procedure mitFileClick(Sender: TObject);
    procedure mitFileDeleteClick(Sender: TObject);
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mitFilePropertiesClick(Sender: TObject);
    procedure EditFolder(Sender: TObject);
    procedure EditFile(Sender: TObject);
    procedure pmuFilesPopup(Sender: TObject);
    procedure mitFDeleteClick(Sender: TObject);
    procedure mitTreeDeleteFolderClick(Sender: TObject);
    procedure pmuFoldersPopup(Sender: TObject);
    procedure tvFoldersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure hlbFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sbnNewClick(Sender: TObject);
    procedure sbnLevelUpClick(Sender: TObject);
    procedure mitFExecuteClick(Sender: TObject);
    procedure mitHelpAboutClick(Sender: TObject);
    procedure mitOptionsOptionsClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure mitViewDetailsClick(Sender: TObject);
    procedure mitFViewClick(Sender: TObject);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvFoldersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvFoldersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sbnExpandClick(Sender: TObject);
    procedure sbnContractClick(Sender: TObject);
    procedure sbnHelpClick(Sender: TObject);
    procedure mitHelpHelpClick(Sender: TObject);
    procedure mitHelpContentsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvFoldersChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvFoldersEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FileViewer: TFileViewer;
    EntryFile: string;
    FolderDelete: Boolean;
    ExpandAll: Boolean;
    ExecHide: Boolean;
    FModified: Boolean;
    FileDrop: TDropFileTarget;
    procedure SetupSysImage;
    function AddFolder(const Text: string; FType: TFolderType): TTreeNode;
    procedure DeleteCurrentFile;
    procedure DeleteCurrentFolder;
    procedure SaveEntries;
    procedure LoadEntries;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadSourceFile(const SourceFile: string);
    procedure CreateFolders(Comp: TComponent; Node: TTreeNode);
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure AddFilesToCurrentFolder(Files: TStrings);
    procedure SetShowPreview(Value: Boolean);
    function GetShowPreview: Boolean;
  public
    procedure SetFilter;
    procedure GetIcon(const ContainerFileName: string; Image: TImage);
    function GetImageIndex(FileName: string): Integer;
    property Modified: Boolean read FModified write FModified;
    property ShowPreview: Boolean read GetShowPreview write SetShowPreview;
  end;

  TFilesExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  private
    FFavoriteFiles: TfmFavFiles;
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
  end;

implementation

{$R *.DFM}
{$R FavFolders.res}

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  FileCtrl, ShellApi, GX_FavNewFolder, GX_FavFileProp, GX_EditReader,
  GX_FavOptions,
  {$IFNDEF STANDALONE}
  GX_ConfigurationInfo, GX_GExperts,
  {$ENDIF STANDALONE}
  {$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
  {$ENDIF GX_UseNativeToolsApi}
  GX_GenFunc, CommCtrl;

{$OPTIMIZATION OFF}

type
  TreeSaveOption = (stsSelected, stsExpanded);
  TreeSaveOptions = set of TreeSaveOption;
const
  TreeSaveAll = [stsSelected, stsExpanded];

// Return a text path to the node passed in
// Format:
//       Parent
//         Child1
//            Child2
// returns
//      Parent|Child1|Child2
//
// This is based on the nodes' text (!) property
// not on the node (component) names.
function GetNodePath(Node: TTreeNode): string;
begin
  Assert(Node <> nil);
  Result := Node.Text;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    Result := Node.Text + '|' + Result;
  end;
end;

// Tokenize Source with substrings separated by Delimiter
// into Dest stringlist.
// At termination Dest contains all substrings in Source
procedure StrTok(Source, Delimiter: string; Dest: TStringList);
var
  i: Integer;
  SubString: string;
  Temp: string;
begin
  if (Source = '') or (Delimiter = '') or (Dest = nil) then
    Exit;
  Dest.Clear;
  SubString := Source;
  repeat
    i := Pos(Delimiter, SubString);
    if i = 0 then
      Temp := SubString
    else
      Temp := Copy(SubString, 1, i - 1);
    if Temp <> '' then
      Dest.Add(Temp);
    SubString := Copy(SubString, i + Length(Delimiter), Length(SubString) - i);
  until i = 0;
end;

procedure SaveTreeSettings(Tree: TTreeView; Options: TreeSaveOptions; RegistryKey: string);
var
  Registry: TRegistry;
  CurrentNode: TTreeNode;
begin
  if Tree = nil then Exit;

  if RegistryKey = '' then Exit;
  if RegistryKey[Length(RegistryKey)] <> '\' then
    RegistryKey := RegistryKey + '\';

  if (stsSelected in Options) and (Tree.Selected <> nil) then
  begin
    Registry := TRegistry.Create;
    try
      Registry.OpenKey(RegistryKey + Tree.Name, True);
      {$IFOPT D+}SendDebug('FileMgr: Saving selection ' + GetNodePath(Tree.Selected)); {$ENDIF D-}
      Registry.WriteString('Selected', GetNodePath(Tree.Selected)); // do not localize
    finally
      Registry.Free;
    end;
  end;

  if (stsExpanded in Options) and (Tree.Items.Count > 0) then
  begin
    Registry := TRegistry.Create;
    try
      Registry.DeleteKey(RegistryKey + Tree.Name + '\Expanded'); // do not localize
      Registry.OpenKey(RegistryKey + Tree.Name + '\Expanded', True); // do not localize

      CurrentNode := Tree.Items.GetFirstNode;
      while CurrentNode <> nil do
      begin
        if CurrentNode.Expanded then
        begin
          {$IFOPT D+}SendDebug('FileMgr: Writing expanded ' + GetNodePath(CurrentNode)); {$ENDIF D+}
          Registry.WriteString(GetNodePath(CurrentNode), '');
        end;
        CurrentNode := CurrentNode.GetNext;
      end;

    finally
      Registry.Free;
    end;
  end;
  {$IFOPT D+}SendDebug('FileMgr: Done writing tree state'); {$ENDIF D+}
end;

procedure LoadTreeSettings(Tree: TTreeView; Options: TreeSaveOptions; RegistryKey: string);
var
  Path: string;
  Registry: TRegistry;
  FoundNode: TTreeNode;
  i: Integer;
  Expand: TStringList;

  function FindAtParentLevel(ParentNode: TTreeNode; NodeName: string): TTreeNode;
  var
    CurrentNode: TTreeNode;
  begin
    Result := nil;

    if ParentNode = nil then
      CurrentNode := Tree.Items.GetFirstNode
    else
      CurrentNode := ParentNode.GetFirstChild;

    while CurrentNode <> nil do
    begin
      if CompareText(CurrentNode.Text, NodeName) = 0 then
      begin
        Result := CurrentNode;
        Break;
      end;
      CurrentNode := CurrentNode.GetNextSibling;
    end;
  end;

  function GetNode(const NodePath: string): TTreeNode;
  var
    PathList: TStringList;
    CurrentNode: TTreeNode;
    j: Integer;
  begin
    Result := nil;
    PathList := TStringList.Create;
    try
      StrTok(NodePath, '|', PathList);
      if PathList.Count = 0 then
        Exit;

      CurrentNode := nil;
      for j := 0 to PathList.Count - 2 do
      begin
        CurrentNode := FindAtParentLevel(CurrentNode, PathList[j]);
        if CurrentNode = nil then
          Exit;
      end;
      Result := FindAtParentLevel(CurrentNode, PathList[PathList.Count - 1]);
    finally
      PathList.Free;
    end;
  end;

begin
  if (Tree = nil) or (Tree.Items.Count <= 0) then Exit;

  if RegistryKey = '' then Exit;
  if RegistryKey[Length(RegistryKey)] <> '\' then
    RegistryKey := RegistryKey + '\';

  if stsExpanded in Options then
  begin
    Registry := TRegistry.Create;
    try
      Registry.OpenKey(RegistryKey + Tree.Name + '\Expanded', True); // do not localize

      Expand := TStringList.Create;
      try
        Registry.GetValueNames(Expand);
        for i := 0 to Expand.Count - 1 do
        begin
          {$IFOPT D+}SendDebug('FileMgr: Getting for expansion ' + Expand[i]); {$ENDIF D+}
          FoundNode := GetNode(Expand[i]);
          if FoundNode <> nil then
            FoundNode.Expand(False);
        end;
      finally
        Expand.Free;
      end;
    finally
      Registry.Free;
    end;
  end;

  if stsSelected in Options then
  begin
    Registry := TRegistry.Create;
    try
      Registry.OpenKey(RegistryKey + Tree.Name, True);

      Path := Registry.ReadString('Selected'); // do not localize
      {$IFOPT D+}SendDebug('FileMgr: Restoring selection ' + Path); {$ENDIF D+}
      FoundNode := GetNode(Path);
      if FoundNode <> nil then
        Tree.Selected := FoundNode;
    finally
      Registry.Free;
    end;
  end;
end;

resourcestring
  SFavourites = 'Favorites';

procedure TfmFavFiles.GetIcon(const ContainerFileName: string; Image: TImage);
var
  Icon: HIcon;
  ID: Word;
begin
  if not FileExists(ContainerFileName) then Exit;
  Icon := ExtractAssociatedIcon(HInstance, PChar(ContainerFileName), ID);
  if Icon <> 0 then
  begin
    Image.Picture.Icon.Handle := Icon;
    Image.Visible := True;
    Image.Refresh;
  end
  else
    Image.Visible := False;
end;

procedure TfmFavFiles.FormCreate(Sender: TObject);
resourcestring
  SImagesNotLoaded = 'Images were not loaded.';
  SOpenFilter = // Note: localize only the descriptive text, not the extensions
  'Source Files (*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.pas;*.cpp;*.hpp;*.c;*.h)|*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.pas;*.cpp;*.hpp;*.c;*.h' +
    '|Project Files (*.dpr;*.bpr;*.dpk;*.bpk;*.bpg)|*.dpr;*.bpr;*.dpk;*.bpk;*.bpg' +
    '|Pascal Files (*.pas;*.inc)|*.pas;*.inc' +
    '|Help Files (*.hlp)|*.hlp' +
    '|Graphics Files (*.bmp;*.wmf)|*.bmp;*.wmf' +
    '|Text Files (*.txt;*.me;*.asc)|*.txt;*.me;*.asc' +
    '|Executable Files (*.exe)|*.exe' +
    '|SQL Scripts (*.sql)|*.sql' +
    '|C/C++ (*.c;*.cpp;*.h;*.hpp)|*.c;*.cpp;*.h;*.hpp' +
    '|All Files (*.*)|*.*';
const
  FaveFavFile = 'Fave.fav'; // do not localize
begin
  {$IFDEF GX_VER130_up}
  splTreeView.AutoSnap := False;
  splFileView.AutoSnap := False;
  {$ENDIF GX_VER130_up}
  dlgGetFiles.Filter := SOpenFilter;
  FileViewer := TFileViewer.Create(nil);
  FileViewer.Parent := pnlFileView;
  FileViewer.Align := alClient;
  try
    with tvFolders do
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);
    ExpandAll := False;
    FolderDelete := True;
    ExecHide := True;
    ShowPreview := True;
    SetupSysImage;
    FileDrop := TDropFileTarget.Create(nil);
    FileDrop.OnDrop := DropFiles;
    FileDrop.Dragtypes := [dtCopy, dtMove, dtLink];
    FileDrop.ShowImage := True;
    FileDrop.Register(ListView);
    // Assign a default entry file - might be changed later
    {$IFDEF STANDALONE}
    EntryFile := 'C:\' + FaveFavFile; // do not localize
    {$ELSE}
    EntryFile := ConfigInfo.ConfigPath + FaveFavFile;
    {$ENDIF STANDALONE}

    //! StH: FOLDERIMAGES has a Delphi-specific bitmap
    //! that is not appropriate for C++Builder
    imlFolders.ResourceLoad(rtBitmap, 'FOLDERIMAGES', clOlive); // do not localize
    if imlFolders.Count = 0 then
      MessageDlg(SImagesNotLoaded, mtWarning, [mbOK], 0);
    CenterForm(Self);
    LoadSettings;
    LoadEntries;
    if (tvFolders.Selected = nil) and (tvFolders.Items.Count > 0) then
      tvFolders.Selected := tvFolders.Items[0];
    ListView.Columns[0].Width := ColumnTextWidth;
    ListView.Columns[1].Width := ColumnTextWidth;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    {$IFOPT D+}SendDebug('File Mgr: Closing'); {$ENDIF}
    Action := caHide;
    SaveEntries;
    SaveSettings;
    //tvFolders.Selected := nil;
    //Application.ProcessMessages;
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebug('File Mgr: ' + E.Message); {$ENDIF}
      ShowExceptionErrorMessage(E);
    end;
  end;
end;

procedure TfmFavFiles.mitFileNewClick(Sender: TObject);
begin
  mitFileNew.Enabled := (tvFolders.Selected <> nil);
end;

procedure TfmFavFiles.mitNewFolderClick(Sender: TObject);
begin
  try
    with TfmFavNewFolder.Create(nil) do
    try
      FavoriteFilesForm := Self;
      if ShowModal = mrOK then
        tvFolders.Selected := AddFolder(edtFolderName.Text, TFolderType(cbxFolderType.ItemIndex));
    finally
      Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TfmFavFiles.AddFolder(const Text: string; FType: TFolderType): TTreeNode;
var
  Node: TTreeNode;
  Files: TGXFiles;
begin
  Files := nil;
  try
    if tvFolders.Selected = nil then
    begin
      Files := TGXFiles.Create(Root);
      Node := tvFolders.Items.AddObject(nil, Text, Files);
    end
    else
    begin
      Files := TGXFiles.Create(TGXFiles(tvFolders.Selected.Data));
      Node := tvFolders.Items.AddChildObject(tvFolders.Selected, Text, Files);
    end;
    Modified := True;
  except
    on E: Exception do
    begin
      Files.Free;
      raise;
    end;
  end;
  Files.FolderType := FType;
  Node.ImageIndex := Ord(FType) * 2;
  Node.SelectedIndex := Node.ImageIndex + 1;
  Files.Folder := Text;
  Result := Node;
end;

procedure TfmFavFiles.mitFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmFavFiles.mitNewFileClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  if tvFolders.Selected = nil then Exit;
  try
    SetFilter;
    CurrentIdeFolder := GetCurrentDir;
    try
      if dlgGetFiles.Execute then
        AddFilesToCurrentFolder(dlgGetFiles.Files);
    finally
      SetCurrentDir(CurrentIdeFolder);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.mitFileClick(Sender: TObject);
var
  IsEnabled: Boolean;
begin
  mitFileDelete.Enabled := False;
  if ActiveControl = tvFolders then
  begin
    IsEnabled := (tvFolders.Selected <> nil);
    mitFileDelete.Enabled := IsEnabled;
    mitFileProperties.Enabled := IsEnabled;
  end
  else if ActiveControl = ListView then
  begin
    IsEnabled := (ListView.Selected <> nil);
    mitFileDelete.Enabled := IsEnabled;
    mitFileProperties.Enabled := IsEnabled;
  end;
end;

procedure TfmFavFiles.mitFileDeleteClick(Sender: TObject);
begin
  if tvFolders.Selected = nil then Exit;
  if ActiveControl = tvFolders then
    DeleteCurrentFolder
  else
    if ActiveControl = ListView then
      DeleteCurrentFile;
end;

procedure TfmFavFiles.tvFoldersChange(Sender: TObject; Node: TTreeNode);
var
  Files: TGXFiles;
  mFile: TGXFile;
  i: Integer;
  LItem: TListItem;
resourcestring
  SItems = '%d files';
begin
  try
    if tvFolders.Selected <> nil then
      sbnLevelUp.Enabled := not (tvFolders.Selected.Level = 0)
    else
      sbnLevelUp.Enabled := False;

    if (csDestroying in ComponentState) then Exit;
    if tvFolders.Selected = nil then Exit;

    Screen.Cursor := crHourglass;
    ListView.Items.BeginUpdate;
    try
      ListView.Items.Clear;
      ListView.SortType := stNone;
      Files := TGXFiles(tvFolders.Selected.Data);
      for i := 0 to Files.ComponentCount - 1 do
        if Files.Components[i] is TGXFile then
        begin
          mFile := TGXFile(Files.Components[i]);
          LItem := ListView.Items.Add;
          LItem.Caption := mFile.DName;
          LItem.SubItems.Add(mFile.Description);
          LItem.Data := mFile;
          LItem.ImageIndex := GetImageIndex(mFile.FileName);
        end;
      StatusBar.SimpleText := Format(SItems, [ListView.Items.Count]);
    finally
      ListView.SortType := stText;
      ListView.Items.EndUpdate;
      tvFolders.Selected := tvFolders.Selected;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.DeleteCurrentFile;
var
  i: Integer;
begin
  try
    if ListView.Selected = nil then Exit;
    i := 0;
    while i <= ListView.Items.Count - 1 do
    begin
      if ListView.Items[i].Selected then
      begin
        TGXFile(ListView.Items[i].Data).Free;
        ListView.Items[i].Delete;
      end
      else
        Inc(i);
    end;
    ListView.Arrange(arDefault);
    Modified := True;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.SaveEntries;
resourcestring
  SSaveError = 'Your favorite file settings could not be saved:'#13#13 +
    '  %s'#13#13 +
    'Please verify that the path exists and that the file can ' +
    'be written to; optionally choose a different file name by ' +
    'selecting Options | Options...';
var
  Stream: TStream;
begin
  if not Modified then Exit;
  try
    {$IFOPT D+}SendDebug('Saving fave entries'); {$ENDIF D+}
    Stream := TFileStream.Create(EntryFile, fmCreate);
    try
      Stream.WriteComponent(Root);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebugEx('Save Entries: ' + E.Message, mtError); {$ENDIF D+}
      MessageDlg(Format(SSaveError, [E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfmFavFiles.CreateFolders(Comp: TComponent; Node: TTreeNode);
var
  Files: TGXFiles;
  CNode: TTreeNode;
  i: Integer;
begin
  for i := 0 to Comp.ComponentCount - 1 do
    if Comp.Components[i] is TGXFiles then
    begin
      Files := TGXFiles(Comp.Components[i]);
      CNode := tvFolders.Items.AddChildObject(Node, Files.Folder, Files);
      CNode.ImageIndex := Ord(Files.FolderType) * 2;
      CNode.SelectedIndex := (Ord(Files.FolderType) * 2) + 1;
      CreateFolders(Files, CNode);
    end;
end;

procedure TfmFavFiles.LoadEntries;
resourcestring
  SSaveWarning = 'Your folder settings for the Favorite Files manager ' +
    'are not valid - the folder you want to save to does not exist; please ' +
    'make sure that you are writing to an existing folder by ' +
    'selecting Options | Options..., otherwise you might loose your settings.';
var
  Stream: TStream;
  Node: TTreeNode;
begin
  try
    Modified := False;
    if not FileExists(EntryFile) then
    begin
      Node := tvFolders.Items.AddObject(nil, SFavourites, Root);
      Root.Folder := SFavourites;
      Root.FolderType := GX_FavUtil.ftNormal;

      if not DirectoryExists(ExtractFilePath(EntryFile)) then
        MessageDlg(SSaveWarning, mtWarning, [mbOK], 0);
    end
    else
    begin
      Stream := TFileStream.Create(EntryFile, fmOpenRead + fmShareDenyWrite);
      try
        Root := TGXFiles(Stream.ReadComponent(Root));
        Node := tvFolders.Items.AddObject(nil, Root.Folder, Root);
      finally
        Stream.Free;
      end;
    end;

    Node.ImageIndex := 0;
    Node.SelectedIndex := 1;
    CreateFolders(Root, Node);
    Node.Expand(ExpandAll);
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.mitFilePropertiesClick(Sender: TObject);
begin
  if ActiveControl = tvFolders then
    EditFolder(tvFolders)
  else if ActiveControl = ListView then
    EditFile(ListView);
end;

procedure TfmFavFiles.EditFolder(Sender: TObject);
var
  Dlg: TfmFavNewFolder;
  Files: TGXFiles;
resourcestring
  SFolderProperties = 'Folder Properties';
begin
  if tvFolders.Selected = nil then
    Exit;
  try
    Files := TGXFiles(tvFolders.Selected.Data);
    Dlg := TfmFavNewFolder.Create(nil);
    try
      Dlg.FavoriteFilesForm := Self;
      Dlg.Caption := SFolderProperties;
      Dlg.edtFolderName.Text := Files.Folder;
      Dlg.cbxFolderType.ItemIndex := Ord(Files.FolderType);
      if Dlg.ShowModal = mrOK then
      begin
        Modified := True;
        Files.Folder := Dlg.edtFolderName.Text;
        Files.FolderType := TFolderType(Dlg.cbxFolderType.ItemIndex);
        tvFolders.Selected.Text := Files.Folder;
        tvFolders.Selected.ImageIndex := Ord(Files.FolderType) * 2;
        tvFolders.Selected.SelectedIndex := (Ord(Files.FolderType) * 2) + 1;
      end;
    finally
      Dlg.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.EditFile(Sender: TObject);
var
  mFile: TGXFile;
  Dlg: TfmFavFileProp;
begin
  if ListView.Selected = nil then Exit;
  try
    mFile := TGXFile(ListView.Selected.Data);
    Dlg := TfmFavFileProp.Create(nil);
    try
      with Dlg do
      begin
        FavoriteFilesForm := Self;
        edtFilename.Text := mFile.Filename;
        edtName.Text := mFile.DName;
        edtDescription.Text := mFile.Description;
        cbxExecuteType.ItemIndex := Ord(mFile.ExecType);
        edtExecuteUsing.Text := mFile.ExecProg;
        GetIcon(mFile.FileName, imlIcon);
        if ShowModal = mrOK then
        begin
          Modified := True;
          mFile.FileName := edtFilename.Text;
          mFile.Description := edtDescription.Text;
          mFile.DName := edtName.Text;
          mFile.ExecType := TExecType(cbxExecuteType.ItemIndex);
          mFile.ExecProg := edtExecuteUsing.Text;
          with ListView.Selected do
          begin
            Caption := mFile.DName;
            SubItems.Clear;
            SubItems.Add(mFile.Description);
            ImageIndex := GetImageIndex(mFile.FileName);
          end;
        end;
      end;
    finally
      Dlg.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.pmuFilesPopup(Sender: TObject);
var
  IsListViewSelected: Boolean;
begin
  mitFNewFile.Enabled := (tvFolders.Selected <> nil);
  IsListViewSelected := (ListView.Selected <> nil);
  mitFDelete.Enabled := IsListViewSelected;
  mitFExecute.Enabled := IsListViewSelected;
  mitFProperties.Enabled := IsListViewSelected;
end;

procedure TfmFavFiles.mitFDeleteClick(Sender: TObject);
begin
  DeleteCurrentFile;
end;

procedure TfmFavFiles.mitTreeDeleteFolderClick(Sender: TObject);
begin
  DeleteCurrentFolder;
end;

procedure TfmFavFiles.pmuFoldersPopup(Sender: TObject);
var
  IsFolderSelected: Boolean;
begin
  mitTreeNewFolder.Enabled := True;
  IsFolderSelected := (tvFolders.Selected <> nil);
  mitTreeDeleteFolder.Enabled := IsFolderSelected;
  mitTreeProperties.Enabled := IsFolderSelected;
end;

procedure TfmFavFiles.DeleteCurrentFolder;
resourcestring
  SConfirmDeleteFolder = 'Do you want to delete the current folder %s?';
  SCannotDeleteRoot = 'You cannot delete the root folder. Please select a different folder.';
var
  SelectedItem: TTreeNode;
begin
  try
    SelectedItem := tvFolders.Selected;
    if SelectedItem = nil then Exit;
    if SelectedItem.Level = 0 then
    begin
      MessageDlg(SCannotDeleteRoot, mtError, [mbOK], 0);
      Exit;
    end;
    if FolderDelete then
      if MessageDlg(Format(SConfirmDeleteFolder, [SelectedItem.Text]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        Exit;
    TGXFiles(SelectedItem.Data).Free;
    ListView.Items.Clear; { Items freed by folder being deleted }
    SelectedItem.Delete;
    Modified := True;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.tvFoldersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if tvFolders.IsEditing then Exit;
  case Key of
    VK_DELETE:
      begin
        DeleteCurrentFolder;
        Key := 0;
      end;
    VK_INSERT:
      begin
        mitNewFolderClick(tvFolders);
        Key := 0;
      end;
  end;
end;

procedure TfmFavFiles.hlbFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      begin
        DeleteCurrentFile;
        Key := 0;
      end;
    VK_INSERT:
      begin
        mitNewFileClick(ListView);
        Key := 0;
      end;
  end;
end;

procedure TfmFavFiles.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('FileMgr', 'Left', Left);
    RegIni.WriteInteger('FileMgr', 'Top', Top);
    RegIni.WriteInteger('FileMgr', 'Width', Width);
    RegIni.WriteInteger('FileMgr', 'Height', Height);
    RegIni.WriteInteger('FileMgr', 'Splitter', Max(tvFolders.Width, 30));
    RegIni.WriteInteger('FileMgr', 'Splitter2', Max(FileViewer.Height, 30));
    RegIni.WriteString('FileMgr', 'SaveFile', EntryFile);
    RegIni.WriteBool('FileMgr', 'FolderDelete', FolderDelete);
    RegIni.WriteBool('FileMgr', 'ExpandAll', ExpandAll);
    RegIni.WriteBool('FileMgr', 'ExecHide', ExecHide);
    RegIni.WriteBool('FileMgr', 'ShowPreview', ShowPreview);
    RegIni.WriteInteger('FileMgr', 'ListView', Ord(ListView.ViewStyle));
  finally
    RegIni.Free;
  end;

  SaveTreeSettings(tvFolders, TreeSaveAll, ConfigInfo.RegKey + '\GExperts\FileMgr\');
end;

procedure TfmFavFiles.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    Left := RegIni.ReadInteger('FileMgr', 'Left', Left);
    Top := RegIni.ReadInteger('FileMgr', 'Top', Top);
    Width := RegIni.ReadInteger('FileMgr', 'Width', Width);
    Height := RegIni.ReadInteger('FileMgr', 'Height', Height);
    tvFolders.Width := RegIni.ReadInteger('FileMgr', 'Splitter', tvFolders.Width);
    FileViewer.Height := RegIni.ReadInteger('FileMgr', 'Splitter2', FileViewer.Height);
    EntryFile := RegIni.ReadString('FileMgr', 'SaveFile', EntryFile);
    FolderDelete := RegIni.ReadBool('FileMgr', 'FolderDelete', FolderDelete);
    ExpandAll := RegIni.ReadBool('FileMgr', 'ExpandAll', ExpandAll);
    ExecHide := RegIni.ReadBool('FileMgr', 'ExecHide', ExecHide);
    ShowPreview := RegIni.ReadBool('FileMgr', 'ShowPreview', ShowPreview);
    ListView.ViewStyle := TViewStyle(RegIni.ReadInteger('FileMgr', 'ListView', Ord(ListView.ViewStyle)));
  finally
    RegIni.Free;
  end;
  LoadTreeSettings(tvFolders, TreeSaveAll, ConfigInfo.RegKey + '\GExperts\FileMgr\')
end;

procedure TfmFavFiles.SetupSysImage;
var
  AHandle: DWord;
  FileInfo: TSHFileInfo;
begin
  try
    //! StH: Who is responsible for freeing that returned handle?
    //! We do not do it, since both imagelists share their images
    //! The Win32 API docs do not mention anything.
    AHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
    if AHandle <> 0 then
    begin
      imlSystem.Handle := AHandle;
      imlSystem.ShareImages := True;
    end;

    AHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SYSICONINDEX);
    if AHandle <> 0 then
    begin
      imlSysLarge.Handle := AHandle;
      imlSysLarge.ShareImages := True;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TfmFavFiles.GetImageIndex(FileName: string): Integer;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_SMALLICON or SHGFI_SYSICONINDEX) <> 0 then
    Result := FileInfo.IIcon
  else
    Result := 0;
end;

procedure TfmFavFiles.sbnNewClick(Sender: TObject);
begin
  if ActiveControl = tvFolders then
    mitNewFolderClick(Sender)
  else if ActiveControl = ListView then
    mitNewFileClick(ListView);
end;

procedure TfmFavFiles.sbnLevelUpClick(Sender: TObject);
begin
  if tvFolders.Selected <> nil then
    if tvFolders.Selected.Parent <> nil then
      tvFolders.Selected := tvFolders.Selected.Parent;
end;

procedure TfmFavFiles.SetFilter;
var
  Files: TGXFiles;
begin
  if tvFolders.Selected = nil then
    Exit;
  Files := TGXFiles(tvFolders.Selected.Data);
  case Files.FolderType of
    GX_FavUtil.ftNormal: dlgGetFiles.FilterIndex := 10;
    GX_FavUtil.ftDelphi: dlgGetFiles.FilterIndex := 1;
    GX_FavUtil.ftBitmap,
      GX_FavUtil.ftGlyph: dlgGetFiles.FilterIndex := 5;
    GX_FavUtil.ftHelp: dlgGetFiles.FilterIndex := 4;
  else
    dlgGetFiles.FilterIndex := 10;
  end;
end;

procedure TfmFavFiles.mitFExecuteClick(Sender: TObject);
var
  mFile: TGXFile;
resourcestring
  SSaveWarning = 'You did not save the current project.'#13#10 +
    'Do you want to load the new project, anyway?';
  SFileDoesNotExist = 'Could not find the file %s to execute it.';
begin
  if (csDestroying in ComponentState) then Exit;
  try
    if ListView.Selected = nil then Exit;
    if Modified then SaveEntries;
    mFile := TGXFile(ListView.Selected.Data);
    if not (FileExists(mFile.FileName) or DirectoryExists(mFile.FileName)) then
    begin
      MessageDlg(Format(SFileDoesNotExist, [mFile.FileName]), mtError, [mbOK], 0);
      Exit;
    end
    else
    case mFile.ExecType of
      etDelphi: LoadSourceFile(mFile.FileName);
      etShell: GXShellExecute(mFile.FileName, '', True);
      etCustom: GXShellExecute(mFile.ExecProg, mFile.FileName, True);
      etProject:
        begin
          {$IFDEF GX_UseNativeToolsApi}
          if (BorlandIdeServices as IOTAModuleServices).CloseAll then
          {$ELSE not GX_UseNativeToolsApi}
          if ToolServices.CloseProject then
          {$ENDIF not GX_UseNativeToolsApi}
          begin
            // D5/6 crash when calling OpenProject on a .BPG
            if ExtractUpperFileExt(mFile.FileName) = '.BPG' then
              ToolServices.OpenFile(mFile.FileName)
            else
              ToolServices.OpenProject(mFile.FileName);
            {$IFNDEF STANDALONE}
            if ExecHide then
              Self.Hide;
            {$ENDIF STANDALONE}
          end;
        end;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.LoadSourceFile(const SourceFile: string);
var
  ModIntf: TIModuleInterface;
  EditRead: TEditReader;
resourcestring
  SCouldNotOpen = 'Could not open file %s';
begin
  try
    ModIntf := ToolServices.GetModuleInterface(SourceFile);
    try
      if ModIntf = nil then
      begin
        ToolServices.OpenFile(SourceFile);
        ModIntf := ToolServices.GetModuleInterface(SourceFile);
        if ModIntf = nil then
        begin
          MessageDlg(Format(SCouldNotOpen, [SourceFile]), mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      ModIntf.Free;
    end;

    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create(SourceFile);
    try
      if EditRead.Mode = mmFile then
      begin
        MessageDlg(Format(SCouldNotOpen, [SourceFile]), mtError, [mbOK], 0);
        Exit;
      end;
      EditRead.ShowSource;
    finally
      EditRead.Free;
      Self.Hide;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.mitHelpAboutClick(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmFavFiles.mitOptionsOptionsClick(Sender: TObject);
var
  Dlg: TfmFavOptions;
begin
  try
    Dlg := TfmFavOptions.Create(nil);
    try
      Dlg.chkConfirmFolderDelete.Checked := FolderDelete;
      Dlg.chkExpandAllOnLoad.Checked := ExpandAll;
      Dlg.chkHideOnExecute.Checked := ExecHide;
      Dlg.edtSaveFile.Text := EntryFile;
      Dlg.chkShowPreview.Checked := ShowPreview;
      if Dlg.ShowModal = mrOK then
      begin
        FolderDelete := Dlg.chkConfirmFolderDelete.Checked;
        ExpandAll := Dlg.chkExpandAllOnLoad.Checked;
        if CompareText(Dlg.edtSaveFile.Text, EntryFile) <> 0 then
          Modified := True;
        EntryFile := Dlg.edtSaveFile.Text;
        ExecHide := Dlg.chkHideOnExecute.Checked;
        ShowPreview := Dlg.chkShowPreview.Checked;
      end;
    finally
      Dlg.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
resourcestring
  SFilesSelected = '%d files selected';
  SFileMissingString = ' (missing)';
var
  LoadFile: string;
begin
  if (csDestroying in ComponentState) then Exit;
  try
    if (ListView.Selected <> nil) and ShowPreview then
    begin
      if ListView.SelCount = 1 then
      begin
        LoadFile := TGXFile(ListView.Selected.Data).Filename;
        StatusBar.SimpleText := LoadFile;
        if FileExists(LoadFile) or DirectoryExists(LoadFile) then
        begin
          if not (FileViewer.LoadedFile = LoadFile) then
            FileViewer.LoadFromFile(LoadFile);
        end
        else
          StatusBar.SimpleText := StatusBar.SimpleText + SFileMissingString;
      end
      else
        StatusBar.SimpleText := Format(SFilesSelected, [ListView.SelCount]);
    end
    else
    begin
      StatusBar.SimpleText := '';
      FileViewer.Clear;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.mitViewDetailsClick(Sender: TObject);
begin
  ListView.ViewStyle := TViewStyle(TMenuItem(Sender).Tag);
end;

procedure TfmFavFiles.mitFViewClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to mitFView.Count - 1 do
    TMenuItem(mitFView.Items[i]).Checked := (Ord(ListView.ViewStyle) = TMenuItem(mitFView.Items[i]).Tag);
end;

procedure TfmFavFiles.ListViewEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  try
    TGXFile(Item.Data).DName := S;
    Modified := True;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  try
    TGXFiles(Node.Data).Folder := S;
    Modified := True;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.ListViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TfmFavFiles.tvFoldersDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  Accept := (Source is TTreeview) or (Source is TListView);
  if Source is TTreeView then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if Node = nil then
      Accept := False
    else
      if tvFolders.Selected.Level = 0 then
        Accept := False
  end;
end;

procedure TfmFavFiles.tvFoldersDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node: TTreeNode;
  i: Integer;
  mFile: TGXFile;
  Files: TGXFiles;
begin
  try
    Node := tvFolders.GetNodeAt(X, Y);
    if Node = nil then Exit;
    if Source = tvFolders then
    begin
      Screen.Cursor := crHourglass;
      if (tvFolders.Selected = nil) or (tvFolders.Selected = Node) or Node.HasAsParent(tvFolders.Selected) then
        Exit;
      tvFolders.Items.BeginUpdate;
      try
        tvFolders.Selected.MoveTo(Node, naAddChild);
        Files := TGXFiles.Create(TGXFiles(Node.Data));
        Files.AssignFiles(TGXFiles(tvFolders.Selected.Data));
        TGXFiles(tvFolders.Selected.Data).Free;
        tvFolders.Selected.Data := Files;
        tvFolders.Selected.DeleteChildren;
        CreateFolders(Files, tvFolders.Selected);
      finally
        tvFolders.Items.EndUpdate;
        Screen.Cursor := crDefault;
      end;
    end
    else if Source = ListView then
    begin
      i := 0;
      while i <= ListView.Items.Count - 1 do
      begin
        if ListView.Items[i].Selected then
        begin
          mFile := TGXFile.Create(TGXFiles(Node.Data));
          mFile.AssignFile(TGXFile(ListView.Items[i].Data));
          TGXFile(ListView.Items[i].Data).Free;
          ListView.Items[i].Delete;
        end
        else
          Inc(i);
      end;
    end;
  except
    on E: Exception do
    begin
      ShowExceptionErrorMessage(E);
      tvFolders.EndDrag(False);
    end;
  end;
end;

procedure TfmFavFiles.sbnExpandClick(Sender: TObject);
begin
  tvFolders.Items[0].Expand(True);
end;

procedure TfmFavFiles.sbnContractClick(Sender: TObject);
begin
  tvFolders.Items[0].Collapse(True);
end;

procedure TfmFavFiles.sbnHelpClick(Sender: TObject);
begin
  {$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 15);
  {$ENDIF STANDALONE}
end;

procedure TfmFavFiles.mitHelpHelpClick(Sender: TObject);
begin
  {$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 15);
  {$ENDIF STANDALONE}
end;

procedure TfmFavFiles.mitHelpContentsClick(Sender: TObject);
begin
  {$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTENTS, 0);
  {$ENDIF STANDALONE}
end;

procedure TfmFavFiles.FormDestroy(Sender: TObject);
begin
  FileViewer.Free;
  FileViewer := nil;
  FileDrop.Unregister;
  FileDrop.Free;
  FileDrop := nil;
  try
    {$IFOPT D+}SendDebug('File Mgr: Closing');{$ENDIF}
    //tvFolders.Selected := nil;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmFavFiles.tvFoldersChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := True;
end;

procedure TfmFavFiles.tvFoldersEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  tvFolders.EndDrag(False);
end;

procedure TfmFavFiles.ListViewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    mitFExecuteClick(ListView);
  end;
end;

function TFilesExpert.IconFileName: string;
begin
  Result := 'FileMgr';
end;

procedure TfmFavFiles.FormShow(Sender: TObject);
begin
  //LoadSettings;
end;

procedure TfmFavFiles.FormHide(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfmFavFiles.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  AddFilesToCurrentFolder(FileDrop.Files);
end;

procedure TfmFavFiles.AddFilesToCurrentFolder(Files: TStrings);
var
  mFile: TGXFile;
  i: Integer;
  LItem: TListItem;
  FileExt: string;
begin
  if (Files = nil) or (Files.Count < 1) or (tvFolders.Selected = nil) then
    Exit;

  ListView.Items.BeginUpdate;
  Screen.Cursor := crHourglass;
  try
    for i := 0 to Files.Count - 1 do
    begin
      mFile := TGXFile.Create(TGXFiles(tvFolders.Selected.Data));
      try
        mFile.FileName := Files[i];
        mFile.Description := mFile.FileName;
        mFile.DName := ExtractFileName(mFile.FileName);
        FileExt := ExtractUpperFileExt(mFile.FileName);

        // do not localize any of the below items
        if (FileExt = '.PAS')
        {$IFDEF GX_BCB}
        or (FileExt = '.CPP') or (FileExt = '.C') or (FileExt = '.HPP') or (FileExt = '.H')
        {$ENDIF GX_BCB} then
        begin
          // etDelphi is a misnomer for BCB, but cannot be changed, since
          // this would break existing FAVE.FAV files
          mFile.ExecType := etDelphi;
        end
        else
          // Meaning of project file changes depending upon compiler
          {$IFDEF GX_BCB}
          if (FileExt = '.BPR') or (FileExt = '.BPK') then
          {$ELSE}
          if (FileExt = '.DPR') or (FileExt = '.DPK') or (FileExt = '.BPG') then
          {$ENDIF GX_BCB}
            mFile.ExecType := etProject
        else
          mFile.ExecType := etShell;

        mFile.ExecProg := '';
        LItem := ListView.Items.Add;
        LItem.Caption := mFile.DName;
        LItem.SubItems.Add(mFile.Description);
        LItem.Data := mFile;
      except
        on E: Exception do
        begin
          mFile.Free;
          raise;
        end;
      end;
      Modified := True;
      LItem.ImageIndex := GetImageIndex(mFile.FileName);
    end;
  finally
    ListView.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmFavFiles.SetShowPreview(Value: Boolean);
begin
  if Value <> pnlFileView.Visible then
  begin
    splFileView.Visible := Value;
    pnlFileView.Visible := Value;
    FileViewer.Clear;
    if (Value) and (ListView.Selected <> nil) then
      ListViewChange(ListView, ListView.Selected, ctState);
  end;
end;

function TfmFavFiles.GetShowPreview: Boolean;
begin
  Result := pnlFileView.Visible;
end;

//************************ TFilesExpert ********************

constructor TFilesExpert.Create;
begin
  inherited Create;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TFilesExpert.Destroy;
begin
  inherited Destroy;
end;

procedure TFilesExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    else
    begin
      if Assigned(FFavoriteFiles) then
      begin
        if FFavoriteFiles.Visible then
          FFavoriteFiles.Close;
      end;

      FFavoriteFiles.Free;
      FFavoriteFiles := nil;
    end;
  end;
end;

function TFilesExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Favorite &Files';
begin
  Result := SMenuCaption;
end;

function TFilesExpert.GetMenuName: string;
begin
  Result := 'GX_Filemgr';
end;

function TFilesExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TFilesExpert.GetName: string;
begin
  Result := 'File_Manager';
end;

function TFilesExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Favorite Files Manager';
begin
  Result := SDisplayName;
end;

procedure TFilesExpert.Click(Sender: TObject);
begin
  if FFavoriteFiles = nil then
    FFavoriteFiles := TfmFavFiles.Create(nil);
  if FFavoriteFiles.WindowState = wsMinimized then
    FFavoriteFiles.WindowState := wsNormal;
  FFavoriteFiles.Show;
end;

initialization
  RegisterGX_Expert(TFilesExpert);
end.


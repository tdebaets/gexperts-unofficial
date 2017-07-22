unit GX_CodeLib;

{$I GX_CondDefine.inc}

{$IFNDEF GX_NOBDE}

{$IFDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF SYNTAXMEMO}

{$IFDEF MWEDIT}
{$UNDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF MWEDIT}


//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, Db, DBTables, Buttons, ToolIntf, ExptIntf,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  {$IFNDEF GX_ENHANCED_EDITOR}
  StdCtrls, Printers,
  {$ENDIF GX_ENHANCED_EDITOR}
  {$IFDEF SYNTAXMEMO}
  SyntaxEd,
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  mwCustomEdit,
  {$ENDIF MWEDIT}
  GX_EditReader, ComCtrls, GX_EditWriter, Clipbrd, Registry, BDE, GX_Experts;

type
  SearchRecord = record
    Text: string;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
  end;

type
  TCodeLayout = (clSide, clTop);

type
  TfmCodeLib = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N1: TMenuItem;
    PrinterSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    Contents1: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    Delete1: TMenuItem;
    sbOpen: TSpeedButton;
    sbDelete: TSpeedButton;
    sbCut: TSpeedButton;
    sbCopy: TSpeedButton;
    sbPaste: TSpeedButton;
    sbFind: TSpeedButton;
    ilTreeView: TImageList;
    PrinterSetupDialog1: TPrinterSetupDialog;
    pnlView: TPanel;
    pmTopics: TPopupMenu;
    mitNew: TMenuItem;
    mitDelete: TMenuItem;
    pmCode: TPopupMenu;
    mitCut: TMenuItem;
    mitCopy: TMenuItem;
    mitPaste: TMenuItem;
    N4: TMenuItem;
    mitHighlighting: TMenuItem;
    mitPascal: TMenuItem;
    mitCPP: TMenuItem;
    N5: TMenuItem;
    CopyfromDelphi1: TMenuItem;
    PasteintoDelphi1: TMenuItem;
    N6: TMenuItem;
    mitCopyfromDelphi: TMenuItem;
    mitPasteintoDelphi: TMenuItem;
    N7: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    sbExpand: TSpeedButton;
    sbContract: TSpeedButton;
    N8: TMenuItem;
    Updatedatabase1: TMenuItem;
    mitNone: TMenuItem;
    Timer1: TTimer;
    sbPasteToDelphi: TSpeedButton;
    sbCopyFromDelphi: TSpeedButton;
    RootFolder1: TMenuItem;
    Folder1: TMenuItem;
    Snippet1: TMenuItem;
    mitNewRootFolder: TMenuItem;
    mitNewFolder: TMenuItem;
    mitNewSnippet: TMenuItem;
    dlgOpen: TOpenDialog;
    Options1: TMenuItem;
    Options2: TMenuItem;
    mitMakeRoot: TMenuItem;
    PackDatabase1: TMenuItem;
    sbFolder: TSpeedButton;
    mitSep: TMenuItem;
    mitExpandAll: TMenuItem;
    mitContractAll: TMenuItem;
    mitHTML: TMenuItem;
    mitSQL: TMenuItem;
    tvTopics: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure CodeTextChange(Sender: TObject);
    procedure tvTopicsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure Delete1Click(Sender: TObject);
    procedure PrinterSetup1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sbCutClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbPasteClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SetLanguage(Sender: TObject);
    procedure CopyfromDelphi1Click(Sender: TObject);
    procedure PasteIntoDelphi1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure pmCodePopup(Sender: TObject);
    procedure sbFindClick(Sender: TObject);
    procedure FindNext1Click(Sender: TObject);
    procedure tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure sbExpandClick(Sender: TObject);
    procedure sbContractClick(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure Snippet1Click(Sender: TObject);
    procedure RootFolder1Click(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure tvTopicsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Updatedatabase1Click(Sender: TObject);
    procedure Options2Click(Sender: TObject);
    procedure tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure mitMakeRootClick(Sender: TObject);
    procedure pmTopicsPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PackDatabase1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvTopicsDblClick(Sender: TObject);
  private
    FModified: Boolean;
    FSearch: SearchRecord;
    FLayout: TCodeLayout;
    FDatabasePath: string;
    {$IFDEF SYNTAXMEMO}
    CodeText: TSyntaxMemo;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    CodeText: TmwCustomEdit;
    {$ENDIF MWEDIT}
    {$IFNDEF GX_ENHANCED_EDITOR}
    CodeText: TRichEdit;
    {$ENDIF GX_ENHANCED_EDITOR}
    function CreateNewDB(FileName: string): TTable;
    function OpenDB(FileName: string): TTable;
    procedure InitializeTreeView;
    procedure SaveRecord;
    procedure DoSearch(First: Boolean);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure SetModified(New: Boolean);
    procedure LoadSystemData(FileName: string);
    procedure SetLayout(New: TCodeLayout);
    function PackTable(Tbl: TTable; db: TDatabase): DBIResult;
    procedure SortNodes;
  public
    CodeDB: TTable;
    function AddFolder(Node: TTreeNode; Desc: string): TTreeNode;
    function AddCode(Desc: string): TTreeNode;
    property Modified: Boolean read FModified write SetModified;
    property DatabasePath: string read FDatabasePath write FDatabasePath;
    property Layout: TCodeLayout read FLayout write SetLayout;
  end;

  TCodeLibExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  private
    FCodeLibForm: TfmCodeLib;
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
{$R CodeLib.res}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_CodeNew, GX_CodeSrch, GX_GenFunc, GX_Progress, GX_CodeOpt,
  GX_GExperts, GX_ConfigurationInfo,
  {$IFDEF SYNTAXMEMO}
  SynParse,
  {$ENDIF SYNTAXMEMO}
  CommCtrl, MMSystem;

resourcestring
  SCouldNotCreateDatabase = 'Could not create database.';

function TfmCodeLib.CreateNewDB(FileName: string): TTable;
begin
  Result := TTable.Create(Self);
  try
    with Result do
    begin
      // do not localize any of the following lines
      DatabaseName := ExtractFilePath(FileName);
      TableName := ExtractFileName(FileName);
      TableType := ttParadox;
      FieldDefs.Add('Key', ftAutoInc, 0, False);
      FieldDefs.Add('Type', ftString, 1, True);
      FieldDefs.Add('Parent', ftInteger, 0, False);
      FieldDefs.Add('System', ftInteger, 0, False);
      FieldDefs.Add('Topic', ftString, 50, False);
      FieldDefs.Add('Code', ftMemo, 0, False);
      FieldDefs.Add('Language', ftString, 1, False);
      IndexDefs.Add('Main', 'Key', [ixPrimary]);
      IndexDefs.Add('Parent', 'Parent', []);
      IndexDefs.Add('System', 'System', []);
      CreateTable;
      IndexName := 'Parent';
      Open;
    end;
  except
    on E: Exception do
    begin
      Result.Free;
      Result := nil; // We swallow the exception, hence need a defined function result
      ShowExceptionErrorMessage(E);
    end;
  end;
  {$IFOPT D+}SendDebug('Created new code database');{$ENDIF}
end;

function TfmCodeLib.OpenDB(FileName: string): TTable;
begin
  Result := TTable.Create(Self);
  with Result do
  begin
    DatabaseName := ExtractFilePath(FileName);
    TableName := ExtractFileName(FileName);
    TableType := ttParadox;
    try
      Open;
      IndexName := 'Parent';  // do not localize
    except
      on E: Exception do
      begin
        Close;
        Result.Free;
        Result := nil;
      end;
    end;
  end;
  {$IFOPT D+}SendDebug('Opened code database');{$ENDIF}
end;

procedure TfmCodeLib.InitializeTreeView;

  procedure LoadTreeView(Node: TTreeNode; Parent: Integer);
  var
    BookMark: TBookMark;
    RNode: TTreeNode;
  begin
    BookMark := CodeDB.GetBookMark;
    try
      if CodeDB.Locate('Parent', Parent, []) then // do not localize
        while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = Parent) do // do not localize
        begin
          RNode := tvTopics.Items.AddChildObject(Node, CodeDB.FieldByName('Topic').AsString, // do not localize
            Pointer(CodeDB.FieldByName('Key').AsInteger)); // do not localize
          if CodeDB.FieldByName('Type').AsString = 'F' then // do not localize
          begin
            RNode.ImageIndex := 0; // Closed folder
            RNode.SelectedIndex := 1; // Open folder
            LoadTreeView(RNode, CodeDB.FieldByName('Key').AsInteger); // do not localize
          end
          else
          begin
            RNode.ImageIndex := 2; // Code snippet
            RNode.SelectedIndex := 2;
          end;
          CodeDB.Next;
        end;
    finally
      CodeDB.GotoBookMark(BookMark);
      CodeDB.FreeBookMark(BookMark);
    end;
  end;

var
  Node: TTreeNode;
begin
  tvTopics.SortType := stNone;
  tvTopics.Items.BeginUpdate;
  try
    CodeDB.First;
    while (not CodeDB.EOF) and (CodeDB.FieldByName('Parent').AsInteger = 0) do // do not localize
    begin
      Node := tvTopics.Items.AddObject(nil, CodeDB.FieldByName('Topic').AsString, // do not localize
        Pointer(CodeDB.FieldByName('Key').AsInteger)); // do not localize
      Node.ImageIndex := 0; // Closed folder
      Node.SelectedIndex := 1;
      LoadTreeView(Node, CodeDB.FieldByName('Key').AsInteger); // do not localize
      CodeDB.Next;
    end;
  finally
    tvTopics.SortType := stText;
    tvTopics.Items.EndUpdate;
  end;
  {$IFOPT D+}SendDebug('Finished creating tree view nodes');{$ENDIF}
end;

procedure TfmCodeLib.FormCreate(Sender: TObject);
resourcestring
  SImagesNotLoaded = 'Images were not loaded.';
begin
  {$IFDEF SYNTAXMEMO}
  CodeText := TSyntaxMemo.Create(Self);
  CodeText.Parser2 := GetSyntaxParser(gxpPAS);
  CodeText.ActiveParser := Ord(gxpPAS);
  CodeText.Options := CodeText.Options - [smoShowGutter];
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  CodeText := TmwCustomEdit.Create(Self);
  //CodeText.Options := CodeText.Options - [mweoScrollPastEol];
  SetmwEditHighLighter(CodeText, gxpPAS);
  //GetIDEHighLigherSettings(CodeText.HighLighter);
  CodeText.Gutter.Width := 0;
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  CodeText := TRichEdit.Create(Self);
  CodeText.ScrollBars := ssBoth;
  CodeText.PlainText:= True;
  CodeText.WordWrap := False;
  {$ENDIF GX_ENHANCED_EDITOR}
  CodeText.Align := alClient;
  CodeText.PopupMenu := pmCode;
  CodeText.OnChange := CodeTextChange;
  CodeText.Parent := pnlView;
  CodeText.ReadOnly := True;
  try
    Screen.Cursor := crHourglass;
    try
      CodeDB := nil;
      {$IFOPT D+}SendDebug('Setting CodeLib database path');{$ENDIF}
      {$IFNDEF DLL}
      DatabasePath := ExtractFilePath(Application.ExeName);
      {$ELSE}
      DatabasePath := ConfigInfo.ConfigPath;
      {$ENDIF DLL}
      DatabasePath := AddSlash(DatabasePath);
      {$IFOPT D+}SendDebug('Database path: ' + DatabasePath);{$ENDIF}
      FLayout := clSide;

      with tvTopics do
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);

      FModified := False;
      ilTreeView.ResourceLoad(rtBitmap, 'CODEDB_IMAGES', clOlive); // do not localize
      if ilTreeView.Count = 0 then
        MessageDlg(SImagesNotLoaded, mtError, [mbOK], 0);
      CenterForm(Self);
      {$IFOPT D+}SendDebug('Loading CodeLib settings');{$ENDIF}
      LoadSettings;
      {$IFOPT D+}SendDebug('Opening Database');{$ENDIF}
      CodeDB := OpenDB(DatabasePath + 'CodeDB.DB'); // do not localize
      if CodeDB = nil then
        CodeDB := CreateNewDB(DatabasePath + 'CodeDB.DB'); // do not localize
      if CodeDB = nil then
      begin
        MessageDlg(SCouldNotCreateDatabase, mtError, [mbOK], 0);
        Exit;
      end;
      {$IFOPT D+}SendDebug('Opened database');{$ENDIF}
      InitializeTreeView;
      mitPascal.Checked := True;
      FModified := False;
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.FormDestroy(Sender: TObject);
begin
  if FModified then
    SaveRecord;

  {$IFOPT D+}SendDebug('Saving code librarian settings');{$ENDIF}
  SaveSettings;

  {$IFOPT D+}SendDebug('Freeing CodeDB');{$ENDIF}
  CodeDB.Free;
  CodeDB := nil;
end;

procedure TfmCodeLib.SetLayout(New: TCodeLayout);
begin
  if FLayout <> New then
  begin
    FLayout := New;
    case FLayout of
      clSide:
        begin
          tvTopics.Align := alLeft;
          tvTopics.Width := Self.Width div 2;
          Splitter1.Align := alLeft;
          if Splitter1.Left < tvTopics.Left then
            Splitter1.Left := Self.Width;
          Splitter1.Cursor := crHSplit;
          CodeText.Align := AlClient;
        end;
      clTop:
        begin
          tvTopics.Align := alTop;
          tvTopics.Height := Self.Height div 2;
          Splitter1.Align := alTop;
          if Splitter1.Top < tvTopics.Top then
            Splitter1.Top := Self.Height;
          CodeText.Align := AlClient;
          Splitter1.Cursor := crVSplit;
        end;
    end;
  end;
end;

function TfmCodeLib.AddFolder(Node: TTreeNode; Desc: string): TTreeNode;
var
  NNode: TTreeNode;
begin
  Result := nil;
  try
    with CodeDB do
    begin
      Insert;
      {$IFOPT D+}SendDebug('Inserted a new folder');{$ENDIF}
      //FieldByName('Key').AsInteger := 1;
      if Node <> nil then
        FieldByName('Parent').AsInteger := Integer(Node.Data)  // do not localize
      else
        FieldByName('Parent').AsInteger := 0;  // do not localize
      FieldByName('Topic').AsString := Desc; // do not localize
      FieldByname('Type').AsString := 'F';  // do not localize
      Post;
      DbiSaveChanges(CodeDB.Handle);
    end;
    NNode := tvTopics.Items.AddChildObject(Node, Desc,
      Pointer(CodeDB.FieldByName('Key').AsInteger)); // do not localize
    NNode.ImageIndex := 0; // Closed folder
    NNode.SelectedIndex := 1; // Open Folder
    Result := NNode;
    SortNodes;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.SetModified(New: Boolean);
resourcestring
  SModified = 'Modified';
begin
  FModified := New;
  if FModified then
    StatusBar1.Panels[1].Text := SModified
  else
    StatusBar1.Panels[1].Text := ' '; // no need to localize
end;

function TfmCodeLib.AddCode(Desc: string): TTreenode;
var
  Node: TTreeNode;
begin
  Result := nil;
  try
    if tvTopics.Selected = nil then Exit;
    with CodeDB do
    begin
      Insert;
      //FieldByname('Key').AsInteger := 1;
      FieldByName('Parent').AsInteger := Integer(tvTopics.Selected.Data);  // do not localize
      FieldByName('Topic').AsString := Desc;  // do not localize
      FieldByName('Type').AsString := 'C';  // do not localize
      if mitPascal.Checked then
        FieldByName('Language').AsString := 'P'  // do not localize
      else
        FieldByName('Language').AsString := 'C';  // do not localize
      Post;
      DbiSaveChanges(CodeDB.Handle);
    end;
    Node := tvTopics.Items.AddChildObject(tvTopics.Selected, Desc,
      Pointer(CodeDB.FieldByName('Key').AsInteger));  // do not localize
    Node.ImageIndex := 2; // Code snippet
    Node.SelectedIndex := 2;
    Result := Node;
    SortNodes;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.New1Click(Sender: TObject);
begin
  if tvTopics.Selected = nil then
  begin
    Snippet1.Enabled := False;
    mitNewSnippet.Enabled := False;
  end
  else
  if tvTopics.Selected.ImageIndex = 2 then // Code snippet
  begin
    Snippet1.Enabled := False;
    mitNewSnippet.Enabled := False;
    Folder1.Enabled := False;
    mitNewFolder.Enabled := False;
  end
  else // in every other case...
  begin
    Snippet1.Enabled := True;
    mitNewSnippet.Enabled := True;
    Folder1.Enabled := True;
    mitNewFolder.Enabled := True;
  end;
end;

procedure TfmCodeLib.CodeTextChange(Sender: TObject);
var
  IsEnabled: Boolean;
begin
  {$IFDEF MWEDIT}
  IsEnabled := CodeText.SelAvail;
  {$ELSE MWEDIT}
  IsEnabled := (CodeText.SelLength > 0);
  {$ENDIF MWEDIT}
  sbCopy.Enabled := IsEnabled;
  sbCut.Enabled := IsEnabled;
  {$IFDEF SYNTAXMEMO}
  if smrcTEXT in CodeText.ChangeReason then
    Modified := True;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  Modified := CodeText.Modified;
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  Modified := True;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TfmCodeLib.tvTopicsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;
  // Do not alter value of AllowChange
end;

procedure TfmCodeLib.SaveRecord;
begin
  // Do not localize any of the following lines
  try
    Modified := False;
    if not CodeDB.Active then Exit;
    if tvTopics.Selected = nil then Exit;
    if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), []) then
    begin
      CodeDB.Edit;
      CodeDB.FieldByName('Topic').AsString := tvTopics.Selected.Text;
      if tvTopics.Selected.ImageIndex = 2 then  // Code snippet
      begin
        CodeDB.FieldByName('Code').Assign(CodeText.Lines);
        if mitPascal.Checked then
          CodeDB.FieldByName('Language').AsString := 'P'
        else
        if mitCPP.Checked then
          CodeDB.FieldByName('Language').AsString := 'C'
        else
        if mitHTML.Checked then
          CodeDB.FieldByName('Language').AsString := 'H'
        else
        if mitSQL.Checked then
          CodeDB.FieldByName('Language').AsString := 'S'
        else
          CodeDB.FieldByName('Language').AsString := 'N'

      end;
      CodeDB.Post;
      DbiSaveChanges(CodeDB.Handle);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  Modified := True;
  SortNodes;
end;

procedure TfmCodeLib.tvTopicsChange(Sender: TObject; Node: TTreeNode);

  procedure AssignParser(Parser: TGXSyntaxParser);
  begin
    {$IFDEF SYNTAXMEMO}
    CodeText.Parser2 := GetSyntaxParser(Parser);
    CodeText.ActiveParser := 2;
    CodeText.Gutter := 0;
    (CodeText.Parser2 as TSyntaxMemoParser).UpdateEditors;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    SetmwEditHighLighter(CodeText, Parser);
    {$ENDIF MWEDIT}
  end;

begin
  try
    if (Node <> nil) and (Node.ImageIndex = 2) then // Code snippet
    begin
      sbFolder.Enabled := False;
      sbOpen.Enabled := False;
      if CodeDB.Locate('Key', Integer(Node.Data), []) then  // do not localize
      begin
        CodeText.Lines.BeginUpdate;
        try
          {$IFDEF GX_ENHANCED_EDITOR}
          if CodeDB.FieldByName('Language').AsString = 'N' then  // do not localize
          begin
            // This is raw text
            mitNone.Checked := True;
            {$IFDEF SYNTAXMEMO}
            CodeText.ActiveParser := 1; // this implicitly assigns NO parser
            CodeText.Gutter := 0;
            {$ENDIF SYNTAXMEMO}
            {$IFDEF MWEDIT}
            SetmwEditHighLighter(CodeText, gxpNone);
            {$ENDIF MWEDIT}
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'C' then  // do not localize
          begin
            // This is CPP source code
            mitCPP.Checked := True;
            AssignParser(gxpCPP);
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'H' then  // do not localize
          begin
            // This is HTML source code
            mitHTML.Checked := True;
            AssignParser(gxpHTML);
          end
          else
          if CodeDB.FieldByName('Language').AsString = 'S' then  // do not localize
          begin
            // This is SQL code
            mitSQL.Checked := True;
            AssignParser(gxpSQL);
          end
          else
          begin
            // This is (Object) Pascal source code
            mitPascal.Checked := True;
            AssignParser(gxpPAS);
          end;
          {$ENDIF GX_ENHANCED_EDITOR}
          CodeText.Lines.Assign(CodeDB.FieldByName('Code'));  // do not localize
        finally
          CodeText.Lines.EndUpdate;
        end;
      end;
      CodeText.ReadOnly := False;
    end
    else
    begin
      sbFolder.Enabled := True;
      sbOpen.Enabled := True;
      CodeText.Lines.Clear;
      CodeText.ReadOnly := True;
    end;
    Modified := False;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.Delete1Click(Sender: TObject);

  procedure DeleteRecords(Node: TTreeNode);
  var
    RNode: TTreeNode;
  begin
    RNode := Node.GetNext;
    while (RNode <> nil) and (RNode.Level > Node.Level) do
    begin
      if CodeDB.Locate('Key', Integer(RNode.Data), []) then  // do not localize
        CodeDB.Delete;
      RNode := RNode.GetNext;
    end;
  end;

resourcestring
  SSnippet = 'snippet';
  SFolder = 'folder';
  SConfirmDelete = 'Delete this %s?';
var
  NodeType: string;
begin
  try
    if tvTopics.Selected = nil then Exit;
    if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), []) then  // do not localize
    begin
      //if CodeDB.FieldByName('System').AsInteger > 0 then
      //begin
      //  MessageDlg('You cannot delete system topics', mtError, [mbOK], 0);
      //  Exit;
      //end;
      if tvTopics.Selected.ImageIndex = 2 then // Code snippet
        NodeType := SSnippet
      else
        NodeType := SFolder;
      if MessageDlg(Format(SConfirmDelete, [NodeType]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        CodeDB.Delete;
        DeleteRecords(tvTopics.Selected);
        tvTopics.Selected.Delete;
      end;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.PrinterSetup1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfmCodeLib.Print1Click(Sender: TObject);
{$IFNDEF GX_ENHANCED_EDITOR}
var
  PrintBuf: TextFile;
  i: Integer;
{$ENDIF GX_ENHANCED_EDITOR}
{$IFDEF MWEDIT}
resourcestring
  RS_PAGENUMBER = 'Page ';
var
  Options: TmwPrintOptions;
{$ENDIF MWEDIT}
begin
  {$IFDEF SYNTAXMEMO}
    try
      CodeText.Print;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
    Options.SelectedOnly := CodeText.SelAvail;
    Options.Highlighted  := CodeText.HighLighter <> nil;
    Options.WrapLongLines:= False; // Not implemented yet!
    Options.Copies       := 1;
    Options.PrintRange   := Rect(0,0,0,0); // Not using it here.
    Options.MarginUnits  := muThousandthsOfInches; // muMillimeters; / Rect(30,30,30,30);
    Options.Margins      := Rect(500, 500, 500, 500); // 1/2 inch margins
    Options.Header       := TStringList.Create;
    Options.Footer       := TStringList.Create;
    try
      if tvTopics.Selected <> nil then
        Options.Title := tvTopics.Selected.Text;
      Options.Header.AddObject('$title$', TObject(hfaCenter));
      Options.Header.Add('');
      Options.Footer.Add('');
      Options.Footer.AddObject(RS_PAGENUMBER + '$pagenum$', TObject(hfaRight));
      CodeText.Print(nil, Options);
    finally
      Options.Header.Free;
      Options.Footer.Free;
    end;
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
    AssignPrn(PrintBuf);
    Rewrite(PrintBuf);
    try
      for i := 0 to CodeText.Lines.Count-1 do
        WriteLn(PrintBuf, CodeText.Lines[i]);
    finally
      CloseFile(PrintBuf);
    end;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TfmCodeLib.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfmCodeLib.sbCutClick(Sender: TObject);
begin
  try
    CodeText.CutToClipboard;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.sbCopyClick(Sender: TObject);
begin
  try
    CodeText.CopyToClipboard;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.sbPasteClick(Sender: TObject);
begin
  try
    if not CodeText.ReadOnly then
      CodeText.PasteFromClipBoard
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmCodeLib.SetLanguage(Sender: TObject);
begin
  Modified := True;
  {$IFDEF SYNTAXMEMO}
    if Sender = mitNone then
    begin
      CodeText.ActiveParser := 1;
    end
    else
    begin
      if Sender = mitPascal then
      begin
        CodeText.Parser2 := GetSyntaxParser(gxpPAS);
      end
      else
      if Sender = mitCPP then
      begin
        CodeText.Parser2 := GetSyntaxParser(gxpCPP);
      end
      else
      if Sender = mitHTML then
      begin
        CodeText.Parser2 := GetSyntaxParser(gxpHTML);
      end
      else
      if Sender = mitSQL then
      begin
        CodeText.Parser2 := GetSyntaxParser(gxpSQL);
      end
      else
        raise Exception.Create('Internal error selecting language');

      CodeText.ActiveParser := 2;
      (CodeText.Parser2 as TSyntaxMemoParser).UpdateEditors;
    end;

    CodeText.Gutter := 0;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
    if Sender = mitNone then
    begin
      SetmwEditHighLighter(CodeText, gxpNone);
    end
    else
    begin
      if Sender = mitPascal then
      begin
        SetmwEditHighLighter(CodeText, gxpPAS);
      end
      else
      if Sender = mitCPP then
      begin
        SetmwEditHighLighter(CodeText, gxpCPP);
      end
      else
      if Sender = mitHTML then
      begin
        SetmwEditHighLighter(CodeText, gxpHTML);
      end
      else
      if Sender = mitSQL then
      begin
        SetmwEditHighLighter(CodeText, gxpSQL);
      end
      else
        raise Exception.Create('Internal error selecting language');
    end;
  {$ENDIF MWEDIT}
  TMenuItem(Sender).Checked := True;
end;

resourcestring
  SNotForDfmFiles = 'This IDE expert is not for use in .DFM files.';

procedure TfmCodeLib.CopyfromDelphi1Click(Sender: TObject);
var
  FileName: string;
  EditRead: TEditReader;
begin
  try
    FileName := ToolServices.GetCurrentFile;
    if IsDfm(FileName) then
      raise Exception.Create(SNotForDfmFiles);

    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create(FileName);
    try
      CodeText.SelText := EditRead.GetBlock;
    finally
      EditRead.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.PasteIntoDelphi1Click(Sender: TObject);
var
  FileName: string;
  EditWrite: TEditWriter;
begin
  try
    FileName := ToolServices.GetCurrentFile;
    if IsDfm(FileName) then
      raise Exception.Create(SNotForDfmFiles);

    Screen.Cursor := crHourGlass;
    EditWrite := TEditWriter.Create(FileName);
    try
      EditWrite.WriteAtCurrentPos(CodeText.Text);
    finally
      EditWrite.Free;
      Screen.Cursor := crDefault;
    end;
    Hide;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.Edit1Click(Sender: TObject);
var
  IsEnabled: Boolean;
begin
  {$IFDEF MWEDIT}
  IsEnabled := CodeText.SelAvail;
  {$ELSE MWEDIT}
  IsEnabled := (CodeText.SelLength > 0);
  {$ENDIF MWEDIT}
  Cut1.Enabled := IsEnabled;
  Copy1.Enabled := IsEnabled;
  Paste1.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not CodeText.ReadOnly));
end;

procedure TfmCodeLib.pmCodePopup(Sender: TObject);
var
  IsEnabled: Boolean;
begin
  {$IFDEF MWEDIT}
  IsEnabled := CodeText.SelAvail;
  {$ELSE MWEDIT}
  IsEnabled := (CodeText.SelLength > 0);
  {$ENDIF MWEDIT}
  mitCut.Enabled := IsEnabled;
  mitCopy.Enabled := IsEnabled;
  mitPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not CodeText.ReadOnly));
end;

procedure TfmCodeLib.sbFindClick(Sender: TObject);
begin
  try
    with TfmCodeSearch.Create(nil) do
    try
      if ShowModal = mrOK then
      begin
        FSearch.Text := edSearch.Text;
        FSearch.CaseSensitive := cbCaseSensitive.Checked;
        FSearch.WholeWord := cbWholeWord.Checked;
        DoSearch(True);
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.DoSearch(First: Boolean);

  function DoMatch(const Text: string): Integer;
  var
    MatchPos: Integer;
  begin
    Result := -1;
    if FSearch.CaseSensitive then
      MatchPos := Pos(FSearch.Text, Text)
    else
      MatchPos := CaseInsensitivePos(FSearch.Text, Text);
    if (MatchPos > 0) and FSearch.WholeWord then
    begin
      // If the previous character is alphabetic, there isn't a match
      if MatchPos > 1 then
        if UpCase(Text[MatchPos - 1]) in ['A'..'Z'] then
          Exit;
      // If the next character is alphabetic, we didn't find a word match
      if MatchPos + Length(FSearch.Text) <= Length(Text) then
        if UpCase(Text[MatchPos + Length(FSearch.Text)]) in ['A'..'Z'] then
          Exit;
      Result := MatchPos;
    end
    else
      Result := MatchPos;
  end;

  function GetByteSelStart: Integer;
  begin
    {$IFDEF SYNTAXMEMO}
    Result := CodeText.CaretPosition.ByteOffsetFromCharOffset[CodeText.SelStart];
    Inc(Result, CodeText.Perform(EM_LINEFROMCHAR, CodeText.SelStart, 0))
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    Result := CodeText.GetSelStart;
    {$ENDIF MWEDIT}
    {$IFNDEF GX_ENHANCED_EDITOR}
    Result := CodeText.SelStart;
    {$ENDIF GX_ENHANCED_EDITOR}
  end;

var
  Node: TTreeNode;
  Match: Integer;
  InTopic: Boolean;
  FirstLoop: Boolean;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      Node := nil;
      if not First then
      begin
        //if ActiveControl = CodeText then
          Node := tvTopics.Selected
        //else
        //  Node := tvTopics.Selected.GetNext;
      end;
      if First or (Node = nil) then
        Node := tvTopics.Items.GetFirstNode;
      Match := 0;
      InTopic := False;
      FirstLoop := True;
      while Node <> nil do
      begin
        //{$IFOPT D+}SendDebug('Starting search from '+Node.Text+ ' for '+IntToStr(Integer(Node.Data)));{$ENDIF}
        if CodeDB.Locate('Key', Integer(Node.Data), []) then
        begin
          //{$IFOPT D+}SendDebug('Found the key: '+IntToStr(Integer(Node.Data)));{$ENDIF}
          {$IFDEF MWEDIT}
          if FirstLoop and (ActiveControl = CodeText) and (CodeText.SelAvail) then
          {$ELSE MWEDIT}
          if FirstLoop and (ActiveControl = CodeText) and (CodeText.SelStart + CodeText.SelLength > 0) then
          {$ENDIF MWEDIT}
          begin
            InTopic := False;
            {$IFDEF MWEDIT}
            Match := DoMatch(Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + (CodeText.GetSelEnd - CodeText.GetSelStart) , 999999));
            {$ELSE MWEDIT}
            Match := DoMatch(Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + CodeText.SelLength, 999999));
            {$ENDIF MWEDIT}
            //{$IFOPT D+}SendDebug('InterText search found '+FSearch.Text+' at '+IntToStr(Match)+' in '+Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + CodeText.SelLength, 999999));{$ENDIF}
            if Match > 0 then
            begin
              //{$IFOPT D+}SendDebug('Found a match at position '+IntToStr(Match)+'("'+Copy(Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + CodeText.SelLength, 999999), 1, 15)+'")'+' SelStart = '+IntToStr(CodeText.SelStart)+' SelLength = '+IntToStr(CodeText.SelLength));{$ENDIF}
              //CodeText.Perform(EM_LINEFROMCHAR, CodeText.SelStart, 0);
              {$IFDEF MWEDIT}
              Match := Match + GetByteSelStart + (CodeText.GetSelEnd - CodeText.GetSelStart) - 1;
              {$ELSE MWEDIT}
              Match := Match + GetByteSelStart + CodeText.SelLength - 1;
              {$ENDIF MWEDIT}
              //{$IFOPT D+}SendDebug('Matched Text: "'+Copy(CodeDB.FieldByName('Code').AsString, Match, 12)+'"');{$ENDIF}
            end;
          end
          else
          begin
            if not FirstLoop then
            begin
              InTopic := True;
              Match := DoMatch(CodeDB.FieldByName('Topic').AsString);
            end;
            //{$IFOPT D+}SendDebug('Topic match on '+CodeDB.FieldByName('Topic').AsString+' returned '+IntToStr(Match));{$ENDIF}
            if Match = 0 then
            begin
              Match := DoMatch(CodeDB.FieldByName('Code').AsString);
              InTopic := False;
              //{$IFOPT D+}SendDebug('Code match on '+CodeDB.FieldByName('Code').AsString+' returned '+IntToStr(Match));{$ENDIF}
            end;
          end;
          if Match > 0 then Break;
        end;
        Node := Node.GetNext;
        FirstLoop := False;
      end;
      if Node = nil then
        SysUtils.Beep;
      if Match > 0 then
      begin
        //{$IFOPT D+}SendDebug('Found a match!  InTopic: '+BooleanText(InTopic));{$ENDIF}
        //{$IFOPT D+}SendDebug('Match Text: '+Copy(CodeText.Lines.Text, Match, 10));{$ENDIF}
        tvTopics.Selected := Node;
        if InTopic then
          tvTopics.SetFocus
        else
        begin
          CodeText.SetFocus;
          {$IFDEF SYNTAXMEMO}
          Match := CodeText.CaretPosition.CharDistanceFromByteDistance(0, Match);
          {$ENDIF SYNTAXMEMO}
          Dec(Match);
          CodeText.Perform(EM_SETSEL, Match, Match + Length(FSearch.Text));
          {$IFDEF MWEDIT}
          CodeText.SetSelStart(Match + 1);
          CodeText.SetSelEnd(Match + Length(FSearch.Text) + 1);
          {$ENDIF MWEDIT}
          {$IFNDEF SYNTAXMEMO}
          CodeText.Perform(EM_SCROLLCARET, 0, 0);
          {$ENDIF SYNTAXMEMO}
(*
          {$IFDEF SYNTAXMEMO}
          CodeText.SelStart := Match - 1 - CodeText.Perform(EM_LINEFROMCHAR, Match, 0);
          {$ELSE SYNTAXMEMO}
          CodeText.SelStart := Match - 1;
          {$ENDIF SYNTAXMEMO}
          CodeText.SelLength := Length(FSearch.Text);
*)
          //{$IFOPT D+}SendDebug('Focused Text: '+Copy(CodeText.Lines.Text, Match - 1, 10));{$ENDIF}
        end;
      end;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmCodeLib.FindNext1Click(Sender: TObject);
begin
  if FSearch.Text <> '' then
    DoSearch(False);
end;

procedure TfmCodeLib.tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
resourcestring
  SCannotAttach = 'Subitems cannot be attached to a code snippet, only folders.';
var
  Node: TTreeNode;
begin
  try
    if tvTopics.Selected = nil then
      Exit;
    Node := tvTopics.GetNodeAt(X, Y);
    if Node = nil then
      Exit;
    if Node.ImageIndex = 2 then // Code snippet
    begin
      MessageDlg(SCannotAttach, mtError, [mbOK], 0);
      Exit;
    end;
    if CodeDB.Locate('Key', Integer(tvTopics.Selected.Data), []) then  // do not localize
    begin
      CodeDB.Edit;
      CodeDB.FieldByName('Parent').AsInteger := Integer(Node.Data);  // do not localize
      CodeDB.Post;
      DbiSaveChanges(CodeDB.Handle);
      tvTopics.Selected.MoveTo(Node, naAddChild);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Sender);
end;

procedure TfmCodeLib.sbExpandClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Expand(True);
    Node := Node.GetNextSibling;
  end;
end;

procedure TfmCodeLib.sbContractClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  // OnChanging doesn't fire under Delphi 5 when calling Collapse below
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;
  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;
  // OnChange doesn't fire under Delphi 5 when calling Collapse above
  tvTopicsChange(tvTopics, tvTopics.Selected);
end;

procedure TfmCodeLib.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the following lines
  {$IFDEF DLL}
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  {$ELSE}
  RegIni := TRegIniFile.Create('Software\GExperts');
  {$ENDIF DLL}
  try
    RegIni.WriteInteger('CodeLib', 'Left', Left);
    RegIni.WriteInteger('CodeLib', 'Top', Top);
    RegIni.WriteInteger('CodeLib', 'Width', Width);
    RegIni.WriteInteger('CodeLib', 'Height', Height);
    RegIni.WriteInteger('CodeLib', 'Layout', Ord(Layout));
    if Layout = clSide then
      RegIni.WriteInteger('CodeLib', 'Splitter', tvTopics.Width)
    else
      RegIni.WriteInteger('CodeLib', 'Splitter', tvTopics.Height);
    RegIni.WriteString('CodeLib', 'DatabasePath', DatabasePath);
  finally
    RegIni.Free;
  end;
  {$IFDEF DLL}
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts\CodeLib');
  {$ELSE}
  RegIni := TRegIniFile.Create('Software\GExperts\CodeLib');
  {$ENDIF DLL}
  try
    SaveFont(RegIni, 'Editor', CodeText.Font);
    SaveFont(RegIni, 'TreeView', tvTopics.Font);
  finally
    RegIni.Free;
  end;
end;

procedure TfmCodeLib.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the following lines
{$IFDEF DLL}
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
{$ELSE}
  RegIni := TRegIniFile.Create('Software\GExperts');
{$ENDIF DLL}
  try
    Left := RegIni.ReadInteger('CodeLib', 'Left', Left);
    Top := RegIni.ReadInteger('CodeLib', 'Top', Top);
    Width := RegIni.ReadInteger('CodeLib', 'Width', Width);
    Height := RegIni.ReadInteger('CodeLib', 'Height', Height);
    Layout := TCodeLayout(RegIni.ReadInteger('CodeLib', 'Layout', 0));
    if Layout = clSide then
      tvTopics.Width := RegIni.ReadInteger('CodeLib', 'Splitter', tvTopics.Width)
    else
      tvTopics.Height := RegIni.ReadInteger('CodeLib', 'Splitter', tvTopics.Height);
    DatabasePath := RegIni.ReadString('CodeLib', 'DatabasePath', DatabasePath);
  finally
    RegIni.Free;
  end;
{$IFDEF DLL}
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts\CodeLib');
{$ELSE}
  RegIni := TRegIniFile.Create('Software\GExperts\CodeLib');
{$ENDIF DLL}
  try
    LoadFont(RegIni, 'Editor', CodeText.Font);
    LoadFont(RegIni, 'TreeView', tvTopics.Font);
  finally
    RegIni.Free;
  end;
end;

procedure TfmCodeLib.Help2Click(Sender: TObject);
begin
{$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 17);
{$ENDIF STANDALONE}
end;

procedure TfmCodeLib.Contents1Click(Sender: TObject);
begin
{$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTENTS, 0);
{$ENDIF STANDALONE}
end;

procedure TfmCodeLib.StatusBar1Resize(Sender: TObject);
begin
  with StatusBar1 do
    Panels[0].Width := Width - Panels[1].Width - Panels[2].Width - Panels[3].Width - Panels[4].Width;
end;

procedure TfmCodeLib.Snippet1Click(Sender: TObject);
resourcestring
  SNewCode = 'New Code';
var
  Node: TTreeNode;
begin
  Screen.Cursor := crHourglass;
  try
    Node := AddCode(SNewCode);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

resourcestring
  SNewFolder = 'New Folder';

procedure TfmCodeLib.RootFolder1Click(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := AddFolder(nil, SNewFolder);
  if Node <> nil then
  begin
    tvTopics.Selected := Node;
    tvTopics.Selected.EditText;
  end;
end;

procedure TfmCodeLib.Folder1Click(Sender: TObject);
var
  Node: TTreeNode;
begin
  Screen.Cursor := crHourglass;
  try
    Node := AddFolder(tvTopics.Selected, SNewFolder);
    if Node <> nil then
    begin
      tvTopics.Selected := Node;
      tvTopics.Selected.EditText;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmCodeLib.tvTopicsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F2) and (tvTopics.Selected <> nil) then
    tvTopics.Selected.EditText;
end;

procedure TfmCodeLib.Updatedatabase1Click(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if dlgOpen.Execute then
      LoadSystemData(dlgOpen.FileName);
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmCodeLib.LoadSystemData(FileName: string);
resourcestring
  SCheckingSystemDB = 'Checking system database';
var
  SystemDB: TTable;
  Progress: TfmProgress;
  Count: Integer;

  procedure SetSystemNumbers;
  begin
    StatusBar1.Panels[0].Text := SCheckingSystemDB;
    Progress.Progress.Max := SystemDB.RecordCount;
    Application.ProcessMessages;
    SystemDB.First;
    while not SystemDB.EOF do
    begin
      SystemDB.Edit;
      SystemDB.FieldByName('System').AsInteger := SystemDB.FieldByName('Key').AsInteger;  // do not localize
      SystemDB.Post;
      SystemDB.Next;
      with Progress.Progress do
        Position := Position + 1;
    end;
    Progress.Progress.Position := 0;
  end;

  procedure LoadSystemFolders(CParent: Integer);
  var
    CurrentBookmark: TBookMark;
    rp, p: Integer;  //! StH: What is the meaning/purpose of these variables?
  begin
    CurrentBookmark := SystemDB.GetBookMark;
    try
      // do not localize any of the following lines
      if not CodeDB.Locate('System', SystemDB.FieldByName('System').AsInteger, []) then
      begin
        CodeDB.Insert;
        CodeDB.FieldbyName('System').AsInteger := SystemDB.FieldByName('System').AsInteger;
        CodeDB.FieldbyName('Type').AsString := SystemDB.FieldByName('Type').AsString;
        CodeDB.FieldbyName('Parent').AsInteger := CParent;
        CodeDB.FieldbyName('Topic').AsString := SystemDB.FieldByName('Topic').AsString;
        CodeDB.FieldbyName('Code').AsString := SystemDB.FieldByName('Code').AsString;
        CodeDB.FieldbyName('Language').AsString := SystemDB.FieldByName('Language').AsString;
        //CodeDB.FieldByName('Key').AsInteger := -1;
        CodeDB.Post;
        DbiSaveChanges(CodeDB.Handle);
      end
      else
      begin
        // Just update the database
        CodeDB.Edit;
        CodeDB.FieldByName('Topic').AsString := SystemDB.FieldByName('Topic').AsString;
        CodeDB.FieldByName('Code').AsString := SystemDB.FieldByName('Code').AsString;
        CodeDB.FieldByName('Language').AsString := SystemDB.FieldByname('Language').AsString;
        CodeDB.Post;
        DbiSaveChanges(CodeDB.Handle);
      end;
      Inc(Count);
      Progress.Progress.Position := Count;
      Application.ProcessMessages;
      p := SystemDB.FieldByName('System').AsInteger;  // do not localize
      rp := CodeDB.FieldByName('Key').AsInteger;  // do not localize
      if SystemDB.Locate('Parent', p, []) then  // do not localize
        while (SystemDB.FieldByName('Parent').AsInteger = p) and (not SystemDB.Eof) do  // do not localize
        begin
          LoadSystemFolders(rp);
          SystemDB.Next;
        end;
    finally
      SystemDB.GotoBookMark(CurrentBookmark);
      SystemDB.FreeBookMark(CurrentBookmark);
    end;
  end;

resourcestring
  SLoadingSystemDB = 'Loading system database, please wait';
begin
  try
    Screen.Cursor := crHourglass;
    Progress := TfmProgress.Create(nil);
    try
      SystemDB := TTable.Create(nil);
      try
        Progress.Parent := Self;
        Progress.Show;
        SystemDB.DatabaseName := ExtractFilePath(FileName);
        SystemDB.TableName := ExtractFileName(FileName);
        SystemDB.Open;
        SetSystemNumbers;
        SystemDB.IndexName := 'Parent';  // do not localize
        SystemDB.First;
        Count := 0;
        StatusBar1.Panels[0].Text := SLoadingSystemDB;
        while (SystemDB.FieldByName('Parent').AsInteger = 0) and (not SystemDB.Eof) do  // do not localize
        begin
          LoadSystemFolders(0);
          SystemDB.Next;
        end;

      finally
        SystemDB.Free;
        tvTopics.Items.Clear;
        InitializeTreeView;
        StatusBar1.Panels[0].Text := '';
      end;
    finally
      Progress.Free;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.Options2Click(Sender: TObject);
var
  Dlg: TfmCodeOptions;
begin
  Dlg := TfmCodeOptions.Create(nil);
  try
    with Dlg do
    begin
      edPath.Text := DatabasePath;
      if Layout = clSide then
        rbSide.Checked := True
      else
        rbTop.Checked := True;
      {$IFOPT D+}SendDebug('Setting fcTreeView.Text to '+tvTopics.Font.Name);{$ENDIF}
      fcTreeView.ItemIndex := fcTreeView.Items.IndexOf(tvTopics.Font.Name);
      {$IFOPT D+}SendDebug('fcTreeView.Text is '+fcTreeView.Text);{$ENDIF}
      udTreeView.Position := tvTopics.Font.Size;
      fcEditor.ItemIndex := fcEditor.Items.IndexOf(CodeText.Font.Name);
      udEditor.Position := CodeText.Font.Size;
    end;

    if Dlg.ShowModal = mrOK then
    begin
      if (DatabasePath <> Dlg.edPath.Text) then
      begin
        if CodeDB <> nil then
          CodeDB.Close;

        CodeDB.Free;
        CodeDB := nil;
        tvTopics.Items.Clear;
        DatabasePath := AddSlash(Dlg.edPath.Text);
        CodeDB := OpenDB(DatabasePath + 'CodeDB.DB');
        if CodeDB = nil then
          CodeDB := CreateNewDB(DatabasePath + 'CodeDB.DB');
        if CodeDB = nil then
        begin
          MessageDlg(SCouldNotCreateDatabase, mtError, [mbOK], 0);
          Exit;
        end;
        InitializeTreeView;
      end;
      if Dlg.rbSide.Checked then
        Layout := clSide
      else
        Layout := clTop;

      with tvTopics.Font do
      begin
        Name := Dlg.fcTreeView.Text;
        Size := Trunc(StrToInt(Dlg.eTreeView.Text));
      end;
      with CodeText.Font do
      begin
        Name := Dlg.fcEditor.Text;
        Size := Trunc(StrToInt(Dlg.eEditor.Text));
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TfmCodeLib.tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  AutoScroll := True;
end;

procedure TfmCodeLib.tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  AutoScroll := False;
end;

procedure TfmCodeLib.mitMakeRootClick(Sender: TObject);
begin
  with tvTopics do
  if ((Selected <> nil) and (Selected.Level > 0) and (not (Selected.ImageIndex = 2))) then
  begin
    CodeDB.Edit;
    CodeDB.FieldByName('Parent').AsInteger := 0;  // do not localize
    tvTopics.Selected.MoveTo(nil, naAdd);
    Modified := True;
    SortNodes;
  end;
end;

procedure TfmCodeLib.pmTopicsPopup(Sender: TObject);
begin
  mitMakeRoot.Enabled := False;
  if (tvTopics.Selected = nil) then
    Exit;
  mitMakeRoot.Enabled := (not (tvTopics.Selected.Level = 0)) and (not (tvTopics.Selected.ImageIndex = 2));
end;

procedure TfmCodeLib.Timer1Timer(Sender: TObject);
resourcestring
  SNumLock = 'NUM';
  SCapsLock = 'CAP';
  SScrollLock = 'SCR';
begin
  if (GetKeyState(VK_NUMLOCK) and $01) <> 0 then
    StatusBar1.Panels[3].Text := SNumLock
  else
    StatusBar1.Panels[3].Text := '';

  if (GetKeyState(VK_CAPITAL) and $01) <> 0 then
    StatusBar1.Panels[2].Text := SCapsLock
  else
    StatusBar1.Panels[2].Text := '';

  if (GetKeyState(VK_SCROLL) and $01) <> 0 then
    StatusBar1.Panels[4].Text := SScrollLock
  else
    StatusBar1.Panels[4].Text := '';
end;

function TfmCodeLib.PackTable(Tbl: TTable; db: TDatabase): DBIResult;
var
  TblDesc: CRTblDesc;
begin
  //Result:=DBIERR_NA;
  FillChar(TblDesc, SizeOf(CRTblDesc), 0);
  StrPCopy(TblDesc.szTblName, Tbl.TableName);
  TblDesc.bPack := True;
  Result := DbiDoRestructure(Db.Handle, 1, @TblDesc, nil, nil, nil, False);
end;

procedure TfmCodeLib.PackDatabase1Click(Sender: TObject);
var
  Error: DbiResult;
  ErrorMsg: string;
  Special: DBIMSG;
resourcestring
  SSuccessful = 'Successful';
  SInvalidParameter = 'The specified table name or the pointer to the table name is NULL.';
  SInvalidHandle = 'The specified database handle or cursor handle is invalid or NULL.';
  SNoSuchTable = 'Table name does not exist.';
  SUnknownTableType = 'Table type is unknown.';
  SNeedExclAccess = 'The table is not open in exclusive mode.';
begin
  try
    CodeDB.Close;
    try
      CodeDB.Exclusive := True;
      // CodeDB.Open;
      Error := PackTable(CodeDB, CodeDB.Database);
      // if not CodeDB.Database.Connected then CodeDB.Database.Open;
      // Error:= dgPackParadoxTable(CodeDB,CodeDB.Database);
      CodeDB.Active := False;
    finally
      CodeDB.Exclusive := False;
      CodeDB.Active := True;
    end;
    case Error of
      DBIERR_NONE:
        ErrorMsg := SSuccessful;
      DBIERR_INVALIDPARAM:
        ErrorMsg := SInvalidParameter;
      DBIERR_INVALIDHNDL:
        ErrorMsg := SInvalidHandle;
      DBIERR_NOSUCHTABLE:
        ErrorMsg := SNoSuchTable;
      DBIERR_UNKNOWNTBLTYPE:
        ErrorMsg := SUnknownTableType;
      DBIERR_NEEDEXCLACCESS:
        ErrorMsg := SNeedExclAccess;
    else
      DbiGetErrorString(Error, Special);
      ErrorMsg := Format('[%d]: %s', [Error, Special]); // do not localize
    end;
    MessageDlg(ErrorMsg, mtWarning, [mbOk], 0);
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmCodeLib.FormHide(Sender: TObject);
begin
  if FModified then
    SaveRecord;
  CodeDB.Close;
end;

procedure TfmCodeLib.FormShow(Sender: TObject);
begin
  if (CodeDB <> nil) and not CodeDB.Active then
    CodeDB.Open;
end;

procedure TfmCodeLib.SortNodes;
begin
  // Is there a better way?
  tvTopics.SortType := stNone;
  tvTopics.SortType := stText;
end;

procedure TfmCodeLib.tvTopicsDblClick(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
  begin
    if tvTopics.Selected.ImageIndex = 2 then // Code snippet
      PasteIntoDelphi1Click(Self);
  end;
end;


//*************************** TCodeLibExpert ******************

constructor TCodeLibExpert.Create;
begin
  inherited Create;

  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TCodeLibExpert.Destroy;
begin
  FCodeLibForm.Free;
  FCodeLibForm := nil;

  inherited Destroy;
end;

procedure TCodeLibExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    else
    begin
      if Assigned(FCodeLibForm) then
      begin
        if FCodeLibForm.Visible then
          FCodeLibForm.Close;
      end;

      FCodeLibForm.Free;
      FCodeLibForm := nil;
    end;
  end;
end;

function TCodeLibExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Code &Librarian';
begin
  Result := SMenuCaption;
end;

function TCodeLibExpert.GetMenuName: string;
begin
  Result := 'GX_CodeLib';
end;

function TCodeLibExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TCodeLibExpert.GetName: string;
begin
  Result := 'Code_Librarian';
end;

function TCodeLibExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Code Librarian';
begin
  Result := SDisplayName;
end;

procedure TCodeLibExpert.Click(Sender: TObject);
resourcestring
  SSetConfigPath = 'You must set the configuration path in the GExperts Options dialog for the Code Librarian to work.';
begin
  {$IFOPT D+}SendDebug('Activating CodeLib expert');{$ENDIF}
  if ConfigInfo.ConfigPath = '' then
  begin
    MessageDlg(SSetConfigPath, mtInformation, [mbOK], 0);
    Exit;
  end;
  {$IFOPT D+}SendDebug('Creating CodeLib form');{$ENDIF}
  if FCodeLibForm = nil then
    FCodeLibForm := TfmCodeLib.Create(nil);
  if FCodeLibForm.WindowState = wsMinimized then
    FCodeLibForm.WindowState := wsNormal;
  {$IFOPT D+}SendDebug('Showing CodeLib form');{$ENDIF}
  FCodeLibForm.Show;
end;

function TCodeLibExpert.IconFileName: string;
begin
  Result := 'CodeLib';
end;

initialization
  RegisterGX_Expert(TCodeLibExpert);

{$ELSE GX_NOBDE}
interface implementation
{$ENDIF GX_NOBDE}

end.


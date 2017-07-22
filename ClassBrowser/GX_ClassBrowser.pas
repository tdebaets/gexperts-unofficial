unit GX_ClassBrowser;

{$I GX_CondDefine.inc}

{$IFDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF SYNTAXMEMO}

{$IFDEF MWEDIT}
{$UNDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF MWEDIT}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is NOT fully compatible with C++Builder (we don't parse C++ code)

// Is there a good/easy way to add visual display of multiple interface iheritance?

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Buttons, ExtCtrls, FastSortTreeView, ComCtrls,
  GX_ClassMgr, GX_ClassParsing, StdCtrls, Registry, EditIntf, ExptIntf, ToolIntf,
  Clipbrd, Printers,
  {$IFDEF SYNTAXMEMO}
  SyntaxEd, SynParse,
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  mwCustomEdit, mwPasSyn,
  {$ENDIF MWEDIT}
  {$IFDEF ACEREPORTER}
  ACEOut, ACEFile,
  {$ENDIF ACEREPORTER}
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  GX_Experts;

type
  TViewMode = (vtList, vtTree);

  TClassProjectNotifier = class;

  TfmClassBrowser = class(TForm)
    pnlToolbar: TPanel;
    StatusBar: TStatusBar;
    Splitter1: TSplitter;
    pnlData: TPanel;
    pcMain: TPageControl;
    tshMembers: TTabSheet;
    tshInherit: TTabSheet;
    tshCode: TTabSheet;
    sbAdd: TSpeedButton;
    sbRemove: TSpeedButton;
    sbRefresh: TSpeedButton;
    sbFind: TSpeedButton;
    Panel3: TPanel;
    sbConstant: TSpeedButton;
    sbMethod: TSpeedButton;
    sbType: TSpeedButton;
    sbVariable: TSpeedButton;
    sbProperty: TSpeedButton;
    sbPrivate: TSpeedButton;
    sbProtected: TSpeedButton;
    sbPublic: TSpeedButton;
    sbPublished: TSpeedButton;
    lvInfo: TListView;
    ilClasses: TImageList;
    ilFolder: TImageList;
    Splitter2: TSplitter;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Add1: TMenuItem;
    Remove1: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    ClassReport1: TMenuItem;
    PrinterSetup1: TMenuItem;
    N6: TMenuItem;
    Exit1: TMenuItem;
    View2: TMenuItem;
    View1: TMenuItem;
    List1: TMenuItem;
    Tree1: TMenuItem;
    Options1: TMenuItem;
    Options2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Refresh1: TMenuItem;
    N3: TMenuItem;
    Properties2: TMenuItem;
    scInherit: TScrollBox;
    PrinterSetupDialog1: TPrinterSetupDialog;
    pmInfo: TPopupMenu;
    Properties3: TMenuItem;
    ClassHierarchy1: TMenuItem;
    GotoClass1: TMenuItem;
    Goto1: TMenuItem;
    N4: TMenuItem;
    UnitNames1: TMenuItem;
    Help2: TMenuItem;
    N5: TMenuItem;
    Contents1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    pnlMethod: TPanel;
    tvBrowse: TFastSortTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure tvBrowseChange(Sender: TObject; Node: TTreeNode);
    procedure pnlDataResize(Sender: TObject);
    procedure lvInfoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure pcMainChange(Sender: TObject);
    procedure List1Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Properties2Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure Options2Click(Sender: TObject);
    procedure NewFilter(Sender: TObject);
    procedure ClassReport1Click(Sender: TObject);
    procedure PrinterSetup1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sbFindClick(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure Properties3Click(Sender: TObject);
    procedure ClassHierarchy1Click(Sender: TObject);
    procedure GotoClass1Click(Sender: TObject);
    procedure Goto1Click(Sender: TObject);
    procedure UnitNames1Click(Sender: TObject);
    procedure pmInfoPopup(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    ProjectNotifier: TClassProjectNotifier;
    IsFirst: Boolean;
    ViewMode: TViewMode;
    FClassList: TClassList;
    fmProgress: TfmClassParsing;
    PrimitiveTop: Boolean;
    StayInPackage: Boolean;
    ParseRecursing: Boolean;
    AutomaticallyHideBrowser: Boolean;
    ClassKey: string;
    InheritColor: TColor;
    LoadProject: Boolean;
    StartingDir: string;
    FCurrentCodePaneFile: string;
    ClassHierarchyFontSize: Integer;
    ClassHierarchyBoxWidth: Integer;
    ClassHierarchyBoxSpace: Integer;
    ClassHierarchyFont: string;
    Filters: array[0..8] of Boolean;

    procedure EndParse(Sender: TObject);
    procedure ParseFile(FileName: string; FileIndex, FileCount: Integer);
    procedure LoadObjects(Item: TClassItem; ONode: TTreeNode);
    procedure LoadList(OInfo: TBrowseClassInfoCollection);
    procedure LoadClassList(Item: TClassItem; ONode: TTreeNode);
    procedure LoadClassTree(Item: TClassItem; ONode: TTreeNode);
    function GetMethodString(M: TBrowseMethodInfoItem): string;
    function CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
    procedure LoadCode;
    procedure GetInheritedList(List: TStringList; const StartClassName: string);
    procedure DrawInheritance;
    procedure DrawResize;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadAllObjects;
    {$IFDEF ACEREPORTER}
    procedure PrintClass(OInfo: TBrowseClassInfoCollection; AOut: TAceOutput; AceCanvas: TAceCanvas);
    procedure PreviewReport(AceFile: TAceFile);
    procedure PrintClassDiagram(OInfo: TBrowseClassInfoCollection; AOut: TAceOutput; AceCanvas: TAceCanvas; BoxSize, VSpace: Integer);
    {$ELSE ACEREPORTER}
    procedure PrintClass(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
    procedure PrintClassDiagram(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas; BoxSize, VSpace: Integer);
    {$ENDIF ACEREPORTER}
    procedure RefreshNode;
    procedure AddProject;
    procedure RemoveProject;
    procedure ClickInheritancePanel(Sender: TObject);
    function FilterTab(const Source: string): string;
  private
    {$IFDEF SYNTAXMEMO}
    MethodText: TSyntaxMemo;
    CodeText: TSyntaxMemo;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    MethodText: TmwCustomEdit;
    CodeText: TmwCustomEdit;
    {$ENDIF MWEDIT}
    {$IFNDEF GX_ENHANCED_EDITOR}
    MethodText: TRichEdit;
    CodeText: TRichEdit;
    {$ENDIF GX_ENHANCED_EDITOR}
  public
    property ClassList: TClassList read FClassList;
  end;

  TClassProjectNotifier = class(TIAddInNotifier)
  private
    fmClassBrowser: TfmClassBrowser;
  public
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
  end;

  TClassExpert = class(TGX_EnhExpert)
  private
    fmClassBrowser: TfmClassBrowser;
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
  end;

implementation

{$R *.DFM}
{$R CLASSIMG.RES}
{$R CLASSES.RES}

uses
  GX_VerDepConst,
  GX_ClassIdentify, GX_ConfigurationInfo, GX_EditReader, GX_ClassProp,
  GX_ClassOptions,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  {$IFDEF ACEREPORTER}
  GX_PrintPreview, GX_PrinterFunc,
  {$ENDIF ACEREPORTER}
  GX_ClassReport,
  GX_ClassMethProp, GX_GenFunc, GX_GExperts, CommCtrl, FileCtrl;


procedure TClassProjectNotifier.EventNotification(
  NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  // Nothing
end;

procedure TClassProjectNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    fnProjectOpened:
      begin
        with Self.fmClassBrowser do
        begin
          RemoveProject;
          if Visible then
            AddProject
          else
            LoadProject := True;
        end;
      end;
    fnProjectClosing:
      Self.fmClassBrowser.RemoveProject;
  end;
end;

procedure ClearBitmap(Bitmap: TBitmap);
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Height := 0;
    Bitmap.Width := 0;
  end;
end;

procedure TfmClassBrowser.FormCreate(Sender: TObject);
begin
  StartingDir := ExtractFilePath(Application.ExeName);
  IsFirst := True;
  LoadProject := True;
  {$IFNDEF STANDALONE}
  ClassKey := ConfigInfo.RegKey + '\GExperts';  // do not localize
  {$ELSE}
  ClassKey := '\Software\GExperts\';
  {$ENDIF STANDALONE}

  {$IFDEF SYNTAXMEMO}
  MethodText := TSyntaxMemo.Create(Self);
  CodeText := TSyntaxMemo.Create(Self);
  try
    CodeText.Parser2 := GetSyntaxParser(gxpPAS);
    MethodText.Parser2 := GetSyntaxParser(gxpPAS);

    CodeText.ActiveParser := 2;
    MethodText.ActiveParser := 2;

    (CodeText.Parser2 as TSyntaxMemoParser).UpdateEditors;

    MethodText.Options := MethodText.Options - [smoShowGutter];
    CodeText.Options := CodeText.Options - [smoShowGutter];
  except
    on E: Exception do
    begin
      // do nothing
    end;
  end;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  MethodText := TmwCustomEdit.Create(Self);
  CodeText := TmwCustomEdit.Create(Self);
  SetmwEditHighLighter(MethodText, gxpPAS);
  SetmwEditHighLighter(CodeText, gxpPAS);
  MethodText.Gutter.Width := 0;
  CodeText.Gutter.Width := 0;
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  MethodText := TRichEdit.Create(Self);
  CodeText := TRichEdit.Create(Self);
  CodeText.ScrollBars := ssBoth;
  MethodText.ScrollBars := ssBoth;
  CodeText.PlainText := True;
  MethodText.PlainText := True;
  CodeText.WordWrap := False;
  MethodText.WordWrap := False;
  {$ENDIF GX_ENHANCED_EDITOR}
  MethodText.Parent := pnlMethod;
  MethodText.Align := alClient;
  MethodText.ReadOnly := True;
  CodeText.Parent := tshCode;
  CodeText.Align := alClient;
  CodeText.ReadOnly := True;
  PrimitiveTop := False;
  StayInPackage := False;
  ParseRecursing := False;
  AutomaticallyHideBrowser := True;
  ViewMode := vtTree;
  InheritColor := clRed;
  fmProgress := nil;
  FClassList := TClassList.Create;
  FClassList.OnParseFile := ParseFile;
  FClassList.OnEndParse := EndParse;
  with tvBrowse do
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);
  ilClasses.ResourceLoad(rtBitmap, 'CLASSIMAGES', clBlue);  // do not localize
  ilFolder.ResourceLoad(rtBitmap, 'CLASSIMG', clTeal);  // do not localize
  with ilClasses do
  begin
    GetBitmap(6, sbConstant.Glyph);
    GetBitmap(7, sbMethod.Glyph);
    GetBitmap(8, sbType.Glyph);
    GetBitmap(9, sbVariable.Glyph);
    GetBitmap(10, sbProperty.Glyph);
    GetBitmap(2, sbPrivate.Glyph);
    GetBitmap(3, sbProtected.Glyph);
    GetBitmap(4, sbPublic.Glyph);
    GetBitmap(5, sbPublished.Glyph);
  end;
  LoadSettings;
  {$IFNDEF STANDALONE}
  ProjectNotifier := TClassProjectNotifier.Create;
  ProjectNotifier.fmClassBrowser := Self;
  ToolServices.AddNotifier(ProjectNotifier);
  {$ENDIF STANDALONE}
end;

procedure TfmClassBrowser.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  ClassList.SaveToFile;
  {$IFNDEF STANDALONE}
  ToolServices.RemoveNotifier(ProjectNotifier);
  ProjectNotifier.Free;
  ProjectNotifier := nil;
  // RemoveProject;
  {$ENDIF STANDALONE}
  FClassList.Free;
  FClassList := nil;
end;

procedure TfmClassBrowser.AddProject;
var
  Node: TTreeNode;
  Item: TClassItem;
begin
  Node := tvBrowse.Items.Add(nil, ExtractPureFileName(ToolServices.GetProjectName));
  Node.ImageIndex := 1;
  Node.SelectedIndex := 2;
  Item := ClassList.Add;
  with Item do
  begin
    Item.IsProject := True;
    Item.Directory := ExtractFilePath(ToolServices.GetProjectName);
    Item.Name := ExtractPureFilename(ToolServices.GetProjectName);
    if FileExists(Item.Directory + Item.Name + '.GEX') then
      Item.LoadFromFile
    else
      Item.Load;
  end;
  Node.Data := Item;
  LoadObjects(Item, Node);
end;

resourcestring
  SClassesParsed = '%d classes parsed in %g seconds';
  SSelectClassFirst = 'Please select a class in the tree first';

procedure TfmClassBrowser.RemoveProject;
var
  ONode: TTreeNode;
  Node: TTreeNode;
  Item: TClassItem;
begin
  if tvBrowse.Items.Count = 0 then
    Exit;
  Node := tvBrowse.Items[0];
  if Node = nil then
    Exit;
  while Node <> nil do
  begin
    Item := TClassItem(Node.Data);
    if Item.IsProject then
    begin
      Item.SaveToFile;
      Item.Free;
      ONode := Node.GetNextSibling;
      Node.Free;
      Node := ONode;
    end
    else
      Node := Node.GetNextSibling;
  end;
end;

procedure TfmClassBrowser.sbAddClick(Sender: TObject);

  function GetID: string;
  begin
    with TfmClassIdentify.Create(nil) do
    try
      if ShowModal = mrOK then
        Result := edtID.Text
      else
        Result := '';
    finally
      Free;
    end;
  end;

var
  New, SDir: string;
  Node: TTreeNode;
  Ticks: DWORD;
  Item: TClassItem;
begin
  try
    New := GetID;
    if New = '' then
      Exit;
    SDir := StartingDir;
    if GetDir(Self, SDir) then
    begin
      StartingDir := SDir;
      Node := tvBrowse.Items.Add(nil, New);
      Node.ImageIndex := 1;
      Node.SelectedIndex := 2;
      Ticks := GetTickCount;
      Item := ClassList.Add;
      with Item do
      begin
        Item.IsProject := False;
        Item.Directory := SDir;
        Item.Name := New;
        Item.Load;
      end;
      Node.Data := Pointer(Item);
      LoadObjects(Item, Node);
      Ticks := GetTickCount - Ticks;
      StatusBar.SimpleText := Format(SClassesParsed, [Item.ClassCount, Ticks / 1000]);
      StatusBar.Repaint;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.LoadAllObjects;
var
  i: Integer;
  Node: TTreeNode;
  AClassItem: TClassItem;
begin
  tvBrowse.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    tvBrowse.Selected := nil;
    tvBrowse.Items.Clear;
    for i := 0 to ClassList.Count-1 do
    begin
      AClassItem := ClassList.Items[i];
      Node := tvBrowse.Items.AddObject(nil, AClassItem.Name, AClassItem);
      with Node do
      begin
        ImageIndex := 1;
        SelectedIndex := 2;
      end;
      LoadObjects(AClassItem, Node);
    end;
  finally
    tvBrowse.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.LoadObjects(Item: TClassItem; ONode: TTreeNode);
resourcestring
  SSorting = 'Sorting...';
begin
  tvBrowse.Items.BeginUpdate;
  Screen.Cursor := crHourglass;
  tvBrowse.SortType := stNone;
  try
    tvBrowse.Selected := nil;
    // while ONode.Count > 0 do
    //  ONode.Item[0].Free;
    ONode.DeleteChildren;
    case ViewMode of
      vtList: LoadClassList(Item, ONode);
      vtTree: LoadClassTree(Item, ONode);
    else
      Assert(False, 'Unknown view mode!');
    end;
  finally
    StatusBar.SimpleText := SSorting;
    StatusBar.Repaint;
    tvBrowse.SortType := ComCtrls.stText;
    Screen.Cursor := crDefault;
    tvBrowse.Items.EndUpdate;
    StatusBar.SimpleText := '';
  end;
end;

procedure TfmClassBrowser.LoadClassTree(Item: TClassItem; ONode: TTreeNode);
var
  TempClassList: TStringList;
  Node: TTreeNode;
  i: Integer;
  IncludeUnitName: Boolean;

  procedure AddObjects(INode: TTreeNode);
  var
    j: Integer;
    ANode: TTreeNode;
    NodeText: string;
    AClassItem: TBrowseClassInfoCollection;
  begin
    j := TempClassList.Count-1;
    while j >= 0 do
    begin
      AClassItem := TBrowseClassInfoCollection(TempClassList.Objects[j]);

      if CompareText(AClassItem.ObjectDerivedFrom, TBrowseClassInfoCollection(INode.Data).Name) = 0 then
      begin
        if IncludeUnitName then
        begin
          with AClassItem do
            NodeText := UnitName + '.' + Name;
        end
        else
          NodeText := AClassItem.Name;

        ANode := tvBrowse.Items.AddChildObject(INode, NodeText, AClassItem);
        ANode.ImageIndex := 0;
        ANode.SelectedIndex := 0;

        TempClassList.Delete(j);

        AddObjects(ANode);
        j := TempClassList.Count-1;
      end
      else
        Dec(j);
    end;
  end;

var
  NodeText: string;
  ClassInfoCollItem: TBrowseClassInfoCollection;
begin
  IncludeUnitName := UnitNames1.Checked;

  TempClassList := TStringList.Create;
  try
    for i := 0 to Item.ClassCount - 1 do
    begin
      ClassInfoCollItem := Item.ClassItem[i];
      TempClassList.AddObject(ClassInfoCollItem.Name, ClassInfoCollItem);
    end;

    // First add root items
    i := TempClassList.Count-1;
    while i >= 0 do
    begin
      ClassInfoCollItem := TBrowseClassInfoCollection(TempClassList.Objects[i]);

      if Item.ObjectByName(ClassInfoCollItem.ObjectDerivedFrom) = nil then
      begin
        // This is a root item - ObjectDerivedFrom = nil --> no ancestor
        if IncludeUnitName then
        begin
          with ClassInfoCollItem do
            NodeText := UnitName + '.' + Name;
        end
        else
          NodeText := ClassInfoCollItem.Name;

        Node := tvBrowse.Items.AddChildObject(ONode, NodeText, ClassInfoCollItem);
        Node.ImageIndex := 0;
        Node.SelectedIndex := 0;

        TempClassList.Delete(i);

        AddObjects(Node);
        i := TempClassList.Count-1;
      end
      else
        Dec(i);
    end;

    Assert(TempClassList.Count = 0, 'Bad algorithm building tree');

  finally
    TempClassList.Free;
  end;
end;

procedure TfmClassBrowser.LoadClassList(Item: TClassItem; ONode: TTreeNode);
var
  INode: TTreeNode;
  i: Integer;
  NodeText: string;
  IncludeUnitName: Boolean;
begin
  IncludeUnitName := UnitNames1.Checked;
  for i := 0 to Item.ClassCount-1 do
  begin
    if IncludeUnitName then
    begin
      with Item.ClassItem[i] do
        NodeText := UnitName + '.' + Name;
    end
    else
      NodeText := Item.ClassItem[i].Name;

    INode := tvBrowse.Items.AddChildObject(ONode, NodeText, Item.ClassItem[i]);
    INode.ImageIndex := 0;
    INode.SelectedIndex := 0;
  end;
end;

procedure TfmClassBrowser.ParseFile(FileName: string; FileIndex, FileCount: Integer);
resourcestring
  SParsingProgress = 'Parsing classes in %s ...';
begin
{$DEFINE DoShowProgressForm}
{$IFDEF DoShowProgressForm}
  if fmProgress = nil then
  begin
    fmProgress := TfmClassParsing.Create(Self);
    with fmProgress do
    begin
      Progress.Position := 0;
      Progress.Min := 0;
      Progress.Max := FileCount;
      Show;
    end;
  end;
  if fmProgress <> nil then
  begin
    fmProgress.lblParsing.Caption := Format(SParsingProgress, [ExtractFilename(Filename)]);
    fmprogress.Progress.Position := FileIndex;
  end;
  Application.ProcessMessages;
{$ENDIF DoShowProgressForm}
end;

procedure TfmClassBrowser.EndParse(Sender: TObject);
begin
  fmProgress.Free;
  fmProgress := nil;
end;

procedure TfmClassBrowser.LoadList(OInfo: TBrowseClassInfoCollection);
const
  {$IFDEF GX_VER130_up}
  EmptyImage = -1;
  {$ELSE GX_VER130_up}
  EmptyImage = 11;
  {$ENDIF GX_VER130_up}
var
  AMethod: TBrowseMethodInfoItem;
  i: Integer;
  ListItem: TListItem;
  {$IFNDEF GX_VER130_up}
  Styles: DWORD;
  {$ENDIF GX_VER130_up}


  procedure SetSubItemImage(const ListItem: TListItem; SubItem, Value: Integer);
  {$IFNDEF GX_VER130_up}
  var
    LvItem: TLVItem;
  {$ENDIF GX_VER130_up}
  begin
    {$IFDEF GX_VER130_up}
    ListItem.SubItemImages[SubItem] := Value;
    {$ELSE GX_VER130_up}
    LvItem.iItem := ListItem.Index;
    LvItem.iSubItem := SubItem+1;
    LvItem.mask := LVIF_IMAGE;
    LvItem.iImage := Value;
    ListView_SetItem(ListItem.Handle, LvItem);
    {$ENDIF GX_VER130_up}
  end;

begin
  try
    lvInfo.Items.BeginUpdate;
    try
      lvInfo.Items.Clear;
      // Only Delphi 5+ support SubItemImages
      {$IFNDEF GX_VER130_up}
      if LvInfo.HandleAllocated then
      begin
        Styles := ListView_GetExtendedListViewStyle(LvInfo.Handle);
        Styles := Styles or LVS_EX_SUBITEMIMAGES;
        ListView_SetExtendedListViewStyle(LvInfo.Handle, Styles);
      end;
      {$ENDIF GX_VER130_up}

      MethodText.Lines.Clear;
      for i := 0 to OInfo.Count-1 do
      begin
        AMethod := OInfo.Items[i];
        if CheckFilter(AMethod) then
        begin
          listItem := lvInfo.Items.Add;
          ListItem.Caption := '';
          ListItem.ImageIndex       := Ord(AMethod.MethodDeclare) + 2;
          ListItem.SubItems.Add('');
          SetSubItemImage(ListItem, 0, Ord(AMethod.MethodType) + 6);
          ListItem.SubItems.Add('');
          if AMethod.cVirtual then
            SetSubItemImage(ListItem, 1, 12)
          else
            SetSubItemImage(ListItem, 1, EmptyImage);
          ListItem.SubItems.Add('');
          if AMethod.cAbstract then
            SetSubItemImage(ListItem, 2, 12)
          else
            SetSubItemImage(ListItem, 2, EmptyImage);
          ListItem.SubItems.Add('');
          if AMethod.cOverride then
            SetSubItemImage(ListItem, 3, 12)
          else
            SetSubItemImage(ListItem, 3, EmptyImage);
          ListItem.SubItems.Add(StripWhiteSpace(AMethod.DName));
          ListItem.Data := AMethod;
        end;
      end;
    finally
      lvInfo.Items.EndUpdate;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TfmClassBrowser.GetMethodString(M: TBrowseMethodInfoItem): string;
begin
  Result := '';
  Result := IntToStr(Ord(M.MethodDeclare)) + #9 + IntToStr(Ord(M.MethodType)) + #9 +
            IntToStr(Ord(M.cVirtual)) + #9 + IntToStr(Ord(M.cAbstract)) + #9 +
            IntToStr(Ord(M.cOverride)) + #9 + FilterTab(M.DName);
end;

procedure TfmClassBrowser.tvBrowseChange(Sender: TObject; Node: TTreeNode);
resourcestring
  SSourceModule = 'Source module: %s  (%d ms)';  // <-- remove (%d), only used for timing
var
  OInfo: TBrowseClassInfoCollection;
  TimeSpent: DWORD;
begin
  Screen.Cursor := crHourglass;
  try
    if Node = nil then
      sbRemove.Enabled := False
    else
      sbRemove.Enabled := (Node.Level = 0);
    lvInfo.Items.BeginUpdate;
    lvInfo.Items.Clear;
    lvInfo.Items.EndUpdate;
    if (tvBrowse.Selected <> nil) and (tvBrowse.Selected.Data <> nil) then
      if tvBrowse.Selected.Level > 0 then
      begin
        OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
        if not OInfo.IsLoaded then OInfo.LoadMethods;
        TimeSpent := GetTickCount;
        if pcMain.ActivePage = tshCode then LoadCode;
        if pcMain.ActivePage = tshInherit then DrawInheritance;
        TimeSpent := GetTickCount - TimeSpent;
        LoadList(OInfo);
        StatusBar.SimpleText := Format(SSourceModule, [OInfo.UnitName, TimeSpent]);
      end
      else
      begin
        StatusBar.SimpleText := '';
        lvInfo.Items.BeginUpdate;
        lvInfo.Items.Clear;
        lvInfo.Items.EndUpdate;
        MethodText.Lines.Clear;
        CodeText.Lines.Clear;
        FCurrentCodePaneFile := '';
        while scInherit.ControlCount > 0 do
          scInherit.Controls[0].Free;
      end;
  finally
    Screen.Cursor := crDefault;
  end;
  pnlDataResize(Self);
end;

function TfmClassBrowser.CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
begin
  Result := True;
  case MInfo.MethodType of
    ctConstant:   Result := sbConstant.Down;
    ctMethod:     Result := sbMethod.Down;
    ctType:       Result := sbType.Down;
    ctVariable:   Result := sbVariable.Down;
    ctProperty:   Result := sbProperty.Down;
  end;
  if Result then
    case MInfo.MethodDeclare of
      cdPrivate:   Result := sbPrivate.Down;
      cdProtected: Result := sbProtected.Down;
      cdPublic:    Result := sbPublic.Down;
      cdPublished: Result := sbPublished.Down;
    end;
end;

procedure TfmClassBrowser.pnlDataResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
  with lvInfo do
  begin
    if lvInfo.ClientWidth > 50 then
    begin
      Columns[5].Width := Max(lvInfo.ClientWidth - Columns[0].Width - Columns[1].Width -
        Columns[2].Width - Columns[3].Width - Columns[4].Width, 0);
    end;
  end;
  tshMembers.Width := pcMain.ActivePage.Width;
  tshMembers.Height := pcMain.ActivePage.Height;
  DrawResize;
end;

procedure TfmClassBrowser.DrawResize;
var
  i: Integer;
  W: Integer;
  ShapeLeft: Integer;
  L: Integer;
begin
  SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(False), 0);
  try
    tshInherit.Width := pcMain.ActivePage.Width;
    tshInherit.Height := pcMain.ActivePage.Height;
    scInherit.Width := tshInherit.Width;
    scInherit.Height := tshInherit.Height;
    W := scInherit.Width - 20;
    if W > 300 then W := 300;
    L := (scInherit.Width - W) div 2;
    ShapeLeft := L + (W div 2) - 2;
    for i := 0 to scInherit.ControlCount - 1 do
      if scInherit.Controls[i] is TPanel then
        with TPanel(scInherit.Controls[i]) do
          SetBounds(L, Top, W, Height)
      else
        if scInherit.Controls[i] is TShape then
          with TShape(scInherit.Controls[i]) do
            SetBounds(ShapeLeft, Top, 2, 20);
  finally
    SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(True), 0);
    for i := 0 to scInherit.ControlCount - 1 do
       scInherit.Controls[i].Invalidate;
    scInherit.Invalidate;
  end;
end;

procedure TfmClassBrowser.lvInfoChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  InfoItem: TBrowseMethodInfoItem;
begin
  MethodText.Text := '';
  if lvInfo.Selected <> nil then
  begin
    with lvInfo do
      InfoItem := TBrowseMethodInfoItem(lvInfo.Selected.Data);
    if InfoItem <> nil then
    begin
      MethodText.Text := StripWhiteSpace(FilterTab(InfoItem.DName));
      StatusBar.SimpleText := StripWhiteSpace(FilterTab(MethodText.Text));
    end;
  end;
end;

procedure TfmClassBrowser.LoadCode;
var
  FileFreshLoad: Boolean; //! StH: Includes work-around for TSyntaxMemo problem

  procedure LoadCodePane(const F: string);
  var
    EditRead: TEditReader;
    AMemoryStream: TMemoryStream;
  begin
    FileFreshLoad := (FCurrentCodePaneFile = '');

    if FCurrentCodePaneFile = F then
    begin
      if CodeText.Lines.Count > 2 then
        Exit;
    end
    else
      FCurrentCodePaneFile := F;

    CodeText.Lines.Clear;
    EditRead := TEditReader.Create(F);
    try
      EditRead.BufSize := 29 * 1024;
      AMemoryStream := TMemoryStream.Create;
      try
        EditRead.SaveToStream(AMemoryStream);
        AMemoryStream.Position := 0;
        CodeText.Lines.LoadFromStream(AMemoryStream);
      finally
        AMemoryStream.Free;
      end;
    finally
      EditRead.Free;
    end;
  end;

var
  LineNumber: Integer;
  SourceFileName: string;
  MInfo: TBrowseMethodInfoItem;
begin
  try
    Screen.Cursor := crHourglass;
    CodeText.Lines.BeginUpdate;
    try
      if tvBrowse.Selected = nil then Exit;
      if tvBrowse.Selected.Level = 0 then Exit;
      SourceFileName := TBrowseClassInfoCollection(tvBrowse.Selected.Data).FileName;
      LineNumber := -1;
      if lvInfo.Selected = nil then
        LineNumber := TBrowseClassInfoCollection(tvBrowse.Selected.Data).RefreshLineNo
      else
      begin
        MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
        case MInfo.MethodType of
          ctConstant,
          ctType,
          ctVariable,
          ctProperty:   LineNumber := MInfo.GetInterfaceLine;
          ctMethod:     LineNumber := MInfo.GetImplementationLine;
        end;
      end;

      LoadCodePane(SourceFileName);

      if LineNumber >= 0 then
      begin
        {$IFDEF SYNTAXMEMO}
        //! StH: This should work around a bug/issue in TSyntaxMemo
        //! It apparently does not like the combination of LOADING
        //! and then immediately jumping to another position with
        //! BeginUpdate enabled, if the memo was EMPTY.
        if FileFreshLoad then
        begin
          CodeText.Lines.EndUpdate; // it is safe to call EndUpdate more than once
          {$IFOPT D+} SendDebug('Working around TSyntaxMemo problem in class browser'); {$ENDIF D+}
        end;

        CodeText.CaretPosition.TopLine := LineNumber+1;
        CodeText.CaretPosition.Row := LineNumber+1;
        CodeText.CaretPosition.OffsetColumn := 1;
        {$ENDIF SYNTAXMEMO}
        {$IFDEF MWEDIT}
        CodeText.TopLine := LineNumber + 1;
        CodeText.CaretXY := Point(1, LineNumber + 1);
        {$ENDIF MWEDIT}
        {$IFNDEF GX_COMP_PACKAGE}
        SendMessage(CodeText.Handle, EM_LINESCROLL, 0, -9999);
        SendMessage(CodeText.Handle, EM_LINESCROLL, 0, LineNumber);
        {$ENDIF GX_COMP_PACKAGE}
      end;
    finally
      CodeText.Lines.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tshCode then
    LoadCode
  else
    if pcMain.ActivePage = tshInherit then
      DrawInheritance;
end;

procedure TfmClassBrowser.List1Click(Sender: TObject);
var
  TempViewMode: TViewMode;
begin
  try
    tvBrowse.Selected := nil;
    TempViewMode := TViewMode(TMenuItem(Sender).Tag);
    if TempViewMode = ViewMode then Exit;
    ViewMode := TempViewMode;

    LoadAllObjects;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.View1Click(Sender: TObject);
begin
  List1.Checked := (ViewMode = vtList);
  Tree1.Checked := (ViewMode = vtTree);
end;

procedure TfmClassBrowser.Properties2Click(Sender: TObject);
var
  OInfo: TBrowseClassInfoCollection;
begin
  if tvBrowse.Selected = nil then Exit;
  if tvBrowse.Selected.Level = 0 then Exit;
  OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
  with TfmClassProp.Create(Self) do
  try
    edtClassName.Text := OInfo.Name;
    edtDerivedFrom.Text := OInfo.DerivedFrom;
    mmoFileName.Text := OInfo.FileName;
    edtLineNo.Text := IntToStr(OInfo.LineNo);
    edtUnit.Text := OInfo.UnitName;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmClassBrowser.PopupMenu1Popup(Sender: TObject);
begin
  GotoClass1.Enabled := True;
  Properties2.Enabled := True;
  Refresh1.Enabled := True;
  if tvBrowse.Selected = nil then
  begin
    Properties2.Enabled := False;
    Refresh1.Enabled := False;
    GotoClass1.Enabled := False;
  end;
  if tvBrowse.Selected.Level = 0 then
  begin
    Properties2.Enabled := False;
    GotoClass1.Enabled := False;
  end;
end;

procedure TfmClassBrowser.Refresh1Click(Sender: TObject);
begin
  if tvBrowse.Selected = nil then Exit;
  if tvBrowse.Selected.Level = 0 then
    RefreshNode
  else
  begin
    TBrowseClassInfoCollection(tvBrowse.Selected.Data).LoadMethods;
    tvBrowseChange(tvBrowse, tvBrowse.Selected);
  end;
end;

procedure TfmClassBrowser.RefreshNode;
var
  Ticks: DWORD;
  Item: TClassItem;
begin
  Screen.Cursor := crHourglass;
  try
    Ticks := GetTickCount;
    Item := TClassItem(tvBrowse.Selected.Data);
    Item.Recurse := ParseRecursing;
    Item.Load;
    LoadObjects(Item, tvBrowse.Selected);
    StatusBar.SimpleText := Format(SClassesParsed, [Item.ClassCount, (GetTickCount - Ticks) / 1000]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.GetInheritedList(List: TStringList; const StartClassName: string);
var
  LastVisitedLevel: Integer;
  FirstNode: TTreeNode;

  function GetInherited(const Comp: string): string;
  var
    OInfo: TBrowseClassInfoCollection;
    Node: TTreeNode;
  begin
    Result := '';
    Node := FirstNode;
    while Node <> nil do
    begin
      if Node.Level > LastVisitedLevel then
      begin
        if CompareText(TBrowseClassInfoCollection(Node.Data).Name, Comp) = 0 then
        begin
          OInfo := TBrowseClassInfoCollection(Node.Data);
          Result := OInfo.DerivedFrom;
          // We could instead recurse the treeview backwards to get
          // this information if we are in "tree" mode
          if StayInPackage then
            LastVisitedLevel := Node.Level;
          Break;
        end;
      end;
      Node := Node.GetNext;
    end;
  end;

var
  InheritingFromClassName: string;
  CommaPosition: Integer;
begin
  FirstNode := tvBrowse.Items.GetFirstNode;
  LastVisitedLevel := FirstNode.Level;
  try
    InheritingFromClassName := GetInherited(StartClassName);
    while InheritingFromClassName <> '' do
    begin
      List.Add(InheritingFromClassName);
      CommaPosition := Pos(',', InheritingFromClassName);
      // With multiple ancestors, we want to search for the first one
      if CommaPosition > 0 then
        InheritingFromClassName := Copy(InheritingFromClassName, 1, CommaPosition - 1);

      InheritingFromClassName := GetInherited(InheritingFromClassName);
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.ClickInheritancePanel(Sender: TObject);
//var
//  Panel: TPanel;
begin
  // Panel := Sender as TPanel;
  // Jump to the class declaration, or treeview node?
end;

procedure TfmClassBrowser.DrawInheritance;
var
  InheritList: TStringList;
  i: Integer;
  ClassIndex: Integer;
  ClassControlLeft: Integer;
  ClassControlWidth: Integer;
  ShapeLeft: Integer;
  DrawClassName: string;
begin
  try
    if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then Exit;
    // Get the list of ancestor classes
    InheritList := TStringList.Create;
    Screen.Cursor := crHourglass;
    try
      DrawClassName := TBrowseClassInfoCollection(tvBrowse.Selected.Data).Name;
      InheritList.Add(DrawClassName);
      GetInheritedList(Inheritlist, DrawClassName);

      scInherit.DisableAutoRange;
      SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(False), 0);
      try
        while scInherit.ControlCount > 0 do
          scInherit.Controls[0].Free;

        // Set up parameters for drawing the respective class items
        ClassControlWidth := (scInherit.Width - 20);
        if ClassControlWidth > 300 then ClassControlWidth := 300;
        ClassControlLeft := (scInherit.Width - ClassControlWidth) div 2;
        ShapeLeft := ClassControlLeft + (ClassControlWidth div 2) - 2;

        if PrimitiveTop then
          ClassIndex := 0
        else
          ClassIndex := InheritList.Count - 1;

        for i := InheritList.Count - 1 downto 0 do
        begin
          with TPanel.Create(scInherit) do
          begin
            Visible := True;
            Parent := scInherit;
            OnClick := ClickInheritancePanel;
            SendMessage(Handle, WM_SETREDRAW, Integer(True), 0);
            BorderWidth := 1;
            FullRepaint := False;
            Caption := InheritList.Strings[ClassIndex];
            BevelInner := bvLowered;
            SetBounds(ClassControlLeft, (i * 50) + 10, ClassControlWidth, 30);
            if {Form.}Canvas.TextWidth(Caption) > ClientWidth - (2*BorderWidth) then
            begin
              ShowHint := True;
              Hint := Caption;
            end;
          end;
          if i > 0 then
          begin
            with TShape.Create(scInherit) do
            begin
              Visible := True;
              Parent := scInherit;
              SetBounds(ShapeLeft, ((i - 1) * 50) + 40, 2, 20);
            end;
          end;
          if PrimitiveTop then
            Inc(ClassIndex)
          else
            Dec(ClassIndex);
        end; // for i :=

      finally
        scInherit.EnableAutoRange;
        SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(True), 0);
        for i := 0 to scInherit.ControlCount-1 do
        begin
          scInherit.Controls[i].Invalidate;
        end;
        scInherit.Invalidate;
      end;

    finally
      InheritList.Free;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.SaveSettings;
var
  RegIni: TRegIniFile;
  i: Integer;
begin
  try
    // do not localize any of the following lines
    RegIni := TRegIniFile.Create(ClassKey);
    try
      RegIni.WriteInteger('ClassBrowse', 'Left', Left);
      RegIni.WriteInteger('ClassBrowse', 'Top', Top);
      RegIni.WriteInteger('ClassBrowse', 'Width', Width);
      RegIni.WriteInteger('ClassBrowse', 'Height', Height);
      RegIni.WriteInteger('ClassBrowse', 'Split', tvBrowse.Width);
      RegIni.WriteInteger('ClassBrowse', 'ViewMode', Ord(ViewMode));
      RegIni.WriteString('ClassBrowse', 'StoragePath', AddSlash(ClassList.StoragePath));
      RegIni.WriteBool('ClassBrowse', 'PrimitiveTop', PrimitiveTop);
      RegIni.WriteBool('ClassBrowse', 'StayInPackage', StayInPackage);
      RegIni.WriteBool('ClassBrowse', 'ParseRecursing', ParseRecursing);
      RegIni.WriteBool('ClassBrowse', 'AutomaticallyHideBrowser', AutomaticallyHideBrowser);
      RegIni.WriteBool('ClassBrowse', 'Unitnames', UnitNames1.Checked);
      RegIni.WriteInteger('ClassBrowse', 'ClassHierarchyFontSize', ClassHierarchyFontSize);
      RegIni.WriteInteger('ClassBrowse', 'ClassHierarchyBoxWidth', ClassHierarchyBoxWidth);
      RegIni.WriteInteger('ClassBrowse', 'ClassHierarchyBoxSpace', ClassHierarchyBoxSpace);
      RegIni.WriteString('ClassBrowse', 'ClassHierarchyFont', 'Arial');
      for i := Low(Filters) to High(Filters) do
        RegIni.WriteBool('ClassBrowse', 'Filter' + IntToStr(i), Filters[i]);

      SaveFont(RegIni, 'ClassBrowse\ClassFont1', tvBrowse.Font);
      SaveFont(RegIni, 'ClassBrowse\ClassFont2', lvInfo.Font);
      SaveFont(RegIni, 'Editor', CodeText.Font);
    finally
      RegIni.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.LoadSettings;
var
  RegIni: TRegIniFile;
  TempStoragePath: string;
  i: Integer;
begin
  try
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    // do not localize any of the following lines
    RegIni := TRegIniFile.Create(ClassKey);
    try
      Left := RegIni.ReadInteger('ClassBrowse', 'Left', Left);
      Top := RegIni.ReadInteger('ClassBrowse', 'Top', Top);
      Width := RegIni.ReadInteger('ClassBrowse', 'Width', Width);
      Height := RegIni.ReadInteger('ClassBrowse', 'Height', Height);
      tvBrowse.Width := RegIni.ReadInteger('ClassBrowse', 'Split', tvBrowse.Width);
      if tvBrowse.Width = 0 then tvBrowse.Width := 100;
      ViewMode := TViewMode(RegIni.ReadInteger('ClassBrowse', 'ViewMode', Ord(ViewMode)));
      UnitNames1.Checked := RegIni.ReadBool('ClassBrowse', 'Unitnames', False);
      TempStoragePath := RegIni.ReadString('ClassBrowse', 'StoragePath', '');
      ClassHierarchyFontSize := RegIni.ReadInteger('ClassBrowse', 'ClassHierarchyFontSize', 8);
      ClassHierarchyBoxWidth := RegIni.ReadInteger('ClassBrowse', 'ClassHierarchyBoxWidth', 25);
      ClassHierarchyBoxSpace := RegIni.ReadInteger('ClassBrowse', 'ClassHierarchyBoxSpace', 10);
      ClassHierarchyFont := RegIni.ReadString('ClassBrowse', 'ClassHierarchyFont', 'Arial');

      if TempStoragePath <> '' then
        ClassList.StoragePath := AddSlash(TempStoragePath)
      else
      begin
        {$IFDEF STANDALONE}
        ClassList.StoragePath := AddSlash(ExtractFilePath(Application.ExeName));
        Exit;
        {$ELSE}
        ClassList.StoragePath := AddSlash(ConfigInfo.ConfigPath + ClassBrowserStorageFolder);
        {$ENDIF STANDALONE}
      end;
      PrimitiveTop := RegIni.ReadBool('ClassBrowse', 'PrimitiveTop', PrimitiveTop);
      StayInPackage := RegIni.ReadBool('ClassBrowse', 'StayInPackage', StayInPackage);
      ParseRecursing := RegIni.ReadBool('ClassBrowse', 'ParseRecursing', ParseRecursing);
      AutomaticallyHideBrowser := RegIni.ReadBool('ClassBrowse', 'AutomaticallyHideBrowser', AutomaticallyHideBrowser);
      for i := Low(Filters) to High(Filters) do
        Filters[i] := RegIni.ReadBool('Class Browser', 'Filter' + IntToStr(i), True);  // do not localize

      LoadFont(RegIni, 'ClassBrowse\ClassFont1', tvBrowse.Font);
      LoadFont(RegIni, 'ClassBrowse\ClassFont2', lvInfo.Font);
      LoadFont(RegIni, 'Editor', CodeText.Font);
    finally
      RegIni.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.Options2Click(Sender: TObject);
var
  Dlg: TfmClassOptions;
  i: Integer;
begin
  try
    Dlg := TfmClassOptions.Create(Self);
    try
      with Dlg do
      begin
        edStorage.Text := ClassList.StoragePath;
        cbTreeView.ItemIndex := cbTreeView.Items.IndexOf(tvBrowse.Font.Name);
        cbListView.ItemIndex := cbListView.Items.IndexOf(lvInfo.Font.Name);
        cbEditor.ItemIndex := cbEditor.Items.IndexOf(CodeText.Font.Name);
        sTreeView.Value := tvBrowse.Font.Size;
        sListView.Value := lvInfo.Font.Size;
        sEditor.value := CodeText.Font.Size;
        cbTop.Checked := PrimitiveTop;
        cbStayInPackage.Checked := StayInPackage;
        cbParseRecursing.Checked := ParseRecursing;
        cbAutoHide.Checked := AutomaticallyHideBrowser;
      end;
      { Set Filters }
      for i := 0 to Dlg.tshFilters.ControlCount - 1 do
        if Dlg.tshFilters.Controls[i] is TCheckBox then
        begin
          Tag := TCheckBox(Dlg.tshFilters.Controls[i]).Tag;
          TCheckBox(Dlg.tshFilters.Controls[i]).Checked := Filters[Tag];
        end;
      if Dlg.ShowModal = mrOK then
      begin
        if Dlg.edStorage.Text <> ClassList.StoragePath then
        begin
          ClassList.StoragePath := AddSlash(Dlg.edStorage.Text);
          LoadAllObjects;
        end;
        for i := 0 to Dlg.tshFilters.ControlCount - 1 do
          if Dlg.tshFilters.Controls[i] is TCheckBox then
          begin
            Tag := TCheckBox(Dlg.tshFilters.Controls[i]).Tag;
            Filters[Tag] := TCheckBox(Dlg.tshFilters.Controls[i]).Checked;
          end;
        with Dlg do
        begin
          tvBrowse.Font.Name := cbTreeView.Text;
          lvInfo.Font.Name := cbListView.Text;
          CodeText.Font.Name := cbEditor.Text;
          CodeText.Font.Size := Trunc(sEditor.Value);
          tvBrowse.Font.Size := Trunc(sTreeView.Value);
          lvInfo.Font.Size := Trunc(sListView.Value);
          PrimitiveTop := cbTop.Checked;
          StayInPackage := cbStayInPackage.Checked;
          ParseRecursing := cbParseRecursing.Checked;
          AutomaticallyHideBrowser := cbAutoHide.Checked;
        end;
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

procedure TfmClassBrowser.NewFilter(Sender: TObject);
var
  OInfo: TBrowseClassInfoCollection;
begin
  if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
    Exit;
  Self.Cursor := crHourglass;
  try
    //lvInfo.Items.Clear;
    OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
    LoadList(OInfo);
    StatusBar.SimpleText := OInfo.UnitName + ': ' + IntToStr(OInfo.LineNo);
  finally
    Self.Cursor := crDefault;
  end;
end;

{$IFNDEF ACEREPORTER}
procedure TfmClassBrowser.ClassReport1Click(Sender: TObject);
resourcestring
  SClassReport = 'Class Report';
var
  OInfo: TBrowseClassInfoCollection;
begin
  try
    if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
      Exit;
    OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
    Screen.Cursor := crHourglass;
    Printer.Title := SClassReport;
    Printer.BeginDoc;
    try
      PrintClass(OInfo, Printer.Canvas);
    finally
      Printer.EndDoc;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.PrintClass(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
resourcestring
  SName = 'Name';
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;
var
  Row: Integer;
  i: Integer;
  List: TStringList;
  MInfo: TBrowseMethodInfoItem;
  FontHeight: Integer;
  ColumnWidth: Integer;
  BitmapSize: Integer;
  Bitmap: TBitmap;

  procedure PrintHeader;
  begin
    with ACanvas do
    begin
      Font.Style := [fsBold];
      TextOut(PR_OffsetX, Row, 'Vi');
      TextOut(PR_OffsetX + ColumnWidth, Row, 'Ty');
      TextOut(PR_OffsetX + ColumnWidth * 2, Row, 'Vr');
      TextOut(PR_OffsetX + ColumnWidth * 3, Row, 'Ab');
      TextOut(PR_OffsetX + ColumnWidth * 4, Row, 'Ov');
      TextOut(PR_OffsetX + ColumnWidth * 6, Row, SName);
      MoveTo(PR_OffSetX, Row + FontHeight + 1);
      LineTo(PR_OffsetX + Printer.PageWidth - (2 * PR_OffSetX), Row + FontHeight + 1);
      Row := Row + FontHeight + 4;
      Font.Style := [];
    end;
  end;

procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: Pointer;
  HeaderSize: DWord;
  ImageSize: DWord;
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage, ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0, 0,                            // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY);
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;

resourcestring
  SClass = 'Class: ';
  SAncestor = 'Ancestor: ';
  SUnit = 'Unit: ';
begin
  Row := PR_OffSetY;
  with ACanvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 12;
    Font.Style := [fsBold];
    TextOut(PR_OffsetX, Row, SClass + OInfo.Name);
    Row := Row + ACanvas.TextHeight('W') + 5;
    Font.Size := 10;
    FontHeight := ACanvas.TextHeight('WXYZ');
    ColumnWidth:= ACanvas.TextWidth('WW');
    BitmapSize := FontHeight - 1;
    TextOut(PR_OffSetX, Row, SAncestor + OInfo.DerivedFrom);
    Row := Row + FontHeight + 5;
    TextOut(PR_OffSetX, Row, SUnit + OInfo.UnitName);
    Row := Row + FontHeight + 10;
    PrintHeader;
    Font.Style := [];
    List := TStringList.Create;
    Bitmap := TBitmap.Create;

    try
      for i := 0 to OInfo.Count - 1 do
        List.AddObject(GetMethodString(OInfo.Items[i]), OInfo.Items[i]);
      for i := 0 to List.Count - 1 do
      begin
        MInfo := TBrowseMethodInfoItem(List.Objects[i]);
        ClearBitmap(Bitmap);
        ilClasses.GetBitmap(Ord(MInfo.MethodDeclare) + 2, Bitmap);
        PrintBitmap(ACanvas, Rect(PR_OffsetX, Row, PR_OffsetX + BitmapSize, Row + BitmapSize), Bitmap);
        ClearBitmap(Bitmap);
        ilClasses.GetBitmap(Ord(MInfo.MethodType) + 6, Bitmap);
        PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth, Row, PR_OffsetX + ColumnWidth + BitmapSize, Row + BitmapSize), Bitmap);

        if MInfo.cVirtual then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*2, Row, PR_OffsetX + ColumnWidth*2 + BitmapSize, Row + BitmapSize), Bitmap);
        end;
        if MInfo.cAbstract then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*3, Row, PR_OffsetX + ColumnWidth*3 + BitmapSize, row + BitmapSize), Bitmap);
        end;
        if MInfo.cOverride then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*4, Row, PR_OffsetX + ColumnWidth*4 + BitmapSize, row + BitmapSize), Bitmap);
        end;

        TextOut(PR_OffSetX + ColumnWidth*6, Row, FilterTab(MInfo.DName));
        Row := Row + FontHeight + 1;
        if Row + ((FontHeight + 1) * 3) > Printer.PageHeight then
        begin
          Printer.NewPage;
          Row := PR_OffsetY;
          PrintHeader;
        end;
      end;
    finally
      Bitmap.Free;
      List.Free;
    end;
  end;
end;

{$ELSE ACEREPORTER}

procedure TfmClassBrowser.ClassReport1Click(Sender: TObject);
resourcestring
  SClassReport = 'Class Report';
var
  AOut: TAceOutPut;
  OInfo: TBrowseClassInfoCollection;
begin
  try
    if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
      raise Exception.Create(SSelectClassFirst);
    OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
    Screen.Cursor := crHourglass;
    AOut := TAceOutPut.Create;
    try
      AOut.Destination := adAceFile;
      AOut.Description := SClassReport;
      AOut.BeginDoc;
      PrintClass(OInfo, AOut, AOut.AceCanvas);
      AOut.EndDoc;
      Screen.Cursor := crDefault;
      PreviewReport(AOut.AceFile);
      // Set output.AceFile to nil so AcePreview file is not freed
      // with output.AceFile
      AOut.AceFile := nil;
    finally
      AOut.Free;
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.PrintClass(OInfo: TBrowseClassInfoCollection; AOut: TAceOutput; AceCanvas: TAceCanvas);
resourcestring
  SName = 'Name';
var
  Row: Integer;
  i: Integer;
  List: TStringList;
  MInfo: TBrowseMethodInfoItem;
  FontHeight: Integer;
  Bitmap: TBitmap;

  procedure PrintHeader;
  begin
    with AceCanvas do
    begin
      Font.Style := [fsBold];
      TextOut(PR_OffsetX, Row, 'Vi');
      TextOut(PR_OffsetX + 20, Row, 'Ty');
      TextOut(PR_OffsetX + 40, Row, 'Vr');
      TextOut(PR_OffsetX + 60, Row, 'Ab');
      TextOut(PR_OffsetX + 80, Row, 'Ov');
      TextOut(PR_OffsetX + 120, Row, SName);
      MoveTo(PR_OffSetX, Row + FontHeight + 1);
      LineTo(PR_OffsetX + AOut.ACEFile.PageWidth - (2 * PR_OffSetX), Row + FontHeight + 1);
      Row := Row + FontHeight + 4;
      Font.Style := [];
    end;
  end;

resourcestring
  SClass = 'Class: ';
  SAncestor = 'Ancestor: ';
  SUnit = 'Unit: ';
begin
  Row := PR_OffSetY;
  with AceCanvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 12;
    Font.Style := [fsBold];
    TextOut(PR_OffsetX, Row, SClass + OInfo.Name);
    Row := Row + GetTextHeight(AceCanvas, 'W') + 5;
    Font.Size := 10;
    FontHeight := GetTextHeight(AceCanvas, 'WXYZ');
    TextOut(PR_OffSetX, Row, SAncestor + OInfo.DerivedFrom);
    Row := Row + FontHeight + 5;
    TextOut(PR_OffSetX, Row, SUnit + OInfo.UnitName);
    Row := Row + FontHeight + 10;
    PrintHeader;
    Font.Style := [];
    List := TStringList.Create;
    Bitmap := TBitmap.Create;

    try
      for i := 0 to OInfo.Count - 1 do
        List.AddObject(GetMethodString(OInfo.Items[i]), OInfo.Items[i]);
      for i := 0 to List.Count - 1 do
      begin
        MInfo := TBrowseMethodInfoItem(List.Objects[i]);
        ClearBitmap(Bitmap);
        ilClasses.GetBitmap(Ord(MInfo.MethodDeclare) + 2, Bitmap);
        Draw(PR_OffsetX, Row, Bitmap);
        ClearBitmap(Bitmap);
        ilClasses.GetBitmap(Ord(MInfo.MethodType) + 6, Bitmap);
        Draw(PR_OffsetX + 20, Row, Bitmap);

        if MInfo.cVirtual then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          Draw(PR_OffsetX + 40, Row, Bitmap);
        end;
        if MInfo.cAbstract then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          Draw(PR_OffsetX + 60, Row, Bitmap);
        end;
        if MInfo.cOverride then
        begin
          ClearBitmap(Bitmap);
          ilClasses.GetBitmap(12, Bitmap);
          Draw(PR_OffsetX + 80, Row, Bitmap);
        end;
        //if MInfo.cInherited then
        //  TextOut(PR_OffsetX+100, Row, Classes.ObjectName[MInfo.DB,MInfo.ONum]);

        TextOut(PR_OffSetX + 120, Row, FilterTab(MInfo.DName));
        Row := Row + FontHeight + 1;
        if Row + ((FontHeight + 1) * 3) > AOut.AceFile.PageHeight then
        begin
          AOut.NewPage;
          Row := PR_OffsetY;
          PrintHeader;
        end;
      end;
    finally
      Bitmap.Free;
      List.Free;
    end;
  end;
end;

procedure TfmClassBrowser.PreviewReport(AceFile: TAceFile);
begin
  with TfmPreview.Create(nil) do
  try
    AcePreview.LoadFromAceFile(AceFile);
    ShowModal;
  finally
    Free;
  end;
end;
{$ENDIF ACEREPORTER}

procedure TfmClassBrowser.PrinterSetup1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfmClassBrowser.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfmClassBrowser.sbRemoveClick(Sender: TObject);
var
  Item: TClassItem;
begin
  if tvBrowse.Selected = nil then
  begin
    sbRemove.Enabled := False;
    Exit;
  end;
  Item := TClassItem(tvBrowse.Selected.Data);
  tvBrowse.Selected.Free;
  Item.Free;
  if tvBrowse.Selected = nil then
    sbRemove.Enabled := False;
end;

procedure TfmClassBrowser.File1Click(Sender: TObject);
begin
  Add1.Enabled := (ClassList.StoragePath <> '');
  if tvBrowse.Selected = nil then
    Remove1.Enabled := False
  else
    Remove1.Enabled := (tvBrowse.Selected.Level = 0);
end;

procedure TfmClassBrowser.FormActivate(Sender: TObject);
resourcestring
  SLoadingClasses = 'Loading stored classes...';
  SLoadingProject = 'Loading current project...';
begin
  if IsFirst then
  begin
    IsFirst := False;
    Application.ProcessMessages;
    Screen.Cursor := crHourglass;
    try
      StatusBar.SimpleText := SLoadingClasses;
      StatusBar.Repaint;
      ClassList.LoadFromFile;
      LoadAllObjects;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  {$IFNDEF STANDALONE}
  if LoadProject then
  begin
    StatusBar.SimpleText := SLoadingProject;
    StatusBar.Repaint;
    LoadProject := False;
    RemoveProject;
    Application.ProcessMessages;
    AddProject;
  end;
  {$ENDIF STANDALONE}
end;

procedure TfmClassBrowser.sbFindClick(Sender: TObject);
resourcestring
  SFindClass = 'Find Class';
  SEnterClassName = 'Enter the name of the class you wish to find';
var
  Find: string;
  Node: TTreeNode;
begin
  try
    if tvBrowse.Items.Count = 0 then Exit;
    Find := InputBox(SFindClass, SEnterClassName, '');
    if Find = '' then Exit;
    Node := tvBrowse.Items[0];
    while Node <> nil do
    begin
      if (Node.Level > 0) and (CompareText(Find, TBrowseClassInfoCollection(Node.Data).Name) = 0) then
        Break;
      Node := Node.GetNext;
    end;
    if Node <> nil then
      tvBrowse.Selected := Node;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClassBrowser.Properties1Click(Sender: TObject);
begin
  if (ActiveControl = tvBrowse) or (lvInfo.Selected = nil) then
    Properties2Click(Sender)
  else
    Properties3Click(Sender)
end;

procedure TfmClassBrowser.Properties3Click(Sender: TObject);
var
  MInfo: TBrowseMethodInfoItem;
begin
  if lvInfo.Selected = nil then Exit;
  MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
  with TfmClassMethProp.Create(nil) do
  try
    lblMethod.Caption := StripWhiteSpace(FilterTab(MInfo.DName));
{  if MInfo.cInherited then
    laDerivedFrom.Caption:=Classes.ObjectName[MInfo.DB,MInfo.ONum]
  else
    laDerivedFrom.Caption:='';}
    chkAbstract.Checked := MInfo.cAbstract;
    chkOverride.Checked := MINfo.cOverride;
    chkVirtual.Checked := MInfo.cVirtual;
    chkMessageHandler.Checked := MInfo.cMessage;
    ShowModal;
  finally
    Free;
  end;
end;

{$IFNDEF ACEREPORTER}
procedure TfmClassBrowser.PrintClassDiagram(OInfo: TBrowseClassInfoCollection;
                                            ACanvas: TCanvas;
                                            BoxSize, VSpace: Integer);
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;

var
  x, y: Integer;
  BoxW, BoxH: Integer;
  IndentX, IndentY: Integer;
  PageNum: Integer;

  procedure PrintClassSquare(Info: TBrowseClassInfoCollection; Level: Integer; var Py: Integer);
  var
    Rect: TRect;
    Node: TTreeNode;
    np: Integer;
    op: Integer;
    OldBrushColor : TColor;
  begin
    if y + BoxH + 20 > Printer.PageHeight then
    begin
      Printer.NewPage;
      y := PR_OffsetY;
      Py := 0;
      Inc(PageNum);
    end;
    x := PR_OffSetX + IndentX * Level;
    Rect := Classes.Rect(x, y, x + BoxW, y + BoxH);
    OldBrushColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := clBlack;
    ACanvas.FrameRect(Rect);
    ACanvas.Brush.Color := OldBrushColor;
    np := Rect.Bottom;
    op := PageNum;
    if Level > 0 then
      with ACanvas do
      begin
        MoveTo(Rect.Left, Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Py);
      end;
    Inc(Rect.Left, 1);
    Inc(Rect.Top, 1);
    Dec(Rect.Bottom, 1);
    Dec(Rect.Right, 1);
    ACanvas.TextRect(Rect, x + 4, y + 4, Info.Name);
    y := y + BoxH + IndentY;
    Node := tvBrowse.Items[0];
    StatusBar.SimpleText := Info.Name;
    while Node <> nil do
    begin
      if op <> PageNum then
        np := 0;
      Application.ProcessMessages;
      if Node.Level > 0 then
        if CompareText(TBrowseClassInfoCollection(Node.Data).DerivedFrom, Info.Name) = 0 then
          PrintClassSquare(TBrowseClassInfoCollection(Node.Data), Level + 1, np);
      Node := Node.GetNext;
    end;
  end;

var
  p, i: Integer;
  st: string;
begin
  PageNum := 1;
  x := PR_OffsetX;
  y := PR_OffsetY;
  st := '';
  for i := 1 to BoxSize do
    st := st + 'W';
  BoxW := ACanvas.TextWidth(st) + 8;
  BoxH := ACanvas.TextHeight(st) + 8;
  IndentX := (BoxW div 2);
  IndentY := VSpace;
  p := 0;
  PrintClassSquare(OInfo, 0, p);
end;

procedure TfmClassBrowser.ClassHierarchy1Click(Sender: TObject);
var
  OInfo: TBrowseClassInfoCollection;
  Dlg: TfmClassReport;
resourcestring
  SClassReport = 'Class Hierarchy Report';
begin
  try
    if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
      Exit;
    Dlg := TfmClassReport.Create(Self);
    try
      Dlg.spnFontSize.Value := ClassHierarchyFontSize;
      Dlg.spnBoxSize.Value := ClassHierarchyBoxWidth;
      Dlg.spnBoxSpacing.Value := ClassHierarchyBoxSpace;
      Dlg.cbxFont.ItemIndex := Dlg.cbxFont.Items.IndexOf(ClassHierarchyFont);

      if Dlg.ShowModal = mrCancel then
        Exit;
      OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);

      Screen.Cursor := crHourglass;
      Printer.Title := SClassReport;
      Printer.BeginDoc;
      try
        with Printer.Canvas do
        begin
          Font.Name := Dlg.cbxFont.Text;
          Font.Size := Trunc(Dlg.spnFontSize.Value);
          Font.Style := [];
        end;
        PrintClassDiagram(OInfo, Printer.Canvas, Trunc(Dlg.spnBoxSize.Value),
          Trunc(Dlg.spnBoxSpacing.Value));
      finally
        Printer.EndDoc;
        Screen.Cursor := crDefault;
      end;

    ClassHierarchyFontSize := Dlg.spnFontSize.Value;
    ClassHierarchyBoxWidth := Dlg.spnBoxSize.Value;
    ClassHierarchyBoxSpace := Dlg.spnBoxSpacing.Value;
    ClassHierarchyFont     := Dlg.cbxFont.Text;

    finally
      Dlg.Free;
    end;

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

{$ELSE ACEREPORTER}
procedure TfmClassBrowser.ClassHierarchy1Click(Sender: TObject);
var
  AOut: TAceOutPut;
  OInfo: TBrowseClassInfoCollection;
  Dlg: TfmClassReport;
resourcestring
  SClassReport = 'Class Hierarchy Report';
begin
  try
    if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
      raise Exception.Create(SSelectClassFirst);
    Dlg := TfmClassReport.Create(Self);
    try
      Dlg.spnFontSize.Value := ClassHierarchyFontSize;
      Dlg.spnBoxSize.Value := ClassHierarchyBoxWidth;
      Dlg.spnBoxSpacing.Value := ClassHierarchyBoxSpace;
      Dlg.cbxFont.ItemIndex := Dlg.cbxFont.Items.IndexOf(ClassHierarchyFont);

      if Dlg.ShowModal = mrCancel then
        Exit;
      OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
      Screen.Cursor := crHourglass;
      AOut := TAceOutPut.Create;
      try
        AOut.Destination := adAceFile;
        AOut.Description := SClassReport;
        AOut.BeginDoc;
        with AOut.AceCanvas do
        begin
          Font.Name := Dlg.cbxFont.Text;
          Font.Size := Trunc(Dlg.spnFontSize.Value);
          Font.Style := [];
        end;
        PrintClassDiagram(OInfo, AOut, AOut.AceCanvas, Trunc(Dlg.spnBoxSize.Value),
          Trunc(Dlg.spnBoxSpacing.Value));
        AOut.EndDoc;
        Screen.Cursor := crDefault;
        PreviewReport(AOut.AceFile);
        // Set output.AceFile to nil so AcePreview file is not freed
        // with output.AceFile
        AOut.AceFile := nil;
      finally
        AOut.Free;
        Screen.Cursor := crDefault;
      end;

    ClassHierarchyFontSize := Dlg.spnFontSize.Value;
    ClassHierarchyBoxWidth := Dlg.spnBoxSize.Value;
    ClassHierarchyBoxSpace := Dlg.spnBoxSpacing.Value;
    ClassHierarchyFont     := Dlg.cbxFont.Text;

    finally
      Dlg.Free;
    end;

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;
{$ENDIF ACEREPORTER}

{$IFDEF ACEREPORTER}
procedure TfmClassBrowser.PrintClassDiagram(OInfo: TBrowseClassInfoCollection;
                                            AOut: TAceOutput; AceCanvas: TAceCanvas;
                                            BoxSize, VSpace: Integer);
var
  x, y: Integer;
  BoxW, BoxH: Integer;
  IndentX, IndentY: Integer;
  PageNum: Integer;

  procedure PrintClassSquare(Info: TBrowseClassInfoCollection; Level: Integer; var Py: Integer);
  var
    Rect: TRect;
    Node: TTreeNode;
    np: Integer;
    op: Integer;
  begin
    if y + BoxH + 20 > AOut.AceFile.PageHeight then
    begin
      AOut.NewPage;
      y := PR_OffsetY;
      Py := 0;
      Inc(PageNum);
    end;
    x := PR_OffSetX + IndentX * Level;
    Rect := Classes.Rect(x, y, x + BoxW, y + BoxH);
    FrameRect(AceCanvas, Rect);
    np := Rect.Bottom;
    op := PageNum;
    if Level > 0 then
      with AceCanvas do
      begin
        MoveTo(Rect.Left, Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Py);
      end;
    AceCanvas.TextRect(Rect, x + 2, y + 2, Info.Name);
    y := y + BoxH + IndentY;
    Node := tvBrowse.Items[0];
    StatusBar.SimpleText := Info.Name;
    while Node <> nil do
    begin
      if op <> PageNum then
        np := 0;
      Application.ProcessMessages;
      if Node.Level > 0 then
        if CompareText(TBrowseClassInfoCollection(Node.Data).DerivedFrom, Info.Name) = 0 then
          PrintClassSquare(TBrowseClassInfoCollection(Node.Data), Level + 1, np);
      Node := Node.GetNext;
    end;
  end;

var
  p, i: Integer;
  st: string;
begin
  PageNum := 1;
  x := PR_OffsetX;
  y := PR_OffsetY;
  st := '';
  for i := 1 to BoxSize do
    st := st + 'W';
  BoxW := GetTextWidth(AceCanvas, st) + 4;
  BoxH := GetTextHeight(AceCanvas, st) + 4;
  IndentX := (BoxW div 2);
  IndentY := VSpace;
  p := 0;
  PrintClassSquare(OInfo, 0, p);
end;
{$ENDIF ACERPORTER}

procedure GotoLineInFile(Line: Integer; const FileName: string);
var
  EditRead: TEditReader;
begin
  if not ToolServices.IsFileOpen(FileName) then
    if not ToolServices.OpenFile(FileName) then
      if not ToolServices.OpenFile(FileName) then
      begin
        raise Exception.Create('Error opening ' + FileName);
      end;

  if Line < 0 then
    Exit;

  EditRead := TEditReader.Create(FileName);
  try
    EditRead.GotoLine(Line);
  finally
    EditRead.Free;
  end;
end;

procedure TfmClassBrowser.GotoClass1Click(Sender: TObject);
var
  ClassInfos: TBrowseClassInfoCollection;
begin
  if tvBrowse.Selected = nil then
    Exit;
  ClassInfos := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
  GotoLineInFile(ClassInfos.RefreshLineNo + 1, ClassInfos.FileName);
  Self.Hide;
end;

procedure TfmClassBrowser.Goto1Click(Sender: TObject);
var
  Line: Integer;
  SourceFile: string;
  MInfo: TBrowseMethodInfoItem;
begin
  if (tvBrowse.Selected = nil) or (lvInfo.Selected = nil) then
    Exit;
  Line := 0;

  MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
  case MInfo.MethodType of
    ctConstant,
    ctType,
    ctVariable,
    ctProperty:    Line := MInfo.GetInterfaceLine;
    ctMethod:
      begin
        if MInfo.cAbstract then
          Line := MInfo.GetInterfaceLine
        else
          Line := MInfo.GetImplementationLine;
      end;
  end; // case
  Inc(Line);
  SourceFile := TBrowseClassInfoCollection(tvBrowse.Selected.Data).FileName;

  GotoLineInFile(Line, SourceFile);

  if AutomaticallyHideBrowser then
    Self.Hide;
end;

procedure TfmClassBrowser.UnitNames1Click(Sender: TObject);
begin
  UnitNames1.Checked := not UnitNames1.Checked;
  LoadAllObjects;
end;

procedure TfmClassBrowser.pmInfoPopup(Sender: TObject);
var
  IsEnabled: Boolean;
begin
  IsEnabled := (lvInfo.Selected <> nil);
  Goto1.Enabled := IsEnabled;
  Properties3.Enabled := IsEnabled;
end;

procedure TfmClassBrowser.Help2Click(Sender: TObject);
begin
  {$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 5);
  {$ENDIF STANDALONE}
end;

procedure TfmClassBrowser.Contents1Click(Sender: TObject);
begin
  {$IFNDEF STANDALONE}
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTENTS, 0);
  {$ENDIF STANDALONE}
end;

procedure TfmClassBrowser.Edit1Click(Sender: TObject);
begin
  {$IFDEF SYNTAXMEMO}
  Copy1.Enabled := (ActiveControl is TSyntaxMemo)
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  Copy1.Enabled := (ActiveControl is TmwCustomEdit)
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  Copy1.Enabled := (ActiveControl is TCustomMemo)
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TfmClassBrowser.Copy1Click(Sender: TObject);
begin
  {$IFDEF SYNTAXMEMO}
  if (ActiveControl is TSyntaxMemo) then
    TSyntaxMemo(ActiveControl).CopyToClipboard;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  if (ActiveControl is TmwCustomEdit) then
    TmwCustomEdit(ActiveControl).CopyToClipboard;
  {$ENDIF MWEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  if (ActiveControl is TCustomMemo) then
    TCustomMemo(ActiveControl).CopyToClipboard;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TfmClassBrowser.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

function TfmClassBrowser.FilterTab(const Source: string): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Source) do
    if Result[i] = #9 then
      Result[i] := #32;
end;

{ TClassExpert }

constructor TClassExpert.Create;
begin
  inherited Create;
  HasConfigOptions := False;
  HasMenuItem := True;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

procedure TClassExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    else
    begin
      if Assigned(fmClassBrowser) then
      begin
        if fmClassBrowser.Visible then
          fmClassBrowser.Close;
      end;
      fmClassBrowser.Free;
      fmClassBrowser := nil;
    end;
  end;
end;

destructor TClassExpert.Destroy;
begin
  fmClassBrowser.Free;
  fmClassBrowser := nil;
  inherited Destroy;
end;

function TClassExpert.IconFileName: string;
begin
  Result := 'Class';
end;

function TClassExpert.GetMenuCaption: string;
resourcestring
  SClassBrowserMenuCaption = 'Class Bro&wser';
begin
  Result := SClassBrowserMenuCaption;
end;

function TClassExpert.GetMenuName: string;
begin
  Result := 'GX_Class'; // do not localize
end;

function TClassExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TClassExpert.GetName: string;
begin
  Result := 'Class_Browser'; // do not localize
end;

function TClassExpert.GetDisplayName: string;
begin
  Result := 'Class Browser'; // do not localize
end;

procedure TClassExpert.Click(Sender: TObject);
begin
  if fmClassBrowser = nil then
    fmClassBrowser := TfmClassBrowser.Create(nil);
  if fmClassBrowser.WindowState = wsMinimized then
    fmClassBrowser.WindowState := wsNormal;
  fmClassBrowser.Show;
end;

initialization
  RegisterGX_Expert(TClassExpert);
end.


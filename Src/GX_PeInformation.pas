unit GX_PeInformation;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, GX_PeInfo, ComCtrls, Menus, Registry,
  {$IFDEF ACEREPORTER}
  ACEOut, ACEFile, GX_PrinterFunc,
  {$ENDIF ACEREPORTER}
  GX_Experts, ExptIntf, ToolIntf, GX_IdeDock, DropTarget, DropSource;

type
  TfmPeInformation = class(TfmIdeDockForm)
    pnToolBar: TPanel;
    sbOpen: TSpeedButton;
    dlgOpen: TOpenDialog;
    pcMain: TPageControl;
    tshMSDOS: TTabSheet;
    tshImport: TTabSheet;
    tshExports: TTabSheet;
    tshPEHEader: TTabSheet;
    lvMSDOS: TListView;
    lvPEHeader: TListView;
    lvImports: TListView;
    Splitter1: TSplitter;
    lvImportFunctions: TListView;
    lvExportFunctions: TListView;
    tshPEOptional: TTabSheet;
    lvPEOptionalHeader: TListView;
    sbPrint: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Options1: TMenuItem;
    Decimal1: TMenuItem;
    Hex1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    PrinterSetup1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Help2: TMenuItem;
    N3: TMenuItem;
    sbHelp: TSpeedButton;
    sbCopy: TSpeedButton;
    procedure sbOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvImportsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure SetNumberType(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure PrinterSetup1Click(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    PEInfo: TPEFileInfo;
    FNumberType: TNumberType;
    FFileName: string;
    FileDrop: TDropFileTarget;
    procedure LoadPEInfo(const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    {$IFDEF ACEREPORTER}
    procedure PreviewReport(AceFile: TAceFile);
    {$ENDIF ACEREPORTER}
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
  public
    property FileName: string read FFileName;
    property NumberType: TNumberType read FNumberType write FNumberType;
  end;

  TPEExpert = class(TGX_EnhExpert)
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

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_GenFunc,
  {$IFDEF ACEREPORTER}
  GX_PrintPreview,
  {$ENDIF ACEREPORTER}
  GX_ConfigurationInfo, GX_GExperts, ClipBrd;

var
  fmPeInformation: TfmPeInformation = nil;
  PEExpert: TPEExpert = nil;

procedure SetListViewItem(aItem: TListItem; aValues: string);
var
  ItemList: TStringList;
  i: Integer;
begin
  aValues := '"' + aValues + '"';

  while Pos(#9, aValues) > 0 do
  begin
    Insert('","', aValues, Pos (#9, aValues) + 1);
    Delete(aValues, Pos (#9, aValues), 1);
  end;
  ItemList := TStringList.Create;
  try
    ItemList.CommaText := aValues;
    if ItemList.Count > 0 then
      aItem.Caption := ItemList[0];
    for i := 1 to ItemList.Count - 1 do
      aItem.SubItems.Add(ItemList[i]);
  finally
    ItemList.Free;
  end;
end;

procedure SetListViewItems(ListView: TListView; Items: TStrings);
var
 i: Integer;
 NewItem: TListItem;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    for i := 0 to Items.Count - 1 do
    begin
      NewItem := ListView.Items.Add;
      SetListViewItem(NewItem, Items[i]);
    end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TfmPeInformation.sbOpenClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if dlgOpen.Execute then
      LoadPEInfo(dlgOpen.FileName);
    FormResize(Self);
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmPeInformation.LoadPEInfo(const FileName: string);
resourcestring
  SFormCaption = 'PE Information - ';
begin
  Screen.Cursor := crHourglass;
  try
    Self.FFileName := FileName;
    Caption := SFormCaption + ExtractFileName(FileName);
    pcMain.ActivePage := tshMSDOS;
    PEInfo.Free;
    PEInfo := nil;
    try
      PEInfo := TPEFileInfo.Create(FileName, NumberType);
      lvImportFunctions.Items.Clear;
      SetListViewItems(lvMSDOS, PEInfo.MSDOSHeader);
      SetListViewItems(lvPEHeader, PEInfo.PEHEaderList);
      SetListViewItems(lvPEOptionalHeader, PEInfo.PEOptionalHEaderList);
      SetListViewItems(lvImports, PEInfo.ImportList);
      SetListViewItems(lvExportFunctions, PEInfo.ExportList);
    except
      on E: Exception do
      begin
        ShowExceptionErrorMessage(E);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmPeInformation.FormCreate(Sender: TObject);
begin
  inherited;
  try
    PEInfo := nil;
    pcMain.ActivePage := tshMSDOS;
    CenterForm(Self);
    LoadSettings;
    Decimal1.Checked := (NumberType = ntDecimal);
    Hex1.Checked := (NumberType = ntHex);
    FileDrop := TDropFileTarget.Create(nil);
    FileDrop.OnDrop := DropFiles;
    FileDrop.DragTypes := [dtCopy, dtMove, dtLink];
    FileDrop.ShowImage := True;
    FileDrop.Register(pcMain);
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmPeInformation.FormDestroy(Sender: TObject);
begin
  fmPeInformation := nil;
  try
    SaveSettings;
    PEInfo.Free;
    PEInfo := nil;
    FileDrop.Unregister;
    FileDrop.Free;
    FileDrop := nil;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
  inherited;
end;

procedure TfmPeInformation.lvImportsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  ImpExp: TImportExport;
  i: Integer;
  NewItem: TListItem;
begin
  if Change = ctState then
  begin
    lvImportFunctions.Items.BeginUpdate;
    try
      try
        lvImportFunctions.Items.Clear;
        if lvImports.Selected = nil then
          Exit;
        ImpExp := TImportExport(PEInfo.ImportList.Objects[lvImports.Selected.Index]);
        for i := 0 to ImpExp.Count - 1 do
        begin
          NewItem := lvImportFunctions.Items.Add;
          NewItem.Caption := ImpExp.Items[i].FunctionName;
          NewItem.SubItems.Add(PEInfo.IntToNum(ImpExp.Items[i].Ordinal));
        end;
      except
        on E: Exception do
          ShowExceptionErrorMessage(E);
      end;
    finally
      lvImportFunctions.Items.EndUpdate;
    end;
    FormResize(Self);
  end;
end;

procedure TfmPeInformation.FormResize(Sender: TObject);
begin
  try
    with lvMSDOS do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEOptionalHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvImportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - 1, 0);
    with lvExportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - Columns.Items[2].Width - 1, 0);
  except
    on E: Exception do
    begin
      // ignore
    end;
  end;
end;

procedure TfmPeInformation.SetNumberType(Sender: TObject);
begin
  FNumberType := TNumberType(TMenuItem(Sender).Tag);
  if PEInfo <> nil then
    LoadPEInfo(FFilename);
  Decimal1.Checked := (NumberType = ntDecimal);
  Hex1.Checked := (NumberType = ntHex);
end;

procedure TfmPeInformation.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfmPeInformation.PrinterSetup1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TfmPeInformation.SaveSettings;
begin
  // do not localize any of the below lines
  //! StH: Add StandAlone conditional?
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteInteger('PEDump', 'Left', Left);
    WriteInteger('PEDump', 'Top', Top);
    WriteInteger('PEDump', 'Width', Width);
    WriteInteger('PEDump', 'Height', Height);
    WriteInteger('PEDump', 'Numbers', Integer(NumberType));
    WriteString('PEDump', 'BinPath', ExtractFilePath(dlgOpen.FileName));
  finally
    Free;
  end;
end;

procedure TfmPeInformation.LoadSettings;
begin
  // do not localize any of the below lines
  //! StH: Add StandAlone conditional?
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    Left := ReadInteger('PEDump', 'Left', Left);
    Top := ReadInteger('PEDump', 'Top', Top);
    Width := ReadInteger('PEDump', 'Width', Width);
    Height := ReadInteger('PEDump', 'Height', Height);
    NumberType := TNumberType(ReadInteger('PEDump', 'Numbers', Ord(ntHex)));
    dlgOpen.InitialDir := ReadString('PEDump', 'BinPath', '');
  finally
    Free;
  end;
end;

procedure TfmPeInformation.sbPrintClick(Sender: TObject);
resourcestring
  SPeInfoFor = 'PE Information for ';
  SFile = 'File: ';
  SMsDosHeader = 'MS-DOS Header';
  SPeHeader = 'PE Header';
  SPeOptionalHeader = 'PE Optional Header';
  SImports = 'Imports';
  SFunction = 'Function';
  SOrdinal = 'Ordinal';
  SExports = 'Exports';

{$IFNDEF ACEREPORTER}
var
  RichEdit: TRichEdit;
  Line: string;
  i, j: Integer;
  ImpExp: TImportExport;

  procedure PrintHeader(LV: TListView; Header: string);
  var
    t: Integer;
  begin
    with RichEdit do
    begin
      RichEdit.SelAttributes.Style := [fsBold];
      Lines.Add(Header);

      RichEdit.SelAttributes.Style := [];
      for t := 0 to LV.Items.Count - 1 do
      begin
        Lines.Add('   ' + LV.Items[t].Caption + #9 + ':   ' + LV.Items[t].SubItems[0]);
      end;
      Lines.Add('');
    end;
  end;

begin
  if PEInfo = nil then
    Exit;
  try
    RichEdit := TRichEdit.Create(Self);
    Screen.Cursor := crHourglass;
    try
      with RichEdit do
      begin
        Visible := False;
        Parent := Self;
        Clear;
        DefAttributes.Name := 'Arial';
        DefAttributes.Size := 10;

        Paragraph.TabCount := 1;
        Paragraph.Tab[0]   := 200;
      end;

      // Document header
      RichEdit.Lines.Add('PE Header information for ' + FFileName);
      // AJB: I would like some file info here, date/time, version...
      RichEdit.Lines.Add('');
      { MS-DOS Header }
      PrintHeader(lvMSDos, SMsDosHeader);
      { PE Header }
      PrintHeader(lvPEHeader, SPeHeader);
      { PE Optional Header }
      PrintHeader(lvPEOptionalHeader, SPeOptionalHeader);

      { Imports }
      with RichEdit do
      begin
        RichEdit.Paragraph.TabCount := 2;
        RichEdit.Paragraph.Tab[0]   := 80;
        RichEdit.Paragraph.Tab[1]   := 300;
        RichEdit.SelAttributes.Style := [fsBold];
        SelText := SImports;

        for j := 0 to lvImports.Items.Count - 1 do
        begin
          SelAttributes.Style := [fsUnderLine];
          Lines.Add('   ' + lvImports.Items[j].Caption + #09 + SFunction + #09 + SOrdinal);
          SelAttributes.Style := [];

          ImpExp := TImportExport(PEInfo.ImportList.Objects[j]);

          for i := 0 to ImpExp.count - 1 do
          begin
            Line := ImpExp.Items[i].FunctionName;
            //TODO -cImplement -oArentJan: Use Canvas to measure or wordwrap?
            if Length(Line) > 32 then
              Line := Copy(ImpExp.Items[i].FunctionName, 1, 32) + '...';
            Lines.Add(#09 + Line + #09 + IntToStr(ImpExp.Items[i].Ordinal));
          end;
          Lines.Add('');
        end;
      end;

      { Exports }
      with RichEdit do
      begin
        RichEdit.Paragraph.TabCount := 3;
        RichEdit.Paragraph.Tab[0]   := 20;
        RichEdit.Paragraph.Tab[1]   := 280;
        RichEdit.Paragraph.Tab[2]   := 380;

        RichEdit.SelAttributes.Style := [fsBold];
        SelText := SExports;
        RichEdit.SelAttributes.Style := [];
        for i := 0 to lvExportFunctions.Items.Count - 1 do
        begin
          Lines.Add(#09 + lvExportFunctions.Items[i].Caption + #09 +
            lvExportFunctions.Items[i].SubItems[0] + #09 + lvExportFunctions.Items[i].SubItems[1]);
        end;
      end;

      RichEdit.Print(SPeInfoFor + ExtractFileName(dlgOpen.FileName));
//      RichEdit.Lines.SaveToFile('c:\temp\ExeExplr.rtf');
    finally
      Screen.Cursor := crDefault;
      RichEdit.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;

end;
{$ELSE}
var
  AOut: TAceOutPut;
  FontHeight: Integer;

  procedure PrintReport;
  var
    Row: Integer;
    ImpExp: TImportExport;
    i, j: Integer;

    procedure CheckHeader;
    begin
      if Row + ((FontHeight + 1) * 3) > AOut.AceFile.PageHeight then
      begin
        AOut.NewPage;
        Row := PR_OffsetY;
      end;
    end;

    procedure PrintHeader(LV: TListView; Header: string);
    var
      t: Integer;
    begin
      with AOut.AceCanvas do
      begin
        Row := Row + 10;
        Font.Style := [fsBold];
        TextOut(PR_OffsetX, Row, Header);
        Row := Row + FontHeight + 4;
        Font.Style := [];
        for t := 0 to LV.Items.Count - 1 do
        begin
          TextOut(PR_OffSetX + 20, Row, LV.Items[t].Caption);
          TextOut(PR_OffSetX + 300, Row, ':');
          TextOut(PR_OffSetX + 320, Row, LV.Items[t].SubItems[0]);
          Row := Row + FontHeight + 4;
          CheckHeader;
        end;
      end;
    end;

  begin
    Row := PR_OffsetY;
    { File Header }
    with AOut.AceCanvas do
    begin
      Font.Name := 'Arial';  // do not localize
      Font.Size := 12;
      Font.Style := [fsBold];
      TextOut(PR_OffsetX, Row, SFile + Filename);
      FontHeight := GetTextHeight(AOut.ACECanvas, 'W');
      Row := Row + FontHeight + 4;
    end;
    { MS-DOS Header }
    PrintHeader(lvMSDos, SMsDosHeader);
    { PE Header }
    PrintHeader(lvPEHeader, SPeHeader);
    { PE Optional Header }
    PrintHeader(lvPEOptionalHeader, SPeOptionalHeader);
    { Imports }
    with AOut.AceCanvas do
    begin
      Font.Style := [fsBold];
      Row := Row + 10;
      TextOut(PR_OffsetX, Row, SImports);
      Row := Row + FontHeight + 4;
      Font.Style := [];
      for j := 0 to lvImports.Items.Count - 1 do
      begin
        Font.Style := [fsUnderLine];
        TextOut(PR_OffsetX + 20, Row, lvImports.Items[j].Caption);
        TextOut(PR_OffsetX + 120, Row, SFunction);
        TextOut(PR_OffsetX + 550, Row, SOrdinal);
        Row := Row + FontHeight + 4;
        Font.Style := [];
        ImpExp := TImportExport(PEInfo.ImportList.Objects[j]);
        for i := 0 to ImpExp.count - 1 do
        begin
          TextRect(Bounds(PR_OffSetX + 120, Row, PR_OffSetX + 385, FontHeight + 4),
                    PR_OffSetX + 120, Row, ImpExp.Items[i].FunctionName);
          TextOut(PR_OffSetX + 550, Row, IntToStr(ImpExp.Items[i].Ordinal));
          Row := Row + FontHeight + 4;
          CheckHeader;
        end;
      end;
    end;
    { Exports }
    with AOut.AceCanvas do
    begin
      Font.Style := [fsBold];
      Row := Row + 10;
      TextOut(PR_OffsetX, Row, SExports);
      Font.Style := [];
      Row := Row + FontHeight + 4;
      for i := 0 to lvExportFunctions.Items.Count - 1 do
      begin
        TextRect(Bounds(PR_OffSetX + 20, Row, PR_OffSetX + 335, FontHeight + 4),
                  PR_OffSetX + 20, Row, lvExportFunctions.Items[i].Caption);
        TextOut(PR_OffSetX + 480, Row, lvExportFunctions.Items[i].SubItems[0]);
        TextOut(PR_OffSetX + 580, Row, lvExportFunctions.Items[i].SubItems[1]);
        Row := Row + FontHeight + 4;
        CheckHeader;
      end;
    end;
  end;

begin
  if PEInfo = nil then
    Exit;
  try
    Screen.Cursor := crHourglass;
    AOut := TAceOutPut.Create;
    try
      AOut.Destination := adAceFile;
      AOut.Description := SPeInfoFor + ExtractFileName(dlgOpen.FileName);
      AOut.BeginDoc;
      PrintReport;
      AOut.EndDoc;
      Screen.Cursor := crDefault; // this is OK
      PreviewReport(AOut.AceFile);
      { Set output.AceFile to nil so AcePreview file is not freed
        with output.AceFile }
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

procedure TfmPeInformation.PreviewReport(AceFile: TAceFile);
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

procedure TfmPeInformation.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmPeInformation.Help2Click(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 16);
end;

procedure TfmPeInformation.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 16);
end;

procedure TfmPeInformation.pcMainChange(Sender: TObject);
begin
  // VCL 4.0 Bug workaround - columns in header list not visible (extent = 0)
  {$IFDEF GX_VER120_up}
  pcMain.ActivePage.Realign;
  {$ENDIF GX_VER120_up}
  // Let the listview update so the columns size right
  Application.ProcessMessages;
  FormResize(Self);
end;

procedure TfmPeInformation.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  if (FileDrop.Files = nil) or (FileDrop.Files.Count < 1) then
    Exit;
  LoadPEInfo(FileDrop.Files[0]);
end;

procedure TfmPeInformation.sbCopyClick(Sender: TObject);
var
  List: TListView;
  i, j: Integer;
  ItemString: string;
  SL: TStringList;
begin
  List := nil;
  if ActiveControl is TListView then
    List := ActiveControl as TListView
  else
    for i := pcMain.ActivePage.ControlCount - 1 downto 0 do
      if pcMain.ActivePage.Controls[i] is TListView then
      begin
        List := pcMain.ActivePage.Controls[i] as TListView;
        Break;
      end;
  if List = nil then
    Exit;
  SL := TStringList.Create;
  try
    for i := 0 to List.Items.Count - 1 do
    begin
      ItemString := List.Items.Item[i].Caption;
      for j := 0 to List.Items.Item[i].SubItems.Count - 1 do
        ItemString := ItemString + #09 + List.Items.Item[i].SubItems.Strings[j];
      SL.Add(ItemString);
    end;
    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TfmPeInformation.FormActivate(Sender: TObject);
begin
  // Needed later because docking cancels the registration??
  //if FileDrop <> nil then
  //  FileDrop.Register(pcMain);
end;

{ TPEExpert }

constructor TPEExpert.Create;
begin
  inherited Create;
  PEExpert := Self;
  ShortCut := 0;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TPEExpert.Destroy;
begin
  PEExpert := nil;
  inherited Destroy;
end;

procedure TPEExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      IdeDockManager.RegisterDockableForm(TfmPeInformation, fmPeInformation, 'fmPeInformation')
    else
    begin
      fmPeInformation.Free;
      IdeDockManager.UnRegisterDockableForm(fmPeInformation, 'fmPeInformation');
    end;
  end;
end;

function TPEExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'P&E Information';
begin
  Result := SMenuCaption;
end;

function TPEExpert.GetMenuName: string;
begin
  Result := 'GX_PE';
end;

function TPEExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TPEExpert.GetName: string;
begin
  Result := 'PE_DUMP';
end;

function TPEExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'PE Information';
begin
  Result := SDisplayName;
end;

procedure TPEExpert.Click(Sender: TObject);
begin
  if fmPeInformation = nil then
    fmPeInformation := TfmPeInformation.Create(nil);
  IdeDockManager.ShowForm(fmPeInformation);
end;

function TPEExpert.IconFileName: string;
begin
  Result := 'PE';
end;

initialization
  RegisterGX_Expert(TPEExpert);
end.


unit GX_Configure;

{$I GX_CondDefine.Inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus, Buttons, SpinIntEdit;

type
  TfmConfiguration = class(TForm)
    btnOK: TButton;
    btnHelp: TButton;
    pcConfig: TPageControl;
    tshExperts: TTabSheet;
    tshGeneral: TTabSheet;
    sbxExperts: TScrollBox;
    gbxLocations: TGroupBox;
    lblVCL: TLabel;
    sbVCLDir: TSpeedButton;
    lblConfig: TLabel;
    sbConfigDir: TSpeedButton;
    lblHelp: TLabel;
    sbHelpFile: TSpeedButton;
    edVCLPath: TEdit;
    edConfigPath: TEdit;
    edHelpFile: TEdit;
    dlgHelpFile: TOpenDialog;
    btnCancel: TButton;
    tshIDE: TTabSheet;
    tshEditorExperts: TTabSheet;
    gbxKeyboard: TGroupBox;
    lvEditorExperts: TListView;
    btnConfigure: TButton;
    btnShortcut: TButton;
    meHelp: TMemo;
    tshEditor: TTabSheet;
    gbxEditor: TGroupBox;
    chkMultiLine: TCheckBox;
    chkHotTrack: TCheckBox;
    chkButtons: TCheckBox;
    gbxEditMenu: TGroupBox;
    chkLocalEditMenus: TCheckBox;
    chkLocalEditMenusTop: TCheckBox;
    gbxToolbar: TGroupBox;
    chkEditorToolbar: TCheckBox;
    tshToolBar: TTabSheet;
    lblCategories: TLabel;
    lblAvailable: TLabel;
    sbAdd: TSpeedButton;
    sbRemove: TSpeedButton;
    lblSelected: TLabel;
    lbCategory: TListBox;
    lbAvailable: TListBox;
    lbSelected: TListBox;
    btnSeparator: TButton;
    gbxIDEMenu: TGroupBox;
    chkDisableEditorExperts: TCheckBox;
    gbxFonts: TGroupBox;
    dlgFont: TFontDialog;
    btnOIFont: TButton;
    btnCPFont: TButton;
    chkOIFontEnabled: TCheckBox;
    chkCPFontEnabled: TCheckBox;
    chkShowWindowsMenu: TCheckBox;
    chkDisableIdeEnhancements: TCheckBox;
    chkShowAPIMenu: TCheckBox;
    chkShowCPUMenu: TCheckBox;
    chkDisableEDTEnhancements: TCheckBox;
    gbxFileSaving: TGroupBox;
    spnMinutes: TSpinIntEdit;
    chkAutoSave: TCheckBox;
    lblEvery: TLabel;
    chkDfmAsText: TCheckBox;
    lblMinutes: TLabel;
    chkEditorFontControl: TCheckBox;
    chkShowAttachToMenu: TCheckBox;
    chkEditTabButtonsFlat: TCheckBox;
    rgAlign: TRadioGroup;
    gbxTabDockHost: TGroupBox;
    chkMultiLineTabDockHost: TCheckBox;
    chkDefaultMultiLineTabDockHost: TCheckBox;
    tshPalette: TTabSheet;
    gbxCompPalette: TGroupBox;
    chkCPMultiLine: TCheckBox;
    chkCPAsButtons: TCheckBox;
    chkCPTabsInPopup: TCheckBox;
    chkCPFlat: TCheckBox;
    chkCPTabsInPopupAlphaSort: TCheckBox;
    chkCPScrollOpposite: TCheckBox;
    chkCPRaggedRight: TCheckBox;
    chkAlphabetizeMenu: TCheckBox;
    procedure sbVCLDirClick(Sender: TObject);
    procedure sbConfigDirClick(Sender: TObject);
    procedure sbHelpFileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure lvEditorExpertsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnShortcutClick(Sender: TObject);
    procedure lbCategoryClick(Sender: TObject);
    procedure lbAvailableDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbAvailableDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbButtonGenericDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure sbAddClick(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure lbSelectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSelectedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btnSeparatorClick(Sender: TObject);
    procedure chkDisableEditorExpertsClick(Sender: TObject);
    procedure chkDisableIdeEnhancementsClick(Sender: TObject);
    procedure chkFontEnabledClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure chkDisableEDTEnhancementsClick(Sender: TObject);
    procedure chkAutoSaveClick(Sender: TObject);
    procedure chkCPAsButtonsClick(Sender: TObject);
    procedure chkLocalEditMenusClick(Sender: TObject);
    procedure chkCPTabsInPopupClick(Sender: TObject);
    procedure lbSelectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pcConfigChange(Sender: TObject);
    procedure chkCPMultiLineClick(Sender: TObject);

    procedure ConfigureEditorExpertClick(Sender: TObject);
    procedure chkButtonsClick(Sender: TObject);
    procedure chkEditorToolbarClick(Sender: TObject);
    procedure chkMultiLineTabDockHostClick(Sender: TObject);
    {$IFDEF GX_VER120_up}
    procedure sbxExpertsMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sbxExpertsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    {$ENDIF GX_VER120_up}
  private
    FOIFont: TFont;
    FCPFont: TFont;
    FThumbSize: Integer;
    procedure DisableUnsupportedIdeItems;
    procedure DisableUnsupportedEditorItems;
  private
    procedure LoadExperts;
    procedure SaveExperts;

    procedure LoadGeneral;
    procedure SaveGeneral;

    procedure LoadIdeEnhancements;
    procedure SaveIdeEnhancements;

    procedure LoadEditorEnhancements;
    procedure SaveEditorEnhancements;

    // Editor experts save themselves automatically
    // hence there is no SaveEditorExperts method
    procedure LoadEditorExperts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  {$IFDEF GX_EII} GX_EditorEnhancements, {$ENDIF GX_EII}
  GX_Experts, GX_EditorExpert, GX_IDEEnhance,
  GX_ToolbarButtons, GX_ConfigurationInfo,
  GX_GExperts, GX_GenFunc, GX_EditorShortcut, GX_Actions;

resourcestring
  SButtonCaptionSeparator = 'Separator';

const
  GlyphBackgroundSetting = clSilver;

// *************************************************************

procedure SetupGroupBox(Box: TGroupBox; Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to Box.ControlCount - 1 do
    Box.Controls[i].Enabled := Enable;
end;

// **************************************************************

constructor TfmConfiguration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOIFont := TFont.Create;
  FCPFont := TFont.Create;

  pcConfig.ActivePage := tshExperts;
  LoadExperts;
  LoadGeneral;

  LoadEditorExperts;
  chkDisableEditorExperts.Checked := not ConfigInfo.EditorExpertsEnabled;

  LoadIdeEnhancements;
  chkDisableIdeEnhancements.Checked := not IdeEnhancements.Enabled;
  DisableUnsupportedIdeItems;

  {$IFNDEF GX_EII}
  tshEditorExperts.TabVisible := False;
  meHelp.Color := clInfoBk;
  meHelp.Font.Color := clInfoText;
  {$ENDIF GX_EII}

  {$IFDEF GX_VER120_up}
  //chkAddMenuImages.Checked := ConfigInfo.AddMenuImages;
  OnMouseWheelDown := sbxExpertsMouseWheelDown;
  OnMouseWheelUp := sbxExpertsMouseWheelUp;
  {$ENDIF GX_VER120_up}
  chkAlphabetizeMenu.Checked := ConfigInfo.AlphabetizeMenu;

  LoadEditorEnhancements;
  {$IFDEF GX_EII}
  chkDisableEDTEnhancements.Checked := not EditorEnhancements.Enabled;
  {$ENDIF GX_EII}
  {$IFNDEF GX_EII}
  chkDisableEDTEnhancements.Checked := True;
  tshToolbar.TabVisible := False;
  tshEditor.TabVisible := False;
  {$ENDIF GX_EII}
  chkDisableIdeEnhancementsClick(chkDisableIdeEnhancements);
end;

destructor TfmConfiguration.Destroy;
begin
  FOIFont.Free;
  FOIFont := nil;

  FCPFont.Free;
  FCPFont := nil;

  inherited Destroy;
end;

procedure TfmConfiguration.LoadGeneral;
begin
  with ConfigInfo do
  begin
    edVCLpath.Text := VCLpath;
    edConfigPath.Text := ConfigPath;
    edHelpFile.Text := HelpFile;
  end;
end;

procedure TfmConfiguration.SaveGeneral;
begin
  with ConfigInfo do
  begin
    VCLpath := AddSlash(edVCLpath.Text);
    ConfigPath := AddSlash(edConfigPath.Text);
    HelpFile := edHelpFile.Text;
  end;
end;

procedure TfmConfiguration.LoadExperts;
resourcestring
  SConfigureButtonCaption = 'Configure...';
var
  Panel: TPanel;
  i: Integer;
  AnExpert: TGX_Expert;
const
  RowHeight = 40;
begin
  FThumbSize := RowHeight;
  for i := 0 to GExpertsInst.ExpertCount - 1 do
  begin
    Panel := TPanel.Create(Self);
    Panel.Parent := sbxExperts;
    {.$IFDEF GX_VER120_up}
    Panel.SetBounds(0, i * RowHeight, sbxExperts.Width, RowHeight);
    {.$ELSE}
    //Panel.SetBounds(0, (GExpertsInst.ExpertCount - i - 1) * RowHeight, sbxExperts.Width, RowHeight);
    //Panel.TabOrder := 0;
    {.$ENDIF GX_VER120_up}
    Panel.Tag := i;

    AnExpert := GExpertsInst.ExpertList[i];

    with TImage.Create(Self) do
    begin
      Parent := Panel;
      SetBounds(4, 4, 32, 32);
      Picture.Icon.Assign(AnExpert.Icon);
    end;

    with TCheckBox.Create(Self) do
    begin
      Parent := Panel;
      SetBounds(43, (RowHeight - Height) div 2, sbxExperts.Width - 140, Height);
      Caption := AnExpert.GetDisplayName;
      Checked := AnExpert.Active;
      Tag := i;
    end;

    with THotkey.Create(Self) do
    begin
      Parent := Panel;
      SetBounds(sbxExperts.Width - 245, (RowHeight - Height) div 2, 130, Height);
      Hotkey := AnExpert.Shortcut;
      Visible := AnExpert.HasMenuItem;
      Tag := i;
    end;

    if AnExpert.HasConfigOptions then
    begin
      with TButton.Create(Self) do
      begin
        Parent := Panel;
        Caption := SConfigureButtonCaption;
        SetBounds(sbxExperts.Width - Width - 30, (RowHeight - Height) div 2, Width, Height);
        OnClick := ConfigureEditorExpertClick;
        Tag := i;
      end;
    end;
  end;
  sbxExperts.VertScrollBar.Range := GExpertsInst.ExpertCount * RowHeight;
end;

procedure TfmConfiguration.SaveExperts;
var
  AnExpert: TGX_Expert;
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    AnExpert := GExpertsInst.ExpertList[Components[i].Tag];
    if Components[i] is TCheckBox then
      AnExpert.Active := TCheckBox(Components[i]).Checked
    else if Components[i] is THotKey then
      AnExpert.Shortcut := THotKey(Components[i]).Hotkey;
  end;
  for i := 0 to GExpertsInst.ExpertCount - 1 do
    GExpertsInst.ExpertList[i].SaveSettings;
end;

procedure TfmConfiguration.sbVCLDirClick(Sender: TObject);
var
  TempString: string;
begin
  TempString := edVCLPath.Text;
  if GetDir(Self, TempString) then
    edVCLPath.Text := TempString;
end;

procedure TfmConfiguration.sbConfigDirClick(Sender: TObject);
var
  TempString: string;
begin
  TempString := edConfigPath.Text;
  if GetDir(Self, TempString) then
    edConfigPath.Text := TempString;
end;

procedure TfmConfiguration.sbHelpFileClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  dlgHelpFile.InitialDir := ConfigInfo.ConfigPath;

  CurrentIdeFolder := GetCurrentDir;
  try
    if dlgHelpFile.Execute then
      edHelpFile.Text := dlgHelpFile.FileName;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmConfiguration.btnOKClick(Sender: TObject);
begin
  SaveGeneral;
  SaveExperts;
  SaveIdeEnhancements;
  SaveEditorEnhancements;
  ConfigInfo.SaveSettings;

  GXActionManager.UpdateShortCuts;

  ModalResult := mrOK;
end;

procedure TfmConfiguration.btnHelpClick(Sender: TObject);
var
  ActivePage: TTabSheet;
begin
  ActivePage := pcConfig.ActivePage;
  if ActivePage = tshEditorExperts then
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 29)
  else if ActivePage = tshIDE then
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 30)
  else if ActivePage = tshPalette then
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 34)
  else if ActivePage = tshEditor then
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 35)
  else if ActivePage = tshToolBar then
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 32)
  else
    WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 12);
end;

procedure TfmConfiguration.ConfigureEditorExpertClick(Sender: TObject);
begin
  GExpertsInst.ExpertList[(Sender as TButton).Tag].Configure;
end;

procedure TfmConfiguration.lvEditorExpertsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
{$IFDEF GX_EII}
  if lvEditorExperts.Selected = nil then
  begin
    btnConfigure.Enabled := False;
    btnShortcut.Enabled := False;
    meHelp.Clear;
    Exit;
  end;
  meHelp.Lines.BeginUpdate;
  try
    meHelp.Lines.Clear;
    GExpertsInst.EditorExpertList[lvEditorExperts.Selected.Index].GetHelpString(meHelp.Lines);
  finally
    meHelp.SelStart := 0;
    meHelp.Lines.EndUpdate;
    btnConfigure.Enabled := GExpertsInst.EditorExpertList[lvEditorExperts.Selected.Index].HasConfigOptions;
    btnShortcut.Enabled := True;
  end;
{$ENDIF GX_EII}
end;

{$IFNDEF GX_EII}
procedure TfmConfiguration.btnConfigureClick(Sender: TObject);
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.btnConfigureClick(Sender: TObject);
var
  EditorExpert: TEditorExpert;
begin
  if lvEditorExperts.Selected = nil then
    Exit;

  EditorExpert := GExpertsInst.EditorExpertList[lvEditorExperts.Selected.Index];
  EditorExpert.Configure;
  EditorExpert.SaveSettings;
end;
{$ENDIF GX_EII}

{$IFNDEF GX_EII}
procedure TfmConfiguration.btnShortcutClick(Sender: TObject);
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.btnShortcutClick(Sender: TObject);
var
  EditorExpert: TEditorExpert;
begin
  if lvEditorExperts.Selected = nil then
    Exit;

  EditorExpert := GExpertsInst.EditorExpertList[lvEditorExperts.Selected.Index];
  with TfmEditorShortcut.Create(nil) do
  begin
    try
      hkyShortcut.HotKey := EditorExpert.ShortCut;
      gbxShortcut.Caption := EditorExpert.Name;
      if ShowModal = mrOK then
      begin
        EditorExpert.ShortCut := hkyShortcut.HotKey;
        EditorExpert.SaveSettings;

        with lvEditorExperts do
        begin
          Items[Selected.Index].Caption     := EditorExpert.Name;
          Items[Selected.Index].SubItems[0] := ShortCutToText(EditorExpert.ShortCut);
        end;
      end;
    finally
      Free;
    end;
  end;
end;
{$ENDIF GX_EII}

procedure TfmConfiguration.lbCategoryClick(Sender: TObject);
var
  i: Integer;
  CurrentCategory: TGxButtonCategory;
begin
  lbAvailable.Items.BeginUpdate;
  try
    lbAvailable.Clear;

    if lbCategory.ItemIndex < 0 then
      Exit;

    CurrentCategory := TGxButtonCategory(lbCategory.ItemIndex);

    for i := Low(GxButtonInfo) to High(GxButtonInfo) do
    begin
      if GxButtonInfo[i].Category = CurrentCategory then
        lbAvailable.Items.AddObject(GxButtonInfo[i].ButtonName, Pointer(i));
    end;
  finally
    lbAvailable.Items.EndUpdate;
  end;
end;

procedure TfmConfiguration.lbAvailableDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  sbRemoveClick(sbRemove);
end;

procedure TfmConfiguration.lbAvailableDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbSelected);
end;

procedure TfmConfiguration.lbButtonGenericDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  Bitmap: TBitmap;
  Listbox: TListbox absolute Control;
  LbCanvas: TCanvas;

  procedure DrawToolbarButton(ToolbarButtonIndex: Integer);
  var
    BtnRect: TRect;
    OldColor: TColor;
  begin
    if ToolbarButtonIndex <> SeparatorMenuItemMarker then
      Bitmap.Handle := LoadBitmap(hInstance, PChar(GxButtonInfo[ToolbarButtonIndex].ResourceName))
    else
      Bitmap.Width := 42;

    if odSelected in State then
      LbCanvas.Brush.Color := clHighlight
    else
      LbCanvas.Brush.Color := clWindow;

    LbCanvas.FillRect(Rect);

    BtnRect := Classes.Rect(Rect.Left, Rect.Top + 1, Rect.Left + 21, Rect.Top + 21);

    // Paint fake button
    OldColor := LbCanvas.Brush.Color;
    LbCanvas.Brush.Color := clBtnface;
    try
      LbCanvas.FillRect(BtnRect);

      LbCanvas.BrushCopy(Classes.Rect(Rect.Left, Rect.Top + 1, Rect.Left + 21, Rect.Top + 21),
                         Bitmap,
                         Classes.Rect(0, 0, 20, 20),
                         GlyphBackgroundSetting);

      //Frame3D(LbCanvas, BtnRect, clBtnHighlight, clBtnShadow, 1);
    finally
      LbCanvas.Brush.Color := OldColor;
    end;
  end;

var
  Offset: Integer;
begin
  Assert(Control is TListBox);

  LbCanvas := Listbox.Canvas;

  Bitmap := TBitmap.Create;
  try
    DrawToolbarButton(Integer(TListBox(Control).Items.Objects[Index]));

    LbCanvas.Brush.Style := bsClear;
    Offset := (Listbox.ItemHeight - LbCanvas.TextHeight('W')) div 2;

    if not lbAvailable.Enabled then
      LbCanvas.Font.Color := clGrayText;
    LbCanvas.TextOut(Rect.Left + (Bitmap.Width div 2) + 2,
                     Rect.Top + OffSet,
                     Listbox.Items[Index]);
  finally
    Bitmap.Free;
  end;
end;

procedure TfmConfiguration.sbAddClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbAvailable.ItemIndex;
  if i < 0 then
    Exit;

  lbSelected.ItemIndex:= lbSelected.Items.AddObject(lbAvailable.Items[i], lbAvailable.Items.Objects[i]);
end;

procedure TfmConfiguration.sbRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbSelected.ItemIndex;
  if i < 0 then
    Exit;

  lbSelected.Items.Delete(lbSelected.ItemIndex);
  if i <= lbSelected.Items.Count - 1 then
    lbSelected.ItemIndex := i
  else
    if lbSelected.Items.Count > 0 then
      lbSelected.ItemIndex := lbSelected.Items.Count - 1;
end;

procedure TfmConfiguration.lbSelectedDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Idx: Integer;
begin
  Idx := lbSelected.ItemAtPos(Point(X, Y), False);
  if Idx = lbSelected.Items.Count then
    Dec(Idx);
  if Sender <> Source then
  begin
    if (Idx < 0) or (Idx = lbSelected.Items.Count - 1) then
      sbAddClick(sbAdd)
    else
    begin
      lbSelected.Items.InsertObject(Idx, lbAvailable.Items[lbAvailable.ItemIndex],
        lbAvailable.Items.Objects[lbAvailable.ItemIndex]);
      lbSelected.ItemIndex := Idx;
    end;
  end
  else
  begin
    if (lbSelected.ItemIndex < 0) or (Idx < 0) then
      Exit;

    lbSelected.Items.Move(lbSelected.ItemIndex, Idx);
  end;
  //lbSelected.ItemIndex := Idx;
  //lbSelected.Selected[Idx] := True;
end;

procedure TfmConfiguration.lbSelectedDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Idx: Integer;
begin
  Accept := ((Source = lbAvailable) or (Source = Sender));
  // Autoscroll the listbox to make dragging easier
  if Y < 15 then
    lbSelected.Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
    if Y > lbSelected.Height - 15 then
      lbSelected.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
  Idx := lbSelected.ItemAtPos(Point(X, Y), False);
  if (Source = lbAvailable) and (Idx > -1) and (Idx < lbSelected.Items.Count) then
    lbSelected.ItemIndex := Idx;
end;

procedure TfmConfiguration.btnSeparatorClick(Sender: TObject);
begin
  lbSelected.Items.AddObject(SButtonCaptionSeparator, Pointer(SeparatorMenuItemMarker));
end;

{$IFNDEF GX_EII}
procedure TfmConfiguration.LoadEditorExperts;
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.LoadEditorExperts;
var
  i: Integer;
  AnExpert: TEditorExpert;
  ListItem: TListItem;
begin
  lvEditorExperts.Items.BeginUpdate;
  try
    lvEditorExperts.Items.Clear;
    for i := 0 to GExpertsInst.EditorExpertCount - 1 do
    begin
      AnExpert := GExpertsInst.EditorExpertList[i];
      ListItem := lvEditorExperts.Items.Add;
      ListItem.Caption := AnExpert.Name;
      ListItem.SubItems.Add(ShortcutToText(AnExpert.ShortCut));
    end;
  finally
    lvEditorExperts.Items.EndUpdate;
  end;
end;
{$ENDIF GX_EII}

procedure TfmConfiguration.LoadIdeEnhancements;
begin
  Assert(IdeEnhancements <> nil);

  chkDisableIdeEnhancements.Checked := not IdeEnhancements.Enabled;

  // Multiline component palette
  chkCPMultiLine.Checked := IdeEnhancements.CPMultiLine;
  chkCPAsButtons.Checked := IdeEnhancements.CPAsButtons;
  chkCPScrollOpposite.Checked := IdeEnhancements.CPScrollOpposite;
  chkCPRaggedRight.Checked := IdeEnhancements.CPRaggedRight;
  {$IFDEF GX_VER120_up}
  chkCPFlat.Checked := IdeEnhancements.CPFlatButtons;
  {$ENDIF GX_VER120_up}
  chkCPTabsInPopup.Checked := IdeEnhancements.CPTabsInPopup;
  chkCPTabsInPopupAlphaSort.Checked := IdeEnhancements.CPTabsInPopupAlphaSort;
  {$IFDEF GX_VER120_up}
  // Tab-docked hosts
  chkMultiLineTabDockHost.Checked := IdeEnhancements.MultilineTabDockHost;
  chkDefaultMultiLineTabDockHost.Checked := IdeEnhancements.DefaultMultilineTabDockHost;
  {$ENDIF GX_VER120_up}
  // File saving
  chkAutoSave.Checked := IdeEnhancements.AutoSave;
  spnMinutes.Value := IdeEnhancements.AutoSaveInterval;
  chkDfmAsText.Checked := IdeEnhancements.SaveDfmAsTxt;
  // Menu
  chkShowWindowsMenu.Checked := IdeEnhancements.ShowWindowsMenu;
  chkCPFontEnabled.Checked := IdeEnhancements.CPFontEnabled;
  chkOIFontEnabled.Checked := IdeEnhancements.OIFontEnabled;
  {$IFDEF VER100}
  chkShowAPIMenu.Checked := IdeEnhancements.ShowApiHelpMenuItem;
  chkShowCPUMenu.Checked := IdeEnhancements.ShowCpuMenuItem;
  {$ENDIF VER100}
  {$IFDEF VER120}
  chkShowAttachToMenu.Checked := IdeEnhancements.ShowAttachToMenuItem;
  {$ENDIF VER120}
  // Fonts
  FOIFont.Assign(IdeEnhancements.OIFont);
  FCPFont.Assign(IdeEnhancements.CPFont);

  chkFontEnabledClick(Self);
  chkAutoSaveClick(chkAutoSave);
  chkCPAsButtonsClick(chkCPAsButtons);
  chkCPTabsInPopupClick(chkCPTabsInPopup);
  chkCPMultiLineClick(chkCPMultiLine);
  chkMultiLineTabDockHostClick(chkMultiLineTabDockHost);
  chkCPMultiLineClick(chkCPMultiLine);

  chkDisableEDTEnhancementsClick(chkDisableEDTEnhancements);
end;

{$IFNDEF GX_EII}
procedure TfmConfiguration.LoadEditorEnhancements;
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.LoadEditorEnhancements;
var
  i, n: Integer;
  j: TGxButtonCategory;
begin
  Assert(EditorEnhancements <> nil);

  chkDisableEDTEnhancements.Checked := not EditorEnhancements.Enabled;

  Assert(EditorEnhancements.ToolbarButtonsList <> nil);

  lbSelected.Items.BeginUpdate;
  try
    lbSelected.Clear;
    for i := 0 to EditorEnhancements.ToolbarButtonsList.Count - 1 do
    begin
      n := Integer(EditorEnhancements.ToolbarButtonsList.Items[i]);
      if n = SeparatorMenuItemMarker then
        lbSelected.Items.AddObject(SButtonCaptionSeparator, Pointer(SeparatorMenuItemMarker))
      else
        lbSelected.Items.AddObject(GxButtonInfo[n].ButtonName, Pointer(n));
    end;
  finally
    lbSelected.Items.EndUpdate;
  end;

  lbCategory.Items.BeginUpdate;
  try
    for j := Low(GxButtonCategoryText) to High(GxButtonCategoryText) do
      lbCategory.Items.Add(GxButtonCategoryText[j]);
  finally
    lbCategory.Items.EndUpdate;
  end;

  lbCategory.ItemIndex := 0;
  lbCategoryClick(lbCategory);

  chkEditorToolBar.Checked := EditorEnhancements.ToolBarVisible;
  chkMultiLine.Checked := EditorEnhancements.MultiLine;

  chkHotTrack.Checked := EditorEnhancements.HotTrack;
  chkButtons.Checked := EditorEnhancements.Buttons;
  chkEditTabButtonsFlat.Checked := EditorEnhancements.ButtonsFlat;
  chkEditorFontControl.Checked := EditorEnhancements.FontControl;

  chkLocalEditMenus.Checked := EditorEnhancements.EditorContextMenus;
  chkLocalEditMenusTop.Checked := not EditorEnhancements.EditorContextMenusAtBottom;

  Assert(EditorEnhancements.ToolbarAlign in [alTop..alRight]);
  rgAlign.ItemIndex := Ord(EditorEnhancements.ToolbarAlign) - 1;

  chkDisableEDTEnhancementsClick(chkDisableEDTEnhancements);
end;
{$ENDIF GX_EII}

procedure TfmConfiguration.SaveIdeEnhancements;
begin
  Assert(IdeEnhancements <> nil);

  IdeEnhancements.Enabled := not chkDisableIdeEnhancements.Checked;

  // Multiline tab dock host
  {$IFDEF GX_VER120_up}
  IdeEnhancements.MultilineTabDockHost := chkMultiLineTabDockHost.Checked;
  IdeEnhancements.DefaultMultilineTabDockHost := chkDefaultMultiLineTabDockHost.Checked;
  {$ENDIF GX_VER120_up}

  // Menus
  //ConfigInfo.AddMenuImages := chkAddMenuImages.Checked;
  GXActionManager.Alphabetical := chkAlphabetizeMenu.Checked;

  IdeEnhancements.ShowWindowsMenu := chkShowWindowsMenu.Checked;
  {$IFDEF VER100}
  IdeEnhancements.ShowApiHelpMenuItem := chkShowAPIMenu.Checked;
  IdeEnhancements.ShowCpuMenuItem := chkShowCPUMenu.Checked;
  {$ENDIF VER100}
  {$IFDEF VER120}
  IdeEnhancements.ShowAttachToMenuItem := chkShowAttachToMenu.Checked;
  {$ENDIF VER120}

  // Component palette
  IdeEnhancements.CPMultiLine := chkCPMultiLine.Checked;
  IdeEnhancements.CPScrollOpposite := chkCPScrollOpposite.Checked;
  IdeEnhancements.CPRaggedRight := chkCPRaggedRight.Checked;
  IdeEnhancements.CPAsButtons := chkCPAsButtons.Checked;
  {$IFDEF GX_VER120_up}
  IdeEnhancements.CPFlatButtons := chkCPFlat.Checked;
  {$ENDIF GX_VER120_up}
  IdeEnhancements.CPTabsInPopup := chkCPTabsInPopup.Checked;
  IdeEnhancements.CPTabsInPopupAlphaSort := chkCPTabsInPopupAlphaSort.Checked;
  // File saving
  IdeEnhancements.AutoSave := chkAutoSave.Checked;
  IdeEnhancements.AutoSaveInterval := spnMinutes.Value;
  IdeEnhancements.SaveDfmAsTxt := chkDfmAsText.Checked;
  // Fonts
  IdeEnhancements.OIFontEnabled := chkOIFontEnabled.Checked;
  IdeEnhancements.CPFontEnabled := chkCPFontEnabled.Checked;
  IdeEnhancements.OIFont.Assign(FOIFont);
  IdeEnhancements.CPFont.Assign(FCPFont);

  IdeEnhancements.SaveSettings;
end;

{$IFNDEF GX_EII}
procedure TfmConfiguration.SaveEditorEnhancements;
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.SaveEditorEnhancements;
var
  i: Integer;
begin
  Assert(EditorEnhancements <> nil);

  Assert(EditorEnhancements.ToolbarButtonsList <> nil);
  {$IFOPT D+}SendDebug('Clearing the toolbar buttons');{$ENDIF D+}
  EditorEnhancements.ToolbarButtonsList.Clear;
  {$IFOPT D+}SendDebug(Format('Adding %d new toolbar buttons', [lbSelected.Items.Count]));{$ENDIF D+}
  for i := 0 to lbSelected.Items.Count - 1 do
    EditorEnhancements.ToolbarButtonsList.Add(lbSelected.Items.Objects[i]);

  {$IFOPT D+}SendDebug('Setting ToolBarVisible to ' + BooleanText(chkEditorToolBar.Checked));{$ENDIF D+}
  EditorEnhancements.ToolBarVisible := chkEditorToolBar.Checked;
  {$IFOPT D+}SendDebug('Setting MultiLine to ' + BooleanText(chkMultiLine.Checked));{$ENDIF D+}
  EditorEnhancements.MultiLine := chkMultiLine.Checked;
  {$IFOPT D+}SendDebug('Setting HotTrack to ' + BooleanText(chkHotTrack.Checked));{$ENDIF D+}
  EditorEnhancements.HotTrack := chkHotTrack.Checked;
  {$IFOPT D+}SendDebug('Setting Buttons to ' + BooleanText(chkButtons.Checked));{$ENDIF D+}
  EditorEnhancements.Buttons := chkButtons.Checked;
  {$IFOPT D+}SendDebug('Setting ButtonsFlat to ' + BooleanText(chkEditTabButtonsFlat.Checked));{$ENDIF D+}
  EditorEnhancements.ButtonsFlat := chkEditTabButtonsFlat.Checked;
  {$IFOPT D+}SendDebug('Setting FontControl to ' + BooleanText(chkEditorFontControl.Checked));{$ENDIF D+}
  EditorEnhancements.FontControl := chkEditorFontControl.Checked;
  {$IFOPT D+}SendDebug('Setting EditorContextMenus to ' + BooleanText(chkLocalEditMenus.Checked));{$ENDIF D+}
  EditorEnhancements.EditorContextMenus := chkLocalEditMenus.Checked;
  {$IFOPT D+}SendDebug('Setting EditorContextMenusTop to ' + BooleanText(chkLocalEditMenusTop.Checked));{$ENDIF D+}
  EditorEnhancements.EditorContextMenusAtBottom := not chkLocalEditMenusTop.Checked;

  {$IFOPT D+}SendDebug('Setting ToolBarAlign to ' + IntToStr(rgAlign.ItemIndex));{$ENDIF D+}
  Assert(rgAlign.ItemIndex >= 0);
  EditorEnhancements.ToolBarAlign := TAlign(rgAlign.ItemIndex + 1);

  {$IFOPT D+}SendDebug('Setting EditorEnhancements.Enabled to ' + BooleanText(not chkDisableEDTEnhancements.Checked));{$ENDIF D+}
  EditorEnhancements.Enabled := not chkDisableEDTEnhancements.Checked;

  {$IFOPT D+}SendDebug('Saving editor enhancements settings');{$ENDIF D+}
  EditorEnhancements.SaveSettings;

  EditorEnhancements.ApplyToolbarSettings;
end;
{$ENDIF GX_EII}

{$IFNDEF GX_EII}
procedure TfmConfiguration.chkDisableEditorExpertsClick(Sender: TObject);
begin
  { do nothing }
end;
{$ELSE}
procedure TfmConfiguration.chkDisableEditorExpertsClick(Sender: TObject);
var
  Enable: Boolean;
begin
  Enable := not chkDisableEditorExperts.Checked;
  meHelp.Lines.Clear;
  if not Enable then
    GExpertsInst.FreeEditorExperts
  else
  begin
    {$IFOPT D+}SendDebug('Loading editor experts from the configuration dialog');{$ENDIF D+}
    GExpertsInst.LoadEditorExperts;
  end;
  lvEditorExperts.Enabled := Enable;
  ConfigInfo.EditorExpertsEnabled := Enable;
  btnShortcut.Enabled := Enable;
  btnConfigure.Enabled := Enable;
  if Enable then
    LoadEditorExperts
  else
    lvEditorExperts.Items.Clear;
  if Enable and (lvEditorExperts.Items.Count > 0) then
    lvEditorExperts.Items[0].Selected := True;
end;
{$ENDIF GX_EII}

procedure TfmConfiguration.chkDisableIdeEnhancementsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := not chkDisableIdeEnhancements.Checked;

  SetupGroupBox(gbxFonts, EnableState);
  SetupGroupBox(gbxCompPalette, EnableState);
  SetupGroupBox(gbxIDEMenu, EnableState);
  SetupGroupBox(gbxFileSaving, EnableState);
  SetupGroupBox(gbxTabDockHost, EnableState);

  chkFontEnabledClick(Self);
  chkCPAsButtonsClick(chkCPAsButtons);
  chkCPTabsInPopupClick(chkCPTabsInPopup);
  chkMultiLineTabDockHostClick(chkMultiLineTabDockHost);
  chkCPMultiLineClick(chkCPMultiLine);

  DisableUnsupportedIdeItems;
end;

procedure TfmConfiguration.chkDisableEDTEnhancementsClick(Sender: TObject);
{$IFDEF GX_EII}
var
  EnableState: Boolean;
{$ENDIF GX_EII}
begin
  {$IFDEF GX_EII}
  EnableState := not chkDisableEDTEnhancements.Checked;
  SetupGroupBox(gbxEditor, EnableState);
  SetupGroupBox(gbxEditMenu, EnableState);
  SetupGroupBox(gbxToolbar, EnableState);
  chkLocalEditMenusClick(chkLocalEditMenus);
  chkEditorToolbarClick(chkEditorToolbar);
  chkButtonsClick(chkButtons);
  {$ENDIF GX_EII}

  DisableUnsupportedEditorItems;
end;

procedure TfmConfiguration.DisableUnsupportedIdeItems;
begin
  // Option not integrated yet
  lblMinutes.Enabled := False;
  spnMinutes.Enabled := False;
  lblEvery.Enabled := False;
  chkAutoSave.Enabled := False;

  {$IFNDEF VER100}
  chkShowAPIMenu.Enabled := False;
  chkShowCPUMenu.Enabled := False;
  {$ENDIF VER100}
  {$IFNDEF VER120}
  chkShowAttachToMenu.Enabled := False;
  {$ENDIF VER120}
  {$IFDEF GX_VER130_up}
  chkDfmAsText.Enabled := False;
  {$ENDIF GX_VER130_up}
  {$IFNDEF GX_VER120_up}
  chkCPFlat.Enabled := False;
  //chkAddMenuImages.Enabled := False;
  SetupGroupBox(gbxTabDockHost, False);
  {$ENDIF GX_VER120_up}
end;

procedure TfmConfiguration.DisableUnsupportedEditorItems;
begin
  {$IFDEF GX_VER120_up}
  chkMultiLine.Enabled := False;
  {$ENDIF GX_VER120_up}

  {$IFDEF GX_VER130_up}
  // Delphi 5+ provide these local edit menu items by default
  chkLocalEditMenus.Enabled := False;
  chkLocalEditMenusTop.Enabled := False;
  {$ENDIF GX_VER130_up}

  {$IFDEF GX_VER110_up}
  // Apparently C++Builder 3+ and Delphi 4+ do not allow inserting the local menu
  // options at the top of the popup menu - all menu items there are destroyed.
  chkLocalEditMenusTop.Enabled := False;
  {$ENDIF GX_VER110_up}

  {$IFNDEF GX_VER120_up}
  chkEditTabButtonsFlat.Enabled := False;
  {$ENDIF GX_VER120_up}
end;

procedure TfmConfiguration.chkFontEnabledClick(Sender: TObject);
var
  IdeEnhancementsEnabled: Boolean;
begin
  IdeEnhancementsEnabled := not chkDisableIdeEnhancements.Checked;
  btnOIFont.Enabled := chkOIFontEnabled.Checked and IdeEnhancementsEnabled;
  btnCPFont.Enabled := chkCPFontEnabled.Checked and IdeEnhancementsEnabled;
end;

procedure TfmConfiguration.btnFontClick(Sender: TObject);
begin
  if Sender = btnOIFont then
  begin
    dlgFont.Font.Assign(FOIFont);
    if dlgFont.Execute then
      FOIFont.Assign(dlgFont.Font);
  end
  else
  if Sender = btnCPFont then
  begin
    dlgFont.Font.Assign(FCPFont);
    if dlgFont.Execute then
      FCPFont.Assign(dlgFont.Font);
  end;
end;

procedure TfmConfiguration.chkAutoSaveClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked;
  lblMinutes.Enabled := EnableState;
  lblEvery.Enabled := EnableState;
  spnMinutes.Enabled := EnableState;
end;

procedure TfmConfiguration.chkCPAsButtonsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
{$IFNDEF GX_VER120_up}
  EnableState := False;
{$ELSE}
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableIDEEnhancements.Checked;
{$ENDIF GX_VER120_up}
  chkCPFlat.Enabled := EnableState;
end;

procedure TfmConfiguration.chkLocalEditMenusClick(Sender: TObject);
var
  EnableState: Boolean;
begin
{$IFNDEF VER100}
  EnableState := False;
{$ELSE}
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableEDTEnhancements.Checked;
{$ENDIF VER100}
  chkLocalEditMenusTop.Enabled := EnableState;
end;

procedure TfmConfiguration.chkCPTabsInPopupClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableIDEEnhancements.Checked;
  chkCPTabsInPopupAlphaSort.Enabled := EnableState;
end;

procedure TfmConfiguration.lbSelectedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    sbRemoveClick(sbRemove);
end;

procedure TfmConfiguration.pcConfigChange(Sender: TObject);
var
  Enable: Boolean;
  i: Integer;
begin
  if pcConfig.ActivePage = tshToolbar then
  begin
    Enable := (not chkDisableEDTEnhancements.Checked) and chkEditorToolbar.Checked;
    for i := 0 to tshToolbar.ControlCount - 1 do
      tshToolbar.Controls[i].Enabled := Enable;
  end;
end;

procedure TfmConfiguration.chkCPMultiLineClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableIdeEnhancements.Checked;

  chkCPScrollOpposite.Enabled := EnableState;
  chkCPRaggedRight.Enabled := EnableState;
  if not EnableState then
  begin
    chkCPScrollOpposite.Checked := False;
    chkCPRaggedRight.Checked := False;
  end;
end;

procedure TfmConfiguration.chkButtonsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableEDTEnhancements.Checked;

  chkEditTabButtonsFlat.Enabled := EnableState;
  DisableUnsupportedEditorItems;
end;

procedure TfmConfiguration.chkEditorToolbarClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableEDTEnhancements.Checked;

  rgAlign.Enabled := EnableState;
end;

procedure TfmConfiguration.chkMultiLineTabDockHostClick(Sender: TObject);
var
  EnableState: Boolean;
begin
{$IFDEF GX_VER120_up}
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableIDEEnhancements.Checked;
{$ELSE}
  EnableState := False;
{$ENDIF GX_VER120_up}

  chkDefaultMultiLineTabDockHost.Enabled := EnableState;
end;

{$IFDEF GX_VER120_up}
procedure TfmConfiguration.sbxExpertsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with sbxExperts.VertScrollBar do
    Position := Position + FThumbSize;
  Handled := True;
end;

procedure TfmConfiguration.sbxExpertsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with sbxExperts.VertScrollBar do
    Position := Position - FThumbSize;
  Handled := True;
end;
{$ENDIF GX_VER120_up}

end.


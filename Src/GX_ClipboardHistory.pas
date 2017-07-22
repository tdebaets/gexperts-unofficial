unit GX_ClipboardHistory;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ClipBrd, Registry, Menus, GX_Experts,
  ToolIntf, ComCtrls, GX_IdeDock;

const
  UM_RESIZECOLS = WM_USER + 523;

type
  TClipInfo = class(TObject)
  private
    FClipTimeStamp: string;
    FClipString: string;
  public
    property ClipTimeStamp: string read FClipTimeStamp write FClipTimeStamp;
    property ClipString: string read FClipString write FClipString;
  end;

  TClipExpert = class;

  TfmClipboardHistory = class(TfmIdeDockForm)
    Splitter: TSplitter;
    mmoClipText: TMemo;
    pnlToolBar: TPanel;
    sbClear: TSpeedButton;
    sbHelp: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    About1: TMenuItem;
    Help2: TMenuItem;
    N1: TMenuItem;
    mitClear: TMenuItem;
    lvClip: TListView;
    sbCopy: TSpeedButton;
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvClipDblClick(Sender: TObject);
    procedure lvClipChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormActivate(Sender: TObject);
    procedure lvClipKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FHelperWindow: TWinControl;
    IgnoreClip: Boolean;
    FDataList: TList;
    SplitterRatio: Double;
    procedure ClearDataList;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
    {$IFDEF GX_VER120_up}
    procedure LoadClips;
    procedure SaveClips;
    {$ENDIF GX_VER120_up}
  public
    { Public declarations }
    procedure Clear;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

  TClipExpert = class(TGX_EnhExpert)
  private
    FMaxClip: Integer;
    FAutoStart: Boolean;
    FAutoClose: Boolean;
    FStoragePath: string;
    function GetStorageFile: string;
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
    property MaxClip: Integer read FMaxClip write FMaxClip;
    property StorageFile: string read GetStorageFile;
  end;

var
  fmClipboardHistory: TfmClipboardHistory = nil;
  ClipExpert: TClipExpert = nil;

implementation

{$R *.DFM}

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_ConfigurationInfo,
  {$IFDEF GX_VER120_up}
  ActiveX, E_StgStr,
  {$ENDIF GX_VER120_up}
  GX_GExperts, GX_ClipboardOptions, GX_GenFunc;

const
  SAVED_CLIPS = 'SAVED_CLIPS';
  STORAGE_FILENAME = 'GXClipbd.gxs';

type
  THelperWinControl = class(TWinControl)
  private
    FPrevWindow: HWnd;
    procedure WMChangeCBChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WMDrawClipBoard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function FirstLineOfText(const AClipString: string): string;
begin
  with TStringList.Create do
  try
    Text := AClipString;
    if Count > 0 then
      Result := Strings[0];
  finally
    Free;
  end;
end;

{ THelperWinControl }

constructor THelperWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'ClipboardChainHelperWindow';
  // the clipboard chaining only works properly if this window is
  // not parented by the the clip form.  The desktop window may not
  // be the best window to choose but it works.
  ParentWindow := GetDesktopWindow;
  Visible := False;
  {$IFOPT D+}SendDebug('In THelperWinControl Create'); {$ENDIF}
  FPrevWindow := SetClipBoardViewer(Self.Handle);
  {$IFOPT D+}SendDebug('FPrevWindow = ' + IntToStr(FPrevWindow)); {$ENDIF}
end;

destructor THelperWinControl.Destroy;
begin
  {$IFOPT D+}SendDebug('In THelperWinControl Destroy'); {$ENDIF}
  try
    ChangeClipBoardChain(Self.Handle, FPrevWindow);
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebug('Clip Chain Destroy: ' + E.Message); {$ENDIF}
    end;
  end;
  inherited Destroy;
end;

procedure THelperWinControl.WMChangeCBChain(var Msg: TMessage);
begin
  {$IFOPT D+}SendDebug('In THelperWinControl WMChangeCBChain'); {$ENDIF}
  if Msg.wParam = LongInt(FPrevWindow) then
    FPrevWindow := Msg.lParam
  else if (FPrevWindow <> 0) then
    SendMessage(FPrevWindow, WM_CHANGECBCHAIN, Msg.WParam, Msg.LParam);
end;

procedure THelperWinControl.WMDrawClipBoard(var Msg: TMessage);
var
  ItemCount: Integer;
  ClipItem: TListItem;
  Info: TClipInfo;
begin
  {$IFOPT D+}SendDebug('In THelperWinControl WMDrawClipBoard'); {$ENDIF}
  if not Assigned(fmClipboardHistory) then
    Exit;
  if fmClipboardHistory.IgnoreClip then
    Exit;
  try
    if ClipBoard.HasFormat(CF_TEXT) then
    begin
      {$IFOPT D+}SendDebug('New clipboard string: ' + ClipBoard.AsText); {$ENDIF}
      fmClipboardHistory.mmoClipText.Text := ClipBoard.AsText;

      Info := TClipInfo.Create;
      fmClipboardHistory.FDataList.Insert(0, Info);
      Info.ClipString := fmClipboardHistory.mmoClipText.Text;
      Info.ClipTimeStamp := TimeToStr(Time);
      {$IFOPT D+}SendDebug('Inserted ClipString: ' + Info.ClipString); {$ENDIF}

      ClipItem := fmClipboardHistory.lvClip.Items.Insert(0);
      ClipItem.Caption := Info.ClipTimeStamp;
      ClipItem.SubItems.Add(Trim(FirstLineOfText(Clipboard.AsText)));
      {$IFOPT D+}SendDebug('First Line: ' + Trim(FirstLineOfText(Clipboard.AsText))); {$ENDIF}
      ClipItem.Data := Info;

      ItemCount := fmClipboardHistory.lvClip.Items.Count;
      if ItemCount > ClipExpert.MaxClip then
      begin
        Dec(ItemCount);
        fmClipboardHistory.lvClip.Items.Delete(ItemCount);
        TClipInfo(fmClipboardHistory.FDataList[ItemCount]).Free;
        fmClipboardHistory.FDataList.Delete(ItemCount);
      end;
      fmClipboardHistory.lvClip.Selected := fmClipboardHistory.lvClip.Items[0];
      fmClipboardHistory.lvClip.ItemFocused := fmClipboardHistory.lvClip.Selected;
      //fmClip.lvClipClick(fmClip.lvClip);
    end;
    if FPrevWindow > 0 then
      SendMessage(FPrevWindow, WM_DRAWCLIPBOARD, Msg.WParam, Msg.LParam);
  except
    on E: Exception do
    begin
      // Ignore Exceptions
    end;
  end;
end;

{ TfmClipboardHistory }

procedure TfmClipboardHistory.FormResize(Sender: TObject);
begin
  with lvClip do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
  mmoClipText.Height := Trunc(SplitterRatio * (mmoClipText.Height + lvClip.Height));
end;

procedure TfmClipboardHistory.SplitterMoved(Sender: TObject);
begin
  SplitterRatio := mmoClipText.Height / (lvClip.Height + mmoClipText.Height);
  FormResize(Self);
end;

procedure TfmClipboardHistory.FormCreate(Sender: TObject);
begin
  try
    {$IFOPT D+}SendDebug('Creating clipboard history data list'); {$ENDIF}
    FDataList := TList.Create;
    SplitterRatio := 0.50;
    LoadSettings;
    CenterForm(Self);
    IgnoreClip := False;
    {$IFOPT D+}SendDebug('Creating clipboard history THelperWinControl'); {$ENDIF}
    // the helper window is parented by the Desktop Window and
    // it chains the clipboard for us.
    FHelperWindow := THelperWinControl.Create(nil);
    {$IFOPT D+}SendDebug('Clipboard history helper window created'); {$ENDIF}

    {$IFDEF GX_VER120_up}
    // now load any saved clips from our structured storage
    LoadClips;
    {$ENDIF GX_VER120_up}

    // Doesn't appear to work
    //SetWindowLong(lvClip.Handle, GWL_STYLE,
    //   GetWindowLong(lvClip.Handle, GWL_STYLE) and (not WS_HSCROLL));
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebug('TfmClipboardHistory.FormCreate: ' + E.Message); {$ENDIF}
      ShowExceptionErrorMessage(E);
    end;
  end;
  inherited;
end;

procedure TfmClipboardHistory.FormDestroy(Sender: TObject);
begin
  try
    {$IFDEF GX_VER120_up}
    // First save our clips for posterity
    SaveClips;
    {$ENDIF GX_VER120_up}
    // Now free everything
    FHelperWindow.Free;
    Clear;
    FDataList.Free;
    SaveSettings;
  except
    on E: Exception do
    begin
      ShowExceptionErrorMessage(E);
      {$IFOPT D+}SendDebug('TfmClipboardHistory.FormDestroy: ' + E.Message); {$ENDIF}
    end;
  end;
  inherited;
  fmClipboardHistory := nil;
end;

procedure TfmClipboardHistory.sbCopyClick(Sender: TObject);
var
  ClipItem: TListItem;
  Info: TClipInfo;
  idx: Integer;
begin
  if lvClip.Selected = nil then Exit;
  try
    IgnoreClip := True;
    if mmoClipText.SelLength = 0 then
    begin
      idx := lvClip.Selected.Index;
      ClipBoard.AsText := mmoClipText.Text;

      lvClip.Items.Delete(idx);
      TClipInfo(FDataList[idx]).Free;
      FDataList.Delete(idx);

      Info := TClipInfo.Create;
      FDataList.Insert(0, Info);
      Info.ClipString := mmoClipText.Text;
      Info.ClipTimeStamp := TimeToStr(Time);

      ClipItem := lvClip.Items.Insert(0);
      ClipItem.Caption := Info.ClipTimeStamp;
      ClipItem.SubItems.Add(Trim(mmoClipText.Lines[0]));
      ClipItem.Data := Info;

      lvClip.Selected := lvClip.Items[0];
      lvClip.ItemFocused := lvClip.Selected;
    end
    else
      mmoClipText.CopyToClipBoard;
    if ClipExpert.FAutoClose then Close;
  finally
    Application.ProcessMessages;
    IgnoreClip := False;
  end;
end;

procedure TfmClipboardHistory.sbClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TfmClipboardHistory.ClearDataList;
var
  i: Integer;
begin
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
      TClipInfo(FDataList.Items[i]).Free;
    FDataList.Clear;
  end;
  lvClip.Items.Clear;
end;

procedure TfmClipboardHistory.Clear;
begin
  try
    ClearDataList;
    mmoClipText.Lines.Clear;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClipboardHistory.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  try
    // do not localize
    RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
    try
      RegIni.WriteInteger('ClipWin', 'Left', Left);
      RegIni.WriteInteger('ClipWin', 'Top', Top);
      RegIni.WriteInteger('ClipWin', 'Width', Width);
      RegIni.WriteInteger('ClipWin', 'Height', Height);
      RegIni.WriteInteger('ClipWin', 'SplitterRatio', Round(SplitterRatio * 100));
    finally
      RegIni.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClipboardHistory.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  try
    // do not localize
    RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
    try
      Left := RegIni.ReadInteger('ClipWin', 'Left', Left);
      Top := RegIni.ReadInteger('ClipWin', 'Top', Top);
      Width := RegIni.ReadInteger('ClipWin', 'Width', Width);
      Height := RegIni.ReadInteger('ClipWin', 'Height', Height);
      SplitterRatio := RegIni.ReadInteger('ClipWin', 'SplitterRatio', 50) / 100;
      mmoClipText.Height :=  Trunc(SplitterRatio * (mmoClipText.Height + lvClip.Height));
    finally
      RegIni.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmClipboardHistory.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 13);
end;

procedure TfmClipboardHistory.Edit1Click(Sender: TObject);
begin
  Copy1.Enabled := (mmoClipText.SelLength > 0) or (lvClip.Selected <> nil);
end;

procedure TfmClipboardHistory.Contents1Click(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTENTS, 0);
end;

procedure TfmClipboardHistory.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmClipboardHistory.Exit1Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfmClipboardHistory.Help2Click(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 13);
end;

procedure TfmClipboardHistory.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfmClipboardHistory.lvClipDblClick(Sender: TObject);
begin
  sbCopyClick(Self);
end;

{$IFDEF GX_VER120_up}
procedure TfmClipboardHistory.LoadClips;
var
  Storage: TStructuredStorage;
  Stream: IStream;
  Success: Boolean;
  i: Integer;
  NumClips: Integer;
  TimeStr: string;
  ClipStr: string;
  Info: TClipInfo;
  ClipItem: TListItem;
begin
  ClearDataList;
  Storage := TStructuredStorage.Create;
  try
    Success := Storage.OpenStorageFile(ClipExpert.StorageFile,
      STGM_DIRECT or STGM_READ or STGM_SHARE_DENY_WRITE);
    if Success then
    try
      if Storage.OpenStream(Storage.StorageFile, SAVED_CLIPS,
        STGM_DIRECT or STGM_READ or STGM_SHARE_EXCLUSIVE, Stream) then
      begin
        Success := Storage.ReadInt(Stream, NumClips);
        if Success then
        begin
          for i := 0 to NumClips - 1 do
          begin
            Storage.ReadString(Stream, TimeStr);
            Storage.ReadString(Stream, ClipStr);

            Info := TClipInfo.Create;
            FDataList.Add(Info);
            Info.ClipString := ClipStr;
            Info.ClipTimeStamp := TimeStr;

            ClipItem := lvClip.Items.Add;
            ClipItem.Caption := Info.ClipTimeStamp;
            ClipItem.SubItems.Add(FirstLineOfText(ClipStr));
            ClipItem.Data := Info;
          end;
          if lvClip.Items.Count > 0 then
          begin
            lvClip.Selected := lvClip.Items[0];
            lvClip.ItemFocused := lvClip.Selected;
            //lvClipClick(lvClip);
          end;
        end;
      end;
    finally
      Stream := nil; // closes the stream ?
      Storage.CloseStorageFile;
    end;
    // before we free our structured storage obect
    // try to defragment our storage file
    Storage.DefragmentStorageFile(ClipExpert.StorageFile);
  finally
    Storage.Free;
  end;
end;

procedure TfmClipboardHistory.SaveClips;
var
  Storage: TStructuredStorage;
  Success: Boolean;
  i: Integer;
  Stream: IStream;
begin
  try
    Storage := TStructuredStorage.Create;
    try
      if FileExists(ClipExpert.StorageFile) then
      begin
        Success := Storage.OpenStorageFile(ClipExpert.StorageFile,
          STGM_DIRECT or STGM_READWRITE or
          STGM_SHARE_EXCLUSIVE);
      end
      else
      begin
        Success := Storage.CreateStorageFile(ClipExpert.StorageFile,
          STGM_DIRECT or STGM_READWRITE or
          STGM_SHARE_EXCLUSIVE);
      end;
      if Success then
      try
        // delete the existing structured storage file
        Storage.DestroyElement(Storage.StorageFile, SAVED_CLIPS);
        // create a new structured storage file
        Success := Storage.CreateStream(Storage.StorageFile, SAVED_CLIPS,
          STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, Stream);
        if Success then
        begin
          Storage.WriteInt(Stream, FDataList.Count);
          {$IFOPT D+}SendDebug(Format('Saving %d clipboard strings to storage', [FDataList.Count])); {$ENDIF}
          for i := 0 to FDataList.Count - 1 do
          begin
            Storage.WriteString(Stream, TClipInfo(FDataList[i]).ClipTimeStamp);
            Storage.WriteString(Stream, TClipInfo(FDataList[i]).ClipString);
          end;
        end;
      finally
        Stream := nil;
        Storage.CloseStorageFile;
      end;
    finally
      Storage.Free;
    end;
  except
    {$IFOPT D+}SendDebugEx('Unable to successfully write clipboard storage file.  Deleting it.', mtError); {$ENDIF}
    // The file is likely corrupt if we can't write everything to it
    DeleteFile(ClipExpert.StorageFile);
  end;
end;
{$ENDIF GX_VER120_up}

procedure TfmClipboardHistory.lvClipChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if lvClip.Selected <> nil then
  begin
    mmoClipText.Lines.Text := TClipInfo(lvClip.Selected.Data).ClipString;
    sbCopy.Enabled := True;
    Copy1.Enabled := True;
  end
  else
  begin
    sbCopy.Enabled := False;
    Copy1.Enabled := True;
  end;
end;

procedure TfmClipboardHistory.UMResizeCols(var Msg: TMessage);
begin
  FormResize(Self);
end;

procedure TfmClipboardHistory.FormActivate(Sender: TObject);
begin
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

procedure TfmClipboardHistory.lvClipKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then
    sbCopyClick(Self);
end;

{ TClipExpert }

constructor TClipExpert.Create;
begin
  inherited Create;
  FStoragePath := ConfigInfo.ConfigPath;
  HasConfigOptions := True;

  HasMenuItem := True;
  FMaxClip := 20;

  ClipExpert.Free;
  ClipExpert := Self;
end;

destructor TClipExpert.Destroy;
begin
  fmClipboardHistory.Free;
  fmClipboardHistory := nil;
  ClipExpert := nil;
  inherited Destroy;
end;

function TClipExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Clipboard &History';
begin
  Result := SMenuCaption;
end;

function TClipExpert.GetMenuName: string;
begin
  Result := 'GX_Clip';
end;

function TClipExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TClipExpert.GetName: string;
begin
  Result := 'Clipboard_Expert';
end;

function TClipExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Clipboard History';
begin
  Result := SDisplayName;
end;

procedure TClipExpert.Click(Sender: TObject);
begin
  // If the form doesn't exist, create it.
  if fmClipboardHistory = nil then
    fmClipboardHistory := TfmClipboardHistory.Create(nil);
  // Show the form using IdeDockManager.ShowForm.
  IdeDockManager.ShowForm(fmClipboardHistory);
end;

procedure TClipExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FMaxClip := RegIni.ReadInteger('Clip', 'Maximum', 20);
    FAutoStart := RegIni.ReadBool('Clip', 'AutoStart', False);
    FAutoClose := RegIni.ReadBool('Clip', 'AutoClose', False);
  finally
    RegIni.Free;
  end;
  // This procedure is only called once so it is safe to register the form here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmClipboardHistory, fmClipboardHistory, 'fmClipboardHistory');
    // FormClass      Form var       form Name (must be unique).
  if FAutoStart and (fmClipboardHistory = nil) then
    fmClipboardHistory := TfmClipboardHistory.Create(nil);
end;

procedure TClipExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('Clip', 'Maximum', FMaxClip);
    RegIni.WriteBool('Clip', 'AutoStart', FAutoStart);
    RegIni.WriteBool('Clip', 'AutoClose', FAutoClose);
  finally
    RegIni.Free;
  end;
end;

procedure TClipExpert.Configure;
var
  Dlg: TfmClipboardOptions;
begin
  Dlg := TfmClipboardOptions.Create(nil);
  try
    Dlg.edtMaxClip.Text := IntToStr(FMaxClip);
    Dlg.chkAutoStart.Checked := FAutoStart;
    Dlg.chkAutoClose.Checked := FAutoClose;
    if Dlg.ShowModal = mrOK then
    begin
      FAutoStart := Dlg.chkAutoStart.Checked;
      FAutoClose := Dlg.chkAutoClose.Checked;
      try
        FMaxClip := StrToInt(Dlg.edtMaxClip.Text);
      except
        on E: EConvertError do
        begin
          // Swallow exceptions
          FMaxClip := 20;
        end;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

function TClipExpert.IconFileName: string;
begin
  Result := 'Clip';
end;

function TClipExpert.GetStorageFile: string;
begin
  Result := FStoragePath + STORAGE_FILENAME;
end;

procedure TClipExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      fmClipboardHistory.Free;
      fmClipboardHistory := nil;
    end;
  end;
end;

initialization
  RegisterGX_Expert(TClipExpert);
end.


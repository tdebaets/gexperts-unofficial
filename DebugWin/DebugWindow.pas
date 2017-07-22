unit DebugWindow;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, ExtCtrls, StdCtrls, Registry,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  ShellAPI, Menus, ComCtrls, TrayIcon;

type
  TDebugType = (dtMessage, dtSQL);

type
  TDebugMessage = record
    DebugType: TDebugType;
    MessageType: TMsgDlgType;
    Msg: string;
  end;

  TfmDebug = class(TForm)
    pnToolBar: TPanel;
    sbClear: TSpeedButton;
    sbCopy: TSpeedButton;
    pmuTaskBar: TPopupMenu;
    mitTrayShutdown: TMenuItem;
    mitShow: TMenuItem;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ClearBuffer1: TMenuItem;
    mitSep: TMenuItem;
    mitShutdown: TMenuItem;
    mitExit: TMenuItem;
    mitOptions: TMenuItem;
    ilDebug: TImageList;
    lvMessages: TListView;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Image1: TImage;
    mitTrayClear: TMenuItem;
    sbSave: TSpeedButton;
    Export1: TMenuItem;
    dlgSaveLog: TSaveDialog;
    pmuListbox: TPopupMenu;
    mnuClearItems: TMenuItem;
    mnuShowItemInDialog: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mitTrayShutdownClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mitShowClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mitExitClick(Sender: TObject);
    procedure mitOptionsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbSaveClick(Sender: TObject);
    procedure lvMessagesDblClick(Sender: TObject);
    procedure pmuListboxPopup(Sender: TObject);
  private
    AllowClose: Boolean;
    OnTop: Boolean;
    TaskIcon: TTrayIcon;
    procedure COPYDATA(var Message: TWMCopyData); message WM_COPYDATA;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure WinMsgHandler(var Msg: TMsg; var Handled: Boolean);
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Message: TMessage); message WM_ENDSESSION;
  end;

const
  TopMenu = 100;

var
  fmDebug: TfmDebug;

implementation

{$R *.DFM}

uses
  ClipBrd,
  DebugOptions;

procedure TfmDebug.COPYDATA(var Message: TWMCopyData);
var
  NewMsg: TDebugMessage;
  ListItem: TListItem;

  procedure AddMessage(MessageType: TMsgDlgType; const MessageText: string);
  begin
    if ConfigInfo.Bottom or (lvMessages.Items.Count = 0) then
      ListItem := lvMessages.Items.Add
    else
      ListItem := lvMessages.Items.Insert(0);
    ListItem.Caption := MessageText;
    ListItem.ImageIndex := Ord(MessageType);
    ListItem.SubItems.Add(TimeToStr(Time));
  end;

  procedure GetMessage;
  var
    CData: TCopyDataStruct;
    MessageContent: PChar;
    i: Integer;
  begin
    CData := Message.CopyDataStruct^;
    MessageContent := CData.lpData;
    if MessageContent[0] = #3 then
    begin
      sbClearClick(sbClear);
      Exit;
    end;

    if MessageContent[0] = #2 then
      NewMsg.DebugType := dtSQL
    else
      NewMsg.DebugType := dtMessage;
    NewMsg.MessageType := TMsgDlgType(Integer(MessageContent[1]) - 1);
    i := 2;
    while MessageContent[i] <> #0 do
    begin
      NewMsg.Msg := NewMsg.Msg + MessageContent[i];
      Inc(i);
    end;
  end;

var
  OldClientWidth: Integer;
begin
  GetMessage;
  OldClientWidth := lvMessages.ClientWidth;
  if NewMsg.DebugType = dtMessage then
    AddMessage(NewMsg.MessageType, NewMsg.Msg);
  // Resize the header when the scrollbar is added
  if not (lvMessages.ClientWidth = OldClientWidth) then
    FormResize(Self);
  if not Visible then
    TaskIcon.Icon := Image1.Picture.Icon;
  if ConfigInfo.OnMessage then
    Show;
end;

procedure TfmDebug.FormCreate(Sender: TObject);
resourcestring
  SAlwaysOnTop = '&Always On Top';
begin
  //ilDebug.ResourceLoad(rtBitmap, 'GLOGIMAGES', clOlive);
  //if ilDebug.Count = 0 then
  //begin
  //  MessageDlg('Could not load Images', mtError, [mbOK], 0);
  //  Exit;
  //end;
  Application.OnMessage := WinMsgHandler;
  { Add a separator }
  AppendMenu(GetSystemMenu(Self.Handle, False), MF_SEPARATOR, 0, '');
  { Add your menu choice. Since the Item ID is high, using the MF_BYPOSITION
    constant will place it last on the system menu }
  AppendMenu(GetSystemMenu(Self.Handle, False), MF_BYPOSITION, TopMenu, PChar(SAlwaysOnTop));
  OnTop := False;
  Application.ShowHint := True;

  TaskIcon := TTrayIcon.Create(Self);
  TaskIcon.Icon := Icon;
  TaskIcon.Active := True;
  TaskIcon.PopupMenu := pmuTaskBar;
  TaskIcon.ToolTip := Application.Title;
  TaskIcon.OnDblClick := TrayIconDblClick;
  AllowClose := False;
  LoadSettings;
end;

procedure TfmDebug.TrayIconDblClick(Sender: TObject);
begin
  Show;
  BringToFront;
  Application.BringToFront;
end;

procedure TfmDebug.sbClearClick(Sender: TObject);
begin
  lvMessages.Items.BeginUpdate;
  lvMessages.Items.Clear;
  lvMessages.Items.EndUpdate;
  TaskIcon.Icon := Icon;
end;

procedure TfmDebug.sbCopyClick(Sender: TObject);
var
  CopyStrings: TStrings;
  CopyText: PChar;
  i: Integer;
begin
  CopyStrings := TStringList.Create;
  try
    for i := 0 to lvMessages.Items.Count - 1 do
      if lvMessages.Items[i].Selected then
        CopyStrings.Add(IntToStr(lvMessages.Items[i].ImageIndex) + #9 + lvMessages.Items[i].Caption + #9 + lvMessages.Items[i].SubItems[0]);

    CopyText := CopyStrings.GetText;
    try
      Clipboard.SetTextBuf(CopyText);
    finally
      StrDispose(CopyText);
    end;
  finally
    CopyStrings.Free;
  end;
end;

procedure TfmDebug.FormResize(Sender: TObject);
begin
  if lvMessages.ClientWidth > 100 then
    lvMessages.Column[0].Width := lvMessages.ClientWidth - lvMessages.Column[1].Width;
end;

procedure TfmDebug.FormShow(Sender: TObject);
begin
  TaskIcon.Icon := Icon;
  FormResize(Self);
end;

procedure TfmDebug.mitTrayShutdownClick(Sender: TObject);
begin
  AllowClose := True;
  Close;
end;

procedure TfmDebug.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := AllowClose;
  if not CanClose then
    Hide;
end;

procedure TfmDebug.mitShowClick(Sender: TObject);
begin
  Show;
end;

procedure TfmDebug.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  RegIni := TRegIniFile.Create('Software\GExperts');
  try
    RegIni.WriteInteger('Debug', 'Left', Left);
    RegIni.WriteInteger('Debug', 'Top', Top);
    RegIni.WriteInteger('Debug', 'Width', Width);
    RegIni.WriteInteger('Debug', 'Height', Height);
    RegIni.WriteString('Debug', 'SavePath', dlgSaveLog.InitialDir);
  finally
    RegIni.Free;
  end;
end;

procedure TfmDebug.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;

  RegIni := TRegIniFile.Create('Software\GExperts');
  try
    Left := RegIni.ReadInteger('Debug', 'Left', Left);
    Top := RegIni.ReadInteger('Debug', 'Top', Top);
    Width := RegIni.ReadInteger('Debug', 'Width', Width);
    Height := RegIni.ReadInteger('Debug', 'Height', Height);
    dlgSaveLog.InitialDir := RegIni.ReadString('Debug', 'SavePath', '');
  finally
    RegIni.Free;
  end;
end;

procedure TfmDebug.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfmDebug.WinMsgHandler(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.Message = WM_SYSCOMMAND then { if the message is a system one... }
    if Msg.wParam = TopMenu then
      if not OnTop then
      begin
        SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
        CheckMenuItem(GetSystemMenu(Self.Handle, False), TopMenu, MF_CHECKED);
        OnTop := True;
      end
      else
      begin
        SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
        CheckMenuItem(GetSystemMenu(Self.Handle, False), TopMenu, MF_UNCHECKED);
        OnTop := False;
      end
    else
      if Msg.wParam = SC_RESTORE then
        Show;
end;

procedure TfmDebug.mitExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmDebug.mitOptionsClick(Sender: TObject);
begin
  with TfmDebugOptions.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmDebug.WMQueryEndSession(var Message: TMessage);
begin
  TaskIcon.Active := False;
  AllowClose := True;
  Close;
  inherited;
end;

procedure TfmDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TaskIcon.Active := False;
  TaskIcon.Free;
  TaskIcon := nil;
  Action := caFree;
end;

procedure TfmDebug.WMEndSession(var Message: TMessage);
begin
  AllowClose := True;
  Close;
  inherited;
end;

procedure TfmDebug.sbSaveClick(Sender: TObject);
resourcestring
  SSaveError = 'Error saving messages: ';
var
  i: Integer;
  Messages: TStringList;
begin
  if lvMessages.Items.Count > 0 then
  begin
    // Note: this is not part of the IDE, hence we
    // do not do any "current folder" protection.
    if dlgSaveLog.Execute then
    try
      Messages := TStringList.Create;
      try
        for i := 0 to lvMessages.Items.Count - 1 do
        begin
          Messages.Add(
             IntToStr(lvMessages.Items[i].ImageIndex) + #9 +
             lvMessages.Items[i].Caption + #9 +
             lvMessages.Items[i].SubItems[0]);
        end;
        Messages.SaveToFile(dlgSaveLog.FileName);
        dlgSaveLog.InitialDir := ExtractFilePath(dlgSaveLog.FileName);
      finally
        Messages.Free;
      end;
    except
      on E: Exception do
        MessageDlg(SSaveError + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfmDebug.lvMessagesDblClick(Sender: TObject);
begin
  if lvMessages.Selected <> nil then
    MessageDlg(lvMessages.Selected.Caption, mtInformation, [mbOK], 0);
end;

procedure TfmDebug.pmuListboxPopup(Sender: TObject);
begin
  mnuShowItemInDialog.Enabled := lvMessages.Selected <> nil;
end;

end.


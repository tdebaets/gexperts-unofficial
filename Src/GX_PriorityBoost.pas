unit GX_PriorityBoost;

// Original Author: Stefan Hoffmeister <Stefan.Hoffmeister@econos.de>

{$I GX_CondDefine.inc}

interface

uses
  ToolIntf, ExptIntf,
  GX_Experts,
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls;

type
  TGxBoostSetting = (bsIdlePriority, bsRealtimePriority, bsTunePriority);

type
  TfmPriorityBoostConfig = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    btHelp: TButton;
    gbBoost: TGroupBox;
    rbIdlePriority: TRadioButton;
    rbRealtimePriority: TRadioButton;
    rbTunePriority: TRadioButton;
    gbBoostTuning: TGroupBox;
    lNeutral: TLabel;
    lLowPriority: TLabel;
    lHighPriority: TLabel;
    trkBoost: TTrackBar;
    cbApplicationRealtime: TCheckBox;
    procedure rbGenericPriorityClick(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
  private
    procedure SetTuningGroupboxState(Enabled: Boolean);
    function GetBoostSetting: TGxBoostSetting;
    procedure SetBoostSetting(const Value: TGxBoostSetting);
    function GetAppRealTime: Boolean;
    procedure SetAppRealTime(const Value: Boolean);
    function GetBoost: Integer;
    procedure SetBoost(const Value: Integer);
  public
    property Boost: Integer read GetBoost write SetBoost;
    property BoostSetting: TGxBoostSetting read GetBoostSetting write SetBoostSetting;
    property AppRealTime: Boolean read GetAppRealTime write SetAppRealTime;
  end;

  TGxBoostNotifier = class(TIAddInNotifier)
  private
    FOwner: TGX_Expert;
    FBoostSetting: TGxBoostSetting;
    FPriorityBoost: Integer;
    FAppRealTimeClass: Boolean;
    procedure SetBoostSetting(const Value: TGxBoostSetting);
    procedure SetPriorityBoost(const Value: Integer);
    procedure SetAppRealTimeClass(const Value: Boolean);
  private
    FThreadClassPriority: Integer;
    FSavedThreadPriority: Integer;
    FSavedAppClass: DWORD;
    procedure DoBoostPriority;
    procedure DoRestorePriority;
  public
    constructor Create(AOwner: TGX_Expert);

    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;

    property AppRealTimeClass: Boolean read FAppRealTimeClass write SetAppRealTimeClass;
    property BoostSetting: TGxBoostSetting read FBoostSetting write SetBoostSetting;
    property PriorityBoost: Integer read FPriorityBoost write SetPriorityBoost;
  end;

  TGxPriorityBoostExpert = class(TGX_Expert)
  private
    FCompileNotifier: TGxBoostNotifier;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  Registry, GX_GExperts, GX_ConfigurationInfo, GX_GenFunc;

{ TfmGxPriorityBoostConfig }

function TfmPriorityBoostConfig.GetBoostSetting: TGxBoostSetting;
begin
  if rbIdlePriority.Checked then
    Result := bsIdlePriority
  else if rbRealtimePriority.Checked then
    Result := bsRealtimePriority
  else if rbTunePriority.Checked then
    Result := bsTunePriority
  else
    raise Exception.Create('Internal error');
end;

procedure TfmPriorityBoostConfig.rbGenericPriorityClick(Sender: TObject);
begin
  SetTuningGroupboxState(rbTunePriority.Checked);
  cbApplicationRealtime.Enabled := rbRealtimePriority.Checked;
end;

procedure TfmPriorityBoostConfig.SetBoostSetting(const Value: TGxBoostSetting);
begin
  case Value of
    bsIdlePriority:
      rbIdlePriority.Checked := True;
    bsRealtimePriority:
      rbRealtimePriority.Checked := True;
    bsTunePriority:
      rbTunePriority.Checked := True;
  else
    Assert(False);
  end;
  SetTuningGroupboxState(rbTunePriority.Checked);
end;

procedure TfmPriorityBoostConfig.SetTuningGroupboxState(Enabled: Boolean);
begin
  gbBoostTuning.Enabled := Enabled;
  lLowPriority.Enabled := Enabled;
  lNeutral.Enabled := Enabled;
  lHighPriority.Enabled := Enabled;
  trkBoost.Enabled := Enabled;
end;

function TfmPriorityBoostConfig.GetAppRealTime: Boolean;
begin
  Result := cbApplicationRealtime.Checked;
end;

procedure TfmPriorityBoostConfig.SetAppRealTime(const Value: Boolean);
begin
  cbApplicationRealtime.Checked := Value;
end;

function TfmPriorityBoostConfig.GetBoost: Integer;
begin
  Result := trkBoost.Position;
end;

procedure TfmPriorityBoostConfig.SetBoost(const Value: Integer);
begin
  trkBoost.Position := Value;
end;

{ TGxPriorityBoostExpert }

constructor TGxPriorityBoostExpert.Create;
begin
  inherited Create;
  // Default to inactive
  DefaultActive := False;

  HasConfigOptions := True;
  HasMenuItem := False;

  FCompileNotifier := TGxBoostNotifier.Create(Self);
  ToolServices.AddNotifierEx(FCompileNotifier);
end;

destructor TGxPriorityBoostExpert.Destroy;
begin
  ToolServices.RemoveNotifier(FCompileNotifier);

  FCompileNotifier.Free;
  FCompileNotifier := nil;

  inherited Destroy;
end;

function TGxPriorityBoostExpert.GetName: string;
begin
  Result := 'GX_PriorityBoostExpert';
end;

function TGxPriorityBoostExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Priority Booster';
begin
  Result := SDisplayName;
end;

procedure TGxPriorityBoostExpert.Configure;
begin
  with TfmPriorityBoostConfig.Create(nil) do
  try
    Boost := FCompileNotifier.FPriorityBoost;
    BoostSetting := FCompileNotifier.FBoostSetting;
    AppRealTime := FCompileNotifier.AppRealTimeClass;

    if ShowModal = mrOK then
    begin
      FCompileNotifier.PriorityBoost := Boost;
      FCompileNotifier.BoostSetting := BoostSetting;
      FCompileNotifier.AppRealTimeClass := AppRealTime;
      SaveSettings;
    end;

  finally
    Release;
  end;
end;

procedure TGxPriorityBoostExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('PriorityBooster', 'Boost', FCompileNotifier.PriorityBoost);
    RegIni.WriteInteger('PriorityBooster', 'Setting', Ord(FCompileNotifier.BoostSetting));
    RegIni.WriteBool('PriorityBooster', 'AppRealTime', FCompileNotifier.AppRealTimeClass);
  finally
    RegIni.Free;
  end;
end;

procedure TGxPriorityBoostExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FCompileNotifier.PriorityBoost :=
      RegIni.ReadInteger('PriorityBooster', 'Boost', FCompileNotifier.PriorityBoost);
    FCompileNotifier.BoostSetting :=
      TGxBoostSetting(RegIni.ReadInteger('PriorityBooster', 'Setting', Ord(FCompileNotifier.BoostSetting)));
    FCompileNotifier.AppRealTimeClass :=
      RegIni.ReadBool('PriorityBooster', 'AppRealTime', FCompileNotifier.AppRealTimeClass);
  finally
    RegIni.Free;
  end;
end;

procedure TGxPriorityBoostExpert.Click(Sender: TObject);
begin
  // do nothing
end;

function TGxPriorityBoostExpert.IconFileName: string;
begin
  Result := 'PriBoost';
end;

function TGxPriorityBoostExpert.GetMenuCaption: string;
begin
  Result := '';
end;

function TGxPriorityBoostExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TGxPriorityBoostExpert.GetMenuName: string;
begin
  Result := '';
end;

{ TGxBoostNotifier }

constructor TGxBoostNotifier.Create(AOwner: TGX_Expert);
begin
  inherited Create;
  FOwner := AOwner;
  FBoostSetting := bsTunePriority;
  FPriorityBoost := 0;
end;

procedure TGxBoostNotifier.DoBoostPriority;
var
  CurrentThread: THandle;
  CurrentProcess: THandle;
begin
  if not FOwner.Active then
    Exit;

  if FAppRealTimeClass then
  begin
    CurrentProcess := GetCurrentProcess;
    FSavedAppClass := GetPriorityClass(CurrentProcess);
    SetPriorityClass(CurrentProcess, REALTIME_PRIORITY_CLASS);
  end;

  CurrentThread := GetCurrentThread;
  FSavedThreadPriority := GetThreadPriority(CurrentThread);
  SetThreadPriority(CurrentThread, FThreadClassPriority);
end;

procedure TGxBoostNotifier.DoRestorePriority;
var
  CurrentThread: THandle;
  CurrentProcess: THandle;
begin
  if not FOwner.Active then
    Exit;

  CurrentThread := GetCurrentThread;
  Assert(GetThreadPriority(CurrentThread) = FThreadClassPriority);
  SetThreadPriority(CurrentThread, FSavedThreadPriority);

  if FAppRealTimeClass then
  begin
    CurrentProcess := GetCurrentProcess;
    Assert(GetPriorityClass(CurrentProcess) = REALTIME_PRIORITY_CLASS);
    SetPriorityClass(CurrentProcess, FSavedAppClass);
  end;
end;

procedure TGxBoostNotifier.EventNotification(
  NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  case NotifyCode of
    enBeforeCompile:
      DoBoostPriority;

    enAfterCompile:
      DoRestorePriority;
  end;
end;

procedure TGxBoostNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // nothing
end;

const
  BoostToPriorityMap: array[-2.. + 2] of Integer =
    (THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL,
     THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST);

procedure TGxBoostNotifier.SetAppRealTimeClass(const Value: Boolean);
begin
  FAppRealTimeClass := Value;
end;

procedure TGxBoostNotifier.SetBoostSetting(const Value: TGxBoostSetting);
begin
  FBoostSetting := Value;

  case FBoostSetting of
    bsIdlePriority:
      FThreadClassPriority := THREAD_PRIORITY_IDLE;

    bsRealtimePriority:
      FThreadClassPriority := THREAD_PRIORITY_TIME_CRITICAL;

    bsTunePriority:
      if FBoostSetting = bsTunePriority then
        FThreadClassPriority := BoostToPriorityMap[FPriorityBoost];
  else
    Assert(False);
  end;
end;

procedure TGxBoostNotifier.SetPriorityBoost(const Value: Integer);
begin
  Assert((Value >= -2) and (Value <= 2));
  FPriorityBoost := Value;

  if FBoostSetting = bsTunePriority then
    FThreadClassPriority := BoostToPriorityMap[Value];
end;

procedure TfmPriorityBoostConfig.btHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 28);
end;

initialization
  RegisterGX_Expert(TGxPriorityBoostExpert);
end.


unit GX_Experts;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  {$IFDEF GX_HasActionSupport}
  ActnList,
  {$ENDIF GX_HasActionSupport}
  Classes, Windows, SysUtils, Menus, ToolIntf, ExptIntf, EditIntf,
  Dialogs, Graphics, Registry;

type
  // Declare type to use it interchangeably.
  {$IFDEF GX_HasActionSupport}
  TGXAction = TAction;
  {$ELSE}
  TGXAction = TMenuItem;
  {$ENDIF GX_HasActionSupport}

  TGX_Expert = class(TObject)
  private
    FIcon: TIcon;
    FActive: Boolean;
    FShortCut: TShortCut;
    FHasConfigOptions: Boolean;
    FHasMenuItem: Boolean;
    FDefaultActive: Boolean;
    FImageIndex: Integer;
    FExpertIndex: Integer;
    FAction: TGXAction;
  protected
    procedure SetActive(New: Boolean); virtual;
    function IconFileName: string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetMenuCaption: string; virtual; abstract;
    function GetMenuName: string; virtual; abstract;
    function GetMenuMask: string; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetDisplayName: string; virtual; abstract;
    procedure Click(Sender: TObject); virtual; abstract;
    procedure Configure; virtual;
    procedure AfterConfig; virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    property Active: Boolean read FActive write SetActive;
    property Icon: TIcon read FIcon;
    property Action: TGXAction read FAction write FAction;
    property HasConfigOptions: Boolean read FHasConfigOptions write FHasConfigOptions;
    property HasMenuItem: Boolean read FHasMenuItem write FHasMenuItem;
    property DefaultActive: Boolean read FDefaultActive write FDefaultActive;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property ImageIndex: Integer read FImageIndex;
    property ExpertIndex: Integer read FExpertIndex write FExpertIndex;
  end;

  TGX_EnhExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    destructor Destroy; override;
  end;

type
  TGX_ExpertClass = class of TGX_Expert;

var
  GX_ExpertList: TList = nil;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);
function GetGX_ExpertClass(const ClassName: string): TGX_ExpertClass;
function GetGX_ExpertClassByIndex(const Index: Integer): TGX_ExpertClass;

implementation

uses
  {$IFDEF GX_UseNativeToolsApi}
  ToolsAPI,  // If you get an error here, please read SourceCode.txt
  {$ENDIF GX_UseNativeToolsApi}
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_GExperts, GX_GenFunc, GX_ConfigurationInfo, GX_Actions, GX_MessageBox;

type
  TShowMissingIconMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

// Note: Don't call LoadSettings in Create.  This is done for you
// when the expert is created.  See TGExperts.InstallAddIn.
constructor TGX_Expert.Create;
var
  IconFile: string;
begin
  inherited Create;
  FHasConfigOptions := True;
  HasMenuItem := True;
  // Don't set Active to True by default because this causes problems with
  // TGX_EnhExpert.  Instead use DefaultActive and let LoadSettings do it
  DefaultActive := True;
  FShortcut := 0;

  FImageIndex := -1;
  FIcon := TIcon.Create;
  IconFile := IconFileName;
  if IconFile <> '' then
  begin
    IconFile := ConfigInfo.ConfigPath + 'Icons\' + IconFile;
    if ExtractFileExt(IconFile) = '' then
      IconFile := IconFile + '.ico';
    if FileExists(IconFile) then
    begin
      FIcon.LoadFromFile(IconFile);
      {$IFDEF GX_UseNativeToolsApi}
      // Register the Image.
      if not IsStandAlone then
        FImageIndex := (BorlandIDEServices as INTAServices).ImageList.AddIcon(FIcon);
      {$ENDIF GX_UseNativeToolsApi}
    end
    else
      ShowGxMessageBox(TShowMissingIconMessage);
  end
  {$IFOPT D+}
  else
    SendDebugEx('Icon filename missing for expert ' + Self.ClassName, mtError);
  {$ENDIF D+}
end;

destructor TGX_Expert.Destroy;
begin
  FIcon.Free;
  FIcon := nil;
  inherited Destroy;
end;

procedure TGX_Expert.Configure;
resourcestring
  SNoConfigOptions = 'No configuration for this expert are available.';
begin
  MessageDlg(SNoConfigOptions, mtInformation, [mbOK], 0);
end;

procedure TGX_Expert.AfterConfig;
begin
  // nothing, by default
end;

procedure TGX_Expert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    ShortCut := RegIni.ReadInteger('ShortCut', GetName, ShortCut);
    Active := RegIni.ReadBool('Active', GetName, DefaultActive);
  finally
    RegIni.Free;
  end;
end;

procedure TGX_Expert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('Active', GetName, Active);
    RegIni.WriteInteger('ShortCut', GetName, ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TGX_Expert.SetActive(New: Boolean);
begin
  FActive := New;
end;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);
var
  ClassName: string;
begin
  ClassName := AClass.ClassName;
  {$IFOPT D+} SendDebug('Registering expert: ' + ClassName); {$ENDIF D+}
  if GetGX_ExpertClass(ClassName) <> nil then
    Exit;
  GX_ExpertList.Add(AClass);
end;

function GetGX_ExpertClass(const ClassName: string): TGX_ExpertClass;
var
  I: Integer;
begin
  Assert(GX_ExpertList <> nil, 'Uses clauses are out of order.  GX_ExpertList is nil!');

  for I := 0 to GX_ExpertList.Count - 1 do
  begin
    Result := GX_ExpertList[I];
    if Result.ClassNameIs(ClassName) then Exit;
  end;
  Result := nil;
end;

function GetGX_ExpertClassByIndex(const Index: Integer): TGX_ExpertClass;
begin
  Result := nil;
  if (Index >= 0) and (Index <= GX_ExpertList.Count - 1) then
    Result := GX_ExpertList[Index];
end;

function TGX_Expert.IconFileName: string;
begin
  Result := '';
end;

destructor TGX_EnhExpert.Destroy;
begin
  // Set active to False, this makes it possible to handle all creation and
  // destruction inside SetActive
  Active := False;
  inherited Destroy;
end;

procedure TGX_EnhExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if HasMenuItem and not IsStandAlone then
    begin
      if New then
        GXActionManager.AddExpertAction(Self)
      else
        GXActionManager.RemoveExpertAction(Self);
    end;
  end;
end;

{ TShowMissingIconMessage }

function TShowMissingIconMessage.GetMessage: string;
resourcestring
  SBadIconFile =
    'Some of the default GExperts icons are missing.  Make sure that the icons ' +
    'exist in a directory called "Icons" underneath the main GExperts ' +
    'storage directory, which is defined in the GExperts configuration ' +
    'dialog.';
begin
  Result := SBadIconFile;
end;

function TShowMissingIconMessage.ShouldShow: Boolean;
const
  ShownOnce: Boolean = False;
begin
  Result := not ShownOnce;
  ShownOnce := True;
end;

initialization
  GX_ExpertList := TList.Create;

finalization
  GX_ExpertList.Free;
  GX_ExpertList := nil;

end.




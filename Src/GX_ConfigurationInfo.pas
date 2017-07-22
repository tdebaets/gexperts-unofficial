unit GX_ConfigurationInfo;

{$I GX_CondDefine.Inc}

interface

uses
  Classes;

type
  TConfigInfo = class(TObject)
  private
    FVclPath: string;
    FConfigPath: string;
    FHelpFile: string;
    FRegKey: string;
    FEditorExpertsEnabled: Boolean;
    //FAddMenuImages: Boolean;
    FAlphabetizeMenu: Boolean;
  private
    function GetHelpFile: string;
    procedure SetVclPath(const Value: string);
    procedure LoadSettings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings;

    property RegKey: string read FRegKey;
    property HelpFile: string read GetHelpFile write FHelpFile;
    property VCLPath: string read FVclPath write SetVclPath;
    property ConfigPath: string read FConfigPath write FConfigPath;
    property EditorExpertsEnabled: Boolean read FEditorExpertsEnabled write FEditorExpertsEnabled;
    //property AddMenuImages: Boolean read FAddMenuImages write FAddMenuImages;
    property AlphabetizeMenu: Boolean read FAlphabetizeMenu write FAlphabetizeMenu;
  public
    class function NewInstance: TObject; override; // Fix for BCB 5 somehow
    procedure FreeInstance; override;
  end;

function ConfigInfo: TConfigInfo;
procedure FreeConfigInfo;

implementation

uses
  Windows, SysUtils, Registry, FileCtrl,
  ExptIntf,
  GX_VerDepConst,
  GX_IDEEnhance,
  {$IFDEF GX_EII} GX_EditorEnhancements, {$ENDIF GX_EII}
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc, GX_MessageBox;

type
  TShowBadDirectoryMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  FPrivateConfigurationInfo: TConfigInfo = nil;
  FCanFreeConfigurationInfo: Boolean = False;

constructor TConfigInfo.Create;
begin
  {$IFOPT D+}SendDebug('Creating configuration info');{$ENDIF D+}
  FPrivateConfigurationInfo := Self;

  inherited Create;

  FRegKey := GxGetIdeBaseRegistryKey;
  FVclPath := GxGetIdeInstallationDirectory + '\Source\VCL\'; // do not localize
  FConfigPath := ExtractFilePath(DLLName);

  {$IFDEF GX_VER120_up}
  //FAddMenuImages := True;
  {$ENDIF GX_VER120_up}

  {$IFDEF GX_EII}
  EditorEnhancements.Enabled := False;
  //EditorExpertsEnabled := True;
  {$ENDIF GX_EII}

  IdeEnhancements.Enabled := False;

  LoadSettings;
  ShowGxMessageBox(TShowBadDirectoryMessage);
end;

destructor TConfigInfo.Destroy;
begin
  {$IFDEF GX_EII}
  FreeEditorEnhancements;
  {$ENDIF}
  FreeIdeEnhancements;

  inherited Destroy;
end;

function TConfigInfo.GetHelpFile: string;
begin
  Result := FHelpFile;
end;

procedure TConfigInfo.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  {$IFOPT D+}SendDebug('Loading configuration info settings');{$ENDIF D+}
  // do not localize any of the following strings
  RegIni := TRegIniFile.Create(RegKey + '\GExperts');
  try
    VCLPath := AddSlash(RegIni.ReadString('Misc', 'VCLPath', FVclPath));
    FConfigPath := AddSlash(RegIni.ReadString('Misc', 'ConfigPath', FConfigPath));
    FHelpFile := RegIni.ReadString('Misc', 'HelpFile', FConfigPath + 'GExperts.hlp');
    EditorExpertsEnabled := RegIni.ReadBool('Misc', 'EditorExpertsEnabled', True);
    IdeEnhancements.Enabled := RegIni.ReadBool('Misc', 'IdeEnhancementsEnabled', False);
    //FAddMenuImages := RegIni.ReadBool('Misc', 'AddMenuImages', FAddMenuImages);
    FAlphabetizeMenu := RegIni.ReadBool('Misc', 'AlphabetizeMenu', False);
    {$IFDEF GX_EII}
    EditorEnhancements.Enabled := RegIni.ReadBool('Misc', 'EditorEnhancementsEnabled', False);
    {$ENDIF GX_EII}
  finally
    RegIni.Free;
  end;
end;

procedure TConfigInfo.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the following strings
  RegIni := TRegIniFile.Create(RegKey + '\GExperts');
  try
    RegIni.WriteString('Misc', 'VCLPath', FVclPath);
    RegIni.WriteString('Misc', 'ConfigPath', FConfigPath);
    RegIni.WriteString('Misc', 'HelpFile', FHelpFile);
    RegIni.WriteBool('Misc', 'EditorExpertsEnabled', EditorExpertsEnabled);
    RegIni.WriteBool('Misc', 'IdeEnhancementsEnabled', IdeEnhancements.Enabled);
    //RegIni.WriteBool('Misc', 'AddMenuImages', FAddMenuImages);
    RegIni.WriteBool('Misc', 'AlphabetizeMenu', FAlphabetizeMenu);
    {$IFDEF GX_EII}
    RegIni.WriteBool('Misc', 'EditorEnhancementsEnabled', EditorEnhancements.Enabled);
    {$ENDIF GX_EII}
  finally
    RegIni.Free;
  end;
end;

procedure TConfigInfo.SetVclPath(const Value: string);
begin
  FVclPath := AddSlash(Value);
end;

procedure TConfigInfo.FreeInstance;
begin
  Assert(FCanFreeConfigurationInfo);
  inherited FreeInstance;
end;

class function TConfigInfo.NewInstance: TObject;
begin
  Assert((FPrivateConfigurationInfo = nil) and not FCanFreeConfigurationInfo);
  Result := inherited NewInstance;
end;

function ConfigInfo: TConfigInfo;
begin
  Result := FPrivateConfigurationInfo;
end;

procedure FreeConfigurationInfo;
begin
  FCanFreeConfigurationInfo := True;

  FPrivateConfigurationInfo.Free;
  FPrivateConfigurationInfo := nil;
end;

procedure FreeConfigInfo;
begin
  FreeConfigurationInfo;
end;

{ TShowBadDirectoryMessage }

function TShowBadDirectoryMessage.GetMessage: string;
resourcestring
  SBadConfigPath =
    'The storage directory defined in the GExperts configuration dialog ' +
    'does not exist.  Please correct the problem.';
begin
  Result := SBadConfigPath;
end;

function TShowBadDirectoryMessage.ShouldShow: Boolean;
var
  DirAttributes: Integer;
begin
  // The directory must exist and be writeable to be valid
  Result := True;
  if DirectoryExists(ConfigInfo.ConfigPath) then
  begin
    DirAttributes := FileGetAttr(ConfigInfo.ConfigPath);
    DirAttributes := DirAttributes and faReadOnly;
    if DirAttributes = 0 then
      Result := False;
  end;
end;

initialization

  FPrivateConfigurationInfo := TConfigInfo.Create;

finalization

  FreeConfigurationInfo;

end.

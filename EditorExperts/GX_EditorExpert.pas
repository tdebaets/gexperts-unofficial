unit GX_EditorExpert;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Classes, Windows, Sysutils, Menus, Dialogs,
  ToolIntf, ExptIntf, EditIntf;

type
  TEditorExpert = class(TObject)
  private
    FShortCut: TShortCut;
    function GetBaseRegistryKey: string;
  protected
    FButtonNo: Integer;
    FHasConfigOptions: Boolean;
    FName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetInterfaces(var ModIntf: TIModuleInterface; var EditIntf: TIEditorInterface);
    procedure Configure; virtual;
    procedure Execute; virtual; abstract;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    procedure GetHelpString(List: TStrings); virtual;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property BaseRegistryKey: string read GetBaseRegistryKey;
    property ButtonNo: Integer read FButtonNo;
    property Name: string read FName;
    property HasConfigOptions: Boolean read FHasConfigOptions;
  end;

  TEditorExpertClass = class of TEditorExpert;

function EditorExpertClassList: TList;

procedure RegisterEditorExpert(AClass: TEditorExpertClass);
function GetExpertClass(const ClassName: string): TEditorExpertClass;
function GetExpertClassByIndex(const Index: Integer): TEditorExpertClass;

implementation

uses
  GX_GExperts,
  GX_ConfigurationInfo,
  Consts;

constructor TEditorExpert.Create;
begin
  inherited Create;
  FButtonNo := 0;
  FHasConfigOptions := True;
end;

destructor TEditorExpert.Destroy;
begin
  inherited Destroy;
end;

procedure TEditorExpert.LoadSettings;
begin
  // Nothing, by default
end;

procedure TEditorExpert.SaveSettings;
begin
  // Nothing, by default
end;

procedure TEditorExpert.GetHelpString(List: TStrings);
resourcestring
  SNoHelpAvailable = 'No help available.';
begin
  List.Text := SNoHelpAvailable;
end;

procedure TEditorExpert.Configure;
resourcestring
  SNoConfigurationOptions = 'There are no configuration options for this expert.';
begin
  MessageDlg(SNoConfigurationOptions, mtInformation, [mbOK], 0);
end;

procedure TEditorExpert.GetInterfaces(var ModIntf: TIModuleInterface; var EditIntf: TIEditorInterface);
begin
  { Get module interface }
  ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile);
  if ModIntf = nil then
    raise Exception.Create('EditWrite:No Module Interface');

  { Get Editor Interface }
  EditIntf := ModIntf.GetEditorInterface;
  if EditIntf = nil then
    raise Exception.Create('EditWrite:No Editor Interface');
end;

procedure RegisterEditorExpert(AClass: TEditorExpertClass);
var
  ClassName: string;
begin
  ClassName := AClass.ClassName;
  if GetExpertClass(ClassName) <> nil then
  begin
    {raise EFilerError.CreateFmt(SDuplicateClass, [ClassName]);}
    Exit;
  end;
  EditorExpertClassList.Add(AClass);
end;

function GetExpertClass(const ClassName: string): TEditorExpertClass;
var
  I: Integer;
begin
  for I := 0 to EditorExpertClassList.Count - 1 do
  begin
    Result := EditorExpertClassList[I];
    if Result.ClassNameIs(ClassName) then
      Exit;
  end;
  Result := nil;
end;

function GetExpertClassByIndex(const Index: Integer): TEditorExpertClass;
begin
  Result := nil;
  if (Index >= 0) and (Index <= EditorExpertClassList.Count - 1) then
    Result := EditorExpertClassList[Index];
end;

function TEditorExpert.GetBaseRegistryKey: string;
begin
  Result := ConfigInfo.RegKey + '\GExperts\EditorExperts'; // do not localize
end;

var
  PrivateEditorExpertClassList: TList;

function EditorExpertClassList: TList;
begin
  Result := PrivateEditorExpertClassList;
end;

initialization

  PrivateEditorExpertClassList := TList.Create;

finalization

  PrivateEditorExpertClassList.Free;
  PrivateEditorExpertClassList := nil;

end.


unit GX_ProofReaderExpert;

{$I GX_CondDefine.inc}

{$IFNDEF GX_NOBDE}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Classes, ToolIntf, GX_Experts,
  GX_ProofReaderDM;

type
  TAutoProofReader = class(TGX_Expert)
  private
    FProofReaderDataModule: TdmProofReader;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuMask: string; override;
    function GetMenuName: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
  end;

implementation

uses
  SysUtils, GX_GExperts, Menus, Dialogs,
  GX_ProofReaderConfig, GX_GenFunc;

{ TAutoProofReader }

constructor TAutoProofReader.Create;
begin
  inherited Create;

  // do not localize
  //ShortCut := Menus.ShortCut(Word('H'), [ssCtrl]);
  HasConfigOptions := True;
  HasMenuItem := True;
end;

destructor TAutoProofReader.Destroy;
begin
  FProofReaderDataModule.Free;
  FProofReaderDataModule := nil;

  inherited Destroy;
end;

procedure TAutoProofReader.Configure;
var
  CreatedDataModuleToConfigure: Boolean;
begin
  CreatedDataModuleToConfigure := False;
  try
    if FProofReaderDataModule = nil then
    begin
      FProofReaderDataModule := TdmProofReader.Create(nil);
      CreatedDataModuleToConfigure := True;
    end;

  with TfmProofReaderConfig.Create(nil) do
  try
    ProofReaderDM := FProofReaderDataModule;
    pcAPR.ActivePage := tsHistory;
    ActiveControl := grdHistory;
    ShowModal;
  finally
    Free;
  end;
  SaveSettings;

  finally
    if CreatedDataModuleToConfigure then
    begin
      FProofReaderDataModule.Free;
      FProofReaderDataModule := nil;
    end;
  end;
end;

procedure TAutoProofReader.Click(Sender: TObject);
begin
  Configure;
end;

function TAutoProofReader.GetDisplayName: string;
resourcestring
  SProofDisplayName = 'Code Proofreader';
begin
  Result := SProofDisplayName;
end;

function TAutoProofReader.GetMenuCaption: string;
resourcestring
  SProofMenuCaption = 'Code &Proofreader...';
begin
  Result := SProofMenuCaption;
end;

function TAutoProofReader.GetMenuMask: string;
begin
  Result := '';
end;

function TAutoProofReader.GetMenuName: string;
begin
  Result := 'GX_APR';
end;

function TAutoProofReader.GetName: string;
begin
  Result := 'Code_Proofreader';
end;

procedure TAutoProofReader.LoadSettings;
begin
  inherited LoadSettings;
end;

procedure TAutoProofReader.SaveSettings;
begin
  inherited SaveSettings;
  if Assigned(FProofReaderDataModule) then
    FProofReaderDataModule.SaveSettings;
end;

procedure TAutoProofReader.SetActive(New: Boolean);
begin
  inherited SetActive(New);

  if Active then
  begin
    if FProofReaderDataModule = nil then
      FProofReaderDataModule := TdmProofReader.Create(nil);

    if not FProofReaderDataModule.IsInstalled then
      FProofReaderDataModule.Initialize;
  end
  else
  begin
    FProofReaderDataModule.Free;
    FProofReaderDataModule := nil;
  end;
end;

function TAutoProofReader.IconFileName: string;
begin
  Result := 'AutoCorr';
end;

initialization
  RegisterGX_Expert(TAutoProofReader);

{$ELSE GX_NOBDE}
interface implementation
{$ENDIF GX_NOBDE}

end.


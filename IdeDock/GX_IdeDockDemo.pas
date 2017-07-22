unit GX_IdeDockDemo;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts,
  ToolIntf,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, IniFiles, GX_IdeDock;

type
  TDemoDockForm = class(TfmIdeDockForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean); override;
    procedure LoadWindowState(MemIniFile: TMemIniFile); override;
  end;

// The following DEFINE indicates whatever the expert can be
// loaded/unload at runtime (requires adjustments in GX_Expert).
{.$DEFINE LoadUnload}

type
  TDemoExpert = class(TGX_Expert)
  protected
    {$IFDEF LoadUnload}
    procedure SetActive(Value: Boolean); override;
    {$ENDIF LoadUnload}
  public
    constructor Create; override;
    destructor Destroy; override;
    {$IFNDEF LoadUnload}
    procedure LoadSettings; override;
    {$ENDIF LoadUnload}
    {$IFDEF GX_UseNativeToolsApi}
    procedure Click(Sender: TObject); override;
    {$ELSE}
    procedure Click(Sender: TIMenuItemIntf); override;
    {$ENDIF GX_UseNativeToolsApi}
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
  end;

var
  // This variable is required for docking support
  DemoDockForm: TDemoDockForm;
  // Use this variable to get information from the expert
  DemoExpert: TDemoExpert;

implementation

{$R *.DFM}

uses
  GX_DbugIntf;

procedure TDemoDockForm.FormCreate(Sender: TObject);
var
  AClass: TClass;
begin
  inherited;  // <-- make sure that inherited is called!
  // Do something useful...
  AClass := Self.ClassParent;
  while AClass <> nil do
  begin
    SendDebug(AClass.ClassName + IntToStr(AClass.InstanceSize));
    AClass := AClass.ClassParent;
  end;
end;

procedure TDemoDockForm.FormDestroy(Sender: TObject);
begin
  inherited;  // <-- make sure that inherited is called!
  // Do something useful...
  DemoDockForm := nil;
end;

procedure TDemoDockForm.SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean);
begin
  SendDebug('SaveWindowState');
  inherited SaveWindowState(MemIniFile, ABoolean);
  SendDebug('After SaveWindowState');
  SendDebug(Panel1.Name);

  MemIniFile.WriteInteger('DemoDockForm' {form name}, 'SplitPos',  Panel1.Width);
end;

procedure TDemoDockForm.LoadWindowState(MemIniFile: TMemIniFile);
begin
  SendDebug('LoadWindowState');
  inherited LoadWindowState(MemIniFile); // save position and size (and docking stuff)

  Panel1.Width := MemIniFile.ReadInteger('DemoDockForm' {form name}, 'SplitPos', Panel1.Width);
end;

{ TDemoExpert }

constructor TDemoExpert.Create;
begin
  inherited Create;

  DemoExpert := Self;
end;

destructor TDemoExpert.Destroy;
begin
{$IFDEF LoadUnload}
  // Unregister the form.
  Active := False;
{$ELSE}
  // It is not required to unregister the form,
  // and since we don't know if the form is
  // registered we don't unregister it.
  DemoDockForm.Free;
  DemoDockForm := nil;
{$ENDIF LoadUnLoad}

  DemoExpert := nil;

  inherited Destroy;
end;

{$IFDEF LoadUnload}
procedure TDemoExpert.SetActive(Value: Boolean);
begin
  if Active <> Value then
  begin
    inherited SetActive(Value);
    if Value then
    begin
      // If active is set to True register the form.
      IdeDockManager.RegisterDockableForm(TDemoDockForm, DemoDockForm, 'DemoDockForm')
      //                                   FormClass      Form var       form Name (must be unique).
    end
    else
    begin
      // If active is set to False, then Free and unregister the form.
      DemoDockForm.Free;
      DemoDockForm := nil;

      IdeDockManager.UnRegisterDockableForm('DemoDockForm');
    end;
  end;
end;
{$ENDIF LoadUnload}

{$IFNDEF LoadUnload}
procedure TDemoExpert.LoadSettings;
begin
  inherited LoadSettings;
  // This procedure is only called once so it
  // is safe to register the form here.
  if Active then
  begin
    IdeDockManager.RegisterDockableForm(TDemoDockForm, DemoDockForm, 'DemoDockForm');
    //                                  FormClass      Form var       form Name (must be unique).
  end;
end;
{$ENDIF LoadUnload}

{$IFDEF GX_UseNativeToolsApi}
procedure TDemoExpert.Click(Sender: TObject);
{$ELSE}
procedure TDemoExpert.Click(Sender: TIMenuItemIntf);
{$ENDIF GX_UseNativeToolsApi}
begin
  // If the form doesn't exist, create it.
  if DemoDockForm = nil then
    DemoDockForm := TDemoDockForm.Create(nil);
  // Show the form using IdeDockManager.ShowForm.
  IdeDockManager.ShowForm(DemoDockForm);
end;

function TDemoExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Docking demo expert';
begin
  Result := SMenuCaption;
end;

function TDemoExpert.GetMenuName: string;
begin
  Result := 'GX_IdeDockDemo';
end;

function TDemoExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TDemoExpert.GetName: string;
begin
  Result := 'IdeDockDemo';
end;

function TDemoExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'IDE Dock demo';
begin
  Result := SDisplayName;
end;

initialization
  RegisterGX_Expert(TDemoExpert);

end.

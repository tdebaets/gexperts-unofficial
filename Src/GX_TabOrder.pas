unit GX_TabOrder;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EditIntf, ToolIntf, ExptIntf, StdCtrls, GX_Experts;

type
  TCompObject = class(TObject)
  public
    ComponentName: string;
    ComponentClass: string;
  end;

type
  TfmTabOrder = class(TForm)
    gbxComponents: TGroupBox;
    btnOK: TButton;
    btnClose: TButton;
    lbxComps: TListBox;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FormIntf: TIFormInterface;
    ModIntf: TIModuleInterface;
  end;

  TTabExpert = class(TGX_EnhExpert)
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
  ClipBrd,
  {$IFDEF DLL} GX_ConfigurationInfo, GX_GExperts, {$ENDIF DLL}
  GX_GenFunc;

procedure TfmTabOrder.FormCreate(Sender: TObject);
var
  CompIntf: TIComponentInterface;

  function GetParentName: string;
  var
    Comp, Parent: TWinControl;
  begin
    Result := 'NO PARENT';
    if CompIntf = nil then
      Exit;
    Comp := TWinControl(CompIntf.GetComponentHandle);
    if Comp <> nil then
      if Comp.Parent <> nil then
      begin
        Parent := TWinControl(Comp.Parent);
        Result := Parent.Name;
      end;
  end;

var
  i: Integer;
  UnitName: string;
  CurrentFile: string;
  FileExt: string;
  TaborderData: Integer;
  Tabstop: Boolean;
  ComponentName: string;
  ParentName: string;
  TempCompObj: TCompObject;
resourcestring
  SDfmOnly = 'This expert is for use in .DFM files only.';
  SCouldNotGetModuleIntf = ' - Could not get module interface';
  SCouldNotGetFormIntf = ' - Could not get form interface';
  SShiftSelectComponents = 'Please Shift+Select the components in tab order first.';
  SNoDataModules = 'You cannot set the tab order of components on a data module.';
  SSameParentRequired = 'All selected components must have the same parent';
begin
  try
    CurrentFile := UpperCase(ToolServices.GetCurrentFile);
    FileExt := ExtractFileExt(CurrentFile);
    if FileExt <> '.DFM' then
    begin
      MessageDlg(SDfmOnly, mtError, [mbOK], 0);
      Exit;
    end;

    // Get module interface
    i := Pos(FileExt, CurrentFile);
    if i > 0 then
    begin
      UnitName := Copy(CurrentFile, 1, i) + 'PAS';

      ModIntf := ToolServices.GetModuleInterface(UnitName);
      if ModIntf = nil then
      begin
        UnitName := Copy(CurrentFile, 1, i) + 'CPP';

        ModIntf := ToolServices.GetModuleInterface(UnitName);
        if ModIntf = nil then
        begin
          MessageDlg(UnitName + SCouldNotGetModuleIntf, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end
    else
    begin
      UnitName := CurrentFile;
      if ModIntf = nil then
      begin
        MessageDlg(UnitName + SCouldNotGetModuleIntf, mtError, [mbOK], 0);
        Exit;
      end;
    end;

    FormIntf := ModIntf.GetFormInterface;
    if FormIntf = nil then
    begin
      MessageDlg(SCouldNotGetFormIntf, mtError, [mbOK], 0);
      Exit;
    end;
    if (FormIntf.GetSelCount = 1) and
        (TComponent(FormIntf.GetSelComponent(0).GetComponentHandle) is TDataModule) then
    begin
      MessageDlg(SNoDataModules, mtError, [mbOK], 0);
      Exit;
    end;
    if (FormIntf.GetSelCount = 1) and
       (TComponent(FormIntf.GetSelComponent(0).GetComponentHandle) is TCustomForm) then
    begin
      MessageDlg(SShiftSelectComponents, mtError, [mbOK], 0);
      Exit;
    end;

    for i := 0 to FormIntf.GetSelCount - 1 do
    begin
      CompIntf := FormIntf.GetSelComponent(i);
      try
        if CompIntF <> nil then
        begin
          if not (TControl(CompIntf.GetComponentHandle) is TWinControl) then
            raise Exception.Create('All selected controls must descend from TWinControl');
          if i = 0 then
            ParentName := GetParentName;
          if ParentName <> GetParentName then
          begin
            lbxComps.Clear;
            MessageDlg(SSameParentRequired, mtError, [mbOK], 0);
            Break;
          end;
          SetLength(ComponentName, 255);
          if CompIntf.GetPropValuebyName('TABSTOP', Tabstop) then
            if CompIntf.GetPropValueByName('TABORDER', TaborderData) then
              if CompIntf.GetPropValuebyName('NAME', ComponentName) then
              begin
                TempCompObj := TCompObject.Create;
                try
                  TempCompObj.ComponentName := ComponentName;
                  TempCompObj.ComponentClass := CompIntf.GetComponentType;
                  lbxComps.Items.AddObject(ComponentName + ': ' + CompIntf.GetComponentType, TempCompObj);
                except
                  on E: Exception do
                  begin
                    ShowExceptionErrorMessage(E);
                    TempCompObj.Free;
                    Abort;
                  end;
                end;
              end;
        end;
      finally
        CompIntf.Free;
      end;
    end;
    lbxComps.ItemIndex := -1;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmTabOrder.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  try
    for i := 0 to lbxComps.Items.Count-1 do
      lbxComps.Items.Objects[i].Free;

    FormIntf.Free;
    FormIntf := nil;
    ModIntf.Free;
    ModIntf := nil;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmTabOrder.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmTabOrder.btnOKClick(Sender: TObject);
var
  i: Integer;
  TaborderData: Integer;
  Tabstop: Boolean;
  CompIntf: TIComponentInterface;
resourcestring
  SCouldNotSetTabstops = 'Could not set tab stops:'#13#10;
begin
  if FormIntf = nil then Exit;
  try
    for i := 0 to lbxComps.Items.Count - 1 do
    begin
      CompIntf := FormIntf.FindComponent(TCompObject(lbxComps.Items.Objects[i]).ComponentName);
      try
        if CompIntf <> nil then
          if CompIntf.GetPropValuebyName('TABSTOP', Tabstop) then
            if CompIntf.GetPropValuebyName('TABORDER', TaborderData) then
              CompIntf.SetPropbyName('TABORDER', i);
      finally
        CompIntf.Free;
      end;
    end;
  except
    on E: Exception do
      ProcessExceptionMsg(E, SCouldNotSetTabstops);
  end;

  Self.Close;
end;

procedure TfmTabOrder.btnHelpClick(Sender: TObject);
begin
{$IFDEF DLL}
  WinHelp(Self.handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 11);
{$ENDIF DLL}
end;

// copies all component names to the clipboard
procedure TfmTabOrder.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ClipboardString: string;
  i: Integer;
begin
  try
    if (ssCtrl in Shift) and ((Key = 67) or (Key = 99)) then
    begin
      ClipboardString := '';
      for i := 0 to lbxComps.Items.Count - 1 do
        ClipboardString := ClipboardString + TCompObject(lbxComps.Items.Objects[i]).ComponentName + #13#10;
      Clipboard.AsText := ClipboardString;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

//************************ TTabExpert ********************

constructor TTabExpert.Create;
begin
  inherited Create;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TTabExpert.Destroy;
begin
  inherited Destroy;
end;

function TTabExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Set Tab &Order...';
begin
  Result := SMenuCaption;
end;

function TTabExpert.GetMenuName: string;
begin
  Result := 'GX_TabOrder';
end;

function TTabExpert.GetMenuMask: string;
begin
  Result := '.DFM';
end;

function TTabExpert.GetName: string;
begin
  Result := 'Tab_Order';
end;

function TTabExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Set Tab Order';
begin
  Result := SDisplayName;
end;

procedure TTabExpert.Click(Sender: TObject);
begin
  try
    with TfmTabOrder.Create(nil) do
    try
      if lbxComps.Items.Count <> 0 then
        ShowModal;
    finally
      Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TTabExpert.IconFileName: string;
begin
  Result := 'Tab';
end;

procedure TTabExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Set Tab Order is a modal expert
    end;
  end;
end;

initialization
  RegisterGX_Expert(TTabExpert);
end.


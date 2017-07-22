unit GX_ComponentGrid;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, Grids, ExtCtrls, EditIntf, ToolIntf, ExptIntf, SortGrid, GX_Experts,
  StdCtrls, SpinIntEdit;

type
  TfmComponentGrid = class(TForm)
    pnlToolbar: TPanel;
    StringGrid: TSortGrid;
    sbSave: TSpeedButton;
    sbHelp: TSpeedButton;
    sbNumbering: TSpeedButton;
    sbPrint: TSpeedButton;
    pnlNumbering: TPanel;
    lblStart: TLabel;
    lblSkipBy: TLabel;
    sbRenumber: TSpeedButton;
    edStart: TSpinIntEdit;
    edSkipBy: TSpinIntEdit;
    procedure FormDestroy(Sender: TObject);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure sbSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sbHelpClick(Sender: TObject);
    procedure sbNumberingClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbRenumberClick(Sender: TObject);
  private
    FormIntf: TIFormInterface;
    ModIntf: TIModuleInterface;
    FModified: Boolean;
    FormCaption: string;
    procedure LoadComponents;
    function SetLength(AString: string; ReqLen: Integer): string;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGridExpert = class(TGX_EnhExpert)
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

resourcestring
  SNotAvailable = 'N/A';

implementation

{$R *.DFM}

uses
  GX_GenFunc, GX_GExperts, GX_ConfigurationInfo, Printers;

function  TfmComponentGrid.SetLength(AString: string; ReqLen: Integer): string;
var
  I: Integer;
  Pad: string;
begin
  Pad := '';
  if (Length(AString) > ReqLen) then
  begin
    Result := Copy(AString,1,ReqLen);
    Exit;
  end;
  for I := 0 to ReqLen - Length(AString) do
    Pad := Pad + ' ';
  Result := AString + Pad;
end;

constructor TfmComponentGrid.Create(AOwner: TComponent);
resourcestring
  SCellComponent = 'Component';
begin
  inherited Create(AOwner);
  FormIntf := nil;
  ModIntf := nil;
  FModified := False;
  with StringGrid do
  begin
    Cells[0, 0] := SCellComponent;
    Cells[1, 0] := 'Parent';       // do not localize
    Cells[2, 0] := 'Tag';          // do not localize
    Cells[3, 0] := 'HelpContext';  // do not localize
  end;
  LoadComponents;
end;

procedure TfmComponentGrid.LoadComponents;
resourcestring
  SNoParent = 'No Parent';
var
  CompIntf, FormCompIntf: TIComponentInterface;

  function GetParentName: string;
  var
    Comp: TControl;
    Parent: TControl;
  begin
    Result := SNoParent;
    if CompIntf = nil then
      Exit;
    if TComponent(CompIntf.GetComponentHandle) is TControl then
    begin
      Comp := TControl(CompIntf.GetComponentHandle);
      if Comp <> nil then
      begin
        if Comp.Parent <> nil then
        begin
          Parent := TControl(Comp.Parent);
          Result := Parent.Name;
        end;
      end;
    end;
  end;

resourcestring
  SCouldNotGetModuleIntf = 'Could not get module interface for %s.';
  SCouldNotGetFormIntf = 'Could not get form interface.  Make sure you are viewing the DFM as a form and not as text.';
  SDfmFilesOnly = 'This Expert is for use in .DFM files only.';

var
  UnitName: string;
  CurrentFileName: string;
  i, p: Integer;
  ComponentTag: string;
  ComponentHelpContext: string;
  ComponentName: string;
  ParentName: string;
  FileExt: string;
begin
  CurrentFileName := UpperCase(ToolServices.GetCurrentFile);
  if not IsDfm(CurrentFileName) then
    raise Exception.Create(SDfmFilesOnly);

  // Get module interface
  FileExt := ExtractFileExt(CurrentFileName);
  i := Pos(FileExt, CurrentFileName);
  if i > 0 then
  begin
    UnitName := Copy(CurrentFileName, 1, i) + 'PAS';
    ModIntf := ToolServices.GetModuleInterface(UnitName);
    if ModIntf = nil then
    begin
      UnitName := Copy(CurrentFileName, 1, i) + 'CPP'; //! StH: Or is that HPP?
      ModIntf := ToolServices.GetModuleInterface(UnitName);
    end;
  end
  else
  begin
    UnitName := CurrentFileName;
    ModIntf := ToolServices.GetModuleInterface(UnitName);
  end;

  if ModIntf = nil then
    raise Exception.CreateFmt(SCouldNotGetModuleIntf, [UnitName]);

  FormIntf := ModIntf.GetFormInterface;
  if FormIntf = nil then
    raise Exception.Create(SCouldNotGetFormIntf);

  FormCompIntf := FormIntf.GetFormComponent;
  try
    if FormCompIntf <> nil then
      for i := 0 to FormCompIntf.GetComponentCount - 1 do
      begin
        CompIntf := FormCompIntf.GetComponent(i);
        try
          if CompIntF <> nil then
          begin
            ParentName := GetParentName;
            SetLength(ComponentName, 255);
            p := GetPropIndex(CompIntf, 'TAG');  // do not localize
            if p < 0 then
              ComponentTag := SNotAvailable
            else
              ComponentTag := GetPropAsString(CompIntf, p);
            p := GetPropIndex(CompIntf, 'HELPCONTEXT');  // do not localize
            if p < 0 then
              ComponentHelpContext := SNotAvailable
            else
              ComponentHelpContext := GetPropAsString(CompIntf, p);
            if Length(Trim(ComponentHelpContext)) = 0 then
              ComponentHelpContext := '0';
            p := GetPropIndex(CompIntf, 'NAME');  // do not localize
            if p < 0 then
              ComponentName := SNotAvailable
            else
              ComponentName := GetPropAsString(CompIntf, p);
            with StringGrid do
            begin
              Cells[0, RowCount - 1] := ComponentName;
              Cells[1, RowCount - 1] := ParentName;
              Cells[2, RowCount - 1] := ComponentTag;
              Cells[3, RowCount - 1] := ComponentHelpContext;
              Objects[0, RowCount - 1] := Pointer(i);
              RowCount := RowCount + 1;
            end;
          end;
        finally
          CompIntf.Free;
        end;
      end;
  finally
    if FormCompIntf.GetComponentHandle <> nil then
    begin
      if (TObject(FormCompIntf.GetComponentHandle) is TCustomForm) then
        FormCaption := TCustomForm(FormCompIntf.GetComponentHandle).Caption
      else
        FormCaption := TPersistent(FormCompIntf.GetComponentHandle).ClassName;
    end;
    FormCompIntf.Free;
  end;
  if StringGrid.RowCount > 2 then
    StringGrid.RowCount := StringGrid.RowCount - 1;
end;

procedure TfmComponentGrid.FormDestroy(Sender: TObject);
begin
  FormIntf.Free;
  FormIntf := nil;

  ModIntf.Free;
  ModIntf := nil;
end;

procedure TfmComponentGrid.StringGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  FModified := True;
end;

procedure TfmComponentGrid.sbSaveClick(Sender: TObject);
var
  n, p, i: Integer;
  CompIntf: TIComponentInterface;
  FormCompIntf: TIComponentInterface;
begin
  FormCompIntf := FormIntf.GetFormComponent;
  try
    for i := 1 to StringGrid.RowCount - 1 do
    begin
      n := Integer(StringGrid.Objects[0, i]);
      CompIntf := FormCompIntf.GetComponent(n);
      if CompIntf <> nil then
      begin
        p := GetPropIndex(CompIntf, 'TAG');
        if p >= 0 then
          SetPropAsString(CompIntf, p, StringGrid.Cells[2, i]);
        p := GetPropIndex(CompIntf, 'HELPCONTEXT');
        if p >= 0 then
          SetPropAsString(CompIntf, p, StringGrid.Cells[3, i]);
      end;
    end;
    FModified := False;
  finally
    FormCompIntf.Free;
  end;
end;

procedure TfmComponentGrid.FormClose(Sender: TObject; var Action: TCloseAction);
resourcestring
  SSaveChanges = 'You have unsaved changes, save them before closing?';
begin
  if FModified then
    if MessageDlg(SSaveChanges, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      sbSaveClick(sbSave);
end;

procedure TfmComponentGrid.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = 3) and (StringGrid.Cells[3, ARow] = SNotAvailable) then
    CanSelect := False;
end;

procedure TfmComponentGrid.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 22);
end;

procedure TfmComponentGrid.sbNumberingClick(Sender: TObject);
begin
  If sbNumbering.Down then
  begin
    edStart.Text := IntToStr(0);
    edSkipBy.Text := IntToStr(10);
    pnlNumbering.Visible := True;
    edStart.SetFocus;
  end
  else
    pnlNumbering.Visible := False;
end;

procedure TfmComponentGrid.sbPrintClick(Sender: TObject);
var
  i, Page: Integer;
  F: TextFile;
  ReportName: string;
begin
  ReportName := FormCaption;
  InputQuery('Report Name', 'Enter the name for this report', ReportName);
  Printer.Canvas.Font.Size := 10;
  Printer.Canvas.Font.Pitch := fpFixed;
  Printer.Title := 'Component Help Context Report';
  Printer.Orientation := poPortrait;
  AssignPrn(F);
  Rewrite(F);
  Page := Printer.PageNumber;
  writeLn(F,'Page# '+IntToStr(Page)+#9+#9+#9+#9+ReportName);
  writeln(F,'  ');
  writeLn(F,'ComponentName'+#9+#9+'Parent'+#9+#9+'Tag'+#9+#9+'Help ID');
  for i := 1 to StringGrid.RowCount -1 do
    begin
      If (Page <> Printer.PageNumber) then
        begin
          Page := Printer.PageNumber;
          writeLn(F, 'Page# '+IntToStr(Page)+#9+#9+#9+#9+ReportName);
          writeln(F, ' ');
          writeLn(F, 'ComponentName'+#9+#9+'Parent'+#9+#9+'Tag'+#9+#9+'Help ID');
        end;
      with StringGrid do
        WriteLn(F, SetLength(Cells[0,i], 23) + SetLength(Cells[1,i], 15)
          + SetLength(Cells[2,i],15)+Cells[3,i]);
    end;
  System.CloseFile(F);
  //ShowMessage('Document has been printed.');
end;

procedure TfmComponentGrid.sbRenumberClick(Sender: TObject);
var
  i: Integer;
  StartAt, SkipBy: Integer;
begin
  StartAt := edStart.Value;
  SkipBy := edSkipBy.Value;
  for i := 1 to StringGrid.RowCount - 1 do
    if StringGrid.Cells[3, i] <> 'N/A' then
    begin
      StringGrid.Cells[3, i] := IntToStr(StartAt);
      StartAt := StartAt + SkipBy;
    end;
end;

{ TGridExpert }

constructor TGridExpert.Create;
begin
  inherited Create;
  ShortCut := 0;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TGridExpert.Destroy;
begin
  inherited Destroy;
end;

function TGridExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Component &Grid...';
begin
  Result := SMenuCaption;
end;

function TGridExpert.GetMenuName: string;
begin
  Result := 'GX_Grid';
end;

function TGridExpert.GetMenuMask: string;
begin
  Result := '.DFM';
end;

function TGridExpert.GetName: string;
begin
  Result := 'Component_Grid_Expert';
end;

function TGridExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Component Grid';
begin
  Result := SDisplayName;
end;

procedure TGridExpert.Click(Sender: TObject);
begin
  with TfmComponentGrid.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TGridExpert.IconFileName: string;
begin
  Result := 'Grid';
end;

procedure TGridExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Component Grid is a modal expert
    end;
  end;
end;

initialization
  RegisterGX_Expert(TGridExpert);
end.


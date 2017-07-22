unit FieldByNameExpert;

interface

uses Windows, Classes, SysUtils, ExptIntf, ToolIntf, EditIntf, Dialogs, db, TypInfo;

type
  TFieldByNameExpert = class(TIExpert)
  private
    fMenuItem: TIMenuItemIntf;
    procedure ShowByNameDlg(StatementName: String; FieldList, ParamList: TStrings);
    function GetSelFieldNameList(ADataSet: TDataSet): TStrings;
    function GetSelParamNameList(ADataSet: TDataSet): TStrings;
    procedure MenuClick(Sender: TIMenuItemIntf);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetGlyph: HICON; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    function GetName: string; override;
    function GetPage: string; override;
    function GetState: TExpertState; override;
    function GetStyle: TExpertStyle; override;

  end;

implementation

uses
  ByNameList, InsertMenuItemUnit, GroupPackageExpertRegUnit;

resourcestring
  sAuthor = 'Daniel Work';
  sComment = 'This expert will get the names of the fields (and if available params) in the selected TDataSet';
  sIDString = 'DanielWork.FieldByName Expert';
  sName = 'FieldByName Expert';
  sPage = 'Forms';
  sEntryName = 'FieldByNameExpert';
  sEntryCaption = '&FieldByName Expert...';


constructor TFieldByNameExpert.Create;
begin
  inherited create;
  fMenuItem := InsertMenuItem(iaChild, sParentEntryName, sEntryCaption, sEntryName, '', 0, 0, 0, [mfEnabled, mfVisible], self.MenuClick);
end;

destructor TFieldByNameExpert.Destroy;
begin
  self.fMenuItem.Free;
  inherited destroy;
end;

procedure TFieldByNameExpert.MenuClick(Sender: TIMenuItemIntf);
begin
  self.Execute;
end;

procedure TFieldByNameExpert.Execute;
var
  IModule: TIModuleInterface;
  IForm: TIFormInterface;
  ICurrentComponent: TIComponentInterface;
  CurrentComponent: TComponent;
begin
  with ToolServices do
    { If the user is editing a form, GetCurrentFile return the path to
      the DFM file, but we need the path to the PAS file. If the user is
      not editing a DFM file, punt. }
    if CompareText(ExtractFileExt(GetCurrentFile), '.dfm') = 0 then
      IModule := GetModuleInterface(ChangeFileExt(GetCurrentFile, '.pas'))
    else
      IModule := nil;

  if IModule = nil then
    raise Exception.Create('Please Select a Form');

  try
    IForm := IModule.GetFormInterface;
    if IForm = nil then
      raise Exception.Create('Please Select a Form');
    try
      ICurrentComponent := IForm.GetSelComponent(0);
      if Assigned(ICurrentComponent) then begin
        CurrentComponent := TComponent(ICurrentComponent.GetComponentHandle);
        if CurrentComponent is TDataSet then begin
          ShowByNameDlg(TDataSet(CurrentComponent).Name,
                        GetSelFieldNameList(TDataSet(CurrentComponent)),
                        GetSelParamNameList(TDataSet(CurrentComponent)));
        end{if}
        else ShowMessage('Not A TDataSet');
      end;{for}
    finally
      IForm.Free;
    end;
  finally
    IModule.Free;
  end;
end;

procedure TFieldByNameExpert.ShowByNameDlg(StatementName: String; FieldList, ParamList: TStrings);
var
  ByNameListDlg: TByNameListDlg;
begin
  if Assigned(FieldList) and Assigned(ParamList) then begin
    if (FieldList.Count > 0) or (ParamList.Count > 0)  then begin
      ByNameListDlg := TByNameListDlg.Create(NIL);
      try
        if Assigned(ByNameListDlg) then begin
          ByNameListDlg.FieldList.AddStrings(FieldList);
          ByNameListDlg.ParamList.AddStrings(ParamList);
          ByNameListDlg.StatementName := StatementName;
          ByNameListDlg.ByNameRadioGroupClick(Self);
          ByNameListDlg.ShowModal;
        end;{if}
      finally
        ByNameListDlg.Free;
      end;{except}
    end{if}
    else ShowMessage('No Field / No Params');
  end;{if}
end;

function TFieldByNameExpert.GetAuthor: string;
begin
  Result := sAuthor
end;

function TFieldByNameExpert.GetComment: string;
begin
  Result := sComment
end;

{ For a DLL, set the application icon in the project options.
  For a package, add a MAINICON resource explicitly. }
function TFieldByNameExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'MAINICON')
end;

function TFieldByNameExpert.GetIDString: string;
begin
  Result := sIDString
end;

function TFieldByNameExpert.GetMenuText: string;
begin
  Result := ''
end;

function TFieldByNameExpert.GetName: string;
begin
  Result := sName
end;

function TFieldByNameExpert.GetPage: string;
begin
  Result := sPage
end;

function TFieldByNameExpert.GetState: TExpertState;
begin
  Result := [ esEnabled ]
end;

function TFieldByNameExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn
end;

function TFieldByNameExpert.GetSelFieldNameList(ADataSet: TDataSet): TStrings;
var
  nCount: Integer;
  ActiveOrig: Boolean;

  function FindItem(Item: String; aStrings: TStrings): Boolean;
  begin
    Result := (aStrings.IndexOf(Item) <> -1);
  end;

begin
  ActiveOrig := ADataSet.Active;
  ADataSet.Active := True;
  Result := TStringList.Create;
  try
    for nCount := 0 to (ADataSet.FieldCount - 1) do begin
      if FindItem(ADataSet.Fields[nCount].FieldName, Result) then
        Result.Append(ADataSet.Fields[nCount].FullName)
      else
        Result.Append(ADataSet.Fields[nCount].FieldName);
    end;{for}
    ADataSet.Active := ActiveOrig;
  except
    Result.Free;
  end;{except}
end;

function TFieldByNameExpert.GetSelParamNameList(ADataSet: TDataSet): TStrings;
var
  AParams: TParams;
  ParamsPropInfo: PPropInfo;
  TmpCount: Integer;
  ActiveOrig: Boolean;
begin
  ActiveOrig := ADataSet.Active;
  ADataSet.Active := True;
  Result := TStringList.Create;
  try
    ParamsPropInfo := GetPropInfo(ADataSet.ClassInfo, 'Params');
    if Assigned(ParamsPropInfo) then begin
      if (ParamsPropInfo^.PropType^.Kind = tkClass) then
        if (TObject(GetOrdProp(ADataSet, ParamsPropInfo)) is TParams) then begin
          AParams := TParams(GetOrdProp(ADataSet, ParamsPropInfo));
          for TmpCount := 0 to (AParams.Count - 1) do Result.Append(AParams[TmpCount].Name);
        end{if}
        else Result.Clear;
    end;{if}
    ADataSet.Active := ActiveOrig;
  except
    Result.Free;
  end;{except}
end;

end.

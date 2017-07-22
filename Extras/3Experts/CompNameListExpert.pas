unit CompNameListExpert;

interface

uses Windows, Classes, SysUtils, ExptIntf, ToolIntf, EditIntf, Dialogs;

type
  TCompNameListExpert = class(TIExpert)
  private
    fMenuItem : TIMenuItemIntf;

    procedure MenuClick( Sender : TIMenuItemIntf );
    function GetSelComponentNameList(IForm : TIFormInterface) : TStrings;
    procedure ShowCompListDlg(SelCompList : TStrings);
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
  CompListDlg, InsertMenuItemUnit, GroupPackageExpertRegUnit;

resourcestring
  sAuthor = 'Daniel Work';
  sComment = 'This expert will get the names of the compents selected on the current form';
  sIDString = 'DanielWork.TCompNameList Expert';
  sName = 'TCompNameList Expert';
  sPage = 'Forms';
  sEntryName = 'CompNameList';
  sEntryCaption = '&CompNameList Expert...';


constructor TCompNameListExpert.Create;
begin
  inherited create;
  fMenuItem := InsertMenuItem( iaChild, sParentEntryName, sEntryCaption, sEntryName, '', 0, 0, 0, [mfEnabled, mfVisible], self.MenuClick );
end;

destructor TCompNameListExpert.Destroy;
begin
  self.fMenuItem.Free;
  inherited destroy;
end;

procedure TCompNameListExpert.MenuClick( Sender : TIMenuItemIntf );
begin
  self.Execute;
end;

procedure TCompNameListExpert.Execute;
var
  IModule: TIModuleInterface;
  IForm : TIFormInterface;
  CompList : TStrings;
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
      CompList := GetSelComponentNameList(IForm);
      try
        if Assigned(CompList) then ShowCompListDlg(CompList);
      finally
        if Assigned(CompList) then CompList.Free;
      end;{finally}
    finally
      IForm.Free;
    end;
  finally
    IModule.Free;
  end;
end;

function TCompNameListExpert.GetAuthor: string;
begin
  Result := sAuthor
end;

function TCompNameListExpert.GetComment: string;
begin
  Result := sComment
end;

{ For a DLL, set the application icon in the project options.
  For a package, add a MAINICON resource explicitly. }
function TCompNameListExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'MAINICON')
end;

function TCompNameListExpert.GetIDString: string;
begin
  Result := sIDString
end;

function TCompNameListExpert.GetMenuText: string;
begin
  Result := ''
end;

function TCompNameListExpert.GetName: string;
begin
  Result := sName
end;

function TCompNameListExpert.GetPage: string;
begin
  Result := sPage
end;

function TCompNameListExpert.GetState: TExpertState;
begin
  Result := [ esEnabled ]
end;

function TCompNameListExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn
end;


function TCompNameListExpert.GetSelComponentNameList(IForm : TIFormInterface) : TStrings;
var
  nCount : Integer;
  cCompName : String;
begin
  Result := NIL;
  if Assigned(IForm) then begin
    Result := TStringList.Create;
    try
     for nCount := 0 to (IForm.GetSelCount - 1) do begin
       cCompName := TComponent(IForm.GetSelComponent(nCount).GetComponentHandle).Name;
       if cCompName <> SysUtils.EmptyStr then
         Result.Append(cCompName);
     end;{for}
    except
      Result.Free;
    end;{except}
  end;{if}
end;

procedure TCompNameListExpert.ShowCompListDlg(SelCompList : TStrings);
var
  SelCompListDlg: TSelCompListDlg;
begin
  if Assigned(SelCompList) then begin
    if SelCompList.Count > 0 then begin
      SelCompListDlg := TSelCompListDlg.Create(NIL);
      try
        if Assigned(SelCompListDlg) then begin
          SelCompListDlg.SrcList.Clear;
          SelCompListDlg.SrcList.Items.AddStrings(SelCompList);
          SelCompListDlg.DstList.Clear;
          SelCompListDlg.ShowModal;
        end;{if}
      finally
        SelCompListDlg.Free;
      end;{except}
    end;{if}
  end;{if}
end;

end.

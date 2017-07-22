unit GroupPackageExpertUnit;

interface

uses Windows, Classes, ExptIntf, ToolIntf, EditIntf;

type

  TGroupPackageExpert = class(TIExpert)
  private
    fMenuItem : TIMenuItemIntf;
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

uses InsertMenuItemUnit, GroupPackageExpertRegUnit;

resourcestring
  sAuthor = 'Daniel Work';
  sComment = 'This expert will provide a menu space for the other experts';
  sIDString = 'DanielWork.TGroupPackage Expert';
  sName = 'TGroupPackage Expert';
  sPage = 'Forms';

constructor TGroupPackageExpert.Create;
begin
  inherited create;
  fMenuItem := InsertMenuItemUnit.InsertMenuItem( iaBefore, 'ToolsToolsItem', sParentEntryCaption, sParentEntryName,
                                                 '', 0, 0, 0, [mfEnabled, mfVisible], NIL);
end;

destructor TGroupPackageExpert.Destroy;
begin
  self.fMenuItem.Free;
  inherited destroy;
end;

procedure TGroupPackageExpert.Execute;
begin
end;

function TGroupPackageExpert.GetAuthor: string;
begin
  Result := sAuthor
end;

function TGroupPackageExpert.GetComment: string;
begin
  Result := sComment
end;

{ For a DLL, set the application icon in the project options.
  For a package, add a MAINICON resource explicitly. }
function TGroupPackageExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'MAINICON')
end;

function TGroupPackageExpert.GetIDString: string;
begin
  Result := sIDString
end;

function TGroupPackageExpert.GetMenuText: string;
begin
  Result := ''
end;

function TGroupPackageExpert.GetName: string;
begin
  Result := sName
end;

function TGroupPackageExpert.GetPage: string;
begin
  Result := sPage
end;

function TGroupPackageExpert.GetState: TExpertState;
begin
  Result := [ esEnabled ]
end;

function TGroupPackageExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn
end;

end.

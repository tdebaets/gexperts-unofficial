unit SpaceByPixelExpertUnit;

interface

uses Windows, Classes, SysUtils, ExptIntf, ToolIntf, EditIntf, Dialogs, controls;

type
  TSpacingDirection = (sdHorizontal, sdVertical);

  TSpaceByPixelExpert = class(TIExpert)
  private
    fMenuItem : TIMenuItemIntf;

    procedure MenuClick( Sender : TIMenuItemIntf );
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

uses ByPixelDialogUnit, InsertMenuItemUnit, GroupPackageExpertRegUnit;

resourcestring
  sAuthor = 'Daniel Work';
  sComment = 'This expert will space the selected controls by a number of pixels';
  sIDString = 'DanielWork.TSpaceByPixel Expert';
  sName = 'TSpaceByPixel Expert';
  sPage = 'Forms';
  sEntryName = 'SpaceByPixel';
  sEntryCaption = '&SpaceByPixel Expert...';


constructor TSpaceByPixelExpert.Create;
begin
  inherited create;
  fMenuItem := InsertMenuItem( iaChild, sParentEntryName, sEntryCaption, sEntryName, '', 0, 0, 0, [mfEnabled, mfVisible], self.MenuClick);
end;

destructor TSpaceByPixelExpert.Destroy;
begin
  self.fMenuItem.Free;
  inherited destroy;
end;

procedure TSpaceByPixelExpert.MenuClick( Sender : TIMenuItemIntf );
begin
  self.Execute;
end;

procedure TSpaceByPixelExpert.Execute;
var
  IModule: TIModuleInterface;
  IForm : TIFormInterface;
  ICurrentComponent : TIComponentInterface;
  ParentControl,
  CurrentControl,
  PrevControl : TControl;
  nCount,
  nPixBy : Word;
  SpacingDirection : TSpacingDirection;
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
      if (IForm.GetSelCount <= 0) then
        raise Exception.Create('No controls are selected on the form');

      for nCount := 0 to (IForm.GetSelCount - 1) do begin
        ICurrentComponent := IForm.GetSelComponent(nCount);
        if not(ICurrentComponent.IsTControl) then
          raise Exception.Create(TComponent(ICurrentComponent.GetComponentHandle).Name
           + ' is not a TControl');
      end;{for}
      ParentControl := TControl(IForm.GetSelComponent(0).GetComponentHandle).Parent;
      for nCount := 0 to (IForm.GetSelCount - 1) do begin
        if not(ParentControl = TControl(IForm.GetSelComponent(nCount).GetComponentHandle).Parent) then
          raise Exception.Create(TComponent(IForm.GetSelComponent(nCount).GetComponentHandle).Name +
            ' has a different parent from the other controls');
      end;{for}

      nPixBy := 0;
      ShowByPixelDialog(SpacingDirection, nPixBy);
      if (nPixBy > 0) then begin
        PrevControl := NIL;
        for nCount := 0 to (IForm.GetSelCount - 1) do begin
          CurrentControl := TControl(IForm.GetSelComponent(nCount).GetComponentHandle);
          if (PrevControl = NIL) then
            PrevControl := CurrentControl
          else begin
            case SpacingDirection of
              sdHorizontal : CurrentControl.Left := (PrevControl.Left + PrevControl.Width) + nPixBy;
              sdVertical   : CurrentControl.Top := (PrevControl.Top + PrevControl.Height) + nPixBy;
            end;{Case}
            PrevControl := CurrentControl;
          end;{else}
        end;{for}
      end;{if}
    finally
      IForm.Free;
    end;
  finally
    IModule.Free;
  end;
end;

function TSpaceByPixelExpert.GetAuthor: string;
begin
  Result := sAuthor
end;

function TSpaceByPixelExpert.GetComment: string;
begin
  Result := sComment
end;

{ For a DLL, set the application icon in the project options.
  For a package, add a MAINICON resource explicitly. }
function TSpaceByPixelExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'MAINICON')
end;

function TSpaceByPixelExpert.GetIDString: string;
begin
  Result := sIDString
end;

function TSpaceByPixelExpert.GetMenuText: string;
begin
  Result := ''
end;

function TSpaceByPixelExpert.GetName: string;
begin
  Result := sName
end;

function TSpaceByPixelExpert.GetPage: string;
begin
  Result := sPage
end;

function TSpaceByPixelExpert.GetState: TExpertState;
begin
  Result := [ esEnabled ]
end;

function TSpaceByPixelExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn
end;

end.

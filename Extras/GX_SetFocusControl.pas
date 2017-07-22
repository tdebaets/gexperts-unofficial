unit GX_SetFocusControl;

{$I GX_CondDefine.inc}

(*
 Set FocusControl property

 Original author: Primoz Gabrijelcic <gabr@17slon.com>
 Creation date  : 1999-11-03
 Last change    : 1999-11-04
 Version        : 1.0

 Change history :
   1.0: 1999-11-04
   - First public release.

 Tested with: D4.

 Thanks to python.
*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DsgnIntf, ToolIntf, ExptIntf, RplWizInfo, EditIntf,
  GX_Experts, ExtCtrls;

type
  TSetFocusControlExpert = class(TGX_Expert)
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  private
    procedure DoSetFocusCode;
  end;

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GExperts,
  GX_GenFunc,
  TypInfo,
  Clipbrd,
  Registry;

{ TSetFocusControlExpert }

constructor TSetFocusControlExpert.Create;
begin
  inherited Create;
  HasConfigOptions := False;
  HasMenuItem := True;
end; { TSetFocusControlExpert.Create }

function TSetFocusControlExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Set FocusContro&l';
begin
  Result := SMenuCaption;
end; { TSetFocusControlExpert.GetMenuCaption }

function TSetFocusControlExpert.GetMenuName: string;
begin
  Result := 'GX_SetFocusControl';
end; { TSetFocusControlExpert.GetMenuName }

function TSetFocusControlExpert.GetMenuMask: string;
begin
  Result := '.DFM';
end; { TSetFocusControlExpert.GetMenuMask }

function TSetFocusControlExpert.GetName: string;
begin
  Result := 'Set_FocusControl';
end; { TSetFocusControlExpert.GetName }

function TSetFocusControlExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Set FocusControl property';
begin
  Result := SDisplayName;
end; { TSetFocusControlExpert.GetDisplayName }

procedure TSetFocusControlExpert.Click(Sender: TObject);
begin
  DoSetFocusCode;
end; { TSetFocusControlExpert.Click }

function TSetFocusControlExpert.IconFileName: string;
begin
  Result := '';
end; { TSetFocusControlExpert.IconFileName }

procedure TSetFocusControlExpert.DoSetFocusCode;
var
  FormIntf       : TIFormInterface;
  ModIntf        : TIModuleInterface;
  CompIntf       : TIComponentInterface;
  CompIntf2      : TIComponentInterface;
  i              : integer;
  CurrentFileName: string;
  focusControl   : TWinControl;
  focusControl2  : TWinControl;
  comp           : TComponent;
  comp2          : TComponent;
  setDone        : boolean;
resourcestring
  SDfmOnly = 'This expert is for use in .DFM files only.';
  SCouldNotGetModuleIntf = ' - Could not get module interface';
  SCouldNotGetFormIntf = ' - Could not get form interface';
  SSelectTwoComponents = 'Please select two components first.';
  SSetFailed = 'Set failed. One component must have FocusControl property, other must not.';
begin
  ModIntf := nil;
  FormIntf := nil;
  CompIntf := nil;
  if ExtractFileExt(UpperCase(ToolServices.GetCurrentFile)) <> '.DFM' then
    MessageDlg(SDfmOnly, mtError, [mbOK], 0)
  else begin
    // first assume that we have a Pascal source file
    CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.PAS'));
    try
      ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
      if ModIntf = nil then begin
        // try again with C++ source file
        CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.CPP'));
        ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
      end;
      if ModIntf = nil then
        MessageDlg(CurrentFileName + SCouldNotGetModuleIntf, mtError, [mbOK], 0)
      else begin
        FormIntf := ModIntf.GetFormInterface;
        if FormIntf = nil then
          MessageDlg(CurrentFileName + SCouldNotGetFormIntf, mtError, [mbOK], 0)
        else begin
          if FormIntf.GetSelCount <> 2 then
            MessageDlg(SSelectTwoComponents, mtError, [mbOK], 0)
          else begin
            setDone := false;
            for i := 0 to 1 do begin
              CompIntf := FormIntf.GetSelComponent(i);
              try
                if CompIntf <> nil then begin
                  if CompIntf.GetPropValuebyName('FOCUSCONTROL',focusControl) then begin
                    CompIntf2 := FormIntf.GetSelComponent(1-i);
                    try
                      if CompIntf2 <> nil then begin
                        if not CompIntf2.GetPropValuebyName('FOCUSCONTROL',focusControl2) then begin
                          comp2 := CompIntf2.GetComponentHandle;
                          if comp2 is TWinControl then begin
                            comp := CompIntf.GetComponentHandle;
                            SetOrdProp(comp, GetPropInfo(comp.ClassInfo,'FocusControl'), integer(comp2));
                            setDone := true;
                          end;
                        end;
                      end;
                    finally
                      CompIntf2.Free;
                      CompIntf2 := nil;
                    end;
                  end;
                end;
              finally
                CompIntf.Free;
                CompIntf := nil;
              end;
            end; //for
            if not setDone then
              MessageDlg(SSetFailed, mtError, [mbOK], 0);
          end;
        end;
      end;
    finally
      if ModIntf <> nil then ModIntf.Free;
      if FormIntf <> nil then FormIntf.Free;
      if CompIntf <> nil then CompIntf.Free;
    end;
  end;
end; { TSetFocusControlExpert.DoSetFocusCode }

initialization
  RegisterGX_Expert(TSetFocusControlExpert);
end.

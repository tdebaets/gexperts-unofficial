
{*******************************************************}
{                                                       }
{  Implements a TToolbar descendant that has a Menu to  }
{  make IDE like Toolbar menus very easy.  This works   }
{  only in Delphi 4.0                                   }
{                                                       }
{       Copyright (c) 1995,98 Inprise Corporation       }
{                                                       }
{*******************************************************}

// CMDialogChar hack by Erik Berry (July, 1999)

unit MenuBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, Menus;

type
  TMenuBar = class(TToolBar)
  private
    FMenu: TMainMenu;
    procedure SetMenu(const Value: TMainMenu);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EdgeBorders default [];
    property Menu: TMainMenu read FMenu write SetMenu;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TMenuBar]);
end;

function IsChildOfParent(Par: TWinControl; Child: TWinControl): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Child = nil) or (Par = nil) then Exit;
  for i := 0 to Par.ControlCount - 1 do
  begin
    if Par.Controls[i] = Child then
    begin
      Result := True;
      Break;
    end
    else if (not Result) and (Par.Controls[i] is TWinControl) then
      Result := IsChildOfParent(TWinControl(Par.Controls[i]), Child);
  end;
end;

{ TMenuBar }

procedure TMenuBar.CMDialogChar(var Message: TCMDialogChar);
begin
  if ((Parent <> nil) and (Parent.Parent <> nil) and
   (not IsChildOfParent(Self.Parent,  Screen.ActiveControl))) then
    begin
      // EB: Ignore keypresses when we're on a docked form that doesn't
      // contain the ActiveControl.  Prevents unnecessary appearance of the
      // menus in the editor, and when a tab (docked form) is inactive
    end
    else
      inherited;
end;

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Flat := True;
  ShowCaptions := True;
  EdgeBorders := [];
  ControlStyle := [csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption];
end;

procedure TMenuBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TMenuBar.SetMenu(const Value: TMainMenu);
var
  i: Integer;
  Button: TToolButton;
begin
  if FMenu = Value then exit;
  if Assigned(FMenu) then
    for i := ButtonCount - 1 downto 0 do
      Buttons[i].Free;
  FMenu := Value;
  if not Assigned(FMenu) then exit;
  for i := ButtonCount to FMenu.Items.Count - 1 do
  begin
    Button := TToolButton.Create(Self);
    try
      Button.AutoSize := True;
      Button.Grouped := True;
      Button.Parent := Self;
      Buttons[i].MenuItem := FMenu.Items[i];
    except
      Button.Free;
      raise;
    end;
  end;
  { Copy attributes from each menu item }
  for i := 0 to FMenu.Items.Count - 1 do
    Buttons[i].MenuItem := FMenu.Items[i];
end;

end.

{***************************************************************
 *
 * Unit Name: GX_eAlign
 * Purpose  : To move the line containing the cursor to the
 *            Top/Middle/Bottom of the edit window
 * Author   : Scott Mattes, smattes@erols.com
 * History  : 1999/10/12, initial release
 *
 ****************************************************************}

unit GX_eLineMove;

{$I GX_CondDefine.inc}

interface

uses
  ToolIntf,
//  RzCSIntf,
  GX_uExperts,
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  GX_EditorExpert, ExtCtrls, ComCtrls, toolsapi;

type
  TCursorLineToTop = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Execute; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TCursorLineToMid = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Execute; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TCursorLineToBot = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Execute; override;
    procedure GetHelpString(List: TStrings); override;
  end;

implementation

uses
  GX_DbugIntf,
  Menus,
  ExptIntf, EditIntf, GX_EditRead, GX_uGenFunc, ExpertUtil,
  Registry, GX_GExperts, GX_uConfigurationInfo;

procedure MoveLine( Which : string );
var
  CursorPos : TEditPos;
  EditIntf  : TIEditorInterface;
  EditView  : TIEditView;
  ModIntf   : TIModuleInterface;
  OffSet    : integer;

begin
  // First, we need the module interface
  ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile);
  if ModIntf = nil
  then raise Exception.Create('eLineMove: No Module Interface');

  // Next, we need the Editor interface
  EditIntf := ModIntf.GetEditorInterface;
  if EditIntf = nil
  then raise Exception.Create('eLineMove: No Editor Interface');

  // Finally, we gotta have the View
  EditView := EditIntf.GetView( GetCurrentEditView(EditIntf) );

  // Where is the cursor presently?
  CursorPos := EditView.CursorPos;

  // Change the column value so the screen doesn't scroll left to right
  CursorPos.Col := EditView.TopPos.Col;

  // Move the line with the cursor to the desired position
  OffSet := 0;  // Top position
  if which = 'Middle'
  then offset := EditView.viewsize.cy div 2
  else if which = 'Bottom'
       then offset := EditView.viewsize.cy - 2; // -2 to insure visibility

  // Make sure we are asking for what can be given
  if OffSet < 0
  then OffSet := 0;

  CursorPos.line := CursorPos.line - OffSet;
  EditView.TopPos := CursorPos;

  // but we gotta clean up after ourselves
  EditIntf.Free;

  ModIntf.Free;

  EditView.Free;

// SendDebug('TCursorLineToTop.execute');
end;

{ TCursorLineToTop }

constructor TCursorLineToTop.Create;
begin
  ShortCut := scCtrl + scAlt + Ord('T');

  FName := 'Move line cursor is on to the top of the Editor window';
end;


procedure TCursorLineToTop.Execute;
begin
//ShowMessage( 'Top' );
  MoveLine( 'Top' );
end;

procedure TCursorLineToTop.GetHelpString(List: TStrings);
begin
  list.Text := 'TCursorLineToTop';
end;

{ TCursorLineToMid }

constructor TCursorLineToMid.Create;
begin
  ShortCut := scCtrl + scAlt + Ord('M');

  FName := 'Move line cursor is on to the middle of the Editor window';
end;


procedure TCursorLineToMid.Execute;
begin
  MoveLine( 'Middle' );
end;

procedure TCursorLineToMid.GetHelpString(List: TStrings);
begin
  list.Text := 'TCursorLineToMid';
end;

{ TCursorLineToBot }

constructor TCursorLineToBot.Create;
begin
  ShortCut := scCtrl + scAlt + Ord('B');

  FName := 'Move line cursor is on to the bottom of the Editor window';
end;


procedure TCursorLineToBot.Execute;
begin
  MoveLine( 'Bottom' );
end;

procedure TCursorLineToBot.GetHelpString(List: TStrings);
begin
  list.Text := 'TCursorLineToBot';
end;

initialization
  RegisterEditorExpert(TCursorLineToTop);
  RegisterEditorExpert(TCursorLineToMid);
  RegisterEditorExpert(TCursorLineToBot);
end.


unit InsertMenuItemUnit;

interface

uses Windows, Classes, SysUtils, ExptIntf, ToolIntf, EditIntf;

type
  TInsertAction = (iaBefore, iaAfter, iaChild);


function InsertMenuItem(Action: TInsertAction;
  TargetName, Caption, Name, Hint: string;
  ShortCut, Context, GroupIndex: Integer;
  Flags: TIMenuFlags;
  EventHandler: TIMenuClickEvent): TIMenuItemIntf;
  
implementation

{ The insert action dictates how InsertMenuItem interprets the
  target menu item. Use iaBefore to insert an item immediately
  before the target (i.e., at the same index). Use iaAfter to
  add a new item after the target (index + 1). Use iaChild to
  create a child menu item, where the target is the parent
  menu. }

{ Insert a new menu item relative to the item named
  TargetName. Pass the Caption .. EventHandler arguments
  directly to InsertItem. }
function InsertMenuItem(Action: TInsertAction;
  TargetName, Caption, Name, Hint: string;
  ShortCut, Context, GroupIndex: Integer;
  Flags: TIMenuFlags;
  EventHandler: TIMenuClickEvent): TIMenuItemIntf;
var
  TargetItem: TIMenuItemIntf;
  ParentItem: TIMenuItemIntf;
  Index: Integer;
begin
  Result := nil;
  { Get the menu bar interface. }
  with ToolServices.GetMainMenu do
    try
      { Look up the target menu item. }
      TargetItem := FindMenuItem(TargetName);
      if TargetItem = nil then
        raise Exception.CreateFmt('Cannot find menu item, %s',
                                  [TargetName]);
      try
        if Action = iaChild then
        begin
          { Create a child of the target item. }
          ParentItem := TargetItem;
          ParentItem.AddRef; { avoid a double free }
          Index := 0;
        end
        else
        begin
          { To create a sibling of the target, get the
            target's parent. }          ParentItem := TargetItem.GetParent;
          Index := TargetItem.GetIndex;
          if Action = iaAfter then
            Inc(Index);
        end;
        if ParentItem = nil then
          raise Exception.CreateFmt('No menu parent for %s',
                                    [TargetName]);
        try
          Result := ParentItem.InsertItem(Index,
                        Caption, Name, Hint, ShortCut, Context,
                        GroupIndex, Flags, EventHandler);
        finally
          ParentItem.Free;
        end;
      finally
        TargetItem.Free;
      end;
    finally
      Free;
    end;
end;

end.

unit FastSortTreeView;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  CommCtrl, ComCtrls;

type
  { This is a special treeview that merely sorts that little
    bit faster (a whole lot faster, to be more accurate).
    Plenty of stuff has been copied from the Delphi 4.0 source
    code since none of the interesting methods was virtual.
    For VCL 4.0 this class more or less is a stub that
    passes everything through to its ancestor. }
  TFastSortTreeView = class(TTreeView)
  {$IFNDEF GX_VER120_up}
  private
    FSortType: TSortType;
  protected
    procedure SetSortType(Value: TSortType);
  public
    { These two methods should have been virtual in
      the VCL source code; unfortunately
      Borland / Inprise were not wise enough
      to implement this the way it should be }
    function AlphaSort: Boolean;
    function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
  published
    property SortType: TSortType read FSortType write SetSortType default stNone;
  {$ENDIF GX_VER120_up}
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GExperts', [TFastSortTreeView]);
end;

{$IFNDEF GX_VER120_up}

// copied from the Delphi 4.0 VCL source code
function TFastSortTreeView.AlphaSort: Boolean;
var
  Node: TTreeNode;
begin
  if HandleAllocated then
  begin
    Result := CustomSort(nil, 0);

    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      if Node.HasChildren then
        Node.AlphaSort;
      Node := Node.GetNext;
    end;
  end
  else
    Result := False;
end;

function DefaultTreeViewSort(Node1, Node2: TTreeNode; lParam: Integer): Integer; stdcall;
begin
  // Note: Delphi 3.02 and C++Builder 3.0 will *NOT* fire
  // the OnCompare event here - this is an incompatibility
  // which was deliberately created.
  Result := lstrcmp(PChar(Node1.Text), PChar(Node2.Text));
end;

// copied from the Delphi 4.0 VCL source code
function TFastSortTreeView.CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
var
  SortCB: TTVSortCB;
  Node: TTreeNode;
begin
  Result := False;
  if HandleAllocated then
  begin
    with SortCB do
    begin
      if not Assigned(SortProc) then
        lpfnCompare := Pointer(@DefaultTreeViewSort)
      else
        lpfnCompare := SortProc;
      hParent := TVI_ROOT;
      lParam := Data;
      Result := TreeView_SortChildrenCB(Handle, SortCB, 0);
    end;

    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      if Node.HasChildren then
        Node.CustomSort(SortProc, Data);
      Node := Node.GetNext;
    end;
  end;
end;

// copied from the Delphi 4.0 VCL source code
procedure TFastSortTreeView.SetSortType(Value: TSortType);
begin
  if SortType <> Value then
  begin
    FSortType := Value;
    if ((SortType in [stData, stBoth]) and Assigned(OnCompare)) or
       (SortType in [stText, stBoth]) then
    begin
      AlphaSort;
    end;
  end;
end;

{$ENDIF GX_VER120_up}

end.

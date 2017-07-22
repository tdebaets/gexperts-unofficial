unit GX_CPULabels;

// Original Author: Python <python@softhome.net>
// This expert works only in Delphi 4, and allows you to
// name addresses in the CPU view window.  It has no icon yet.

{$I GX_CondDefine.inc}

interface

uses
  Menus, GX_Experts, Classes, Controls, ComCtrls, Dialogs, GX_IdeDock;

type
  TfmCPULabels = class(TfmIdeDockForm)
    pmuLabels: TPopupMenu;
    mitAdd: TMenuItem;
    mitDelete: TMenuItem;
    mitClear: TMenuItem;
    lvwLabels: TListView;
    mitEditLabel: TMenuItem;
    mitOpen: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    mitSave: TMenuItem;
    mitSaveAs: TMenuItem;
    mitSep1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mitAddClick(Sender: TObject);
    procedure lvwLabelsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mitDeleteClick(Sender: TObject);
    procedure mitClearClick(Sender: TObject);
    procedure mitEditLabelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mitOpenClick(Sender: TObject);
    procedure mitSaveClick(Sender: TObject);
    procedure mitSaveAsClick(Sender: TObject);
  private
    procedure OnAdd(Index: Integer);
    procedure OnChangeLabel(Index: Integer);
    procedure OnClear;
    procedure OnDelete(Index: Integer);
  public
    procedure Refresh(Sender: TObject);
  end;

var
  fmCPULabels: TfmCPULabels;
  EditMenuItem: TMenuItem;
  CPULabelsMenuItem: TMenuItem;

type
  TCPULabelExpert = class(TGX_EnhExpert)
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
    procedure SetActive(New: Boolean); override;
  end;

implementation

{$R *.DFM}

uses
  {$IFOPT D+} DbugIntf, {$ENDIF}
  SysUtils, Consts, TypInfo, GX_CPULabelAdd, GX_GEXperts, Forms;

{ TCPULabelList }

type
  { Class for saving the CPU labels.
    Typecast Object to Integer to get the address.
    Sorted on Address.
    Only use AddObject to add new items to the list.
    GetLabel is assigned to TDisassemblerView to return the label }

  TGetLabelEvent = procedure (Sender: TObject; Address: Integer; var RetLabel: string; var Result: Boolean) of object;

  TCPULabelList = class(TStringList)
  protected
    FFileName: string;
    FOrgGetLabel: TGetLabelEvent;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
  public
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    function FindAddress(Address: Integer; var Index: Integer): Boolean; virtual;
    procedure GetLabel(Sender: TObject; Address: Integer; var RetLabel: string; var Result: Boolean);
    procedure EditLabel(Sender: TObject);  // used by the CPU popup menu item
    procedure CPULabels(Sender: TObject);  // used by the CPU popup menu item
  end;

var
  CPULabelList: TCPULabelList;

function TCPULabelList.Add(const S: string): Integer;
begin
  Error(SSortedListError, 0);
  Result := 0;
end;

function TCPULabelList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if FindAddress(Integer(AObject), Result) then
    Error(SDuplicateString, 0);
  if S = '' then exit;
  inherited Insert(Result, S);
  inherited PutObject(Result, AObject);
  if fmCPULabels <> nil then
    fmCPULabels.OnAdd(Result);
end;

procedure TCPULabelList.Clear;
begin
  inherited Clear;
  if fmCPULabels <> nil then
    fmCPULabels.OnClear;
end;

procedure TCPULabelList.Delete(Index: Integer);
begin
  inherited Delete(index);
  if fmCPULabels <> nil then
    fmCPULabels.OnDelete(Index);
end;

procedure TCPULabelList.Exchange(Index1, Index2: Integer);
begin
  Error(SSortedListError, 0);
end;

procedure TCPULabelList.Insert(Index: Integer; const S: string);
begin
  Error(SSortedListError, 0);
end;

type
  PObject = ^TObject;

function TCPULabelList.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 6);
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    PObject(P)^ := GetObject(I);
    Inc(P, 4);
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

procedure TCPULabelList.SetTextStr(const Value: string);
var
  P, Start: PChar;
  S: string;
  AObject: TObject;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while PChar(Value) + Length(Value) - P >= 4 do
      begin
        AObject := PObject(P)^;
        inc(P, 4);
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        AddObject(S, AObject);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TCPULabelList.Put(Index: Integer; const S: string);
begin
  if S <> '' then
  begin
    inherited Put(Index, S);
    if fmCPULabels <> nil then
      fmCPULabels.OnChangeLabel(Index);
  end
  else
    // Remove an empty item
    Delete(Index);
end;

procedure TCPULabelList.PutObject(Index: Integer; AObject: TObject);
begin
  Error(SSortedListError, 0);
end;

function TCPULabelList.FindAddress(Address: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Integer(Objects[I]) - Address;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TCPULabelList.GetLabel(Sender: TObject; Address: Integer; var RetLabel: string; var Result: Boolean);
var
  I: Integer;
begin
  if FindAddress(Address, I) then
  begin
    // If the address if found return the string.
    RetLabel := Strings[I];
    Result := True;
  end
  else
    // Otherwise call the original OnGetLabel.
    if Assigned(FOrgGetLabel) then
      FOrgGetLabel(Sender, Address, RetLabel, Result);
end;

procedure TCPULabelList.EditLabel(Sender: TObject);
var
  Address: Integer;
  Index: Integer;
  NewLabel: string;
begin
  Sender := TComponent(Sender).Owner;
  Address := GetOrdProp(Sender, GetPropInfo(Sender.ClassInfo, 'SelectedAddress'));
  if CPULabelList.FindAddress(Address, Index) then
  begin
    NewLabel := InputBox('Edit label', 'New label:', CPULabelList[Index]);
    CPULabelList[Index] := NewLabel;
  end
  else
  begin
    NewLabel := InputBox('Edit label', 'New label:', '');
    CPULabelList.AddObject(NewLabel, TObject(Address));
  end;
end;

procedure TCPULabelList.CPULabels(Sender: TObject);
begin
  if fmCPULabels = nil then
    fmCPULabels := TfmCPULabels.Create(nil);
  IdeDockManager.ShowForm(fmCPULabels);
end;

{ TfmCPULabels }

procedure TfmCPULabels.OnAdd(Index: Integer);
begin
  with lvwLabels.Items.Insert(Index) do
  begin
    Data := pointer(CPULabelList.Objects[Index]);
    Caption := Format('$%p', [Data]);
    SubItems.Text := CPULabelList[Index];
  end;
end;

procedure TfmCPULabels.OnChangeLabel(Index: Integer);
begin
  lvwLabels.Items[Index].SubItems[0] := CPULabelList[Index];
end;

procedure TfmCPULabels.OnClear;
begin
  lvwLabels.Items.Clear;
end;

procedure TfmCPULabels.OnDelete(Index: Integer);
begin
  lvwLabels.Items.Delete(Index);
end;

procedure TfmCPULabels.Refresh(Sender: TObject);
const
  SLabel = '$%p'#1310'%s';
var
  I: Integer;
begin
  lvwLabels.Items.Clear;
  lvwLabels.AllocBy := CPULabelList.Count;
  with CPULabelList do
    for I := 0 to Count -1 do
      with lvwLabels.Items.Add do
      begin
        Data := pointer(Objects[I]);
        Caption := Format('$%p', [Data]);
        SubItems.Text := Strings[I];
      end;
end;

procedure TfmCPULabels.mitAddClick(Sender: TObject);
begin
  with TfmAddCPULabel.Create(nil) do
  try
    if ShowModal = mrOk then
      CPULabelList.AddObject(edtLabel.Text, TObject(Address));
  finally
    Free;
  end;
end;

procedure TfmCPULabels.FormCreate(Sender: TObject);
begin
  inherited;
  Refresh(Sender);
end;

procedure TfmCPULabels.lvwLabelsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  mitDelete.Enabled := Selected;
  mitEditLabel.Enabled := Selected;
end;

procedure TfmCPULabels.mitDeleteClick(Sender: TObject);
begin
  CPULabelList.Delete(lvwLabels.Selected.Index);
end;

procedure TfmCPULabels.mitClearClick(Sender: TObject);
begin
  CPULabelList.Clear;
end;

procedure TfmCPULabels.mitEditLabelClick(Sender: TObject);
var
  NewLabel: string;
begin
  with lvwLabels.Selected do
  begin
    NewLabel := InputBox('Edit label', 'New label:', SubItems[0]);
    CPULabelList[Index] := NewLabel;
  end;
end;

function SetEvent(AComponent: TComponent; EventName: string; Method: TMethod): TMethod;
begin
  Result := GetMethodProp(AComponent, GetPropInfo(AComponent.ClassInfo, EventName));
  SetMethodProp(AComponent, GetPropInfo(AComponent.ClassInfo, EventName), Method);
end;

{ TEditMenuItem }

type
  TEditMenuItem = class(TMenuItem)
  public
    destructor Destroy; override;
  end;

destructor TEditMenuItem.Destroy;
begin
  inherited Destroy;
  EditMenuItem := nil;
end;

{ TCPULabelsMenuItem }

type
  TCPULabelsMenuItem = class(TMenuItem)
  public
    destructor Destroy; override;
  end;

destructor TCPULabelsMenuItem.Destroy;
begin
  inherited Destroy;
  CPULabelsMenuItem := nil;
end;

procedure AttachGetLabel(DisassemblerView: TComponent);
var
  Method: TGetLabelEvent;
begin
  // Create the MenuItem's and insert them into the DisAssembler Popup
  EditMenuItem := TEditMenuItem.Create(DisassemblerView);
  with EditMenuItem do
  begin
    Caption := '&Edit label';
    OnClick := CPULabelList.EditLabel;
  end;
  CPULabelsMenuItem := TCPULabelsMenuItem.Create(DisassemblerView);
  with CPULabelsMenuItem do
  begin
    Caption := '&CPU labels';
    OnClick := CPULabelList.CPULabels;
  end;
  with TForm(DisassemblerView).PopupMenu.Items do
  begin
    Add(EditMenuItem);
    Add(CPULabelsMenuItem);
  end;
  // Set the OnGetLabel event
  Method := CPULabelList.GetLabel;
  CPULabelList.FOrgGetLabel := TGetLabelEvent(SetEvent(DisassemblerView, 'OnGetLabel', TMethod(Method)));
end;

procedure UnAttachGetLabel;
var
  Comp: TComponent;
begin
  Comp := Application.FindComponent('DisassemblyView');
  if Comp <> nil then
  begin
    Comp := Comp.FindComponent('DisassemblerView1');
    if Comp <> nil then
      SetEvent(Comp, 'OnGetLabel', TMethod(CPULabelList.FOrgGetLabel));
  end;
  EditMenuItem.Free;
  CPULabelsMenuItem.Free;
end;

{ TAttacher }

type
  TAttacher = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  end;

  TDisAssAttacher = class(TComponent)
  private
    OrgOnCreate: TNotifyEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DisAssCreate(Sender: TObject);
  end;

var
  Attacher: TComponent;

{ TAttacher }

procedure TAttacher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent.ClassName = 'TDisassemblyView') and (Operation = opInsert) then
    TDisAssAttacher.Create(AComponent);
end;

destructor TAttacher.Destroy;
begin
  inherited Destroy;
  Attacher := nil;
end;

{ TDisAttacher }

procedure TDisAssAttacher.Notification(AComponent: TComponent; Operation: TOperation);
var
  Method: TNotifyEvent;
begin
  inherited Notification(AComponent, Operation);
  // If TDisassemblerView inserted ReWrite OnCreate
  if (AComponent.ClassName = 'TDisassemblerView') and (Operation = opInsert) then
  begin
    Method := DisAssCreate;
    OrgOnCreate := TNotifyEvent(SetEvent(AComponent.Owner, 'OnCreate', TMethod(Method)));
  end;
end;

procedure TDisAssAttacher.DisAssCreate(Sender: TObject);
begin
  // Inherited OnCreate
  if Assigned(OrgOnCreate) then
    OrgOnCreate(Sender);
  AttachGetLabel(TComponent(Sender).FindComponent('DisassemblerView1'));
  // Cleanup yourself
  Free;
  Attacher.Free;
end;

{ TCPULabelExpert }

constructor TCPULabelExpert.Create;
begin
  inherited Create;
  HasMenuItem := True;
  HasConfigOptions := False
end;

destructor TCPULabelExpert.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TCPULabelExpert.Click(Sender: TObject);
begin
  Assert(Active);
  if fmCPULabels = nil then
    fmCPULabels := TfmCPULabels.Create(nil);
  IdeDockManager.ShowForm(fmCPULabels);
end;

procedure TCPULabelExpert.SetActive(New: Boolean);
var
  Comp: TComponent;
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    begin
      CPULabelList := TCPULabelList.Create;
      IdeDockManager.RegisterDockableForm(TfmCPULabels, fmCPULabels, 'fmCPULabels');
      Comp := Application.FindComponent('DisassemblyView');
      if Comp = nil then
        Attacher := TAttacher.Create(Application)
      else
        AttachGetLabel(Comp.FindComponent('DisassemblerView1'));
    end
    else
    begin
      UnAttachGetLabel;
      Attacher.Free;
      fmCPULabels.Free;
      CPULabelList.Free;
      IdeDockManager.UnRegisterDockableForm(fmCPULabels, 'fmCPULabels');
    end;
  end;
end;

function TCPULabelExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'CPU Labels';
begin
  Result := SDisplayName;
end;

function TCPULabelExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TCPULabelExpert.GetMenuName: string;
begin
  Result := 'GX_CPULabels';
end;

function TCPULabelExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&CPU Labels';
begin
  Result := SMenuCaption;
end;

function TCPULabelExpert.GetName: string;
begin
  Result := 'CPULabels_Expert';
end;

function TCPULabelExpert.IconFileName: string;
begin
  Result := 'CPULabels';
end;

procedure TfmCPULabels.FormDestroy(Sender: TObject);
begin
  inherited;
  fmCPULabels := nil;
end;

procedure TfmCPULabels.mitOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    CPULabelList.FFileName := '';
    CPULabelList.LoadFromFile(dlgOpen.FileName);
    CPULabelList.FFileName := dlgOpen.FileName;
  end;
end;

procedure TfmCPULabels.mitSaveClick(Sender: TObject);
begin
  if CPULabelList.FFileName <> '' then
    CPULabelList.SaveToFile(CPULabelList.FFileName)
  else
    mitSaveAsClick(Sender);
end;

procedure TfmCPULabels.mitSaveAsClick(Sender: TObject);
begin
   dlgSave.FileName := CPULabelList.FFileName;
   if dlgSave.Execute then
   begin
     CPULabelList.SaveToFile(dlgSave.FileName);
     CPULabelList.FFileName := dlgSave.FileName;
   end;
end;

initialization
  RegisterGX_Expert(TCPULabelExpert);

end.

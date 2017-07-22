unit RplWizInfo;

{$I GX_CondDefine.inc}

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  Component Replace Wizard.

  Temporarily store component and property information
  for doing search & replace of components. Replacing
  a component means deleting the old one and inserting the
  new one. This might involve deleting all the children
  of the old component, which might be a lot. Hence the need
  for a comprehensive way to store an arbitrary tree of components.

  A TCompInfo object represents a single component. It can have
  children, which are stored in a TCompList, which is just a list
  of TCompInfo objects. Each TCompInfo object also has a list of
  properties, which is a TPropList object. A property list contains
  a list of TPropInfo objects. Each TPropInfo object represents
  a single property. The property value might be a string, Variant,
  etc., so the TPropInfo object must be able to handle anything.

  Each object knows how to create itself from a TIComponentInterface,
  to capture an existing component's state. When replacing a component,
  TCompInfo creates a TCompInfo object for the component, its properties
  and its child components. Then it deletes the component interface,
  and creates a new one, with the new type. It then asks all the properties
  to apply themselves to the new component. Only those with matching
  names and types apply themselves. The others are silently ignored.
  (This avoids nagging the user about minor problems, e.g., converting
  a TBitBtn to a TButton, but introduces the possibility that the user might
  make a gross error, e.g., replacing a TBitBtn with a TBlobField. There is
  no undo, so this is potentially a problem, but it is better to assume
  the user is intelligent than to assume the user is an idiot.) TCompInfo
  then recreates all the child objects.

  To replace components, descend the component hierarchy once. Any
  component that is to be replaced, replace it and recreates its children.
  When recreating children, any old child that was to be replace is replaced.
  Thus, the recursive descent bifurcates the first time it encounters
  a component to be replaced. The Search routine descends the tree looking
  for a component to replace. The Replace routine does the replacement.
}

interface

uses SysUtils, Classes, EditIntf, OwnerList;

{ Represent a component as a type name and a list of
  properties. A property has a type and a value. }
type
  TPropertyValue = record
    { Delphi does not let you store a Variant or a string in a variant
      record, so use a character array that is big enough to hold
      a Variant. Delphi stores a string as a pointer, so use Pointer
      as the type. }
    case TPropertyType of
    ptInteger, ptChar, ptEnumeration, ptSet, ptWChar:
              (IntegerValue: LongInt;);
    ptFloat:  (FloatValue: Extended;);
    ptMethod: (MethodValue: TMethod;);
    ptClass, ptString, ptLString, ptLWString: (PtrValue: Pointer;);
    ptVariant: (VariantValue: array[1..SizeOf(Variant)] of Byte;);
  end;

  TPropInfo = class
  private
    fPropType: TPropertyType;
    fPropValue: TPropertyValue;
    fPropName: string;
  public
    constructor Create(Index: Integer; CompIntf: TIComponentInterface);
    procedure SetComponent(CompIntf: TIComponentInterface);
    function GetIntValue(Default: Integer): Integer;
    function GetStrValue(Default: string): string;
    property Name: string read fPropName;
    property PropType: TPropertyType read fPropType;
    property PropValue: TPropertyValue read fPropValue;
  end;

{$WARNINGS OFF}
  TPropList = class(TOwnerList)
  private
    fStream: TMemoryStream;
    fLeft: Integer;
    fTop: Integer;
    function GetPropInfo(Index: Integer): TPropInfo;
  protected
    function GetIntegerPropByName(const PropName: string): LongInt;
    property Stream: TMemoryStream read fStream;
  public
    // We deliberately hide the base virtual method
    constructor Create(CompIntf: TIComponentInterface); {$IFDEF GX_VER120_up} reintroduce; {$ENDIF GX_VER120_up}
    destructor Destroy; override;
    procedure SetComponent(CompIntf: TIComponentInterface);
    function GetName: string;
    function GetLeft: LongInt;
    function GetTop: LongInt;
    function GetWidth: LongInt;
    function GetHeight: LongInt;
    property Prop[Index: Integer]: TPropInfo read GetPropInfo; default;
  end;
{$WARNINGS ON}


{$WARNINGS OFF}
  TCompInfo = class;
  TCompList = class(TOwnerList)
  private
    function GetCompInfo(Index: Integer): TCompInfo;
  protected
    function FindComponent(CompIntf: TIComponentInterface): TCompInfo;
    procedure GetMatchingComponents(List: TStrings; OldType: string);
    function Replace(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
    function Search(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
  public
    // We deliberately hide the base virtual method
    constructor Create(CompIntf: TIComponentInterface); {$IFDEF GX_VER120_up} reintroduce; {$ENDIF GX_VER120_up}
    property Children[Index: Integer]: TCompInfo read GetCompInfo; default;
  end;
{$WARNINGS ON}

  TCompInfo = class
  private
    fTypeName: string;
    fProperties: TPropList;
    fChildren: TCompList;
    fInterface: TIComponentInterface;
    procedure SetInterface(NewIntf: TIComponentInterface);
  protected
    function FindComponent(CompIntf: TIComponentInterface): TCompInfo;
    function Search(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
    function Replace(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
    procedure CreateComponent(Parent: TCompInfo; FormIntf: TIFormInterface; NewType: string);
    procedure GetMatchingComponents(List: TStrings; OldType: string);
  public
    constructor Create(CompIntf: TIComponentInterface);
    destructor Destroy; override;
    function GetName: string;
    property TypeName: string read fTypeName write fTypeName;
    property Children: TCompList read fChildren;
    property Properties: TPropList read fProperties;
    property ComponentInterface: TIComponentInterface read fInterface write SetInterface;
  end;

  TFormInfo = class
  private
    fCompInfo: TCompInfo;
    fInterface: TIFormInterface;
  protected
    function FindComponent(CompIntf: TIComponentInterface): TCompInfo;
  public
    constructor Create(FormIntf: TIFormInterface);
    destructor Destroy; override;
    procedure GetMatchingComponents(List: TStrings; OldType: string);
    procedure GetSelectedComponents(List: TStrings; OldType: string);
    function ReplaceComponents(List: TStrings; NewType: string): Integer;
    property Component: TCompInfo read fCompInfo;
    property FormInterface: TIFormInterface read fInterface;
  end;

implementation

uses Windows, Controls;

const
  BufferSize = 8192;

resourcestring
  sCannotCreate = 'Cannot create a new %s';

{ TPropInfo }
constructor TPropInfo.Create(Index: Integer; CompIntf: TIComponentInterface);
begin
  inherited Create;
  fPropName := CompIntf.GetPropName(Index);
  fPropType := CompIntf.GetPropType(Index);
  CompIntf.GetPropValue(Index, fPropValue);
end;

{ Set the property value in the component, but only if it has a property
  of the same name and type. }
procedure TPropInfo.SetComponent(CompIntf: TIComponentInterface);
var
  NewType: TPropertyType;
begin
  NewType := CompIntf.GetPropTypeByName(Name);
  if NewType = PropType then
    CompIntf.SetPropByName(Name, fPropValue);
end;

function TPropInfo.GetIntValue(Default: Integer): Integer;
begin
  if PropType in [ptInteger, ptChar, ptEnumeration, ptSet, ptWChar] then
    Result := PropValue.IntegerValue
  else
    Result := Default
end;

function TPropInfo.GetStrValue(Default: string): string;
begin
  if PropType in [ptString, ptLString, ptLWString] then
    Result := string(PropValue.PtrValue)
  else
    Result := Default
end;

type
  TExposePersistent = class(TPersistent)
  public
    procedure DefineProperties(Filer: TFiler); override;
  end;

procedure TExposePersistent.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer)
end;

{ TPropList }
constructor TPropList.Create(CompIntf: TIComponentInterface);
var
  I: Integer;
  Comp: TExposePersistent;
  Writer: TWriter;
begin
  inherited Create;
  fLeft := -1;
  fTop := -1;
  for I := 0 to CompIntf.GetPropCount-1 do
    Add(TPropInfo.Create(I, CompIntf));
  Comp := TExposePersistent(CompIntf.GetComponentHandle);
  fStream := TMemoryStream.Create;
  Writer := TWriter.Create(Stream, BufferSize);
  try
    Comp.DefineProperties(Writer);
  finally
    Writer.Free;
  end;
  // Added May, 2000 by EB to support Top/Left for non-visual TComponents
  if ((Comp.InheritsFrom(TComponent)) and (not Comp.InheritsFrom(TControl))) then
  begin
    fLeft := LongRec(TComponent(Comp).DesignInfo).Lo;
    fTop := LongRec(TComponent(Comp).DesignInfo).Hi;
  end;
end;

destructor TPropList.Destroy;
begin
  fStream.Free;
  inherited Destroy;
end;

function TPropList.GetPropInfo(Index: Integer): TPropInfo;
begin
  Result := TPropInfo(Items[Index])
end;

procedure TPropList.SetComponent(CompIntf: TIComponentInterface);
var
  I: Integer;
  Reader: TReader;
  Comp: TExposePersistent;
begin
  for I := 0 to Count-1 do
    Prop[I].SetComponent(CompIntf);
  Comp := TExposePersistent(CompIntf.GetComponentHandle);
  Stream.Position := 0;
  Reader := TReader.Create(Stream, BufferSize);
  try
    Comp.DefineProperties(Reader);
  finally
    Reader.Free;
  end;
end;

{ Lookup a property by name, and if it is an integer, then
  return the integer value. Otherwise, return zero. }
function TPropList.GetIntegerPropByName(const PropName: string): LongInt;
var
  I: Integer;
  PropInfo: TPropInfo;
begin
  Result := -1;  { -1 tells Delphi to use the default (sort of) }
  for I := 0 to Count-1 do
  begin
    PropInfo := Prop[I];
    if CompareText(PropInfo.Name, PropName) = 0 then
    begin
      Result := PropInfo.GetIntValue(-1);
      Break;
    end;
  end;
end;

function TPropList.GetLeft: LongInt;
begin
  Result := GetIntegerPropByName('Left')
end;

function TPropList.GetTop: LongInt;
begin
  Result := GetIntegerPropByName('Top')
end;

function TPropList.GetWidth: LongInt;
begin
  Result := GetIntegerPropByName('Width')
end;

function TPropList.GetHeight: LongInt;
begin
  Result := GetIntegerPropByName('Height')
end;

{ Get the value of the Name property. }
function TPropList.GetName: string;
var
  I: Integer;
  PropInfo: TPropInfo;
begin
  Result := '';
  for I := 0 to Count-1 do
  begin
    PropInfo := Prop[I];
    if CompareText(PropInfo.Name, 'Name') = 0 then
    begin
      Result := PropInfo.GetStrValue('');
      Break;
    end;
  end;
end;


{ TCompList }

function EnumChildren(CompList: Pointer; CompIntf: TIComponentInterface): Boolean; stdcall;
begin
  try
    TCompList(CompList).Add(TCompInfo.Create(CompIntf));
    Result := True;
  finally
    CompIntf.Free;
  end;
end;

constructor TCompList.Create(CompIntf: TIComponentInterface);
begin
  inherited Create;
  CompIntf.GetChildren(Self, EnumChildren);
end;

function TCompList.GetCompInfo(Index: Integer): TCompInfo;
begin
  Result := TCompInfo(Items[Index])
end;

{ Look for a component represented by CompIntf. Return nil for not found. }
function TCompList.FindComponent(CompIntf: TIComponentInterface): TCompInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    Result := Children[I].FindComponent(CompIntf);
    if Result <> nil then
      Exit;
  end;
end;

function TCompList.Replace(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
    Inc(Result, Children[I].Replace(Parent, FormIntf, List, NewType));
end;

function TCompList.Search(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count-1 do
    Inc(Result, Children[I].Search(Parent, FormIntf, List, NewType));
end;

{ Add to List all components whose type is OldType. }
procedure TCompList.GetMatchingComponents(List: TStrings; OldType: string);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Children[I].GetMatchingComponents(List, OldType);
end;

{ TCompInfo }
constructor TCompInfo.Create(CompIntf: TIComponentInterface);
begin
  inherited Create;
  fChildren := TCompList.Create(CompIntf);
  fProperties := TPropList.Create(CompIntf);
  fTypeName := CompIntf.GetComponentType;
  CompIntf.AddRef;
  fInterface := CompIntf;
end;

destructor TCompInfo.Destroy;
begin
  Children.Free;
  Properties.Free;
  ComponentInterface.Free;
  inherited Destroy;
end;

{ Change the component interface reference. Take care now to free
  the old interface until it is safe to do so. }
procedure TCompInfo.SetInterface(NewIntf: TIComponentInterface);
var
  OldIntf: TIComponentInterface;
begin
  OldIntf := fInterface;
  if NewIntf <> nil then
    NewIntf.AddRef;
  fInterface := NewIntf;
  OldIntf.Free;
end;

{ Create a new component of type NewType, duplicating the old component's
  properties. }
procedure TCompInfo.CreateComponent(Parent: TCompInfo; FormIntf: TIFormInterface; NewType: string);
var
  NewIntf: TIComponentInterface;
  Comp: TComponent;
begin
  with Properties do
    NewIntf := FormIntf.CreateComponent(Parent.ComponentInterface, NewType, GetLeft, GetTop, GetWidth, GetHeight);
  if NewIntf = nil then
    raise Exception.CreateFmt(sCannotCreate, [NewType]);

  try
    Properties.SetComponent(NewIntf);
    ComponentInterface := NewIntf;
  finally
    NewIntf.Free;
  end;
  // Added May, 2000 by EB to support Top/Left for non-visual TComponents
  if Properties.fLeft <> -1 then
  begin
    Comp := TComponent(ComponentInterface.GetComponentHandle);
    Comp.DesignInfo := MakeLong(Properties.fLeft, Properties.fTop);
  end;
end;

{ Return the component's name, that is, the value of its Name property. }
function TCompInfo.GetName: string;
begin
  Result := Properties.GetName;
end;

{ Search for the component whose interface is CompIntf. Return nil
  for not found. To search for an interface, compare component handles,
  which are unique among all existing components. }
function TCompInfo.FindComponent(CompIntf: TIComponentInterface): TCompInfo;
begin
  if CompIntf.GetComponentHandle = ComponentInterface.GetComponentHandle then
    Result := Self
  else
    Result := Children.FindComponent(CompIntf);
end;

{ Find all components whose type is OldType; add the names of the
  matching components to List. }
procedure TCompInfo.GetMatchingComponents(List: TStrings; OldType: string);
begin
  Children.GetMatchingComponents(List, OldType);
  if CompareText(TypeName, OldType) = 0 then
    List.Add(GetName);
end;

{ Create a component and its children. If the component is named in List,
  use NewType for its type. }
function TCompInfo.Replace(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
var
  Index: Integer;
  Count: Integer;
begin
  Index := List.IndexOf(GetName);
  if Index < 0 then
  begin
    { Not in list, so use old type name. }
    CreateComponent(Parent, FormIntf, TypeName);
    Count := 0;
  end
  else
  begin
    { Create component with new type name. }
    List.Delete(Index);
    CreateComponent(Parent, FormIntf, NewType);
    Count := 1;
  end;

  { Recursively recreate child components. }
  Result := Children.Replace(Self, FormIntf, List, NewType) + Count;
end;

{ If this component's name is in List, delete it and recreate the component
  using type, NewType, and recursively recreate its children. If any children
  are in List, recreate them with the NewType. If this component is not in
  the list, search its children. }
function TCompInfo.Search(Parent: TCompInfo; FormIntf: TIFormInterface; List: TStrings; NewType: string): Integer;
var
  Index: Integer;
begin
  Index := List.IndexOf(GetName);
  if Index < 0 then
    { Not in List, so search the children. }
    Result := Children.Search(Self, FormIntf, List, NewType)
  else
  begin
    { Found a component to replace. Delete the old one, create a new one,
      and recreate all of its children. }
    List.Delete(Index);
    ComponentInterface.Delete;
    CreateComponent(Parent, FormIntf, NewType);
    Result := Children.Replace(Self, FormIntf, List, NewType) + 1;
  end
end;

{ TFormInfo }
constructor TFormInfo.Create(FormIntf: TIFormInterface);
var
  CompIntf: TIComponentInterface;
begin
  inherited Create;
  FormIntf.AddRef;
  fInterface := FormIntf;
  CompIntf := FormInterface.GetFormComponent;
  try
    fCompInfo := TCompInfo.Create(CompIntf);
  finally
    CompIntf.Free;
  end;
end;

destructor TFormInfo.Destroy;
begin
  fCompInfo.Free;
  fInterface.Free;
  inherited Destroy;
end;

{ Look up the component represented by CompIntf. }
function TFormInfo.FindComponent(CompIntf: TIComponentInterface): TCompInfo;
begin
  Result := Component.FindComponent(CompIntf);
end;

{ Fill List with the names of all components whose type is OldType. }
procedure TFormInfo.GetMatchingComponents(List: TStrings; OldType: string);
begin
  Component.GetMatchingComponents(List, OldType);
end;

{ Examine all selected components, and add to the list any components
  whose type is OldType. Also search the children of selected components. }
procedure TFormInfo.GetSelectedComponents(List: TStrings; OldType: string);
var
  I: Integer;
  Info: TCompInfo;
  CompIntf: TIComponentInterface;
begin
  for I := 0 to FormInterface.GetSelCount-1 do
  begin
    CompIntf := FormInterface.GetSelComponent(I);
    try
      Info := FindComponent(CompIntf);
      Info.GetMatchingComponents(List, OldType);
    finally
      CompIntf.Free;
    end;
  end;
end;

{ Replace all the components whose names are in List. Replace them
  with new components of type NewType. Return the number of
  replacements performed. }
function TFormInfo.ReplaceComponents(List: TStrings; NewType: string): Integer;
begin
  { Start by selecting the form, so when parent = nil, the new components
    will be children of the form, not the current selection. }
  Component.ComponentInterface.Select;
  Result := Component.Search(nil, FormInterface, List, NewType);
  // Added May, 2000 by EB to refresh Top/Left for non-visual TComponents
  FormInterface.MarkModified;
end;

end.

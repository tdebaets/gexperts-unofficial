unit GX_RTTI;

//********************************************************************************
//* This unit was orginally from Marco Cantu's book Delphi Developers Handbook.  *
//* It has been renamed here to avoid conflict.                                  *
//********************************************************************************

interface

uses
  TypInfo, Classes;

// *** RTTI ***

// show RTTI info in a dialog box
procedure ShowRttiDetail (pti: PTypeInfo);
// show RTTI info (generic)
procedure ShowRTTI (pti: PTypeInfo; sList: TStrings);
// show RTTI information for method pointers (from Chapter 4)
procedure ShowMethod (pti: PTypeInfo; sList: TStrings);
// show RTTI information for class type (from Chapter 4)
procedure ShowClass (pti: PTypeInfo; sList: TStrings);
// show RTTI information for ordinal types (from Chapter 4)
procedure ShowOrdinal (pti: PTypeInfo; sList: TStrings);
// list enumerated values (from Chapter 4)
procedure ListEnum (pti: PTypeInfo; sList: TStrings; ShowIndex: Boolean);

// *** property values ***

// return the property value as a string
function GetPropValAsString (Obj: TObject; PropInfo: PPropInfo): string;
// turn the value of a set into a string
function SetToString (Value: Cardinal; pti: PTypeInfo): string;
// sort properties: extracted from TypInfo.pas
procedure SortPropList(PropList: PPropList; PropCount: Integer); assembler;

// *** other ***
function GetPropertyType(Obj: TObject; const PropName: String): TTypeKind;
procedure GetEnumList(Obj: TObject; const PropName: String;SList: TStrings);
// test bit: extracted from Chapter 1 of DDH
function IsBitOn (Value: Integer; Bit: Byte): Boolean;

implementation

uses
  SysUtils, Graphics, Controls, Forms, StdCtrls, Buttons;

// redeclaration of RTTI type
type
  TParamData = record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
    // beware: string length varies!!!
  end;
  PParamData = ^TParamData;

// show RTTI information for method pointers
procedure ShowMethod (pti: PTypeInfo; sList: TStrings);
var
  ptd: PTypeData;
  pParam: PParamData;
  nParam: Integer;
  Line: string;
  pTypeString, pReturnString: ^ShortString;
begin
  // protect against misuse
  if pti^.Kind <> tkMethod then
    raise Exception.Create ('Invalid type information');

  // get a pointer to the TTypeData structure
  ptd := GetTypeData (pti);

  // 1: access the TTypeInfo structure
  sList.Add ('Type Name: ' + pti^.Name);
  sList.Add ('Type Kind: ' + GetEnumName (
    TypeInfo (TTypeKind),
    Integer (pti^.Kind)));

  // 2: access the TTypeData structure
  sList.Add ('Method Kind: ' + GetEnumName (
    TypeInfo (TMethodKind),
    Integer (ptd^.MethodKind)));
  sList.Add ('Number of parameter: ' +
    IntToStr (ptd^.ParamCount));

  // 3: access to the ParamList
  // get the initial pointer and
  // reset the parameters counter
  pParam := PParamData (@(ptd^.ParamList));
  nParam := 1;
  // loop until all parameters are done
  while nParam <= ptd^.ParamCount do
  begin
    // read the information
    Line := 'Param ' + IntToStr (nParam) + ' > ';
    // add type of parameter
    if pfVar in pParam^.Flags then
      Line := Line + 'var ';
    if pfConst in pParam^.Flags then
      Line := Line + 'const ';
    if pfOut in pParam^.Flags then
      Line := Line + 'out ';
    // get the parameter name
    Line := Line + pParam^.ParamName + ': ';
    // one more type of parameter
    if pfArray in pParam^.Flags then
      Line := Line + ' array of ';
    // the type name string must be located...
    // moving a pointer past the params and
    // the string (including its size byte)
    pTypeString := Pointer (Integer (pParam) +
      sizeof (TParamFlags) +
      Length (pParam^.ParamName) + 1);
    // add the type name
    Line := Line + pTypeString^;
    // finally, output the string
    sList.Add (Line);
    // move the pointer to the next structure,
    // past the two strings (including size byte)
    pParam := PParamData (Integer (pParam) +
      sizeof (TParamFlags) +
      Length (pParam^.ParamName) + 1 +
      Length (pTypeString^) + 1);
    // increase the parameters counter
    Inc (nParam);
  end;
  // show the return type if a function
  if ptd^.MethodKind = mkFunction then
  begin
    // at the end, instead of a param data,
    // there is the return string
    pReturnString := Pointer (pParam);
    sList.Add ('Returns > ' + pReturnString^);
  end;
end;

// show RTTI information for class type
procedure ShowClass (pti: PTypeInfo; sList: TStrings);
var
  ptd: PTypeData;
  ppi: PPropInfo;
  pProps: PPropList;
  nProps, I: Integer;
  ParentClass: TClass;
begin
  // protect against misuse
  if pti.Kind <> tkClass then
    raise Exception.Create ('Invalid type information');

  // get a pointer to the TTypeData structure
  ptd := GetTypeData (pti);

  // access the TTypeInfo structure
  sList.Add ('Type Name: ' + pti.Name);
  sList.Add ('Type Kind: ' + GetEnumName (
    TypeInfo (TTypeKind),
    Integer (pti.Kind)));

  // access the TTypeData structure
  {omitted: the same information of pti^.Name...
  sList.Add ('ClassType: ' + ptd^.ClassType.ClassName);}
  sList.Add ('Size: ' + IntToStr (
    ptd.ClassType.InstanceSize) + ' bytes');
  sList.Add ('Defined in: ' + ptd.UnitName + '.pas');

  // add the list of parent classes (if any)
  ParentClass := ptd.ClassType.ClassParent;
  if ParentClass <> nil then
  begin
    sList.Add ('');
    sList.Add ('=== Parent classes ===');
    while ParentClass <> nil do
    begin
      sList.Add (ParentClass.ClassName);
      ParentClass := ParentClass.ClassParent;
    end;
  end;

  // add the list of properties (if any)
  nProps := ptd.PropCount;
  if nProps > 0 then
  begin
    // format the initial output
    sList.Add ('');
    sList.Add ('=== Properties (' +
      IntToStr (nProps) + ') ===');
    // allocate the required memory
    GetMem (pProps, sizeof (PPropInfo) * nProps);
    // protect the memory allocation
    try
      // fill the TPropList structure
      // pointed to by pProps
      GetPropInfos(pti, pProps);
      // sort the properties
      SortPropList(pProps, nProps);
      // show name and data type of each property
      for I := 0 to nProps - 1 do
      begin
        ppi := pProps [I];
        sList.Add (ppi.Name + ': ' +
          ppi.PropType^.Name);
      end;
    finally
      // free the allocated memmory
      FreeMem (pProps, sizeof (PPropInfo) * nProps);
    end;
  end;
end;

// show RTTI information for ordinal types
procedure ShowOrdinal (pti: PTypeInfo; sList: TStrings);
var
  ptd: PTypeData;
begin
  // protect against misuse
  if not (pti^.Kind in [tkInteger, tkChar,
      tkEnumeration, tkSet, tkWChar]) then
    raise Exception.Create ('Invalid type information');

  // get a pointer to the TTypeData structure
  ptd := GetTypeData (pti);

  // access the TTypeInfo structure
  sList.Add ('Type Name: ' + pti^.Name);
  sList.Add ('Type Kind: ' + GetEnumName (
    TypeInfo (TTypeKind),
    Integer (pti^.Kind)));

  // access the TTypeData structure
  sList.Add ('Implement: ' + GetEnumName (
    TypeInfo (TOrdType),
    Integer (ptd^.OrdType)));

  // a set has no min and max
  if pti^.Kind <> tkSet then
  begin
    sList.Add ('Min Value: ' + IntToStr (ptd^.MinValue));
    sList.Add ('Max Value: ' + IntToStr (ptd^.MaxValue));
  end;

  // add the enumeration base type
  // and the list of the values
  if pti^.Kind = tkEnumeration then
  begin
    sList.Add ('Base Type: ' + (ptd^.BaseType)^.Name);
    sList.Add ('');
    sList.Add ('Values...');
    ListEnum (pti, sList, True);
  end;

  // show RRTI info about set base type
  if  pti^.Kind = tkSet then
  begin
    sList.Add ('');
    sList.Add ('Set base type information...');
    ShowOrdinal (ptd^.CompType^, sList);
  end;
end;

// list enumerated values
procedure ListEnum (pti: PTypeInfo;
  sList: TStrings; ShowIndex: Boolean);
var
  I: Integer;
begin
  with GetTypeData(pti)^ do
    for I := MinValue to MaxValue do
      if ShowIndex then
        sList.Add ('  ' + IntToStr (I) + '. ' +
         GetEnumName (pti, I))
      else
        sList.Add (GetEnumName (pti, I));
end;

// generic procedure, calling the other ones
procedure ShowRTTI (pti: PTypeInfo; sList: TStrings);
begin
  case pti^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      ShowOrdinal (pti, sList);
    tkMethod:
      ShowMethod (pti, sList);
    tkClass:
      Showclass (pti, sList);
    tkString, tkLString:
    begin
      sList.Add ('Type Name: ' + pti^.Name);
      sList.Add ('Type Kind: ' + GetEnumName (
        TypeInfo (TTypeKind), Integer (pti^.Kind)));
    end
    else
      sList.Add ('Undefined type information');
  end;
end;

// show the RTTI information inside a modal dialog box
procedure ShowRttiDetail (pti: PTypeInfo);
var
  Form: TForm;
begin
  Form := TForm.Create (Application);
  try
    Form.Width := 250;
    Form.Height := 300;
    // middle of the screen
    Form.Left := Screen.Width div 2 - 125;
    Form.Top := Screen.Height div 2 - 150;
    Form.Caption := 'RTTI Details for ' + pti.Name;
    Form.BorderStyle := bsDialog;
    with TMemo.Create (Form) do
    begin
      Parent := Form;
      Width := Form.ClientWidth;
      Height := Form.ClientHeight - 35;
      ReadOnly := True;
      Color := clBtnFace;
      ShowRTTI (pti, Lines);
    end;
    with TBitBtn.Create (Form) do
    begin
      Parent := Form;
      Left := Form.ClientWidth div 3;
      Width := Form.ClientWidth div 3;
      Top := Form.ClientHeight - 32;
      Height := 30;
      Kind := bkOK;
    end;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

// support function: get the form (or data module)
// owning the component
function GetOwnerForm (Comp: TComponent): TComponent;
begin
  while not (Comp is TForm) and
      not (Comp is TDataModule) do
    Comp := Comp.Owner;
  Result := Comp;
end;

// from the Bits1 example (Chapter 1)
function IsBitOn (Value: Integer; Bit: Byte): Boolean;
begin
  Result := (Value and (1 shl Bit)) <> 0;
end;

// support function: convert set value
// into a string as in the Object Inspector

function SetToString (Value: Cardinal;
  pti: PTypeInfo): string;
var
  Res: String;  // result
  BaseType: PTypeInfo;
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  // open the expression
  Res := '[';
  // get the type of the enumeration
  // the set is based onto
  BaseType := GetTypeData(pti).CompType^;
  // for each possible value
  for I := GetTypeData (BaseType).MinValue
      to GetTypeData (BaseType).MaxValue do
    // if the bit I (computed as 1 shl I) is set,
    // then the corresponding element is in the set
    // (the and is a bitwise and, not a boolean operation)
    if IsBitOn (Value, I) then
    begin
      // add the name of the element
      Res := Res + GetEnumName (BaseType, I) + ', ';
      Found := True;
    end;
  if Found then
    // remove the final comma and space (2 chars)
    Res := Copy (Res, 1, Length (Res) - 2);
  // close the expression
  Result := Res + ']';
end;

// return the property value as a string
function GetPropValAsString (Obj: TObject;
  PropInfo: PPropInfo): string;
var
  Pt: Pointer;
  Word: Cardinal;
begin
  case PropInfo.PropType^.Kind of

    tkUnknown:
      Result := 'Unknown type';

    tkChar:
    begin
      Word := GetOrdProp (Obj, PropInfo);
      if Word > 32 then
        Result := Char (Word)
      else
        Result := '#' + IntToStr (Word);
    end;

    tkWChar:
    begin
      Word := GetOrdProp (Obj, PropInfo);
      if Word > 32 then
        Result := WideChar (Word)
      else
        Result := '#' + IntToStr (Word);
    end;


    tkInteger:
      if PropInfo.PropType^.Name = 'TColor' then
        Result := ColorToString (GetOrdProp (Obj, PropInfo))
      else if PropInfo.PropType^.Name = 'TCursor' then
        Result := CursorToString (GetOrdProp (Obj, PropInfo))
      else
        Result := Format ('%d', [GetOrdProp (Obj, PropInfo)]);

    tkEnumeration:
      Result := GetEnumName (PropInfo.PropType^,
        GetOrdProp (Obj, PropInfo));

    tkFloat:
      Result := FloatToStr (GetFloatProp (Obj, PropInfo));

    tkString, tkLString:
      Result := GetStrProp (Obj, PropInfo);

    tkSet:
      Result := SetToString (GetOrdProp (Obj, PropInfo),
        PropInfo.PropType^);

    tkClass:
    begin
      Pt := Pointer (GetOrdProp (Obj, PropInfo));
      if Pt = nil then
        Result := '(None)'
      else
        Result := '('+TPersistent(Pt).ClassName+')';
    end;

    tkMethod:
    begin
      Pt := GetMethodProp (Obj, PropInfo).Code;
      if Pt <> nil then
        Result := GetOwnerForm (Obj as TComponent).
          MethodName (Pt)
      else
        Result := '';
    end;

    tkVariant:
      Result := GetVariantProp (Obj, PropInfo);

    tkArray, tkRecord, tkInterface:
      Result := 'Unsupported type';

    else
      Result := 'Undefined type';
  end;
end;

function GetPropertyType(Obj: TObject; const PropName: String): TTypeKind;
var PropInfo:PPropInfo;
C: TComponentClass;
Begin
    c:=TComponentClass(GetClass(Obj.ClassName));
    PropInfo:=GetPropInfo(C.ClassInfo,PropName);
    if Propinfo<>nil then
      Result:=PropInfo.PropType^.Kind;
End;

procedure GetEnumList(Obj: TObject; const PropName: String;SList: TStrings);
var PropInfo:PPropInfo;
C: TComponentClass;
Begin
    c:=TComponentClass(GetClass(Obj.ClassName));
    PropInfo:=GetPropInfo(C.ClassInfo,PropName);
    if propinfo<>nil then
       ListEnum(PropInfo.PropType^,SList,false);
End;


// code extracted from TypInfo.pas
procedure SortPropList(PropList: PPropList; PropCount: Integer); assembler;
asm
        { ->    EAX Pointer to prop list        }
        {       EDX Property count              }
        { <-    nothing                         }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ECX,EAX
        XOR     EAX,EAX
        DEC     EDX
        CALL    @@qsort
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@qsort:
        PUSH    EAX
        PUSH    EDX
        LEA     EDI,[EAX+EDX]           { pivot := (left + right) div 2 }
        SHR     EDI,1
        MOV     EDI,[ECX+EDI*4]
        ADD     EDI,OFFSET TPropInfo.Name
@@repeat:                               { repeat                        }
@@while1:
        CALL    @@compare               { while a[i] < a[pivot] do inc(i);}
        JAE     @@endWhile1
        INC     EAX
        JMP     @@while1
@@endWhile1:
        XCHG    EAX,EDX
@@while2:
        CALL    @@compare               { while a[j] > a[pivot] do dec(j);}
        JBE     @@endWhile2
        DEC     EAX
        JMP     @@while2
@@endWhile2:
        XCHG    EAX,EDX
        CMP     EAX,EDX                 { if i <= j then begin          }
        JG      @@endRepeat
        MOV     EBX,[ECX+EAX*4]         { x := a[i];                    }
        MOV     ESI,[ECX+EDX*4]         { y := a[j];                    }
        MOV     [ECX+EDX*4],EBX         { a[j] := x;                    }
        MOV     [ECX+EAX*4],ESI         { a[i] := y;                    }
        INC     EAX                     { inc(i);                       }
        DEC     EDX                     { dec(j);                       }
                                        { end;                          }
        CMP     EAX,EDX                 { until i > j;                  }
        JLE     @@repeat

@@endRepeat:
        POP     ESI
        POP     EBX

        CMP     EAX,ESI
        JL      @@rightNonEmpty         { if i >= right then begin      }
        CMP     EDX,EBX
        JG      @@leftNonEmpty1         { if j <= left then exit        }
        RET

@@leftNonEmpty1:
        MOV     EAX,EBX
        JMP     @@qsort                 { qsort(left, j)                }

@@rightNonEmpty:
        CMP     EAX,EBX
        JG      @@leftNonEmpty2
        MOV     EDX,ESI                 { qsort(i, right)               }
        JMP     @@qsort
@@leftNonEmpty2:
        PUSH    EAX
        PUSH    ESI
        MOV     EAX,EBX
        CALL    @@qsort                 { qsort(left, j)                }
        POP     EDX
        POP     EAX
        JMP     @@qsort                 { qsort(i, right)               }

@@compare:
        PUSH    EAX
        PUSH    EDI
        MOV     ESI,[ECX+EAX*4]
        ADD     ESI,OFFSET TPropInfo.Name
        PUSH    ESI
        XOR     EBX,EBX
        MOV     BL,[ESI]
        INC     ESI
        CMP     BL,[EDI]
        JBE     @@firstLenSmaller
        MOV     BL,[EDI]
@@firstLenSmaller:
        INC     EDI
        TEST    BL,BL
        JE      @@endLoop
@@loop:
        MOV     AL,[ESI]
        MOV     AH,[EDI]
        AND     EAX,$DFDF
        CMP     AL,AH
        JNE     @@difference
        INC     ESI
        INC     EDI
        DEC     EBX
        JNZ     @@loop
@@endLoop:
        POP     ESI
        POP     EDI
        MOV     AL,[ESI]
        MOV     AH,[EDI]
        CMP     AL,AH
        POP     EAX
        RET
@@difference:
        POP     ESI
        POP     EDI
        POP     EAX
        RET
@@exit:
end;

end.

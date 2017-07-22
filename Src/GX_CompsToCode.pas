unit GX_CompsToCode;

{TODO -oStefan -cC++Builder: Implement C++ support throughout }

{$I GX_CondDefine.inc}

(*
 Copy Component Creation Code to Clipboard

 Original author: Primoz Gabrijelcic <gabr@17slon.com>
 Creation date  : 1999-06-20
 Last change    : 1999-11-04
 Version        : 1.01a

 Change history :
   1.01a: 1999-11-04
   - Small changes required to compile with GExperts 0.98.

   1.01: 1999-06-26
   - Added ability to prepend exported code with commented component code. That
     way you can recreate components back if necessary.

   1.0: 1999-06-25
   - 'Name' property is set to the component name.
   - Events are exported.
   - Binary properties can be skipped, exported commented or exported
     uncommented.
   - Simple configuration form added. Currently, user can only select what will
     happen to binary properties.
   - Expert now works correctly when nothing is selected.

   0.9: 1999-06-21
   - First beta release.

   0.8: 1999-06-20
   - First alpha version created by copying and mangling GX_CompReplace.

 Tested with: D4, D3, CB4

 Thanks to John Hansen, Stefan Hoffmeister for help and suggestions.

 Notes:
  (to-do) When exporting whole form, created code still needs some tweaking.
  (to-do) Language should be set according to source file but currently Pascal
    is always generated.
    - C is PITA, sometimes.
  (to-do) Config option: Remove component (default: no). Make a backup copy.
          Store it somewhere in memory, make a button on config
          "Restore component".
  (to-do) Config option: paste (commented) object binary.
  (to-do) There is no real icon, just a combination of Clip and Grid - feel
    free to create a better icon.
  (feature) Special processing works only for TTabSheet and descendants, not
    for other TabSheet equivalents (TTab95Sheet for example) or other strange
    components (TTeeChart, for example).
*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ToolIntf, ExptIntf, RplWizInfo, EditIntf,
  {$IFDEF GX_VER140_up}
  DesignIntf,
  {$ELSE not GX_VER140_up}
  DsgnIntf,
  {$ENDIF}
  GX_Experts, ExtCtrls;

type
  TfmCompsToCode = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    rgpBinProps: TRadioGroup;
    gbxGenerated: TGroupBox;
    chkPrepend: TCheckBox;
  private
  end;

  TBinProps = (bpSkip, bpComment, bpUncomment);

  TCompsToCodeExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  private
    ccBinProps: TBinProps;
    ccPrepend: Boolean;
    function DoCopyCreationCode: Boolean;
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_ConfigurationInfo, GX_GExperts, GX_GenFunc, GX_MessageBox,
  Clipbrd, Registry;

type
  TCCOptions = (ccBinaryRemove, ccBinaryComment, ccBinaryUncomment,
    ccIncludeObjectText);
  TCCOptionSet = set of TCCOptions;

  TComponentCreate = class(TObject)
  private
    ccObj: TStringList;
    ccDecl: TStringList;
    ccCrea: TStringList;
    ccImpl: TStringList;
    ccDumped: TStringList;
    ccCompDef: TStringList;
    ccInpPos: Integer;
    ccLn: string;
    ccULn: string;
    ccIsDirty: Boolean;
    ccForm: string;
    ccOptions: TCCOptionSet;
    function EOF: Boolean;
    procedure Readln;
    procedure DumpComponent(Comp: TComponent; out Obj, Decl, Crea, Impl: TStringList);
    procedure StreamAndParse(Comp: TComponent; out Obj, Decl, Crea, Impl, Sub: TStringList);
    procedure ParseComponent(Comp: TComponent; out Decl, Crea, Impl, Sub: TStringList);
    function GetDumped: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dump(Comp: TComponent; Options: TCCOptionSet);
    property Dumped: TStringList read GetDumped;
  end;

  TShowCodeOnClipboardMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

procedure AppendStringList(Source, Additional: TStrings);
var
  i: Integer;
begin
  for i := 0 to Additional.Count - 1 do
    Source.Add(Additional[i]);
end;

function FindChild(Cntr: TWinControl; const ControlName: string): TControl;
var
  i: Integer;
begin
  Result := nil;
  with Cntr do
    for i := 0 to ControlCount - 1 do
      if CompareText(Controls[i].Name, ControlName) = 0 then
      begin
        Result := Controls[i];
        Exit;
      end;
end;

{ TCompsToCodeExpert }

constructor TCompsToCodeExpert.Create;
begin
  inherited Create;
  HasConfigOptions := True;
  HasMenuItem := True;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

function TCompsToCodeExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Components to Code';
begin
  Result := SMenuCaption;
end;

function TCompsToCodeExpert.GetMenuName: string;
begin
  Result := 'GX_CompsToCode';
end;

function TCompsToCodeExpert.GetMenuMask: string;
begin
  Result := '*.DFM';
end;

function TCompsToCodeExpert.GetName: string;
begin
  Result := 'ComponentsToCode';
end;

function TCompsToCodeExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Components to Code';
begin
  Result := SDisplayName;
end;

procedure TCompsToCodeExpert.Click(Sender: TObject);
begin
  if DoCopyCreationCode then
    ShowGxMessageBox(TShowCodeOnClipboardMessage);
end;

procedure TCompsToCodeExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    ccBinProps := TBinProps(RegIni.ReadInteger('ComponentsToCode', 'BinaryProperties', 1));
    ccPrepend := RegIni.ReadBool('ComponentsToCode', 'PrependWithComponent', False);
  finally
    RegIni.Free;
  end;
end;

procedure TCompsToCodeExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('ComponentsToCode', 'BinaryProperties', Ord(ccBinProps));
    RegIni.WriteBool('ComponentsToCode', 'PrependWithComponent', ccPrepend);
  finally
    RegIni.Free;
  end;
end;

procedure TCompsToCodeExpert.Configure;
var
  Dlg: TfmCompsToCode;
begin
  Dlg := TfmCompsToCode.Create(nil);
  try
    Dlg.rgpBinProps.ItemIndex := Ord(ccBinProps);
    Dlg.chkPrepend.Checked := ccPrepend;
    if Dlg.ShowModal = mrOK then
    begin
      ccBinProps := TBinProps(Dlg.rgpBinProps.ItemIndex);
      ccPrepend := Dlg.chkPrepend.Checked;
      SaveSettings;
    end;
  finally
    Dlg.Free;
  end;
end;

function TCompsToCodeExpert.IconFileName: string;
begin
  Result := 'CompsToCode';
end;

{ TODO -cCleanup -oAnyone: This needs to be broken up and simplified }
function TCompsToCodeExpert.DoCopyCreationCode: Boolean;
var
  FormIntf: TIFormInterface;
  ModIntf: TIModuleInterface;
  CompIntf: TIComponentInterface;
  CompIntf2: TIComponentInterface;
  i, j: Integer;
  CurrentFileName: string;
  compConv: TComponentCreate;
  comp: TComponent;
  compParent: TControl;
  shouldDump: Boolean;
  dumpOptions: TCCOptionSet;
begin
  FormIntf := nil;
  CompIntf := nil;
  ModIntf := nil;
  Result := False;

  try
    // first assume that we have a Pascal source file
    CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.PAS'));
    ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
    if ModIntf = nil then
    begin
      // try again with C++ source file
      CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.CPP'));
      ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
    end;

    if ModIntf = nil then
      Exit;

    FormIntf := ModIntf.GetFormInterface;
    if FormIntf <> nil then
    begin
      if FormIntf.GetSelCount > 0 then
      begin
        compConv := TComponentCreate.Create;
        try
          for i := 0 to FormIntf.GetSelCount - 1 do
          begin
            CompIntf := FormIntf.GetSelComponent(i);
            try
              if CompIntf <> nil then
              begin
                // Only export component if it is not a child of another
                // component also selected for export.
                // Primarily used to remove duplicates when user selects
                // Edit.Select All, GExperts.Copy Component Creation.
                comp := CompIntf.GetComponentHandle;
                shouldDump := True;
                if (comp is TWinControl) or (comp is TGraphicControl) then
                begin
                  if comp is TWinControl then
                    compParent := (comp as TWinControl).Parent
                  else
                    compParent := (comp as TGraphicControl).Parent;
                  for j := 0 to FormIntf.GetSelCount - 1 do
                  begin
                    if i <> j then
                    begin
                      CompIntf2 := FormIntf.GetSelComponent(j);
                      if CompIntf2 <> nil then
                      begin
                        try
                          if compParent = TComponent(CompIntf2.GetComponentHandle) then
                          begin
                            shouldDump := False;
                            break; //for
                          end;
                        finally
                          CompIntf2.Free;
                        end;
                      end;
                    end;
                  end; //for
                end;
                if shouldDump then
                begin
                  dumpOptions := [];
                  case ccBinProps of
                    bpSkip: Include(dumpOptions, ccBinaryRemove);
                    bpComment: Include(dumpOptions, ccBinaryComment);
                    bpUncomment: Include(dumpOptions, ccBinaryUncomment);
                  end;
                  if ccPrepend then
                    Include(dumpOptions, ccIncludeObjectText);
                  compConv.Dump(comp, dumpOptions);
                end;
              end;
            finally
              CompIntf.Free;
              CompIntf := nil;
            end;
          end; //for
          Clipboard.AsText := compConv.Dumped.Text;
          Result := True;
        finally
          compConv.Free;
        end;
      end;
    end;
  finally
    ModIntf.Free;
    FormIntf.Free;
    CompIntf.Free;
  end;
end; { TCompsToCodeExpert.DoCopyCreationCode }

procedure TCompsToCodeExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here
    end;
  end;
end;

{ TComponentCreate }

const
  CFmtPascalVar = 'var';
  CFmtPascalDeclaration = '  %s: %s;';
  CFmtPascalCreation = '  %s := %s.Create(Self);';
  CFmtPascalWith = '  with %s do' + #13#10 + '  begin';
  CFmtPascalParent = '    Parent := %s;';
  CFmtPascalName = '    Name := ''%s'';';
  CFmtPascalPageControl = '    PageControl := %s;';
  CFmtPascalBinaryCmt = '//  %s := // please assign';
  CFmtPascalBinaryUncmt = '    %s := // please assign';
  CFmtPascalAdd = '    %s.Add(%s);';
  CFmtPascalAssign = '    %s := %s;';
  CFmtPascalEnd = '  end;';
  CFmtPascalActivePage = '  %s.%s := %s;';
  CFmtPascalWithAdd = '  with %s.Add do begin ';

constructor TComponentCreate.Create;
begin
  inherited Create;
  ccDumped := TStringList.Create;
  ccObj := TStringList.Create;
  ccDecl := TStringList.Create;
  ccCrea := TStringList.Create;
  ccImpl := TStringList.Create;
end; { TComponentCreate.Create }

destructor TComponentCreate.Destroy;
begin
  ccDumped.Free;
  ccObj.Free;
  ccDecl.Free;
  ccCrea.Free;
  ccImpl.Free;
  inherited Destroy;
end; { TComponentCreate.Destroy }

procedure TComponentCreate.Dump(Comp: TComponent; Options: TCCOptionSet);
begin
  ccOptions := Options;
  DumpComponent(Comp, ccObj, ccDecl, ccCrea, ccImpl);
  ccIsDirty := True;
end; { TComponentCreate.Dump }

procedure TComponentCreate.DumpComponent(Comp: TComponent;
  out Obj, Decl, Crea, Impl: TStringList);

  procedure _DumpComponent(Comp: TComponent; out Obj, Decl, Crea, Impl: TStringList);
  var
    i: Integer;
    Sub: TStringList;
  begin
    Sub := TStringList.Create;
    try
      StreamAndParse(Comp, Obj, Decl, Crea, Impl, Sub);
      if Comp is TWinControl then
        with Comp as TWinControl do
        begin
          for i := 0 to ControlCount - 1 do
            if Sub.IndexOf(Controls[i].Name) < 0 then
              _DumpComponent(Controls[i], Obj, Decl, Crea, Impl);
        end;
    finally
      Sub.Free;
    end;
  end; { _DumpComponent }

resourcestring
  SSelectComponentsFirst = 'Please select one or more components first';

begin
  if Comp.Owner = nil then
    raise Exception.Create(SSelectComponentsFirst)
  else
    ccForm := UpperCase(Comp.Owner.Name);
  _DumpComponent(Comp, Obj, Decl, Crea, Impl);
end; { TComponentCreate.DumpComponent }

function TComponentCreate.EOF: Boolean;
begin
  Result := (ccInpPos >= ccCompDef.Count);
end;

function TComponentCreate.GetDumped: TStringList;
begin
  if ccIsDirty then
  begin
    ccDumped.Assign(ccObj);
    ccDumped.Add(CFmtPascalVar);
    AppendStringList(ccDumped, ccDecl);
    ccDumped.Add('');
    AppendStringList(ccDumped, ccCrea);
    AppendStringList(ccDumped, ccImpl);
    ccIsDirty := False;
  end;
  Result := ccDumped;
end;

// TODO -oStefan -cC++Builder: Translate - this is the core
// This is one huge method - 180 lines!
procedure TComponentCreate.ParseComponent(comp: TComponent;
  out Decl, Crea, Impl, Sub: TStringList);
var
  p: Integer;
  compSub: TStringList;
  compName: string;
  compClass: string;
  cName: string;
  propVal: string;
  propName: string;
  last: Boolean;
  childComp: TComponent;
  parent: string;
  tmps: string;

  procedure Log(List: TStringList; Fmt: string; Values: array of const);
  begin
    List.Add(Format(Fmt, Values));
  end; { Log }

  procedure ProcGlyph(propName: string);
  begin
    if ([ccBinaryComment, ccBinaryUnComment] * ccOptions) <> [] then
    begin
      p := LastDelimiter('.', propName);
      if p > 0 then
        propName := Copy(propName, 1, p - 1);
      if ccBinaryComment in ccOptions then
        Log(impl, CFmtPascalBinaryCmt, [propName])
      else
        Log(impl, CFmtPascalBinaryUnCmt, [propName]);
    end;
    while not EOF do
    begin
      Readln;
      if ccLn[Length(ccLn)] = '}' then
        Break;
    end; //while
  end; { ProcGlyph }

  procedure ProcSL(propName: string);
  var
    p: Integer;
  begin
    p := LastDelimiter('.', propName);
    if p > 0 then
    begin
      propName := Copy(propName, 1, p - 1);
      last := False;
      while not (EOF or last) do
      begin
        Readln;
        if (Length(ccLn) > 0) and (ccLn[Length(ccLn)] = ')') then
        begin
          ccLn := Copy(ccLn, 1, Length(ccLn) - 1);
          last := True;
        end;
        // D5 (at least) sometimes inserts completely blank lines where the
        // next line has a ' +' in it at the end, which should be ignored.  One
        // example is TUpdateSQL with IDE generated SQL from DBDEMOS.biolife.db
        if (Length(ccLn) > 2) and (Copy(ccLn, Length(ccLn) - 1, 2) = ' +') then
          Delete(ccLn, Length(ccLn) - 1, 2);
        if (Length(ccLn) > 0) then
          Log(impl, CFmtPascalAdd, [propName, ccLn]);
      end; //while
    end;
  end; { ProcSL }

  procedure ProcItems(propName: string);
  var
    pVal: string;
    pName: string;
    last: Boolean;
    p: Integer;
  begin
    last := False;
    while not (EOF or last) do
    begin
      Readln;
      if ccULn = 'ITEM' then
      begin
        Log(impl, '  ' + CFmtPascalWithAdd, [propName]);
        while not (EOF or last) do
        begin
          Readln;
          if (ccULn = 'END') or (ccUln = 'END>') then
          begin
            Log(impl, '  ' + CFmtPascalEnd, [nil]);
            if ccLn[Length(ccLn)] = '>' then
              last := True;
            Break;
          end
          else
          begin
            p := Pos('=', ccLn);
            if p > 0 then
            begin
              pName := TrimRight(Copy(ccLn, 1, p - 1));
              pVal := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
              Log(impl, '  ' + CFmtPascalAssign, [pName, pVal]);
            end;
          end;
        end; //while
      end;
    end; //while
  end; { ProcItems }

begin
  compSub := TStringList.Create;
  try
    ccLn := Copy(ccLn, Length('object') + 2, Length(ccLn) - Length('object') - 1);
    p := Pos(':', ccLn);
    if p > 0 then
    begin
      compName := TrimRight(Copy(ccLn, 1, p - 1));
      compClass := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
      Log(decl, CFmtPascalDeclaration, [compName, compClass]);
      Log(crea, CFmtPascalCreation, [compName, compClass]);
      Log(impl, CFmtPascalWith, [compName]);
      if comp is TControl then
      begin
        if (comp as TControl).Parent is TCustomForm then
          parent := 'Self'
        else
          parent := (comp as TControl).Parent.Name;
      end
      else
        parent := ''; // not really needed, just to keep Delphi from issuing warnings
      Log(impl, CFmtPascalName, [compName]);
      if comp is TControl then
        Log(impl, CFmtPascalParent, [parent]);
      if comp is TTabSheet then
        Log(impl, CFmtPascalPageControl, [parent]);
      while not EOF do
      begin
        Readln;
        if ccULn = 'END' then
          Break
        else if Copy(ccULn, 1, Length('object')) = 'OBJECT' then
        begin
          tmps := Copy(ccLn, Length('object') + 2, Length(ccLn) - Length('object') - 1);
          p := Pos(':', tmps);
          if p > 0 then
          begin
            cName := TrimRight(Copy(tmps, 1, p - 1));
            sub.Add(cName);
            childComp := comp;
            if comp is TWinControl then
            begin
              childComp := FindChild(comp as TWinControl, cName);
              if not assigned(childComp) then
                childComp := comp;
            end;
            ParseComponent(childComp, decl, crea, compSub, sub);
          end;
        end
        else
        begin
          p := Pos('=', ccLn);
          if p > 0 then
          begin
            propName := TrimRight(Copy(ccLn, 1, p - 1));
            propVal := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
            p := Pos('.', propVal);
            if p > 0 then
            begin
              if UpperCase(Copy(propVal, 1, p - 1)) = ccForm then
                propVal := Copy(propVal, p + 1, Length(propVal) - p);
            end;
            if propVal = '{' then // glyphs etc - skip
              ProcGlyph(propName)
            else if propVal = '(' then // string lists
              ProcSL(propName)
            else if propVal = '<' then // ListView columns etc
              ProcItems(propName)
            else
              Log(impl, CFmtPascalAssign, [propName, propVal]);
          end; //if p > 0
        end; //else if not skip
      end; //while not EOF
      Log(impl, CFmtPascalEnd, [nil]);
    end;
    AppendStringList(impl, compSub);
  finally
    compSub.Free;
  end;
end; { TComponentCreate.ParseComponent }

procedure TComponentCreate.Readln;
begin
  ccLn := Trim(ccCompDef[ccInpPos]);
  ccUln := UpperCase(ccLn);
  Inc(ccInpPos);
end;

procedure TComponentCreate.StreamAndParse(comp: TComponent;
  out obj, decl, crea, impl, sub: TStringList);
var
  i: Integer;
  tmpWriter: TWriter;
  iStream: TMemoryStream;
  oStream: TMemoryStream;
begin
  iStream := TMemoryStream.Create;
  try
    // Suggested by John Hansen:
    tmpWriter := TWriter.Create(iStream, 4096);
    try
      tmpWriter.Root := comp.Owner;
      tmpWriter.WriteSignature;
      tmpWriter.WriteComponent(comp);
      tmpWriter.WriteListEnd;
      tmpWriter.WriteListEnd;
    finally
      tmpWriter.Free;
    end;
    iStream.Position := 0;
    oStream := TMemoryStream.Create;
    try
      ObjectBinaryToText(iStream, oStream);
      ccCompDef := TStringList.Create;
      try
        oStream.Position := 0;
        ccCompDef.LoadFromStream(oStream);
        if ccIncludeObjectText in ccOptions then
        begin
          for i := 0 to ccCompDef.Count - 1 do
            obj.Add('//' + ccCompDef[i]);
          obj.Add('');
        end;
        ccInpPos := 0;
        while not EOF do
        begin
          Readln;
          if Copy(ccULn, 1, Length('object')) = 'OBJECT' then
            ParseComponent(comp, decl, crea, impl, sub);
        end; //while
      finally
        ccCompDef.Free;
      end;
    finally
      oStream.Free;
    end;
  finally
    iStream.Free;
  end;
end; { TComponentCreate.StreamAndParse }

{ TShowCodeOnClipboardMessage }

function TShowCodeOnClipboardMessage.GetMessage: string;
resourcestring
  SCopyToClipboardComplete =
    'The code to create the selected components has been copied to the clipboard.'#13 +
    #13 +
    'You can now paste the generated code into the IDE ' +
    'editor at the appropriate position.';
begin
  Result := SCopyToClipboardComplete;
end;

initialization

  RegisterGX_Expert(TCompsToCodeExpert);

end.


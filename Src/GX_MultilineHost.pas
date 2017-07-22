unit GX_MultilineHost;

// Original Author: Stefan Hoffmeister <Stefan.Hoffmeister@econos.de>

{$I GX_CondDefine.inc}

{$IFDEF GX_VER120_up}

interface

uses
  ToolIntf, ExptIntf,
  GX_Experts,
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls;

type
  TGxMultiLineTabDockHostsManager = class(TObject)
  private
    FSqueezingCode: Pointer;
    FDockableFormClass: TClass;
    FNotificationVmtIndex: Integer;
  protected
    procedure DisableHooking;
    procedure EnableHooking;
    function DetermineNotificationVmtIndex: Integer;
    function GetTabDockHostFormClass: TClass;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  DefaultToMultiLine: Boolean = False;

implementation

uses
  Menus, ComCtrls,
  GX_VerDepConst,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  Registry, GX_GExperts, GX_ConfigurationInfo;

type
  TNotificationSignature = procedure(Self: TObject; AComponent: TComponent; Operation: TOperation); register;

var
  ReplacedNotification: TNotificationSignature;

const
  AddedMultiLineMenuItemName = 'mnuGxMultiLineTab';

type
  TGxSqueezedInClass = class(TComponent)
  // We cannot have any data members, since the class may never be instantiated...
  private
    procedure InsertMultilineControls(TabDockHost: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure PopupMenuPopup(Sender: TObject);
    procedure MultiLineClick(Sender: TObject);
  end;

procedure TGxSqueezedInClass.InsertMultilineControls(TabDockHost: TWinControl);
resourcestring
  SMultiLine = 'Multiline';
var
  MenuItem: TMenuItem;
  FoundPopupMenu: TPopupMenu;
  FoundPageControl: TPageControl;
  i: Integer;
begin
  for i := 0 to TabDockHost.ComponentCount-1 do
  begin
    if TabDockHost.Components[i] is TPopupMenu then
    begin
      FoundPopupMenu := TPopupMenu(TabDockHost.Components[i]);
      if FoundPopupMenu.FindComponent(AddedMultiLineMenuItemName) <> nil then
        Break;

      FoundPopupMenu.OnPopup := PopupMenuPopup;

      // Add a break line
      MenuItem := TMenuItem.Create(FoundPopupMenu);
      MenuItem.Caption := '-';

      FoundPopupMenu.Items.Insert(0, MenuItem);

      // Our multi-line entry
      MenuItem := TMenuItem.Create(FoundPopupMenu);
      MenuItem.Name := AddedMultiLineMenuItemName;
      MenuItem.Checked := DefaultToMultiLine;
      MenuItem.Caption := SMultiLine;
      MenuItem.OnClick := MultiLineClick;

      FoundPopupMenu.Items.Insert(0, MenuItem);

      // Done looping
      Break;
    end;
  end;

  if DefaultToMultiLine then
    for i := 0 to TabDockHost.ComponentCount-1 do
    begin
      // OutputDebugString(PChar(AnsiString(TabDockHost.Components[i].ClassName)));
      if TabDockHost.Components[i] is TPageControl then
      begin
        FoundPageControl := TPageControl(TabDockHost.Components[i]);
        FoundPageControl.MultiLine := True;
      end;
    end;
end;

procedure TGxSqueezedInClass.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Warning: Handle "Self" with care...
  Assert(@ReplacedNotification <> nil);
  ReplacedNotification(Self, AComponent, Operation);

  Assert(TComponent(Self) is TWinControl);
  InsertMultilineControls(TWinControl(Self));
end;

procedure TGxSqueezedInClass.MultiLineClick(Sender: TObject);
var
  i: Integer;
  IsMultiLine: Boolean;
begin
  // Warning: Handle "Self" with care...
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    IsMultiLine := Checked;
  end;

  for i := 0 to ComponentCount-1 do
    if Components[i] is TPageControl then
      TPageControl(Components[i]).MultiLine := IsMultiLine;
end;

procedure TGxSqueezedInClass.PopupMenuPopup(Sender: TObject);
var
  TabDockForm: TCustomForm;
  FoundPageControl: TPageControl;
  MenuItem: TMenuItem;
  i: Integer;
begin
  // Warning: Handle "Self" with care...
  TabDockForm := (Sender as TPopupMenu).Owner as TCustomForm;
  for i := 0 to TabDockForm.ComponentCount-1 do
  begin
    if TabDockForm.Components[i] is TPageControl then
    begin
      FoundPageControl := TPageControl(TabDockForm.Components[i]);
      MenuItem := (Sender as TPopupMenu).FindComponent(AddedMultiLineMenuItemName) as TMenuItem;
      Assert(MenuItem <> nil);
      MenuItem.Enabled := not (FoundPageControl.TabPosition in [tpLeft, tpRight]);
      MenuItem.Checked := FoundPageControl.MultiLine;
    end;
  end;
end;

{ TGxMultiLineTabDockHostsManager }

constructor TGxMultiLineTabDockHostsManager.Create;
(*
var
  i: Integer;
  TabDockHostForm: TComponent;
*)
begin
  inherited Create;

  FSqueezingCode := @TGxSqueezedInClass.Notification;

  FDockableFormClass := GetTabDockHostFormClass;
  FNotificationVmtIndex := DetermineNotificationVmtIndex;

  EnableHooking;
(*
  // Scan for tab dock hosts that are already present
  // and add ourselves to those forms that are already
  // present.
  // The order is important - first we must enable
  // hooking, since this is tested (implicitly) in
  // the squeezing class
  for i := 0 to Screen.FormCount-1 do
  begin
    if CompareText(Screen.Forms[i].ClassName, TTabDockHostFormName) = 0 then
    begin
      TabDockHostForm := Screen.Forms[i];
      with TGxSqueezedInClass.Create(TabDockHostForm) do
        InsertMultilineControls(TabDockHostForm);
    end;
  end;
*)
end;

destructor TGxMultiLineTabDockHostsManager.Destroy;
begin
  DisableHooking;

  FDockableFormClass := nil;
  FNotificationVmtIndex := -1;
  FSqueezingCode := nil;

  inherited Destroy;
end;

function GetVirtualMethodPointer(AClass: TClass; const Index: Cardinal): Pointer;
type
  PPointer = ^Pointer;
begin
  Result := PPointer(Cardinal(AClass) + Index * SizeOf(Pointer))^;
end;

procedure SetVirtualMethodPointer(AClass: TClass; const Index: Cardinal; const Method: Pointer);
var
  PatchAddress: Pointer;
  DummyProtection: DWORD;
  OldProtection: DWORD;
begin
  PatchAddress := Pointer(Cardinal(AClass) + Index * SizeOf(Pointer));

  // Set memory access rights so that we can write into this page
  if not VirtualProtect(PatchAddress, SizeOf(Pointer), PAGE_READWRITE, @OldProtection) then
    RaiseLastWin32Error;

  try
    // Write method into VMT
    Pointer(PatchAddress^) := Method;

  finally
    // Restore memory access rights
    if not VirtualProtect(PatchAddress, SizeOf(Pointer), OldProtection, @DummyProtection) then
      RaiseLastWin32Error;
  end;

  // Make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

procedure TGxMultiLineTabDockHostsManager.DisableHooking;
begin
  if FDockableFormClass = nil then
    Exit;

  Assert(@ReplacedNotification <> nil);

  if FNotificationVmtIndex < 0 then
    Exit;

  Assert(FSqueezingCode = GetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex));
  SetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex, @ReplacedNotification);
  @ReplacedNotification := nil;
end;

procedure TGxMultiLineTabDockHostsManager.EnableHooking;
begin
  if FDockableFormClass = nil then
    Exit;

  Assert(@ReplacedNotification = nil);

  if FNotificationVmtIndex < 0 then
    Exit;

  @ReplacedNotification := GetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex);
  SetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex, FSqueezingCode);
end;

function TGxMultiLineTabDockHostsManager.GetTabDockHostFormClass: TClass;
type
  PPointer = ^Pointer;

  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record
    e_magic: Word;
    e_cblp: Word;
    e_cp: Word;
    e_crlc: Word;
    e_cparhdr: Word;
    e_minalloc: Word;
    e_maxalloc: Word;
    e_ss: Word;
    e_sp: Word;
    e_csum: Word;
    e_ip: Word;
    e_cs: Word;
    e_lfarlc: Word;
    e_ovno: Word;
    e_res: array[0..3] of Word;
    e_oemid: Word;
    e_oeminfo: Word;
    e_res2: array[0..9] of Word;
    e_lfanew: Longint;
  end;

var
  NtHeader: PImageNtHeaders;
  SectionHeader: PImageSectionHeader;

  function GetSectionHeader(const ASectionName: string): Boolean;
  var
    i: Integer;
  begin
    SectionHeader := PImageSectionHeader(NtHeader);
    Inc(PImageNtHeaders(SectionHeader));
    Result := False;

    for i := 0 to NtHeader.FileHeader.NumberOfSections - 1 do
    begin
      if StrLIComp(PChar(@SectionHeader^.Name), PChar(ASectionName), IMAGE_SIZEOF_SHORT_NAME) = 0 then
      begin
        Result := True;
        Break;
      end;
      Inc(SectionHeader);
    end;
  end;

  function InRangeOrNil(const APointer, PLowerBound, PUpperBound: Pointer): Boolean;
  begin
    Result := (APointer = nil) or
                ((Integer(PLowerBound) <= Integer(APointer)) and
                 (Integer(APointer) <= Integer(PUpperBound)));
  end;

var
  DosHeader: PImageDosHeader;
  PCodeBegin: PChar;
  PCodeEnd: PChar;
  PCodeScanner: PChar;
  p: PChar;

  FoundClassName: PShortString;

  ScannedModule: HMODULE;
begin
  Result := nil;

  //! StH:
  // The following scanner is based on code posted by Ludovic Dubois
  //
  //   From: "Ludovic Dubois" <ldubois@prettyobjects.com>
  //   Newsgroups: borland.public.delphi.opentoolsapi
  //   Subject: [Hack] Classes Hack!
  //   Date: Sat, 7 Nov 1998 15:40:48 -0500
  //   Message-ID: <722b75$mer7@forums.borland.com>
  //
  // Note: this code is **not** safe for general purpose use, since there
  // are MANY hidden assumptions which in this use-case incidentally
  // happen to be all valid.
  //
  // All changes with respect to the semantics of the posted
  // code are fully intentional.
  //

  ScannedModule := GetModuleHandle(TTabDockHostFormClassContainer);
  if ScannedModule = 0 then
  begin
    {$IFOPT D+}SendDebugEx(TTabDockHostFormClassContainer + ' could not be loaded from GX_MultilineHost.pas', mtError);{$ENDIF D+}
    Exit;
  end;

  // Get PE DOS header
  DosHeader := PImageDosHeader(ScannedModule);

  if not DosHeader.e_magic = IMAGE_DOS_SIGNATURE then
    Exit;

  // Get NT header
  NtHeader := PImageNtHeaders(Longint(DosHeader) + DosHeader.e_lfanew);
  if IsBadReadPtr(NtHeader, SizeOf(IMAGE_NT_HEADERS)) or
     (NtHeader.Signature <> IMAGE_NT_SIGNATURE) or
     (NtHeader.FileHeader.SizeOfOptionalHeader <> SizeOf(NtHeader.OptionalHeader)) then
  begin
    Exit;
  end;

  // Find the code section
  if not GetSectionHeader('CODE') then
    Exit;

  // Compute beginning and end of the code section
  PCodeBegin := PChar(ScannedModule + SectionHeader.VirtualAddress);
  PCodeEnd := PCodeBegin + (SectionHeader.SizeOfRawData - 3);
  PCodeScanner := PCodeBegin;
  while PCodeScanner < PCodeEnd do
  begin
    p := PPointer(PCodeScanner)^;
    // Search for a(ny) class, employing some heuristics
    if (p = (PCodeScanner - vmtSelfPtr)) and
       InRangeOrNil(PPointer(p+vmtClassName)^,    p,          PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtDynamicTable)^, p,          PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtMethodTable)^,  p,          PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtFieldTable)^,   p,          PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtInitTable)^,    PCodeBegin, PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtAutoTable)^,    PCodeBegin, PCodeEnd) and
       InRangeOrNil(PPointer(p+vmtIntfTable)^,    PCodeBegin, PCodeEnd) then
    begin
      FoundClassName := PShortString(PPointer(p+vmtClassName)^);
      if IsValidIdent(FoundClassName^) and (FoundClassName^ = TTabDockHostFormName) then
      begin
        Result := TClass(p);
        Break;
      end;

      Inc(PCodeScanner, 4);
    end
    else
      Inc(PCodeScanner);
  end;
end;

function TGxMultiLineTabDockHostsManager.DetermineNotificationVmtIndex: Integer;
type
  PPointer = ^Pointer;
const
  MaxIndex = 500;
var
  VmtScanner: PPointer;
  Index: Integer;
begin
  Result := -1;

  if FDockableFormClass = nil then
    Exit;

  // We need to "find" the index of the Notification virtual function;
  // theoretically hard-coding the index would work, too, but that
  // breaks as soon as the VCL changes somewhere deep down.
  // Accomplish this by finding the index in our squeezing class
  // (TGxSqueezedInClass) and simply use that index for our target
  // class - it is guaranteed to be the same index by virtue of
  // virtual method tables (and not compiler version dependent).
  VmtScanner := PPointer(TGxSqueezedInClass);
  Index := -1;

  // MaxIndex is just a sanity checker in order to
  // scan at most MaxIndex VMT entries.
  while Index < MaxIndex do
  begin
    Inc(Index);
    if VmtScanner^ = FSqueezingCode then
      Break;
    Inc(VmtScanner);
  end;

  if Index = MaxIndex then
    Exit;

  Result := Index;
end;

{$ELSE GX_VER120_up}
interface implementation
{$ENDIF GX_VER120_up}

end.


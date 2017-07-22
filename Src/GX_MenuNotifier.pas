unit GX_MenuNotifier;

{$I GX_CondDefine.inc}

//************************************************************************************
// This unit controls the menu enabled state of menu items added by GExperts
// To use it, first create and register the project notifier
// After you create your menu items, add them to the menuItems in the project notifier
// along with the extension the expert should work against.
// Here are some examples:
//      if ProcMenuItem<>nil then
//        AddObject('.PAS;.DPR',ProcMenuItem);
//      if MsgExpMenuItem<>nil then
//        AddObject('.PAS;.DPR',MsgExpMenuItem);
//      if DebugInsertItem<>nil then
//        AddObject('.PAS;.DPR',DebugInsertItem);
//      if TabsMenuItem<>nil then
//        AddObject('.DFM',TabsMenuItem);
//      if CompEditMenu<>nil then
//        AddObject('.DFM',CompEditMenu);
//
// Before freeing your menu interfaces, make sure you remove and free the project
// notifier first.
//************************************************************************************

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  GX_BaseModuleNotifiers,
  Classes, SysUtils, ToolIntf, EditIntf, ExptIntf,
  TypInfo,
  Dialogs, IniFiles, Menus;

type
  TMenuItems = class(TStringList);

type
  TStatistics = class(TObject)
  public
    Filename: string;
    OpenTime: Integer;
    WorkTime: Integer;
    TotalOpenTime: Integer;
    TotalWorkTime: Integer;
  end;

  TModuleNotifier = class(TBaseModuleNotifier)
  private
  (*
    OpenStart: TDateTime;
    ModifyStart: TDateTime;
    ModifyLast: TDateTime;
    TotalModify: Integer;
    procedure UpdateModifyStat;
  *)
  public
    procedure Notify(NotifyCode: TNotifyCode); override;
  end;

  TProjectNotifier = class(TBaseProjectNotifier)
  private
    FMenuItems: TMenuItems;
  private
  (*
    ElapseTime: Integer;
    StatsList: TStringList;
    procedure SaveStats;
    procedure LoadStats;
  *)
  protected
    procedure DoProjectOpened(const FileName: string; var Cancel: Boolean); override;
    procedure DoFileOpened(const FileName: string; var Cancel: Boolean); override;
    procedure DoFileClosing(const FileName: string; var Cancel: Boolean); override;
  public
    constructor Create(ModuleNotifierClass: TBaseModuleNotifierClass);
    destructor Destroy; override;
  public
    property MenuItems: TMenuItems read FMenuItems;
  end;

implementation

uses
  Windows,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc, GX_Experts;

procedure EnableMenuItems(MenuItems: TMenuItems);
var
  FileExt: string;
  i: Integer;
begin
  FileExt := ExtractUpperFileExt(ToolServices.GetCurrentFile);

  for i := 0 to MenuItems.Count - 1 do
    if MenuItems.Objects[i] <> nil then
    begin
      if MenuItems.Objects[i] <> nil then
        Assert(MenuItems.Objects[i] is TGXAction, 'MenuItem Object is not a TGXAction: ' +MenuItems.Strings[i]+': '+ IntToStr(i));

      if Pos(FileExt, MenuItems.Strings[i]) = 0 then
      begin
        if TGXAction(MenuItems.Objects[i]) <> nil then
          TGXAction(MenuItems.Objects[i]).Enabled := False;
      end
      else
      begin
        if TGXAction(MenuItems.Objects[i]) <> nil then
          TGXAction(MenuItems.Objects[i]).Enabled := True;
      end;
    end;
end;

{ TProjectNotifier }

constructor TProjectNotifier.Create(ModuleNotifierClass: TBaseModuleNotifierClass);
begin
  inherited Create(ModuleNotifierClass);

  //ElapseTime := 60;
  //StatsList := TStringList.Create;
  FMenuItems := TMenuItems.Create;

  {$IFOPT D+}SendDebug('Created Project Notifier'){$ENDIF}
end;

destructor TProjectNotifier.Destroy;
begin
  // Due to our design here, it is an error if we end up with unfreed notifiers

  {$IFOPT D+}
  // Delphi 5 and Delphi 4 don't always fire ncModuleDeleted for packages with
  // desktop saving off.  Another workaround would be to not install notifiers
  // for DPKs or bomb here only on remaining module notifiers for non-DPKs
  if (FNotifiers <> nil) and (FNotifiers.Count > 0) then
    SendDebugEx('Existing module notifier not freed: ' + TBaseModuleNotifier(FNotifiers[0]).FileName, mtWarning);
  {$ENDIF}

  FMenuItems.Free;
  FMenuItems := nil;
  (*
  if StatsList <> nil then
  begin
    for i := 0 to StatsList.Count-1 do
      StatsList.Objects[i].Free;
  end;
  StatsList.Free;
  StatsList := nil;
  *)
  inherited Destroy;
end;

procedure TProjectNotifier.DoProjectOpened(const FileName: string; var Cancel: Boolean);
begin
  inherited DoProjectOpened(FileName, Cancel);

  // LoadStats;
end;

procedure TProjectNotifier.DoFileOpened(const FileName: string; var Cancel: Boolean);
begin
  EnableMenuItems(MenuItems);

  inherited DoFileOpened(FileName, Cancel);
end;

procedure TProjectNotifier.DoFileClosing(const FileName: string; var Cancel: Boolean);
var
  i: Integer;
begin
  // SaveStats;

  Assert(FMenuItems <> nil);
  for i := 0 to FMenuItems.Count - 1 do
  begin
    if TGXAction(FMenuItems.Objects[i]) <> nil then
    TGXAction(FMenuItems.Objects[i]).Enabled := False;
  end;

  inherited DoFileClosing(FileName, Cancel);
end;

(*
procedure TProjectNotifier.SaveStats;
var
  IniFile: TIniFile;
  CurrentFile: string;
  i: Integer;
  Stat: TStatistics;
begin
  CurrentFile := ToolServices.GetCurrentFile;
  if IsDpr(CurrentFile) then
    Exit;
  CurrentFile := ChangeFileExt(CurrentFile, '.STS');
  IniFile := TIniFile.Create(CurrentFile);
  try
    IniFile.WriteInteger('Stats', 'Count', StatsList.Count);
    for i := 0 to StatsList.Count - 1 do
    begin
      Stat := StatsList.Objects[i] as TStatistics;
      IniFile.WriteString('Stats', 'FileName' + IntToStr(i), Stat.FileName);
      Inifile.WriteInteger('Stats', 'Open' + IntToStr(i), Stat.TotalOpenTime);
      IniFile.WriteInteger('Stats', 'Work' + IntToStr(i), Stat.TotalWorkTime);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TProjectNotifier.LoadStats;
var
  IniFile: TIniFile;
  CurrentFile: string;
  i, c: Integer;
  Stat: TStatistics;
begin
  CurrentFile := ToolServices.GetCurrentFile;
  if IsDpr(CurrentFile) then
    Exit;
  CurrentFile := ChangeFileExt(CurrentFile, '.STS');
  IniFile := TIniFile.Create(CurrentFile);
  try
    c := IniFile.ReadInteger('Stats', 'Count', 0);
    for i := 0 to c - 1 do
    begin
      Stat := TStatistics.Create;
      Stat.FileName := IniFile.ReadString('Stats', 'FileName' + IntToStr(i), Stat.FileName);
      Stat.TotalOpenTime := Inifile.ReadInteger('Stats', 'Open' + IntToStr(i), Stat.TotalOpenTime);
      Stat.TotalWorkTime := IniFile.ReadInteger('Stats', 'Work' + IntToStr(i), Stat.TotalWorkTime);
      StatsList.AddObject(Stat.FileName, Stat);
    end;
  finally
    IniFile.Free;
  end;
end;
*)

{ TModuleNotifier }

procedure TModuleNotifier.Notify(NotifyCode: TNotifyCode);
begin
  inherited Notify(NotifyCode);

    {$IFOPT D+} SendDebug('Notification ' + GetEnumName(TypeInfo(TNotifyCode), Ord(NotifyCode)) +': "'+Self.FFileName+'"'); {$ENDIF}
    case NotifyCode of

      ncEditorSelected,
      ncFormSelected:
        begin
          Assert(FProjectNotifier is TProjectNotifier);
          EnableMenuItems(TProjectNotifier(FProjectNotifier).MenuItems);
        end;

    ncEditorModified,
    ncFormModified:
      begin
        //UpdateModifyStat;
      end;
  end;
end;

(*
procedure TModuleNotifier.UpdateModifyStat;
var
  DateTime: TDateTime;
begin
  DateTime := Now;

  if ModifyStart = 0 then
  begin
    ModifyStart := DateTime;
    Exit;
  end;

  if ModifyLast = 0 then
  begin
    if DateTime > ModifyStart + FProjectNotifier.ElapseTime then
      TotalModify := TotalModify + FProjectNotifier.ElapseTime;
    ModifyLast := DateTime;
  end;
end;
*)

end.


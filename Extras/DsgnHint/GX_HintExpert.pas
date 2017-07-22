unit GX_HintExpert;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,Registry,TypInfo;

type TMouseHook = record
    Point: TPoint;
    HWND: Word;
    HitTestCode: Word;
    ExtraInfo: Longint;
  end;

type
  TfmHintWindow = class(TForm)
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    FPropList: TStringList;
    LCol: Integer;
    procedure SetSize;
  public
    { Public declarations }
    Control: TComponent;
    NewVisible: Boolean;
    AllowProcess: Boolean;
    function LoadProps: Boolean;
    function GetPropName(i: Integer): String;
    function GetPropValue(i: Integer):String;
  end;

var
  fmHintWindow: TfmHintWindow=nil;
  HintHooked: Boolean=False;
  Hook: HHook;

procedure SetHook;
procedure RemoveHook;

implementation

{$R *.DFM}

uses GX_dbugintf,ExpertUtil,GX_GExperts,GX_uGenFunc,GX_DsgnHint,GX_RTTI;

function CheckForNonVisual(Control: TComponent;Point: TPoint):TComponent;
var Form: TForm;
DataM: TDataModule;
CLeft,CTop: Integer;
i: Integer;
Begin

  DataM:=nil;
  Form:=nil;
  Result:=Control;

  if (Control is TForm) then
    Form:=TForm(Control)
  else
    if (Control.Owner is TForm) then
      Form:=TForm(Control.Owner);

  if (uppercase(Control.ClassName)='TCOMPONENTCONTAINER') and
     (Control.Owner<>nil) then
    Begin
    Form:=TForm(Control.Owner);
    for i:=0 to Form.ComponentCount-1 do
      if Form.Components[i] is TDataModule then
        Begin
        DataM:=TDataModule(Form.Components[i]);
        Break;
        End;
    End;

  if DataM<>nil then
    Begin
    Point:=TWinControl(Control).ScreenToClient(Point);
    For i:=0 to DataM.ComponentCount-1 do
      if not (DataM.Components[i] is TControl) then
        Begin
        CLeft:=LongRec(DataM.Components[i].DesignInfo).Lo;
        CTop:=LongRec(DataM.Components[i].DesignInfo).Hi;
        if (Point.X>=CLeft) and (Point.X<=CLeft+28) and
           (Point.Y>=CTop) and (Point.Y<=CTop+28) then
           Begin
           Result:=DataM.Components[i];
           Exit;
           End;
        End;
    End
  else
    Begin {Check for TForm}
    if Form=nil then exit;
    Point:=Form.ScreenToClient(Point);
    For i:=0 to Form.ComponentCount-1 do
      if not (Form.Components[i] is TControl) then
        Begin
        CLeft:=LongRec(Form.Components[i].DesignInfo).Lo;
        CTop:=LongRec(Form.Components[i].DesignInfo).Hi;
        if (Point.X>=CLeft) and (Point.X<=CLeft+28) and
           (Point.Y>=CTop) and (Point.Y<=CTop+28) then
           Begin
           Result:=Form.Components[i];
           Exit;
           End;
        End;
    End;
End;

function MouseMsgHook(nCode: Integer; wParam: Longint; var MouseHook: TMouseHook): Longint; stdcall;
var
  Control: TComponent;
  Form: TForm;
begin
  {Process message before doing anything }
  Result := CallNextHookEx(Hook, nCode, wParam, Longint(@MouseHook));

  if (Application.MainForm<>nil) and (Application.MainForm.WindowState=wsMinimized) then exit;
  if Application.Terminated then exit;

  {Only mouse move events }
  if (nCode >= 0) and (wParam = WM_MOUSEMOVE) and fmHintWindow.AllowProcess then
    Begin
    if fmHintWindow.NewVisible then
      Begin
      ShowWindow(fmHintWindow.Handle,SW_HIDE);
      fmHintWindow.Timer1.Enabled:=False;
      fmHintWindow.Control:=nil;
      fmHintWindow.NewVisible:=False;
      End;
    Control := TComponent(FindDragTarget(MouseHook.Point, True));
    if Control<>nil then
      Begin
      if (csDesigning in Control.ComponentState) then
        Begin
        Control:=CheckForNonVisual(Control,MouseHook.Point); {Non visual controls take precedence}
        if (Control<>fmHintWindow.Control) and (not fmHintWindow.Visible) then
          Begin
          fmHintWindow.Control:=Control;
          if fmHintWindow.LoadProps then
            Begin
            fmHintWindow.Timer1.Enabled:=False;
            fmHintWindow.Timer1.Enabled:=True;
            fmHintWindow.Left:=MouseHook.Point.X+2;
            fmHintWindow.Top:=MouseHook.Point.Y+2;
            End
          End
        End;
      End;
    End;
end; { MouseMsgHook }

procedure TfmHintWindow.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Close;
end;

function TfmHintWindow.LoadProps: Boolean;
var RegIni: TRegIniFile;
i: Integer;
PropList: PPropList;
PropInfo:PPropInfo;
Data: PTypeData;
Begin
  FPropList.Clear;
  if DesignHintExpert.ShowClassName then
    FPropList.Add('Classname'+#9+Control.ClassName);
  RegIni:=TRegIniFile.Create(ConfigInfo.RegKey+'\GExperts\DesignHints');
  Try
    Data:=GetTypeData(Control.Classinfo);
    GetMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    Try
      GetPropInfos(Control.ClassInfo,PropList);
      for i:=0 to Data^.propCount-1 do
        Begin
        PropInfo:=PropList^[i];
        if (PropInfo^.PropType^.Kind<>tkMethod) and
           (PropInfo^.PropType^.Kind<>tkInterface) then
          if RegIni.ReadBool(Control.ClassName,PropInfo^.Name,False) then
            FPropList.Add(PropInfo.Name+#9+GetPropValAsString(Control,PropInfo));
        End;
    finally
    FreeMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    End;
  finally
  RegIni.Free;
  End;
  Result:=(FPropList.Count>0);
  if Result then SetSize;
End;

procedure TfmHintWindow.SetSize;
var Rcol,i: Integer;
Begin
  Lcol:=0;
  RCol:=0;
  for i:=0 to fPropList.Count-1 do
    Begin
    if Canvas.TextWidth(GetPropName(i))>LCol then
      LCol:=Canvas.TextWidth(GetPropName(i));
    if Canvas.TextWidth(GetPropValue(i))>RCol then
      RCol:=Canvas.TextWidth(GetPropValue(i));
    End;
  inc(LCol,6);
  Width:=LCol+RCol+10;
  Height:=FPropList.Count*(Canvas.TextHeight('W')+4);
End;

procedure TfmHintWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ShowWindow(Handle,SW_HIDE);
  Timer1.Enabled:=False;
  Control:=nil;
  NewVisible:=False;
end;

procedure TfmHintWindow.FormCreate(Sender: TObject);
begin
  AllowProcess:=True;
  NewVisible:=False;
  Timer1.Interval:=Application.HintPause;
  Caption:='';
  FPropList:=TStringList.Create;
  FPropList.Sorted:=True;
end;


procedure TfmHintWindow.FormDestroy(Sender: TObject);
begin
  FPropList.Free;
end;

function TfmHintWindow.GetPropName(i: Integer):String;
var p: Integer;
Begin
  p:=pos(#9,FPropList.Strings[i]);
  if p>0 then
    Result:=trim(Copy(FPropList.Strings[i],1,p-1))
  else
    Result:=FPropList.Strings[i];
End;

function TfmHintWindow.GetPropValue(i: Integer):String;
var p: Integer;
Begin
  p:=pos(#9,FPropList.Strings[i]);
  if p>0 then
    Result:=trim(Copy(FPropList.Strings[i],p+1,length(FPropList.Strings[i])))
  else
    Result:=FPropList.Strings[i];
End;

procedure SetHook;
Begin
  if Hook<>0 then exit;
  Hook := SetWindowsHookEx(WH_MOUSE, @MouseMsgHook, 0, GetCurrentThreadID);
  fmHintWindow:=TFmHintWindow.Create(Application);
  HintHooked:=True;
End;

procedure RemoveHook;
Begin
  if Hook <> 0 then UnhookWindowsHookEx(Hook);
  if (fmHintWindow<>nil) and (not Application.Terminated) then
    Begin
    fmHintWindow.Free;
    fmHintWindow:=nil;
    End;
  HintHooked:=False;
  Hook:=0;
  {$IFOPT D+}SendDebug('Removing mouse hook');{$ENDIF}
End;

procedure TfmHintWindow.FormPaint(Sender: TObject);
var Row: Integer;
i: Integer;
begin
  Row:=3;
  With Canvas do
    Begin
    Pen.Color:=clBlack;
    Brush.Color:=clBlack;
    FrameRect(ClientRect);
    Brush.Color:=clInfoBk;
    Brush.Style:=bsClear;
    MoveTo(LCol,0);
    LineTo(LCol,Height);
    for i:=0 to FPropList.Count-1 do
      Begin
      TextOut(4,Row,GetPropName(i));
      TextOut(LCol+4,Row,GetPropValue(i));
      if i<>FPropList.Count-1 then
        Begin
        MoveTo(0,Row+Textheight('W')+2);
        LineTo(ClientWidth,Row+Textheight('W')+2);
        End;
      Row:=Row+Textheight('W')+4;
      End;
    End;
end;

procedure TfmHintWindow.Timer1Timer(Sender: TObject);
var NControl: TComponent;
MousePos: TPoint;
begin
  Timer1.Enabled:=False;
  {Do we have properties to show?}
  if FPropList.Count=0 then exit;
  {Is the application active}
  if not Application.Active then exit;
  {Are we still positioned over the same control when the timer was set}
  GetCursorPos(MousePos);
  NControl := TComponent(FindDragTarget(MousePos, True));
  if NControl<>nil then
    if (csDesigning in NControl.ComponentState) then
      NControl:=CheckForNonVisual(NControl,MousePos); {Non visual controls take precedence}
  if NControl<>Control then exit;

  {All Conditions met, show hint window}
  AllowProcess:=False;
  Try
  ShowWindow(Self.Handle,SW_SHOWNOACTIVATE);
  SetWindowPos(Self.handle,HWND_TOPMOST,Left,Top,Width,Height,SWP_NOACTIVATE);
  NewVisible:=True;
  Application.ProcessMessages;
  finally
  AllowProcess:=True;
  End;
end;

procedure TfmHintWindow.Timer2Timer(Sender: TObject);
begin
  AllowProcess:=True;
  Timer2.Enabled:=False;
end;

end.

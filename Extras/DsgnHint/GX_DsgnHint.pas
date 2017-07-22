unit GX_DsgnHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, ComCtrls,GX_HintExpert,ToolIntf,ExptIntf,GX_uExperts;

type
  TfmDesignHint = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    tshProperties: TTabSheet;
    Image31: TImage;
    cbClassname: TCheckBox;
    lbComps: TListBox;
    cblbProps: TCheckListBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCompsClick(Sender: TObject);
    procedure lbCompsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cblbPropsClickCheck(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FHintList: TStrings;
  public
    { Public declarations }
    procedure SaveHintSettings;
    procedure LoadHintSettings;
    procedure BuildComponentList;
  end;

  TDesignHintExpert = class(TGX_Expert)
  private
    FShowClassName: Boolean;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
  published
    property ShowClassName: Boolean read FShowClassName write FShowClassName;
  end;

var
  fmDesignHint: TfmDesignHint=nil;
  DesignHintExpert: TDesignHintExpert=nil;

implementation

{$R *.DFM}

uses Registry,TypInfo,GX_GExperts,LibIntf;

constructor TDesignHintExpert.Create;
begin
  inherited Create;
  DesignHintExpert := Self;
  ShortCut := 0;
  HasConfigOptions := True;
  HasMenuItem := False;
end;

destructor TDesignHintExpert.Destroy;
begin
  DesignHintExpert := nil;
  inherited Destroy;
end;

function TDesignHintExpert.GetMenuCaption: string;
begin
  Result := '';
end;

function TDesignHintExpert.GetMenuName: string;
begin
  Result := 'GX_DESIGNHINT';
end;

function TDesignHintExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TDesignHintExpert.GetName: string;
begin
  Result := 'DesignHint_Window';
end;

function TDesignHintExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Design time hints';
begin
  Result := SDisplayName;
end;

function TDesignHintExpert.IconFileName: string;
begin
  Result := 'DsgnHint';
end;

procedure TDesignHintExpert.Configure;
Begin
  fmDesignHint:=TfmDesignHint.Create(Application);
  Try
  fmDesignHint.cbClassName.Checked:=ShowClassName;
  fmDesignHint.ShowModal;
  finally
  fmDesignHint.Free;
  End;
End;

procedure TDesignHintExpert.LoadSettings;
var RegIni: TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create(ConfigInfo.RegKey+'\GExperts');
  Try
  FShowClassName:=RegIni.ReadBool('DesignHints','ShowClassName',False);
  finally
  RegIni.Free;
  End;
End;

procedure TDesignHintExpert.SaveSettings;
var RegIni: TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create(ConfigInfo.RegKey+'\GExperts');
  Try
  RegIni.WriteBool('DesignHints','ShowClassName',ShowClassName);
  finally
  RegIni.Free;
  End;
End;

procedure TDesignHintExpert.SetActive(New: Boolean);
Begin
  inherited SetActive(New);
  if Active then SetHook
  else RemoveHook;
End;

//********************************** TfmDesignHint ***************************************
procedure TfmDesignHint.BuildComponentList;
var i,j: Integer;
Begin
 for i:=0 to ToolServices.GetModuleCount-1 do
   for j:=0 to ToolServices.GetComponentCount(i)-1 do
     lbComps.Items.Add(ToolServices.GetComponentName(i,j));
End;

procedure TfmDesignHint.LoadHintSettings;
var RegIni: TRegIniFile;
i,j,k: Integer;
CheckState: Boolean;
First:Boolean;
Begin
  First:=True;
  RegIni:=TRegIniFile.Create(ConfigInfo.RegKey+'\GExperts\DesignHints');
  Try
  for k:=0 to lbComps.Items.Count-1 do
    if lbComps.Selected[k] then
      Begin
      for i:=0 to cblbProps.Items.Count-1 do
        Begin
        j:=FHintList.Indexof(lbComps.Items[k]+','+cblbProps.Items[i]);
        if j>=0 then
          CheckState:=Boolean(Integer(FHintList.Objects[j]))
        else
          CheckState:=RegIni.ReadBool(lbComps.Items[k],cblbProps.Items[i],False);
        if First then
          cblbProps.Checked[i]:=CheckState
        else
          Begin
          if ((cblbProps.State[i]=cbChecked) and (not CheckState)) or
             ((cblbProps.State[i]=cbUnChecked) and (CheckState)) then
            cblbProps.State[i]:=cbGrayed;
          End;
        End;
      First:=False;
      End;
  finally
  RegIni.Free;
  End;
End;

procedure TfmDesignHint.SaveHintSettings;

  function GetCName(St: String):String;
  var p: Integer;
  Begin
    p:=pos(',',st);
    if p>0 then
      Result:=Copy(st,1,p-1)
    else
      Result:=st;
  End;

  function GetPName(St: String):String;
  var p: Integer;
  Begin
    p:=pos(',',st);
    if p>0 then
      Result:=Copy(st,p+1,length(st))
    else
      Result:=st;
  End;


var RegIni: TRegIniFile;
i: Integer;
Begin
  RegIni:=TRegIniFile.Create(ConfigInfo.RegKey+'\GExperts\DesignHints');
  Try
  for i:=0 to FHintList.Count-1 do
    RegIni.WriteInteger(GetCName(FHintList.Strings[i]),GetPName(FHintList.Strings[i]),Integer(FHintList.Objects[i]));
  finally
  RegIni.Free;
  End;
End;

procedure TfmDesignHint.FormCreate(Sender: TObject);
begin
  FHintList:=TStringList.Create;
  BuildComponentList;
  LoadHintSettings;
end;

procedure TfmDesignHint.FormDestroy(Sender: TObject);
begin
  FHintList.Free;
end;

procedure TfmDesignHint.lbCompsClick(Sender: TObject);
var PropName:String;
PropList: PPropList;
PropInfo:PPropInfo;
Data: PTypeData;
C: TComponentClass;
i: Integer;

  procedure AddProperties(Index: Integer);
  var j: Integer;
  Begin
    c:=TComponentClass(GetClass(lbComps.Items[Index]));
    Data:=GetTypeData(C.Classinfo);
    GetMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    Try
      GetPropInfos(C.ClassInfo,PropList);
      for j:=0 to Data^.propCount-1 do
        Begin
        PropInfo:=PropList^[j];
        if (PropInfo^.PropType^.Kind<>tkMethod) and
           (PropInfo^.PropType^.Kind<>tkInterface) then
          cblbProps.Items.Add(PropInfo^.Name);
        End;
    finally
    FreeMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    End;
  End;

  procedure RemoveProperties(Index: Integer);
  var j,k: Integer;
  Begin
    k:=0;
    c:=TComponentClass(GetClass(lbComps.Items[Index]));
    Data:=GetTypeData(C.Classinfo);
    GetMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    Try
      GetPropInfos(C.ClassInfo,PropList);
      while k<=cblbProps.Items.Count-1 do
        Begin
        for j:=0 to Data^.propCount-1 do
          Begin
          PropInfo:=PropList^[j];
          if (PropInfo^.PropType^.Kind<>tkMethod) and
             (PropInfo^.PropType^.Kind<>tkInterface) then
            if CompareText(cblbProps.Items[k],PropInfo^.Name)=0 then
              Break;
          End;
        if j>Data^.PropCount-1 then
          cblbprops.Items.Delete(k)
        else
          inc(k);
        End;
    finally
    FreeMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
    End;
  End;

begin
  cblbProps.Items.BeginUpdate;
  Try
  cblbProps.Items.Clear;
  for i:=0 to lbComps.Items.Count-1 do
    if lbComps.Selected[i] then
      Begin
      if cblbProps.Items.Count=0 then
        AddProperties(i)
      else
        RemoveProperties(i)
      End;
  LoadHintSettings;
  finally
  cblbProps.Items.EndUpdate;
  End;
end;

procedure TfmDesignHint.lbCompsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var C: TPersistentClass;
H: Integer;
FRect:TRect;
TopColor,BottomColor:TColor;
{$IFDEF VER100}
PI: TIPaletteItem;
{$ENDIF}
{$IFDEF VER120}
PI: IPaletteItem;
{$ENDIF}
begin
  TopColor:=clBtnHighlight; BottomColor:=clBtnShadow;
  With lbComps.Canvas do
    Begin
    if odSelected in State then
      Begin
      Brush.Color:=clHighlight;
      Font.Color:=clHighlightText;
      End
    else
      Begin
      Brush.Color:=clWindow;
      Font.Color:=clWindowText;
      End;
    FillRect(Rect);
    C:=GetClass(lbComps.Items[Index]);
    {$IFDEF VER100}
    if C<>nil then
      Begin
      PI:=DelphiIDE.GetPaletteItem(TComponentClass(C));
      PI.paint(lbComps.Canvas,Rect.Left+2,Rect.Top+1);
      FRect:=Classes.Rect(Rect.Left+2,Rect.Top+1,Rect.Left+30,Rect.Top+29);
      Frame3d(lbComps.Canvas,FRect,TopColor,BottomColor,1);
      End;
    {$ENDIF}
    {$IFDEF VER120}
    if C<>nil then
      Begin
      PI:=DelphiIDE.GetPaletteItem(TComponentClass(C));
      PI.paint(lbComps.Canvas,Rect.Left+2,Rect.Top+1);
      FRect:=Classes.Rect(Rect.Left+2,Rect.Top+1,Rect.Left+30,Rect.Top+29);
      Frame3d(lbComps.Canvas,FRect,TopColor,BottomColor,1);
      End;
    {$ENDIF}
    H:=((Rect.Bottom-Rect.Top)-TextHeight(lbComps.Items[Index])) div 2;
    TextOut(Rect.Left+34,Rect.Top+H,lbComps.Items[Index]);
    End;
end;

procedure TfmDesignHint.cblbPropsClickCheck(Sender: TObject);
var i,c,p: Integer;
begin
  for c:=0 to lbComps.Items.Count-1 do
    if lbComps.Selected[c] then
      Begin
      p:=cblbProps.ItemIndex;
      if (c<0) or (p<0) then exit;
      i:=FHintList.indexof(lbComps.Items[c]+','+cblbProps.Items[p]);
      if i<0 then
        FHintList.AddObject(lbComps.Items[c]+','+cblbProps.Items[p],Pointer(Integer(cblbProps.Checked[p])))
      else
        FHintList.Objects[i]:=Pointer(Integer(cblbProps.Checked[p]));
      End;
end;

procedure TfmDesignHint.btnOKClick(Sender: TObject);
begin
  SaveHintSettings;
  DesignHintExpert.ShowClassName:=cbClassName.Checked;
end;

initialization
  RegisterGX_Expert(TDesignHintExpert);

end.


end.

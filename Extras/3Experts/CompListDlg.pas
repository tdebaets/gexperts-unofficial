unit CompListDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, Spin, Clipbrd;

const
  BLANK = #32;

type
  TSelCompListDlg = class(TForm)
    CopyToClipBoardBtn: TButton;
    CancelBtn: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    IndentSpinEdit: TSpinEdit;
    Label1: TLabel;
    PrefixComboBox: TComboBox;
    SuffixComboBox: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    IndentFirstLineCheckBox: TCheckBox;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
    procedure CopyToClipBoardBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelCompListDlg: TSelCompListDlg;

implementation

{$R *.DFM}
//function strPadChR(const S: String; C: Char; Len: Integer): String;
//begin
//  Result := S;
//  while Length(Result) < Len do Result := Result + C;
//end;

function strPadChL(const S: String; C: Char; Len: Integer): String;
begin
  Result:=S;
  while Length(Result)<Len do Result:=C+Result;
end;

function strTrimChL(const S: String; C: Char): String;
begin
  Result:=S;
  while (Length(Result)>0) and (Result[1]=C) do Delete(Result,1,1);
end;

function strTrimChR(const S: String; C: Char): String;
begin
  Result:=S;
  while (Length(Result)> 0) and (Result[Length(Result)]=C) do
    Delete(Result,Length(Result),1);
end;



function strTrim(const S: String): String;
begin
  Result:=StrTrimChR(StrTrimChL(S,BLANK),BLANK);
end;

procedure TSelCompListDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TSelCompListDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TSelCompListDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I], 
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

procedure TSelCompListDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TSelCompListDlg.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TSelCompListDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TSelCompListDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TSelCompListDlg.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;




procedure TSelCompListDlg.CopyToClipBoardBtnClick(Sender: TObject);
var
  ResultList : TStringList;
  cTmp : String;
  nCount : Integer;
begin
  ResultList := TStringList.Create;
  try
    for nCount := 0 to (DstList.Items.Count - 1) do begin
      if ((nCount <> 0) or (IndentFirstLineCheckBox.Checked)) then
        cTmp := strPadChL('', BLANK, IndentSpinEdit.Value);
      cTmp := cTmp + PrefixComboBox.Text + strTrim(DstList.Items.Strings[nCount]) + SuffixComboBox.Text;
      ResultList.Append(cTmp);
    end;{for}
    Clipboard.Clear;
    Clipboard.SetTextBuf(PChar(ResultList.Text));
  finally
    ResultList.Free;
  end;{finally}
end;

end.

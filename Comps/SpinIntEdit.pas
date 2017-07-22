unit SpinIntEdit;

{ This is a quickly hacked TSpinEdit replacement; cut and pasted
  from existing source code with no claims for elegance.
  It is designed for quick'n'dirty use in GExperts, no more, no less. }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ComCtrls;

type
  TSpinIntEdit = class(TCustomEdit)
  private
    FUpDown: TUpDown;

    FValue: Integer;
    FIncrement: Integer;
    function GetMaximum: SmallInt;
    function GetMinimum: SmallInt;
    procedure SetMaximum(const Value: SmallInt);
    procedure SetMinimum(const Value: SmallInt);
    function IsWithInMinMax(const Value: SmallInt): Boolean;
  private
  (*
    procedure WMCut(var Message: TWMPaste); message WM_CUT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
  *)
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Change; override;

    procedure AdjustSpin;
    procedure CreateSpin;
    procedure DestroySpin;

    procedure SetEditRect;

    function GetValue: Integer;
    procedure SetValue(Value: Integer);

    function TextToInt(const Value: string): Integer;
    function IntToText(const Value: Integer): string;

    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);

    procedure Increase; virtual;
    procedure Decrease; virtual;

    function IsValidChar(const Key: Char): Boolean; virtual;
    function IsValidData(const NewData: string): Boolean; virtual;
    function IsValidValueString(const ValueString: string): Boolean; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure UpdateValue; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Maximum: SmallInt read GetMaximum write SetMaximum default 100;
    property Minimum: SmallInt read GetMinimum write SetMinimum default 0;
    property Value: Integer read GetValue write SetValue;
    property TabOrder;
    property Enabled;
  end;


procedure Register;

implementation

(*
uses
  ClipBrd;
*)

procedure Register;
begin
  RegisterComponents('GExperts', [TSpinIntEdit]);
end;

(*
procedure TSpinIntEdit.WMPaste(var Message: TWMPaste);
var
  ClipStr: string;
begin
  if ReadOnly then Exit;

  with Clipboard do
  begin
    Open;
    try
      ClipStr := '';
      if HasFormat(CF_TEXT) then
        ClipStr := AsText;
    finally
      Close;
    end;
  end;

  if not IsValidData(ClipStr) then
    Exit;

  inherited;
end;

procedure TSpinIntEdit.WMCut(var Message: TWMPaste);
begin
  if ReadOnly then Exit;

  inherited;
end;
*)
procedure TSpinIntEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;

  inherited;
end;

procedure TSpinIntEdit.Change;
begin
  inherited Change;

  UpdateValue;
end;

procedure TSpinIntEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;

  UpdateValue;
end;

procedure TSpinIntEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TSpinIntEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:    Increase;
    VK_DOWN:  Decrease;
  else
    { do nothing }
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TSpinIntEdit.KeyPress(var Key: Char);
begin
  if not IsValidData(Key) then
    Key := #0;
    
  inherited KeyPress(Key)
end;

function TSpinIntEdit.IsValidData(const NewData: string): Boolean;
var
  EditText: string[20];

  EditStart: DWORD;
  EditEnd: DWORD;

  i: Integer;
begin
  Result := True;

  { always allow #8 = Delete as a valid key }
  if NewData = #8 then
    Exit;

  { verify that all keys }
  for i := 1 to Length(NewData) do
    Result := Result and IsValidChar(NewData[i]);

  { terminate early if even the characters(s) are not OK }
  if not Result then
    Exit;

  EditText[0] := Char(SendMessage(Handle, WM_GETTEXT, SizeOf(EditText)-1, Integer(@EditText[1])));

  SendMessage(Handle, EM_GETSEL, Integer(@EditStart), Integer(@EditEnd));

  { Remove currently selected text from edit control }
  Delete(EditText, EditStart+1, EditEnd-EditStart);

  { Insert typed character at caret position }
  Insert(NewData, EditText, EditStart+1);

  Result := IsValidValueString(EditText);
end;


constructor TSpinIntEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FIncrement := 1;

  CreateSpin;
end;

destructor TSpinIntEdit.Destroy;
begin
  DestroySpin;

  inherited Destroy;
end;

procedure TSpinIntEdit.CreateWnd;
begin
  inherited CreateWnd;

  SetEditRect;
end;

procedure TSpinIntEdit.Loaded;
begin
  inherited Loaded;

  if Text = '' then
    Text := '0';
end;

procedure TSpinIntEdit.CreateSpin;
begin
  if FUpDown = nil then
  begin
    FUpDown := TUpDown.Create(Self);
    with FUpDown do
    begin
      Parent := Self;
      OnMouseDown := Self.OnMouseDown;
      OnClick := UpDownClick;
      SetBounds(0, 0, Width, Height);
      Visible := Self.Visible;
    end;
  end;
end;

procedure TSpinIntEdit.DestroySpin;
begin
  FUpDown.Free;
  FUpDown := nil;
end;

procedure TSpinIntEdit.SetEditRect;
var
  Loc: TRect;
begin
  if FUpDown <> nil then
  begin
    Loc.Bottom := ClientHeight + 1; { +1 is a work-around for Windows paint bug }
    Loc.Right := ClientWidth - FUpDown.Width - 2;
    Loc.Top := 0;
    Loc.Left := 0;
  end
  else
  begin
    Loc.Bottom := ClientHeight;
    Loc.Right := ClientWidth;
    Loc.Top := 0;
    Loc.Left := 0;
  end;

  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TSpinIntEdit.UpClick(Sender: TObject);
begin
  Increase;
end;

procedure TSpinIntEdit.DownClick(Sender: TObject);
begin
  Decrease;
end;

procedure TSpinIntEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btPrev: DownClick(Sender);
    btNext: UpClick(Sender);
  else
    Assert(False);
  end;
end;

procedure TSpinIntEdit.AdjustSpin;
begin
  if NewStyleControls then
    FUpDown.SetBounds(Width - 19, 0, 15, Height - 4)
  else
    FUpDown.SetBounds(Width - 15, 0, 15, Height);

  SetEditRect;
end;

procedure TSpinIntEdit.WMSize(var Message: TWMSize);
begin
  inherited;

  AdjustSpin;
end;

function TSpinIntEdit.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TSpinIntEdit.SetValue(Value: Integer);
begin
  try
    Text := IntToText(Value);
    FValue := Value;
  except
    Text := '0';
    FValue := 0;
  end;
  FUpDown.Position := Value;
end;

function TSpinIntEdit.IsValidChar(const Key: Char): Boolean;
begin
  Result := (Key in ['0'..'9', '+', '-']);
end;

function TSpinIntEdit.IsValidValueString(const ValueString: string): Boolean;
begin
  Result := False;
  try
    Result := IsWithInMinMax(TextToInt(ValueString));
  except
    on E: Exception do
      { swallow any exception }
  end;
end;

function TSpinIntEdit.TextToInt(const Value: string): Integer;
begin
  if Value <> '' then
    Result := StrToInt(Value)
  else
    Result := 0;
end;

function TSpinIntEdit.IntToText(const Value: Integer): string;
begin
  Result := IntToStr(Value)
end;

procedure TSpinIntEdit.UpdateValue;
begin
  try
    FValue := TextToInt(Self.Text);
  except
    FValue := 0;
  end;
end;

procedure TSpinIntEdit.Increase;
var
  Temp: Integer;
begin
  Temp := FValue + FIncrement;
  if IsWithInMinMax(Temp) then
    SetValue(Temp);
end;

procedure TSpinIntEdit.Decrease;
var
  Temp: Integer;
begin
  Temp := FValue - FIncrement;
  if IsWithInMinMax(Temp) then
    SetValue(Temp);
end;

function TSpinIntEdit.GetMaximum: SmallInt;
begin
  Result := FUpDown.Max;
end;

function TSpinIntEdit.GetMinimum: SmallInt;
begin
  Result := FUpDown.Min;
end;

procedure TSpinIntEdit.SetMaximum(const Value: SmallInt);
begin
  FUpDown.Max := Value;
end;

procedure TSpinIntEdit.SetMinimum(const Value: SmallInt);
begin
  FUpDown.Min := Value;
end;

function TSpinIntEdit.IsWithInMinMax(const Value: SmallInt): Boolean;
begin
  Result := ((Value >= Minimum) and (Value <= Maximum));
end;

end.

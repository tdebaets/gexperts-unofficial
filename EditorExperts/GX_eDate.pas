unit GX_eDate;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Menus,
  StdCtrls,
  GX_EditorExpert;

type
  TDateExpert = class(TEditorExpert)
  private
    FDateFormat: string;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
    property DateFormat: string read FDateFormat write FDateFormat;
  end;

type
  TfmDateFormat = class(TForm)
    lblFormat: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

uses
  Registry, GX_EditWriter, ToolIntf, ExptIntf,
  GX_GenFunc;

{$R *.DFM}

constructor TDateExpert.Create;
resourcestring
  SDateExpertName = 'Insert Date/Time';
begin
  inherited Create;
  //ButtonNo := ??;
  ShortCut := scCtrl + scAlt + Ord('A');
  FName := SDateExpertName;
  FDateFormat := ShortDateFormat;
end;

procedure TDateExpert.GetHelpString(List: TStrings);
resourcestring
  SDateExpertHelp =
    '  This expert inserts the current date/time at the cursor position in '+
    'the code editor.  The format of the date/time text is configurable '+
    'using standard VCL date format specifiers.  See the FormatDateTime '+
    'help topic in the VCL documentation for full details.';
begin
  List.Text := SDateExpertHelp;
end;

procedure TDateExpert.Configure;
begin
  with TfmDateFormat.Create(nil) do
  try
    cbFormat.Text := DateFormat;
    if ShowModal = mrOK then
      DateFormat := cbFormat.Text;
  finally
    Free;
  end;
end;

procedure TDateExpert.SaveSettings;
begin
  inherited;
  // do not localize any of the below items
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    WriteInteger('DateExpert', 'ShortCut', ShortCut);
    WriteString('DateExpert', 'Format', DateFormat);
  finally
    Free;
  end;
end;

procedure TDateExpert.LoadSettings;
begin
  inherited;
  // do not localize any of the below items
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    ShortCut := ReadInteger('DateExpert', 'ShortCut', ShortCut);
    DateFormat := ReadString('DateExpert', 'Format', FDateFormat);
  finally
    Free;
  end;
end;

procedure TDateExpert.Execute;
var
  EditWrite: TEditWriter;
  InsertString: string;
begin
  try
    EditWrite := TEditWriter.Create(ToolServices.GetCurrentFile);
    try
      try
        InsertString := FormatDateTime(FDateFormat, Date + Time);
      except
        on E: EConvertError do
          InsertString := 'Invalid date/time format';
      end;
      EditWrite.WriteAtCurrentPos(InsertString);
    finally
      EditWrite.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

initialization
  RegisterEditorExpert(TDateExpert);
end.


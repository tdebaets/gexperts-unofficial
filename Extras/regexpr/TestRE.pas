{$B-}
unit TestRE;

{
 simple test of regular expression

 Andrey Sorokin, Saint-Petersburg, Russia
 anso@mail.ru, anso@usa.net
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls,
  RegExpr;

type
  TfmTestRE = class(TForm)
    edRegExpr: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edInputString: TEdit;
    btnViewPCode: TBitBtn;
    btnTestString: TBitBtn;
    btnClose: TBitBtn;
    lblTestResult: TLabel;
    btnTemplatePhonePiter: TSpeedButton;
    btnTemplatePhone: TSpeedButton;
    btnTemplatePassport: TSpeedButton;
    btnTemplateMail: TSpeedButton;
    btnTemplateInteger: TSpeedButton;
    btnTemplateRealNumber: TSpeedButton;
    btnTemplateURL: TSpeedButton;
    procedure btnViewPCodeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnTestStringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTemplatePhonePiterClick(Sender: TObject);
    procedure btnTemplatePhoneClick(Sender: TObject);
    procedure btnTemplatePassportClick(Sender: TObject);
    procedure btnTemplateMailClick(Sender: TObject);
    procedure btnTemplateIntegerClick(Sender: TObject);
    procedure btnTemplateRealNumberClick(Sender: TObject);
    procedure btnTemplateURLClick(Sender: TObject);
   private
    r : TRegExpr;
  end;

var
  fmTestRE: TfmTestRE;

implementation
{$R *.DFM}

uses PCode;

procedure TfmTestRE.FormCreate(Sender: TObject);
 begin
  r := TRegExpr.Create;
 end;

procedure TfmTestRE.FormDestroy(Sender: TObject);
 begin
  if Assigned (r)
   then r.Free;
 end;

procedure TfmTestRE.btnViewPCodeClick(Sender: TObject);
 begin
  with TfmPseudoCodeViewer.Create (Application) do begin
    r.Expression := edRegExpr.Text;
    edSource.Text := r.Expression;
    Memo1.Lines.Text := r.Dump;
    ShowModal;
   end;
 end;

procedure TfmTestRE.btnCloseClick(Sender: TObject);
 begin
  Close;
 end;

procedure TfmTestRE.btnTestStringClick(Sender: TObject);
 var i : integer;
 begin
  try
    r.Expression := edRegExpr.Text;
    if r.Exec (edInputString.Text) then begin
       lblTestResult.Caption := '';
       for i := 0 to r.MatchCount - 1 do begin
         if lblTestResult.Caption <> ''
          then lblTestResult.Caption := lblTestResult.Caption + ', ';
         lblTestResult.Caption := lblTestResult.Caption
          + IntToStr (r.MatchPos [i])
          + '-' + IntToStr (r.MatchPos [i] + r.MatchLen [i] - 1);
        end;
       edInputString.SetFocus; 
       edInputString.SelStart := r.MatchPos [0] - 1;
       edInputString.SelLength := r.MatchLen [0];
       lblTestResult.Caption := 'Reg.expr found ! ('
        + lblTestResult.Caption  + ')';
       lblTestResult.Font.Color := clGreen;
      end
     else begin
       lblTestResult.Caption := 'Regexpr. not found in string.';
       lblTestResult.Font.Color := clPurple;
      end;
    except on E:Exception do begin
      lblTestResult.Caption := 'Error in TRegExpr: "' + E.Message + '"';
      lblTestResult.Font.Color := clRed;
     end;
   end;
 end;

procedure TfmTestRE.btnTemplatePhonePiterClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '(\d\d\d-?\d\d-?\d\d)';
 end;

procedure TfmTestRE.btnTemplatePhoneClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '((\+\d *)?(\(\d+\) *)?\d+(-\d*)*)';
 end;

procedure TfmTestRE.btnTemplatePassportClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '(([IVXCL][IVXCL][IVXCL]-[à-ß][à-ß] *[N¹]? *)?öööööö)';
 end;

procedure TfmTestRE.btnTemplateMailClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '([_a-zA-Z\d\-\.]+@[_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+)+)';
 end;


procedure TfmTestRE.btnTemplateIntegerClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '([+\-]?\d+)';
 end;

procedure TfmTestRE.btnTemplateRealNumberClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '([+\-]?\d+(\.\d+)?([eE][+\-]?\d+)?)';
 end;

procedure TfmTestRE.btnTemplateURLClick(Sender: TObject);
 begin
  edRegExpr.Text :=
   '([Ff][Tt][Pp]|[Hh][Tt][Tt][Pp])://(' // protocol
   + '[_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+)+' // TCP addr
   + ')((/[ _a-zA-Z\d\-\\\.]+)+)*'; // unix path
 end;

end.



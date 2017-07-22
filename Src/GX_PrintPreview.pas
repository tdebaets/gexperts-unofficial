unit GX_PrintPreview;

{$I GX_CondDefine.inc}

{$IFNDEF ACEREPORTER}
interface implementation
{$ELSE ACEREPORTER}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AcePrev, ExtCtrls, Buttons, AceFile, AceSetup, StdCtrls;

type
  TfmPreview = class(TForm)
    Panel1: TPanel;
    AcePreview: TAcePreview;
    sbPrint: TSpeedButton;
    sbStart: TSpeedButton;
    sbPrevious: TSpeedButton;
    sbNext: TSpeedButton;
    sbEnd: TSpeedButton;
    pnPage: TPanel;
    ZoomValues: TComboBox;
    procedure sbPrintClick(Sender: TObject);
    procedure sbStartClick(Sender: TObject);
    procedure sbEndClick(Sender: TObject);
    procedure sbPreviousClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomValuesChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdatePage;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  Printers, Clipbrd,
  AceOut, AceDest, AcePStat, AceGoto;

procedure TfmPreview.sbPrintClick(Sender: TObject);
var
  pd: TAcePrintDestination;
  ps: TAcePreviewStatus;
  CopyPS: TAcePrinterSetup;
begin
  CopyPS := TAcePrinterSetup.Create;
  CopyPS.Assign(AcePreview.AcePrinterSetup);

  pd := TAcePrintDestination.Create(nil);
  try
    pd.Viewer := Self;
    pd.Preview := AcePreview;
    pd.AcePrinterSetup := AcePreview.AcePrinterSetup;
    pd.AcePrinterSetup.SetData;

    try
      if pd.ShowModal = mrOk then
      begin
        ps := TAcePreviewStatus.Create(Self);
        try
          ps.Preview := ACEPreview;
          ps.Show;
          ACEPreview.IgnorePrinterSettings := not CopyPS.IsEqual(pd.AcePrinterSetup);
  {        pd.OverrideSettings.Checked;}
          ACEPreview.PrintStatus := ps.UpdateStatus;
          ACEPreview.SendPagesToPrinter(pd.StartPage, pd.EndPage);
        finally
          ps.Free;
        end;
      end;
    finally
      pd.Free;
    end;
  finally
    CopyPS.Free;
  end;
end;

procedure TfmPreview.sbStartClick(Sender: TObject);
begin
  AcePreview.FirstPage;
  UpdatePage;
end;

procedure TfmPreview.UpdatePage;
resourcestring
  SPreviewPage = 'Page: %d/%d';
begin
  pnPage.Caption := Format(SPreviewPage, [ACEPreview.Page, ACEPreview.PageCount]);
  if ACEPreview.Description <> '' then
    Caption := ACEPreview.Description;
  ZoomValues.ItemIndex := Ord(ACEPreview.AceZoom);
end;

procedure TfmPreview.sbEndClick(Sender: TObject);
begin
  AcePreview.LastPage;
  UpdatePage;
end;

procedure TfmPreview.sbPreviousClick(Sender: TObject);
begin
  AcePreview.PriorPage;
  UpdatePage;
end;

procedure TfmPreview.sbNextClick(Sender: TObject);
begin
  AcePreview.NextPage;
  UpdatePage;
end;

procedure TfmPreview.FormShow(Sender: TObject);
begin
  UpdatePage;
end;

procedure TfmPreview.ZoomValuesChange(Sender: TObject);
begin
  ACEPreview.AceZoom := TAceZoom(TComboBox(Sender).ItemIndex);
end;
{$ENDIF ACEREPORTER}
end.

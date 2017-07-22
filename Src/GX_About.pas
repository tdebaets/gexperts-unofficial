unit GX_About;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmAbout = class(TForm)
    lblGExperts: TLabel;
    btnClose: TButton;
    lblVersion: TLabel;
    pnlLogo: TPanel;
    imgLogo: TImage;
    lblNotes: TLabel;
    btnEmail: TButton;
    lblWebPage: TLabel;
    lblProjectLeader: TLabel;
    lblOriginalAuthor: TLabel;
    lblContributors: TLabel;
    lblErik: TLabel;
    lblGerald: TLabel;
    lblWebSite: TLabel;
    pnlContributors: TPanel;
    lbxContributors: TListBox;
    lblAlpha: TLabel;
    procedure btnEmailClick(Sender: TObject);
    procedure lblWebPageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  end;

implementation

uses GX_GenFunc;

{$R *.DFM}

procedure TfmAbout.btnEmailClick(Sender: TObject);
var
  URL: string;
begin
  // do not localize
  URL := 'mailto:' + 'eberry@gexperts.org';
  GXShellExecute(URL, '', True);
end;

procedure TfmAbout.lblWebPageClick(Sender: TObject);
begin
  GXShellExecute((Sender as TLabel).Caption, '', True);
end;

procedure TfmAbout.FormCreate(Sender: TObject);
resourcestring
  SVersion = 'Version';
  SUnknown = '<unknown>';
var
  FileName: string;
  V1, V2, V3 , V4: Word;
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  SetLength(FileName, MAX_PATH);
  GetModuleFilename(hInstance, PChar(FileName), MAX_PATH-1);
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize <> 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        V1 := dwFileVersionMS shr 16;
        V2 := dwFileVersionMS and $FFFF;
        V3 := dwFileVersionLS shr 16;
        V4 := dwFileVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
    lblVersion.Caption := Format('%s %d.%d%d', [SVersion, V1, V2, V3]);
    if V4 <> 0 then
      lblVersion.Caption := lblVersion.Caption + '.' + IntToStr(V4);
  end
  else
    lblVersion.Caption := Format('%s %s', [SVersion, SUnknown]);
end;

end.


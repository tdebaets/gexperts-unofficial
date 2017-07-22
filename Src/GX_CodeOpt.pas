unit GX_CodeOpt;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls;

type
  TfmCodeOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pgeCodeOpt: TPageControl;
    tabPaths: TTabSheet;
    tabLayout: TTabSheet;
    tabFonts: TTabSheet;
    lblDatabase: TLabel;
    edPath: TEdit;
    sbBrowse: TSpeedButton;
    lblNetFile: TLabel;
    edNetFile: TEdit;
    sbNetFile: TSpeedButton;
    rbSide: TRadioButton;
    pnlSideSide: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    rbTop: TRadioButton;
    pnlTopBottom: TPanel;
    Shape3: TShape;
    Shape4: TShape;
    Label3: TLabel;
    Label4: TLabel;
    fcTreeview: TComboBox;
    fcEditor: TComboBox;
    Label5: TLabel;
    udTreeview: TUpDown;
    udEditor: TUpDown;
    eTreeview: TEdit;
    eEditor: TEdit;
    procedure sbBrowseClick(Sender: TObject);
    procedure sbNetFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure eNumericKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  GX_ConfigurationInfo, GX_GExperts, GX_GenFunc;

procedure TfmCodeOptions.sbBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edPath.Text;
  if GetDir(Self, Temp) then
    edPath.Text := Temp;
end;

procedure TfmCodeOptions.sbNetFileClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edNetFile.Text;
  if GetDir(Self, Temp) then
    edNetFile.Text := Temp;
end;

procedure TfmCodeOptions.FormCreate(Sender: TObject);
begin
  fcTreeview.Items.Assign(Screen.Fonts);
  fcEditor.Items.Assign(Screen.Fonts);
end;

procedure TfmCodeOptions.eNumericKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', #8]) then
    Key := #0;
end;

end.

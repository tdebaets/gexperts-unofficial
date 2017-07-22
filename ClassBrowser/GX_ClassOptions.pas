unit GX_ClassOptions;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, StdCtrls, Buttons, SpinIntEdit, Controls, ComCtrls, Classes, Forms;

type
  TfmClassOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    pcClassOptions: TPageControl;
    tshGeneric: TTabSheet;
    tshFilters: TTabSheet;
    gbxFonts: TGroupBox;
    lblTreeViewFont: TLabel;
    lblListViewFont: TLabel;
    gbxFiles: TGroupBox;
    lblStoragePath: TLabel;
    edStorage: TEdit;
    sbStoragePath: TSpeedButton;
    gbxFilters: TGroupBox;
    cbConstants: TCheckBox;
    cbMethods: TCheckBox;
    cbTypes: TCheckBox;
    cbVariables: TCheckBox;
    cbProperties: TCheckBox;
    cbPrivate: TCheckBox;
    cbProtected: TCheckBox;
    cbPublic: TCheckBox;
    cbPublished: TCheckBox;
    gbxDiagram: TGroupBox;
    cbTop: TCheckBox;
    lblEditorFont: TLabel;
    cbTreeView: TComboBox;
    cbListView: TComboBox;
    cbEditor: TComboBox;
    sTreeView: TSpinIntEdit;
    sListView: TSpinIntEdit;
    sEditor: TSpinIntEdit;
    cbStayInPackage: TCheckBox;
    gbxSearch: TGroupBox;
    cbParseRecursing: TCheckBox;
    cbAutoHide: TCheckBox;
    procedure sbStoragePathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  end;


implementation

{$R *.DFM}

uses
  GX_GenFunc;

procedure TfmClassOptions.sbStoragePathClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edStorage.Text;
  if GetDir(Self, Temp) then
    edStorage.Text := AddSlash(Temp);
end;

procedure TfmClassOptions.FormCreate(Sender: TObject);
begin
  cbTreeView.Items.Assign(Screen.Fonts);
  cbListView.Items.Assign(Screen.Fonts);
  cbEditor.Items.Assign(Screen.Fonts);
end;

end.

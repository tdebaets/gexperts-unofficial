unit GX_EditorShortcut;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfmEditorShortcut = class(TForm)
    gbxShortcut: TGroupBox;
    lblShortcut: TLabel;
    hkyShortcut: THotKey;
    btnCancel: TButton;
    btnOK: TButton;
  end;

implementation

{$R *.DFM}

end.

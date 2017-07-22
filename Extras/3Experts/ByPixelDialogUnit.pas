unit ByPixelDialogUnit;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, SpaceByPixelExpertUnit;

type
  TByPixelDialog = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    SpacingDirectionRadioGroup: TRadioGroup;
    PixelByEdit: TEdit;
    PixelByLabel: TLabel;
  end;

procedure ShowByPixelDialog(var SpacingDirection : TSpacingDirection; var nPixelBy : Word);

implementation

{$R *.DFM}

procedure ShowByPixelDialog(var SpacingDirection : TSpacingDirection; var nPixelBy : Word);
var
  ByPixelDialog : TByPixelDialog;
begin
  SpacingDirection := sdHorizontal;
  nPixelBy := 0;

  ByPixelDialog := TByPixelDialog.Create(NIL);
  try
    with ByPixelDialog do begin
      ShowModal;
      if ModalResult = mrOK then begin
        case SpacingDirectionRadioGroup.ItemIndex of
          0 : SpacingDirection := sdHorizontal;
          1 : SpacingDirection := sdVertical;
        end;{case}
        nPixelBy := StrToIntDef(PixelByEdit.Text, 0);
      end;{if}
    end;{with}
  finally
    ByPixelDialog.Free;
  end;{finally}
end;

end.

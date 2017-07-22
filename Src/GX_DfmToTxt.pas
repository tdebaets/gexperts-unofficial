unit GX_DfmToTxt;

// Original Author: John Hansen <John_Hansen@tcsmgmt.com>

{$I GX_CondDefine.inc}

interface

uses
  ExpertUtil, EditIntf;

type
  TDfm2TxtHelper = class(TObject)
  private
    fProjNotifier: TRLProjectNotifier;
    fModNotifier: TModuleNotifier;
    fExtension: string;
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    procedure ModAfterSave(Sender: TObject; ModuleInterface: TIModuleInterface);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FileExtension: string read fExtension write fExtension;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  SysUtils, Classes;

const
  DEFAULT_EXTENSION = '.txt';

{ TDfm2TxtHelper }

constructor TDfm2TxtHelper.Create;
begin
  inherited Create;
  fActive := False;
  fExtension := DEFAULT_EXTENSION;
end;

destructor TDfm2TxtHelper.Destroy;
begin
  fModNotifier.Free;
  fModNotifier := nil;
  fProjNotifier.Free;
  fProjNotifier := nil;
  inherited Destroy;
end;

procedure TDfm2TxtHelper.ModAfterSave(Sender: TObject; ModuleInterface: TIModuleInterface);
var
  TextName: string;
  FormIntf: TIFormInterface;
  BinaryStream: TFileStream;
  TextStream: TFileStream;
begin
  FormIntf := nil;
  BinaryStream := nil;
  TextStream := nil;
  // do nothing if we are not supposed to be active
  if not Active then Exit;
  try
    try
      // does this module have a form?
      FormIntf := ModuleInterface.GetFormInterface;
      if Assigned(FormIntf) then
      begin
        TextName := ChangeFileExt(FormIntf.FileName, FileExtension);
        {$IFOPT D+}SendDebug('DFM2TXT in AfterSave: name = ' + TextName);{$ENDIF}
        BinaryStream := TFileStream.Create(FormIntf.FileName, fmOpenRead or fmShareDenyNone);
        {$IFOPT D+}SendDebug('DFM2TXT in AfterSave: opened the DFM file for reading'); {$ENDIF}
        TextStream := TFileStream.Create(TextName, fmCreate);
        {$IFOPT D+}SendDebug('DFM2TXT in AfterSave: opened the DXT file for writing'); {$ENDIF}
        ObjectResourceToText(BinaryStream, TextStream);
        {$IFOPT D+}SendDebug('DFM2TXT in AfterSave: wrote file');{$ENDIF}
        // Synchronize the dates of the DFM and TXT
        FileSetDate(TextStream.Handle, FileGetDate(BinaryStream.Handle));
      end;
    finally
      FormIntf.Free;
      BinaryStream.Free;
      TextStream.Free;
    end;
  except
    // swallow exceptions
    on E: Exception do
      {$IFOPT D+}SendDebug('DFM2TXT AfterSave Exception: ' + E.Message);{$ENDIF}
  end;
end;

procedure TDfm2TxtHelper.SetActive(const Value: Boolean);
begin
  {$IFDEF GX_VER130_up}  // Delphi 5 + provide this by default
  Exit; // Code in OwnerList unit causes shutdown problems in D5??
  {$ENDIF GX_VER130_up}
  if Value = FActive then
    Exit;
  if Value then
  begin
    fProjNotifier := TRLProjectNotifier.Create(nil);
    fModNotifier := TModuleNotifier.Create(nil);
    fProjNotifier.ModuleNotifier := fModNotifier;
    fModNotifier.OnAfterSave := ModAfterSave;
  end
  else
  begin
    fProjNotifier.Free;
    fProjNotifier := nil;
    fModNotifier.Free;
    fModNotifier := nil;
  end;
  FActive := Value;
end;

end.


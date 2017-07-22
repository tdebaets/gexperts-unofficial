unit GX_EditWriter;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  SysUtils, Classes, EditIntf, ExptIntf, ToolIntf, Dialogs;

type
  TEditWriter = class(TObject)
  private
    FEditIntf: TIEditorInterface;
    ModIntf: TIModuleInterface;
    FPos: Integer;
    FEof: Boolean;
    FLineLength: Integer;
    function GetSyntaxHighlighter: TSyntaxHighlighter;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function GetColPos: Integer;
    procedure GotoCurrentPos;
    function GotoLine(LineNo: Integer): Boolean;
    procedure Insert(const Text: string);
    procedure DirectInsert(const Text: string);
    procedure WriteAtCurrentPos(TextToWrite: string);
    procedure ScrollToTopOfFile;
    property SyntaxHighlighter: TSyntaxHighlighter read GetSyntaxHighlighter;
  published
    property Eof: Boolean read FEof;
    property Pos: Integer read FPos;
  end;

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc;

const
  MAXSIZE = 4096;

constructor TEditWriter.Create(const FileName: string);
resourcestring
  SNoModuleIntf = 'EditWriter: No module interface';
  SNoEditorIntf = 'EditWriter: No editor interface';
  //SEditingNoKnownSourceFile = 'Error - you are trying to write to an unknown (source) file type.';
begin
  inherited Create;

  FPos := 0;
  FEof := False;

{  if not IsKnownSourceFile(FileName) then
  begin
    raise Exception.Create(SEditingNoKnownSourceFile);
  end;}

  { Get module interface }
  ModIntf := ToolServices.GetModuleInterface(FileName);
  if ModIntf = nil then
    raise Exception.Create(SNoModuleIntf);

  { Get Editor Interface }
  FEditIntf := ModIntf.GetEditorInterface;
  if FEditIntf = nil then
    raise Exception.Create(SNoEditorIntf);
end;

destructor TEditWriter.Destroy;
begin
  FEditIntf.Free;
  FEditIntf := nil;

  ModIntf.Free;
  ModIntf := nil;

  inherited Destroy;
end;

function TEditWriter.GotoLine(LineNo: Integer): Boolean;
var
  Lf: array[0..MAXSIZE] of Char;
  EditRead: TIEditReader;
  L: Integer;
  i: Integer;
  size: Integer;
resourcestring
  SCouldNotGetReader = 'Could not get reader interface to editor';
begin
  Result := False;
  EditRead := FEditIntf.CreateReader;
  if EditRead = nil then
    raise Exception.Create(SCouldNotGetReader);

  try
    L := 1;
    FPos := 0;
    while (L <> LineNo) and not FEof do
    begin
      size := EditRead.GetText(FPos, Lf, MAXSIZE);
      FEof := (size < MAXSIZE);
      if size = MAXSIZE then
      begin
        for i := MAXSIZE - 1 downto 0 do
          if lf[i] = #10 then
          begin
            lf[i + 1] := #0;
            size := i + 1;
            if lf[size] = #0 then
              Dec(size);
            Break;
          end;
      end;

      i := 0;
      while i <= size-1 do
      begin
        case lf[i] of
          #10:
            Inc(L);

          #13:
            begin
              Inc(L);
              if lf[i + 1] = #10 then
              begin
                Inc(i);
                Inc(FPos);
              end;
            end;

          #0:
            Break;
        end;

        Inc(i);
        Inc(FPos);
        if L = LineNo then
          Break;
      end;  // while
    end;

    if L = LineNo then
    begin
      size := EditRead.GetText(FPos, lf, 1024);
      i := 0;
      while (lf[i] <> #13) and (i <= size) do
        Inc(i);
      FLineLength := i;
    end;
  finally
    EditRead.Free;
  end;
end;

function TEditWriter.GetColPos: Integer;
var
  EditView: TIEditView;
  CurrentView: Integer;
begin
  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  try
    Result := EditView.GetPos(0).Col;
  finally
    EditView.Free;
  end;
end;

procedure TEditWriter.GotoCurrentPos;
var
  EditView: TIEditView;
  CurrentView: Integer;
  Ep: TEditPos;
  i: Integer;
begin
  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  try
    Ep := EditView.GetPos(0);
    GotoLine(Ep.Line);
    if Ep.Col - 1 > FLineLength then
    begin
      FPos := FPos + FLineLength;
      for i := FLineLength to Ep.Col - 1 do
      begin
        // NOTE: inserting spaces does not work! Scott Mattes, 98/11/25
        //! StH: What about inserting "x      x" and then deleting the "x"s?
        Insert(' ');
        Inc(FPos);
      end
    end
    else
      FPos := FPos + Ep.Col - 1;
  finally
    EditView.Free;
  end;
end;

resourcestring
  SCouldNotGetWriter = 'Could not get writer interface to editor';

procedure TEditWriter.Insert(const Text: string);
var
  EditWrite: TIEditWriter;
begin
  EditWrite := FEditIntf.CreateUndoableWriter;
  if EditWrite = nil then
    raise Exception.Create(SCouldNotGetWriter);
  try
    EditWrite.CopyTo(FPos);
    EditWrite.Insert(PChar(Text));
  finally
    EditWrite.Free;
  end;
end;

procedure TEditWriter.DirectInsert(const Text: string);
var
  EditWrite: TIEditWriter;
begin
  EditWrite := FEditIntf.CreateUndoableWriter;
  if EditWrite = nil then
    raise Exception.Create(SCouldNotGetWriter);
  try
    EditWrite.Insert(PChar(Text));
  finally
    EditWrite.Free;
  end;
end;

//*************************************************************
// Name: WriteAtCurrentPos
// Purpose: A replacement for GotoCurrentPos. This procedure
//          handles cases where the user wants the code
//          inserted beyond the end of the current line by
//          prepending spaces to the text to be inserted
//          and then calling Insert
//*************************************************************

//! StH: cross-check this routine with
//!     procedure TEditWriter.GotoCurrentPos;
//!  which might be doing the same, is at least similar.
//!  Consolidate.
procedure TEditWriter.WriteAtCurrentPos(TextToWrite: string);
var
  EditView: TIEditView;
  CurrentView: Integer;
  Ep: TEditPos;
  i: Integer;
begin
  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  try
    Ep := EditView.GetPos(0);
  finally
    EditView.Free;
  end;

  GotoLine(Ep.Line);
  // If the insert point will be after the end of the current
  // line, we need to pad the text to be inserted with leading
  // spaces.
  if Ep.Col - 1 > FLineLength then
  begin
    FPos := FPos + FLineLength;
    for i := FLineLength to Ep.Col - 2 do
      TextToWrite := ' ' + TextToWrite;
  end
  else
    FPos := FPos + Ep.Col - 1;
  // Now, write it to the edit window
  Insert(TextToWrite);
end;

procedure TEditWriter.ScrollToTopOfFile;
const
  TopPosition: TEditPos = (Col: 1; Line: 1);
var
  EditView: TIEditView;
  CurrentView: Integer;
begin
  Assert(FEditIntf.GetViewCount > 0);

  CurrentView := GetCurrentEditView(FEditIntf);
  Assert(CurrentView >= 0);

  EditView := FEditIntf.GetView(CurrentView);
  try
    EditView.TopPos := TopPosition;
    EditView.CursorPos := TopPosition;
  finally
    EditView.Free;
  end;
end;

function TEditWriter.GetSyntaxHighlighter: TSyntaxHighlighter;
begin
  Assert(FEditIntf <> nil);

  Result := FEditIntf.SetSyntaxHighlighter(shQuery);
end;

end.


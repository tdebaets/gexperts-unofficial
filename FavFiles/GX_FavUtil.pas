unit GX_FavUtil;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Classes;

type
  // Note: ftDelphi and etDelphi both have an analogous meaning
  // in C++Builder: ftDelphi is a synonym for a C++Builder (source) folder,
  // etDelphi is a (C++Builder source) file that should be executed by
  // the C++Builder IDE. ftDelphi and etDelphi must not be changed in name
  // since the streaming system uses them - if they were changed, all existing
  // FAVE.FAV files would be broken.
  TFolderType = (ftNormal, ftDelphi, ftBitmap, ftGlyph, ftHelp);
  TExecType = (etDelphi, etShell, etCustom, etProject);

  TGXFiles = class(TComponent)
  private
    FFolder: string;
    FFolderType: TFolderType;
  protected
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignFiles(Source: TGXFiles);
  published
    property Folder: string read FFolder write FFolder;
    property FolderType: TFolderType read FFolderType write FFolderType;
  end;

  TGXFile = class(TComponent)
  private
    FDName: string;
    FFileName: string;
    FDescription: string;
    FExecType: TExecType;
    FExecProg: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignFile(Source: TGXFile);
  published
    property Description: string read FDescription write FDescription;
    property FileName: string read FFileName write FFileName;
    property DName: string read FDName write FDName;
    property ExecType: TExecType read FExecType write FExecType;
    property ExecProg: string read FExecProg write FExecProg;
  end;

const
  MaxFolders = 5;
{$IFDEF GX_BCB}
  FolderNames: array[0..MaxFolders-1] of string = ('Normal', 'C++Builder', 'Bitmaps', 'Glyphs', 'Help File');
{$ELSE}
  FolderNames: array[0..MaxFolders-1] of string = ('Normal', 'Delphi', 'Bitmaps', 'Glyphs', 'Help File');
{$ENDIF GX_BCB}

var
  Root: TGXFiles;
  CompCount: Integer = 1;

procedure SetCompCount(Comp: TComponent);

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  SysUtils;

procedure SetCompCount(Comp: TComponent);
var
  i: Integer;
begin
  if Copy(Comp.Name, 1, 6) = 'Folder' then  // do not localize
    i := StrToInt(Copy(Comp.Name, 7, Length(Comp.Name)))  // do not localize
  else
    if Copy(Comp.Name, 1, 4) = 'File' then
      i := StrToInt(Copy(Comp.Name, 5, Length(Comp.Name)))
    else
      i := CompCount + 1;

  if i >= CompCount then
    CompCount := i + 1;
end;

constructor TGXFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'Folder' + IntToStr(CompCount);  // do not localize
  Inc(CompCount);
end;

procedure TGXFiles.AssignFiles(Source: TGXFiles);
var
  i: Integer;
begin
  Folder := Source.Folder;
  FolderType := Source.FolderType;
  for i := 0 to Source.ComponentCount - 1 do
    if Source.Components[i] is TGXFile then
    begin
      with TGXFile.Create(Self) do
        AssignFile(TGXFile(Source.Components[i]));
    end
    else
      with TGXFiles.Create(Self) do
        AssignFiles(TGXFiles(Source.Components[i]));
end;

procedure TGXFiles.ReadData(Reader: TReader);
var
  Component: TComponent;
begin
  Reader.Owner := Self;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    Component := Reader.ReadComponent(nil);
    { have to make sure there is no duplicate }
    SetCompCount(Component);
  end;
  Reader.ReadListEnd;
  if Reader.Owner <> nil then
    Reader.Owner := Self.Owner
  else
    Reader.Owner := Root;
end;

procedure TGXFiles.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to ComponentCount - 1 do
    Writer.WriteComponent(Components[i]);
  Writer.WriteListEnd;
end;

procedure TGXFiles.DefineProperties(Filer: TFiler);
var
  HasData: Boolean;
begin
  HasData := (ComponentCount > 0);
  Filer.DefineProperty('List', ReadData, WriteData, HasData); // do not localize
end;

constructor TGXFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'File' + IntToStr(CompCount); // do not localize
  Inc(CompCount);
end;

procedure TGXFile.AssignFile(Source: TGXFile);
begin
  Description := Source.Description;
  FileName := Source.FileName;
  DName := Source.DName;
  ExecType := Source.ExecType;
  ExecProg := Source.ExecProg;
end;

initialization
  RegisterClass(TGXFiles);
  RegisterClass(TGXFile);
  Root := TGXFiles.Create(nil);

finalization
  Root.Free;
  Root := nil;
end.


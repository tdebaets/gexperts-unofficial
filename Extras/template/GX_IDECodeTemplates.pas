unit GX_IDECodeTemplates;

interface

uses
  Classes;

type
  TIDECodeTemplateList = class;
  TIDECodeTemplate = class
  private
    FOwner : TIDECodeTemplateList;
    FDescription: String;
    FName: String;
    FCode: TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetDescription(const Value: String);
    procedure SetName(const Value: String);
  public
    constructor Create(aOwner : TIDECodeTemplateList); virtual;
    destructor  Destroy; override;
    property Name : String read FName write SetName;
    property Description : String read FDescription write SetDescription;
    property Code : TStrings read FCode write SetCode;
  end;

  TIDECodeTemplateList = class
  private
    FList : TList;
    function  GetTemplates(aIndex: integer): TIDECodeTemplate;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure ClearTemplates;
    function  Add : TIDECodeTemplate;
    function  Count : integer;
    function  IndexOf(const aName : string) : integer;
    procedure LoadFromFile(const aFilename : string);
    procedure SaveToFile(const aFilename : string);
    procedure Delete(aIndex : integer);
    property  Templates[aIndex : integer] : TIDECodeTemplate read GetTemplates;
  end;

implementation

uses
  SysUtils;
  
{ TIDECodeTemplate }

constructor TIDECodeTemplate.Create(aOwner : TIDECodeTemplateList);
begin
  inherited Create;
  FOwner := aOwner;
  FCode := TStringList.Create;
  FOwner.FList.Add(self);
end;

destructor TIDECodeTemplate.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

procedure TIDECodeTemplate.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
end;

procedure TIDECodeTemplate.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TIDECodeTemplate.SetName(const Value: String);
begin
  FName := Value;
end;

{ TIDECodeTemplateList }

function TIDECodeTemplateList.Add: TIDECodeTemplate;
begin
  result := TIDECodeTemplate.Create(self);
end;

procedure TIDECodeTemplateList.ClearTemplates;
var
  i : integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TIDECodeTemplate(FList[i]).Free;
  end;
  FList.Clear;
end;

function TIDECodeTemplateList.Count: integer;
begin
  result := FList.Count;
end;

constructor TIDECodeTemplateList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TIDECodeTemplateList.Delete(aIndex: integer);
begin
  TIDECodeTemplate(FList[aIndex]).Free;
  FList.Delete(aIndex);
end;

destructor TIDECodeTemplateList.Destroy;
begin
  ClearTemplates;
  inherited Destroy;
end;

function TIDECodeTemplateList.GetTemplates(aIndex: integer): TIDECodeTemplate;
begin
  result := TIDECodeTemplate(FList[aIndex]);
end;

function TIDECodeTemplateList.IndexOf(const aName: string): integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if AnsiCompareText(aName, Templates[i].Name) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TIDECodeTemplateList.LoadFromFile(const aFilename: string);
var
  i : integer;
  tmpSL : TStringList;
  ICT : TIDECodeTemplate;
begin
  ClearTemplates;
  tmpSL := TStringList.Create;
  try
    tmpSL.LoadFromFile(aFilename);
    // now create our templates from this file
    i := 0;
    ICT := nil;
    while i < tmpSL.Count do
    begin
      // we've found a template if the line starts with '['
      if Pos('[', tmpSL[i]) = 1 then
      begin
        // we've found the next template so add one to our list
        // before we do that delete the blank line (if there is one) from
        // the previous template
        if assigned(ICT) then
        begin
          if ICT.Code[ICT.Code.Count-1] = '' then
            ICT.Code.Delete(ICT.Code.Count-1);
        end;
        ICT := Self.Add;
        ICT.Name := Trim(Copy(tmpSL[i], 2, Pos('|', tmpSL[i])-2));
        ICT.Description := Trim(Copy(tmpSL[i], Pos('|', tmpSL[i])+1, Length(tmpSL[i])));
        inc(i);
      end;
      if assigned(ICT) then
      begin
        ICT.Code.Add(tmpSL[i]);
      end;
      inc(i);
    end;
  finally
    tmpSL.Free;
  end;
end;

procedure TIDECodeTemplateList.SaveToFile(const aFilename: string);
var
  i, j : integer;
  ICT : TIDECodeTemplate;
  tmpSL : TStringList;
begin
  tmpSL := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      ICT := Templates[i];
      tmpSL.Add('[' + ICT.Name + ' | ' + ICT.Description + ']');
      for j := 0 to ICT.Code.Count - 1 do
      begin
        tmpSL.Add(ICT.Code[j]);
      end;
      tmpSL.Add('');
    end;
    tmpSL.SaveToFile(aFilename);
  finally
    tmpSL.Free;
  end;
end;

end.

{***************************************************************
 *
 * Unit Name: GX_eSortLines
 * Purpose  : To sort the Lines selected in the editor
 * Author   : Scott Mattes, smattes@erols.com
 * History  :
 *
 ****************************************************************}

unit GX_eSortLines;

{$I GX_CondDefine.inc}

interface

uses
  ToolIntf,
  RzCSIntf,
  GX_uExperts,
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  GX_EditorExpert, ExtCtrls, ComCtrls;

type
  TSortLines = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Execute; override;
//    procedure SaveSettings; override;
//    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TfmSortLines = class(TForm)
    btnDefaultSort: TButton;
    btnCancel      : TButton;
    btOk           : TButton;
    edtSortPairs   : TEdit;
    Label2         : TLabel;
    lblNote        : TLabel;
    lblRuler       : TLabel;
    lsbSortedLines : TListBox;
    mmoSortedLines : TMemo;
    Panel1         : TPanel;
    Panel2         : TPanel;
    pnlAltSort     : TPanel;
    rbnAscending: TRadioButton;
    rbnDescending: TRadioButton;

    procedure btnDefaultSortClick        ( Sender : TObject);
    procedure edtSortPairsChange     ( Sender : TObject);
    procedure FormActivate           ( Sender : TObject);
    procedure FormDestroy            ( Sender : TObject);
    procedure FormKeyPress           ( Sender : TObject; var Key: Char);
    procedure LoadMemo;
    procedure mmoSortedLinesMouseDown( Sender : TObject;
                                       Button : TMouseButton;
                                       Shift: TShiftState;
                                       X, Y: Integer);
    procedure mmoSortedLinesMouseUp  ( Sender : TObject;
                                       Button: TMouseButton;
                                       Shift  : TShiftState;
                                       X, Y: Integer);
    procedure rbnAscendingClick(Sender: TObject);
  private
    ClearEdit       : Boolean;
    DefaultSortPair : Boolean;
    MaxLength       : integer;
    CodeList        : TStringList;
    SortList        : TStringList;

    procedure MakeSortList;
    procedure SortLines;
  end;

  TGxSortLines = class(TGX_Expert)
//  private
//    FSomeData: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetDisplayName: string; override;
    function GetMenuCaption: string; override;
    function GetMenuMask: string; override;
    function GetMenuName: string; override;
    function GetName: string; override;
    function IconFileName: string; override;

    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
//    {$IFDEF GX_UseNativeToolsApi}
    procedure Click(Sender: TObject); override;
//    {$ELSE}
//    procedure Click(Sender: TIMenuItemIntf); override;
//    {$ENDIF GX_UseNativeToolsApi}
  end;


implementation

{$R *.DFM}

uses
  GX_DbugIntf,
  Menus,
  ExptIntf, EditIntf, GX_EditRead, GX_uGenFunc, ExpertUtil,
  Registry, GX_GExperts, GX_uConfigurationInfo;


{ TGxSortLines }

constructor TGxSortLines.Create;
begin
  inherited Create;

  HasConfigOptions := True;
  HasMenuItem := True;
//
//  // Assign a default to your data
//  FSomeData := 42;
end;

destructor TGxSortLines.Destroy;
begin
  inherited Destroy;
end;

function TGxSortLines.GetMenuCaption: string;
resourcestring
{#ToDo1 -  how is the 'proper' way to get the short cut on the menu?}
  SMenuCaption = '&Sort Selected Lines Shift+Ctl+Alt+S';
begin
  Result := SMenuCaption;
end;

function TGxSortLines.GetMenuName: string;
begin
  Result := 'GX_SortSelectedLines';
end;

function TGxSortLines.GetMenuMask: string;
begin
  // File extensions for which the menu item is available;
  // an empty string means always available
  Result := '';
end;

function TGxSortLines.GetName: string;
begin
  Result := 'SortSelectedLines';
end;

function TGxSortLines.GetDisplayName: string;
resourcestring
  SDisplayName = 'Sort Selected Lines';
begin
  Result := SDisplayName;
end;

procedure TGxSortLines.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button :-)';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

procedure TGxSortLines.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // write your settings here, for example
//    RegIni.WriteInteger('SampleExpert', 'TestSetting', FSomeData);
  finally
    RegIni.Free;
  end;

end;

procedure TGxSortLines.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // read your settings here, for example
//    FSomeData := RegIni.ReadInteger('SampleExpert', 'TestSetting', FSomeData);
  finally
    RegIni.Free;
  end;

  inherited LoadSettings;
end;

//{$IFDEF GX_UseNativeToolsApi}
procedure TGxSortLines.Click(Sender: TObject);
//{$ELSE}
//procedure TGxSortLines.Click(Sender: TIMenuItemIntf);
//{$ENDIF GX_UseNativeToolsApi}
var
  EditIntf : TIEditorInterface;
  ModIntf  : TIModuleInterface;

begin
  with TfmSortLines.Create(nil) do
  try
    if ShowModal = mrOK
    then begin
           // First, we need the module interface
           ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile);
           if ModIntf = nil
           then raise Exception.Create('EditWrite:No Module Interface');

           // so that we can get the Editor interface
           EditIntf := ModIntf.GetEditorInterface;
           if EditIntf = nil
           then raise Exception.Create('EditWrite:No Editor Interface');

           // so that we can replace the selected text
           ReplaceSelection(EditIntf, 0, CodeList.Text);

           // but we gotta clean up after ourselves
           EditIntf.Free; // we don't have to set local var to nil

           ModIntf.Free; // we don't have to set local var to nil

           if CodeList <> nil
           then begin
                  CodeList.Free;
                  CodeList := nil;
                end;
         end;
  finally
    // now, do away with the form
    Release;
  end;
end;

function TGxSortLines.IconFileName: string;
begin
  Result := 'FileNameWithoutExtension';
end;

{ TSortLines }

constructor TSortLines.Create;
begin
  ShortCut := scCtrl + scAlt + Ord('S');

  FName := 'Sort Lines in Editor';
  FButtonNo := 67;

end;

procedure TSortLines.Execute;
var
  CodeList : TStringList;
  EditIntf : TIEditorInterface;
  EditRead : TEditReader;
  ModIntf  : TIModuleInterface;
  BlkTypOld: TBlockType;
//  PosStart : TCharPos;
  PosEnd   : TCharPos;

begin
  CodeList      := TStringList.Create;
  // Since this edit reader is destroyed almost
  // almost immediately, do not call FreeFileData
  EditRead      := TEditReader.Create(ToolServices.GetCurrentFile);

  // First, we need the module interface
  ModIntf := ToolServices.GetModuleInterface(ToolServices.GetCurrentFile);
  if ModIntf = nil
  then raise Exception.Create('EditWrite:No Module Interface');

  // so that we can get the Editor interface
  EditIntf := ModIntf.GetEditorInterface;
  if EditIntf = nil
  then raise Exception.Create('EditWrite:No Editor Interface');

  BlkTypOld := editintf.blocktype;
  editintf.blocktype := btLine;

  // if selected area ends on line, but before first char
  // we adjust the area so that we don't effect that line
  PosEnd := editintf.blockafter;
  if PosEnd.CharIndex = 0
  then begin
         PosEnd.Line := PosEnd.Line - 1;
         PosEnd.CharIndex := High(PosEnd.CharIndex);
         editintf.BlockAfter := PosEnd;
       end;

  CodeList.Text := EditRead.GetBlock;

  EditRead.Free; // we don't have to set local var to nil

  if CodeList.Count = 0
  then begin
         MessageDlg('There are no lines selected!', mtInformation, [mbOK], 0);
         Exit;
       end; // if/then

  CodeList.Sorted := True;

  // replace the selected text
  ReplaceSelection(EditIntf, 0, CodeList.Text);

  // restore the blocktype to what it was
  editintf.blocktype := BlkTypOld;

  // but we gotta clean up after ourselves
  EditIntf.Free; // we don't have to set local var to nil

  ModIntf.Free; // we don't have to set local var to nil

  if CodeList <> nil
  then begin
         CodeList.Free;
       end;
//begin
// SendDebug('tsortlines.execute');
end;

procedure TSortLines.GetHelpString(List: TStrings);
begin
  list.Text := 'tsortlines';
end;

{ TfmSortLines }

//*********************************************************
//    Name: TfmSortLines.btnDefaultSort
// Purpose: Redo the display to the default sort order
//*********************************************************
procedure TfmSortLines.btnDefaultSortClick(Sender: TObject);
begin
  CodeList.Sorted           := True;
  LoadMemo;
//  mmoSortedLines.Lines.Text := CodeList.Text;

  edtSortPairs.Text         := '1-' + IntToStr( MaxLength );

  DefaultSortPair           := True;
end;

//*********************************************************
//    Name: TfmSortLines.edtSortPairsChange
// Purpose: when user changes desired sort order, change it
//   Notes: resorts the display after each change
//*********************************************************
procedure TfmSortLines.edtSortPairsChange(Sender: TObject);
begin
  if Length( edtSortPairs.Text ) = 0
  then begin
         CodeList.Sorted := True;
         LoadMemo;
//         mmoSortedLines.Lines.Text := CodeList.Text ;
         Exit;
       end;

  MakeSortList;
  SortLines;
end;

//*********************************************************
//    Name: TfmSortLines.FormActivate
// Purpose:
//   Notes:
//*********************************************************
procedure TfmSortLines.FormActivate(Sender: TObject);
var
  EditRead : TEditReader;
  nLoop    : integer;

begin
  CodeList      := TStringList.Create;
  // Since this edit reader is destroyed almost
  // almost immediately, do not call FreeFileData
  EditRead      := TEditReader.Create(ToolServices.GetCurrentFile);

  CodeList.Text := EditRead.GetBlock;

  EditRead.Free; // we don't have to set local var to nil
{#ToDo1 - maybe this shouldn't be here?}
  if CodeList.Count = 0
  then begin
         MessageDlg('There are no lines selected!', mtInformation, [mbOK], 0);
         modalresult := mrCancel;
         Close;
       end; // if/then

  CodeList.Sorted := True;
// figure out the longest line length
  for nLoop := 0 to pred( CodeList.Count ) do
      MaxLength := max( MaxLength, Length( CodeList[ nLoop ] ) );

  LoadMemo;
//  mmoSortedLines.Lines.Text := CodeList.Text;
  edtSortPairs.Text         := '1-' + IntToStr( MaxLength );
  DefaultSortPair           := True;
end;

//*********************************************************
//    Name: TfmSortLines.FormDestroy
// Purpose: clean up some stuff I made
//*********************************************************
procedure TfmSortLines.FormDestroy(Sender: TObject);
begin
  if CodeList <> nil
  then begin
         CodeList.Free;
         CodeList := nil;
       end;

  if SortList <> nil
  then begin
         SortList.Free;
         SortList := nil;
       end;
end;

//*********************************************************
//    Name: TfmSortLines.FormKeyPress
// Purpose: Close form if escape is pressed
//*********************************************************
procedure TfmSortLines.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27
  then begin
         key := #00;
         Close;
       end;
end;

//*********************************************************
//    Name: TfmSortLines.MakeSortList
// Purpose: make up a comma delimited sort list to make it
//          real easy to loop through
//   Notes: the user enters '1-4,10-15' and we change it to
//          '1,4,10,15' before adding it to the stringlist
//*********************************************************
procedure TfmSortLines.LoadMemo;
var
  nLoop : integer;

begin
//codesite.SendInteger('num of Lines', codelist.Count );
  try
    mmoSortedLines.Lines.Text := CodeList.Text;
  except
//CodeSite.SendMsg( 'could not load the memo the easy way' );
//CodeSite.SendInteger( 'size of codelist.text', Length( codelist.Text ) );
    mmoSortedLines.Clear;
    mmoSortedLines.Lines.beginupdate;
    for nLoop := 0 to pred( CodeList.Count ) do
    begin
      try
        mmoSortedLines.Lines.Add( CodeList[ nLoop ] );
      except
//codesite.sendmsg( 'end of loading one at a time' );
//codesite.SendInteger('num of Lines', mmosortedlines.Lines.Count );
        MessageDlg( 'Only ' +
                    IntToStr( mmoSortedLines.Lines.Count ) +
                    ' could be loaded for display.'+
                    #13+#10+
                    #13+#10+
                    'This will NOT effect how many lines will be sorted!', mtInformation, [mbOK], 0);
        break;
      end; // try/except
    end; // for
    mmoSortedLines.Lines.Endupdate;
  end; // try/except
//codesite.SendInteger('num of Lines', mmosortedlines.Lines.Count );
end;

procedure TfmSortLines.MakeSortList;

  // because StringReplace is not available in D3
  function StrRep( sText : string;
                          inChar : string;
                          outChar : string ) : string;
  var
    nLoop : integer;

  begin
    Result := '';
    for nLoop := 1 to Length( sText ) do    // Iterate
        if sText[ nLoop ] = inChar
        then Result := Result + outChar
        else Result := Result + sText[ nLoop ];
  end;

begin
  if SortList = nil
  then SortList := TStringList.Create;

  if Length( edtSortPairs.Text ) = 0
  then SortList.Text      := '1,' +
                             IntToStr( MaxLength )
  else SortList.CommaText := StrRep( edtSortPairs.Text,
                                            '-',
                                            ','
                                          );
end;

//*********************************************************
//    Name: TfmSortLines.mmoSortedLinesMouseDown
// Purpose: the user is going to use the mouse to change the
//          default sort order
//   Notes:
//*********************************************************
procedure TfmSortLines.mmoSortedLinesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//CodeSite.SendMsg( 'entering mousedown' );
  if DefaultSortPair
  then begin
         DefaultSortPair := False;
         ClearEdit       := True;
//         edtSortPairs.Text := '';
       end;
end;

//*********************************************************
//    Name: TfmSortLines.mmoSortedLinesMouseUp
// Purpose: Update the sort pair list with the columns the
//          user just hilighted
//   Notes: does not Handle the case where the user starts
//          hilighting on one line and stops on another
//*********************************************************
procedure TfmSortLines.mmoSortedLinesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ColPair : string;
  comma   : string;
  nLoop   : integer;

begin
//CodeSite.SendMsg( 'memo entering mouseup' );
//  if DefaultSortPair = False // Length( edtSortPairs.Text ) > 0
//  then comma := ', '
//  else comma := '';

  ColPair := '';
  // look for the line break occurring before the selected columns
  for nLoop := mmoSortedLines.selstart downto 2 do
  begin
    if ( mmoSortedLines.Text[ nLoop-2 ] = #13 ) and
       ( mmoSortedLines.Text[ nLoop-1 ] = #10 )
    then begin
           ColPair := comma +
                      IntToStr( mmoSortedLines.selstart - nLoop + 2 ) +
                      '-' +
                      IntToStr( mmoSortedLines.selstart +
                                mmoSortedLines.sellength -
                                nLoop + 1
                              );
           break;
         end; // if/then
  end; // for

  // must have been the First line
  if length( ColPair ) = 0
  then ColPair := comma +
                  IntToStr( mmoSortedLines.selstart + 1 ) +
                  '-' +
                  IntToStr( mmoSortedLines.selstart +
                            mmoSortedLines.sellength
                          ) ;

  if DefaultSortPair = False // Length( edtSortPairs.Text ) > 0
  then begin
         if ClearEdit
         then clearEdit := False
         else ColPair := edtSortPairs.Text +
                         ', ' +
                         ColPair;
       end;
  edtSortPairs.Text := ColPair;


//  MakeSortList;
//  SortLines;
end;

//*********************************************************
//    Name: TfmSortLines.SortLines
// Purpose: here we set up the sort key for each line and
//          do the sorting
//*********************************************************
procedure TfmSortLines.SortLines;
var
  LineSortKey : string;
  nLoopLines  : integer;
  nLoopPairs  : integer;
  StartCol    : integer;
  TempLine    : string;

begin
  CodeList.Sorted := False;
  // we need an even number of entries
  if ( SortList.Count mod 2 ) <> 0
  then Exit; // SortList.Add( IntToStr( MaxLength ) );

  // construct the sort key for each line and
  // Add it to the front of each line
  for nLoopLines := 0 to pred( CodeList.Count ) do
  begin
    LineSortKey := '';
    nLoopPairs := 0;
    TempLine := Copy( CodeList[ nLoopLines ] +
                      StringOfChar( ' ', MaxLength ),
                      1,
                      MaxLength
                     );

    repeat
      LinesortKey := LinesortKey +
                     Copy( TempLine,
                           StrToInt( SortList[ nLoopPairs ] ),
                           StrToInt( SortList[ nLoopPairs + 1 ] ) - StrToInt( SortList[ nLoopPairs ] )
                         );
      nLoopPairs := nLoopPairs + 2
    until nLoopPairs >= Sortlist.Count;

    CodeList[ nLoopLines ] := LineSortKey +
                              CodeList[ nLoopLines ];
  end; // for nLoopLines

  // establish the default sort order of ascending
  CodeList.Sorted := True;
  CodeList.Sorted := False;

  StartCol        := Length( LineSortKey ) + 1;

  if rbnAscending.Checked
  then for nLoopLines := 0 to pred( CodeList.Count ) do
         CodeList[ nLoopLines ] := Copy( CodeList[ nLoopLines ],
                                         StartCol,
                                         MaxLength
                                       )
  else for nLoopLines := pred( CodeList.Count ) downto 0 do
         begin
           CodeList.Add( Copy( CodeList[ nLoopLines ],
                               StartCol,
                               MaxLength
                              )
                       );
           CodeList.Delete( nLoopLines );
         end;


  mmoSortedLines.Clear; // be sure to start fresh
  LoadMemo;
end;

procedure TfmSortLines.rbnAscendingClick(Sender: TObject);
begin
  MakeSortList;
  SortLines;
end;


initialization
  RegisterGX_Expert(TGxSortLines);
  RegisterEditorExpert(TSortLines);
end.


{***************************************************************
 *
 * Unit Name: GX_eAlign
 * Purpose  : To align the Lines selected in the editor
 *            by a list of chars
 * Author   : Scott Mattes, smattes@erols.com
 * History  :
 *
 ****************************************************************}

unit GX_eAlign;

{$I GX_CondDefine.inc}

interface

uses
  ToolIntf,
  RzCSIntf,
  GX_uExperts,
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  GX_EditorExpert, ExtCtrls, ComCtrls;

type
  TAlign = class(TEditorExpert)
  public
    fAlignChars : string;
    constructor Create; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

//  TGxAlign = class(TGX_Expert)
//  private
//    fAlignChars: String;
//  public
//    constructor Create; override;
//    destructor Destroy; override;
//
//    function GetDisplayName: string; override;
//    function GetMenuCaption: string; override;
//    function GetMenuMask   : string; override;
//    function GetMenuName   : string; override;
//    function GetName       : string; override;
//    function IconFileName  : string; override;
//
//    procedure Configure; override;
//    procedure LoadSettings; override;
//    procedure SaveSettings; override;
////    {$IFDEF GX_UseNativeToolsApi}
//    procedure Click(Sender : TObject); override;
////    {$ELSE}
////    procedure Click(Sender: TIMenuItemIntf); override;
////    {$ENDIF GX_UseNativeToolsApi}
//  end;


implementation

uses
  GX_DbugIntf,
  Menus,
  ExptIntf, EditIntf, GX_EditRead, GX_uGenFunc, ExpertUtil,
  Registry, GX_GExperts, GX_uConfigurationInfo;


//{ TGxAlign }
//
//constructor TGxAlign.Create;
//begin
//  inherited Create;
//
//  HasConfigOptions := True;
//  HasMenuItem := True;
//
//  // Assign a default to your data
//  fAlignChars := ':'  +#13#10+
//                 ':=' +#13#10+
//                 ','  +#13#10+
//                 '='  +#13#10+
//                 '<'  +#13#10+
//                 '>'  +#13#10+
//                 '/'  +#13#10+
//                 '+'  +#13#10+
//                 '-';
//end;
//
//destructor TGxAlign.Destroy;
//begin
//  inherited Destroy;
//end;
//
//function TGxAlign.GetMenuCaption: string;
//resourcestring
//{#ToDo1 -  how is the 'proper' way to get the short cut on the menu?}
//  SMenuCaption = '&Align Selected Lines Shift+Ctl+Alt+Z';
//begin
//  Result := SMenuCaption;
//end;
//
//function TGxAlign.GetMenuName: string;
//begin
//  Result := 'GX_AlignSelectedLines';
//end;
//
//function TGxAlign.GetMenuMask: string;
//begin
//  // File extensions for which the menu item is available;
//  // an empty string means always available
//  Result := '';
//end;
//
//function TGxAlign.GetName: string;
//begin
//  Result := 'AlignSelectedLines';
//end;
//
//function TGxAlign.GetDisplayName: string;
//resourcestring
//  SDisplayName = 'Align Selected Lines';
//begin
//  Result := SDisplayName;
//end;
//
//procedure TGxAlign.Configure;
//resourcestring
//  SYouClickedConfigure = 'You clicked the Configuration button :-)';
//begin
//  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
//end;
//
//procedure TGxAlign.SaveSettings;
//var
//  RegIni: TRegIniFile;
//begin
//  inherited SaveSettings;
//
////  fAlignChars := lsbAlignList.Text;
//  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
//  try
//    // write your settings here, for example
//    RegIni.WriteString('AlignExpert', 'AlignChars', fAlignChars);
//  finally
//    RegIni.Free;
//  end;
//
//end;
//
//procedure TGxAlign.LoadSettings;
//var
//  RegIni    : TRegIniFile;
//  TempChars : string;
//
//begin
//  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
//  try
//    // read your settings here, for example
//    TempChars := RegIni.ReadString('AlignExpert', 'AlignChars', fAlignChars);
//    if Length( TempChars ) > 0
//    then fAlignChars := TempChars;
//  finally
//    RegIni.Free;
//  end;
//
//  inherited LoadSettings;
//end;
//
////{$IFDEF GX_UseNativeToolsApi}
//procedure TGxAlign.Click(Sender: TObject);
////{$ELSE}
////procedure TGxAlign.Click(Sender: TIMenuItemIntf);
////{$ENDIF GX_UseNativeToolsApi}
////var
////  EditIntf : TIEditorInterface;
////  ModIntf  : TIModuleInterface;
////
//begin
//  with TfmAlign.Create(nil) do
//  try
//    lsbAlignList.Items.Text := fAlignChars;
//    if ShowModal = mrOK
//    then begin
//           if lsbAlignList.Items.Text <> fAlignChars
//           then SaveSettings;
//         end;
//  finally
//    // now, do away with the form
//    Release;
//  end;
//end;
//
//function TGxAlign.IconFileName: string;
//begin
//  Result := 'FileNameWithoutExtension';
//end;

{ TAlign }

constructor TAlign.Create;
begin
  ShortCut := scCtrl + {scAlt +} Ord('A');

  FName := 'Align Lines in Editor';
//  FButtonNo := 67;
end;

procedure AlignLines( alist : TStringList );
var
  nLoopChars : integer;
  nLoopLines : integer;
  CharList   : TStringList;
  CurPos : integer;
  maxPos     : integer;
  NewLine : string[255];

begin
  CharList := TStringList.Create;
  CharList.Text := ':'  +#13#10+
                   ':=' +#13#10+
                   ','  +#13#10+
                   '='  +#13#10+
                   '<'  +#13#10+
                   '>'  +#13#10+
                   '<=' +#13#10+
                   '>=' +#13#10+
                   '*'  +#13#10+
                   '/'  +#13#10+
                   '+'  +#13#10+
                   '-';

  for nLoopChars := 0 to pred( CharList.Count ) do
  begin
    // find maxpos of the current character
    maxpos := 0;
    for nLoopLines := 0 to pred( aList.Count ) do
    begin
      maxpos := max( maxpos,
                     Pos( CharList[ nLoopChars ],
                          aList[ nLoopLines ]
                        )
                   );
    end; // for nLoopLines

    // change lines based on maxpos of current character
    for nLoopLines := 0 to pred( aList.Count ) do
    begin
      FillChar( NewLine, SizeOf( NewLine ), ' ' );
      CurPos := Pos( charlist[ nLoopChars ], aList[ nLoopLines ] );

      // if there is nothing to do here, let's move on
      if curpos = 0
      then continue;

      // get the first part of the line, minus the special char
      newline := Copy(
                       Copy( aList[ nLoopLines ],
                             1,
                             curpos - 1
                           ) + newline,
                       1,
                       maxpos - 1) +
      // put in the special char
                 charlist[nloopchars] +
      // put the rest of the line after the new pos of special char
                 Copy( aList[nlooplines],
                       curpos + Length(charlist[nloopchars]),
                       999
                     );

      // save line to list
      aList[nlooplines] := newline;
    end; // for nLoopLines
  end;  // for nLoopChars
  CharList.Free;
end;

procedure TAlign.Execute;
var
  CodeList : TStringList;
  EditIntf : TIEditorInterface;
  EditRead : TEditReader;
  ModIntf  : TIModuleInterface;
  BlkTypOld: TBlockType;
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

  AlignLines(codelist);

  // so that we can replace the selected text
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
// SendDebug('TAlign.execute');
end;

procedure TAlign.GetHelpString(List: TStrings);
begin
  list.Text := 'TAlign';
end;

procedure TAlign.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // write your settings here, for example
    RegIni.WriteString('AlignExpert', 'AlignChars', fAlignChars);
  finally
    RegIni.Free;
  end;

end;

procedure TAlign.LoadSettings;
var
  RegIni    : TRegIniFile;
  TempChars : string;

begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // read your settings here, for example
    TempChars := RegIni.ReadString('AlignExpert', 'AlignChars', fAlignChars);
    if Length( TempChars ) > 0
    then fAlignChars := TempChars;
  finally
    RegIni.Free;
  end;

  inherited LoadSettings;
end;

initialization
//  RegisterGX_Expert(TGxAlign);
  RegisterEditorExpert(TAlign);
end.


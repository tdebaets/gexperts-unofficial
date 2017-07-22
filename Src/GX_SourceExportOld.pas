unit GX_SourceExportOld;

{$I GX_CondDefine.inc}

{$IFNDEF SYNTAXMEMO}
interface implementation
{$ELSE SYNTAXMEMO}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SyntaxEd, SynParse, StdCtrls, ToolIntf, ExptIntf, GX_EditReader, GX_Experts,
  GX_GenFunc, ExtCtrls;

type
  TfmSourceExportOld = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    SyntaxMemo: TSyntaxMemo;
    dlgSaveFile: TSaveDialog;
    btnOutputStyle: TButton;
    btnClipboard: TButton;
    gbxScope: TGroupBox;
    rbSelected: TRadioButton;
    rbFile: TRadioButton;
    bvlQuestion: TBevel;
    bvlPreview: TBevel;
    lDescription: TLabel;
    btnHelp: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOutputStyleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rbSelectedClick(Sender: TObject);
    procedure rbFileClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    FSavePath: string;
    FHighlighterToUse: TGXSyntaxParser;
  public
    procedure SaveSettings;
    procedure LoadSettings;
  end;

  TSourceExportExpert = class(TGX_Expert)
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  EditIntf,
  GX_GExperts, Clipbrd, Registry, GX_ConfigurationInfo;

procedure TfmSourceExportOld.rbSelectedClick(Sender: TObject);
var
  TempString: string;
  EditRead: TEditReader;
begin
  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
  try
    TempString := EditRead.GetBlock;
    SyntaxMemo.Text := TempString;
  finally
    EditRead.Free;
  end;
end;

procedure TfmSourceExportOld.rbFileClick(Sender: TObject);
var
  EditRead: TEditReader;
  MemStream: TMemoryStream;
begin
  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
  try
    MemStream := TMemoryStream.Create;
    try
      EditRead.SaveToStream(MemStream);
      MemStream.Position := 0;
      SyntaxMemo.LoadFromStream(MemStream);
    finally
      MemStream.Free;
    end;
  finally
    EditRead.Free;
  end;
end;

// Adds data from a stream to the clipboard in specified format.
procedure SaveStreamToClipboardFormat(const ClipboardFormat: Word; Stream: TStream);
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Integer;
begin
  Stream.Position := 0;
  Size := Stream.Size;

  Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Size);
  try
    DataPtr := GlobalLock(Data);
    try
      Stream.ReadBuffer(DataPtr^, Size);
      Clipboard.SetAsHandle(ClipboardFormat, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    on E: Exception do
    begin
      GlobalFree(Data);
    end;
  end;
end;


procedure TfmSourceExportOld.btnSaveClick(Sender: TObject);
const
  ClpRTFFormat  = 'Rich Text Format'; // do not localize
  ClpHTMLFormat = 'HTML Format';      // do not localize
  TerminatingNull: Char = #0; // Needs to be global initialized variable

var
  HTMLStream: TMemoryStream;
  RTFStream: TMemoryStream;
  CF_HTML: Word;
  CF_RTF: Word;

  CurrentIdeFolder: string;
begin
  Screen.Cursor := crHourGlass;
  try
    SyntaxMemo.SaveFormat := sfHTML;
    dlgSaveFile.InitialDir := FSavePath;
    if Sender = btnSave then
    begin
      CurrentIdeFolder := GetCurrentDir;
      try
        if dlgSaveFile.Execute then
        begin
          FSavePath := ExtractFilePath(ExpandFileName(dlgSaveFile.FileName));
          SyntaxMemo.SaveToFile(dlgSaveFile.FileName);

          ModalResult := mrOK;
        end;
      finally
        SetCurrentDir(CurrentIdeFolder);
      end;
    end
    else
    begin
      HTMLStream := TMemoryStream.Create;
      RTFStream := TMemoryStream.Create;
      try
        SyntaxMemo.SaveFormat := sfHTML;
        SyntaxMemo.SaveToStream(HTMLStream);
        HTMLStream.Write(TerminatingNull, SizeOf(TerminatingNull));

        SyntaxMemo.SaveFormat := sfRTF;
        SyntaxMemo.SaveToStream(RTFStream);
        RTFStream.Write(TerminatingNull, SizeOf(TerminatingNull));

        Clipboard.Open;
        try
          Clipboard.Clear;
          CF_HTML:= RegisterClipboardFormat(ClpHTMLFormat);
          CF_RTF := RegisterClipboardFormat(ClpRTFFormat);
          // Standard text gives HTML code
          SaveStreamToClipboardFormat(CF_TEXT, HTMLStream);
          // #TODO2 HTML Format should have an header. (defined by Microsoft)
          // but Outlook Express and Eudora seem to work without this header.
          // See: http://msdn.microsoft.com/workshop/networking/clipboard/htmlclipboard.asp
          SaveStreamToClipboardFormat(CF_HTML, HTMLStream);
          SaveStreamToClipboardFormat(CF_RTF, RTFStream);
        finally
          Clipboard.Close;
        end;
      finally
        HTMLStream.Free;
        RTFStream.Free;
      end;

      ModalResult := mrOK;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmSourceExportOld.FormCreate(Sender: TObject);
var
  EditRead: TEditReader;
  TempString: string;
begin
  LoadSettings;
  try
    FHighlighterToUse := gxpPAS; //! StH: gxpNone??

    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
    if EditRead <> nil then
    try
      {$IFOPT D+}SendDebug('Geting Edit Block');{$ENDIF}
      TempString := EditRead.GetBlock;
      {$IFOPT D+}SendDebug('Edit Block: ' + TempString);{$ENDIF}
      if Length(TempString) < 2 then
      begin
        rbSelected.Enabled := False;
        rbFile.Checked := True;
      end
      else
      begin
        // This will trigger rbSelected.Click
        rbSelected.Checked := True;
      end;

      case EditRead.SyntaxHighlighter of
        shPascal:
          FHighlighterToUse := gxpPAS;

      {$IFDEF GX_VER110_up}
        shC:
          FHighlighterToUse := gxpCPP;
      {$ENDIF GX_VER110_up}

        shSQL:
          FHighlighterToUse := gxpSQL;
      else
        FHighlighterToUse := gxpPAS; //! StH: really make Pascal the default ? gxpNone???
      end;
    finally
      EditRead.Free;
    end;

    SyntaxMemo.Parser2 := GetSyntaxParser(FHighlighterToUse);

    case FHighlighterToUse of
      gxpPAS:
        begin
          with SyntaxMemo.Parser2 as TSyntaxMemoParser do
          begin
            UseRegistry := True;
            RegistryKey := ConfigInfo.RegKey + '\GExperts\SourceExport';
            StylesFromRegistry(True, '');
          end;
          {$IFOPT D+}SendDebug('UpdateEditors');{$ENDIF}
        end;
    else
       //! StH: We would want to add more customization
       //! options for other highlighters here?
    end; // case

    SyntaxMemo.ActiveParser := 2;
    SyntaxMemo.Options := SyntaxMemo.Options - [smoShowGutter];

  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmSourceExportOld.btnOutputStyleClick(Sender: TObject);
begin
  SyntaxMemo.ModifyProperties;
  {$IFOPT D+}SendDebug('Saving color styles');{$ENDIF}

  //! StH: FIXME - save other customized settings, too?
  if FHighlighterToUse = gxpPAS then
    TSyntaxMemoParser(SyntaxMemo.Parser2).StylesToRegistry;
  {$IFOPT D+}SendDebug('Saved color styles');{$ENDIF}
end;

constructor TSourceExportExpert.Create;
begin
  inherited Create;
  ShortCut := 0;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

function TSourceExportExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Source Export...';
begin
  Result := SMenuCaption;
end;

function TSourceExportExpert.GetMenuName: string;
begin
  Result := 'GX_SourceExport';
end;

function TSourceExportExpert.GetMenuMask: string;
begin
  Result := '*.PAS;*.DPR;*.DPK;*.INC;*.TXT;*.CPP;*.HPP;*.C;*.H';
end;

function TSourceExportExpert.GetName: string;
begin
  Result := 'SourceExport';
end;

function TSourceExportExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Source Export';
begin
  Result := SDisplayName;
end;

procedure TSourceExportExpert.Click(Sender: TObject);
begin
  with TfmSourceExportOld.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmSourceExportOld.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfmSourceExportOld.SaveSettings;
begin
  // do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteString('SourceExport', 'SavePath', FSavePath);
  finally
    Free;
  end;
end;

procedure TfmSourceExportOld.LoadSettings;
begin
  // do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    FSavePath := ReadString('SourceExport', 'SavePath', '');
  finally
    Free;
  end;
end;

function TSourceExportExpert.IconFileName: string;
begin
  Result := 'SourceExport';
end;

procedure TfmSourceExportOld.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 26);
end;

initialization
  RegisterGX_Expert(TSourceExportExpert);

{$ENDIF SYNTAXMEMO}
end.


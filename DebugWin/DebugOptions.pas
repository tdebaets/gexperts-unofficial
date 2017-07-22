unit DebugOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TDebugConfigData = class(TObject)
  public
    Start: Boolean;
    OnMessage: Boolean;
    Bottom: Boolean;
    constructor Create;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

type
  TfmDebugOptions = class(TForm)
    gbxView: TGroupBox;
    chkShowOnStartup: TCheckBox;
    chkShowOnMessage: TCheckBox;
    gbxMessages: TGroupBox;
    chkNewAtBottom: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConfigInfo: TDebugConfigData = nil;
  fmDebugOptions: TfmDebugOptions;

implementation

{$R *.DFM}

uses
  Registry;

constructor TDebugConfigData.Create;
begin
  inherited Create;

  LoadSettings;
end;

procedure TDebugConfigData.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // Do not localize
  RegIni := TRegIniFile.Create('Software\GExperts\Debug');
  try
    Start := RegIni.ReadBool('View', 'Startup', False);
    OnMessage := RegIni.ReadBool('View', 'OnMessage', False);
    Bottom := RegIni.ReadBool('Messages', 'Bottom', False);
  finally
    RegIni.Free;
  end;
end;

procedure TDebugConfigData.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  RegIni := TRegIniFile.Create('Software\GExperts\Debug');
  try
    RegIni.WriteBool('View', 'Startup', Start);
    RegIni.WriteBool('View', 'OnMessage', OnMessage);
    RegIni.WriteBool('Messages', 'Bottom', Bottom);
  finally
    RegIni.Free;
  end;
end;


procedure TfmDebugOptions.FormCreate(Sender: TObject);
begin
  chkShowOnStartup.Checked := ConfigInfo.Start;
  chkShowOnMessage.Checked := ConfigInfo.OnMessage;
  chkNewAtBottom.Checked := ConfigInfo.Bottom;
end;

procedure TfmDebugOptions.btnOKClick(Sender: TObject);
begin
  ConfigInfo.Start := chkShowOnStartup.Checked;
  ConfigInfo.OnMessage := chkShowOnMessage.Checked;
  ConfigInfo.Bottom := chkNewAtBottom.Checked;
  ConfigInfo.SaveSettings;
  ModalResult := mrOK;
end;

initialization
  ConfigInfo := TDebugConfigData.Create;

finalization
  ConfigInfo.Free;
  ConfigInfo := nil;

end.

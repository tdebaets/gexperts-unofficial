unit GX_FakeIdeDock;

interface

{$I GX_CondDefine.inc}

uses
  Windows, Messages,
  SysUtils, IniFiles,
  Classes, Forms, Controls;


{$IFNDEF GX_VER120_up}
// Define TMemIniFile for those compilers
// that do not declare it.
type
  TMemIniFile = class(TObject)
  public
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual; abstract;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual; abstract;
    function ReadBool (const Section, Ident: string; Default: Boolean): Boolean; virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual; abstract;
  end;

// Define TCMDockClient for those compilers
// that do not declare it.
type
  TCMDockClient = TMessage;
{$ENDIF GX_VER120_up}

  // TFakeIdeDockForm is a place holder for the
  //
type
  TfmFakeIdeDockForm = class(TForm)
  protected
    procedure LoadDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure LoadDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure SaveDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure SaveDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure ZoomWindow; virtual; // TDesktopForm.ZoomWindow();
    procedure EditAction{(???)}; virtual;  // TDesktopForm.EditAction(Libintf.TEditAction)
    procedure GetEditState{(???)}; virtual; // TDesktopForm.GetEditState(SetInLibIntf[0..12])
  public
    procedure SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean); virtual; // TDockableForm.SaveWindowState(TMemIniFile, Boolean)
    procedure LoadWindowState(MemIniFile: TMemIniFile); virtual; // TDockableForm::LoadWindowState(TMemIniFile)
  protected
    procedure CreateDockParent(Message: TCMDockClient); virtual;
    procedure SetDockable(const Value: Boolean); virtual;
    procedure ForceShow; virtual;
  published
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;


implementation

{$R *.DFM}

procedure TfmFakeIdeDockForm.LoadDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl);
begin
end;

procedure TfmFakeIdeDockForm.LoadDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl);
begin
end;

procedure TfmFakeIdeDockForm.SaveDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl);
begin
end;

procedure TfmFakeIdeDockForm.SaveDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl);
begin
end;

procedure TfmFakeIdeDockForm.ZoomWindow;
begin
end;

procedure TfmFakeIdeDockForm.EditAction;
begin
end;

procedure TfmFakeIdeDockForm.GetEditState;
begin
end;

procedure TfmFakeIdeDockForm.SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean);
begin
end;

procedure TfmFakeIdeDockForm.LoadWindowState(MemIniFile: TMemIniFile);
begin
end;

procedure TfmFakeIdeDockForm.CreateDockParent(Message: TCMDockClient);
begin
end;

procedure TfmFakeIdeDockForm.SetDockable(const Value: Boolean);
begin
end;

procedure TfmFakeIdeDockForm.ForceShow;
begin
end;

procedure TfmFakeIdeDockForm.FormCreate(Sender: TObject);
begin
  // Create a dummy FormCreate method to inherit
end;

procedure TfmFakeIdeDockForm.FormDestroy(Sender: TObject);
begin
  // Create a dummy FormDestroy method to inherit
end;

end.


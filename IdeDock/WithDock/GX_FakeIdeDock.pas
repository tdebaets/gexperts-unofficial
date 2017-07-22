unit GX_FakeIdeDock;

interface

uses
  Classes, Forms, Controls, SysUtils, Windows, IniFiles;

  // TFakeIdeDockForm mirrors the IDE's TDockableForm
  // class closely, so that we can fake later on that
  // we descend from the (for us) inaccessible TDockableForm
  // in the core IDE package (CORIDE??.BPL).
  // We achieve this by simply tweaking the ClassParent of
  // our own IDE docking form.
type
  TfmFakeIdeDockForm = class(TForm)
  protected
    // Added, unknown private and protected data fields
    // in base TDockableForm class
    Unknown: array[0..22] of Integer;

    // Virtual methods in added TDockableForm,
    // mirrored here for later overriding by
    // our own IDE form
    procedure LoadDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure LoadDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure SaveDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure SaveDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); virtual;
    procedure ZoomWindow; virtual; // TDesktopForm.ZoomWindow()
    procedure EditAction{(???)}; virtual; // TDesktopForm.EditAction(Libintf.TEditAction)
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


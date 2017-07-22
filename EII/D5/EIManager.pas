////////////////////////////////////////////////////////////////
//                      EII Version 1.0a                      //
//            Copyright © 1998 by Markus Spoettl              //
//                    All rights reserved                     //
////////////////////////////////////////////////////////////////
//                                                            //
//   Please read the license agreement shipped with this      //
//   software. If you cannot agree to all terms of the        //
//   license agreement you MAY NOT use this software.         //
//                                                            //
//   *** CONTACT AUTHORS ***                                  //
//   If you have any questions regarding this software you    //
//   can contact                                              //
//     Markus Spoettl <markus@onlineloop.com>                 //
//   Please read the online help shipped with this software.  //
//                                                            //
//   *** WEB AND MAILING LIST ***                             //
//   Please visit the EII home page at                        //
//     http://www.onlineloop.com/markus/eii                   //
//   for latest news on EII. If you plan to use EII please    //
//   add yourself to the EII mailing list. You can find       //
//   details to how to subscribe to the mailing list at the   //
//   web pages.                                               //
//                                                            //
//   *** DISCLAIMER ***                                       //
//   EII IS PROVIDED "AS-IS". NO WARRANTIES OF ANY KIND,      //
//   EXPRESSED OR IMPLIED, ARE MADE AS TO IT OR ANY MEDIUM    //
//   IT MAY BE ON. NO REMEDY WILL BE PROVIDED FOR INDIRECT,   //
//   CONSEQUENTIAL, PUNITIVE OR INCIDENTAL DAMAGES ARISING    //
//   FROM IT, INCLUDING SUCH FROM NEGLIGENCE, STRICT          //
//   LIABILITY, OR BREACH OF WARRANTY OR CONTRACT, EVEN       //
//   AFTER NOTICE OF THE POSSIBILITY OF SUCH DAMAGES.         //
////////////////////////////////////////////////////////////////

//CE_Desc_Include(..\Help\Overviews.Txt)
//CE_Desc_Include(..\Help\EIManager.Txt)

unit EIManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, 
  ExtCtrls, EIPanel, EIEditWrapper, EIExtension;

type
    TEIManager = class;

    EEIException = class(Exception);

    TIDEManager = class;

    TEIManagerDestroyNotify = procedure (Sender: TEIManager) of Object;
    TEIManagerEditorControlMessage = procedure (Sender: TEIManager; var Msg: TMessage) of Object;

    TEIManager = class(TComponent)
      private
        FIDEManager: TIDEManager;

        FEditor: TForm;
        FEditControl: TWinControl;
        FTabControl: TWinControl;
        FMessageList: TWinControl;
        FEditorPopupMenu: TPopupMenu;
        FMessageViewPopupMenu: TPopupMenu;
        FStatusbar: TWinControl;

        FEIEditWrapper: TEIEditWrapper;

        FIntegratedComponentList: TList;

        FEIExtensions:TList;

        FOnEIManagerDestroy: TEIManagerDestroyNotify;

        FBeforeEditorControlMessageProcess: TMessageProcessProc;
        FAfterEditorControlMessageProcess: TMessageProcessProc;

        procedure DoBeforeEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
        procedure DoAfterEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
        function GetExtensionCount: Integer; 
        function GetExtensions(Index: Integer): TEIExtension; 
        function GetExtensionByName(ClassName: String): TEIExtension; 
      protected
        procedure SetupEditor;
        procedure InstallEditControlHook;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public
        constructor CreateManager(AOwner: TForm; AIDEManager: TIDEManager; ABeforeEditorControlMessageProcessProc, AAfterEditorControlMessageProcessProc: TMessageProcessProc);
        destructor Destroy; override;

        // Component integration
        function RequestIntegration(AComponent: TComponent): TEIPanel;
        procedure AttachComponent(AComponent: TComponent);
        procedure UnAttachComponent(AComponent: TComponent);
        function CheckIntegrated(AComponent: TComponent): BOOLEAN;

        function Dock(AForm: TForm): TEIPanel;
        procedure Float(AForm: TForm);

        // Extension handling
        procedure AddExtension(EIExtension: TEIExtension);
        procedure RemoveExtension(EIExtension: TEIExtension);
        function ExecuteCommand(Msg: TEIExtensionMessage):Boolean;

        property OnDestroy: TEIManagerDestroyNotify read FOnEIManagerDestroy write FOnEIManagerDestroy;

        // Routed properties
        property Editor: TForm read FEditor;
        property EditControl: TWinControl read FEditControl;
        property TabControl: TWinControl read FTabControl;
        property MessageList: TWinControl read FMessageList;
        property Statusbar: TWinControl read FStatusbar;
        property EditorPopupMenu: TPopupMenu read FEditorPopupMenu;
        property MessageViewPopupMenu: TPopupMenu read FMessageViewPopupMenu;

        // Extension handling
        property ExtensionCount: Integer read GetExtensionCount;
        property Extensions[Index:Integer]: TEIExtension read GetExtensions;
        property ExtensionByName[ClassName:String]: TEIExtension read GetExtensionByName;
        end;

    TEIMessageFlag = (eimfEditorMessages, eimfManagerMessages);
    TEIMessageFlags = set of TEIMessageFlag;
    TEIManagerMessage = Integer;

    TEIMessageNotifier = class(TObject)
      public
        procedure BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); virtual;
        procedure AfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); virtual;

        procedure OnIDEManagerDestroy(IDEManager: TIDEManager); virtual;

        procedure OnManagerMessage(Manager: TEIManager; Msg: TEIManagerMessage); virtual;
        procedure OnManagerCreated(Manager: TEIManager); virtual;
        procedure OnManagerDestroying(Manager: TEIManager); virtual;

        procedure OnActiveFormChanged(AForm: TCustomForm); virtual;
        procedure OnApplicationActivate(Activated: BOOLEAN); virtual;
        end;

    TEIMessageNotifierList = class(TObject)
      private
        FList: TList;

        function GetCount: Integer;
        function GetNotifier(i: Integer): TEIMessageNotifier;
      public
        constructor Create;
        destructor Destroy; override;

        procedure Add(AEIMessageNotifier: TEIMessageNotifier);
        procedure Remove(AEIMessageNotifier: TEIMessageNotifier);

        procedure BroadcastBeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); virtual;
        procedure BroadcastAfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); virtual;

        procedure BroadcastIDEManagerDestroy(IDEManager: TIDEManager);
        procedure BroadcastManagerMessage(Manager: TEIManager; Msg: TEIManagerMessage);
        procedure BroadcastManagerCreated(Manager: TEIManager);
        procedure BroadcastManagerDestroying(Manager: TEIManager);

        procedure BroadCastOnActiveFormChanged(AForm: TCustomForm);
        procedure BroadCastOnApplicationActivate(Activated: BOOLEAN);

        property Count: Integer read GetCount;
        property Notifier[i: Integer]: TEIMessageNotifier read GetNotifier;
        end;

    TIDEManager = class(TComponent)
      private
        FEIManagerList: TList;
        FEIMessageNotifierList: TEIMessageNotifierList;
        FEIIExpertNameList: TStringlist;

        FEIExtensionClasses: TList;
        FEIExtensions: TList;

        function GetEIManagerCount: Integer;
        function GetEIManager(i: Integer): TEIManager;
        procedure DetectExistingEditorWindows;
        function DetectNewEditorWindow(ACreatedForm: TForm): BOOLEAN;

        procedure OnManagerDestroy(AManager: TEIManager);

        procedure AddEIManager(AEIManager: TEIManager);

        procedure DoBeforeEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
        procedure DoAfterEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
      private
        // Hooked IDE events
        FOldOnActiveFormChangeEvent: TNotifyEvent;
        FOldOnApplicationActivate: TNotifyEvent;
        FOldOnApplicationDeActivate: TNotifyEvent;

        procedure OnActiveFormChange(Sender: TObject);
        procedure OnApplicationActivate(Sender: TObject);
        procedure OnApplicationDeActivate(Sender: TObject);
        function GetExtensions(Index:integer): TEIExtension; 
        function GetExtensionCount: Integer; 
        function GetExtensionByName(ClassName: String): TEIExtension;
        function GetExtensionClasses(Index: Integer): TEIExtensionClass;
        function GetExtensionClassCount: Integer;

        function GetEIIExpertCount: Integer;
        function GetEIIExpertNames(i: Integer): String;
      protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        // Component integration and editor moving
        function RequestIntegration(AEIManager: TEIManager; AComponent: TComponent{; AEIManagerNotify: TEIManagerNotify}): TEIPanel;
        procedure AttachComponent(AEIManager: TEIManager; AComponent: TComponent);
        procedure UnattachComponent(AComponent: TComponent);
        function MovePanel(DestForm: TForm; AWinControl: TWinControl; Area: TAlign): integer;

        function GetManagerByEditor(AEditor: TForm): TEIManager;

        procedure AddNotifier(ANotifier: TEIMessageNotifier);
        procedure RemoveNotifier(ANotifier: TEIMessageNotifier);

        // Called by InstallIDEIntegrationManager
        procedure AddEIIExpertname(AEIIExpertname: String);

        // Extension handling
        procedure AddExtensionClass(EIExtensionClass: TEIExtensionClass);
        procedure RemoveExtensionClass(EIExtensionClass: TEIExtensionClass);
        procedure CreateExtensionsForEditor(EIManager: TEIManager);
        procedure AddExtension(EIExtension: TEIExtension);
        procedure RemoveExtension(EIExtension: TEIExtension);
        function ExecuteCommand(Msg: TEIExtensionMessage):Boolean;

        // Managers
        property EIManagerCount: Integer read GetEIManagerCount;
        property EIManagers[i: Integer]: TEIManager read GetEIManager;

        // Extension handling
        property Extensions[i: Integer]: TEIExtension read GetExtensions;
        property ExtensionCount: Integer read GetExtensionCount;
        property ExtensionByName[ClassName:String]: TEIExtension read GetExtensionByName;
        property ExtensionClasses[Index:Integer]: TEIExtensionClass read GetExtensionClasses;
        property ExtensionClassCount: Integer read GetExtensionClassCount;

        // Using experts 
        property EIIExpertCount: Integer read GetEIIExpertCount;
        property EIIExpertNames[i: Integer]: String read GetEIIExpertNames;
        end;

function FindIDEIntegrationManager: TIDEManager;
function InstallIDEIntegrationManager(AEIIExpertname: String): TIDEManager;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIManager
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TEIManager.CreateManager(AOwner: TForm; AIDEManager: TIDEManager; ABeforeEditorControlMessageProcessProc, AAfterEditorControlMessageProcessProc: TMessageProcessProc);
begin
    inherited Create(AOwner);
    FIntegratedComponentList := TList.Create;

    FBeforeEditorControlMessageProcess := ABeforeEditorControlMessageProcessProc;
    FAfterEditorControlMessageProcess := AAfterEditorControlMessageProcessProc;

    FIDEManager := AIDEManager;

    FEditor := AOwner;
    FEditControl := nil;
    FEIEditWrapper := nil;

    FTabControl := nil;
    FMessageList := nil;
    FStatusbar := nil;
    FEditorPopupMenu := nil;

    //** new stuff **//
    FEIExtensions:=TList.Create;
    //**//

    SetupEditor;
    InstallEditControlHook;
    //** new stuff **//
    FIDEManager.CreateExtensionsForEditor(Self);
    //**//
end;

destructor TEIManager.Destroy;
begin
    // Let the IDEManager know that we are about
    // to be destroyed
    if (Assigned(FOnEIManagerDestroy)) then
        FOnEIManagerDestroy(self);

    // note: Since the EIEditWrapper is a component
    // we don't have to destroy it because is will
    // be freed by the framework

    FIntegratedComponentList.Free;
    FEIExtensions.Free;
    inherited Destroy;
end;

procedure TEIManager.Notification(AComponent: TComponent; Operation: TOperation);
var i: Integer;
begin
    if (Operation = opRemove) then begin
        if (AComponent = FIDEManager) then begin
            FIDEManager := nil;
            end
        else begin
            i := FIntegratedComponentList.IndexOf(AComponent);
            if (i <> -1) then
                FIntegratedComponentList.Delete(i);
            end;
        end;

    inherited Notification(AComponent, Operation);
end;

procedure TEIManager.DoBeforeEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
begin
    if (Assigned(FBeforeEditorControlMessageProcess)) then begin
        FBeforeEditorControlMessageProcess(Self, OriginalMessage, Msg);
        end;
end;

procedure TEIManager.DoAfterEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
begin
    if (Assigned(FAfterEditorControlMessageProcess)) then begin
        FAfterEditorControlMessageProcess(Self, OriginalMessage, Msg);
        end;
end;

procedure TEIManager.SetupEditor;
const TABCONTROL_CLASS_NAME = 'TXTabControl';
      MESSAGEVIEW_CLASS_NAME = 'TMessageViewForm';
      POPUPMENU_CLASS_NAME = 'TPopupMenu';
      EDITOR_POPUPMENU_COMPONENT_NAME = 'EditorLocalMenu';
      MESSAGEVIEW_POPUPMENU_COMPONENT_NAME = 'MsgViewPopupMenu';
      STATUSBAR_CLASS_NAME = 'TStatusbar';
      EDITCONTROL_CLASS_NAME = 'TEditControl';
var i: Integer;
begin
    // First search for the Tabcontrol, the message view
    // and the popup menu of the editor and set the
    // internal fields.
    for i := 0 to (FEditor.ComponentCount-1) do
    begin
      if (CompareText(FEditor.Components[i].Classname, TABCONTROL_CLASS_NAME) = 0) then
      begin
        if (FEditor.Components[i] is TWinControl) then
          FTabControl := TWinControl(FEditor.Components[i]);
      end else if (CompareText(FEditor.Components[i].Classname, MESSAGEVIEW_CLASS_NAME) = 0) then
      begin
        if (FEditor.Components[i] is TWinControl) then
          FMessageList := TWinControl(FEditor.Components[i]);
      end else if (CompareText(FEditor.Components[i].Classname, STATUSBAR_CLASS_NAME) = 0) then
      begin
        if (FEditor.Components[i] is TWinControl) then
          FStatusbar := TWinControl(FEditor.Components[i]);
      end else if (CompareText(FEditor.Components[i].Classname, POPUPMENU_CLASS_NAME) = 0) then
      begin
        if ((FEditor.Components[i] is TPopupMenu) and (CompareText(FEditor.Components[i].Name,EDITOR_POPUPMENU_COMPONENT_NAME) = 0)) then
           FEditorPopupMenu := TPopupMenu(FEditor.Components[i])
        else if ((FEditor.Components[i] is TPopupMenu) and (CompareText(FEditor.Components[i].Name,MESSAGEVIEW_POPUPMENU_COMPONENT_NAME) = 0)) then
           FMessageViewPopupMenu := TPopupMenu(FEditor.Components[i]);
      end else if (CompareText(FEditor.Components[i].Classname, EDITCONTROL_CLASS_NAME) = 0) then
      begin
        if (FEditor.Components[i] is TWinControl) then
           FEditControl := TWinControl(FEditor.Components[i]);
      end;
    end;


    //NEW: * no control adjustments to make
    //     * raise an EAbort execption (this is catched in the IDEManager code)
    if ((FTabControl = nil) or (FEditControl = nil) or (FStatusbar = nil)) then begin
        raise EAbort.Create('Could not set up editor for integration!')
        end;
(*
    if ((FTabControl <> nil) and (FMessageList <> nil) and (FStatusbar <> nil)) then
    begin
      FTabControl.Align := alClient;
      FStatusbar.Align := alBottom;
      FMessagelist.Align := alBottom;
    end else
      raise EEIException.Create('Could not set up editor for integration!');
*)
end;

procedure TEIManager.InstallEditControlHook;
begin
    if (FEditControl <> nil) then begin
        FEIEditWrapper := TEIEditWrapper.CreateAndHook(FEditControl, DoBeforeEditorControlMessageProcess, DoAfterEditorControlMessageProcess);
        end;
end;

function TEIManager.CheckIntegrated(AComponent: TComponent): BOOLEAN;
begin
    result := (FIntegratedComponentList.IndexOf(AComponent) <> -1);
end;

function TEIManager.RequestIntegration(AComponent: TComponent{; AEIManagerNotify: TEIManagerNotify}): TEIPanel;
begin
    if (Assigned(AComponent))then begin
        // Do not allow duplicate component integration
        if (FIntegratedComponentList.IndexOf(AComponent) <> -1) then begin
            raise EEIException.Create('Duplicate integration request!');
            end
        else begin
            result := TEIPanel.Create(nil); //** new modified **//
            FEditor.InsertControl(result);  //** new modified **//
            //result := TEIPanel.Create(FEditor);
            (* so, dies ist eine äquivalente version von deinen Create...
               die komponente wird dann auch zerstöert, wenn der Editor
               geschlossen wird... mein movepanel setzt mit removecontrol
               und insertcontrol das editorfenster auch neu... diese lösung
               sollte wunderbar funktionieren... ;-)

               es ist zwar nicht offiziell ok, aber in der Borland hilfe und
               dem source stehen die sachen auch so, also sollte man es verwenden
               können! Ich habe keine fehler entdeckt!
            *)
            result.Parent := FEditor;
            result.Visible := FALSE;
            result.Align := alLeft;
            FIntegratedComponentList.Add(AComponent);
            end;
        end
    else begin
        raise EEIException.Create('Invalid integration request!');
        end;
end;

//** new **//
procedure TEIManager.AttachComponent(AComponent: TComponent);
begin
    if (Assigned(AComponent))then begin
        // Do not allow duplicate component integration
        if (FIntegratedComponentList.IndexOf(AComponent) <> -1) then begin
            raise EEIException.Create('Duplicate integration request!');
            end
        else begin
            FIntegratedComponentList.Add(AComponent);
            end;
        end
    else begin
        raise EEIException.Create('Invalid integration request!');
        end;
end;
//** end of new **//

procedure TEIManager.UnAttachComponent(AComponent: TComponent);
var i: Integer;
begin
    i := FIntegratedComponentList.IndexOf(AComponent);
    if (i <> -1) then
        FIntegratedComponentList.Delete(i);
end;


{Thanks to Peter Friese for providing the code for this function}
function TEIManager.Dock(AForm: TForm): TEIPanel;
begin
    Result := RequestIntegration(AForm);

    with Result do begin
        TitleBarCaption := AForm.Caption;
        Align           := alTop;
        Height          := AForm.ClientHeight + Result.TitleBarHeight;
        end;

    with AForm do begin
        BorderStyle  := bsNone;
        Parent       := Result;
        Align        := alClient;
        end;
end;

{Thanks to Peter Friese for providing the code for this function}
procedure TEIManager.Float(AForm: TForm);
var ptTopLeftPanel : TPoint;
    ptScreenTopLeftPanel : TPoint;
    formwidth, formheight: Integer;
    AEIPanel: TEIPanel;
begin
    if ((AForm.Parent <> nil) and (AForm.Parent is TEIPanel)) then begin
        AEIPanel := TEIPanel(AForm.Parent);

        UnattachComponent(AForm);
        ptTopLeftPanel.x := AEIPanel.Left;
        ptTopLeftPanel.y := AEIPanel.Top;

        ptScreenTopLeftPanel := FEditor.ClientToScreen(ptTopLeftPanel);
        formwidth := AForm.Width;   //**
        formheight := AForm.Height; //** ein Problem gibt es noch...
                                    //** Du beachtest nicht die Splitterbar...
                                    //** wäre toll, wenn doch, sonst verkleinert
                                    //** sich das Fenster immer mehr!
        AForm.Visible := FALSE;
        AEIPanel.Visible := FALSE;
        AForm.Parent := nil;

        AForm.Align := alNone;
        AForm.BorderStyle := bsSizeToolWin;
        AForm.Left := ptScreenTopLeftPanel.x;
        AForm.Top  := ptScreenTopLeftPanel.y;

        AForm.ClientWidth  := formWidth;
        AForm.ClientHeight := formHeight;

        AForm.Visible := TRUE;

        AEIPanel.Free;
        end
    else begin
        raise EEIException.Create('Invalid float request!');
        end;
end;

//** new stuff **//

//Method_Marker(TEIManager.AddExtension)

procedure TEIManager.AddExtension(EIExtension: TEIExtension);
begin
    FEIExtensions.Add(EIExtension);
end;

//Method_Marker(TEIManager.GetExtensionCount)

function TEIManager.GetExtensionCount: Integer;
begin
    result:=FEIExtensions.Count;
end;

//Method_Marker(TEIManager.GetExtensions)

function TEIManager.GetExtensions(Index: Integer): TEIExtension;
begin
    result := nil;
    if (FEIExtensions.Count > Index) then
        result := FEIExtensions[Index];
end;

//Method_Marker(TEIManager.GetExtensionByName)

function TEIManager.GetExtensionByName(ClassName: String): TEIExtension;
var i:Integer;
begin
    result := nil;
    for i := 0 to (FEIExtensions.Count-1) do begin
        if (Extensions[i].ClassName = ClassName) then begin
            result := Extensions[i];
            break;
            end;
        end;
end;

//Method_Marker(TEIManager.RemoveExtension)

procedure TEIManager.RemoveExtension(EIExtension: TEIExtension);
begin
    FEIExtensions.Remove(EIExtension);
end;

//Method_Marker(TEIManager.ExecuteCommand)

function TEIManager.ExecuteCommand(Msg: TEIExtensionMessage):Boolean;
var i,p:Integer;
    EIExtension:TEIExtension;
begin
    result := FALSE;
    p := Pos('.', Msg.Command);
    if (p > 0) then begin
        EIExtension := ExtensionByName[Copy(Msg.Command, 1, p - 1)];
        Msg.Command := Copy(Msg.Command, p + 1, MAXINT);

        if (EIExtension <> nil) then begin
            result := EIExtension.ExecuteCommand(Msg);
            end;
        end
    else begin
        for i := 0 to (ExtensionCount - 1) do begin
            if (Extensions[i].ExecuteCommand(Msg)) then begin
                result := TRUE;
                break;
                end;
            end;
        end;
end;


{ClassMarker_Method(TEIManager)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIMessageNotifier
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TEIMessageNotifier.BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage);
begin
end;

procedure TEIMessageNotifier.AfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage);
begin
end;

procedure TEIMessageNotifier.OnIDEManagerDestroy(IDEManager: TIDEManager);
begin
end;

procedure TEIMessageNotifier.OnManagerMessage(Manager: TEIManager; Msg: TEIManagerMessage);
begin
end;

procedure TEIMessageNotifier.OnManagerCreated(Manager: TEIManager);
begin
end;

procedure TEIMessageNotifier.OnManagerDestroying(Manager: TEIManager);
begin
end;

procedure TEIMessageNotifier.OnActiveFormChanged(AForm: TCustomForm);
begin
end;

procedure TEIMessageNotifier.OnApplicationActivate(Activated: BOOLEAN);
begin
end;

{ClassMarker_Method(TEIMessageNotifier)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIMessageNotifierList
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TEIMessageNotifierList.Create;
begin
    inherited Create;
    FList := TList.Create;
end;

destructor TEIMessageNotifierList.Destroy;
begin
    FList.Clear;
    FList.Free;
    inherited Destroy;
end;

function TEIMessageNotifierList.GetCount: Integer;
begin
    result := FList.Count;
end;

function TEIMessageNotifierList.GetNotifier(i: Integer): TEIMessageNotifier;
begin
    result := TEIMessageNotifier(FList.Items[i]);
end;

procedure TEIMessageNotifierList.Add(AEIMessageNotifier: TEIMessageNotifier);
begin
    if (FList.IndexOf(AEIMessageNotifier) = -1) then
        FList.Add(AEIMessageNotifier);
end;

procedure TEIMessageNotifierList.Remove(AEIMessageNotifier: TEIMessageNotifier);
begin
    FList.Remove(AEIMessageNotifier);
end;

procedure TEIMessageNotifierList.BroadcastBeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].BeforeEditorMessageProcess(Manager, OriginalMessage, Msg);
        end;
end;

procedure TEIMessageNotifierList.BroadcastAfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); 
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].AfterEditorMessageProcess(Manager, OriginalMessage, Msg);
        end;
end;

procedure TEIMessageNotifierList.BroadcastIDEManagerDestroy(IDEManager: TIDEManager);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnIDEManagerDestroy(IDEManager);
        end;
end;

procedure TEIMessageNotifierList.BroadcastManagerMessage(Manager: TEIManager; Msg: TEIManagerMessage);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnManagerMessage(Manager, Msg);
        end;
end;

procedure TEIMessageNotifierList.BroadcastManagerCreated(Manager: TEIManager);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnManagerCreated(Manager);
        end;
end;

procedure TEIMessageNotifierList.BroadcastManagerDestroying(Manager: TEIManager);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnManagerDestroying(Manager);
        end;
end;

procedure TEIMessageNotifierList.BroadCastOnActiveFormChanged(AForm: TCustomForm);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnActiveFormChanged(AForm);
        end;
end;

procedure TEIMessageNotifierList.BroadCastOnApplicationActivate(Activated: BOOLEAN);
var i: Integer;
begin
    for i := 0 to (Count-1) do begin
        Notifier[i].OnApplicationActivate(Activated);
        end;
end;

{ClassMarker_Method(TEIMessageNotifierList)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TIDEManager
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TIDEManager.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    FEIManagerList := TList.Create;

    FEIMessageNotifierList := TEIMessageNotifierList.Create;

    FEIIExpertNameList := TStringlist.Create;
    FEIIExpertNameList.Sorted := TRUE;
    FEIIExpertNameList.Duplicates := dupIgnore;

    //** new stuff **//
    FEIExtensionClasses := TList.Create;
    FEIExtensions := TList.Create;
    //**//

    // Hook the IDE events of interest
    FOldOnActiveFormChangeEvent := Screen.OnActiveFormChange;
    Screen.OnActiveFormChange := OnActiveFormChange;

    FOldOnApplicationActivate := Application.OnActivate;
    Application.OnActivate := OnApplicationActivate;

    FOldOnApplicationDeActivate := Application.OnDeActivate;
    Application.OnDeActivate := OnApplicationDeActivate;

    // Install EIManager objects to all editor windows
    // which already exist.
    DetectExistingEditorWindows;
end;

destructor TIDEManager.Destroy;
begin
    FEIMessageNotifierList.BroadcastIDEManagerDestroy(Self);

    // restore hooked IDE events
    Screen.OnActiveFormChange := FOldOnActiveFormChangeEvent;
    Application.OnActivate := FOldOnApplicationActivate;
    Application.OnDeActivate := FOldOnApplicationDeActivate;

    FEIIExpertNameList.Free;

    FEIMessageNotifierList.Free;
    FEIManagerList.Free;
    //** new stuff **//
    FEIExtensionClasses.Free;
    FEIExtensions.Free;
    //**//

    inherited Destroy;
end;

function TIDEManager.GetEIManagerCount: Integer;
begin
    result := FEIManagerList.Count;
end;

function TIDEManager.GetEIManager(i: Integer): TEIManager;
begin
    result := TEIManager(FEIManagerList.Items[i]);
end;


function TIDEManager.GetManagerByEditor(AEditor: TForm): TEIManager;
var I: Integer;
begin
    result := nil;
    for i := 0 to (EIManagerCount-1) do begin
        if (EIManagers[i].Editor = AEditor) then begin
            result := EIManagers[i];
            break;
            end;
        end;
end;

procedure TIDEManager.AddEIManager(AEIManager: TEIManager);
begin
    FEIManagerList.Add(AEIManager);
end;

procedure TIDEManager.DetectExistingEditorWindows;
var i: Integer;
begin
    for i := 0 to (Screen.FormCount-1) do begin
        DetectNewEditorWindow(Screen.Forms[i]);
        end;
end;

function TIDEManager.DetectNewEditorWindow(ACreatedForm: TForm): BOOLEAN;
const EDITOR_FORM_CLASS_NAME = 'TEditWindow';
var NewManager: TEIManager;
begin
    result := FALSE;
    if (ACreatedForm = nil) then exit; //just to be sure

    //now we look for the new form and we check whether
    //it's an editor
    if (CompareText(ACreatedForm.Classname, EDITOR_FORM_CLASS_NAME) = 0) then begin
        if (GetManagerByEditor(ACreatedForm) = nil) then begin
            try
                NewManager := TEIManager.CreateManager( ACreatedForm,
                                                    self,
                                                    DoBeforeEditorControlMessageProcess,
                                                    DoAfterEditorControlMessageProcess);
                NewManager.Name:='EIManager';
                AddEIManager(NewManager);
                NewManager.OnDestroy := OnManagerDestroy;

                FEIMessageNotifierList.BroadcastManagerCreated(NewManager);
            except
                //Eat Exceptions
            end;
            end;
        end;
end;

procedure TIDEManager.OnActiveFormChange(Sender: TObject);
begin
    //First we call the stored event handler because we
    //are probably not the only once how use this handler
    if (Assigned(FOldOnActiveFormChangeEvent)) then
        FOldOnActiveFormChangeEvent(Sender);

    // Ignore this event when the application is about to
    // be destroyed, because the internal objects (Screen, ...)
    // may not be valid an we can run into problems.
    // Moreover we want to detect new editors here, but when
    // the application get destroyed, this is very unlikely :)
    if ((Application.Terminated) or (csDestroying in Application.ComponentState)) then
        exit
    else
        DetectNewEditorWindow(Screen.ActiveForm);

    if (Screen.ActiveForm <> nil) then
        FEIMessageNotifierList.BroadCastOnActiveFormChanged(Screen.ActiveForm);
end;

procedure TIDEManager.OnApplicationActivate(Sender: TObject);
begin
    if (Assigned(FOldOnApplicationActivate)) then
        FOldOnApplicationActivate(Sender);

    FEIMessageNotifierList.BroadCastOnApplicationActivate(TRUE);
end;

procedure TIDEManager.OnApplicationDeActivate(Sender: TObject);
begin
    if (Assigned(FOldOnApplicationDeActivate)) then
        FOldOnApplicationDeActivate(Sender);

    FEIMessageNotifierList.BroadCastOnApplicationActivate(FALSE);
end;

procedure TIDEManager.OnManagerDestroy(AManager: TEIManager);
var i: Integer;
begin
    // Remove destroyed Manager Objects from the
    // Manager list
    i := FEIManagerList.IndexOf(AManager);
    if (i <> -1) then begin
        FEIMessageNotifierList.BroadcastManagerDestroying(AManager);
        FEIManagerList.Delete(i);
        end;
end;

procedure TIDEManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
end;

procedure TIDEManager.DoBeforeEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
begin
    FEIMessageNotifierList.BroadcastBeforeEditorMessageProcess(TEIManager(Sender), OriginalMessage, Msg);
end;

procedure TIDEManager.DoAfterEditorControlMessageProcess(Sender: TObject; OriginalMessage: TMessage; var Msg: TMessage);
begin
    FEIMessageNotifierList.BroadcastAfterEditorMessageProcess(TEIManager(Sender), OriginalMessage, Msg);
end;

function TIDEManager.RequestIntegration(AEIManager: TEIManager; AComponent: TComponent): TEIPanel;
var i: Integer;
begin
    for i := 0 to (EIManagerCount-1) do begin
        if ((EIManagers[i] <> AEIManager) and (EIManagers[i].CheckIntegrated(AComponent))) then
            raise EEIException.Create('Component already attached to another editor');
        end;
    result := AEIManager.RequestIntegration(AComponent);
end;

//** new **//
procedure TIDEManager.AttachComponent(AEIManager: TEIManager; AComponent: TComponent);
var i: Integer;
begin
    for i := 0 to (EIManagerCount-1) do begin
        if ((EIManagers[i] <> AEIManager) and (EIManagers[i].CheckIntegrated(AComponent))) then
            raise EEIException.Create('Component already attached to another editor');
        end;
  AEIManager.AttachComponent(AComponent);
end;
//** end of new **//

procedure TIDEManager.UnattachComponent(AComponent: TComponent);
var i: Integer;
begin
    for i := 0 to (EIManagerCount-1) do begin
        if (EIManagers[i].CheckIntegrated(AComponent)) then begin
            EIManagers[i].UnAttachComponent(AComponent);
            end;
        end;
end;

//** new **//
function TIDEManager.MovePanel(DestForm: TForm; AWinControl: TWinControl; Area: TAlign): integer;
var newManager: TEIManager;
    EIPanel:TEIPanel;
begin
    result:=-1;
    newManager := GetManagerByEditor(DestForm);

    if ((newManager <> nil) and (AWinControl.Parent <> nil) and
            (AWinControl.Parent is TEIPanel)) then begin

        EIPanel:=TEIPanel(AWinControl.Parent);   // EIPanel herausfinden.
        UnAttachComponent(AWinControl);          // Komponente von
                                                 // EIManager wegbringen!
        EIPanel.Parent.RemoveControl(EIPanel);   // EIPanel von Owner lösen!
        AttachComponent(newManager,AWinControl); // Komponente an neuen
                                                 // EIManager binden!
        DestForm.InsertControl(EIPanel);         // EIPanel an neuen Owner binden!
        EIPanel.Parent:=DestForm;                // Parent von EIPanel setzen!
        EIPanel.Align:=Area;                     // Align neu setzen

        newManager.FStatusBar.Top:=DestForm.ClientHeight;
        result := FEIManagerList.IndexOf(newManager);
        end;
end;

procedure TIDEManager.AddNotifier(ANotifier: TEIMessageNotifier);
begin
    FEIMessageNotifierList.Add(ANotifier);
end;

procedure TIDEManager.RemoveNotifier(ANotifier: TEIMessageNotifier);
begin
    FEIMessageNotifierList.Remove(ANotifier);
end;

//** new stuff **//

//Method_Marker(TIDEManager.AddExtension)

procedure TIDEManager.AddExtensionClass(EIExtensionClass: TEIExtensionClass);
var EIExtension:TEIExtension;
    i:Integer;
begin
    if (EIExtensionClass.ExtensionType = etNone) then
        exit;

    // we only add extension classes of
    // unique class types
    if (FEIExtensionClasses.IndexOf(EIExtensionClass) = -1) then begin
        // we only add extension classes
        // with unique class names
        for i := 0 to (FEIExtensionClasses.Count-1) do begin
            if (TEIExtensionClass(FEIExtensionClasses[i]).Classname = EIExtensionClass.Classname) then
                exit;
            end;

        FEIExtensionClasses.Add(EIExtensionClass);

        // if the extension is a IDE (system) extension
        // we immediately create the instance of the extension
        if (EIExtensionClass.ExtensionType = etIDE) then begin
            EIExtension := EIExtensionClass.Create(Self);
            EIExtension.Name := Copy(EIExtension.ClassName, 2, MAXINT);
            FEIExtensions.Add(EIExtension);
            end
        // if the extension is a Editor extension
        // we immediately create the instances for all existing
        // EIManagers
        else if (EIExtensionClass.ExtensionType = etEditor) then begin
            for i:=0 to (EIManagerCount - 1) do begin
                EIExtension := EIExtensionClass.Create(EIManagers[i]);
                EIManagers[i].AddExtension(EIExtension);
                end;
            end;
        end;
end;

//Method_Marker(TIDEManager.RemoveExtension)

procedure TIDEManager.RemoveExtensionClass(EIExtensionClass: TEIExtensionClass);
var EIExtension:TEIExtension;
    i: Integer;
begin
    if (FEIExtensionClasses.IndexOf(EIExtensionClass) >= 0) then begin
        // If the extension is an etEditor extension
        // we delete and free all instances from the
        // existing managers
        if (EIExtensionClass.ExtensionType = etEditor) then begin
            for i := 0 to (FEIManagerList.Count-1) do begin
                EIExtension := EIManagers[i].ExtensionByName[EIExtensionClass.ClassName];
                if (EIExtension <> nil) then begin
                    EIManagers[i].RemoveExtension(EIExtension);
                    EIExtension.Free;
                    end;
                end;
            end;

        // Remove the class itself
        FEIExtensionClasses.Remove(EIExtensionClass);

        // Remove the instance if it was
        // a etIDE extension
        EIExtension := ExtensionByName[EIExtensionClass.ClassName];

        if ((EIExtension <> nil) and (EIExtension.ExtensionType = etIDE)) then begin
            FEIExtensions.Remove(EIExtension);
            EIExtension.Free;
            end;
        end;
end;

//Method_Marker(TIDEManager.CreateExtensionsForEditor)

procedure TIDEManager.CreateExtensionsForEditor(EIManager: TEIManager);
var EIExtensionClass: TEIExtensionClass;
    EIExtension: TEIExtension;
    i: Integer;
begin
    for i := 0 to (FEIExtensionClasses.Count-1) do begin
        EIExtensionClass := FEIExtensionClasses[i];

        if (EIExtensionClass.ExtensionType = etEditor) then begin
            EIExtension := EIExtensionClass.Create(EIManager);
            EIExtension.Name := Copy(EIExtension.ClassName, 2, MAXINT);
            EIManager.AddExtension(EIExtension);
            end;
        end;
end;

//Method_Marker(TIDEManager.GetExtensions)

function TIDEManager.GetExtensions(Index:integer): TEIExtension;
begin
    result := nil;
    if (FEIExtensions.Count > Index) then
        result := FEIExtensions[Index];
end;

//Method_Marker(TIDEManager.GetExtensionCount)

function TIDEManager.GetExtensionCount: Integer;
begin
    result := FEIExtensions.Count;
end;

//Method_Marker(TIDEManager.GetExtensionByName)

function TIDEManager.GetExtensionByName(ClassName: String): TEIExtension;
var i:Integer;
begin
    result := nil;
    for i := 0 to (FEIExtensions.Count-1) do begin
        if ((Extensions[i].ClassName = ClassName) or (Extensions[i].Name = ClassName)) then begin
            result:=Extensions[i];
            break;
            end;
        end;
end;

//Method_Marker(TIDEManager.GetExtensionClasses)

function TIDEManager.GetExtensionClasses(Index: Integer): TEIExtensionClass;
begin
    result := nil;
    if (FEIExtensionClasses.Count > Index) then
        result := FEIExtensionClasses[Index];
end;

//Method_Marker(TIDEManager.AddExtension)

procedure TIDEManager.AddExtension(EIExtension: TEIExtension);
begin
    FEIExtensions.Add(EIExtension);
end;

//Method_Marker(TIDEManager.RemoveExtension)

procedure TIDEManager.RemoveExtension(EIExtension: TEIExtension);
begin
    FEIExtensions.Remove(EIExtension);
end;

//Method_Marker(TIDEManager.GetExtensionClassCount)

function TIDEManager.GetExtensionClassCount: Integer;
begin
    result := FEIExtensionClasses.Count;
end;

//Method_Marker(TIDEManager.ExecuteCommand)

function TIDEManager.ExecuteCommand(Msg: TEIExtensionMessage):Boolean;
var i,p:Integer;
    EIExtension:TEIExtension;
    originalCommand: String;
begin
    result := FALSE;
    originalCommand := Msg.Command;
    p := Pos('.', Msg.Command);
    if (p > 0) then begin
        EIExtension := ExtensionByName[Copy(Msg.Command, 1, p - 1)];
        Msg.Command := Copy(Msg.Command, p + 1, MAXINT);

        if (EIExtension <> nil) then begin
            result := EIExtension.ExecuteCommand(Msg);
            end;
        end
    else begin
        for i := 0 to (ExtensionCount - 1) do begin
            if (Extensions[i].ExecuteCommand(Msg)) then begin
                result := TRUE;
                break;
                end;
            end;
        end;

    if (EIManagerCount > 0) then begin
        for i := 0 to (EIManagerCount-1) do begin
            // Restore the command string for all
            // Managers so that the managers can
            // decide where to send the message to
            Msg.Command := originalCommand;
            EIManagers[i].ExecuteCommand(Msg);
            end;
        end;
end;

function TIDEManager.GetEIIExpertCount: Integer;
begin
    result := FEIIExpertNameList.Count;
end;

function TIDEManager.GetEIIExpertNames(i: Integer): String;
begin
    result := FEIIExpertNameList.Strings[i];
end;

procedure TIDEManager.AddEIIExpertname(AEIIExpertname: String);
begin
    FEIIExpertNameList.Add(AEIIExpertname);
end;

{ClassMarker_Method(TIDEManager)}

////////////////////////////////////////////////////////////////////////////////////////////////
// Service Routines
////////////////////////////////////////////////////////////////////////////////////////////////

function FindIDEIntegrationManager: TIDEManager;
var i: Integer;
begin
    result := nil;
    for i := 0 to (Application.ComponentCount-1) do begin
        if (CompareText(Application.Components[i].Classname, TIDEManager.Classname) = 0) then begin
            // we found an installed IDEManager, return this one
            result := TIDEManager(Application.Components[i]);
            break;
            end;
        end;
end;

function InstallIDEIntegrationManager(AEIIExpertname: String): TIDEManager;
begin
    result := FindIDEIntegrationManager;
    // No IDEManager found, so let's create a new manager.
    if (result = nil) then begin
        result := TIDEManager.Create(Application);
        result.Name:='IDEManager';
        end;
    result.AddEIIExpertname(AEIIExpertname);
end;

end.

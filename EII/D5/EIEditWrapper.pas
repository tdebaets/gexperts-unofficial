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

//CE_Desc_Include(..\Help\EIEditWrapper.Txt)

unit EIEditWrapper;

interface

uses SysUtils, Windows, Classes, Forms, Messages, Controls, TypInfo;

type
    TMessageProcessProc = procedure (Sender: TObject; OriginalMessage: TMessage; var ModifiedMessage: TMessage) of Object;

    TCustomControlHacker = class (TCustomControl); // this is enables us to access protected parts of TCustomControls...

    TEIEditWrapper = class(TComponent)
      private
        FEditor: TCustomControlHacker;
        OldWndProc: TFarProc;
        NewWndProc: Pointer;

        FBeforeEditorControlMessageProcess: TMessageProcessProc;
        FAfterEditorControlMessageProcess: TMessageProcessProc;

        procedure HookOwner;

        procedure UnhookOwner;
        procedure HookWndProc(var msg: TMessage);

        function GetTopLine: Integer;
        function GetLineCount: Integer;
        function GetLinesInWindow: Integer;
        function GetCurrentLine: Integer;
      protected
      public
        constructor CreateAndHook(AOwner: TComponent; ABeforeEditorControlMessageProcessProc, AAfterEditorControlMessageProcessProc: TMessageProcessProc);
        destructor Destroy; override;

        property CurrentLine: Integer read GetCurrentLine;
        property LineCount: Integer read GetLineCount;
        property TopLine: Integer read GetTopLine;
        property LinesInWindow: Integer read GetLinesInWindow;
        end;


implementation

constructor TEIEditWrapper.CreateAndHook(AOwner: TComponent; ABeforeEditorControlMessageProcessProc, AAfterEditorControlMessageProcessProc: TMessageProcessProc);
begin
    if (uppercase(AOwner.ClassName)<>'TEDITCONTROL') then
        raise Exception.Create('Invalid create class for TEIEditWrapper');
        
    inherited Create(AOwner);

    FEditor := TCustomControlHacker(AOwner);

    FBeforeEditorControlMessageProcess := ABeforeEditorControlMessageProcessProc;
    FAfterEditorControlMessageProcess := AAfterEditorControlMessageProcessProc;
    
    HookOwner;
end;

destructor TEIEditWrapper.Destroy;
begin
    UnHookOwner;
    inherited Destroy;
end;

procedure TEIEditWrapper.HookOwner;
begin
    { If there is no parent, we can't hook it. }
    if Owner = NIL then exit;

    { Get the old window procedure via API call and store it. }
    OldWndProc := TFarProc(GetWindowLong(FEditor.Handle, GWL_WNDPROC));
    { Convert our object method into something Windows knows how to call }
    NewWndProc := MakeObjectInstance(HookWndProc);
    { Install it as the new Parent window procedure }
    if (SetWindowLong(FEditor.Handle, GWL_WNDPROC, LongInt(NewWndProc)) = 0) then begin
        OldWndProc := nil;
        if (NewWndProc <> nil) then begin
            FreeObjectInstance(NewWndProc);
            NewWndProc := nil;
            end;
        end;
end;

{ Remove our window function and reinstall the original. }
procedure TEIEditWrapper.UnhookOwner;
begin
    { We must have a parent, and we must have already hooked it. }
    if (Owner <> NIL) and assigned(OldWndProc) then
        if FEditor.HandleAllocated then
            { Set back to original window procedure }
            SetWindowLong(FEditor.Handle, GWL_WNDPROC, LongInt(OldWndProc));
    { If we have created a window procedure via MakeObjectInstance, }
    { it must be disposed of.                                       }
    if assigned(NewWndProc) then
        FreeObjectInstance(NewWndProc);
    { Reset variables to NIL }
    NewWndProc := NIL;
    OldWndProc := NIL;
end;

{ The window procedure that is installed into our parent. }
procedure TEIEditWrapper.HookWndProc(var msg: TMessage);
var OriginalMessage: TMessage;
begin
    if Owner = nil then exit;
    
    try // Added this block to keep exceptions from escaping
        // Store the original message
        OriginalMessage := msg;

        // Let the notifiers see the message before the
        // control processes it
        if (Assigned(FBeforeEditorControlMessageProcess)) then begin
            FBeforeEditorControlMessageProcess(Self, OriginalMessage, msg);
            end;

        // Call the original window proc only
        // if no notifier has marked the message as processed
        // that is when the result of the message is 1
        if (msg.Result <> 1) then
            msg.Result := CallWindowProc(OldWndProc,TForm(Owner).Handle,msg.Msg,msg.WParam,msg.LParam);

        // Let the notifiers see the message after the
        // control has processed it
        if (not (OriginalMessage.Msg = WM_DROPFILES)) then begin
            if (Assigned(FAfterEditorControlMessageProcess)) then begin
                FAfterEditorControlMessageProcess(Self, OriginalMessage, msg);
                end;
            end;
    except
        {on E: Exception do MessageBox(Application.Mainform.Handle,
            PChar('EIEditWrapper Exception: '+E.Message+#13#10+
            'Please report this error along with a description of what you were doing when the error occurred (Message: '+IntToStr(OriginalMessage.Msg)+').'), 'EIEditWrapper Exception', MB_OK);}
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Property Get Methods
////////////////////////////////////////////////////////////////////////////////////////////////

function TEIEditWrapper.GetTopLine: Integer;
var PropInfo: PPropInfo;
begin
    Result:=-1;
    if FEditor=nil then exit;
    PropInfo:=GetPropInfo(FEditor.ClassInfo,'TopLine');
    Result:=GetOrdProp(FEditor,PropInfo);
end;

function TEIEditWrapper.GetLineCount: Integer;
var PropInfo: PPropInfo;
begin
    Result:=-1;
    if FEditor=nil then exit;
    PropInfo:=GetPropInfo(FEditor.ClassInfo,'LineCount');
    Result:=GetOrdProp(FEditor,PropInfo);
end;

function TEIEditWrapper.GetLinesInWindow: Integer;
var PropInfo: PPropInfo;
begin
    Result:=-1;
    if FEditor=nil then exit;
    PropInfo:=GetPropInfo(FEditor.ClassInfo,'LinesInWindow');
    Result:=GetOrdProp(FEditor,PropInfo);
end;

function TEIEditWrapper.GetCurrentLine: Integer;
var PropInfo: PPropInfo;
begin
    Result:=-1;
    if FEditor=nil then exit;
    PropInfo:=GetPropInfo(FEditor.ClassInfo,'CaretY');
    Result:=GetOrdProp(FEditor,PropInfo);
end;

end.

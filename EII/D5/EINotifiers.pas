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

//CE_Desc_Include(..\Help\EINotifiers.Txt)

unit EINotifiers;

interface

uses EIManager, Messages, Controls, Graphics;


type
    TEIKeyboardNotifier = class(TEIMessageNotifier)
      private
        FOnKeyDown: TKeyEvent;
        FOnKeyUp: TKeyEvent;

        procedure DoProcessKeyMsg(Manager: TEIManager; Msg: TWMKey);
      public
        // We are not interested in processing the messages processed by the
        // editor control, therefore we only override the BeforeEditorMessageProcess
        procedure BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); override;

        property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
        property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
        end;

    TEIEditorPaintEvent = procedure (Sender: TObject; Canvas: TCanvas);

    TEIPaintNotifier = class(TEIMessageNotifier)
      private
        FOnPaint: TEIEditorPaintEvent;

        procedure DoProcessPaintMsg(Manager: TEIManager; Msg: TMessage);
      public
        // We only paint after the editor has processed the WM_PAINT message,
        // therefore we do not implement the BeforeEditorMessageProcess method
        procedure AfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); override;

        property OnPaint: TEIEditorPaintEvent read FOnPaint write FOnPaint;
        end;

implementation

uses Forms, Classes, Windows;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIPaintNotifier
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TEIKeyboardNotifier.BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage);
begin
    case OriginalMessage.Msg of
        CN_KEYDOWN,
        CN_KEYUP:
            DoProcessKeyMsg(Manager, TWMKey(msg));
        end;
end;

procedure TEIKeyboardNotifier.DoProcessKeyMsg(Manager: TEIManager; Msg: TWMKey);
var Shift: TShiftState;
begin
    Shift := KeyDataToShiftState(Msg.KeyData);
    if (Msg.Msg = CN_KEYDOWN) then begin
        if (Assigned(FOnKeyDown)) then begin
            FOnKeyDown(Manager, Msg.CharCode, Shift);
            end;
        end
    else if (Msg.Msg = CN_KEYUP) then begin
        if (Assigned(FOnKeyUp)) then begin
            FOnKeyUp(Manager, Msg.CharCode, Shift);
            end;
        end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIPaintNotifier
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TEIPaintNotifier.AfterEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage); 
begin
    if (OriginalMessage.Msg = WM_PAINT) then begin
        DoProcessPaintMsg(Manager, OriginalMessage);
        end;
end;

procedure TEIPaintNotifier.DoProcessPaintMsg(Manager: TEIManager; Msg: TMessage);
var ControlCanvas: TControlCanvas;
    DC: THandle;
begin
    if (Assigned(FOnPaint) and (Manager.EditControl <> nil)) then begin
        ControlCanvas := TControlCanvas.Create;
        try
            DC := GetDC(Manager.EditControl.Handle);
            if (DC <> 0) then begin
                ControlCanvas.Handle := DC;
                FOnPaint(Manager, ControlCanvas);
                ControlCanvas.Handle := 0;
                ReleaseDC(Manager.EditControl.Handle, DC);
                end;
        finally
            ControlCanvas.Free;
        end;
        end;
end;

end.

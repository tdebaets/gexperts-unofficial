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

unit EIExtension;

// ----------------------------------------------------------------------------
    interface
// ----------------------------------------------------------------------------

uses
  Classes,SysUtils;

//CE_Desc_Include(..\Help\EIExtension.Txt)

// ----------------------------------------------------------------------------

type
  TEIExtensionType=(etNone,etEditor,etIDE,etRegisterOnly);

// ----------------------------------------------------------------------------
// -- TEIExtensionMessage                                                    --
// ----------------------------------------------------------------------------

  TEIExtensionMessage=class(TObject)
  private
    FSender:TObject;
    FCommand:String;     // Hier Befehle für die Extension.
    FCommandID:Integer;  // Evtl. eine Unter-ID oder Sub-Command.
    FData:TObject;       // Evtl. Befehlsparameter.
  protected
  public
    constructor Create(ASender:TObject);
    constructor CreateCommand(ASender:TObject;ACommand:String);
    constructor CreateFullCommand(ASender:TObject;ACommand:String;ACommandID:Integer;AData:TObject);

    property Sender:TObject    read FSender;
    property Command:String    read FCommand   write FCommand;
    property CommandID:Integer read FCommandID write FCommandID;
    property Data:TObject      read FData      write FData;
  published
  end;

// ----------------------------------------------------------------------------
// -- TEIExtension                                                           --
// ----------------------------------------------------------------------------

  TEIExtensionClass=class of TEIExtension;
  TEIExtension=class(TComponent)
  private
  protected
    function GetCommandCount:Integer;                             virtual;
    function GetCommands(Index:Integer):String;                   virtual;
    function GetCommandFlags(Index:Integer):Integer;              virtual;
  public
    class function ExtensionType:TEIExtensionType;                virtual;
    class function ExtensionName:String;                          virtual;
    class function ExtensionFlags:Integer;                        virtual;
    function ExecuteCommand(var Msg:TEIExtensionMessage):Boolean; virtual;

    property CommandCount:Integer                read GetCommandCount;
    property Commands[Index:Integer]:String      read GetCommands;
    property CommandFlags[Index:Integer]:Integer read GetCommandFlags;
  published
  end;

// ----------------------------------------------------------------------------

procedure RegisterEIExtensionClass(EIExtensionClass:TEIExtensionClass);

// ----------------------------------------------------------------------------
    implementation
// ----------------------------------------------------------------------------

uses
  EIManager;

// ----------------------------------------------------------------------------
// -- TEIExtensionMessage                                                    --
// ----------------------------------------------------------------------------

    constructor TEIExtensionMessage.Create(ASender:TObject);
    begin
      inherited Create;
      FSender:=ASender;
      FCommand:='';
      FCommandID:=0;
      FData:=nil;
    end;

    constructor TEIExtensionMessage.CreateCommand(ASender:TObject;ACommand:String);
    begin
      inherited Create;
      FSender:=ASender;
      FCommand:=ACommand;
      FCommandID:=0;
      FData:=nil;
    end;

    constructor TEIExtensionMessage.CreateFullCommand(ASender:TObject;ACommand:String;ACommandID:Integer;AData:TObject);
    begin
      inherited Create;
      FSender:=ASender;
      FCommand:=ACommand;
      FCommandID:=ACommandID;
      FData:=AData;
    end;

// ----------------------------------------------------------------------------
// -- TEIExtension                                                           --
// ----------------------------------------------------------------------------

    class function TEIExtension.ExtensionType:TEIExtensionType;
    begin
      result:=etNone;
    end;

    class function TEIExtension.ExtensionName:String;
    begin
      result:=''; 
    end;

    class function TEIExtension.ExtensionFlags:Integer;
    begin
      result:=$00000000;
    end;

    function TEIExtension.ExecuteCommand(var Msg:TEIExtensionMessage):Boolean;
    begin
      result:=false;
    end;

    function TEIExtension.GetCommandCount:Integer;
    begin
      result:=0;
    end;

    function TEIExtension.GetCommands(Index:Integer):String;
    begin
      result:='';
    end;

    function TEIExtension.GetCommandFlags(Index:Integer):Integer;
    begin
      result:=$00000000;
    end;

// ----------------------------------------------------------------------------

procedure RegisterEIExtensionClass(EIExtensionClass:TEIExtensionClass);
var
  IDEManager:TIDEManager;
begin
  IDEManager:=InstallIDEIntegrationManager('RegisterEIExtensionClass');
  IDEManager.AddExtensionClass(EIExtensionClass);
end;

// ----------------------------------------------------------------------------

end.

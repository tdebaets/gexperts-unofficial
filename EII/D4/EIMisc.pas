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

unit EIMisc;

interface

uses Classes, SysUtils;

//CE_Desc_Include(..\Help\EIMisc.Txt)

{Thanks to Gerald Nunn for providing this function}
function IsCodeRushInstalled: Boolean;

implementation

uses Registry;

function IsCodeRushInstalled: Boolean;
var Reg: TRegistry;
    List: TStringList;
    i: Integer;
begin
    IsCodeRushInstalled:=False;
    List:=TStringList.Create;
    try
        Reg:=TRegistry.Create;
        Try
            if not Reg.OpenKey('Software\Borland\Delphi\3.0\Known Packages',False) then
                exit;

            Reg.GetValueNames(List);
            for i:=0 to List.Count-1 do begin
                if uppercase(ExtractFileName(List.Strings[i]))='CODERUSH3.DPL' then begin
                    IsCodeRushInstalled:=True;
                    Break;
                    end;
                    end;
        finally
            Reg.Free;
        end;
    finally
        List.Free;
    end;
End;

end.

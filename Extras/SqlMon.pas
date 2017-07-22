unit SqlMon;

// SqlMon Sends BDE SQL callback information to the GExperts Debug Window.
// Just add this unit to any uses clause in the project and rebuild to
// activate the trace information. Note that using this unit _will_ adversely
// affect SQL performance.  Remove it from production code.
// For more information on GExperts, see: http://www.gexperts.org/
// Authors: Gerald Nunn, Erik Berry

interface

uses Windows, Classes, SysUtils, DB, BDE, Dialogs, DbugIntf;

// Data structure to save the previous callback info
type TDbiCbInfo = record
  ecbType     : CBType;
  iClientData : Longint;
  DataBuffLn  : Word;
  DataBuff    : PCBPROGRESSDesc;
  DbiCbFn     : Pointer;
end;

type PDbiCbInfo = ^TDbiCbInfo;

// Trace callback information
MyTraceDesc = packed record
  eTraceCat    : TRACECat;
  uTotalMsgLen : Word;
  pszTrace     : array [0..DBIMaxTraceLen] of Char;
end;

function BDETraceCB(ecbType: CBType; iClientData: Integer; CbInfo: Pointer): CBRType; export; stdcall;

implementation

var
  TraceInfo: MyTraceDesc;
  OldTraceCbInfo: TDbiCbInfo;  // Variable to save previous callback info

procedure ShowBDEError(n: Integer);
var
  Error: string;
begin
  SetLength(Error, DbiMaxMsgLen + 1);
  DbiGetErrorString(n, PChar(Error));
  SendDebugEx('Trace Error: ' + Error, mtError);
end;

function GetCat(n: Integer): String;
begin
  case n of
    $0:   Result := 'Unknown';
    $1:   Result := 'Query Prepare';  // prepared query statements
    $2:   Result := 'Query Execute';  // executed query statements
    $4:   Result := 'Vendor Error';   // vendor errors
    $8:   Result := 'Statement Ops';  // statement ops (i.e. allocate, free)
    $10:  Result := '(Dis)connect';   // connect / disconnect
    $20:  Result := 'Transaction';    // transaction
    $40:  Result := 'Blob I/O';       // blob i/o
    $80:  Result := 'Miscellaneous';  // misc.
    $100: Result := 'Vendor';
  else
    Result := 'Uknknown Trace ' + IntToStr(n);
  end;
end;

function GetMsgType(n: Integer): TMsgDlgType;
begin
  case n of
    $0,$1,$2:
         Result := mtInformation;
    $4:  Result := mtError;
    $8:  Result := mtInformation;
    $10: Result := mtWarning;
    $20,$40,$80,$100:
         Result := mtInformation;
  else
    Result := mtInformation;
  end;
end;

function BDETraceCB(ecbType:CBType;iClientData:Integer;CbInfo:Pointer):CBRType;
begin
  SendDebugEx(GetCat(TraceInfo.eTracecat) + ', ' +
              intToStr(TraceInfo.uTotalMsgLen) + ', ' +
              TraceInfo.pszTrace,GetMsgType(TraceInfo.eTracecat));
  // Was there a previous callback registered?  If so, call it and return.
  if PDbiCbInfo(iClientData)^.DbiCbFn <> nil then
    Result := pfDBICallBack(PDbiCbInfo(iClientData)^.DbiCbFn)
                            (ecbType,PDbiCbInfo(iClientData)^.iClientData,
                            cbInfo)
  else
    Result := cbrCONTINUE;
end;

procedure InitTracing;
var
  CBReturn: Word;
begin
  DbiInit(nil);
  OldTraceCbInfo.DbiCbFn := nil;
  Session.TraceFlags := [tfQPrepare, tfQExecute, tfError, tfStmt, tfConnect,
                         tfTransact, tfBlob, tfMisc, tfVendor];
  // Get any established trace callback
  CBReturn := DbiGetCallBack(nil, cbTRACE, @OldTraceCbInfo.iClientData,
                             @OldTraceCbInfo.DataBuffLn,@OldTraceCbInfo.DataBuff,
                             pfDBICallBack(OldTraceCbInfo.DbiCbFn));
  // Now register our new callback
  CBReturn := DbiRegisterCallback(nil, cbTrace, Longint(@OldTraceCbInfo),
                                  SizeOf(TraceInfo), @TraceInfo, @BDETraceCB);
  if CBReturn <> DBIERR_NONE then
    ShowBDEError(Result);
end;

procedure FinishTracing;
begin
  // Unregister our trace callback before exiting
  if OldTraceCbInfo.DbiCbFn <> nil then
    DbiRegisterCallBack(nil, cbTrace, OldTraceCbInfo.iClientData,
                        OldTraceCbInfo.DataBuffLn, OldTraceCbInfo.DataBuff,
                        OldTraceCbInfo.DbiCbFn)
  else
    DbiRegisterCallBack(nil, cbTrace, Longint(@OldTraceCbInfo),
                        SizeOf(TraceInfo), @TraceInfo, nil);
  DbiExit;
end;

initialization
  InitTracing;

finalization
  FinishTracing;

end.

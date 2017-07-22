unit E_StgStr;

interface

uses
    ActiveX, SysUtils, Windows;
type
  TStructuredStorage = class
   private
     FFileName : String;
     fGrfMode : integer;
   public
     hr          : HResult;
     StorageFile : IStorage;
     constructor Create; virtual;
     destructor  Destroy; override;

     function  IsStorageFile(const FileName : String) : Boolean;

     function  CreateStorageFile(const FileName : String;
                                       grfMode  : LongInt) : Boolean;

     function  OpenStorageFile(const FileName : String;
                                     grfMode  : LongInt) : Boolean;
     procedure CloseStorageFile;

     function  DefragmentStorageFile(const aFileName : string) : boolean;

     function  CreateSubStorage(const FileName    : String;
                                      grfMode     : LongInt;
                                var   Storage     : IStorage;
                                var   SubStorage  : IStorage) : Boolean;

     function  OpenSubStorage(const FileName   : String;
                                    grfMode    : LongInt;
                              var   Storage    : IStorage;
                              var   SubStorage : IStorage) : Boolean;

     procedure DoneStorage(var Storage : IStorage);

     function  CopyTo(var aFromStorage,
                          aToStorage : IStorage) : boolean;

     function  Commit(var Storage        : IStorage;
                          grfCommitFlags : LongInt) : boolean;

     function  Revert(var Storage : IStorage) : boolean;

     function  MoveElementTo(var   aSrcStorage  : IStorage;
                             const aSrcName     : string;
                             var   aDestStorage : IStorage;
                             const aDestName    : string;
                                   aMakeCopy    : boolean) : Boolean;

     function  DestroyElement(var   aStorage : IStorage;
                              const aElement : string) : Boolean;

     function  RenameElement(var   aStorage : IStorage;
                             const aOldName,
                                   aNewName : string) : Boolean;

     function  SetElementTimes(var   aStorage : IStorage;
                               const aElement : string;
                                     cTime,
                                     aTime,
                                     mTime : TDateTime) : Boolean;

     function  CreateStream(var   Storage     : IStorage;
                            const StreamName  : String;
                                  grfMode     : LongInt;
                            var   Stream      : IStream) : Boolean;

     function  OpenStream(var   Storage     : IStorage;
                          const StreamName  : String;
                                grfMode     : LongInt;
                          var   Stream      : IStream) : Boolean;

     procedure DoneStream(var Stream : IStream);

     function  WriteString(var   Stream : IStream;
                           const S      : String) : Boolean;
     function  ReadString(var Stream    : IStream;
                          var S         : String) : Boolean;

     function  WriteInt(var Stream : IStream;
                            TInt   : Integer) : Boolean;

     function  ReadInt(var Stream    : IStream;
                       var TInt      : Integer) : Boolean;
  end;

implementation

(*
  STGM_DIRECT           = $00000000;
  STGM_TRANSACTED       = $00010000;
  STGM_SIMPLE           = $08000000;

  STGM_READ             = $00000000;
  STGM_WRITE            = $00000001;
  STGM_READWRITE        = $00000002;

  STGM_SHARE_DENY_NONE  = $00000040;
  STGM_SHARE_DENY_READ  = $00000030;
  STGM_SHARE_DENY_WRITE = $00000020;
  STGM_SHARE_EXCLUSIVE  = $00000010;

  STGM_PRIORITY         = $00040000;
  STGM_DELETEONRELEASE  = $04000000;

  STGM_CREATE           = $00001000;
  STGM_CONVERT          = $00020000;
  STGM_FAILIFTHERE      = $00000000;

  STATSTG = record
    pwcsName: POleStr;
    dwType: Longint;
    cbSize: Largeint;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    clsid: TCLSID;
    grfStateBits: Longint;
    reserved: Longint;
  end;

  IStream = interface(IUnknown)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;

*)

(*******************************************************************
 Structured Storage Object to help relieve some of pain when dealing
 with some of it's peculiarities.
*******************************************************************)
constructor TStructuredStorage.Create;
begin
  inherited Create;
  StorageFile := nil;
  FFileName := '';
  FgrfMode := 0;
end;

destructor TStructuredStorage.Destroy;
begin
  StorageFile := nil;
  inherited Destroy;
end;

(*******************************************************************
                           CreateStorageFile
 Create an Ole Structured Storage File
*******************************************************************)
function TStructuredStorage.CreateStorageFile(const FileName : String;
                                                    grfMode  : LongInt) : Boolean;
begin
  hr := StgCreateDocFile(PWideChar(WideString(FileName)),
                         grfMode,
                         0,
                         StorageFile);
  Result := (hr = S_OK);
  if Result then
  begin
    FFileName := FileName;
    FgrfMode  := grfMode;
  end;
end;

function DidFail(hr : Hresult) : Boolean;
begin
  Result := ActiveX.Failed(hr);
end;

(*******************************************************************
                           CreateSubStorage
 Create Sub Storage within a Storage.
*******************************************************************)
function TStructuredStorage.CreateSubStorage(const FileName    : String;
                                                   grfMode     : LongInt;
                                             var   Storage     : IStorage;
                                             var   SubStorage  : IStorage) : Boolean;
begin
  hr := Storage.CreateStorage(PWideChar(WideString(FileName)),
                              grfMode,
                              0,
                              0,
                              SubStorage);
  Result := not DidFail(hr);
end;

(*******************************************************************
                           IsStorageFile
 See if FileName is indeed a structured storage file.
*******************************************************************)
function TStructuredStorage.IsStorageFile(const FileName : String) : Boolean;
begin
  hr := StgIsStorageFile(PWideChar(WideString(FileName)));
  Result := not DidFail(hr);
end;

(*******************************************************************
                           OpenStorageFile
 Open an Ole Structured Storage File
*******************************************************************)
function TStructuredStorage.OpenStorageFile(const FileName : String;
                                                  grfMode  : LongInt) : Boolean;
begin
  Result := False;
  if IsStorageFile(FileName) then
  begin
    hr := StgOpenStorage(PWideChar(WideString(FileName)),
                         nil,
                         grfMode,
                         nil,
                         LongInt(nil),
                         StorageFile);
    Result := (hr = S_OK);
    if Result then
    begin
      FFileName := FileName;
      FgrfMode  := grfMode;
    end;
  end;
end;

(*******************************************************************
                           OpenSubStorage
 Open an Ole Structured sub-Storage within a storage
*******************************************************************)
function TStructuredStorage.OpenSubStorage(const FileName   : String;
                                                 grfMode    : LongInt;
                                           var   Storage    : IStorage;
                                           var   SubStorage : IStorage) : Boolean;
begin
  hr := Storage.OpenStorage(PWideChar(WideString(FileName)),
                            nil,
                            grfMode,
                            nil,
                            LongInt(nil),
                            SubStorage);
  Result := not DidFail(hr);
end;

(*******************************************************************
 Free up an OLE storage.
*******************************************************************)
procedure TStructuredStorage.DoneStorage(var Storage : IStorage);
begin
  Storage := nil;
end;

(*******************************************************************
 Create an Ole Stream within the current storage
*******************************************************************)
function TStructuredStorage.CreateStream(var   Storage     : IStorage;
                                         const StreamName  : String;
                                               grfMode     : LongInt;
                                         var   Stream      : IStream) : Boolean;
begin
  hr := Storage.CreateStream(PWideChar(WideString(StreamName)),
                             grfMode,
                             0,
                             0,
                             Stream);
  Result := not DidFail(hr);
end;

(*******************************************************************
 Open an Ole Stream within the current storage
*******************************************************************)
function TStructuredStorage.OpenStream(var   Storage     : IStorage;
                                       const StreamName  : String;
                                             grfMode     : LongInt;
                                       var   Stream      : IStream) : Boolean;
begin
  hr := Storage.OpenStream(PWideChar(WideString(StreamName)),
                           nil,
                           grfMode,
                           0,
                           Stream);
  Result := not DidFail(hr);
end;

(*******************************************************************
 Free up an OLE stream.
*******************************************************************)
procedure TStructuredStorage.DoneStream(var Stream : IStream);
begin
  Stream := nil;
end;

(*******************************************************************
                           WriteString
 Write a string to an opened storage stream.
*******************************************************************)
function TStructuredStorage.WriteString(var   Stream : IStream;
                                        const S      : String) : Boolean;
var
  Size : LongInt;
begin
  Result := False;
  if WriteInt(Stream, Length(S)) then
  begin
    Size := 0;
    hr := Stream.Write(PChar(S), Length(S), @Size);
    if DidFail(hr) then
      {Raise}
    else
      Result := (Size = Length(S));
  end;
end;

(*******************************************************************
                           ReadString
 Read a string from an opened storage stream.
*******************************************************************)
function TStructuredStorage.ReadString(var Stream    : IStream;
                                       var S         : String) : Boolean;
var
  StrSize : Integer;
  Size    : LongInt;
  Ps      : PChar;
begin
  Result := False;
  S := '';
  if ReadInt(Stream, StrSize) then
  begin
    Size := 0;
    GetMem(Ps, StrSize + 1);
    try
       hr := Stream.Read(Ps, StrSize, @Size);
       if DidFail(hr) then
        {Raise}
       else if Size > 0 then
       begin
         S := String(Ps);
         SetLength(S, Size);
         Result := (Size = StrSize);
       end;
    finally
      FreeMem(Ps, StrSize + 1);
    end;
  end;
end;

(*******************************************************************
                           WriteInt
 Write an Integer to the stream.
*******************************************************************)
function TStructuredStorage.WriteInt(var Stream : IStream;
                                         TInt   : Integer) : Boolean;
var
  Size : LongInt;
begin
  Result := False;
  Size := 0;
  hr := Stream.Write(@TInt,SizeOf(Integer),@Size);
  if DidFail(hr) then
    {Raise}
  else
    Result := (Size = SizeOf(Integer));
end;

(*******************************************************************
                           ReadInt
 Read a string from an opened storage stream.
*******************************************************************)
function TStructuredStorage.ReadInt(var Stream    : IStream;
                                    var TInt      : Integer) : Boolean;
var
  Size : LongInt;
begin
  Result := False;
  Size := 0;
  hr := Stream.Read(@Tint, SizeOf(Integer), @Size);
  if DidFail(hr) then
    {Raise}
  else
    Result := (Size = SizeOf(Integer));
end;

function TStructuredStorage.CopyTo(var aFromStorage, aToStorage : IStorage) : boolean;
begin
  hr := aFromStorage.CopyTo(0, nil, nil, aToStorage);
  result := not DidFail(hr);
end;

function TStructuredStorage.Commit(var Storage : IStorage; grfCommitFlags : LongInt): boolean;
begin
  hr := Storage.Commit(grfCommitFlags);
  Result := not DidFail(hr);
end;

function TStructuredStorage.Revert(var Storage : IStorage): boolean;
begin
  hr := Storage.Revert;
  Result := not DidFail(hr);
end;

function TStructuredStorage.DefragmentStorageFile(const aFileName : string): boolean;
var
  pIStorageOld, pIStorageNew : IStorage;
  st : STATSTG;
  szTemp : String;
begin
  result := False;
  // if we have an open storage file then bail out early
  if StorageFile <> nil then exit;
  // if the specified file is not a storage file then bail out early
  if not IsStorageFile(aFileName) then exit;
  // okay, now try to defragment the file
  // first, create a new temporary storage
  hr := StgCreateDocfile(PWideChar(nil),
                         STGM_CREATE or STGM_READWRITE or
                         STGM_DIRECT or STGM_SHARE_EXCLUSIVE,
                         0,
                         pIStorageNew);
  if not DidFail(hr) then
  begin
    // open the old storage read only
    hr := StgOpenStorage(PWideChar(WideString(aFileName)),
                         nil,
                         STGM_DIRECT or STGM_READ or
                         STGM_SHARE_DENY_WRITE,
                         nil,
                         0,
                         pIStorageOld);
    if not DidFail(hr) then
    begin
      // do the work here
      CopyTo(pIStorageOld, pIStorageNew);
      //Get the name of the temp file.
      pIStorageNew.Stat(st, 0);
      // close the temp storage
      pIStorageNew := nil;
      // close our old storage
      pIStorageOld := nil;
      //Delete the old file before copying back to insure truncation
      DeleteFile(PChar(aFileName));
      szTemp := st.pwcsName;
      MoveFile(PChar(szTemp), PChar(aFileName));
      Result := True;
    end
    else
    begin
      // make sure we close our new storage
      pIStorageNew := nil;
    end;
  end;
end;

procedure TStructuredStorage.CloseStorageFile;
begin
  StorageFile := nil;
  FFileName := '';
end;

function TStructuredStorage.MoveElementTo(var aSrcStorage: IStorage;
  const aSrcName: string; var aDestStorage: IStorage; const aDestName: string;
  aMakeCopy: boolean): Boolean;
var
  grfFlag : LongInt;
begin
  if aMakeCopy then
    grfFlag := STGMOVE_COPY
  else
    grfFlag := STGMOVE_MOVE;
  hr := aSrcStorage.MoveElementTo(PWideChar(WideString(aSrcName)),
                                  aDestStorage,
                                  PWideChar(WideString(aDestName)),
                                  grfFlag);
  Result := not DidFail(hr);
end;

function TStructuredStorage.DestroyElement(var aStorage: IStorage;
  const aElement: string): Boolean;
begin
  hr := aStorage.DestroyElement(PWideChar(WideString(aElement)));
  result := not DidFail(hr);
end;

function TStructuredStorage.RenameElement(var aStorage: IStorage;
  const aOldName, aNewName: string): Boolean;
begin
  hr := aStorage.RenameElement(PWideChar(WideString(aOldName)),
                               PWideChar(WideString(aNewName)));
  result := not DidFail(hr);
end;

function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  DosDateTime : Integer;
begin
  DosDateTime := DateTimeToFileDate(DateTime);
  DosDateTimeToFileTime(LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo, Result);
end;

function TStructuredStorage.SetElementTimes(var aStorage: IStorage;
  const aElement : string; cTime, aTime, mTime: TDateTime): Boolean;
var
  ftC, ftA, ftM : TFileTime;
begin
  ftC := DateTimeToFileTime(cTime);
  ftA := DateTimeToFileTime(aTime);
  ftM := DateTimeToFileTime(mTime);
  hr := aStorage.SetElementTimes(PWideChar(WideString(aElement)),
                                 ftC, ftA, ftM);
  result := not DidFail(hr);
end;

end.

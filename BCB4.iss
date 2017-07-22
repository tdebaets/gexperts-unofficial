[Setup]
AllowNoIcons=1
AppCopyright=Copyright 1996-2000 by GExperts, Inc. and Erik Berry
AppName=GExperts
AppVerName=GExperts for C++Builder 4
AppID=GExpertsBCB4
CompressLevel=9
DefaultDirName={pf}\GExperts
DefaultGroupName=GExperts
LicenseFile=License.txt
InfoBeforeFile=PreInstall.txt
UninstallDisplayIcon={app}\Icons\ExpMgr.ico
AppPublisher=GExperts Development Team
AppPublisherURL=http://www.gexperts.org/
AppVersion=1.0
AppMutex=GExperts.Addin.For.Borland.IDEs

[Files]
Source: "BCB4\ExpMgr.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "BCB4\GDebug.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "BCB4\Grep.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExperts.hlp"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExperts.cnt"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExpert4_bcb.dll"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "DbugIntf.pas"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "Dictionary.px"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Dictionary.db"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Replacement.px"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Replacement.db"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Icons\*.ico"; DestDir: "{app}\Icons"; CopyMode: alwaysoverwrite
Source: "Readme.txt"; DestDir: "{app}"; CopyMode: alwaysoverwrite; Flags: isreadme
Source: "BCBNotes.txt"; DestDir: "{app}"; CopyMode: alwaysoverwrite;

[Icons]
Name: "{group}\Debug Window"; Filename: "{app}\GDebug.exe"
Name: "{group}\Expert Manager"; Filename: "{app}\ExpMgr.exe"
Name: "{group}\GExperts Help"; Filename: "{app}\GExperts.hlp"
Name: "{group}\GExperts Readme"; Filename: "{app}\Readme.txt"
Name: "{group}\BCB Notes"; Filename: "{app}\BCBNotes.txt"
Name: "{group}\Grep Search"; Filename: "{app}\Grep.exe"

[Registry]
Root: HKCU; Subkey: "Software\Borland\C++Builder\4.0\Experts"; ValueType: STRING; ValueName: "GExperts"; ValueData: "{app}\GExpert4_bcb.dll"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "Software\Borland\C++Builder\4.0\GExperts\Misc"; ValueType: STRING; ValueName: "ConfigPath"; ValueData: "{app}"
Root: HKCU; Subkey: "Software\Borland\C++Builder\4.0\GExperts\Misc"; ValueType: STRING; ValueName: "HelpFile"; ValueData: "{app}\GExperts.hlp"
Root: HKCU; Subkey: "Software\GExperts\Debug"; ValueType: STRING; ValueName: "FilePath"; ValueData: "{app}\GDebug.exe"

[Dirs]
Name: "{app}\Icons"

[UninstallDelete]
Type: files; Name: "{app}\ProofReaderHist.*"
Type: files; Name: "{app}\GExpert.*"


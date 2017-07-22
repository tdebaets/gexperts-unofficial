[Setup]
AllowNoIcons=1
AppCopyright=Copyright 1996-2000 by GExperts, Inc. and Erik Berry
AppName=GExperts
AppVerName=GExperts for Delphi 3
AppID=GExpertsDelphi3
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
Source: "D3\ExpMgr.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "D3\GDebug.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "D3\Grep.exe"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExperts.hlp"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExperts.cnt"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "GExpert3.dll"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "DbugIntf.pas"; DestDir: "{app}"; CopyMode: alwaysoverwrite
Source: "Dictionary.px"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Dictionary.db"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Replacement.px"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Replacement.db"; DestDir: "{app}"; CopyMode: onlyifdoesntexist
Source: "Icons\*.ico"; DestDir: "{app}\Icons"; CopyMode: alwaysoverwrite
Source: "Readme.txt"; DestDir: "{app}"; CopyMode: alwaysoverwrite; Flags: isreadme

[Icons]
Name: "{group}\Debug Window"; Filename: "{app}\GDebug.exe"
Name: "{group}\Expert Manager"; Filename: "{app}\ExpMgr.exe"
Name: "{group}\GExperts Help"; Filename: "{app}\GExperts.hlp"
Name: "{group}\GExperts Readme"; Filename: "{app}\Readme.txt"
Name: "{group}\Grep Search"; Filename: "{app}\Grep.exe"

[Registry]
Root: HKCU; Subkey: "Software\Borland\Delphi\3.0\Experts"; ValueType: STRING; ValueName: "GExperts"; ValueData: "{app}\GExpert3.dll"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "Software\Borland\Delphi\3.0\GExperts\Misc"; ValueType: STRING; ValueName: "ConfigPath"; ValueData: "{app}"
Root: HKCU; Subkey: "Software\Borland\Delphi\3.0\GExperts\Misc"; ValueType: STRING; ValueName: "HelpFile"; ValueData: "{app}\GExperts.hlp"
Root: HKCU; Subkey: "Software\GExperts\Debug"; ValueType: STRING; ValueName: "FilePath"; ValueData: "{app}\GDebug.exe"

[Dirs]
Name: "{app}\Icons"

[UninstallDelete]
Type: files; Name: "{app}\ProofReaderHist.*"
Type: files; Name: "{app}\GExpert.*"


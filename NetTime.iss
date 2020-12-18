; -- NetTime.iss --
; Inno Setup installer script for NetTime 2.0
; To get a copy of Inno Setup (which is free) go to:
;    http://www.innosetup.com

[Setup]
AppName=NetTime
AppVerName=NetTime 2.0
AppCopyright=Copyright © 1997-2001 Graham Mainwaring
DefaultDirName={pf}\NetTime
DefaultGroupName=NetTime
UninstallDisplayIcon={app}\NetTime.exe
SourceDir=src
OutputDir=dist

[Files]
Source: "NetTime.exe"; DestDir: "{app}"
Source: "NeTmSv95.exe"; DestDir: "{app}"
Source: "NeTmSvNT.exe"; DestDir: "{app}"
Source: "SvcCfg.exe"; DestDir: "{app}"
Source: "NetTime.hlp"; DestDir: "{app}"
Source: "Servers.ini"; DestDir: "{app}"
Source: "Readme.txt"; DestDir: "{app}"
Source: "RFC2030.txt"; DestDir: "{app}"
Source: "RFC868.txt"; DestDir: "{app}"

[Icons]
Name: "{group}\NetTime"; Filename: "{app}\NetTime.exe"

[Run]
Filename: "{app}\SvcCfg.exe"; Parameters: """{app}"""

[UninstallRun]
Filename: "{app}\SvcCfg.exe"; Parameters: """{app}"" /uninstall"

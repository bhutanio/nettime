NetTime is an unobtrusive taskbar tray application that keeps a Windows PC's
clock in sync with a time server. It supports both the RFC868 protocol (also
known as "traditional unix time") and NTP. Version 1.0 was released in May 
1997 as freeware; NetTime became open-source in November 1998.

Version 2.0 adds substantial new functionality. Most notably, NetTime 2.0 
can now sync to a simple network time protocol (SNTP) server. This protocol
allows sub-second resolution and is also supported by a more extensive
collection of public-access Internet servers. NetTime 2.0 also includes
service code so that it keeps the time synchronized even when the 
interactive user is logged out.

In order to communicate between the time synchronization service and the user
interface, NetTime 2.0 uses COM servers. These have been tested on Windows 98,
NT 4.0 and 2000 and run fine in an 'out-of-the-box' configuration - though if
you're using NT 4.0, Service Pack 2 or higher is strongly recommended. On
Windows 95, NetTime 2.0 works fine but requires that you install DCOM95.EXE,
available for free download from Microsoft.

The source code for NetTime continues to be available under a BSD-style 
license. It compiles without warnings on Delphi 5. I have not tested it on
other versions. In addition to my own code, the following people have 
contributed to NetTime (knowingly or otherwise):

Pete Ness - TTrayIcon component (see ttrayicon.pas)
Kerry Neighbour - NT security code & bug fixes

Graham Mainwaring
graham@mhn.org

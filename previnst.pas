unit previnst;

{Unit PREVINST.pas - 
Include this unit in your project file (.dpr) to keep multiple instances 
  of your application from running.  Works the same way as the standard method 
  of using mutexes, except uses a semaphore and stores the application handle in 
  the semaphore's count so we can get it to show the original instance. 
  You would call it like this:

  program Project1; 

uses 
   Forms, 
   Unit1 in 'Unit1.pas', 
   PREVINST in 'PREVINST.pas'; 

begin 
    if not HavePrevInstance(Application.Handle, 'SOmeWEIRDnameFoRTheSemaphOre') then 
    begin 
     Application.Initialize; 
   Application.CreateForm(TForm1, Form1); 
   Application.Run; 
    end;
  end.} 

interface 

uses Windows; 

function HavePrevInstance(AppHandle: THandle; UniqueName: String): boolean; 

implementation 

function HavePrevInstance(AppHandle: THandle; UniqueName: String): boolean; 
const 
  ERROR_ALREADY_EXISTS = 183; //Not in Windows.pas - dont know why 
  SEMAPHORE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or 3; //Ditto 
  WM_SYSCOMMAND = 274; //Define here so dont have to include Messages.pas 
var 
  MyHandle: THandle; 
begin 
  Result := False; //Set Initial Result to false 
  //Now we create the semaphore and store the applications handle as its count adding one to it so we can keep toggling it back and forth 
  MyHandle := CreateSemaphore(nil, AppHandle + 1, AppHandle + 1, PChar(UniqueName)); 
  //The following will be true if an instance of out app is already running
  if (MyHandle <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then 
  begin 
    Result := True; //Set to result to true 
    //Now we open the semaphore to get its handle 
    MyHandle := OpenSemaphore(SEMAPHORE_ALL_ACCESS, False, PChar(UniqueName)); 
    //at this point MyHandle is our app handle plus one. now we reduce count by one to get back our original handle 
    WaitForSingleObject(MyHandle, 0); 
    //This replaces the value of MyHandle with the Application handle of the prev instance and pushes our count back up again 
    ReleaseSemaphore(MyHandle, 1, @MyHandle); 
    //now we have the handle to the previous instance - do whatever you want 
    //This is what I usually do but you could pop up a messagebox or whatever 
    if IsIconic(MyHandle) then 
     PostMessage(MyHandle, WM_SYSCOMMAND, SC_RESTORE, 0) 
    else 
     SetForeGroundWindow(MyHandle); 
  end; 
end; 

end. 



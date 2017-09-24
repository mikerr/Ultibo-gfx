program HelloVideocube;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Videocube with Raspicam                                      }
{                                                                              }
{ Combining the 3D cube from hello triangle with video from an attached camera  }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{                                                                              }
{ You also MUST create a config.txt file in the root directory of your SD card }
{ with at least the following setting:                                         }
{                                                                              }
{ start_x=1                                                                             }
{ gpu_mem=128                                                                  }
{                                                                              }
{ This version is for Raspberry Pi A/B/A+/B+/Zero                              }

uses
  RaspberryPi, {Include RaspberryPi to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;

 argc:int;      {Some command line arguments to pass to the C code}
 argv:PPChar;

{Link our C library to include the original example} 
{$linklib hello_videocube}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_videocube; cdecl; external 'hello_videocube' name 'hello_videocube';

{Link our C library to include the original example}
{$linklib raspivid}

{Import the main function of the example so we can call it from Ultibo}
function vcraspivid(argc: int; argv: PPChar): int; cdecl; external 'raspivid' name 'vcraspivid';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Videocube');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

  MMALIncludeComponentVideocore;
 argv:=AllocateCommandLine('--timeout 5000 --framerate 25 --bitrate 1200000 --preview 100,100,640,480 --output C:\test.h264',argc);

 {Call the main function of raspivid, it will return here when completed}
 vcraspivid(argc, argv);

 {Release the C command line}
 ReleaseCommandLine(argv);


 {Call the main function of the example, it will return here when completed (if ever)}
 hello_videocube;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Videocube');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

program DisplayAllLeds;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }

{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  MAX7219,
  Ultibo,
  StrUtils;

var
  i, n: integer;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create();

  try
    mx.Send(SCAN_LIMIT, 7);     // set up to scan eight digits
    mx.Send(DECODE_MODE, 0);    // no decode on all codes
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    mx.Send(DISPLAY_TEST, 1);  // Enable test mode
    Sleep(500);
    mx.Send(DISPLAY_TEST, 0);  // Disable test mode

    // Clear all displays
    for i := 1 to 8 do
      mx.Send(i, 0);

    Sleep(500);

    for i := 1 to 8 do
      for n := 0 to 255 do
      begin
        mx.Send(i, n);
        mx.Send(0, 0);
        mx.Send(i, n);
        Sleep(50);
      end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

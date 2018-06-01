program Display1To6;

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
  i: integer;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create();

  try
    mx.Send(SCAN_LIMIT, 5);     // set up to scan six digits
    mx.Send(DECODE_MODE, 255); // Set 7-segment decode mode on
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    mx.Send(DISPLAY_TEST, 1);  // Enable test mode
    Sleep(500);
    mx.Send(DISPLAY_TEST, 0);  // Disable test mode

    // Clear all displays
    for i := 1 to 6 do
      mx.Send(i, 15);

    Sleep(500);

    mx.Send(1, 6);      // displays the number 6 on digit 1
    Sleep(1000);
    mx.Send(2, 5);      // displays the number 5 on digit 2
    Sleep(1000);
    mx.Send(3, 4);      // displays the number 4 on digit 3
    Sleep(1000);
    mx.Send(4, 3);      // displays the number 3 on digit 4
    Sleep(1000);
    mx.Send(5, 2);      // displays the number 2 on digit 5
    Sleep(1000);
    mx.Send(6, 1);      // displays the number 1 on digit 6
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

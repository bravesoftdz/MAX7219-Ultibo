program Display1To6Seg;

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
    mx.Send(SCAN_LIMIT, 5);    // set up to scan six digits
    mx.Send(DECODE_MODE, 0);   // Set 7-segment decode mode on
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    // Clear all displays
    for i := 1 to 6 do
      mx.Send(i, 0);

    Sleep(200);

    for i := 0 to 255 do
    begin
      mx.Send(1, i);
      Sleep(1000);
    end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

program DisplayAllLeds2D;

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
  Ultibo,
  StrUtils,
  Math,
  MAX7219;

var
  i, n: integer;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create(2);

  try
    mx.Send(SCAN_LIMIT, 7);     // set up to scan eight digits
    mx.Send(DECODE_MODE, 0);    // no decode on all codes
    mx.Send(INTENSITY, 10);     // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    // Clear all displays
    for i := 1 to 8 do
      mx.Send(i, 0);

    Sleep(200);

    for i := 1 to 8 do
      for n := 0 to 7 do
      begin
        mx.Send(1, i, Trunc(Power(2, n)));
        mx.Send(2, 9 - i, Trunc(Power(2, n)));
        Sleep(100);
      end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

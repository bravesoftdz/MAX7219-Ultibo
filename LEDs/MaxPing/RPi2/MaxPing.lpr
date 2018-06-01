program MaxPing;

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
  MAX7219;

var
  i, n: integer;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create();

  try
    mx.Send(SCAN_LIMIT, 5);    // set up to scan six digits
    mx.Send(DECODE_MODE, 0);   // Set 7-segment decode mode on
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits
    mx.Send(DISPLAY_TEST, 1);  // Enable test mode
    Sleep(10);
    mx.Send(DISPLAY_TEST, 0);  // Disable test mode

    // Clear all displays
    for i := 1 to 6 do
      mx.Send(i, 0);

    Sleep(200);

    while True do
    begin
      for i := 1 to 6 do
      begin
        if i and 1 = 1 then
        begin
          mx.Send(i, 1 + 2 + 32 + 64);
          mx.Send(7 - i, 1 + 4 + 8 + 16);
        end
        else
        begin
          mx.Send(i, 1 + 4 + 8 + 16);
          mx.Send(7 - i, 1 + 2 + 32 + 64);
        end;

        Sleep(100);
        mx.Send(i, 0);
        mx.Send(7 - i, 0);
      end;

      for i := 5 downto 2 do
      begin
        if i and 1 = 1 then
        begin
          mx.Send(i, 1 + 2 + 32 + 64);
          mx.Send(7 - i, 1 + 4 + 8 + 16);
        end
        else
        begin
          mx.Send(i, 1 + 4 + 8 + 16);
          mx.Send(7 - i, 1 + 2 + 32 + 64);
        end;

        Sleep(100);
        mx.Send(i, 0);
        mx.Send(7 - i, 0);
      end;
    end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

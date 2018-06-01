program DisplayChars3;

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
  Console,
  SysUtils,
  Classes,
  MAX7219;

var
  WindowHandle: TWindowHandle;
  i, n, s: integer;
  mx: TMAX7219;
  Buffer: TArrayDisplay;

begin
  {Let's create a console window so we can report what is happening}
  WindowHandle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  {Display a welcome message}
  ConsoleWindowWriteLn(WindowHandle, 'Display Chars MAX7219');

  mx := TMAX7219.Create(3, DATA_PIN, LOAD_PIN, CLOCK_PIN);

  try
    try
      mx.Send(SCAN_LIMIT, 7);     // set up to scan eight digits
      mx.Send(DECODE_MODE, 0);    // no decode on all codes
      mx.Send(INTENSITY, 10);     // set brightness 0 to 15
      mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

      mx.Send(DISPLAY_TEST, 1);  // Enable test mode
      Sleep(1000);
      mx.Send(DISPLAY_TEST, 0);  // Disable test mode

      // Clear all the display
      for i := 1 to 8 do
        mx.Send(i, 0);

      Sleep(200);
      Buffer := mx.GenString('Amanda & Bianca');

      i := 0;
      s := High(Buffer);

      while True do
      begin

      end;
    except
      on E: Exception do
        ConsoleWindowWriteLn(WindowHandle, 'Error: ' + E.Message);
    end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.


program MaxCount;

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
  i, n, v: integer;
  s: string;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create();

  try
    mx.Send(SCAN_LIMIT, 5);     // set up to scan six digits
    mx.Send(DECODE_MODE, 255); // Set 7-segment decode mode on for all digits
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    // Clear all displays
    for i := 1 to 6 do
      mx.Send(i, 15);

    Sleep(500);

    while True do
    begin
      for i := 0 to 999999 do
      begin
        s := PadLeft(IntToStr(i), 6);

        for n := Length(s) downto 1 do
        begin
          if s[n] = ' ' then
            v := 15
          else
          begin
            v := StrToInt(s[n]);

            if i and 1 = 1 then
              v := V or 128;
          end;

          mx.Send(7 - n, v);
        end;

        Sleep(50);
      end;

      for i := 999998 downto 1 do
      begin
        s := PadLeft(IntToStr(i), 6);

        for n := Length(s) downto 1 do
        begin
          if s[n] = ' ' then
            v := 15
          else
          begin
            v := StrToInt(s[n]);

            if i and 1 = 1 then
              v := V or 128;
          end;

          mx.Send(7 - n, v);
        end;

        Sleep(50);
      end;
    end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

program MaxChrono;

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
  i, v: integer;
  hr, mn, sg: word;
  s: string;
  T1, T2: ULONGLONG;
  mx: TMAX7219;

begin
  mx := TMAX7219.Create();

  try
    mx.Send(SCAN_LIMIT, 5);     // set up to scan six digits
    mx.Send(DECODE_MODE, 255); // Set 7-segment decode mode on
    mx.Send(INTENSITY, 10);    // set brightness 0 to 15
    mx.Send(SHUTDOWN, 1);      // come out of shutdown mode / turn on the digits

    // Clear all displays
    for i := 1 to 6 do
      mx.Send(i, 15);

    Sleep(500);

    hr := 0;
    mn := 0;
    sg := 0;
    T1 := GetTickCount64();

    while True do
    begin
      s := Format('%0.2d%0.2d%0.2d', [hr, mn, sg]);

      for i := Length(s) downto 1 do
      begin
        v := StrToInt(s[i]);
        mx.Send(7 - i, v);
      end;

      Inc(sg);

      if sg > 59 then
      begin
        sg := 0;

        Inc(mn);

        if mn > 59 then
        begin
          mn := 0;

          Inc(hr);

          if hr > 99 then
            hr := 0;
        end;
      end;

      repeat
        T2 := GetTickCount64();
      until T2 - T1 >= 1000;

      T1 := T2;
    end;
  finally
    mx.Free;
  end;

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

program DisplayChars2;

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
  i, n: integer;
  mx: TMAX7219;
  n1, n2, n3: integer;
  p1, p2, p3: integer;

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

      //mx.Send(DISPLAY_TEST, 1);  // Enable test mode
      //Sleep(1000);
      //mx.Send(DISPLAY_TEST, 0);  // Disable test mode

      // Clear all the display
      for i := 1 to 8 do
        mx.Send(i, 0);

      Sleep(500);

      while True do
      begin
        for n := 1 to 255 do
        begin
          n1 := mx.GetCharPos(n); // get char pos
          p1 := sysfont[n1];  // get char length

          if n + 1 > 255 then
          begin
            n2 := mx.GetCharPos(32); // get char pos
            p2 := sysfont[n2];  // get char length
            n3 := n2;
            p3 := p2;
          end
          else
          begin
            n2 := mx.GetCharPos(n + 1); // get char pos
            p2 := sysfont[n2];  // get char length

            if n + 2 > 225 then
            begin
              n3 := mx.GetCharPos(32); // get char pos
              p3 := sysfont[n3];  // get char length
            end
            else
            begin
              n3 := mx.GetCharPos(n + 2); // get char pos
              p3 := sysfont[n3];  // get char length
            end;
          end;

          for i := 1 to 8 do
          begin
            Inc(n1);
            Inc(n2);
            Inc(n3);

            //mx.StartSend();

            try
              if p3 > 0 then
              begin
                //mx.DoSend(i, sysfont[n3]);
                mx.Send(3, i, sysfont[n3]);
                Dec(p3);
              end
              else
                //mx.DoSend(i, 0);
                mx.Send(3, i, 0);

              if p2 > 0 then
              begin
                //mx.DoSend(i, sysfont[n2]);
                mx.Send(2, i, sysfont[n2]);
                Dec(p2);
              end
              else
                //mx.DoSend(i, 0);
                mx.Send(2, i, 0);

              if p1 > 0 then
              begin
                //mx.DoSend(i, sysfont[n1]);
                mx.Send(1, i, sysfont[n1]);
                Dec(p1);
              end
              else
                //mx.DoSend(i, 0);
                mx.Send(1, i, 0);
            finally
              //mx.EndSend();
            end;
          end;

          Sleep(400);
        end;
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


program FreeTypeDemo;

{$mode objfpc}{$H+}

{ Raspberry Pi 3 Application                                                   }
{  A simple app to demostate how to use the FreeType library to draw text onto }
{ the screen.                                                                  }
{ pjde Jan 2017                                                                }

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  Console,
  GraphicsConsole,
  freetypeh                           // FreeType header file
  { Add additional units here };

const
  DPI = 72;                           // dots per inch

var
  FTLib : PFT_Library;                // handle to FreeType library
  res : integer;
  Console1, Console2 : TWindowHandle;
  size: integer;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

procedure Log (s : string);
begin
   ConsoleWindowWriteLn (Console1, s);
end;

function GetRValue (c : cardinal) : byte; inline;
begin
  Result := (c and $ff0000) shr 16;
end;

function GetGValue (c : cardinal) : byte; inline;
begin
  Result :=  (c and $ff00) shr 8;
end;

function GetBValue (c : cardinal) : byte; inline;
begin
  Result := c and $ff;
end;

function rgb (r, g, b : byte)  : cardinal; inline;
begin
  Result := $ff000000 + (r shl 16) + (g shl 8) + b;
end;

procedure DrawText (Console : TWindowHandle; x, y: integer; s, f: string; sz: integer; c: cardinal);
var
  err : integer;
  aFace : PFT_Face;
  fn : string;
  i, tx, ty : integer;
  kerning : boolean;
  glyph_index,
  prev : cardinal;
  delta : FT_Vector;
  bg : LongWord;

  procedure DrawChar (b : FT_Bitmap; dx, dy : integer);
  var
    i , j : LongWord;
    fm : PByte;
    rd, gn, bl : byte;
    Buffer : PLongword;
    BufferSize : integer;
    BufferPos : PLongWord;
  begin
    BufferSize := b.width * b.rows * SizeOf (LongWord);
    if BufferSize = 0 then exit;
    GetMem (Buffer, BufferSize);
    FillChar (Buffer^, BufferSize, 0);
    BufferPos := Buffer;
    for j := 0 to b.rows - 1 do
      for i := 0 to b.width - 1 do
        begin
          LongWord (fm) := LongWord (b.buffer) + j * Longword (b.width) + i; // read alpha value of font char
          rd := ((GetRValue (c) * fm^) + (GetRValue (bg) * (255 - fm^))) div 255;
          gn := ((GetGValue (c) * fm^) + (GetGValue (bg) * (255 - fm^))) div 255;
          bl := ((GetBValue (c) * fm^) + (GetBValue (bg) * (255 - fm^))) div 255;
          BufferPos^ := rgb (rd, gn, bl);
          Inc (BufferPos, 1);
        end;
    GraphicsWindowDrawImage (Console, dx, dy, Buffer, b.width, b.rows, COLOR_FORMAT_ARGB32);
    FreeMem (Buffer);
  end;

begin
  if not Assigned (FTLib) then exit;
  if Console = INVALID_HANDLE_VALUE then exit;
  bg := GraphicsWindowGetBackcolor (Console);  // back ground colour
  aFace := nil;
  tx := x;
  ty := y;
  delta.x := 0;
  delta.y := 0;
  if ExtractFileExt (f) = '' then
    fn := f + '.ttf'
  else
    fn := f;
  err := FT_New_Face (FTLib, PChar (fn), 0, aFace);
  if err = 0 then  // if font face loaded ok
    begin
      err := FT_Set_Char_Size (aFace,                   // handle to face object
             0,                                         // char_width in 1/64th of points - Same as height
             sz * 64,                                   // char_height in 1/64th of points
             DPI,                                       // horizontal device resolution
             0);                                        // vertical device resolution
      if err = 0 then
        begin
          prev := 0;    // no previous char
          kerning := FT_HAS_KERNING (aFace);
          for i := 1 to length (s) do
            begin                                       // convert character code to glyph index
              glyph_index := FT_Get_Char_Index (aFace, cardinal (s[i]));
              if kerning and (prev <> 0) and (glyph_index <> 0) then
                begin
                  FT_Get_Kerning (aFace, prev, glyph_index, FT_KERNING_DEFAULT, &delta);
                  tx := tx + delta.x div 64;
                end;
               // load glyph image into the slot (erase previous one)
               err := FT_Load_Glyph (aFace, glyph_index, FT_LOAD_RENDER);
               if err > 0 then continue;                // ignore errors
               // now draw to our target surface
               DrawChar (aFace^.glyph^.bitmap, tx + aFace^.glyph^.bitmap_left,
                          ty - aFace^.glyph^.bitmap_top);
               tx := tx + aFace^.glyph^.advance.x div 64;
               prev := glyph_index;
            end;
        end;
      FT_Done_Face (aFace);
    end
  else Log ('Can''t find font file ' + fn);
end;


begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOP, false);
  Console2 := GraphicsWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOM);
  WaitForSDDrive;                     // wait for SD Drive to initialise
  Log ('Simple FreeType Demo.');
  FTLib := nil;
  res := FT_Init_FreeType (FTLib);    // initialise library
  if (res = 0) then
    begin

    For size := 8 to 64 do
       begin
            DrawText (Console2, 10, 60, 'FreeType 2 demo.', 'ariali', size-1, COLOR_WHITE);
            DrawText (Console2, 10, 120, 'Welcome to the world of Ultibo.', 'arialbi', size-1, COLOR_WHITE);


            DrawText (Console2, 10, 60, 'FreeType 2 demo.', 'ariali', size, COLOR_GREEN);
            DrawText (Console2, 10, 120, 'Welcome to the world of Ultibo.', 'arialbi', size, COLOR_RED);

            Sleep(100);

       end;
      FT_Done_FreeType (FTLib);       // close library
    end;
  Log ('All done.');
  ThreadHalt (0);
end.

program SideScroller;

{$mode objfpc}{$H+}{$inline on}

{Declare some units used by this example.}
uses
 RaspberryPi3, {The RaspberryPi3 unit gives us all the relevant drivers}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Framebuffer,  {Most of the functions we need come from the Framebuffer unit}
 Console,
 SysUtils;


{Some variables used by the animation}
var
 PageSize:Integer;
 CurrentPage:Integer;
 BufferStart:Pointer;
 FramebufferDevice:PFramebufferDevice;
 FramebufferProperties:TFramebufferProperties;


{A couple of constants, what happens if you change NUM_ELEMENTS?}
const
 NUM_ELEMENTS = 1000;
 RANDOM_MAX:LongInt = 2147483647;


{Some array variables to track the position of each box}
var
 ElementX:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementY:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementDirX:array[0..NUM_ELEMENTS - 1] of Integer;
 ElementDirY:array[0..NUM_ELEMENTS - 1] of Integer;


{The "standard" VGA Mode 13h palette for 8 bit colors (See: https://en.wikipedia.org/wiki/Mode_13h)}
const
 VGAPalette:TFramebufferPalette = (
  Start:0;
  Count:256;
  Entries:
  ($FF000000,$FF0000AA,$FF00AA00,$FF00AAAA,$FFAA0000,$FFAA00AA,$FFAA5500,$FFAAAAAA,$FF555555,$FF5555FF,$FF55FF55,$FF55FFFF,$FFFF5555,$FFFF55FF,$FFFFFF55,$FFFFFFFF,
   $FF000000,$FF141414,$FF202020,$FF2C2C2C,$FF383838,$FF444444,$FF505050,$FF606060,$FF707070,$FF808080,$FF909090,$FFA0A0A0,$FFB4B4B4,$FFC8C8C8,$FFE0E0E0,$FFFCFCFC,
   $FF0000FC,$FF4000FC,$FF7C00FC,$FFBC00FC,$FFFC00FC,$FFFC00BC,$FFFC007C,$FFFC0040,$FFFC0000,$FFFC4000,$FFFC7C00,$FFFCBC00,$FFFCFC00,$FFBCFC00,$FF7CFC00,$FF40FC00,
   $FF00FC00,$FF00FC40,$FF00FC7C,$FF00FCBC,$FF00FCFC,$FF00BCFC,$FF007CFC,$FF0040FC,$FF7C7CFC,$FF9C7CFC,$FFBC7CFC,$FFDC7CFC,$FFFC7CFC,$FFFC7CDC,$FFFC7CBC,$FFFC7C9C,
   $FFFC7C7C,$FFFC9C7C,$FFFCBC7C,$FFFCDC7C,$FFFCFC7C,$FFDCFC7C,$FFBCFC7C,$FF9CFC7C,$FF7CFC7C,$FF7CFC9C,$FF7CFCBC,$FF7CFCDC,$FF7CFCFC,$FF7CDCFC,$FF7CBCFC,$FF7C9CFC,
   $FFB4B4FC,$FFC4B4FC,$FFD8B4FC,$FFE8B4FC,$FFFCB4FC,$FFFCB4E8,$FFFCB4D8,$FFFCB4C4,$FFFCB4B4,$FFFCC4B4,$FFFCD8B4,$FFFCE8B4,$FFFCFCB4,$FFE8FCB4,$FFD8FCB4,$FFC4FCB4,
   $FFB4FCB4,$FFB4FCC4,$FFB4FCD8,$FFB4FCE8,$FFB4FCFC,$FFB4E8FC,$FFB4D8FC,$FFB4C4FC,$FF000070,$FF1C0070,$FF380070,$FF540070,$FF700070,$FF700054,$FF700038,$FF70001C,
   $FF700000,$FF701C00,$FF703800,$FF705400,$FF707000,$FF547000,$FF387000,$FF1C7000,$FF007000,$FF00701C,$FF007038,$FF007054,$FF007070,$FF005470,$FF003870,$FF001C70,
   $FF383870,$FF443870,$FF543870,$FF603870,$FF703870,$FF703860,$FF703854,$FF703844,$FF703838,$FF704438,$FF705438,$FF706038,$FF707038,$FF607038,$FF547038,$FF447038,
   $FF387038,$FF387044,$FF387054,$FF387060,$FF387070,$FF386070,$FF385470,$FF384470,$FF505070,$FF585070,$FF605070,$FF685070,$FF705070,$FF705068,$FF705060,$FF705058,
   $FF705050,$FF705850,$FF706050,$FF706850,$FF707050,$FF687050,$FF607050,$FF587050,$FF507050,$FF507058,$FF507060,$FF507068,$FF507070,$FF506870,$FF506070,$FF505870,
   $FF000040,$FF100040,$FF200040,$FF300040,$FF400040,$FF400030,$FF400020,$FF400010,$FF400000,$FF401000,$FF402000,$FF403000,$FF404000,$FF304000,$FF204000,$FF104000,
   $FF004000,$FF004010,$FF004020,$FF004030,$FF004040,$FF003040,$FF002040,$FF001040,$FF202040,$FF282040,$FF302040,$FF382040,$FF402040,$FF402038,$FF402030,$FF402028,
   $FF402020,$FF402820,$FF403020,$FF403820,$FF404020,$FF384020,$FF304020,$FF284020,$FF204020,$FF204028,$FF204030,$FF204038,$FF204040,$FF203840,$FF203040,$FF202840,
   $FF2C2C40,$FF302C40,$FF342C40,$FF3C2C40,$FF402C40,$FF402C3C,$FF402C34,$FF402C30,$FF402C2C,$FF40302C,$FF40342C,$FF403C2C,$FF40402C,$FF3C402C,$FF34402C,$FF30402C,
   $FF2C402C,$FF2C4030,$FF2C4034,$FF2C403C,$FF2C4040,$FF2C3C40,$FF2C3440,$FF2C3040,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000)
  );



{Helper function to draw a rectangle in given color}
procedure FillRect(X,Y,Width,Height,Color:Integer); inline;
var
 CurrentY:Integer;
 PixelOffset:Cardinal;
begin
 {}
 {Calculate the byte offset of the top left}
 PixelOffset:=(CurrentPage * PageSize) + X + (Y * FramebufferProperties.Pitch);

 {Draw each row of pixels}
 for CurrentY:=0 to Height - 1 do
  begin
   FillChar(Pointer(BufferStart + PixelOffset)^,Width,Color);

   {Move to the next row}
   Inc(PixelOffset,FramebufferProperties.Pitch);
  end;
end;


{Helper function to clear the entire screen in one color}
procedure ClearScreen(Color:Integer); inline;
begin
 {}
 FillChar(Pointer(BufferStart + (CurrentPage * PageSize))^,PageSize,Color);
end;


{This is the animation drawing function}
procedure Draw;
var
 Frame:Integer;
 X:Integer;
 Y:Integer;
 Width:Integer;
 Height:Integer;
 DirectionX:Integer;
 DirectionY:Integer;

 StartTime:Int64;
 CompleteTime:Int64;
 TimeDifference:Int64;

 Element:Integer;

 FPS:Integer;
 Seconds:Integer;

 OffsetX:Integer;
 OffsetY:Integer;
begin
 {}
 {Intialize the random number generator}
 Randomize;

 Width:=1;
 Height:=200;

 for Element:=0 to NUM_ELEMENTS - 1 do
  begin
   ElementX[Element]:= Element;
   ElementY[Element]:= Height;
   Height:= Height + Random(10) - Random(9);
   if (Height=0) then Height:=0;
   if (Height> FramebufferProperties.PhysicalHeight) then Height:=FramebufferProperties.PhysicalHeight - 5;
  end;

 {Setup the frames and seconds, actually we are just setting the total number of frames to be
  drawn but since we know if will draw 60 frames per second then we can confidently predict
  how many seconds the animation will last}
 FPS:=60;
 Seconds:=600;

 {Get the starting time in milliseconds ticks from the RTL GetTickCount64 function}
 StartTime:=GetTickCount64;

 {Loop for the number of seconds times the number of frames per second}
 for Frame:=0 to (FPS * Seconds) - 1 do
  begin
   {Change page to draw to (between 0 and 1), while page 0 is on screen we will draw
    page 1 and while page 1 is on screen we will draw page 0. Doing it that way means
    you don't see any sign of the drawing happening, it just looks like the boxes are
    smoothly floating around the screen.

    This variable determines the Y offset to pass to the framebuffer device and also
    the offset from the start of our framebuffer memory}
   CurrentPage:=(CurrentPage + 1) mod 2;

   {Clear the entire page to black so we can draw the boxes.}
   ClearScreen(0);

   {Draw each element (or box) on the current page. For each one there is an X and Y
    value that determines the current position, there is also a DirectionX and a
    DirectionY which determines which direction the box is moving (Up/Down/Left/Right)
    and also how fast it is moving.

    The width and height of each box were set above to a static value, can you see
    how you could make the boxes different sizes?}
   for Element:=0 to NUM_ELEMENTS - 2 do
    begin
     {Earlier we declared some arrays to store the position of each box and above
      we setup the starting positions. On each iteration of the loop we need to
      retrieve those values from the arrays so we can use them to draw the current
      box}
     X:=Element;
     Y:=0;
     Height:=ElementY[Element];

     {Draw the bouncing box, if you looked at the original code you might notice
      that we have changed the value here for the Ultibo version. What would be
      the result if you changed it back to 15 or even 63 instead?}
     FillRect(X,Y,Width,Height,(Element mod 1) + 1);

     {Save the new position of the current box back to the arrays for the next frame}
     ElementY[Element]:=ElementY[Element+1];
    end;
    ElementY[NUM_ELEMENTS-2]:=Min(FramebufferProperties.PhysicalHeight-1,Height + Random(10) - Random(10));

   {After drawing our frame we need to make sure that all of our pixels have
    actually been written back to memory by the CPU.}
   if (FramebufferProperties.Flags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
    begin
     {If the flag is set we call CleanDataCacheRange() and pass the address and
      size of the current page. This is the fastest way to clean just the area that
      matters to us for the animation}
     CleanDataCacheRange(PtrUInt(BufferStart) + (CurrentPage * PageSize),PageSize);
    end;

   {Switch to the new page by setting the Y offset of the framebuffer device, this
    will cause our new page to appear and then we can start all over again on the
    other page}
   OffsetX:=0;
   OffsetY:=CurrentPage * FramebufferProperties.PhysicalHeight;
   FramebufferDeviceSetOffset(FramebufferDevice,OffsetX,OffsetY,True);

   {Because the actual video hardware only refreshes the screen 60 times per second
    (or 60 FPS) then even though we have changed the Y offset above if we start
    drawing on the next page immediately it may still be showing on screen.

    This would produce a tearing effect of horizontal lines on the screen which
    doesn't look good for our smooth animation.

    In order to handle this we check the framebuffer properties to see if the
    device supports waiting for vertical sync (the time between each frame)
    before we proceed. If it doesn't support this option then we do the best we
    can and wait for an approximate amount of time}
   if (FramebufferProperties.Flags and FRAMEBUFFER_FLAG_SYNC) <> 0 then
    begin
     FramebufferDeviceWaitSync(FramebufferDevice);
    end
   else
    begin
     MicrosecondDelay(1000000 div FPS);
    end;
  end;

 {Get the completion time in milliseconds ticks}
 CompleteTime:=GetTickCount64;

 {Work out the number of milliseconds between the start and end and display some information}
 TimeDifference:=CompleteTime - StartTime;
 ConsoleWriteLn('Completed ' + IntToStr(FPS * Seconds) + ' frames of ' + IntToStr(NUM_ELEMENTS) + ' elements in ' + IntToStr(TimeDifference div 1000) + 's ' + IntToStr(TimeDifference mod 1000) + 'ms');
 ConsoleWriteLn('Frame rate ' + IntToStr((FPS * Seconds) div (TimeDifference div 1000)) + ' frames per second');
end;


begin

 ThreadSetCPU(ThreadGetCurrent,CPU_ID_3);
 Sleep(0);

 {Get the framebuffer device, we could use any device but most likely there is only
  one available so we just ask for the default device}
 FramebufferDevice:=FramebufferDeviceGetDefault;
 if FramebufferDevice <> nil then
  begin
   {Request the framebuffer properties which will tell us the size, depth and all
    sorts of other information about the framebuffer device}
   FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);

   {Release the current framebuffer so we can setup a new one with different settings}
   FramebufferDeviceRelease(FramebufferDevice);

   {Wait for second to allow any messages generated by releasing the framebuffer to
    propogate through the system. To do this properly we really should tell the console
    not to attach to the framebuffer during boot, one of the many details needed when
    creating real world applications}
   Sleep(1000);

   {Now we can adjust the framebuffer properties so that the color depth is 8 bits per
    pixel and the virtual height is twice the physical height which will give us two
    pages to draw our animation on. Again if we were doing this in a real application
    it would be best to check the flags first to see if the framebuffer device even
    supports virtual width and height}
   FramebufferProperties.Depth:=8;

   FramebufferProperties.VirtualWidth:=FramebufferProperties.PhysicalWidth;
   FramebufferProperties.VirtualHeight:=FramebufferProperties.PhysicalHeight * 2;

   {Pass the modified properties to the allocate function to allocate a new
    framebuffer with our changes enabled. Checking the return of this function
    would tell you if it was successful or not}
   FramebufferDeviceAllocate(FramebufferDevice,@FramebufferProperties);

   {Wait again just to be safe}
   Sleep(1000);

   {Because we set 8 bit color for the framebuffer to do this example we also
    need to set a palette. The framebuffer device or the driver might provide
    a default palette but the simplest option is to set the one we want.

    Here we pass a prebuilt structure that contains the VGA Mode 13h color
    palette (or at least one version of it)}
   FramebufferDeviceSetPalette(FramebufferDevice,@VGAPalette);

   {We need to get the framebuffer properties again because we want to know the
    address of the framebuffer memory and also the length of each line in bytes
    which is known as the pitch}
   FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);

   {From the properties work out the framebuffer variables}
   BufferStart:=Pointer(FramebufferProperties.Address);
   PageSize:=FramebufferProperties.Pitch * FramebufferProperties.PhysicalHeight;
   CurrentPage:=0;
  end;

 {Create a full screen console window (So we can output some information later)}
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);

 {Go ahead and draw the animation}
 Draw;

 {Halt this thread when the drawing is done}
 ThreadHalt(0);
end.

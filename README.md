# Ultibo-gfx
Simple graphics with Ultibo (bare metal pascal on pi)

(https://ultibo.org/)

![](http://i.imgur.com/58PlkI5t.jpg)

    sidescroller1.pas     
        80s videogame scrolling landscape (inefficent - redrawing 100s lines each frame)

    sidescroller1.pas     
        same as above, using scrolling framebuffer so only drawing 1 line per frame.
    
    fontdemo.pas
        Demo of rendering truetype TTF fonts
        
    bouncingboxes.pas
        demo of double buffering - 200 boxes bouncing off sides of screen

      (start)
      clear
      dim rc{l%,t%,r%,b%}
      sys "GetClientRect", @hwnd%, rc{}
      window_Width% = rc.r%
      window_Height% = rc.b%
      filename$=""
      ::rem windows filedialog i/o protocol
      dim fs{lStructSize%, hwndOwner%, hInstance%, lpstrFilter%, \
      \      lpstrCustomFilter%, nMaxCustFilter%, nFilterIndex%, \
      \      lpstrFile%, nMaxFile%, lpstrFileTitle%, \
      \      nMaxFileTitle%, lpstrInitialDir%, lpstrTitle%, \
      \      flags%, nFileOffset{l&,h&}, nFileExtension{l&,h&}, \
      \      lpstrDefExt%, lCustData%, lpfnHook%, lpTemplateName%}
      dim fp{t&(260)}
      ::
      ::rem displays at start
      rem message
      (menu)
      print "image to screen"
      print "chose image:"
      print "[1] get local image file"
      print "[q] to quit program"
      ::
      ::rem fetches keyboard choice
      keyb$="":while keyb$<>"1" and keyb$<>"q" and keyb$<>"Q":keyb$=inkey$(1) endwhile
      if keyb$="q" or keyb$="Q" then print "type RUN to run program, or close the window":end
      ::
      ::rem image part
      ::rem image place fetch from windows file-dialog
      ff$ = "image jpg,gif,tif,png"+chr$0+"*.jpg;*.png;*.tif;*.gif"+chr$0+chr$0
      fs.lStructSize% = dim(fs{})
      fs.hwndOwner% = @hwnd%
      fs.lpstrFilter% = ptr(ff$)
      fs.lpstrFile% = fp{}
      fs.nMaxFile% = 260
      fs.flags% =6
      sys "GetOpenFileName", fs{} to result%
      if result% filename$ = $$fp{}
      ::
      cls:rem clears screen to white
      if filename$<>"" then
        procdisplay(filename$,1,window_Height%-26,40,25)::rem display image x,y,w,h program-procedure (the procedure is at the end of this script)
      else
        run:rem goto start
      endif
      ::
      dim map(40,25)
      for yy=1 to 25
      for xx=1 to 40
      mx=xx
      my=26-yy
      x=100+xx*5
      y=100+yy*5
      rgb = tint(xx*2,yy*2)
      red = rgb and 255
      green = rgb >> 8 and 255
      blue = rgb >> 16 and 255
      gcol 0
      if red<64*1 and green<64*1 and blue<64*1 then gcol 0:circle fillx*2,y*2,8:map(mx,my)=0
      if red>64*3 and green>64*3 and blue>64*3 then gcol 15:circle fillx*2,y*2,8:map(mx,my)=1
      if red>64*3 and green<64*1 and blue<64*1 then gcol 1:circle fillx*2,y*2,8:map(mx,my)=2
      if red>64*2 and red<64*3 and green>64*3 and blue>64*3 then gcol 6:circle fillx*2,y*2,8:map(mx,my)=3
      if red>64*2 and red<64*3 and green>64*1 and green<64*2 and blue>64*3 then gcol 5:circle fillx*2,y*2,8:map(mx,my)=4
      if red < 64*1 and green>64*2 and green<64*3 and blue>64*1 and blue<64*1 then gcol 2:circle fillx*2,y*2,8:map(mx,my)=5
      if red < 64*1 and green<64*1 and blue >64*1 then gcol 4:circle fillx*2,y*2,8:map(mx,my)=6
      if red>64*2 and green>64*2 and blue<64*2 then gcol 3:circle fill x*2,y*2,8:map(mx,my)=7
      if red>64*3 and green>64*2 and green<64*3 and blue <64*2 then gcol 11:circle fill x*2,y*2,8:map(mx,my)=8
      if red > 64*1 and red<64*2 and green > 64*1 and green <64*2 and blue < 64*1 then gcol 9:circle fill x*2,y*2,8:map(mx,my)=9
      if red > 64*3 and green>64*1 and green<64*2 and blue>64*1 and blue<64*2 then gcol 9:circle fill x*2,y*2,8:map(mx,my)=10
      if red<64*1 and red>32 and green<64*1 and green>32 and blue<64*1 and blue>32 then gcol 8:circle fill x*2,y*2,8:map(mx,my)=11
      if red<64*2 and red>64*1 and green<64*2 and green>64*1 and blue<64*2 and blue>64*1 then gcol 7:circle fill x*2,y*2,8:map(mx,my)=12
      if red > 64*2 and red<64*3 and green>64*3 and blue > 64*1 and blue < 64*2 then gcol 10: circle fill x*2,y*2,8:map(mx,my)=13
      if red < 64*1 and green<64*2 and green<64*3 and blue>64*3 then gcol 12:circle fill x*2,y*2,8:map(mx,my)=14
      if red>64*2 and red<64*3 and green>64*2 and green<64*3 and blue>64*2 and blue<64*3 then gcol 8:circle fill x*2,y*2,8:map(mx,my)=15
      next
      next
      (quest)
      input "save";ans$
      if left$(ans$,1)="n" or left$(ans$,1)="N" then cls:run:rem goto start
      if left$(ans$,1)<>"y" or left$(ans$,1)="Y" then goto quest
      input "filename";filen$
      filen$=filen$+".bin"
      file%=openout("c:"+filen$)
      for n=1 to 1000:bput#file%,160:next:rem stroke character
      for y=1 to 25
      for x=1 to 40
      bput#file%,map(x,y)
      next x
      next y
      print filen$;" is saved":print tab(0,0):goto menu
      ::
      end:rem the end of program
      ::
      rem some protocol
      def procdisplay(pic$, xpos%, ypos%, xsize%, ysize%)
      local tSI{}, pic%, gdip%, image%, ix%, iy%, gfx%, lGDIP%
      dim pic% local 513

      sys "MultiByteToWideChar", 0, 0, pic$, -1, pic%, 256

      sys "LoadLibrary", "GDIPLUS.DLL" to gdip%
      if gdip% = 0 error 100, "Couldn't load GDIPLUS.DLL"
      sys "GetProcAddress", gdip%, "GdiplusStartup" to `GdiplusStartup`
      sys "GetProcAddress", gdip%, "GdipLoadImageFromFile" to `GdipLoadImageFromFile`
      sys "GetProcAddress", gdip%, "GdipDrawImageRectRectI" to `GdipDrawImageRectRectI`
      sys "GetProcAddress", gdip%, "GdipGetImageHeight" to `GdipGetImageHeight`
      sys "GetProcAddress", gdip%, "GdipGetImageWidth" to `GdipGetImageWidth`
      sys "GetProcAddress", gdip%, "GdipCreateFromHDC" to `GdipCreateFromHDC`
      sys "GetProcAddress", gdip%, "GdipSetSmoothingMode" to `GdipSetSmoothingMode`
      sys "GetProcAddress", gdip%, "GdipDeleteGraphics" to `GdipDeleteGraphics`
      sys "GetProcAddress", gdip%, "GdipDisposeImage" to `GdipDisposeImage`
      sys "GetProcAddress", gdip%, "GdiplusShutdown" to `GdiplusShutdown`

      dim tSI{GdiplusVersion%, DebugEventCallback%, \
      \       SuppressBackgroundThread%, SuppressExternalCodecs%}
      tSI.GdiplusVersion% = 1

      sys `GdiplusStartup`, ^lGDIP%, tSI{}, 0
      sys `GdipLoadImageFromFile`, pic%, ^image%
      if image% = 0 then
        sys `GdiplusShutdown`, lGDIP%
        sys "FreeLibrary", gdip%
        error 90, "Couldn't load " + pic$
      endif

      sys `GdipGetImageWidth`, image%, ^ix%
      sys `GdipGetImageHeight`, image%, ^iy%

      sys `GdipCreateFromHDC`, @memhdc%, ^gfx%
      if gfx%=0 error 90, "GdipCreateFromHDC failed"
      sys `GdipSetSmoothingMode`, gfx%, 2

      sys `GdipDrawImageRectRectI`, gfx%, image%, xpos%, ypos%, xsize%, ysize%, \
      \                                           0, 0, ix%, iy%, 2, 0, 0, 0

      sys `GdipDeleteGraphics`, gfx%
      sys `GdipDisposeImage`, image%
      sys `GdiplusShutdown`, lGDIP%
      sys "FreeLibrary", gdip%
      sys "InvalidateRect", @hwnd%, 0, 0
      sys "UpdateWindow", @hwnd%
      endproc
      ::


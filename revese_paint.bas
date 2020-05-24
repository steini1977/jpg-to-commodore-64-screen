      dim rc{l%,t%,r%,b%},map(39,24)
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
      colour 128+0
      colour 15
      cls:rem clear screen to white
      print "image to screen"
      print "chose image:"
      print "[1] get local image file"
      print "[q] to exit program"
      ::
      ::rem fetches keyboard choice
      keyb$="":while keyb$<>"1" and keyb$<>"q" :keyb$=inkey$(1) endwhile
      if keyb$="q" or keyb$="Q" then cls:print "*bye":end
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
        procdisplay(filename$,10,10,400,250):rem show image
      else
        run
      endif
      for y=0 to 24
      for x=0 to 39
      gcol 15:red=0:green=0:blue=0
      rem rectangle 20+x*10*2,window_Height%*2-y*10*2-40,10*2,10*2
      dim c(3)
      for wy=1 to 9
      for wx=1 to 9
      gcol 15
      rgb=tint(20+x*10*2+wx*2,window_Height%*2-y*10*2-20-wx*2)
      red=red+(rgb and 255)
      green=green+(rgb >> 8 and 255)
      blue=blue+(rgb >> 16 and 255)
      next:next
      c(1)=int(red/81)
      c(2)=int(green/81)
      c(3)=int(blue/81)
      cbm=0:bbc=0
      c=false
      if c=false and c(1)<40 and c(2)<40 and c(3)<40 then cbm=0:c=true
      if c=false and c(1)<80 and c(2)<80 and c(3)<40 then cbm=9:c=true
      if c=false and c(1)<80 and c(2)<80 and c(3)<80 then cbm=11:c=true
      if c=false and c(1)<80 and c(2)<80 and c(3)<160 then cbm=6:c=true:bbc=4
      if c=false and c(1)<80 and c(2)<160 and c(3)<80 then cbm=5:c=true:bbc=2
      if c=false and c(1)<80 and c(2)<160 and c(3)<160 then cbm=3:c=true:bbc=1
      if c=false and c(1)<120 and c(2)<80 and c(3)<40 then cbm=8:c=true
      if c=false and c(1)<120 and c(2)<80 and c(3)<80 then cbm=2:c=true:bbc=1
      if c=false and c(1)<160 and c(2)<80 and c(3)<120 then cbm=10:c=true
      if c=false and c(1)<120 and c(2)<120 and c(3)<120 then cbm=12:c=true
      if c=false and c(1)<120 and c(2)<200 and c(3)<120 then cbm=13:c=true
      if c=false and c(1)<120 and c(2)<120 and c(3)<200 then cbm=14:c=true
      if c=false and c(1)<160 and c(2)<160 and c(3)<160 then cbm=15:c=true
      if c=false and c(1)<200 and c(2)<200 and c(3)<80 then cbm=7:c=true:bbc=3
      if c=false and c(1)<200 and c(2)<200 and c(3)<200 then cbm=1:c=true:bbc=15
      gcol cbm
      rectangle fill 1000+11*2+x*11*2,(window_Height%*2)-(y*11*2)-45,11*2,11*2
      map(x,y)=cbm
      next
      next
      print "save? y/n?";
      keyb$="":while keyb$<>"y" and keyb$<>"Y" and keyb$<>"n" and keyb$<>"n" keyb$=inkey$(0) endwhile
      print keyb$
      if keyb$="n" or keyb$="N" then run
      x=0:y=0:l=0
      a$=a$+"string_a"+chr$(13)+chr$(10)
    1 if x=0 then a$=a$+"            byte "
      a$=a$+str$(map(x,y))
      if x<39 then x=x+1:a$=a$+"," else a$=a$+chr$(13)+chr$(10):y=y+1:x=0
      if y=5 and x=0 then  a$=a$+"string_b"+chr$(13)+chr$(10)
      if y=10 and x=0 then  a$=a$+"string_c"+chr$(13)+chr$(10)
      if y=15 and x=0 then  a$=a$+"string_d"+chr$(13)+chr$(10)
      if y=20 and x=0 then  a$=a$+"string_e"+chr$(13)+chr$(10)
      if not (y=25 and x=0) then goto 1
      cls
      print a$
      keyb$="":while keyb$<>"1" and keyb$<>"q" :keyb$=inkey$(1) endwhile
      if keyb$="q" or keyb$="Q" then cls:print "*bye":end
      run
      ::rem
      ::
      end:rem the end of program
      rem some protocol
      :
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

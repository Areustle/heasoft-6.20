      SUBROUTINE rdtoxy(Rain,Decin,Szx,Szy,Racenter,
     &                  Deccenter,Roll,Pixelsize,Xpos,Ypos)
      implicit none
c
c For a given coordinates (rain and decin) returns image pixel
c given the reference ra, dec, roll and pixel size
c
c I  Rain      d   value of ra to convert in xpixel
c I  Decin     d   value of dec to convert in y pixel
c I  Szx       i   number of pixels X dimension 
c I  Szy       i   number of pixels Y dimension 
c I  RAcenter  d   ra of the reference pixel Xcenter
c I  Decencer  d   dec ofthe reference pixel Ycenter
c I  Roll      d   rotaton angle (ximage convension)
c I  pixelsize d   pixel size
c O  Xpos      r   pixel position 
c O  Ypos      r   pixel position 
c
      INTEGER*4  Szx, Szy
      REAL*4 Xpos, Ypos
      REAL*8 Rain, Decin, Racenter, Deccenter, Pixelsize(2), Roll

      INCLUDE '../include/pi.inc'
c
c Local variable
      real*4 ximh, yimh
      REAL*8 Rainr, Decinr, Racenterr, Deccenterr, roll2 , gamma1
      REAL*8 dera, radian, ralde, derar, aiynor, aixnor, aix, aiy 
c
c return if not size 
      IF ( Szx.EQ.0 .or. Szy.EQ.0 ) THEN
         Xpos = 0.0
         Ypos = 0.0
         RETURN
      ENDIF
c
      radian = 180.d0/PI
      roll2 = -Roll + 270
      gamma1 = -roll2/radian
      ximh = FLOAT(Szx)/2. + 0.5
      yimh = FLOAT(Szy)/2. + 0.5
c
      dera = Racenter - Rain
      if (dera.lt.-300) dera= Racenter + 360.0 - Rain 
      derar=dera/radian
      Decinr=Decin/radian
      Deccenterr=Deccenter/radian
      Rainr=Rain/radian
      Racenterr=Racenter/radian
c
c
c Protected for racenter=rain if ralde = 0 return for 
c Xpos and Ypos image center
c  
      ralde=DACOS(DSIN(Decinr)*DSIN(Deccenterr)+DCOS(Decinr)*
     &            DCOS(Deccenterr)*DCOS(derar))
      aixnor=0.0 
      aiynor=0.0 
      IF(ralde .ne.0) THEN 
         aixnor = ralde/DSIN(ralde)*radian/(-Pixelsize(1))*
     &                             DCOS(Decinr)*DSIN(derar)
         aiynor = ralde/DSIN(ralde)*radian/Pixelsize(2)*
     &                            (DSIN(Decinr)*DCOS(Deccenterr)-
     &                             DCOS(Decinr)*DSIN(Deccenterr)*
     &                             DCOS(derar) )
      ENDIF   
c
      aix = aixnor*DCOS(gamma1) - aiynor*DSIN(gamma1) + ximh
      aiy = aiynor*DCOS(gamma1) + aixnor*DSIN(gamma1) + yimh
      xpos = aix
      ypos = aiy
      RETURN
      END

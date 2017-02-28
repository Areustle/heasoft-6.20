*+SPAT_GAIN
c    -------------------------------------------------------- 
       subroutine spat_gain(dx,dy,pha,pi,yprime,aprime1,
     &         aprime2,aprime3,ha_chan,ha,
     &         k,lf,hf,errflg,chatter)
c    --------------------------------------------------------
      IMPLICIT NONE
c --- DESCRIPTION ------------------------------------------------
c Original program modified to a subroutine ...
c Original comments :
C INPUT file has  PHA, PI,A', A'', A''',Y'
C OUTPUT file has  PHA, PI, A', A'',A''', A'''',Y'
c ----------------------------------------------------------------
c	
c Jane Turner   (Sept 1995) 1.0.0;
c Rehana Yusaf  (Sept 20 1995) 1.0.1; minor cosmetics, values no
c                                     longer read from file
c Rehana Yusaf (Oct 1995) 1.0.2; set pi to A4, corrected value
c
      character(5) version
      parameter (version = '1.0.2')
c ----------------------------------------------------------------
*-

        real aprime1,aprime2, yprime, aprime3,aprime4,dx,dy
	real ha(256), lf(800), hf(800),DENOM, HAV
	real ha_chan(256), k(800)
        integer  pha, pi,A4
        integer ATEMP, YTEMP,errflg,chatter,len

        character(30) errstr
        integer clenact

c
c --- USER INFO ---
c
      errstr = ' ERROR spat_gain Ver '//version
      len = clenact(errstr) 
      
C INPUT file has  PHA, PI,A', A'', A''',Y'

		YTEMP=INT(yprime/10.0)
		IF(YTEMP.LT.1) THEN
			YTEMP=1
		ELSEIF(YTEMP.GT.800) THEN
			YTEMP=800
		ENDIF
		ATEMP=MIN(NINT(aprime3),256)
		ATEMP=MAX(1,ATEMP)
		HAV=ha(ATEMP)
		DENOM=lf(YTEMP)+(HAV*hf(YTEMP))
		aprime4 = aprime3/DENOM
		A4=NINT(aprime4)

C OUTPUT file has PHA, PI,A4(int=PI),A', A'',A''', Y' 

          pi = A4 
          return
50        END

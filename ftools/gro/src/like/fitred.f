c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE FITRED(MAP,MAPTYP,MAPDOC,NFILE,FLAG)
C
C
C  $Id: fitred.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C	Effect: 
C++      Reads FITS file header, and skips to specified 2D image.
C++      Called by PROGRAM MAPGET.
C++
C++      If flag is true, fits file will be read into existing CTL
C++      defined map (the FITS file need not fill the CTL defined map); 
C++      if false, control parameters derived from the FITS file.
C++      Read image NFILE, if NFILE is 0, then set NFILE to number 
C++      of images in map and return without reading.
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     REAL   map data array
c       CHARACTER 	MAPTYP*4		type of map
c 	character(70) 	MAPDOC(10)		MAP documentation
c     	integer		nfile			image number to read -
c	if NFILE is 0, then set NFILE to number of images in map
c       and return without reading
c	logical 	flag			flag is true, fits file 
c	will be read into existing CTL defined map
c
c-----------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
c
c-----------------------------------------------------------------------
C  $Log: fitred.f,v $
C  Revision 1.4  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2007/01/31 21:04:11  irby
C  Replace use of FITSERROR routine (fitserro.f) with calls to fcerr/fcerrm.
C  The reliance of FITSERROR on the Fortran 'access' function prevents it
C  from compiling with gfortran (in which 'access' is not [yet?] implemented).
C  Also, fix some lines that were too long (>72 chars).
C
C  Revision 1.2  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/19  18:57:19  jae
c Added an ftio call (CALL FTCLOS(unit,status))
c to each return to assure proper closing of the
c logical unit LU(14).
c
c Revision 5.1  1996/02/29  20:47:48  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:54  jae
c Subroutine Module for like V5.00
c
C
c
c-------------------------------------------------------------------------

      SUBROUTINE FITRED(MAP,MAPTYP,MAPDOC,NFILE,FLAG)

c     Common blocks used:
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/fitrep.copy' 
      INCLUDE '../COMMON/tmprep.copy'

      save
c
c-------------------------------------------------------------------------
      REAL          BSCALE,BZERO
      REAL          FITORG(2),FITEND(2),CDELTA(2),CRVAL(2),CRPIX(2)
      INTEGER       BITPIX,NAXIS,NAXES(10),STATUS
      INTEGER       BLOCK,NFILE,fpixel,nelements
      character(80)  id, context
      character(70)  mapdoc(10)
      character(4)   maptyp
      character(1)   old_coord_sys
      LOGICAL       FLAG,PIXCENT

      common   /id/id


      id = '$Id: fitred.f,v 1.4 2013/05/21 19:08:25 irby Exp $'
      LOC='FITRED  '

      if (jae_fitred) write(*,*)'Inside subroutine FITRED'

      SIGNAL=' '
      SIGMSG='FITRED: ERROR DEVELOPED IN SUBROUTINE CALLED BY FITRED '
      
C     OPEN THE FILE AND REPORT BACK THE RECORD LENGTH

      CALL FTCLOS(LU(14),STATUS)
      status = 0
      len = len_trim(MAPFILE)
      CALL ftopen(LU(14),MAPFILE(1:len),0,BLOCK,STATUS)
      if (status .ne. 0)then
         context='Error opening MAPFILE'
         call fcerr(context)
         go to 998
      end if
      if (jae_fitred) then
         print *,'FITRED: return from FITS file open:'
         print *,'    UNIT = ',LU(14),'   MAPFILE = ',MAPFILE(1:len)
         print *,'    BLOCKSIZE = ',BLOCK,'   STATUS = ',STATUS
         print *,'    NFILE = ',nfile,'   Flag = ',flag
      endif
      
C     TMPLOC='FTROPN  '
C     CALL FITSERROR(-1,STATUS,TMPLOC)

      if (signal.ne.' ') then
      	 CALL FTCLOS(LU(14),STATUS)
	 return
      endif

      if (FLAG) old_coord_sys=coord_sys

      if (NFILE.eq.0) then

c	  read fits header

	  CALL HDRRED(BITPIX,NAXIS,NAXES,BSCALE,BZERO,PIXCENT,
     &    	      CDELTA,CRVAL,CRPIX,NFILE,MAPTYP,FLAG)
	  IF (SIGNAL.ne.' ') then
      	     CALL FTCLOS(LU(14),STATUS)
	     return
	  endif
      endif

      IF (NAXIS.lt.2.or.NAXIS.gt.3) THEN
         SIGNAL='D'
         SIGMSG ='FITRED: NAXIS is inappropriate for this program.'
         RETURN
      ENDIF

      IF (abs(CDELTA(1)-CDELTA(2)).gt.0.001) THEN
         SIGNAL='S'
         SIGMSG ='FITRED: MAP PIXELS ARE NOT SQUARE'
	 IF (SIGNAL.ne.' ') then
      	    CALL FTCLOS(LU(14),STATUS)
	    return
         endif
      ENDIF

      IF (abs(CDELTA(1)-CTLSCL).gt.0.001) THEN
c        the FITS scale is not as expected

         IF (flag) THEN
c           This is serious - hang it up!

            SIGNAL='F'
            SIGMSG ='FITRED: FITS pixel size differs from CTL'
            return
         else
c           FITS map defines CTL parameters
            CTLSCL=abs(CDELTA(1))
         END IF
      ENDIF

      if (PIXCENT) then
c	 CRVAL describes the center of the pixel
	 FITORG(1)=CRVAL(1)-(CRPIX(1)-1.)*CDELTA(1)
         FITORG(2)=CRVAL(2)-(CRPIX(2)-1.)*CDELTA(2)
      else
c	 CRVAL describes lower, small long. corner
	 FITORG(1)=CRVAL(1)-(CRPIX(1)-1.5)*CDELTA(1)
	 FITORG(2)=CRVAL(2)-(CRPIX(2)-1.5)*CDELTA(2)
      endif

      FITEND(1)=FITORG(1)+(NAXES(1)-1)*CDELTA(1)
      FITEND(2)=FITORG(2)+(NAXES(2)-1)*CDELTA(2)

      IF (flag) THEN
	 if (old_coord_sys.ne.coord_sys) then
            SIGNAL='C'
            SIGMSG ='FITRED: Inconsistent coordinate systems'
	    IF (SIGNAL.ne.' ') then
      	       CALL FTCLOS(LU(14),STATUS)
	       return
	    endif
	 endif

C	 DO FITS FILE EDGES DIFFER FROM CTLREP MAP?

	 IF (abs(FITORG(1)-CTLORG(1)).gt.0.001.OR.
     &       abs(FITORG(2)-CTLORG(2)).gt.0.001.OR.
     &       abs(FITEND(1)-CTLEND(1)).gt.0.001.OR.
     &       abs(FITEND(2)-CTLEND(2)).gt.0.001) THEN
            SIGNAL='Q'
            SIGMSG ='FITRED: FITS FILE EDGES DIFFER FROM CTLREP MAP'

	    if (MAPTYP.eq.'GMAP') then
	       if (NFILE.EQ.0.or..not.verbose) then
	          signal=' '
	       else
                  write(6,'("Counts MAP EDGES:",f6.1," to ",f6.1,
     &                  " and ",f6.1," to ",f6.1)')
     &                  CTLORG(1),CTLEND(1),CTLORG(2),CTLEND(2)
	          write(6,'("Diffuse MAP EDGES:",f6.1," to ",f6.1,
     &                  " and ",f6.1," to ",f6.1)')
     &                  FITORG(1),FITEND(1),FITORG(2),FITEND(2)
	          CALL ERROR(0,LOC)
	       endif
	    else
	       IF (SIGNAL.ne.' ') then
      		  CALL FTCLOS(LU(14),STATUS)
		  return
  	       endif
	    endif
	 ENDIF

C	 ARE PIXEL CENTERS ALIGNED? 

         DIFF1=(FITORG(1)-CTLORG(1))/CTLSCL
         IDIFF1=DIFF1
         DIFF2=(FITORG(2)-CTLORG(2))/CTLSCL
         IDIFF2=DIFF2

         IF (abs(FLOAT(IDIFF1)-DIFF1).gt.1.e-3
     &  	 .OR.abs(FLOAT(IDIFF1)-DIFF1).gt.1.e-3) THEN
            SIGNAL='A'
            SIGMSG ='FITRED: PIXEL CENTERS ARE NOT ALIGNED'
	    if (MAPTYP.eq.'GMAP') then
	       if (NFILE.EQ.0) then
	          signal=' '
	       else
	          call error(-1,loc)
	       endif
	    else
	       IF (SIGNAL.ne.' ') then
      	          CALL FTCLOS(LU(14),STATUS)
	          return
	       endif
	    endif
         ENDIF
      else
c        FITS map defines CTL parameters

         CTLMSZ1=NAXES(1)
         CTLMSZ2=NAXES(2)
         CTLORG(1)=FITORG(1)
         CTLORG(2)=FITORG(2)
         CTLEND(1)=FITEND(1)
         CTLEND(2)=FITEND(2)
	 if (CTLEND(1).lt.CTLORG(1)) then
            SIGNAL='N'
            SIGMSG ='FITRED: CTLEND(1).lt.CTLORG(1)'
            IF (CDELTA(1).gt.0.) THEN
	       IF (SIGNAL.ne.' ') then
      		  CALL FTCLOS(LU(14),STATUS)
		  return
	       endif
	    else
	       CALL ERROR(-1,LOC)
               CTLORG(1)=FITEND(1)
               CTLEND(1)=FITORG(1)
            endif
         endif
      ENDIF

      IF (NFILE.EQ.0) THEN
         NFILE=NAXES(3) !  number of images
      else
         nelements=NAXES(1)*NAXES(2) !map size
         fpixel=(nfile-1) * nelements + 1 ! first pixel
	 call MAPRST(MAP,CTLMSZ1,CTLMSZ2)

C        READ THE ARRAY OF REAL NUMBERS

         CALL DATARED(MAP,TMPMAP,fpixel,nelements,NAXES(1),NAXES(2),
     &                CTLMSZ1,CTLMSZ2,CDELTA,FITORG)
         IF (SIGNAL.ne.' ') then
      	    CALL FTCLOS(LU(14),STATUS)
	    return
	 endif
c
	 write(MAPDOC(1),'("FROM FITS file",a)') MAPFILE(1:55)
	 do i=2,10
	    MAPDOC(i)=' '
	 enddo
      ENDIF

      CALL FTCLOS(LU(14),STATUS)
      if (status .ne. 0)then
         context='Error closing MAPFILE'
         call fcerr(context)
         go to 998
      end if
C     TMPLOC='FTCLOS  '
C     CALL FITSERROR(0,STATUS,TMPLOC)
      SIGNAL = ' '                                                      

      GOTO 999
C     Print out the FITSIO error number and text string
 998  CALL FCERRM(STATUS)

 999  CONTINUE
      RETURN                                                            
      END     

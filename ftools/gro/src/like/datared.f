c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE DATARED(MAP,FITSMAP,fpixel,nelements,NX,NY,
C     &  IMAPWIDTH,IMAPHEIGHT,CDELTA,FITORG)
C
C
C  $Id: datared.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C	Effect: 
c       Called by SUBROUTINE FITRED.
C       Reads FITS file data.
C       Uses William Pence's (HEASARC, GSFC) FITSIO
C       Library of Fortran Subroutines to Read and Write FITS Files.
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c	real	MAP data being read
c	real	FITSMAP scratch memory
c       INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
c	integer	fpixel		first pixel to read for selected image		
c	integer	nelements	map size of selected image
c	integer	NX,NY           width, height in pixels
c	real 	CDELTA(2)	pixel width
c	REAL	FITORG(2)	map origin
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C             UPDATED:    by  JRM
C=======================================================================
C  $Log: datared.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2007/01/31 21:04:11  irby
C  Replace use of FITSERROR routine (fitserro.f) with calls to fcerr/fcerrm.
C  The reliance of FITSERROR on the Fortran 'access' function prevents it
C  from compiling with gfortran (in which 'access' is not [yet?] implemented).
C  Also, fix some lines that were too long (>72 chars).
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:21  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:35  jae
c Subroutine Module for like V5.00
c
C
c                                        
c--------------------------------------------------------------------------

      SUBROUTINE DATARED(MAP,FITSMAP,fpixel,nelements,NX,NY,
     &     IMAPWIDTH,IMAPHEIGHT,CDELTA,FITORG)

c     Common blocks used:
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'

      save

      character(80) id, context
      common /id/id
      INTEGER STATUS,fpixel,nelements
      REAL FITORG(2),CDELTA(2),X_PIXEL,Y_PIXEL
      REAL FITSMAP(NX,NY)
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      logical anyf


      id = '$Id: datared.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
       LOC='DATARED '

C     READ THE ARRAY OF REAL NUMBERS
       CALL ftgpve(LU(14),0,fpixel,nelements,0,FITSMAP,anyf,STATUS)
       if (status .ne. 0)then
          context='Error reading data array'
          call fcerr(context)
          go to 11
       end if
C      IF(STATUS.NE.0) THEN
C         SIGMSG='ERROR DEVELOPED IN SUBROUTINE CALLED BY DATARED   '
C         CALL FITSERROR(1,STATUS,'ftgpve')
C      ENDIF

       IF(anyf)THEN
          SIGNAL='U'
          SIGMSG ='DATARED: Undefined values.'
          RETURN
       ENDIF

C     TRANSFER MAP:                                    
       DO 10 I=1,CTLMSZ1
          CALL MAPCOR(I,1,X_PIXEL,Y_PIXEL)
          CALL ERROR(1,LOC)
          fit_I=(X_PIXEL-FITORG(1))/CDELTA(1)+1.5
          I_fit=fit_I

          if(I_fit.ge.1.and.I_fit.le.NX)then

c     This map column is on the fits map.
          else

c     try L+360.
             fit_I=(360.+X_PIXEL-FITORG(1))/CDELTA(1)+1.5
             I_fit=fit_I

             if(I_fit.ge.1.and.I_fit.le.NX)then

c     This map column is on the fits map.
             else

c     try L-360.
                fit_I=(-360.+X_PIXEL-FITORG(1))/CDELTA(1)+1.5
                I_fit=fit_I

                if(I_fit.ge.1.and.I_fit.le.NX)then

c     This map column is on the fits map.
                else
                   signal='!' 
                   write(sigmsg,*)
     &                 'This fits file does not extend to long.',X_PIXEL
                   return
                endif
             endif
          endif

          DO J=1,CTLMSZ2
             CALL MAPCOR(I,J,X_PIXEL,Y_PIXEL)
             CALL ERROR(1,LOC)
             fit_J=(Y_PIXEL-FITORG(2))/CDELTA(2)+1.5
             J_fit=fit_J
	     if(J_fit.ge.1.and.J_fit.le.NY) then
c     This map row is also on the fits map.
                MAP(I,J)=FITSMAP(I_fit,J_fit)
             else
                signal='!' 
                write(sigmsg,*)
     &               'This fits file does not extend to lat.',Y_PIXEL
                return
	     endif
          enddo

 10    CONTINUE

       GOTO 12
C      Print out the FITSIO error number and text string
 11    CALL FCERRM(STATUS)

 12    CONTINUE

       RETURN
       END
c

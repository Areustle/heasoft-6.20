C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPCNV(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapcnv.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     EFFECT: MAP2 IS MAP1 CONVOLVED WITH THE PSF over a square
c	of width 2*Ranal
c
C=======================================================================
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^		
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C             UPDATED:     by  JRM
c
C=======================================================================
C  $Log: mapcnv.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:28:04  irby
C  Change negative exponents to e.g. -1*number instead of -number for f90
C  compatibility (and put in parens (-1) where necessary).
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:46  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:55  jae
c Subroutine Module for like V5.00
c
C
c
c
c--------------------------------------------------------------------------

      SUBROUTINE MAPCNV(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
c
c--------------------------------------------------------------------------
C
      REAL MAP1(IMAPWIDTH,IMAPHEIGHT),MAP2(IMAPWIDTH,IMAPHEIGHT)
C     
      id = '$Id: mapcnv.f,v 1.3 2013/05/21 19:08:26 irby Exp $'

C     calculate the number of bins appropriate for the convolution.
      CTLNBC = INT(Ranal/CTLSCL)

      write(lu(1),*)'Map total before convolution:'
      sum=totmap(map1,CTLMSZ1,CTLMSZ2)
      write(lu(1),*)sum

      if (sum.lt.1.) then
         write(lu(1),*)'Map is empty!'
         stop 93
      endif
      
      CLAT=1.e6
      xlshift=0.
      bshift=0.
      old_lat=0.

      DO  Ilat = 1,ctlmsz2      ! do entire map
         CALL MAPCOR (1,Ilat,srcL,srcB)
         CALL ERROR(1,LOC)
         the_lat=cos(srcB*PI180)**(-1)

         if (abs(the_lat-old_lat).gt.0.01) then 
c     need to adjust PSF to CLAT
	    CLAT=srcB
	    write(lu(1),'("MAPCNV: Rebinning PSF for latitude=",
     &           f6.1)')CLAT
	    CALL psmmat(xlshift,bshift)
            CALL ERROR(1,LOC)
	    old_lat=the_lat
         endif

         DO  Ilon = 1,ctlmsz1
            MAP2(Ilon,Ilat) = 0. ! convolved map value
            psftotal=0.         !  psf normalization

            DO  100 JL = PSFORG(1)-ctlnbc,PSFORG(1)+ctlnbc
               KL=Ilon+JL-PSFORG(1) ! map longitude pixel index
               IF (kl.LT.1) THEN
                  IF (fullmap) THEN  
C     WRAP AROUND MAP DISCONTINUITY IN LONGITUDE
                     kl=kl+CTLMSZ1
                  ELSE
C     SKIP THIS PIXEL IT IS OFF THE MAP IN LONGITUDE
                     GOTO 100
                  ENDIF
               ENDIF

               IF (kl.GT.CTLMSZ1) THEN
                  IF (fullmap) THEN  
C     WRAP AROUND MAP DISCONTINUITY IN LONGITUDE
                     kl=kl-CTLMSZ1
                  ELSE
C     SKIP THIS PIXEL IT IS OFF THE MAP IN LONGITUDE
                     GOTO 100
                  ENDIF
               ENDIF

               do  JB = PSFORG(2)-ctlnbc,PSFORG(2)+ctlnbc
	          KB=Ilat+JB-PSFORG(2) ! map latitude pixel index

	          if (kb.ge.1.and.kb.le.ctlmsz2) then ! row is on the map
                     psftotal = psftotal + PSF(JL,JB)
                     MAP2(Ilon,Ilat) = MAP2(Ilon,Ilat) + 
     &                    MAP1(KL,KB) * PSF(JL,JB)
	          endif

               enddo
 100        continue

c     write(lu(1),'("psftotal,Ilon,Ilat",f8.2,2i6)') 
c     &                   psftotal,Ilon,Ilat
            MAP2(Ilon,Ilat) = MAP2(Ilon,Ilat) / psftotal
         enddo
      enddo

      write(lu(1),*)'Map total after convolution:'
      sum=totmap(map2,CTLMSZ1,CTLMSZ2)
      write(lu(1),*)sum
      SIGNAL = ' '

      RETURN
      END
c

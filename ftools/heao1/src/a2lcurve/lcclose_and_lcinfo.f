C----------------------------------------------------------------------------
C This subroutine closes the output lightcurve file and issues
C the average background count rate for the lightcurve to the user \
C 
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Nov 6 1998 

      subroutine lcclose_and_lcinfo(userbackflag,avb,avb2,bkgd,fctim,
     &                        nbf,nf,infmx,lcunit,row,hdet,
     &                        hds,ratefile,ichat,errstat)




      implicit none
      INTEGER nbf,infmx,lcunit,row
      INTEGER nf,ichat,errstat
      REAL bkgd , avb, avb2, fctim
      INTEGER*2 hdet, hds(4)
      CHARACTER*(*) ratefile
      LOGICAL userbackflag


      

C Common block declarations

c      common /TASK/ taskname
      
c      character(40) taskname

C Local variables
      
      character(160) message
      real rmsd, diff
c      integer i
      logical lccreate

      
C Initialise variables

      errstat=0
c      i=1
      lccreate=.FALSE.
  

C close the output light curve file
         IF ( row.NE.0 ) THEN
c            CALL CLOSELC(lcunit,row,hdet,i,hds,errstat)
            CALL CLOSELC(lcunit,row,hdet,hds,errstat)
            IF (errstat .eq. 0) then
               message= ' '
               CALL XWRITE(message,ichat)
               WRITE (message,
     &'(''## SUCCESSFULLY CREATED LIGHT CURVE : '', a50)') ratefile
     
               CALL XWRITE(message,ichat)
               lccreate=.TRUE.
            ENDIF
         ELSE
            message= ' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'(''## Light curve  contains no data ..... deleting it'')') 
            CALL XWRITE(message,ichat)
c            CALL FTCLOS(lcunit,errstat)
            CALL FTDELT(lcunit,errstat)
            CALL FTFIOU(lcunit,errstat)
         ENDIF
c      write(6,*) "LCCLOSE_AND_LCINFO: Lightcurve closed computing
c     &        avg bkgd"
         IF ( errstat.NE.0 ) THEN
            WRITE (message,'('' Error closing light curve '', a50)')
     &             ratefile
            CALL XAERROR(message,1)
            CALL FTGMSG(message)
            CALL XAERROR(message,1)
            WRITE (message,'('' FITSIO errstat = '', I3)') errstat
            CALL XAERROR(message,1)
            return
         ELSE

            IF ( avb2.NE.0 .AND. lccreate ) THEN
C Issue info about the overall bkgd level for the lightcurve
               

                  rmsd = SQRT(avb2-avb**2)
                  diff = avb - bkgd/fctim
                  message = ' '
                  CALL XWRITE(message,ichat)
              WRITE (message,
     &'('' Average bkgd for light curve '',F8.2,'' cnts/s'')') 
     & avb
                  CALL XWRITE(message,ichat)
              WRITE (message,
     &'('' with Root Mean Square Deviation '',F8.2)') rmsd 
                  CALL XWRITE(message,ichat)
                     WRITE (message,
     &'('' for '',I4,'' raw data files for which bkgd was measured'')')
     & nbf
                  CALL XWRITE(message,ichat)
                  IF (userbackflag) THEN
                     message = ' '
                    CALL XWRITE(message,ichat)
                     WRITE (message,
     &'('' Diff btween avrge & MODEL bkgd for lc '',F8.2,'' cnts/s'')')
     & diff 
                     CALL XWRITE(message,ichat)
c                     WRITE (message,
c     &'('' for '',I4,'' raw data files for which bkgd was measured'')')
c     & nbf
c                   CALL XWRITE(message,ichat)
                  ENDIF
                  message = ' '
                  CALL XWRITE(message,ichat)
            ENDIF
         ENDIF
         return




         end



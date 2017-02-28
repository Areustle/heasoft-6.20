C----------------------------------------------------------------------------
C This subroutine sets the FOV flags\
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 20 1998 

      subroutine set_fovflags(rootname,ratefile,hdet,hds,emin,emax,
     &                       detector)

      implicit none
      character*(*) rootname,detector,ratefile
      integer*2 hdet,hds(4)
      real emin, emax
   
       

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables
      integer*2 htmp 
      integer j,row,l,k,i
      logical lfovflag,sfovflag
      character(160) message
    
      
C Initialise variables

  


         j = INDEX(rootname,' ')
          i=1
         row = 0
         Write (message,'(i1)') i
         ratefile = rootname(1:j-1)//'_rate'//message(1:1)//'.lc'
         IF ( hdet.EQ.1 ) THEN
            emin = 1.5
            emax = 20.0
            detector = 'MED '
         ELSE
            emin = 2.0
            emax = 60.0
            WRITE (detector,'(''HED-'', i1, '' '')') hdet - 1
         ENDIF
         k = INDEX(detector,' ')
         lfovflag = .FALSE.
         sfovflag = .FALSE.
         DO 450 l = 1 , 4
            htmp = ABS(hds(l))
            IF ( htmp.NE.0 ) THEN
 
C For all detectors, the LEFT field of view is 3.0 x 3.0 degrees.  For
C the MED and HED-3, this is the large field of view.  For HED-1 and HED-2,
C this is the small field of view.
 
               IF ( (htmp.EQ.1) .OR. (htmp.EQ.3) .OR. (htmp.EQ.5) .OR. 
     &              (htmp.EQ.7) ) THEN
                  IF ( (hdet.EQ.1) .OR. (hdet.EQ.4) ) THEN
                     lfovflag = .TRUE.
                  ELSE
                     sfovflag = .TRUE.
                  ENDIF
               ENDIF
 
C For MED and HED-3, the RIGHT field of view is 1.5 x 3.0 degrees, which is
C their small field of view.  For HED-1 and HED-2, the RIGHT field of view
C is the 6.0 x 3.0 degree field of view, which is their large field of view.
 
               IF ( (htmp.EQ.2) .OR. (htmp.EQ.4) .OR. (htmp.EQ.6) .OR. 
     &              (htmp.EQ.8) ) THEN
                  IF ( (hdet.EQ.1) .OR. (hdet.EQ.4) ) THEN
                     sfovflag = .TRUE.
                  ELSE
                     lfovflag = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
 450     CONTINUE
         IF ( (sfovflag) .AND. (.NOT.lfovflag) ) THEN
            detector = detector(1:k-1)//'s'
         ELSEIF ( (lfovflag) .AND. (.NOT.sfovflag) ) THEN
            detector = detector(1:k-1)//'l'
         ENDIF



        return
        end



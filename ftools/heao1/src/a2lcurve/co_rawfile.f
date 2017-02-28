C----------------------------------------------------------------------------
C This subroutine first closes a raw data file and then either re-opens it
C or opens the next raw data file in the list \
C 
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Nov 6 1998 

      subroutine co_rawfile(rawunit,nf,infmx,nr,ARRSZ,rawfiles,
     &                 rawfile,closefile,ichat,errstat)




      implicit none
      integer ARRSZ
      character*(*) rawfile,rawfiles(ARRSZ)
      LOGICAL closefile 
      INTEGER rawunit,nf,infmx,nr,ichat,errstat
 

      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables
      
      character(160) message
      INTEGER hdutype, blocksize


      
C Initialise variables
      errstat=0
      
  
C Close the data file if no further data is available
 
          CALL FTCLOS(rawunit,errstat)
         CALL FTFIOU(rawunit,errstat)
         IF ( nf.GT.infmx ) then
            closefile=.TRUE.
            return
         ENDIF
         IF ( errstat.NE.0 ) THEN
            message = ' Error closing raw data file'
            CALL XAERROR(message,1)
            CALL FTGMSG(message)
            CALL XAERROR(message,1)
            WRITE (message,'('' FITSIO errstat = '', I3)') errstat
            CALL XAERROR(message,1)
            return
         ENDIF

C If scanned background :
C If nt=2 open the NEXT raw data file in the list (since in the code just 
C prior to this subroutine call, nf has been incremented by 1).
C If nt=1 re-open the same raw data file (to then determine the srce counts). 

C If model background :
C nt fixed at 2 therefore open the NEXT raw data file in the list.

         blocksize = 2880
         errstat = 0
         nr = 1
 
         rawfile = rawfiles(nf)
 
 
         CALL FTGIOU(rawunit,errstat)
         CALL FTOPEN(rawunit,rawfile,0,blocksize,errstat)
         IF ( errstat.NE.0 ) THEN
            WRITE (message,'('' Error opening '', A40)') rawfile
            CALL XAERROR(message,1)
            CALL FTGMSG(message)
            CALL XAERROR(message,1)
            WRITE (message,'('' FITSIO errstat = '', I3)') errstat
            CALL XAERROR(message,ichat)
            return
         ENDIF

         CALL FTMRHD(rawunit,1,hdutype,errstat)
         IF ( errstat.NE.0 ) THEN
            WRITE (message,
     &       '('' Failure to open binary extension in  '',         A40)'
     &       ) rawfile
            CALL XAERROR(message,1)
            CALL FTGMSG(message)
            CALL XAERROR(message,1)
            WRITE (message,'('' FITSIO errstat = '', I3)') errstat
            CALL XAERROR(message,1)
            return
         ENDIF
 
         return 
         end



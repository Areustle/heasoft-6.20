C----------------------------------------------------------------------------
C This subroutine processes information from the 1st read of
C a raw data file\
C 
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Nov 6 1998 

      subroutine process_read_info(userbackflag,rawfile,goback,nt,qf,
     &                        nb,nbf,bkgd,db,closefile,avbak,
     &                        chi,qnb,hdet,qfird,avb,avb2,qcont,
     &                        fctim,nfd,nf,ichat)




      implicit none
      LOGICAL userbackflag,qnb,qf,qcont,qfird,goback
      LOGICAL closefile
      INTEGER nt , nb, nbf
      INTEGER nf , nfd, ichat
      REAL db, chi
      REAL bkgd , avb, avb2, avbak,fctim
      INTEGER*2 hdet
      CHARACTER *(*) rawfile


      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables
      
      character(160) message

      
C Initialise variables

      
  
 
      IF (userbackflag .AND. goback) THEN
        goto 1000
      ENDIF
 

C Always set nt=2 if using a model background
      IF ( userbackflag ) nt = 2

      IF (userbackflag .AND. qf .AND. (nb .EQ. 0)) THEN
         message = ' '
         CALL XWRITE(message,ichat)
         message = ' WARNING : File BAD. Data failed quality checks. '
         CALL XWRITE(message,ichat)
         message = ' (For reasons please see a2lcurve.hlp file.)'
         CALL XWRITE(message,ichat)
         message = ' Unable to determine source counts since file BAD'
         CALL XWRITE(message,ichat)

      ENDIF

      IF (.NOT. userbackflag .AND. nb.NE.0 .AND. nt.EQ.0 ) THEN
C Scanned background has been found during the 1st read of raw data file.
 
            nbf = nbf+ 1
            avbak = (bkgd+DB)/fctim
            message = ' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' Overall avrage bkgd for raw data file'',F10.3,'' cnts/s'')') 
     & avbak 
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' This is file number '',I3,'' for which bkgd was measured'')')
     & nbf
            CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)

            avb = (avb*(nbf-1)+avbak)/(nbf)
            avb2 = (avb2*(nbf-1)+avbak**2)/(nbf)
      ENDIF

C Initialise  
      IF ( nt.LT.2 ) THEN
         bkgd=0.0
         nb=0
      ENDIF
 
      IF ( .NOT.(userbackflag .OR. nt.LT.2) ) THEN
         qnb = .FALSE.
         IF ( hdet.NE.0 .AND. nb.EQ.0 ) GOTO 800
      ENDIF
 
 
      GOTO 900
 800  qnb = .TRUE.
 900  IF ( .NOT.userbackflag .OR. qfird ) GOTO 1200
  
C For model background analysis only : 
 1000 IF ( nb.NE.0 ) THEN
C Background has been found for the nbf'th raw data file. 
 
            nbf = nbf+ 1
            avbak = (bkgd+DB)/fctim
            message = ' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' Overall avrage bkgd for raw data file '',F10.3,'' cnts/s'')') 
     & avbak 
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' This is file number '',I3,'' for which bkgd was measured'')')
     & nbf
            CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)
 
            avb = (avb*(nbf-1)+avbak)/(nbf)
            avb2 = (avb2*(nbf-1)+avbak**2)/(nbf)
      ENDIF
            
          
 1100 CONTINUE
 
      IF ( .NOT.qcont ) THEN
         closefile=.TRUE.
         return
      ENDIF
 
 1200 IF ( nt.EQ.2 ) THEN
         chi=0.0
         nb=0
      ENDIF
      DB=0.0
      
 
 
 
C For scanned background analysis only : 
      IF ( .NOT.userbackflag .AND. nt.EQ.2 .AND. qnb ) THEN
C During the 1st loop through the raw data file, no background points have
C been found. Inform user
         IF ( nfd.EQ.0 ) THEN
            message = ' '
            CALL XWRITE(message,ichat)
           WRITE (message,
     &'(''# Starting bkgd calcs for RAW DATA FILE '',I5)') nf
           CALL XWRITE(message,ichat)
           WRITE (message,
     &'('' >>>>>> '',a100)') rawfile
           CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)



         ENDIF
         message = 'WARNING : File BAD. Data failed quality checks. '
         CALL XWRITE(message,ichat)
         message = ' (For reasons please see a2lcurve.hlp file.)'
         CALL XWRITE(message,ichat)
 
 
 
      ENDIF
 
C For scanned background analysis only :  
      IF ( .NOT.userbackflag .AND. nt.EQ.0 .AND. nfd.GT.1 ) THEN
         message = ' '
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'(''# Starting bkgd calcs for RAW DATA FILE '',I5)') nf
         CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' >>>>> '',a100)') rawfile
           CALL XWRITE(message,ichat)
            message = ' '
            CALL XWRITE(message,ichat)


         WRITE (message,
     &'('' Performing further data quality checks ...'')')
         CALL XWRITE(message,ichat)
 
 
      ENDIF



         end



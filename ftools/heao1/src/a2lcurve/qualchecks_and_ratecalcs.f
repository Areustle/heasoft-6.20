C----------------------------------------------------------------------------
C This subroutine performs data quality checks and the background and
C source rate calculations \
C Writes bsu'd source count rate + error to output FITS lightcurve \
C 
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Nov 6 1998 

      subroutine qualchecks_and_ratecalcs(userbackflag,clnflag,
     &     allflag,elcomflag,spinflag,sor,ecp,qdir,qbad,hpt,
     &     qfird,nf,nfd,lcunit,nt,hdet,hds,bkgd,nb,ber,db,chi,
     &                      IDAy,IMSec,HFLag2,ANGlim,
     &                      SPRa,SPDec, rawfile,
     &                      YRA,YDEc,HDSc,sra,sdec,row,
     &                      qr,counter,nrm,avbak,fctim,ichat,errstat)




      implicit none
      LOGICAL userbackflag,clnflag(4),elcomflag(4)
      LOGICAL qdir,qbad,qfird,qr
      REAL sor(3), ecp(3), ber, db, chi, avbak,fctim,bkgd
      REAL anglim(2), SPRa(32),SPDec(32),YRA(32),YDEc(32)
      REAL sra, sdec

      INTEGER nf,nfd,lcunit,nt, nb, counter, nrm, row
      INTEGER ichat, errstat,IDAy,IMSec

      INTEGER*2 spinflag, hdet, hds(4),hpt,HFLag2(4)
      INTEGER*2 HDSc(5,8,8,6), allflag
      CHARACTER *(*) rawfile
 

 

      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables
      
      character(160) message
      logical QOK(4), qan, qel, QCOm(4), QANg
      integer j, jf, i


      
C Initialise variables
      errstat=0

C Reject data with non-zero total overall quality flag values

         IF ( allflag.LE.0 ) THEN
 
            DO 1620 i = 1 , 4
               QOK(i) = clnflag(i)
c               IF ( .NOT.elcomflag(i) ) THEN
c                   QCOm(i) = .TRUE.
c                ELSE
c                   QCOm(i) = .FALSE.
c                ENDIF
               QCOm(i) = elcomflag(i)
 1620       CONTINUE
            DO 1640 j = 1 , 4
               IF ( .NOT.QCOm(j) ) QOK(j) = .FALSE.
 1640       CONTINUE
            qel = .FALSE.
            qan = .FALSE.
            DO 1660 jf = 1 , 4
               IF ( QOK(jf) ) qan = .TRUE.
               IF ( QCOm(jf) ) qel = .TRUE.
 1660       CONTINUE

C Reject ' non-clean ' data or data which has electron contamination
            IF ( .NOT.(.NOT.qan .OR. .NOT.qel) ) THEN
                hpt = IAND(INT(spinflag),1)
 
C Calculate if spin-axis stable and if source in A2 pointing direction               
               CALL XDIRCK(SPRa(16),SPDec(16),YRA(16),YDEc(16),sor,ecp,
     &                     qdir,qbad,hpt)
C Warn if spin axis unstable
               IF ( qbad ) THEN
                  message = ' '
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' Spin axis far from ecliptic at '',I4,I9)')
     & IDAy , IMSec
                  CALL XWRITE(message,ichat)
               ENDIF

C Reject data if source not in A2 pointing direction

               IF ( qdir ) THEN
                    IF ( qfird ) THEN
                      nfd = 1
                      qfird = .FALSE.
                      IF (.NOT. userbackflag) THEN
                         message = ' '
                         CALL XWRITE(message,ichat)
                         WRITE (message,
     &'(''# Starting bkgd calcs for RAW DATA FILE '',I5)')
     & nf
                         CALL XWRITE(message,ichat)
                         WRITE (message,
     &'('' >>>>> '',a100)') rawfile
                         CALL XWRITE(message,ichat)
                         message = ' '
                         CALL XWRITE(message,ichat)

                         WRITE (message,
     &'('' Performing further data quality checks ...'')')
                           CALL XWRITE(message,ichat)
                           message = ' '
                           CALL XWRITE(message,ichat)
c                        WRITE (message,99023) nfd
c                        CALL XWRITE(message,ichat)
                      ENDIF
                    ENDIF
 
 
CCCC    THIS SECTION BRANCHES TO SUBROUTINE DSC5 WHICH 
CCC     FOR A GIVEN RAW DATA FILE, COMPUTES THE BKGD RATE (if nt=1)
CCC     AND THE SOURCE RATE (if nt=2).
 
C Subroutine DSC5 :
C Check that QOK=.T. ('clean data' flag) for the detector concerned.
C Determine whether satellite scan angle is
C within user input range (if yes, QANG=.T.).
C Reject data if QOK=.F., QANG=.F. 
C
C If computing background (nt=1), reject data if the efficiency
C is non-zero. If eff=0 calculate the background from the hdsc
C values for this row in the raw data file. 
C
C If computing source counts (nt=2), reject data if efficiency is
C 2.5exp[-6] < eff < 0.15. For eff>=0.15 calculate source counts. For
C eff < 2.5exp[-6] calculate the deviation between the current
C background counts (for this row) and the overall background level (either
C model or mean scanned background for this file).
C
C Write the calculated bsu'd source rate for a given row of the raw data file
C to the output FITS light curve.  
 
                  
                   CALL DSC5(lcunit,nt,hdet,hds,bkgd,nb,ber,db,chi,
     &                      IDAy,IMSec,HFLag2,ANGlim,
     &                      QOK,QANg,SPRa,SPDec,
     &                      YRA,YDEc,HDSc,sra,sdec,row,
     &                      errstat)
                   IF ( errstat.NE.0 ) THEN
                     message = ' Error writing light curve(s) '
                     CALL XWRITE(message,ichat)
                     CALL FTGMSG(message)
                     CALL XWRITE(message,ichat)
                     WRITE (message,'('' FITSIO errstat = '', I3)')
     &                      errstat
                     CALL XWRITE(message,ichat)
                     return
 
                   ENDIF

C Give information to user (for scanned background only). 
                   qr = .FALSE.
                       IF ( nb.NE.0 ) THEN
                          IF ( .NOT.(qr) ) THEN
                             qr = .TRUE.
                             counter = counter + 1
                             nrm = MOD(counter,10)
                          ENDIF
                        
                          IF ( nt.EQ.1 ) THEN
                             BER= SQRT((bkgd)/nb)
                             IF ( nrm.EQ.1 ) THEN
                                WRITE (message,
     &'('' bkgd cts ='',F10.3,'' Err ='',F10.4,'' No. bkgd pts'',I8)')
     & bkgd , BER, nb
                                CALL XWRITE(message,20)
                                WRITE (message,
     &'('' for the '',I4,''th RAW DATA FILE row with bkgd '')')
     & counter
                                CALL XWRITE(message,20)

                             ENDIF
                          ELSEIF ( nrm.EQ.1 ) THEN
                             IF ( .NOT.userbackflag ) THEN
                                avbak = (bkgd+DB)/fctim
                                WRITE (message,
     &'('' Chisq betwn bkgd cts & overall avrg bkgd for file '',F12.2)')
     & chi
 
                          
                                CALL XWRITE(message,20)
                                WRITE (message,
     &'('' Average bkgd '',F10.3,'' cnts/s '',I4,''th row'')') 
     & avbak,counter
                                CALL XWRITE(message,20)

                             ENDIF
                          ENDIF
                       ENDIF
               ENDIF
            ENDIF
         ENDIF


      return

      
         end



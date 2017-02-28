* this writes a qdp file
      SUBROUTINE WRITE_QDP(Qdpfile,Timelun,Imaxtime,Gti,Imaxgti,
     &                     Mjd,Timewin,Maxgti,Imaxtw,Emin,Emax,
     &                     Regionfile,Status)

      implicit none

      CHARACTER*(*) Qdpfile , Regionfile
      INTEGER Timelun, Status
      INTEGER Imaxtime, Imaxgti, Maxgti, Imaxtw, Emin , Emax
      REAL*8 Gti(2,Maxgti) , Mjd , Timewin(2,Maxgti)
c
c  Local variables
c
      REAL*8 timewin1, timewin2
      INTEGER*4 MBUF , ngot , mm , nget , jj, j
      PARAMETER (MBUF=1000)
      REAL*8 mjd1, time(MBUF)
      INTEGER*4 pha(MBUF)
      INTEGER*4 lun , lun1
      INTEGER*4 i
      character(100) outline , tmpfile
 
      tmpfile = 'extract.tmp'
      CALL GETLUN(Timelun)
 
      CALL OPENWR(Timelun,tmpfile,'old',' ',' ',0,0,Status)
      IF ( Status.NE.0 ) THEN
         CALL XERROR('Unable to reopen temporary file',5)
         CALL XERROR('No light curve data written',5)
         RETURN
      ENDIF
 
      CALL GETLUN(lun)
      Status = 0
      CALL OPENWR(lun,Qdpfile,'new',' ',' ',0,0,Status)
      IF ( Status.NE.0 ) RETURN
      IF ( Emin.NE.0 .AND. Emax.NE.0 ) WRITE (lun,99009) Emin, Emax
 
      WRITE (lun,99012) Mjd
      CALL GETLUN(lun1)
      CALL OPENWR(lun1,Regionfile,'old',' ',' ',0,1,Status)
      IF ( Status.EQ.0 ) THEN
         WRITE (lun,99010)
         DO WHILE ( Status.EQ.0 )
            READ (lun1,'(a)',IOSTAT=Status) outline
            IF ( Status.EQ.0 ) WRITE (lun,99011) outline
         ENDDO
      ENDIF
      CLOSE (lun1)
      Status = 0
      CALL FRELUN(lun1)
      WRITE (lun,99003)
      DO 100 i = 1 , Imaxgti
C         WRITE (lun,99004) Gti(1,i)/86400.d0 , Gti(2,i)/86400.d0
         timewin1=Gti(1,i) - Mjd
         timewin2=Gti(2,i) - Mjd
         WRITE (lun,99004) timewin1,timewin2
 100  CONTINUE
 
* Convert back to days since beginning of year
 
      mjd1 = Mjd + Gti(1,1)/86400.D0
      CALL EXMJD(mjd1)
 
      IF ( Imaxtw.EQ.0 ) THEN
C         WRITE (lun,99001) Mjd + Gti(1,1)/86400.D0 - mjd1 ,
C     &                     Mjd + Gti(2,Imaxgti)/86400.D0 - mjd1
         WRITE (lun,99001) Gti(1,1) - Mjd , Gti(2,Imaxgti) - Mjd
      ELSE
*         WRITE (lun,99001) Timewin(1,1)/86400.d0 - mjd1,
*     &Timewin(2,Imaxtw)/86400.d0 - mjd1
         WRITE (lun,99001) Timewin(1,1) - Mjd , Timewin(2,Imaxtw) - Mjd
      ENDIF
 
      WRITE (lun,99006)
 
      CALL XWRITE(' Writing QDP file',5)
      j = 0
      ngot = 0
      mm = Imaxtime/MBUF + 1
      DO 300 i = 1 , mm
         CALL XCLOCK(i,mm,10)
         nget = MIN(MBUF,Imaxtime-ngot)
         ngot = ngot + nget
         DO 150 jj = 1 , nget
            READ (Timelun,*) time(jj) , pha(jj)
 150     CONTINUE
         DO 200 jj = 1 , nget
            IF ( Emin.EQ.0 .OR. Emax.EQ.0 ) THEN
               WRITE (lun,99007) (time(jj)-Gti(1,1))*86400.D0 , pha(jj)
               j = j + 1
            ELSEIF ( pha(jj).GE.Emin .AND. pha(jj).LE.Emax ) THEN
               WRITE (lun,99007) (time(jj)-Gti(1,1))*86400.D0 , pha(jj)
               j = j + 1
            ENDIF
 200     CONTINUE
 300  CONTINUE
      WRITE (outline,99008) j
      CALL XWRITE(outline,5)
 
      CLOSE (lun)
      CALL FRELUN(lun)
      CLOSE (Timelun)
      CALL FRELUN(Timelun)
      RETURN
99001 FORMAT (1x,'!!H',3x,'-1',3x,g23.16,3x,g23.16,3x,'1',3x,'0')
99002 FORMAT (1x,g23.16,'1',6x,'1')
99003 FORMAT (1x,'!START TIME',3x,'STOP TIME')
99004 FORMAT (1x,'!',g23.16,3x,g23.16)
99005 FORMAT (1x,g23.16,2x,i5)
99006 FORMAT (1x,'TIME',2x,'CTS',2x,'PHA')
99007 FORMAT (1x,g23.16,2x,'1',2x,i5,2x,'    1')
*99007 FORMAT (1x,F23.16,2x,'1',2x,i5,2x,'    1')
99008 FORMAT (1x,'Photons written to QDP file : ',i10)
99009 FORMAT (1x,'! PHA1 ',i5,' PHA2 ',i5)
99010 FORMAT (1x,'! Region: ')
99011 FORMAT (1x,'! ',a)
99012 FORMAT (1x,'! Modified Julian Day of Launch',f20.10)
      END

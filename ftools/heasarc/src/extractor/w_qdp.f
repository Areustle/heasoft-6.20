C*==write_qdp.spg  processed by SPAG 4.50J  at 15:13 on  8 Mar 1995
 
C this writes a qdp file
      SUBROUTINE WRITE_QDP(N,Gti,Imaxgti,Ierr,Imaxlc,Lc,Lcsize,Mintime1)
 
      IMPLICIT NONE

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'

      DOUBLE PRECISION Gti(3,MAXGTI), Mintime1

      INTEGER Imaxlc
      INTEGER Lc(Imaxlc)
      INTEGER N, Imaxgti, Ierr, Lcsize


      DOUBLE PRECISION time, binval , binval0, binsize, dtmp1

      INTEGER pha, qdplun , unblun , lun1, i , timelun
      INTEGER totphot , binnum

      CHARACTER(100) outline
      CHARACTER(256) tmpfile


      DOUBLE PRECISION CALCBINSIZE
      INTEGER LENACT
      EXTERNAL CALCBINSIZE, LENACT

 
      time = 0.0
      pha = 0
      qdplun = 0
      unblun = 0
      lun1 = 0
      i = 0
      timelun = 0
      binval = 0.0
      binval0 = 0.0
      outline = ' '
      tmpfile = ' '
      totphot = 0
      binnum = 0
      dtmp1 = 0.0
      binsize = 0.0
 
      qdplun = -1
      unblun = -1
      IF ( Qdpfile.NE.' ' ) THEN
         CALL GETLUN(qdplun)
         tmpfile = char(92)//Qdpfile
         CALL OPENWR(qdplun,tmpfile,'new',' ',' ',0,0,Ierr)
         IF ( Ierr.NE.0 ) THEN
            CALL fcerr(' Problem opening qdp file')
            CALL fcerrm(ierr)
            RETURN
         ENDIF
      ENDIF
      IF ( Unbinlc.NE.' ' ) THEN
         CALL GETLUN(unblun)
         tmpfile = char(92)//Unbinlc
         CALL OPENWR(unblun,tmpfile,'new',' ',' ',0,0,Ierr)
         IF ( Ierr.NE.0 ) THEN
            CALL fcerr(' Problem opening unbinned qdp file ')
            CALL fcerrm(ierr)
            RETURN
         ENDIF
      ENDIF
      DO i = 1, nkeys
         IF ( key(i) .EQ. Ecol ) THEN
            IF ( qdplun.NE.-1 ) 
     &              WRITE (qdplun,99009) NINT(keyval(1,i)), 
     &                                   NINT(keyval(2,i))
            IF ( unblun.NE.-1 ) 
     &              WRITE (unblun,99009) NINT(keyval(1,i)), 
     &                                   NINT(keyval(2,i))
         ENDIF
      ENDDO
 
      IF ( unblun.NE.-1 ) THEN
 
         tmpfile = 'extract.tmp'
         CALL MAKEFNAME(tmpfile)
 
         CALL GETLUN(timelun)
 
         CALL OPENWR(timelun,tmpfile,'old','U',' ',0,0,Ierr)
         IF ( Ierr.NE.0 ) THEN
            CALL fcerr('Unable to reopen temporary file')
            CALL fcerr('No light curve data written')
            CALL fcerrm(ierr)
         ENDIF
      ENDIF
 
      Ierr = 0
 
      IF ( Regionfile.NE.' ' ) THEN
         CALL GETLUN(lun1)
         CALL OPENWR(lun1,Regionfile,'old',' ',' ',0,1,Ierr)
         IF ( Ierr.EQ.0 ) THEN
            IF ( qdplun.NE.-1 ) WRITE (qdplun,99010)
            IF ( unblun.NE.-1 ) WRITE (unblun,99010)
            DO WHILE ( Ierr.EQ.0 )
               READ (lun1,'(a)',IOSTAT=Ierr) outline
               IF ( Ierr.EQ.0 ) THEN
                  IF ( qdplun.NE.-1 ) WRITE (qdplun,99011)
     &                 outline(1:LENACT(outline))
                  IF ( unblun.NE.-1 ) WRITE (unblun,99011)
     &                 outline(1:LENACT(outline))
               ENDIF
            ENDDO
         ENDIF
         CLOSE (lun1)
         CALL FRELUN(lun1)
      ENDIF
      Ierr = 0
      IF ( qdplun.NE.-1 ) WRITE (qdplun,99003)
      IF ( unblun.NE.-1 ) WRITE (unblun,99003)
      DO 100 i = 1 , Imaxgti
         IF ( unblun.NE.-1 ) WRITE (unblun,99004) Gti(1,i)/86400.D0 , 
     &                              Gti(2,i)/86400.D0
         IF ( qdplun.NE.-1 ) WRITE (qdplun,99004) Gti(1,i)/86400.D0 , 
     &                              Gti(2,i)/86400.D0
 100  CONTINUE
 
C Convert back to days since beginning of year
 
 
      IF ( qdplun.NE.-1 ) WRITE (outline,'(G23.16,a,g13.6)') Mintime1,
     &                           ' Binned at: ' , Binlc
      IF ( qdplun.NE.-1 ) WRITE (qdplun,99016)
     &                           'Light Curve, starts at '//
     &                           outline(1:LENACT(outline))
      IF ( unblun.NE.-1 ) WRITE (outline,'(G23.16)') Mintime1
      IF ( unblun.NE.-1 ) WRITE (unblun,99016)
     &                           'Light Curve, starts at '//
     &                           outline(1:LENACT(outline))
      IF ( qdplun.NE.-1 ) WRITE (qdplun,*) 'label x Delta Time'
      IF ( unblun.NE.-1 ) WRITE (unblun,*) 'label x Delta Time'
      IF ( qdplun.NE.-1 ) WRITE (qdplun,*) 'label y Counts/Time'
      IF ( unblun.NE.-1 ) WRITE (unblun,*) 'label y Counts/Time'
      dtmp1 = Gti(2,Imaxgti) - Mintime1
      IF ( qdplun.NE.-1 ) WRITE (qdplun,*) 'rescale x ' , 0 , dtmp1
      IF ( unblun.NE.-1 ) WRITE (unblun,*) 'rescale x ' , 0 , dtmp1
      IF ( unblun.NE.-1 ) WRITE (unblun,99006)
      IF ( qdplun.NE.-1 ) WRITE (qdplun,99015)
 
      IF ( unblun.NE.-1 ) WRITE (unblun,*) '! MJDREF is ' , Mjdref
      IF ( qdplun.NE.-1 ) WRITE (qdplun,*) '! MJDREF is ' , Mjdref
 
 
      IF ( unblun.NE.-1 ) WRITE (unblun,99001) -1.0, 
     & (emjdrfi+emjdrff+Mintime1/86400.D0) - Mjdref , 
     & (emjdrfi+emjdrff+Gti(2,Imaxgti)/86400.D0) - Mjdref, 1
      IF ( qdplun.NE.-1 ) WRITE (qdplun,99001) Binlc , 
     & (emjdrfi+emjdrff+(Mintime1+Binlc/2.D0)/86400.D0) - Mjdref , 
     & (emjdrfi+emjdrff+(Gti(2,Imaxgti)+Binlc/2.D0)/86400.D0) - Mjdref,
     & 0
 
 
 
      binval = 0.D0
      binval0 = 0.D0
 
 
      IF ( unblun.NE.-1 ) THEN

         Ierr = 0
         CALL XWRITE(' Writing unbinned light curve',5)
         CALL XWRITE(' This may take a while....',5)
 
         READ (timelun,IOSTAT=Ierr) time , pha
 
         DO WHILE ( Ierr.EQ.0 )

            dtmp1 = time - Mintime1
            WRITE (unblun,99007) dtmp1 , pha
            READ (timelun,IOSTAT=Ierr) time , pha

         ENDDO
 
         Ierr = 0
 
         totphot = N

         CLOSE (timelun)
         CALL FRELUN(timelun)
         CLOSE (unblun)
         CALL FRELUN(unblun)

         CALL sort_time(Unbinlc)
 
      ENDIF
 
 
      IF ( qdplun.NE.-1 ) THEN
 
         binval = Mintime1
         binval0 = binval
         binnum = 0
         totphot = 0
         DO 150 i = 1 , Lcsize
 
 
            binsize = CALCBINSIZE(binval,Binlc,Gti,Imaxgti,ierr)
            IF ( binsize.NE.0.D0 .AND. binsize/Binlc.GE.Lcthresh ) THEN
               dtmp1 = binval - Mintime1
               WRITE (qdplun,99013) dtmp1 , Binlc/2.D0 , Lc(i)/binsize , 
     &                              SQRT(FLOAT(Lc(i)))/binsize , 
     &                              binsize/SNGL(Binlc)
            ENDIF
 
            totphot = totphot + Lc(i)
 
            binnum = binnum + 1
 
            binval = binval0 + Binlc*binnum

 150     CONTINUE

         CLOSE (qdplun)
         CALL FRELUN(qdplun)

      ENDIF
 
      RETURN
c MJT 16July96 (g77/linux) doesn't like "bare" format
99001 FORMAT (1x,'!!H',3x,f5.1,3x,g23.16,3x,g23.16,3x,i2,3x,'0')
99003 FORMAT (1x,'!START TIME',3x,'STOP TIME')
99004 FORMAT (1x,'!',g23.16,3x,g23.16)
99006 FORMAT (1x,'TIME',2x,'CTS',2x,'PHA')
99007 FORMAT (1x,g23.16,2x,'1',2x,i5,2x,'    1')
C99007 FORMAT (x,F23.16,2x,'1',2x,i5,2x,'    1')
99009 FORMAT (1x,'! PHA1 ',i5,' PHA2 ',i5)
99010 FORMAT (1x,'! Region: ')
99011 FORMAT (1x,'! ',a)
99013 FORMAT (1x,g23.16,2x,g23.16,2x,g13.6,2x,g13.6,2x,g13.6)
99015 FORMAT (1x,'READ SERR 1 2'/,1x,'color off 3')
99016 FORMAT (1x,'Label Top ',a)
      END

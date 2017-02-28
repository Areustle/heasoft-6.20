 
      SUBROUTINE GETEVENTS(Lun,Npha,Numsrc,Maxpha,Imaxpha,Timelun,
     &                     Imaxtime,Mjd,Winfile,Timewin,Maxgti,Imaxtw,
     &                     Nrows,Xi,Yi,Ti,Ei,Imaxgti,Gti,Colmode,Chan,
     &                     Ratio,Timevar,Chi,Status)
      IMPLICIT NONE
c
c  Read events 
c
c  I  lun        (i)  Logical unit of open events file
c  O  npha       (i)  Spectra for sources
c  I  numsrc     (i)  Number of sources from detect or defined as
c                     regions
c  I  maxpha     (i)  Maximum number of energy channels allowed
c  O  imaxpha    (i)  Maximum photon energy (channel)
c  I  timelun    (i)  Open logical unit for lcurve file
c  O  imaxtime   (i)  Actual maximum time
c  O  mjd        (d)  Modified Julian date
c  I  winfile    (s)  XRONOS-style windows file
c  O  timewin    (d)  Time windows
c  I  maxgti     (i)  Maximum number of GTIs allowed
c  O  imaxtw     (i)  Actual number of time windows
c  I  nrows      (i)  Number of rows in event list
c  I  xi         (i)  X column index
c  I  yi         (i)  Y column index
c  I  ti         (i)  Time column index
c  I  ei         (i)  Energy column index
c  O  imaxgti    (i)  Number GTIs read in
c  O  gti        (d)  GTIs read in
c  I  colmode    (i)  Color mode (0=none 1=detect 2=region list)
c  I  chan       (i)  Channels ranges to use in color-color plot
c  O  ratio      (i)  Hardness ratio arrays
c  I  timevar    (l)  Calculate statistical variability for sources
c  O  chi        (r)  Chi-square statistic (VARIABILITY)
c  O  status     (i)  Error flag (0=OK)
c
      INCLUDE '../stat/detect.inc'
      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'

      INTEGER*4 Lun , Status , Timelun , Imaxtime , Imaxpha
      INTEGER*4 Maxgti , Imaxtw , Imaxgti, Numsrc, Maxpha
      CHARACTER*(*) Winfile
      INTEGER Xi, Yi, Ti, Ei, Nrows, Colmode
      REAL*8 Mjd , Timewin(2,Maxgti) , Gti(2,Maxgti)
      INTEGER*4 Chan(4), Ratio(MAXDET,6)
      LOGICAL Timevar
      REAL*4  Chi(MAXDET) 
      INTEGER*4 Npha(Numsrc,Maxpha)
c
c  MINPHACHAN originally from phot_common.inc
c  Unsure if mission-dependent ( May need to add to MDB )
c
      INTEGER MINPHACHAN
      PARAMETER(MINPHACHAN=15)
c
c  Local variables
c
      INTEGER*4 MBUF
      PARAMETER (MBUF=500)
      INTEGER nfound , nreject, rejreg, rejwin, rejphs
      INTEGER*4 i , jj , ii, ij
      INTEGER*4 ssint(MAXDET), sint_temp(MAXDET)
      REAL*4 dist(MAXDET), dd, total_image, image_distr
      REAL*8 previous_time 
      REAL*8 xs_mjdrd , xs_mjdrf, mjdreff, mjdrefi
 
      character(20) extname
      CHARACTER*(MAX_FILELEN) tmpname, filesave
 
      INTEGER*4 nloop , ngot , nget
 
      REAL*8 dtime(MBUF) , mjd1
      LOGICAL*4 anyf
      LOGICAL flagvals(MBUF)
      LOGICAL*4 good, color, isinreg
      LOGICAL*4 opened
      INTEGER*4 ierr
 
      INTEGER*4 ntime, sumnpha
c     INTEGER*4 tpha(MBUF) , x(MBUF) , y(MBUF), npha(10000,2048)
      INTEGER*4 tpha(MBUF)
      REAL*8 x(MBUF) , y(MBUF)
      character(80) comment
      LOGICAL first_time

      INTEGER*4 eminchan, emaxchan
c
      Status = 0
      color = Colmode.gt.0

      IF ( color ) THEN
         WRITE (ZWRite,99001) chan(1),chan(2)-1,chan(2),chan(3)-1,
     &      chan(3),chan(4)
         CALL XWRITE(ZWRite,10)
      ENDIF
c
      opened = .FALSE.
      ierr = 0
      tmpname = 'extract.tmp'
c
c find the events table
c
      do i = 1, Maxpha
         do jj = 1, Numsrc
            npha(jj,i) = 0
         enddo
      enddo
 
      Imaxtime = 0
      Imaxpha = 0
 
      i = 1
      extname = ' '

c  Initialize to avoid warning
      total_image = 0.
      image_distr = 0.
c  --

      Status = 0
      CALL FTGKYD(Lun,'XS-MJDRD',xs_mjdrd,comment,Status)
      CALL FTGKYD(Lun,'XS-MJDRF',xs_mjdrf,comment,Status)
      Mjd = xs_mjdrd + xs_mjdrf
 
      IF ( Status.NE.0 ) THEN
         CALL XWARN(' xs-mjdrd and xs-mjdrf not found',5)
         CALL XWARN(' search for MJDREFF and MJDREFI',5)
         Status=0
         CALL FTGKYD(Lun,'MJDREFF',mjdreff,comment,Status)
         CALL FTGKYD(Lun,'MJDREFI',mjdrefi,comment,Status)
         Mjd= mjdrefi + mjdreff
         IF(status.NE.0) THEN
            CALL XWARN(' mjdrefi anf mjdreff not found',5)
            CALL XWARN
     &          (' Modified Julian Date for Observation set to 0',5)
            Status = 0
            Mjd = 0
         ENDIF
      ENDIF
 
* Calculate the mjd of the first of this year for the xronos window file
 
*      mjd1 = Mjd + Gti1/86400.D0
      mjd1 = Mjd
 
*
* Convert the GTI's to MJD
*
      DO 200 i = 1 , Imaxgti
         Gti(1,i) = Gti(1,i)/86400.D0 + Mjd
         Gti(2,i) = Gti(2,i)/86400.D0 + Mjd
 200  CONTINUE
 
      ntime = 0
      Imaxtw = 0
*
* Reset the cached file names
*
      good = .TRUE.
      CALL SELECT_INTEN(' ',tpha(1),good)
      CALL SELECT_TIME(' ',dtime(1),good,Timewin,Maxgti,Imaxtw,mjd1)
      CALL SELECT_PHASE(' ',dtime(1),good)
 
c ****************
      first_time = .TRUE. 
      do i = 1, MAXDET
         ssint(i)=0
         sint_temp(i)=0
         dist(i)=0.0
      enddo
 1024 IF (timevar) THEN 
         previous_time=0.
         IF ( first_time ) THEN
            CALL xwrite(' first pass ',10)
            total_image = 1.0 
         ELSE
            CALL xwrite(' second pass ',10)
            total_image = image_distr
         ENDIF 
         image_distr = 0.
      ENDIF
      ngot = 0
      nloop = Nrows/MBUF + 1
      nfound = 0
      nreject = 0
      rejreg = 0
      rejwin = 0
      rejphs = 0
      status = 0
      filesave = ' '
c ****************
      DO 300 i = 1 , nloop
c         CALL XCLOCK(i,nloop,5)
         nget = MIN(MBUF,Nrows-ngot)
         IF ( nget.GT.0 ) THEN
            ngot = ngot + nget
            ii = 1 + (i-1)*MBUF
            CALL FTGCFD(Lun,Xi,ii,1,nget,x,flagvals,anyf,Status)
            CALL FTGCFD(Lun,Yi,ii,1,nget,y,flagvals,anyf,Status)
            CALL FTGCFD(Lun,Ti,ii,1,nget,dtime,flagvals,anyf,Status)
            CALL FTGCFJ(Lun,Ei,ii,1,nget,tpha,flagvals,anyf,Status)
            IF ( Status.NE.0 ) THEN
               CALL XERROR(' Error reading event data',10)
               GOTO 400
            ENDIF
            DO 220 jj = 1 , nget
*
* Convert the time to mjd
*
               dtime(jj) = Mjd + dtime(jj)/86400.D0
 
               good = .TRUE.

               IF ( .not.color ) THEN
                  good = good.and.isinreg(1, x(jj), y(jj))
                  if ( .not.good ) then
                     rejreg = rejreg + 1
                     GOTO 210
                  ENDIF
               ENDIF
 
               CALL SELECT_INTEN(Winfile,tpha(jj),good)
               CALL SELECT_TIME(Winfile,dtime(jj),good,Timewin,Maxgti,
     &                          Imaxtw,mjd1)
               if ( .not.good ) then
                  rejwin = rejwin + 1
                  goto 210
               endif
               CALL SELECT_PHASE(Winfile,dtime(jj),good)
               if ( .not.good ) rejphs = rejphs + 1
 210           IF ( good ) THEN
                  nfound = nfound + 1
c source stats
                  IF ( timevar ) THEN
                     IF ( dtime(jj).LT.previous_time ) THEN
                        write(ZWRite,*) previous_time, dtime(jj)
                        call XWRITE(ZWRite, 10)
                        CALL xerror(' File is not time ordered ',5)
                        RETURN
                     ELSE
                        previous_time = dtime(jj)
                     ENDIF
                     image_distr = image_distr + 1.D0/total_image
                  ENDIF 
                  ij = 1
                  DO WHILE ( ij.LE.Numsrc )
                     if ( Colmode.eq.1 ) then
                        good = HOT(ij).EQ.1 .AND.
     &                         x(jj).GE.DTSox(ij)-BXHa(ij) .AND. 
     &                         x(jj).LE.DTSox(ij)+BXHa(ij) .AND. 
     &                         y(jj).GE.DTSoy(ij)-BXHa(ij) .AND. 
     &                         y(jj).LE.DTSoy(ij)+BXHa(ij)
                     elseif ( Colmode.eq.2 ) then
                        good = isinreg(ij,x(jj),y(jj))
                     else
c                       Spectrum case (PSF_PHA)
                        good = .TRUE.
                     endif
c ***************           
                     if ( good ) then
                        IF ( timevar ) THEN
                           if (first_time) then
                              ssint(ij)=ssint(ij)+1
                           else
                              sint_temp(ij)= sint_temp(ij)+1
                              dd=abs(image_distr-real(sint_temp(ij))
     &                             /real(ssint(ij))) 
                              if (dd.gt.dist(ij)) dist(ij)=dd
                           endif
                        ENDIF 
c ***************
                        if (first_time) then
                           IF( tpha(jj).GT.0 .AND. tpha(jj).LE.maxpha )
     &                     THEN
                              npha(ij,tpha(jj)) = npha(ij,tpha(jj))+1
                              Imaxpha = MAX(Imaxpha,tpha(jj))
                           ENDIF
                           IF ( tpha(jj).GE.Chan(1) .AND. tpha(jj)
     &                          .LT.Chan(2) ) THEN
                              Ratio(ij,1) = Ratio(ij,1) + 1
                           ELSEIF ( tpha(jj).GE.Chan(2) .AND. tpha(jj)
     &                              .LT.Chan(3) ) THEN
                              Ratio(ij,2) = Ratio(ij,2) + 1
                           ELSEIF ( tpha(jj).GE.Chan(3) .AND. tpha(jj)
     &                              .LE.Chan(4) ) THEN
                              Ratio(ij,3) = Ratio(ij,3) + 1
                           ENDIF
                        endif
                     endif
                     ij = ij + 1
                  ENDDO
c
c lightcurve
c
                  IF ( .NOT.opened .AND. ierr.EQ.0 .AND. Timelun.NE.-1 )
     &                 THEN
                     CALL GETLUN(Timelun)
                     CALL OPENWR(Timelun,tmpname,'NEW',' ','L',0,0,ierr)
                     IF ( ierr.NE.0 ) THEN
                        CALL XWARN('Unable to open temporary file',5)
                        CALL XWARN('No light curve data will be written'
     &                             ,5)
                     ELSE
                        opened = .TRUE.
                     ENDIF
                  ENDIF
c                  write(*,*)'ierr, timelun',ierr, timelun
                  IF ( ierr.EQ.0 .AND. Timelun.NE.-1 ) THEN
                     WRITE (Timelun,99003) dtime(jj) , tpha(jj)
                     ntime = ntime + 1
                  ENDIF
               ELSE
                  nreject = nreject + 1
c                  write(*,*)'nreject',nreject
               ENDIF
 220        CONTINUE
         ENDIF
 300  CONTINUE

      IF ( timevar .AND. first_time ) THEN
         first_time = .FALSE.
         GOTO 1024
      ENDIF
c 
c  Free region work buffer
c
      if ( color ) then
         DO i=1,Numsrc
            if (ssint(i).GT.100) THEN
               Chi(i) = 4.*dist(i)**2.*100.*
     &         (2.-exp((100.-ssint(i))/100.))

c
c      the above statement is to limit the number of photons in the
c      calculation of Chi**2 to 200. This is to reduce the correlation
c      between test sensitivity and count rate.  Temp grows linearly, as
c      normal KS test requires, up to a value of 100 after that it deviates
c      from linear growth and is limited to 200.
c
            ELSE
               Chi(i) = 4.*dist(i)**2.*real(ssint(i))
            ENDIF
c     
c 20/04/98 FT - compute the mean on channels
c
            DO 999 jj=1,3
               sumnpha = 0
               ratio(i,jj+3) = 0
               IF(chan(jj).LT.0.OR.chan(jj+1).LT.0.OR.
     &              chan(jj+1).LT.chan(jj)) THEN
                  WRITE (ZWRite,*) 
     &                 ' Unable to compute mean on channel ',
     &                 jj,chan(jj),
     &                 ' - ',chan(jj+1)
                  CALL XWRITE(ZWRite,10)
               ELSE
                  DO ij = chan(jj), chan(jj+1)-1
                     IF(ij.LE.Maxpha) sumnpha = sumnpha + npha(i,ij)*ij
                  ENDDO
                  IF(ratio(i,jj).NE.0.AND.sumnpha.GT.MINPHACHAN) THEN 
                     ratio(i,jj+3) = sumnpha/ratio(i,jj)
                  ELSE
                     eminchan = chan(jj)
                     emaxchan = chan(jj+1)
                     if(eminchan.EQ.0)eminchan=1
                     if(emaxchan.EQ.0)emaxchan=1
                     ratio(i,jj+3) = INT(10**((ALOG10(FLOAT(eminchan))+
     &                    ALOG10(FLOAT(emaxchan)))/2.0))
                  ENDIF               
               ENDIF
 999        CONTINUE
         ENDDO 
      endif
c

c ****************************
      WRITE (ZWRite,99002) nfound , nreject
      CALL XWRITE(ZWRite,10)
      if ( rejreg.gt.0 ) then
         WRITE (ZWRite,*) ' Region cut = ', rejreg, ' events'
         call rmvxbk(ZWRite(2:))
         CALL XWRITE(ZWRite,15)
      endif
      if ( rejwin.gt.0 ) then
         WRITE (ZWRite,*) ' Window cut = ', rejwin, ' events'
         call rmvxbk(ZWRite(2:))
         CALL XWRITE(ZWRite,15)
      endif
      if ( rejphs.gt.0 ) then
         WRITE (ZWRite,*) ' Phase cut = ', rejphs, ' events'
         call rmvxbk(ZWRite(2:))
         CALL XWRITE(ZWRite,15)
      endif
      if(nfound.eq.0)then
        status = 1
        return
      endif
      Imaxtime = ntime
      IF ( Timelun.NE.-1 ) THEN
         CLOSE (Timelun)
         CALL FRELUN(Timelun)
      ENDIF
 
      IF ( Imaxtime.GE.1 ) CALL SORT_TIME
 
 400  IF ( Status.NE.0 ) THEN
         WRITE (ZWRite,'(a,i5)') ' Error status from fitsio was ' , 
     &                          Status
         CALL XERROR(ZWRite,10)
      ENDIF
 
      RETURN
99001 FORMAT (' channel ranges =',3(1x,i4,'-',i4))
99002 FORMAT ('   Accepted:',i10,' Rejected:',i10)
99003 FORMAT (1x,F23.16,2x,i5)
      END

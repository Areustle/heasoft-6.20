
*+RDSSS_INFO
c     ---------------------------------------------------------
      subroutine rdsss_info(iunit,sttime,stptime,majorfrm,minorfrm,
     &                      jcgparm,mean_ice,ra,dec,equinox,meanepoc,
     &                      chatter,errflg)
c     ---------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------
c This subroutine reads some keywords from an Einstein PHA
c spectra. It's primary intended use is with the convertor
c task sss2pha
c NOTE : Assumes file is already open and in correct position.
c        ... close file at end, using FTCLOS, or
c        ... read another extension

c --- VARIABLES -----------------------------------------------
c
      real sttime,stptime,jcgparm,mean_ice
      real ra,dec,equinox,meanepoc
      integer minorfrm,majorfrm,chatter,errflg,iunit
c
c --- KEYWORDS READ -------------------------------------------
c
c     STRTTIME   : obs start time
c     STOPTIME   : obs stop time
c     JCGPARM    : background prediction parameter
c     MEAN_ICE   : mean water ice coating on detector
c     RA         : right acension of target (decimal degrees)
c     DEC        : declination of the target (decimal degrees)
c     MAJORFRM   : major frame time
c     MINORFRM   : minor frame time
c     MEANEPOC   : mean obs time 
c     EQUINOX    : equinox of celestial coordinate
c               
c --- AUTHORS/MODIFICATION ------------------------------------
c
c Rehana Yusaf (Jan 25 1995) 1.0.0; 
c
      character(5) version
      parameter (version='1.0.0')
*-
c INTERNALS
      integer status
      character(40) errstr, wrnstr
      character(70) comm
      character(80) message,errinfo

      errstr = ' ERROR RDSSS_INFO Ver :'//version//':'
      wrnstr = ' WARNING RDSSS_INFO Ver :'//version//':'
      status = 0
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        message = ' ... using RDSSS_INFO Ver : '//version
        call fcecho(message)
      ENDIF
c
c --- READ KEYWORDS ---
c
        call ftgkye(iunit, 'STRTTIME', 
     &          sttime, 
     &          comm,
     &          status)
        IF (status.NE.0) THEN 
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding STRTTIME'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF 

        call ftgkye(iunit, 'STOPTIME',
     &          stptime,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding STOPTIME'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'JCGPARM',
     &          jcgparm,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding JCGPARM'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'MEAN_ICE',
     &          mean_ice,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding MEAN_ICE'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'RA',
     &          ra,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding RA'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'DEC',
     &          dec,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding DEC'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'EQUINOX',
     &          equinox,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding EQUINOX'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkye(iunit, 'MEANEPOC',
     &          meanepoc,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding MEANEPOC'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkyj(iunit, 'MINORFRM',
     &          minorfrm,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding MINORFRM'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        call ftgkyj(iunit, 'MAJORFRM',
     &          majorfrm,
     &          comm,
     &          status)
        IF (status.NE.0) THEN
           IF (chatter.GE.15) THEN
             errinfo = errstr//' finding MAJORFRM'
             call wt_ferrmsg(status,errinfo)
           ENDIF
           status = 0
        ENDIF

        IF (chatter.GE.20) THEN
          message = ' ... keywords have been read'
          call fcecho(message)
        ENDIF
        return
        end
c ---------------------------------------------------------------
c       END OF RDSSS_INFO
c --------------------------------------------------------------- 

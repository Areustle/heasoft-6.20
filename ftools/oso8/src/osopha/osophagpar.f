CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version


c This routine reads parameters in the parameter file for program OSOPHA.f

        SUBROUTINE OSOPHAGPAR(src_equi,arrsz,src_Ra50,src_Dc50,
     &                title,
     &                detector,rawlist,rawphaf,nraw,ph_restriction,
     &                period,ephemeris,
     &                phase_st,phase_sp,fov_check,cont_srcelist,
     &                nsrces,cont_Ra50,cont_Dc50,
     &                ch_start,ch_end,integ_time,
     &                int_low,int_high,
     &                lchan_min,lchan_max,
     &                hchan_min,hchan_max,
     &                beginday,imsecs,endday,imsece,
     &                bg,bgl,bgh,
     &                tchat,lchat,clobber,status)


      IMPLICIT NONE

      
      CHARACTER*(*) title,cont_srcelist,rawphaf(*),rawlist
      LOGICAL clobber,isthere,pipe,fov_check,ph_restriction
      REAL phase_st,phase_sp, bg,bgl,bgh
      INTEGER arrsz,nsrces,ch_start,ch_end,integ_time
      INTEGER status, tchat, lchat, equin, rawlun, blocksize, hdutype
      INTEGER lenact, errstat, detector, listlun, nraw
      INTEGER len, npipe, n, m, i, pipepos, pipepos1
      INTEGER lchan_min,lchan_max,hchan_min,hchan_max
      character(160) errm,rawfile, context, listfile,line
      character(160) source, inp_line(10),file,rawname
      character(40) decstr, rastr, eq(10), ra(10),de(10),
     &    rastrg,decstrg
      DOUBLE PRECISION equinox,radeg,decdeg,dtr, src_equi,
     &    period,ephemeris,sra,sdec
      integer beginyr, beginday, endyr, endday, imsecs, imsece
      real beginsec, endsec
      character(8) stdate, sttime, endate, entime
      REAL src_Dc50, src_Ra50, cont_Ra50(arrsz), 
     &      cont_Dc50(arrsz),int_low, int_high
      integer iy,imon,id,ih,im,is
      character(10) mission
      character(10) raw_det, detnam
      integer mtab(12),ndays

      DATA dtr/.0174532925D0/
      data mtab/31,28,31,30,31,30,31,31,30,31,30,31/

c  Initialize to avoid warning
      npipe = 0
      pipepos = 0
      pipepos1 = 0
c  --

C initialize variables

      blocksize=2880
      errm = ' '
      errm=' '
      status=0
      iy = 0
        imon = 0
        id = 0
        ih = 0
        im = 0
        is = 0
        beginyr=0
        beginday=0
        endyr=0
        endday=0
        imsecs=0
        imsece=0
        errstat=0
        do i=1,10
             ra(i)=' '
             de(i)=' '
             eq(i)=' '
             inp_line(i)=' '
        enddo



C get coord stuff
      call uclgsd('equinox', equinox, status)
      if (status .ne. 0) then
         errm = 'Error reading EQUINOX'
         CALL XAERROR(errm,1)
         return
      endif 

      
      call uclgst('ra', rastr, status)
      if (status .ne. 0) then
         errm = 'Error reading RA'
         CALL XAERROR(errm,1)
         return
      endif 

      call uclgst('dec', decstr, status)
      if (status .ne. 0) then
         errm = 'Error reading DEC' 
         CALL XAERROR(errm,1)
         return
      endif 

C Convert Right Ascension and Declination inputs into B1950.0 coordinates
C in degrees.
 
      equin = INT(equinox)
      CALL PARSERA(rastr,equin,radeg,status)
      IF ( status.NE.0 ) THEN
         errm = '  parsera: Error traslating RA string'
         CALL XAERROR(errm,1)
         return
        ENDIF
      CALL PARSEDEC(decstr,equin,decdeg,status)
      IF ( status.NE.0 ) THEN
         errm = '  parsdec: Error traslate DEC string'
         CALL XAERROR(errm,1)
         return
      ENDIF

      sra = radeg*dtr
      sdec = decdeg*dtr
      IF ( equinox.NE.1950.0D0 ) CALL SLA_PRECES('FK4',equinox,1950.0D0,
     &     sra,sdec)
C  returned values from sla routine are sra, sdec (in radians)
C ..so now convert back to decimal degrees
        
      src_equi = equinox
      src_Dc50 = real(sdec/dtr)
      src_Ra50 = real(sra/dtr)
 
c input title
      CALL UCLGST('rootname',title,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get ROOTNAME parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF
    
C Enter desired detector

150   CALL UCLGSI('detector',detector,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get DETECTOR parameter'
            CALL XAERROR(errm,1)
            return
      ELSE  
c            IF (detector .LT. 0 .OR. detector .GT. 2) THEN
            IF (detector .LT. 1 .OR. detector .GT. 2) THEN
                WRITE (errm,
     &'('' WARNING : Invalid DETECTOR selection. Please re-enter...'')')
                CALL XWRITE(errm,5)
                goto 150
            ENDIF
      ENDIF


c      IF (detector .eq. 0) detnam='DET-A'
      IF (detector .eq. 1) detnam='DET-B'
      IF (detector .eq. 2) detnam='DET-C'



c input listfile containing raw PHA data files
      CALL UCLGST('rawlist',rawlist,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get RAWLIST parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF




C Determine if list file exists 
C Open list file and put raw datafiles into memory

      INQUIRE (FILE=rawlist(:LENACT(rawlist)),EXIST=isthere)
      IF ( .not.isthere) THEN
           WRITE (errm,
     & '( ''ERROR: Cant find input list file : '', a80)')
     &  rawlist
           CALL XAERROR(errm,1)
           status=1
           return
      ENDIF

      rawfile=rawlist(:LENACT(rawlist))
      call ftgiou(listlun,status)
      call openwr(listlun,rawfile,'OLD',' ',' ',0,1,status )
      if ( status .ne. 0 ) then
         WRITE (errm,
     & '( '' Unable to open input raw data file list : '',a80)')
     & rawlist
         call xaerror ( errm,1)
         return
      endif

      nraw=0
      do while (.true.)
         read ( listlun, '(A)',iostat=errstat, end = 155) file
              rawname=file(:lenact(file))
              INQUIRE (FILE=rawname,EXIST=isthere)
              IF ( .not.isthere) THEN
                 WRITE (errm,
     & '( ''ERROR: Cant find raw data file :'', a80)')
     &  rawname
                 CALL XAERROR(errm,1)
                 status=1
                 return
              ELSE
                  nraw=nraw+1
                  rawphaf(nraw)=rawname
               ENDIF
      enddo

 155  continue

      close(listlun)
      call ftfiou(listlun,status)
      if ( status .ne. 0 ) then
              WRITE (errm,
     & '( '' Problem closing input list file : '',a80)')
     &  rawlist
             call xwrite(errm,5)
             return
      endif


      
      IF (nraw .eq. 0) then
         WRITE (errm,
     & '( '' ERROR : input data file list empty !! '', a80)')
     & listfile
         call xaerror (errm,1)
         status=1
         return
      endif

C now obtain the TELESCOP, DETNAM values for all the raw files      
      do i=1,nraw
           rawfile=rawphaf(i)
           call ftgiou(rawlun,status)
           call ftopen (rawlun, rawfile, 0, blocksize,status )
           if ( status .ne. 0 ) then
               WRITE (errm,
     & '( '' Unable to open raw file : '', a80)')
     &  rawfile
             call xwrite(errm,5)
             return
           endif
         
           call ftmahd ( rawlun, 2, hdutype, status )
           if ( status .ne. 0 ) then
              WRITE (errm,
     & '( '' Problem moving to 1st extension : '', a80)')
     &  rawfile
             call xwrite(errm,5)
             return
           endif
      
           call ftgkys ( rawlun, 'TELESCOP', mission, 
     &        errm, status )
           IF (mission(:lenact(mission)) .ne. 'OSO-8') then
              WRITE (errm,
     & '( '' Raw input file for the wrong mission : '', a8)')
     &  mission
              call xwrite(errm,5)
              WRITE (errm,
     & '( '' Raw input file is : '',a80)')
     &  rawfile
              call xwrite(errm,5)
              status=1
              return
           endif

           call ftgkys ( rawlun, 'DETNAM', raw_det, 
     &        errm, status )

           call ftclos(rawlun,status)
           call ftfiou(rawlun,status)
           if ( status .ne. 0 ) then
              WRITE (errm,
     & '( '' Problem closing raw data file : '', a80)')
     &  rawfile
             call xwrite(errm,5)
             return
           endif



C Check that detector selection corresponds to the detector 
C data in the raw input file

  
        
           IF (detnam(:lenact(detnam)) .ne. 
     & raw_det(:lenact(raw_det))) then
             WRITE (errm,
     &'('' Detector mismatch!! Raw data is for detector '', a8)')
     & raw_det
              CALL XAERROR(errm,1)
             WRITE (errm,
     & '( '' for raw data file : '',a80)')
     &  rawfile
             CALL XAERROR(errm,1)
              status=1
              return
           ENDIF
      enddo

     
C perform phase restriction ?
         CALL UCLGSB('ph_restriction',ph_restriction,status)
         IF (status.NE.0) THEN
            errm = 'Unable to get PH_RESTRICTION parameter'
            CALL XAERROR(errm,1)
            return
         ENDIF

         IF (ph_restriction) then
            CALL UCLGSD('period',period,status)
            IF (status.NE.0) THEN
               errm = 'Unable to get PERIOD parameter'
               CALL XAERROR(errm,1)
               return
            ENDIF

            CALL UCLGSD('ephemeris',ephemeris,status)
            IF (status.NE.0) THEN
               errm = 'Unable to get EPHEMERIS parameter'
               CALL XAERROR(errm,1)
               return
            ENDIF
           
 160        CALL UCLGSR('phase_st',phase_st,status)
            IF (status.NE.0) THEN
               errm = 'Unable to get PHASE_ST parameter'
               CALL XAERROR(errm,1)
               return
            ELSE
               IF (phase_st .LT. 0.0 .OR. phase_st .GT. 1.0) THEN
                  WRITE (errm,
     &'('' WARNING : Invalid PHASE_ST selection. Please re-enter...'')')
                  CALL XWRITE(errm,5)
                  goto 160
               ENDIF
            ENDIF
     
 170        CALL UCLGSR('phase_sp',phase_sp,status)
            IF (status.NE.0) THEN
               errm = 'Unable to get PHASE_SP parameter'
               CALL XAERROR(errm,1)
               return
            ELSE
               IF (phase_sp .LT. 0.0 .OR. phase_sp .GT. 1.0) THEN
                  WRITE (errm,
     &'('' WARNING : Invalid PHASE_SP selection. Please re-enter...'')')
                  CALL XWRITE(errm,5)
                  goto 170
               ENDIF
            ENDIF

            IF (phase_st .ge. phase_sp ) then
                WRITE (errm,
     &'('' WARNING : PHASE_SP should be > PHASE_ST !! Re-enter...'')')
                CALL XWRITE(errm,5)
                goto 170
            ENDIF

         ENDIF
     

c contaminating srces in FOV check ?
         CALL UCLGSB('fov_check',fov_check,status)
         IF (status.NE.0) THEN
            errm = 'Unable to get FOV_CHECK parameter'
            CALL XAERROR(errm,1)
           return
         ENDIF

         IF (fov_check) then 
          context = '  Input file list of contaminating srces.'
          CALL XWRITE(context,5)
          context = '  The list MUST be pipe-delimited (i.e.) : '
          CALL XWRITE(context,5)
          context = '         ra | dec | equinox '
          CALL XWRITE(context,5)
          context = '  ...ra coords may be in hh mm ss.s or degrees '
          CALL XWRITE(context,5)
          context = '  ...dec coords may be in dd mm ss.s or degrees '
          CALL XWRITE(context,5)
          context = '  ...for any equinox. '
          CALL XWRITE(context,5)
          context = '  A maximum of 10 sources may be listed '
          CALL XWRITE(context,5)


c input list file containing contaminating sources
          CALL UCLGST('cont_srcelist',cont_srcelist,status)
          IF (status.NE.0) THEN
             errm = 'Unable to get CONT_SRCELIST parameter'
             CALL XAERROR(errm,1)
            return
          ENDIF

C Determine if list file exists 
C Open list file and obtain source info
     
          listfile = cont_srcelist(:LENACT(cont_srcelist))
          INQUIRE (FILE=cont_srcelist,EXIST=isthere)
          IF ( .not.isthere) THEN
             WRITE (errm,
     & '( ''ERROR: Cant find list file : '',a80)')
     &  listfile
             CALL XAERROR(errm,1)
             status=1
             return
          ENDIf
        
          call ftgiou ( listlun, status )
          call openwr(listlun,listfile,'OLD',' ',' ',0,1,status )
          if ( status .ne. 0 ) then
             WRITE (errm,
     & '( '' Unable to open input list file : '',a80)')
     & listfile
             call xaerror ( errm, 1 )
             return
          endif

         
C put all srce info into memory
          nsrces=0
          do while (.true.)
           read ( listlun, '(A)',iostat=errstat, end = 200) line
             if (line .ne. ' ' ) then
                nsrces=nsrces+1
                inp_line(nsrces)=line
             endif
          enddo

 200      continue
          close(listlun)
          call ftfiou(listlun,status)

         
          if (nsrces .eq. 0) then
              WRITE (errm,
     & '( '' ERROR: Empty input list file : '',a80)')
     & listfile
             call xaerror ( errm, 1 )
             status=1
             return
          elseif (nsrces .gt. 10 ) then
             WRITE (errm,
     & '( '' WARNING : Too many sources ...using only the 1st 10 '')')
             call xaerror ( errm, 1 )
             nsrces=10
          endif

                   
          do 205 m=1,nsrces
                source=inp_line(m)
                len=LENACT(source)
                npipe=0
                pipe=.FALSE.
                do n=0,len
                     if (source(len-n:len-n) .eq.'|') then
                        pipe=.TRUE.
                        npipe=npipe+1
                        pipepos=len-n
                     endif                 
                     if (pipe .eqv. .TRUE.) then
                        if (npipe .eq. 1) then
                           eq(m)=source(pipepos+1:len)
                           pipepos1=pipepos
                           call rmvblk(eq(m))
                        endif
                        if (npipe .eq. 2) then
                           de(m)=source(pipepos+1:pipepos1-1)
                           ra(m)=source(1:pipepos-1)
                           call rmvblk(de(m))
                           call rmvblk(ra(m))
                        endif
                        pipe=.FALSE.
                    endif
                enddo
                
 205      continue
   
          IF (npipe .eq. 0) then
             WRITE (errm,
     & '( '' ERROR: Input listfile '',a80,'' is NOT pipe-delimited'')')
     & listfile
             call xaerror ( errm, 1 )
             status=1
             return
          endif

             
C perform precession of coords for any equinox NE 1950     

          do 210 m=1,nsrces
               read (eq(m),fmt='(I4)') equin
               equinox=dble(equin)
               rastrg=ra(m)
               decstrg=de(m)
               CALL PARSERA(rastrg,equin,radeg,status)
               IF ( status.NE.0 ) THEN
                  WRITE (errm,
     & '( '' parsera: Error traslating RA string : '')')
     & rastrg
                  CALL XAERROR(errm,1)
                  return
               ENDIF
               CALL PARSEDEC(decstrg,equin,decdeg,status)
               IF ( status.NE.0 ) THEN
                  WRITE (errm,
     & '( '' parsera: Error traslating DEC string : '')')
     & decstrg
                  CALL XAERROR(errm,1)
                  return
               ENDIF
               
               sra = radeg*dtr
               sdec = decdeg*dtr
           
              IF (equinox.NE.1950.0D0) then
                 CALL SLA_PRECES('FK4',equinox,1950.0D0,sra,sdec)
              ENDIF
C  returned values from sla routine are sra, sdec (in radians)
C ..so now convert back to decimal degrees
              
              cont_Dc50(m) = real(sdec/dtr)
              cont_Ra50(m)= real(sra/dtr)
               
              
        
 210      continue
         

 

         ENDIF
      


C Enter the start and end channels for creation of LC

 211  CALL UCLGSI('ch_start',ch_start,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get CH_START parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
            IF (ch_start .LT. 1 .OR. ch_start .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid CH_START selection. Please re-enter...'')')
               CALL XWRITE(errm,5)
               goto 211
            ENDIF
      ENDIF



 212   CALL UCLGSI('ch_end',ch_end,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get CH_END parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
            IF (ch_end .LT. 1 .OR. ch_end .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid CH_END selection. Please re-enter...'')')
               CALL XWRITE(errm,5)
               goto 212
            ENDIF
      ENDIF

      IF (ch_start .ge. ch_end ) then
                WRITE (errm,
     &'('' WARNING : CH_END should be > CH_START !! Re-enter...'')')
          CALL XWRITE(errm,5)
           goto 212
      ENDIF


C Ener no. of integration records 

      CALL UCLGSI('integ_time',integ_time,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get INTEG_TIME parameter'
            CALL XAERROR(errm,1)
            return
      ENDIF
      

C Enter desired intensity thresholds for PHA accumulation

      CALL UCLGSR('int_low',int_low,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get INT_LOW parameter'
            CALL XAERROR(errm,1)
            return
      ENDIF


 213   CALL UCLGSR('int_high',int_high,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get INT_HIGH parameter'
            CALL XAERROR(errm,1)
            return
      ENDIF
      IF (int_high .le . int_low ) THEN
          WRITE (errm,
     &'('' WARNING : INT_HIGH should be > INT_LOW !! Re-enter...'')')
          CALL XWRITE(errm,5)
           goto 213
      ENDIF
        
     

C Enter desired channel boundaries

 214  CALL UCLGSI('lchan_min',lchan_min,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get LCHAN_MIN parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
           IF (lchan_min .LT. 1 .OR. lchan_min .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid LCHAN_MIN selection. Re-enter...'')')
               CALL XWRITE(errm,5)
               goto 214
            ENDIF
      ENDIF



 215  CALL UCLGSI('lchan_max',lchan_max,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get LCHAN_MAX parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
           IF (lchan_max .LT. 1 .OR. lchan_max .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid LCHAN_MAX selection. Re-enter...'')')
               CALL XWRITE(errm,5)
               goto 215
            ENDIF
      ENDIF

      IF (lchan_max .le . lchan_min ) THEN
          WRITE (errm,
     &'('' WARNING : LCHAN_MAX should be > LCHAN_MIN !! Re-enter...'')')
          CALL XWRITE(errm,5)
           goto 215
      ENDIF




 216  CALL UCLGSI('hchan_min',hchan_min,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get HCHAN_MIN parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
           IF (hchan_min .LT. 1 .OR. hchan_min .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid HCHAN_MIN selection. Re-enter...'')')
               CALL XWRITE(errm,5)
               goto 216
            ENDIF
      ENDIF

      IF (lchan_max .ge . hchan_min ) THEN
          WRITE (errm,
     &'('' WARNING : HCHAN_MIN should be > LCHAN_MAX !! Re-enter...'')')
          CALL XWRITE(errm,5)
           goto 216
      ENDIF



 217   CALL UCLGSI('hchan_max',hchan_max,status)
      IF (status.NE.0) THEN
            errm = 'Unable to get HCHAN_MAX parameter'
            CALL XAERROR(errm,1)
            return
      ELSE
           IF (hchan_max .LT. 1 .OR. hchan_max .GT. 63) THEN
               WRITE (errm,
     &'('' WARNING : Invalid HCHAN_MAX selection. Re-enter...'')')
               CALL XWRITE(errm,5)
               goto 217
            ENDIF
      ENDIF

      IF (hchan_min .ge . hchan_max ) THEN
          WRITE (errm,
     &'('' WARNING : HCHAN_MAX should be > HCHAN_MIN !! Re-enter...'')')
          CALL XWRITE(errm,5)
           goto 217
      ENDIF




C >>>>>>. now do timing stuff


C Get the beginning and end times for the spectrum creation.  If the
C user enters a full year (e.g. 1978 instead of 78), accept the entry and
C correct it.  Accept ending year entries up to 1979 (e.g. although
C 1979 day 1 is after the end of the mission, accept this as an input),
C but do not permit a start year after the end of the mission.

 220  call uclgst('strtdate', stdate, status)
      if (status .ne. 0) then
         errm = 'Unable to get STRTDATE parameter'
         CALL XAERROR(errm,1)
         return
      endif


C Parse the date into day month and year
      call ftc2ii(stdate(1:2),id,status)
      call ftc2ii(stdate(4:5),imon,status)
      call ftc2ii(stdate(7:8),iy,status)
      if (status .ne. 0) goto 999
      if (lenact(stdate) .eq. 0) then
            errm= ' Illegal value! Re-enter the start date'
            call xwrite(errm,1)
            goto 220
      endif
     
c  check on format of input string
      if ((stdate(3:3) .ne. '/') .or. (stdate(6:6) .ne. '/')) then
           
            errm = ' Delimiter should be "/" ..but its OK'
            call xwrite(errm,1)

      endif 


c validate year
      beginyr=iy
      if (beginyr .gt. 99) beginyr = beginyr - 1900
      if ((beginyr .lt. 75) .or. (beginyr .gt. 78)) then
         errm =' Illegal value!  Mission spanned from 1975 to 1978'
         call xwrite(errm,1)
         errm = ' Please enter a valid starting year (1975 - 1978)'
         call xwrite(errm,1)
         go to 220
      endif
c validate month

      if (imon.lt.1 .or. imon.gt.12) then
           errm = 'Illegal value ! Please enter a valid month'
           call xwrite(errm,1)
           go to 220
      endif


c Validate day
      if (id.lt.1.or.id.gt.mtab(imon)) then
                errm = 'Illegal value! Please enter a valid day'
                call xwrite(errm,1)
                errm = 'for this month '
                call xwrite(errm,1)
                go to 220
      endif



  
C now calculate number of days from input date value
        ndays=0
        do i=1,imon
             if (i .gt. 1) then
                if (i .eq. 2 .and. beginyr .eq. 76 ) then
C we have a leap day for February
                   ndays=ndays+mtab(i-1)+1
                else
                   ndays=ndays+mtab(i-1)
                endif
             endif
        enddo
        beginday=ndays+id

        i=0     

       if ((beginday .lt. 176) .and. (beginyr .eq. 75)) then
         errm = 'Mission began on day 176 of 1975 '
         call xwrite(errm,1) 
         errm = ' setting start day to 176'
         call xwrite(errm,1)
         beginday = 176
         beginsec = 0.0
         go to 250
       else if ((beginday .gt. 273) .and. (beginyr .ge. 78)) then
         errm = 'Mission ended on day 273 of 1978'
         call xwrite(errm,1)

         write(errm,'(''which is earlier than '', i3, ''of 19'', i2)')
     +       beginday, beginyr
         call xwrite(errm,1)
         errm = ' Please enter a valid starting year and day '
         call xwrite(errm,1)
         go to 220
      endif
      beginday = beginday + (365 * (beginyr - 75))


 240  call uclgst('strtime', sttime, status)
      if (status .ne. 0) then
         errm = 'Unable to get STRTIME parameter'
         call xwrite(errm,1)
         return
      endif

            
C       Parse the time into hours, minutes, and seconds
        call ftc2ii(sttime(1:2),ih,status)
        call ftc2ii(sttime(4:5),im,status)
        call ftc2ii(sttime(7:8),is,status)
        if (status .ne. 0) goto 999

        if (lenact(sttime) .eq. 0) then
            errm= ' Illegal value! Re-enter the start time'
            call xwrite(errm,1)
            goto 240
        endif



c       check on format of input string
        if ((sttime(3:3) .ne. ':') .or. (sttime(6:6) .ne. ':')) then
            errm = ' Delimiter should be ":" ..but its OK'
            call xwrite(errm,1)
        endif 


C  check for valid inputs

        if (ih .gt. 23) then
            errm = ' WARNING: Invalid number of hours..re-enter'
         call xwrite(errm,1)

            goto 240
        elseif (im .gt. 59) then
            errm = ' WARNING: Invalid number of minutes..re-enter'
         call xwrite(errm,1)

            goto 240
        elseif (is .gt. 59) then
           errm = ' WARNING: Invalid number of seconds..re-enter'
         call xwrite(errm,1)

           goto 240
        endif

       beginsec=(ih*360)+(im*60)+is


       if ((beginsec .lt. 0.0) .or. (beginsec .gt. 86400.0)) then
         errm = 'Illegal value!  0 <= total seconds in day <= 86400 '
         call xwrite(errm,1)
         errm = ' Please enter a valid time of the day '
         call xwrite(errm,1)
         go to 240
      endif


       
 250  call uclgst('enddate', endate, status)
      if (status .ne. 0) then
         errm = 'Unable to get ENDDATE parameter'
         call xwrite(errm,1)
         return
      endif

C       Parse the date into day month and year
        call ftc2ii(endate(1:2),id,status)
        call ftc2ii(endate(4:5),imon,status)
        call ftc2ii(endate(7:8),iy,status)
        if (status .ne. 0) goto 999

       if (lenact(endate) .eq. 0) then
            errm= ' Illegal value! Re-enter the end date'
            call xwrite(errm,1)
            goto 250
        endif


c       check on format of input string
        if ((endate(3:3) .ne. '/') .or. (endate(6:6) .ne. '/')) then
            errm = ' Delimiter should be  "/" ..but its OK'
            call xwrite(errm,1)
        endif 


c validate year
      endyr=iy

       if (endyr .gt. 99) endyr = endyr - 1900
      if ((endyr .lt. 75) .or. (endyr .gt. 79)) then
         errm = 'Illegal value!  Mission spanned from 1975 to 1978'
         call xwrite(errm,1)
         errm = ' Please enter a valid ending year (1975 - 1979)'
         call xwrite(errm,1)
         go to 250
      endif

c validate month

        if (imon.lt.1 .or. imon.gt.12) then
           errm = 'Illegal value ! Please enter a valid month'
           call xwrite(errm,1)
           go to 250
        endif


c Validate day
        if (id.lt.1.or.id.gt.mtab(imon)) then
                errm = 'Illegal value! '
                call xwrite(errm,1)
                errm = 'Please enter a valid day for this month '
                call xwrite(errm,1)
                go to 250
        endif

C now calculate number of days from input date value
       
       ndays=0
        do i=1,imon
            if (i .gt. 1) then
               if (i .eq. 2 .and. beginyr .eq. 76 ) then
C we have a leap day for February
                   ndays=ndays+mtab(i-1)+1
                else
                   ndays=ndays+mtab(i-1)
                endif
            endif
        enddo
        endday=ndays+id

          i=0     

 

       if ((endday .lt. 176) .and. (endyr .eq. 1975)) then
         errm = 'Mission began on day 176 of 1975'
         call xwrite(errm,1)
         write(errm,'(''which is later than '', i3, ''of 19'', i2)')
     + endday, endyr
         call xwrite(errm,1)
         errm = ' Please enter a valid end year and day '
         call xwrite(errm,1)
         go to 250
      endif
      endday = endday + (365 * (endyr - 75))
      if (endday .gt. 1369) endday = 1369

      
 270  call uclgst('endtime', entime, status)
      if (status .ne. 0) then
         errm = 'Unable to get ENDTIME parameter'
         call xwrite(errm,1)
         return
      endif




C       Parse the time into hours, minutes, and seconds
        call ftc2ii(entime(1:2),ih,status)
        call ftc2ii(entime(4:5),im,status)
        call ftc2ii(entime(7:8),is,status)
        if (status .ne. 0) goto 999

        if (lenact(entime) .eq. 0) then
            errm= ' Illegal value! Re-enter the end time'
            call xwrite(errm,1)
            goto 270
        endif

c       check on format of input string
        if ((entime(3:3) .ne. ':') .or. (entime(6:6) .ne. ':')) then
            errm = ' Delimiter should be  ":" ..but its OK'
         call xwrite(errm,1)

        endif 


C  check for valid inputs

        if (ih .gt. 23) then
            errm = ' WARNING: Invalid number of hours..re-enter'
         call xwrite(errm,1)

            goto 270
        elseif (im .gt. 59) then
            errm = ' WARNING: Invalid number of minutes..re-enter'
         call xwrite(errm,1)

            goto 270
        elseif (is .gt. 59) then
           errm = ' WARNING: Invalid number of seconds..re-enter'
         call xwrite(errm,1)

           goto 270
        endif


       endsec=(ih*360)+(im*60)+is

      if ((endsec .lt. 0.0) .or. (endsec .gt. 86400.0)) then
         errm = 'Illegal value!  0 <= total seconds in day <= 86400 '
         call xwrite(errm,1)
         errm = ' Please enter a valid second of the day '
         call xwrite(errm,1)
         go to 270
      endif

      imsecs = INT(beginsec * 1000.0)
      imsece = INT(endsec * 1000.0)


c broad band bkgd level
      CALL UCLGSR('bg',bg,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get BG parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

c soft band bkgd level
      CALL UCLGSR('bgl',bgl,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get BGL parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

c hard band bkgd level
      CALL UCLGSR('bgh',bgh,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get BGH parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

c terminal chat
      CALL UCLGSI('tchat',tchat,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get TCHAT parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

c
c log chat
      CALL UCLGSI('lchat',lchat,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get LCHAT parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

C clobber
      CALL UCLGSB('clobber',clobber,status)
      IF (status.NE.0) THEN
         errm = 'Unable to get CLOBBER parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

  
 999  IF (status .ne. 0) then
         WRITE (errm,
     & '( '' ftc2cii : problem translating input string '')')
         CALL XAERROR(errm,1)
      ENDIF
           
      RETURN
      END








        


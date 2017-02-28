C********************************************************************
C TASK NAME: osorat
C
C FILE NAME: osorat.f
C
C DESCRIPTION: Generates a light curve from the OSORAT database for
C     isolated bright sources.
C
C AUTHOR/DATE: David Dawson, HSTX 12/96 (version 1.0.0)

C version 1.1.0 Lorraine Breedon 07/98 added equinox functionality\
C version 1.2.0 Lorraine Breedon 08/98 now input coords as string\
C                                     write EQUINOX, RADECSYS,
C                                     RA_OBJ, RA_DEC keywords to both
C                                     primary header and 1st binary
C                                     table of output FITS file\
C                                     recalculate average srce 
C                                     count and error and write to 
C                                     SRCCNT and SRCCNTE
C                                     keywords\
C LA 8/98 correct bugs open/close fits file add log and tchat
C
C version 1.3.0 Lorraine Breedon 02/99 added orbit corrn capability\

C
C********************************************************************

C********************************************************************
C SUBROUTINE: osorat
C
C ARGUMENTS: 
C     none
C
C PRIMARY LOCAL VARIABLES:
C     rastr      - ra of source (string)
C     decstr     - declination of source (string)
C     equinox  - equinox
C     detector - detector to produce light curve for
C     filelist - name of text file that contains the input rates fits files
C     arlima   - minimum area limit for A detector
C     arlimb   - minimum area for B detector
C     arlimc   - minimum area for C detector
C     abkg     - A Det. background
C     bbkg     - B Det. background
C     cbkg     - C Det. background
C     abkgerr  - A Det. background error
C     bbkgerr  - B Det. background error
C     cbkgerr  - C Det. background error
C     filter   - data quality filter
C     binspin  - true if lightcurve is to be binned once per spin
C     orbitcor - true if want orbital correction to output LC
C     orblist  - name of text file that contains the input orbit fits files
C
C********************************************************************

      subroutine osorat
      real arlima, arlimb, arlimc, abkg, bbkg, cbkg,
     *     abkgerr, bbkgerr, cbkgerr
      integer status, detector, filter
      integer*4 lenact, parse, ichat, tchat, lchat
      logical binspin, orbitcor,clobber
      double precision equinox
      character(7) VERSION
      PARAMETER (VERSION='1.3.0')

C
      character(40) taskname
      character(80) filelist, outfile,rastr,decstr, orblist
      character(80) log_file, istring, string, errm,message
      common /task/ taskname
C
      data istring,parse /' ',0/
C
      taskname = 'osorat'
      status = 0
      errm=' '
      message = ' '

C---  Get parameters from par file

          
      call grat (rastr, decstr, equinox, detector, filelist, 
     *     orbitcor, orblist,outfile, arlima,
     *     arlimb, arlimc, abkg, bbkg, cbkg, abkgerr, bbkgerr,
     *     cbkgerr, filter, binspin,  
     *     tchat, lchat, clobber, status)

             

C
      if (status.eq.0) then
         call xchaty(tchat,lchat)
         ichat=lchat
         string=taskname(1:lenact(taskname))
         call xwrite(string, ichat)
         log_file='+'//taskname(1:lenact(taskname))//'.log'
         if (lchat.ge.tchat) then
           call setlog (istring, parse, log_file, ' ')
         endif

C---  generate light curve
         call dowork (rastr, decstr, equinox, detector, filelist, 
     *               outfile, arlima, arlimb, arlimc, abkg, bbkg, cbkg, 
     *               abkgerr, bbkgerr, cbkgerr, filter, binspin,
     *               orbitcor, orblist,ichat, clobber,status)
         if(status.ne.0) then
            write(errm, '('' Status from dowork : '',I4)')status
            call xaerror(errm, 5)
         endif
      endif 
      if (status.ne.0) then
         write(errm, '('' Status after closing : '',I4)')status
         call xaerror(errm, 5)
      else
         message = ' '
         CALL XWRITE(message,5)
         WRITE (message,
     & '( '' **** OSORAT '',A7, '' finished ********* '')')
     & version
         CALL XWRITE(message,5)
      ENDIF

      end


C********************************************************************
C SUBROUTINE: grat
C
C DESCRIPTION: gets ra,dec, equinox parameters using xpi interface
C
C AUTHOR/DATE: David Dawson, HSTX 12/96
C              Lorraine Breedon 07/98 added equinox functionality\
C              Lorraine Breedon 02/99 added orbit corrn capability\

C 
C
C ARGUMENTS:
C     rastr       - ra of source to be read from par file (string)
C     decstr      - dec of source to be read from par file (string)
C     equinox  - equinox 
C     detector - 1, 2, or 3 for a, b, or c detector 
C     filelist - name of text file that contains the input fits files
C     outfile  - name of output rates file
C     arlima   - minimum area limit for A detector
C     arlimb   - minimum area for B detector
C     arlimc   - minimum area for C detector
C     abkg     - A Det. background
C     bbkg     - B Det. background
C     cbkg     - C Det. background
C     abkgerr  - A Det. background error
C     bbkgerr  - B Det. background error
C     cbkgerr  - C Det. background error
C     filter   - decides what data to filter out
C     binspin  - true if light curve to be binned once per spin
C     orbitcor - true if want orbital correction to output LC
C     orblist - Name of orbit correction file
C     tchat    - terminal chatter
C     lchat    - log chatter
C     status   - program status
C
C PRIMARY LOCAL VARIABLES:
C     context - error messages
C
C CALLED ROUTINES:
C 

      subroutine grat (rastr, decstr, equinox,detector, filelist, 
     *     orbitcor, orblist,outfile, arlima,
     *     arlimb, arlimc, abkg, bbkg, cbkg, abkgerr, bbkgerr, 
     *     cbkgerr, filter, binspin,tchat, 
     *     lchat, clobber, status)
C
      real arlima, arlimb, arlimc, abkg, bbkg, cbkg, 
     *     abkgerr, bbkgerr, cbkgerr
      integer detector, status, filter, tchat, lchat
      logical binspin,orbitcor,clobber
      character*(*) filelist, outfile,rastr,decstr, orblist
      character(80) context
      double precision equinox

      if (status .ne. 0) return
c
c get equinox
      call uclgsd ('equinox', equinox, status)
      context = 'could not get EQUINOX parameter'
      if ( status .ne. 0 ) goto 999
c
c get ra as string
      call uclgst ('ra', rastr, status)
      context = 'could not get RA parameter'
      if ( status .ne. 0 ) goto 999
c
c get dec as string 
      call uclgst ('dec', decstr, status)
      context = 'could not get DEC parameter'
      if ( status .ne. 0 ) goto 999
c
c get detector number 1, 2,3   
      call uclgsi ('detector', detector, status)
      context = 'could not get DETECTOR parameter'
      if ( status .ne. 0 ) goto 999
     

c get filelist
      call uclgst ( 'filelist', filelist, status )
      context = 'could not get FILELIST parameter'
      if ( status .ne. 0 ) goto 999
     
      
c to apply orbit correction ?
      call uclgsb ( 'orbitcor', orbitcor, status )
      context = 'could not get ORBITCOR parameter'
      if ( status .ne. 0 ) goto 999
      
      
      if (orbitcor .eqv. .true.) then
c get orbit corrction file
         call uclgst ( 'orblist', orblist, status )
         context = 'could not get ORBCFILE parameter'
         if ( status .ne. 0 ) goto 999
      endif
     

c get root filename output 
      call uclgst ( 'outfile', outfile, status )
      context = 'could not get OUTFILE parameter'
      if ( status .ne. 0 ) goto 999
       
c
c get area limit for a cm*2 
      call uclgsr ( 'arlima', arlima, status )
      context = 'could not get ARLIMA parameter'
      if ( status .ne. 0 ) goto 999
c
c get area limit for b 
      call uclgsr ( 'arlimb', arlimb, status )
      context = 'could not get ARLIMB parameter'
      if ( status .ne. 0 ) goto 999
c
c get area limit for c 
      call uclgsr ( 'arlimc', arlimc, status )
      context = 'could not get ARLIMC parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung for a count in 0.16 s bin
      call uclgsr ( 'abkg', abkg, status )
      context = 'could not get ABKG parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung for b count in 0.16 s bin
      call uclgsr ( 'bbkg', bbkg, status )
      context = 'could not get BBKG parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung for c count in 0.16 s bin
      call uclgsr ( 'cbkg', cbkg, status )
      context = 'could not get CBKG parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung error for a count in 0.16 s bin
      call uclgsr ( 'abkgerr', abkgerr, status )
      context = 'could not get ABKGERR parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung error for a count in 0.16 s bin
      call uclgsr ( 'bbkgerr', bbkgerr, status )
      context = 'could not get BBKGERR parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkground error for c (count in 0.16 s bin)
      call uclgsr ( 'cbkgerr', cbkgerr, status )
      context = 'could not get CBKGERR parameter'
      if ( status .ne. 0 ) goto 999
c
c get bagkgroung for a count in 0.16 s bin
      call uclgsi ( 'filter', filter, status )
      context = 'could not get FILTER parameter'
      if ( status .ne. 0 ) goto 999
c
c to bin the data over one spin period (good for a detector)
      call uclgsb ( 'binspin', binspin, status )
      context = 'could not get BINSPIN parameter'
      if ( status .ne. 0 ) goto 999

c

c
c terminal chat
      call uclgsi('tchat',tchat,status)
      context ='Unable to get TCHAT'
      if(status.ne.0) goto 999

c
c log chat
      call uclgsi('lchat',lchat,status)
      context= 'Unable to get LCHAT'
      if(status.ne.0) goto 999
c
c clobber
      call uclgsb('clobber',clobber,status)
      context= 'Unable to get CLOBBER'
      if(status.ne.0) goto 999
c
      if(status.eq.0) goto 1000
c
 999  continue
      call xaerror(context,5)     
1000  continue
      return
      end
         

 
C********************************************************************
C SUBROUTINE:
C     dowork
C
C DESCRIPTION:
C     read in data from orbit, house keeping, and osorates files and
C            generate light curve
C
C AUTHOR:
C     David Dawson, HSTX 12/96
C              Lorraine Breedon 07/98 added equinox functionality\
C              Lorraine Breedon 02/99 added orbit corrn capability\


C
C NOTES:
C
C ARGUMENTS:
C     rastr     - ra of source of interest
C     decstr    - dec of source of interest
C     equinox - equinox
C     detector - 1, 2, or 3 for a, b, or c detector
C     filelist - name of text file that contains the input fits files
C     outfile  - name of output rates file
C     arlima   - minimum area limit for A detector
C     arlimb   - minimum area for B detector
C     arlimc   - minimum area for C detector
C     abkg     - A Det. background
C     bbkg     - B Det. background
C     cbkg     - C Det. background
C     abkgerr  - A Det. background error
C     bbkgerr  - B Det. background error
C     cbkgerr  - C Det. background error
C     filter   - data quality filter
C     binspin  - true if light curve is to have 1 bin per spin
C     orbitcor - true if want orbital correction to output LC
C     orblist - Name of orbit correction file

C     ichat    - chatter 
C     status - program status
C
C PRIMARY LOCAL VARIABLES:
C     ra       - right ascension of source of interest
C     dec      - declination of source of interest
C     equinox  - equinox
C     i, j     - loop index
C     anyf
C     qstart   - flag used to record first good time for TSTART
C     status   - program status
C     datarow  - row index for the rates extension of the data file
C     nrowshk  - number of rows in the housekeeping binary table
C     ratlun   - logical unit number of input rates data file
C     orblun   - logical unit number of orbit file
C     outlun   - logical unit number of output rates file
C     listlun  - logical unit number of input file list
C     hdutype  - hdu type of extension
C     adata    - A detector counts from rates extension
C     bdata    - B detector counts from rates extension
C     cdata    - C detector counts from rates extension
C     data     - counts from rates extension for desired detector
C     max      - maximum array size for housekeeping data
C     blocksize- blocksize of opened fits files
C     arlim    - minimum effective area to accept a data record
C     rate     - calculated rate which is outputed
C     xn       - the max of rate and 1.0 used in error calculation
C     error    - error in rate which is outputed
C     srcvec   - unit vector for the source ra and dec
C     efarea   - array of effective areas for all the data rows 
C                corresponding to one housekeeping row
C     timedat  - time value from the rates extension
C     tstart   - time of start of observation
C     tstop    - time of end of observation
C     timehk   - array of time values from the housekeeping extension
C     refmip   - reference mip(mission instrument pulse) time
C     sunra    - array of sun RA values from the housekeeping extension
C     sundec   - array of sun Dec values from the hk extension
C     spinra   - array of spin RA values from the hk extension
C     spindec  - array of spin Dec values from the hk extension
C     period   - spin period of OSO-8 in ms
C     elemnum  - number of datarows that a hk row applies to
C     miptime  - difference in ms from beginnin of hk record and the 
C                time that OSO-8 spins pass the sun
C     lastdatarow - last row that the current period value is valid
C     clean    - true if data is clean
C     adet     - true if A detector is not occulted
C     bdet     - true if B detector is not occulted
C     cdet     - true if C detector is not occulted
C     ttype    - type of output fits columns
C     tform    - forms of output fits columns
C     tunit    - units for output fits columns
C     stdatestr- date string for start date
C     sttimestr- time string for start time
C     enddatestr- date string for end date
C     endtimestr- string containing end time
C     outrow   - index of output fits table row
C     detname  - detector name
C     ratetot  - sum of rates for current spin if binspin is true
C     errtot   - sum of errors for current
C     cnttot   - counter to sum number of data records in 1 spin
C     avarea   - the average effective area for the current spin
C
C CALLED ROUTINES:
C     convec   - calculates the 3 element unit vector for ra and dec
C     ftgiou   - get new logical unit number
C     ftopen   - open fits file
C     fcerr    - echo message
C     ftmahd   - move to new absolute extension
C     ftgkyj   - get integer keyword
C     ftgcvd   - get a double from a binary array
C     ftgcvj   - get a integer from a binary array
C     ftgcve   - get a real from a binary array
C     ftgcvi   - get an integer*2 from a binary array
C     ftgcl    - get a logical from a binary array
C     ftinit   - initialize a new fits file
C     ftphpr   - write required keywords to primary header
C     ftibin   - write required keywords for a binary table extension
C     calcrpm  - calculate OSO-8 spin period by averaging over interval
C     aarea    - calculate effective area for one housekeeping record
C     ftpcld   - write a real*8 to a binary table
C     ftpcle   - write a real*4 to a binary table
C     ftmkyj   - modify an integer keyword 
C     ftclos   - close a fits file
C********************************************************************

      subroutine dowork (rastr, decstr, equinox,detector, filelist, 
     *     outfile, arlima,
     *     arlimb, arlimc, abkg, bbkg, cbkg, abkgerr, bbkgerr, 
     *     cbkgerr, filter, binspin, orbitcor, orblist,
     *     ichat, clobber,status)




C     Input variables
      real*4 arlima, arlimb, arlimc, abkg, bbkg, cbkg,
     *     abkgerr, bbkgerr, cbkgerr
      integer detector, filter, ichat
      logical binspin,orbitcor,clobber
      character*(*) filelist,rastr,decstr,outfile,orblist
      character(255) string
      double precision equinox
   

 

C---  General Variables

      logical anyf, qstart, baddata
      integer i, j,ii
      integer status, ierr
      integer datarow
      integer nrowshk
      integer ratlun, orblun, outlun, listlun
      integer hdutype
      integer adata, bdata, cdata, data
      integer max,max2
      integer outfile_num
      parameter ( max = 10000 )
      parameter (max2=1000000)
      integer blocksize
      real*8 ra,dec
      real*4 bkg(3), det_area,bkgerror(3)
      real ratetot, errtot, avarea
      double precision firstmip, secondmip,rarad,decrad,radeg,decdeg
      real src_radeg,src_decdeg
      character(85) outfile_full
      character(2) outfile_num_str
      integer cnttot,equi
      real op(8), c_ang



C     Angles beteen which C detector was obstructed
c      data op/84.,112.,169.,196.,270.,298.,354.,23./                    
      data op/23.,84.,112.,169.,196.,270.,298.,354./

C     These values are now gotten from the parameter file.
c      data bkg /3.4, 0.16, 1.6/
c      data bkgerror /0.12, 0.008, 0.08/

      real arlim(3)
c      data arlim /10.0, 5.0, 50.0/

      double precision twopi
      parameter ( twopi = 6.2831853d0)
      real*4 rate, xn, error
      real*8 srcvec(3)
      double precision timedat, tstart, efarea(75)
     
C---  housekeeping data
      double precision timehk(MAX), previous_time, spinra(MAX),
     &     spindec(MAX),sunra(MAX), sundec(MAX)
      real  ssunra, ssundec,sspinra, sspindec,
     *     period
      integer elemnum(MAX), miptime(MAX), lastdatarow
      integer*2 clean(MAX), adet(MAX), bdet(MAX), cdet(MAX)

C---  output fits file variables
      integer outrow
      real*4 srccnt_total, srccnte_total

      character * 160 context
      character * 80 ratename, orbname

C source cnt correction variables
      character(30) sjunk
      character(85) n_outfile(20),outfill
      integer*4 ijunk,nax2val,rate_col
      real*4 rate_val,snewerr_correct,smean_srcecnt
      real*8 srcecnt_total,rate_value(max2),mean_srcecnt,newerr(max2)
      real*8 newerr_total,newerr_correct
      logical ljunk,there

c orbit correction variables
      integer m, k, lenact, finalno, cc,numrow
      integer nfiles, errstat, norbfiles,cnter,orbunit
      character(80) ratefile(max), orbfile(max), ratef_to_use(max),
     &         file,orbf_to_use(max), ratesf, orbf, orbitfile
      real*4 lc_ra,lc_dec
      character(70) comment(3)
      logical isthere, finishcorr, match
      double precision stime,rates_tstart,rates_tstop,
     &    orb_tstart,orb_tstop,tstart_min,tstart_max,tstop_min,
     &    tstop_max



      if (status .ne. 0) return

c  Initialize to avoid warning
      data = 0
      ratetot = 0
      errtot = 0
      cnttot = 0
      finalno = 0
c  --

C---  Initialize variables
      
      outrow = 0
      outlun = 0
      orbunit=0
      ratlun=0
      listlun=0
      orblun=0
      finishcorr=.FALSE.
      previous_time = 0
      outfile_num = 0
      srccnt_total = 0.0
      srccnte_total = 0.0
      call ftgiou ( ratlun, status )
      qstart = .true.
      arlim(1) = arlima
      arlim(2) = arlimb
      arlim(3) = arlimc
      bkg(1) = abkg
      bkg(2) = bbkg
      bkg(3) = cbkg
      bkgerror(1) = abkgerr
      bkgerror(2) = bbkgerr
      bkgerror(3) = cbkgerr
      timedat = 0.0
      if ( detector .eq. 1) then
         det_area = 263.0
      elseif ( detector .eq. 2 ) then
         det_area = 36.7
      elseif ( detector .eq. 3 ) then
         det_area = 237.0
      else
         context = 'Error: invalid detector value.'
         call xaerror( context, 5)
         goto 9999
      endif


C Convert Right Ascension and Declination inputs into B1950.0 coordinates
C in degrees.
 

 
      equi = INT(equinox)
      CALL PARSERA(rastr,equi,radeg,status)
      IF ( status.NE.0 ) THEN
         context = '  parsera: Error traslating RA string'
         CALL xaerror(context, 5)
         GOTO 9999
      ENDIF
      CALL PARSEDEC(decstr,equi,decdeg,status)
      IF ( status.NE.0 ) THEN
         context = '  parsdec: Error traslate DEC string'
         CALL xaerror(context, 5)
         GOTO 9999
      ENDIF
 
   
C precess coords into 1950 epoch if equinox not 1950
      ra= radeg / 360.0d0 * twopi
      dec = decdeg / 360.0d0 * twopi

      if (equinox .ne. 1950.0D0) then
         rarad=ra
         decrad=dec
         call sla_PRECES('FK4', equinox, 1950.0D0, rarad, decrad)
         ra = rarad
         dec = decrad
         radeg = ra *360.0 / twopi
         decdeg = dec * 360.0 /twopi
      endif       

      src_radeg = real(radeg)
      src_decdeg = real(decdeg)

     
C---  convert source position into a cartesian vector
      call CONVEC(ra, dec, srcvec)

C---  Open new fits lightcurve file

c      write (outfile_num_str,'(i2)') outfile_num
c      outfile_full = outfile // outfile_num_str
c      call open_fits( outlun, outfile_full, detector, det_area, status )

c      if ( status.ne.0 ) then
c         goto 9999
c      endif

       
C---  Open text file with list of input rates  fits files
      call ftgiou ( listlun, status )
      call openwr(listlun,filelist,'OLD',' ',' ',0,1,status )
c      call faopen ( listlun, filelist, 1, 0, status )
      if ( status .ne. 0 ) then
         WRITE (context,
     & '( '' Unable to open input data file list : '', a80)')
     & filelist
         call xaerror ( context, 5 )
         goto 9999
      endif

       

C put all rates files into memory
      nfiles=0
      do while (.true.)
         read ( listlun, '(A)',iostat=errstat, end = 10) file
              ratename=file(:lenact(file))
              INQUIRE (FILE=ratename,EXIST=isthere)
              IF ( .not.isthere) THEN
                 WRITE (context,
     & '( ''ERROR: Cant find raw data file : '', a80)')
     &  ratename
                 CALL XAERROR(context,1)
                 goto 9999
              ELSE
                  nfiles=nfiles+1
                  ratefile(nfiles)=ratename
              ENDIF
      enddo

 10   continue
       
      IF (nfiles .eq. 0) then
         WRITE (context,
     & '( '' ERROR : input data file list empty !! '', a80)')
     & filelist
         call xaerror ( context, 5 )
         goto 9999
      endif


       

C---  Open text file with list of input orbit fits files (if requested)
      if (orbitcor .eqv. .true. ) then
         call ftgiou ( orblun, status )
         call openwr(orblun,orblist,'OLD',' ',' ',0,1,status )
         if ( status .ne. 0 ) then
             WRITE (context,
     & '( '' Unable to open input orbit file list : '', a80)')
     & orblist
   
            call xaerror ( context, 5 )
            goto 9999
         endif

       
C put all orbit files into memory
         norbfiles=0
         do while (.true.)
            read ( orblun, '(A)',iostat=errstat, end = 20 ) file
              orbname=file(:lenact(file))
              INQUIRE (FILE=orbname,EXIST=isthere)
              IF ( .not.isthere) THEN
                 WRITE (context,
     & '( ''ERROR: Cant find orbit file : '', a80)')
     &  orbname
                 CALL XAERROR(context,1)
                 goto 9999
              ELSE
                 norbfiles=norbfiles+1
                 orbfile(norbfiles)=orbname
              ENDIF
         enddo

 
 20      continue

         IF (norbfiles .eq. 0) then
            WRITE (context,
     & '( '' ERROR : input orbit file list empty !!  '', a80)')
     & orblist
            call xaerror ( context, 5 )
            goto 9999
         endif




C warn if the number of rates and orbit files do not match
         if (nfiles .ne. norbfiles) then
            WRITE (context,
     & '( '' No. data files does not equal No. orbit files !!'')')
            call xwrite(context,5)
         endif

      
C check that there is an orbit file for each rates file in the list
         finalno=0
         do m=1,nfiles
               match = .FALSE.
               ratesf=ratefile(m)
               call ftgiou(ratlun,status)
               call ftopen ( ratlun, ratesf, 0, blocksize,status )
               if ( status .ne. 0 ) then
                   WRITE (context,
     & '( '' Unable to open rates file : '', a80)')
     &  ratesf
                   call xwrite(context,5)
                   goto 9999
              endif
         
              call ftmahd ( ratlun, 2, hdutype, status )
              if ( status .ne. 0 ) then
                WRITE (context,

     & '( '' Problem moving to 1st extension : '', a80)')
     &  ratesf
                call xwrite(context,5)
                goto 9999
              endif
             
              call ftgkyd ( ratlun, 'TSTART', rates_tstart, 
     &        context, status )
              call ftgkyd ( ratlun, 'TSTOP', rates_tstop,
     &        context, status )

              do k=1,norbfiles
                    orbf=orbfile(k)
                     call ftgiou(orbunit,status)
                     call ftopen ( orbunit, orbf, 0, blocksize,
     &                    status )
                    if ( status .ne. 0 ) then
                       WRITE (context,
     & '( '' Unable to open orbit file : '', a80)')
     &  orbf
                       call xwrite(context,5)
                       goto 9999
                    endif
         
                    call ftmahd (orbunit, 2, hdutype, status )
                    if ( status .ne. 0 ) then
                       WRITE (context,
     & '( '' Problem moving to 1st extension : '', a80)')
     &  orbf
                       call xwrite(context,5)
                       goto 9999
                    endif
             
                    call ftgkyd (orbunit, 'TSTART', orb_tstart, 
     &                     context, status )
                    call ftgkyd (orbunit, 'TSTOP', orb_tstop,
     &                     context, status )
                    tstart_min = orb_tstart - 7200.00
                    tstart_max= orb_tstart + 7200.00
                    tstop_min = orb_tstop - 7200.00
                    tstop_max= orb_tstop + 7200.00
                    if ((rates_tstart .ge. tstart_min) 
     &                 .and. (rates_tstart .le. tstart_max)
     &                 .and. (rates_tstop .ge. tstop_min)
     &                 .and. (rates_tstop.le. tstop_max)) then
C there is an orbit file for this rates file
                           finalno=finalno+1
                           ratef_to_use(finalno)=ratesf
                           orbf_to_use(finalno)=orbf
                           match = .TRUE.
                    endif

                   call ftclos(orbunit,status)
                   call ftfiou(orbunit,status)
              enddo
              if (match .eqv. .FALSE.) then
                  WRITE (context,
     & '( '' WARNING: no orbit data for rates file : '', a80)')
     & ratesf
                  call xwrite(context,5)
                  WRITE (context,
     & '( '' .. Will not use this rates file in LC creation .. '')')
                  call xwrite(context,5)
              endif

              call ftclos(ratlun,status)
              call ftfiou(ratlun,status)
         enddo
      endif

    

C---  Loop through each input fits rates file
      if (orbitcor .eqv. .true. ) then
         cnter=finalno
      else
         cnter=nfiles
      endif
      do m=1,cnter
         if (orbitcor .eqv. .true. ) then
            ratename = ratef_to_use(m)
         else
            ratename = ratefile(m)
         endif
           
     
C---     Open input, rates file
         call ftgiou(ratlun,status)
         call ftopen ( ratlun, ratename, 0, blocksize,status )
         if ( status .ne. 0 ) then
                 WRITE (context,
     & '( '' Unable to open rates file : '', a80)')
     &  ratename
            call xaerror (context, 5)
            goto 9999
         endif
         
C---     Move to first extension of rates file which contains the 
C---          hk data
         call ftmahd ( ratlun, 2, hdutype, status )
         if ( status .ne. 0 ) then
            context = ' Unable to move to first extension of rates'
            call xaerror(context, 5)
            goto 9999
         endif
         
C---     Get the number of rows of hk
         call ftgkyj ( ratlun, 'NAXIS2', nrowshk, context, status )

C---     Check to see if housekeeping arrays are big enough for this file
         if ( nrowshk .gt. MAX) then
            context = 'Too much housekeeping data.'
            call xaerror(context,5)
            goto 9999
         endif

C---     Read in required hk data
         do i = 1, nrowshk
            call FTGCVD ( ratlun, 1, i, 1, 1, 0.0d0, timehk(i), anyf, 
     *           status )
            call FTGCVE ( ratlun, 4, i, 1, 1, 0.0, ssunra, anyf, 
     *           status )
            call FTGCVE ( ratlun, 5, i, 1, 1, 0.0, ssundec, anyf, 
     *           status )
            call FTGCVE ( ratlun, 6, i, 1, 1, 0.0, sspinra, anyf, 
     *           status )
            call FTGCVE ( ratlun, 7, i, 1, 1, 0.0, sspindec, anyf, 
     *           status )
            call FTGCVJ ( ratlun, 9, i, 1, 1, 0, elemnum(i), anyf, 
     *           status )
            call FTGCVJ ( ratlun, 11, i, 1, 1, 0, miptime(i), anyf, 
     *           status )
            call FTGCVI ( ratlun, 12, i, 1, 1, 0, clean(i), anyf, 
     *           status )
            call FTGCVI ( ratlun, 13, i, 1, 1, 0, adet(i), anyf, 
     *           status )
            call FTGCVI ( ratlun, 14, i, 1, 1, 0, bdet(i), anyf, 
     *           status )
            call FTGCVI ( ratlun, 15, i, 1, 1, 0, cdet(i), anyf,
     *           status )
            sunra(i)=DBLE(ssunra)
            sundec(i)=DBLE(ssundec)
            spinra(i)=DBLE(sspinra)
            spindec(i)=DBLE(sspindec)
         enddo
         if ( status. ne. 0 ) then
            context = 'Unable to read housekeeping data'
            call xaerror ( context, 5 )
            goto 9999
         endif
         
C---     Move to rates extension of the data fits file
         call ftmahd ( ratlun, 3, hdutype, status)
         if ( status .ne. 0 ) then
            context = 'Unable to move to rates data extension'
            call xaerror ( context, 5)
            goto 9999
         endif

C---     Close this fits output file and open new file if there is a 
C---     time gap more than a week.
         if ( timehk(1) - previous_time .gt. 604800.0 ) then
            if (outlun .ne. 0) 
     *           call close_fits( outlun, outfile_full,outfile_num,
     *                           outrow, tstart, timedat,
     *           srccnt_total, srccnte_total, binspin,status )
            outrow = 0
            srccnt_total =0.0
            srccnte_total = 0.0
            
            qstart = .true.
            outfile_num = outfile_num + 1
            write (outfile_num_str,'(i2)') outfile_num
            outfile_full = outfile // '_' // outfile_num_str
            if(outfile_num.gt.1) then
              context= ' Found data gap larger than a week'
              call xwrite(context, 10)
              context= ' Open a new lightcurve file'
              call xwrite(context, 10)
              write(context,'('' Lightcurve number '',i2)')outfile_num
              call xwrite(context,10)
            endif
            call rmvblk (outfile_full)
            n_outfile(outfile_num) = outfile_full
            call open_fits( outlun, outfile_full, detector, det_area,
     *           src_radeg,src_decdeg, clobber, status )
            if ( status .ne. 0 ) then
               context = ' Error opening new fits file.'
               call xaerror( context, 5 )
               goto 9999
            endif
         endif
         previous_time = timehk(1)
 
C---     Main loop for calculating light curve
         lastdatarow = 0
         datarow = 0
C---     Loop through all hk data
         do i = 1, nrowshk


C---        Throw out record if datarow is not within reason. 
C---        Could be an overflow from previous record.
            if (elemnum(i).lt.48 .or. elemnum(i).gt.72) then
               datarow = datarow + elemnum(i)
               goto 1000
            endif

C---        Calculate area for this hk record
            call calcarea ( detector, i, timehk, miptime,
     *           clean, adet, bdet(i), cdet(i), lastdatarow,
     *           period, srcvec, spinra(i), spindec(i), sunra(i),
     *           sundec(i), efarea, elemnum(i), firstmip, 
     *           secondmip, status)
            
C---        Loop through all data pertaining to this hk row
            do j = 1, elemnum(i)
               datarow = datarow + 1

               if (efarea(j) .gt. DBLE(arlim(detector))) then

C---              Skip row for the C detector if it is between certain
C                 angles where it is occulted.
                  if ( detector .eq. 3 ) then
                     c_ang = 360. * ( float(j) - 0.5 ) / 
     *                    float(elemnum(i))
                     if ( c_ang.gt.op(1) .and. c_ang.lt.op(2) ) 
     *                    goto 1500
                     if ( c_ang.gt.op(3) .and. c_ang.lt.op(4) ) 
     *                    goto 1500
                     if ( c_ang.gt.op(5) .and. c_ang.lt.op(6) ) 
     *                    goto 1500
                     if ( c_ang.gt.op(7) .and. c_ang.lt.op(8) ) 
     *                    goto 1500
                  endif

                  call FTGCVD (ratlun,1,datarow,1,1,0.0d0,timedat, 
     *                 anyf, status )
                  call FTGCVJ ( ratlun, 2, datarow, 1, 1, 0, adata, 
     *                 anyf, status )
                  call FTGCVJ ( ratlun, 3, datarow, 1, 1, 0, bdata, 
     *                 anyf, status )
                  call FTGCVJ ( ratlun, 4, datarow, 1, 1, 0, cdata, 
     *                 anyf, status )
                  if ( status .ne. 0 ) then
                     context = 'Unable to read input fits file.'
                     call xaerror ( context, 5 )
                     goto 9999
                  endif

C---              Use data for appropriate detector
                  if (detector.eq.1) then
                     data = adata
                  elseif (detector.eq.2) then
                     data = bdata
                  elseif (detector.eq.3) then
                     data = cdata
                  endif

C---              Filter data according to filter flag
                  baddata = .false.
C---              Data is bad if any detector contains bad data
                  if (filter.eq.1 .or. filter.eq.2) then
                     if (adata.gt.254 .or. bdata.gt.254 .or. 
     *                    cdata.gt.254) baddata = .true.
                     if (adata.lt.0 .or. bdata.lt.0 .or. cdata.lt.0)
     *                    baddata = .true.
C---              Data is bad only if interested detector contains bad
C---              data.
                  elseif (filter.eq.3 .or. filter.eq.4) then
                     if (data.gt.254. or. data.lt.0) baddata = .true.
                  endif

C---              Skip rest of data associated with this house keeping row
                  if ((filter.eq.1 .or. filter.eq.3) .and. baddata) then
                     datarow = datarow + elemnum(i) - j
                     write (context,*) 
     *                    'Bad data flag, rest of hk record skipped.', 
     *                    timedat
                     call xwrite (context, 15)
                     goto 1000
                  endif

C---              Skip only the actual 160ms bin containing bad data
                  if ((filter.eq.2 .or. filter.eq.4) .and. baddata) then
                     write (context, *) 
     *                    'Bad data flag, one bin skipped', timedat
                     call xwrite (context, 15)
                     goto 1500
                  endif

C---              Throw out rest of record if data not reasonable
C---              Taken out because we now use a filter parameter
C---              to decide what to do in the event of an overflow.
c                  if (adata.gt.254 .or. bdata.gt.254 .or. 
c     *                 cdata.gt.254) then
c                     datarow = datarow + elemnum(i) - j
c                     write (context,*) 'Overflow flag', timedat, rate
c                     call xwrite(context, 15)
c                     write (context,*) adata, bdata, cdata
c                     call xwrite(context, 15)
c                     goto 1000
c                  endif
c                  if (adata.lt.0 .or. bdata.lt.0 .or. cdata.lt.0) then
c                     datarow = datarow + elemnum(i) - j
c                     write (context,*) 'Negative data', timedat, rate
c                     call xwrite(context, 15)
c                     goto 1000
c                  endif


C---              Store first good time value for TSTART keyword
                  if ( qstart ) then
                     tstart = timedat
                     qstart = .false.
                  endif

                  
C---              Calculate rate and error
                  write(string,12) float(data),bkg(detector),efarea(j)
 12               format("data,bkg(detector),efarea(j)",1x,F5.1,1x,F8.4,
     &                 1x, F19.9)
                  call xwrite(string,55)
                  rate = (float(data) - bkg(detector))/SNGL(efarea(j))/
     &                 0.16
                  xn = amax1 (float(data), 1.0)
                  error = sqrt( xn + bkgerror(detector) * 
     *                 bkgerror(detector) ) / SNGL(efarea(j)) /  0.16

                  rate = rate * det_area
                  error = error * det_area
                  srccnt_total = srccnt_total + rate
                  srccnte_total = srccnte_total + error

                  if ( .not. binspin ) then

C---                 Write time, rate, and error to output file
                     outrow = outrow + 1
                     call FTPCLD ( outlun, 1, outrow, 1, 1, 
     *                    timedat + 0.08d0, status )
                     call FTPCLE ( outlun, 2, outrow, 1, 1, rate, 
     *                    status )
                     call FTPCLE ( outlun, 3, outrow, 1, 1, error, 
     *                    status )
                     call FTPCLE(outlun,4,outrow,1,1,SNGL(efarea(j)),
     *                    status )
                     if ( status .ne. 0 ) then
                        context = 'Error writing to output file.'
                        call xaerror ( context, 5 )
                        goto 9999
                     endif
                  else
C---                 Compute running totals for one bin per spin
                     ratetot = ratetot + rate
                     errtot = sqrt (errtot * errtot + error * error)
                     cnttot = cnttot + 1
C! efarea(i) was error. SS12/31
                     avarea = SNGL(efarea(j)) + avarea  
                  endif
               endif
 1500          continue
            enddo
 1000       continue
            
            


C---        Write output file for 1 bin per spin light curves
            if ( binspin.and. cnttot.gt.0 ) then
               timedat = timehk(i) + dble(period)/1000.d0 / 2.0d0
               avarea = avarea / cnttot
               rate = ratetot / cnttot
               error = errtot / cnttot
               outrow = outrow + 1
               call FTPCLD ( outlun, 1, outrow, 1, 1, 
     *              timedat, status )
               call FTPCLE ( outlun, 2, outrow, 1, 1, rate, 
     *              status )
               call FTPCLE ( outlun, 3, outrow, 1, 1, error, 
     *              status )
               call FTPCLE ( outlun, 4, outrow, 1, 1, avarea, 
     *              status )
               if ( status .ne. 0 ) then
                  context = 'Error writing to output file.'
                  call xaerror ( context, 5)
                  goto 9999
               endif
            endif   
         enddo
         call ftclos (ratlun, status )
         call ftfiou(ratlun,status)
      
      enddo
         

        
      



 9799 continue

C---  Report error for filelist read.
c      context = 'Error reading file list.'
c      call xaerror ( context, 5)
      
 9899 continue
      
  
      call close_fits( outlun,outfile_full,outfile_num,
     *     outrow, tstart, timedat, 
     *     srccnt_total, srccnte_total,binspin, status )
   
       
C ---  ** LB ****
C --- Re-open the new fits lcs file to correct the
C     srccnt_total, srccnte_total calculations AND do orbit corrections

      context=' '
      call xwrite(context,5)
 

      status=0
      do i=1,max2
            rate_value(i)=0.0
            newerr(i)=0.0
      enddo
      if (outfile_num .ge.1) then

        do 9900 ii=1,outfile_num
            finishcorr=.FALSE.
            nax2val=0
            rate_val=0.0
            rate_col=0
            
           outfill=n_outfile(ii)
           outfile_full=outfill(:LENACT(outfill))
            WRITE (context,
     & '( '' Calculating srce count corrns for lightcurve : '', a80)')
     & outfile_full
            call xwrite(context,5)
           

           inquire (file=outfile_full, exist=there)
           if (there .neqv. .true.) then
              WRITE (context,
     & '( '' Error : Cant find Lightcurve : '', a80)')
     & outfile_full
              call xaerror ( context, 5 )
              goto 9900
           else
              call ftgiou(outlun,status)
              call ftopen(outlun, outfile_full, 1, ijunk, status)
              if(status.ne.0)then
                WRITE (context,
     & '( '' Error re-opening '', a80)')
     & outfile_full
                call xaerror ( context, 5 )
                goto 9997
              endif

C Move to the first extension
              call ftmahd(outlun, 2, ijunk, status)
              if(status.ne.0)then
                WRITE (context,
     & '( '' Error moving to 1st ext of'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
              endif

        
C Find out how many rows the FITS lightcurve contains
              call ftgkyj(outlun, 'NAXIS2', nax2val, sjunk, status)
              if(nax2val.eq.0)then
               WRITE (context,
     & '( '' Error FITS light curve empty :'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
              endif


               

C find the RA and DEC
             call FTGKYE (outlun,'RA_OBJ',lc_ra,sjunk,status)
             if(status.ne.0)then
                WRITE (context,
     & '( '' Error getting RA_OBJ value  for'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
             endif

             call FTGKYE (outlun,'DEC_OBJ',lc_dec,sjunk,status)
             if(status.ne.0)then
                WRITE (context,
     & '( '' Error getting DEC_OBJ value for'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
             endif


C determine the start time for the file
             call FTGCVD (outlun,1,1,1,1,0.0d0,stime,anyf,status)
             if(status.ne.0)then
                WRITE (context,
     & '( '' Error getting STIME for'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
             endif

           

C Get the column number of the column to be read
             call ftgcno(outlun,.true.,'RATE',rate_col,status)
             if(status.ne.0)then
                WRITE (context,
     & '( '' Error getting RATE column no. for'', a80)')
     & outfile_full
                call xaerror ( context, 5)
                goto 9996
             endif

C Read each row and calculate error
             srcecnt_total=0.0d0
             do i=1,nax2val
                 rate_value(i)=0.0d0
                 status = 0

                 call ftgcve(outlun,rate_col,i,1,1,0.0,rate_val,
     *                      ljunk,status)
                 if (status .ne. 0) then
                    WRITE (context,
     & '( '' Error getting RATE value for'', a80)')
     & outfile_full
                    call xaerror ( context, 5)
                     goto 9996
                 endif
                 
                 rate_value(i)=DBLE(rate_val)
                 srcecnt_total=srcecnt_total+rate_value(i)
                 
             enddo

             mean_srcecnt=srcecnt_total/DBLE(nax2val)
             newerr_total=0.0d0

             do i=1,nax2val
                  newerr(i)=0.0d0
                  newerr(i) = (rate_value(i) - mean_srcecnt)*
     &                 (rate_value(i) - mean_srcecnt)
                 newerr_total=newerr_total+newerr(i)
             
             enddo

             newerr_correct = dsqrt(newerr_total/(DBLE(nax2val)*
     &            (DBLE(nax2val)-1.0d0)))
        
C write new value of SRCCNT and SRCCNTE to header
             smean_srcecnt=SNGL(mean_srcecnt)
             snewerr_correct=SNGL(newerr_correct)

             call FTMKYE(outlun,'SRCCNT',smean_srcecnt,10,'&',status)
             call FTMKYE(outlun,'SRCCNTE',snewerr_correct,10,'&',status)

C Now perform orbit corrections
             if (orbitcor .eqv. .true.) then
                numrow=1
                do k=1,finalno
                     orbitfile=orbf_to_use(k)
                     call get_times(outlun,nax2val,lc_ra,lc_dec,stime,
     &                     orbitfile,orbunit,outfile_full,finishcorr,
     &                     numrow,status)
         

                     if (status .ne. 0) then
C close current orbit file and current light curve
                        call ftclos(orbunit,status)
                        call ftfiou(orbunit,status)
                        goto 9996
                     endif
                     if (finishcorr) then
                        call ftclos(orbunit,status)
                        call ftfiou(orbunit,status)
                        goto 9995
                     endif
               enddo
             endif
                
C close FITS file again

 9995        if (finishcorr .and. status .eq. 0) then
                
               comment(1) = ' OSORAT summary :'
               comment(2) = ' light-curve corrected for orbital '
               comment(3) = ' variation of satellite '
               do cc = 1,3
                       call ftpcom(outlun,comment(cc),status)
                       if (status .ne. 0) then
                          WRITE (context,
     & '( '' Warning- problems writing comment records to : '', a80)')
     & outfile_full
                           call xaerror ( context, 5)
                       endif
               enddo
             endif
 9996        call ftclos(outlun,status)
 9997        call ftfiou(outlun,status)
             

           
           endif
           

 9900   continue
      endif

             
 9999 continue
      
C---  echo error message
      if ( status .ne. 0 ) then
         call ftgerr(status,context)
         call xaerror(context, 5)
         ierr=status
C---     Close input in case of error, would have been closed 
C---        otherwise
         call ftclos ( ratlun, status )
           status=ierr 
      endif 
      close ( listlun )
      if (orbitcor .eqv. .true. ) then
         close ( orblun )
      endif

      end

C********************************************************************
C SUBROUTINE:
C     aarea
C
C DESCRIPTION:
C     calculates the effective area for all the successive 
C     rows of the oso-8 rates file that use the same house
C     keeping record.
C
C AUTHOR:
C     David Dawson, HSTX 12/96
C
C ARGUMENTS:
C     srcvec     - source position unit vector
C     spinra     - ra of the space craft spin vector
C     spindec    - dec of the space craft spin vector
C     sunra      - ra of sun
C     sundec     - dec of sun
C     period     - period of space craft in ms
C     efarea     - efective area for each row of interval
C     elemnum    - number of rows to compute the effective area for
C     miptime    - time in ms after the housekeeping time that the 
C                  space craft passed the sun.
C     status     - program status
C
C PRIMARY LOCAL VARIABLES:
C     context    - string used for echoing error messages
C     opdet      - minimum angle between detector and source to include
C     area       - area in cm**2 of a detector
C     adec       - angle that a detector is fixed from the spin axis
C     sunvec     - unit vector of the sun position
C     spinvec    - unit vector of the spin position
C     angle      - angle between plane of the sun and the a detector
C     u, v       - temporary unit vectors
C     th         - angle between the a detector and the source
C     cth        - cosine of the angle between the a det and the source
C     areaf      - function that computes the effective area for angle
C     offset     - offset of a detector from the miptime
C     x          - temporary variable
C     delay      - fraction of rotation between the hk starttime and the
C                  sun (mip) time
C     status     - program status
C     i          - loop index
C
C CALLED ROUTINES:
C     convec     - calculates cartesian unit vector from ra and dec
C     vecrot     - rotates vector to new coord system
C     dot        - dot product
C     fcecho     - echos message
C
C********************************************************************

      subroutine AAREA (srcvec, spinra, spindec, sunra, 
     *     sundec, period, efarea, elemnum, miptime, firstmip,
     *     timehk_elem, start_interval, status)

      implicit none
      character(80) context
      real period
      double precision twopi, pid2, delay, firstmip, timehk_elem,
     *     start_interval, temp, efarea(75), areaf, x, opdet,
     &     srcvec(3), spinvec(3),spinra, spindec,spra, spdec, area,
     &     u(3), v(3), th, cth, sunvec(3),angle, adec,sunra, sundec,
     &     offset
      integer elemnum, i, miptime(*), status

      parameter (offset = 234.0)

      data opdet/.08884d0/
      data area/263.0d0/
      data adec/-1.483d0/
      data twopi/6.283185D0/
      data pid2/1.570796327D0/

      areaf(x) = (dacos(x) - x * dsqrt(1.0d0-x*x)) / pid2

      if (status .ne. 0) return

C---  Precess spin axis to 1950 coordinates
      spra=spinra+0.00581d0+0.002526d0*dsin(spinra)*datan(spindec)          
      spdec=spindec+0.002526d0*dcos(spinra)                               

C---  Calculate unit cartesian vectors
      call CONVEC (spra, spdec, spinvec)
      call CONVEC (sunra, sundec, sunvec)

C---  Rotate source vector to coord system with the spin axis
C     as the z axis and the y axis in the plane of the sun
      call VECROT (srcvec, spinvec, sunvec, v)

      do i = 1, elemnum
C---     Compute the angle of the bin relative to the sun
         temp = timehk_elem * 1000d0 - start_interval
         delay = ( temp  + offset 
     *        + 160.0d0 * DBLE(i) ) / DBLE(period)

c         delay = (offset + 160. * i + (160. -  miptime) / period

         angle = dmod ( delay, 1.D0) * twopi
         angle = angle - 0.682424d0
         if (angle .lt. 0d0) angle = angle + twopi
         call CONVEC (angle, adec, u)

C---     Compute angle between actual A detector pointing position
C---           and the source
         call DOT (v, u, cth)
C---     guarantee cth<1 before computing arccosine
         if (cth.gt.1.0d0) then
            context = 'Greater than 1.0 error'
            call xwrite (context, 15)
            cth = 1.0d0
         endif
         th = dacos (cth)
c         write(45,*) timehk + i * 0.16d0, char(9), th / twopi * 360.0 
         if (th .lt. opdet) then
            efarea(i) = areaf(th/opdet) * area
c            write (30,*) timehk_elem, temp, i, period, efarea(i)
         else
            efarea(i) = 0.0d0
         endif
      enddo
      end

C********************************************************************
C SUBROUTINE:
C     bcarea
C
C DESCRIPTION:
C     Calculates effecive area for b and c detector
C
C ARGUMENTS:
C     detector    - 1, 2, or 3 for Det. A, B, or C respectively
C     srcvec      - source position as a cartesian unit vector 
C     spinra      - RA of spin direction
C     spindec     - Dec of spin direction
C     efarea      - array of effective area for each element 
C                   associated with the current housekeeping row
C
C VARIABLES:
C     spra50      - spin RA in 1950 coord. (input data is in 1975)
C     spdec50     - spin Dec in 1950 coord.
C     spinvec     - spin position in cartesion coords.
C     costheta    - cosine of the angle between detector position
C                   and source
C     theta       - angle (radians) between detector and source
C     opdet       - radius (radians) of the detectors field of view
C     detectorarea- area (cm**2) of the detector
C     x           - theta / radius of detecotr
C     areaf       - effective area of the detector
C********************************************************************

      subroutine bcarea (detector, srcvec, spinra, spindec, efarea,
     *     elemnum, status)

      integer detector, elemnum, i, status
      real*8 srcvec(3), spinra, spindec, spra50, spdec50,
     *     spinvec(3), costheta, theta
      real*8 x,areaf, detectorarea, efarea(75), opdet
      character(255) string

      areaf(x) = (dacos(x) - x * dsqrt(1.0d0-x*x))/1.570796327d0

c  Initialize to avoid warning
      opdet = 1.d0
      detectorarea = 0.d0
c  --
      if (status .ne. 0) return

C---  Precess spin axis to 1950 coordinates
      spra50=spinra+0.00581d0+0.002526d0*dsin(spinra)*datan(spindec)          
      spdec50=spindec+0.002526d0*dcos(spinra)                               

c---  convert to cartesian coord.
      call CONVEC ( spra50, spdec50, spinvec )
      
      call DOT (srcvec, spinvec, costheta)
      if (detector .eq. 2) costheta = -costheta
      theta = dacos(costheta)
      if (detector .eq. 2) then
         opdet = 0.05829d0
         detectorarea = 36.7d0
      elseif (detector .eq. 3) then
         opdet = 0.08884d0
         detectorarea = 237.0d0
      endif
      if (theta .lt. opdet) then 
         efarea(1) = areaf(theta/opdet)*detectorarea
         write(string,12) theta
 12      format("theta",F13.9)
         call xwrite(string,55)
      else
         efarea(1) = 0.0d0
      endif
      do i = 2, elemnum
         efarea(i) = efarea(1)
      enddo
      
      return
      end

C********************************************************************
C SUBROUTINE:
C     calcrpm
C
C DESCRIPTION:
C     Uses miptime to calculate time for each hk record that the 
C     spacecraft passes the sun.  The difference in successive 
C     times is averaged over a good time interval to get a better
C     measure of the rpm than the rpm column contains.  This is
C     important for tha A detector which is pointed 5 degrees off 
C     the spin axis.
C
C     Changed to remeber the first time of the good time period.
C
C AUTHOR:
C     David Dawson, HSTX 12/96
C
C ARGUMENTS:
C     startrow- input row to start calculating average from
C     timehk  - array of times from the house keeping extension
C     miptime - offset (ms) that the time that OSO-8 passes the sun
C               is from the beginning of the first 160 ms bin for
C               each housekeeping record.
C     clean   - true if data is clean
C     adet    - true if A Detector is unocculted
C     lastdatarow - last row that the current period value is valid
C     avperiod- average period for good time interval
C     status  - program status
C
C VARIABLES:
C     spinperiod   - time between successive mip times
C     avspinperiod - average spin period
C     mip1, mip2   - time that OSO-8 passed rotated passed the sun
C     datarow      - binary table row from the input fits file
C     totalrec     - number of input rows used to compute aver. period
C
C CALLED ROUTINES:
C     none
C     
C********************************************************************

      subroutine calcrpm (startrow, hktime, miptime, clean, adet,
     *     lastdatarow, avperiod, firstmip, secondmip, status)
      double precision hktime(*), spinperiod, avspinperiod,
     *    mip1, mip2, firstmip, secondmip, temp
      real avperiod
      integer miptime(*), datarow, totalrec, lastdatarow, 
     *     startrow,  status
      integer*2 clean(*), adet(*)

      logical doneonce

      if (status .ne. 0) return

      doneonce = .false.

      datarow = startrow
      totalrec = 0
      avspinperiod = 0.0d0
      mip1 = dble(miptime(datarow)) + hktime(datarow) * 1000.0d0
      temp = mip1
      firstmip = mip1
      datarow = datarow + 1

      do while (clean(datarow).eq.1 .and. adet(datarow).eq.1)
         mip2 = dble(miptime(datarow)) + hktime(datarow) * 1000.0d0
         spinperiod = mip2 - mip1
         if (dabs(spinperiod - avspinperiod) .le. 
     *        (0.03 * avspinperiod) .or. avspinperiod .eq. 
     *        0.0d0) then
            if (spinperiod .ge. 9200.0 .and. spinperiod 
     *           .le. 11200.0) then
               totalrec = totalrec + 1.0            
               avspinperiod = (spinperiod + avspinperiod * 
     *              (totalrec - 1)) /  totalrec
            endif
         endif
         mip1 = mip2
         datarow = datarow + 1
         doneonce = .true.
      enddo

      avperiod = avspinperiod
      lastdatarow = datarow
      secondmip = mip1

      if ( .not.doneonce ) then
         status = 1
c      write(*,*) '-------------'
c      write(*,*) 'startrow,hktime,miptime,clean,adet,lastdatarow,'
c     &           //'avperiod, firstmip, secondmip, status =',
c     &           startrow, hktime, miptime, clean, adet,
c     &           lastdatarow, avperiod, firstmip, secondmip, status
c      write(*,*) 'clean(datarow), adet(datarow) =',
c     &            clean(datarow), adet(datarow)
         return
      endif

      return
         
      
      end

C********************************************************************
C SUBROUTINE:
C     calcarea
C
C DESCRIPTION:
C     Calculates effective area for all data records pertaining to 
C     the current hk record.
C
C ARGUMENTS:
C     detector   - equals 1, 2, 3 for Detector A, B, C respectively
C     hkrow      - the current row from the housekeeping table
C     timehk     - the time value for the current housekeeping row
C     miptime    - offset(ms) between the time of the first bin of 
C                  the hk row and the time the detector passes the sun
C     clean      - true if the data is clean
C     adet       - true if the A detector is not occulted
C     bdet       - true if the B detector is not occulted
C     cdet       - true if the C detector is not occulted
C     lastdatarow- the last hk row that the current calculated average
C                  period is good for
C     period     - calculated average period of the spacecraft for an
C                  interval of good time (clean and unocculted)
C     srcvec     - cartesian unit vector of the source position
C     spinra     - RA of the spin direction
C     spindec    - Dec of the spin direction
C     sunra      - RA of the sun
C     sundec     - Dec of the sun
C     efarea     - array of effective areas for each element associated
C                  with the current housekeeping row
C     elemnum    - number of bins associated with this housekeeping row
C     status     - program status
C
C CALLED ROUTINES:
C     calcrpm    - calculates average rpm for a good time interval
C     aarea      - calculates effective area for the A Detector
C     bcarea     - calculates effective area for the B or C Detector
C
C********************************************************************

      subroutine calcarea ( detector, hkrow, timehk, miptime, 
     *     clean, adet, bdet, cdet, lastdatarow, period, srcvec,
     *     spinra, spindec, sunra, sundec, efarea, elemnum, 
     *     firstmip, secondmip, status)

      implicit none
      integer hkrow, miptime(*), lastdatarow, detector, elemnum, 
     *     status, jj
      real period
      double precision timehk(*), firstmip, secondmip, start_interval,
     *     efarea(75), srcvec(3), spinra, spindec, sunra, sundec
      integer*2 clean(*), adet(*), bdet, cdet

C---  Check to see if a detector data is good
      if (detector .eq. 1 .and. adet(hkrow).eq.1 .and. 
     *     clean(hkrow).eq.1) then
         
C---     Calculate a new average spin period if this is new interval
         if (hkrow .gt. lastdatarow) then
            call calcrpm (hkrow, timehk, miptime, clean, adet, 
     *           lastdatarow, period, firstmip, secondmip, status)
c            start_interval = (timehk(hkrow) - 10.d0 * dble (period)) * 
c     *           1000 + miptime(hkrow)
            start_interval = (timehk(hkrow) * 1000.d0) - 10.d0 * 
     *           dble(period) + dble(miptime(hkrow))

         endif

C---     Calculate effective area for all data pertaining to hk row
         if ( status .eq. 0 ) then
            call AAREA (srcvec, spinra, spindec, 
     *           sunra, sundec, period, efarea, 
     *           elemnum, miptime, firstmip, timehk(hkrow),
     *           start_interval, status)
         else
            do jj = 1, 75
               efarea(jj) = 0.0d0
            enddo
            status = 0
         endif

C---  Calculate effective area for b or c detector
      elseif (((detector .eq. 2 .and. bdet.eq.1)
     *        .or. (detector .eq. 3 .and. cdet.eq.1))
     *        .and. clean(hkrow).eq.1) then
         call BCAREA (detector, srcvec, spinra, spindec, efarea, 
     *        elemnum, status)
      else
         do jj = 1, 75
            efarea(jj) = 0.0d0
         enddo
      endif

      return

      end
C********************************************************************
C SUBROUTINE:
C     convdate
C
C DESCRIPTION:
C     converts time into dd/mm/yy and hh:mm:ss strings.
C
C AUTHOR:
C     David Dawson 12/96
C
C ARGUMENTS:
C     time    - input time in seconds since Jan. 1, 1975
C     datestr - output string containing the date (dd/mm/yy)
C     timestr - output string containing the time (hh:mm:ss)
C     status  - program status
C
C LOCAL VARIABLES:
C     osotime    - input time converted to days
C     fraction   - fraction of day from osotime
C     yy         - year string
C     mm         - month string
C     dd         - day string
C     hhh        - hour string
C     mmm        - minute string
C     sss        - second string
C     dayno      - day number of the year that osotime is in
C     date       - day number of the month that osotime is in
C     hr         - hour stored as an integer
C     min        - minute stored as an integer
C     sec        - second stored as an integer
C     leapday    - equals one in a leap year, 0 otherwise
C 
C CALLED ROUTINES:
C LA 22/10/1998  change the day counter. 
C                The DATE-OBS and -END were 1 day ahead
C
C********************************************************************

      subroutine convdate (time, datestr, timestr, status)
      
      implicit none
      double precision time, osotime, fraction
      character(8) datestr, timestr
      character(2) yy, mm, dd, hhh, mmm, sss
      integer status, dayno, date, hr, min, sec, leapday

      if (status.ne.0) return

c  Initialize to avoid warning
      dayno = 0
c  --

 
C---  Convert to days
      osotime = time / 86400.0d0

C---  Determine yy and day of that year      
      if ( osotime .lt. 365.0d0 ) then
c         dayno = osotime + 1
         dayno = osotime 
         yy = '75'
      elseif (osotime .lt. 731.0d0 ) then
c         dayno = osotime - 364.0d0
         dayno = osotime - 365.0d0
         yy = '76'
      elseif ( osotime .lt. 1096.0d0 ) then
c         dayno = osotime - 730.0d0
         dayno = osotime - 731.0d0
         yy = '77'
      elseif ( osotime .lt. 1461.0d0 ) then
c         dayno = osotime - 1095.0d0
         dayno = osotime - 1096.0d0
         yy = '78'
      endif

C---  Determine if year is a leap year
      if ( yy .eq. '76' ) then
         leapday = 1
      else
         leapday = 0
      endif

C---  Determine month and day of month 
      if ( dayno.le.31 ) then
         mm = '01'
         date = dayno
      elseif ( dayno.le.(59+leapday) ) then
         mm = '02'
         date = dayno - 31
      elseif ( dayno.le.(90+leapday) ) then
         mm = '03'
         date = dayno - 59 - leapday
      elseif ( dayno.le.(120+leapday) ) then
         mm = '04'
         date = dayno - 90 - leapday
      elseif ( dayno.le.(151+leapday) ) then
         mm = '05'
         date = dayno - 120 - leapday
      elseif ( dayno.le.(181+leapday) ) then
         mm = '06'
         date = dayno - 151 - leapday
      elseif ( dayno.le.(212+leapday) ) then
         mm = '07'
         date = dayno - 181 - leapday
      elseif ( dayno.le.(243+leapday) ) then
         mm = '08'
         date = dayno - 212 - leapday
      elseif ( dayno.le.(273+leapday) ) then
         mm = '09'
         date = dayno - 243 - leapday
      elseif ( dayno.le.(304+leapday) ) then
         mm = '10'
         date = dayno - 273 - leapday
      elseif ( dayno.le.(334+leapday) ) then
         mm = '11'
         date = dayno - 304 - leapday
      elseif ( dayno.le.(365+leapday) ) then
         mm = '12'
         date = dayno - 334 - leapday
      endif

C---  Converte date value to a string, and build date string
      write (dd,500) date
 500  format (i2.2)
      datestr = dd // '/' // mm // '/' // yy
      
C---  Compute hours, min, sec of day
      fraction = osotime - dint ( osotime )      
      hr = int( fraction * 24 )
      min = int( ( ( fraction * 24.d0 ) - hr ) * 60.d0 )
      sec = int(((((( fraction * 24.d0 ) - hr) * 60.d0 ) - min ) * 
     *     60.d0 ) + 0.5d0)

C---  Convert time values to a string
      write (hhh, 500) hr
      write (mmm, 500) min
      write (sss, 500) sec
      timestr = hhh // ':' // mmm // ':' // sss

      return

      end


C********************************************************************
C SUBROUTINE:
C     vecrot
C     
C DESCRIPTION:
C     Rotates vector a into frame with b=z direction,
C     and b X c = y direction
C
C AUTHOR:
C
C ARGUMENTS:
C     a  - input vector
C     b  - vector containing z axis of new frame of reference
C     c  - vector used to define new y axis; b X c = y direction
C     d  - output rotated vector
C
C CALLED SUBROUTINES:
C     unitv   - outputs unit vector for given vector
C     dot     - dot product
C     cross   - cross product
C********************************************************************
      subroutine vecrot(a,b,c,d) 
      real*8 a, b, c, d, u, v, x
      dimension a(3),b(3),c(3),d(3),u(3),v(3)                           
      call unitv(b,u)                                                   
       call dot(a,u,x)                                                  
      d(3)=x                                                            
      call cross(b,c,v)                                                 
      call unitv(v,v)                                                   
      call dot(a,v,x)                                                   
      d(2)=x                                                            
      call cross(v,u,u)                                                 
      call unitv(u,u)                                                   
      call dot(a,u,x)                                                   
      d(1)=x                                                            
      return                                                            
      end

C********************************************************************
C SUBROUTINE:
C     convec
C
C DESCRIPTION:
C     This routine uses right asc and decl (in radians) to set up a 
C     unit vector                                                           
C
C AUTHOR:
C
C ARGUMENTS:
C     ra   - input right ascension
C     dec  - input declination
C     v    - output vector in cartesian coordinate
C********************************************************************

      subroutine convec(ra,dec,v)                                       
      real*8 ra, dec, v
      dimension v(3)                                                    

      v(1)=dcos(ra)*dcos(dec)                                             
      v(2)=dsin(ra)*dcos(dec)                                             
      v(3)=dsin(dec)                                                     
      return                                                            
      end

C********************************************************************
C SUBROUTINE:
C     cross
C
C DESCRIPTION:
C     vector cross product of a X b
C
C AUTHOR:
C
C ARGUMENTS:
C     a   - input vector
C     b   - input vector
C     c   - output vector
C********************************************************************

      subroutine cross(a,b,c)                                           
      real*8 a, b, c, x, y, z
      dimension a(3),b(3),c(3)                                          
      x=a(2)*b(3)-a(3)*b(2)                                             
      y=a(3)*b(1)-a(1)*b(3)                                             
      z=a(1)*b(2)-a(2)*b(1)                                             
      c(1)=x                                                            
      c(2)=y                                                            
      c(3)=z                                                            
      return
      end

C********************************************************************
C SUBROUTINE:
C     dot
C
C DESCRIPTION:
C     Computes the dot product
C     
C ARGUMENTS:
C     a, b  - input vectors
C     w     - output vector
C********************************************************************

      subroutine dot(a,b,w)                                                  
      real*8 a(3), b(3), w
      w=a(1)*b(1)+a(2)*b(2)+a(3)*b(3)                                   
      return
      end

C********************************************************************
C SUBROUTINE:
C     unitv
C
C DESCRIPTION:
C     Calculates unit vector for given vector
C
C ARGUMENTS:
C     a  - input vector
C     b  - output vector
C
C********************************************************************

      subroutine unitv(a,b)                                                  
      real*8 a(3), b(3), x
      integer i

      x=dsqrt(a(1)*a(1)+a(2)*a(2)+a(3)*a(3))                             
      do i = 1, 3                                                       
         b(i)=a(i)/x                                                       
      enddo
      return                                                            
      end                                                               

C********************************************************************
C SUBROUTINE:
C     close_fits
C
C DESCRIPTION:
C     Closes light curve fits file
C
C********************************************************************


      subroutine close_fits (outlun, outfile_full,outfile_num,
     *     outrow, tstart, timedat, 
     *     srccnt_total, srccnte_total,binspin, status)
      
      implicit none
      integer outlun, outrow, status, hdutype,outfile_num
      double precision tstart, tstop, timedat, telapse, ontime
      character(8) stdatestr, enddatestr, sttimestr, endtimestr
      real  srccnt_total, srccnte_total, srccnt, srccnte
      logical binspin
      character*(*) outfile_full
      character(160) errm

C---  Modify number of rows in output file and timing keywords
      call FTMKYJ ( outlun, 'NAXIS2', outrow, '&', status )
      call FTMKYD ( outlun, 'TSTART', tstart, 15, '&', status )
      tstop = timedat + 0.160d0
      call FTMKYD ( outlun, 'TSTOP', tstop, 15, '&', status )
      telapse = tstop - tstart
      call FTMKYD ( outlun, 'TELAPSE', telapse, 15, '&', status )
      ontime = dble(outrow) * 0.16d0
      call FTMKYD ( outlun, 'ONTIME', ontime, 15, '&', status )
     
      IF (binspin) call ftpkyg(outlun, 'TIMEDEL', 10.00D0, 2, 
     +     'Time step between rows (seconds)', status)

      
C---  Convert start and stop time to strings and write to both headers
      call convdate ( tstart, stdatestr, sttimestr, status )
      call convdate ( tstop, enddatestr, endtimestr, status )
      call FTMKYS ( outlun, 'DATE-OBS', stdatestr, '&', status )
      call FTMKYS ( outlun, 'TIME-OBS', sttimestr, '&', status )
      call FTMKYS ( outlun, 'DATE-END', enddatestr, '&', status )
      call FTMKYS ( outlun, 'TIME-END', endtimestr, '&', status )
      If (outrow .ne. 0) then
         srccnt = srccnt_total / FLOAT(outrow)
         srccnte = srccnte_total / FLOAT(outrow)
      else 
         WRITE (errm,
     & '( ''WARNING : bad data in file... so deleting file: '',a80)')
     &  outfile_full
          call xaerror(errm,1)
          status=0
          open(unit=outlun, file=outfile_full, status='old')
          close(outlun, status='delete')
          outfile_num = outfile_num -1
          goto 1000
      endif
      call FTMKYE ( outlun, 'SRCCNT', srccnt, 10, '&', status )
      call FTMKYE ( outlun, 'SRCCNTE', srccnte, 10, '&', status )

      call FTMAHD ( outlun, 1, hdutype, status )
      call FTMKYS ( outlun, 'DATE-OBS', stdatestr, '&', status )
      call FTMKYS ( outlun, 'TIME-OBS', sttimestr, '&', status )
      call FTMKYS ( outlun, 'DATE-END', enddatestr, '&', status )
      call FTMKYS ( outlun, 'TIME-END', endtimestr, '&', status )




C---  close remaning files
      call ftclos ( outlun, status )

      call ftfiou ( outlun, status )

 1000 continue
      return
      end


C********************************************************************
C SUBROUTINE:
C     open_fits
C
C DESCRIPTION:
C     Opens new fits light curve file.
C
C LA 22/10/98 mjdref value change from 42413. to 42412.
C LB 07/03/99 added clobber parameter
C********************************************************************

      subroutine open_fits(outlun, outfile, detector, det_area, 
     *    src_radeg,src_decdeg, clobber, status )

      implicit none
      integer outlun, detector, status
      real det_area,src_radeg,src_decdeg
      character(8) detname
      character(80) outfile
      character(20) ttype(4), tform(4), tunit(4)
      character(160) context
      logical clobber


C---  Create output fits file

CCC   temporary diagnostic
      write(context, '('' Open output file '',a)')outfile
      call xwrite (context,10) 
      call FTGIOU ( outlun, status ) 
      call FTINIT ( outlun, outfile, 2880, status )
      if (status .ne. 0) then
         if (clobber) then
            context = ' File already exists, overwriting...'
            call xwrite(context,1)
            status = 0
            open(unit=outlun, file=outfile, status='old')
            close(outlun, status='delete')
            call FTINIT ( outlun, outfile, 2880, status )
         endif
         if (status .ne. 0) then
            write(context, '('' Error creating '', A40)') outfile
            call xaerror(context,1)
            call ftgmsg(context)
            call xaerror(context,1)
            write(context, '('' FITSIO status = '', I3)') status
            call xaerror(context,1)
            go to 999
         endif
      endif  
      call FTPHPR ( outlun, .true., 8, 0, 0, 0, 1, .true., status )
      call FTPDAT ( outlun, status )
      call FTPKYS ( outlun, 'TELESCOP', 'OSO-8', 
     *     'Telescope (mission) name', status )
      call FTPKYS ( outlun, 'INSTRUME', 'GCXSE', 'Instrument name',
     *     status )
      if (detector.eq.1) then
         detname = 'DET-A'
      elseif (detector.eq.2) then
         detname = 'DET-B'
      elseif (detector.eq.3) then
         detname = 'DET-C'
      endif
      write(context, '('' Detector  '',a)')detname
      call xwrite (context,10) 
      call FTPKYS ( outlun, 'DETNAM', detname, 'Detector name',
     *     status )
      call FTPKYS ( outlun, 'DATE-OBS', ' ', 
     *     'Start date of observation (dd/mm/yy)', status )
      call FTPKYS ( outlun, 'TIME-OBS', ' ',
     *     'Start time of observation (hh:mm:ss', status )
      call FTPKYS ( outlun, 'DATE-END', ' ',
     *     'End date of observation (dd/mm/yy)', status )
      call FTPKYS ( outlun, 'TIME-END', ' ',
     *     'End time of observation (hh:mm:ss)', status )

      CALL FTPKYF (outlun, 'EQUINOX',1950.0,1,
     &     'Equinox of coordinate system', status )
      CALL FTPKYS (outlun, 'RADECSYS','FK4    ',
     &     'Stellar reference frame in use', status )
      CALL FTPKYF (outlun, 'RA_OBJ',src_radeg,4,
     &     'Right Ascension of object (degrees)', status )
      CALL FTPKYF (outlun, 'DEC_OBJ',src_decdeg,4,
     &     'Declination of object (degrees)', status )


C---  Set up output binary table
      ttype(1) = 'TIME'
      tform(1) = '1D'
      tunit(1) = 's'
      ttype(2) = 'RATE'
      tform(2) = '1E'
      tunit(2) = 'count /s'
      ttype(3) = 'ERROR'
      tform(3) = '1E'
      tunit(3) = 'count /s'
      ttype(4) = 'EFFAREA'
      tform(4) = '1E'
      tunit(4) = 'cm**2'
      call FTIBIN ( outlun, 1, 4, ttype, tform, tunit, 'RATE',
     *     0, status )
      call FTPDAT ( outlun, status )
      call FTPKYS ( outlun, 'TELESCOP', 'OSO-8', 
     *     'Telescope (mission) name', status )
      call FTPKYS ( outlun, 'INSTRUME', 'GCXSE', 'Instrument name',
     *     status )
      call FTPKYS ( outlun, 'DETNAM', detname, 'Detector name',
     *     status )
      call FTPKYS ( outlun, 'DATE-OBS', ' ', 
     *     'Start date of observation (dd/mm/yy)', status )
      call FTPKYS ( outlun, 'TIME-OBS', ' ',
     *     'Start time of observation (hh:mm:ss', status )
      call FTPKYS ( outlun, 'DATE-END', ' ',
     *     'End date of observation (dd/mm/yy)', status )
      call FTPKYS ( outlun, 'TIME-END', ' ',
     *     'End time of observation (hh:mm:ss)', status )
      call FTPDAT ( outlun, status)
c      call FTPKYD ( outlun, 'MJDREF', 42413.0d0, 15,
       call FTPKYD ( outlun, 'MJDREF', 42412.0d0, 15,
     *     'reference time in MJD', status )
c      call FTPKYS ( outlun, 'TIMESYS', '1975 1 1 00:00:00',
       call FTPKYS ( outlun, 'TIMESYS', '1975 1 0 00:00:00',
     *     'ref. time in YYYY MM DD hh:mm:ss', status )
      call FTPKYS ( outlun, 'TIMEREF', 'LOCAL',
     *     'reference frame for the times', status )
      call FTPKYS ( outlun, 'TASSIGN', 'SATELLITE',
     *     'place of time assignment', status )
      call FTPKYL ( outlun, 'CLOCKAPP', .false.,
     *     'true if clock correction applied', status )
      call FTPKYS ( outlun, 'TIMEUNIT', 's', 
     *     'unit for TSTART, TSTOP, and TIMEZER', status )
      call FTPKYD ( outlun, 'TIMEDEL', 0.16d0, 15,
     *     'integration time in TIMEUNIT', status )
      call FTPKYD ( outlun, 'TSTART', 0.0d0, 10,
     *     'start time counted from TIMESYS', status )
      call FTPKYD ( outlun, 'TSTOP', 0.0d0, 10,
     *     'end time counted from TIMESYS', status )
      call FTPKYD ( outlun, 'TELAPSE', 0.0d0, 10,
     *     'elapsed time of observation', status )
      call FTPKYD ( outlun, 'ONTIME', 0.0d0, 10,
     *     'time on source', status )
      call FTPKYE ( outlun, 'SRCCNT', 0.0, 10, 
     *     'average count in count/s', status )
      call FTPKYE ( outlun, 'SRCCNTE', 0.0, 10, 
     *     'average error in count/s', status )

      call FTPKYE ( outlun, 'GEOAREA', det_area, 10, 
     *     'detector area in cm**2', status )

      CALL FTPKYF (outlun, 'EQUINOX',1950.0,1,
     &     'Equinox of coordinate system', status )
      CALL FTPKYS (outlun, 'RADECSYS','FK4    ',
     &     'Stellar reference frame in use', status )
      CALL FTPKYF (outlun, 'RA_OBJ',src_radeg,4,
     &     'Right Ascension of object (degrees)', status )
      CALL FTPKYF (outlun, 'DEC_OBJ',src_decdeg,4,
     &     'Declination of object (degrees)', status )

999   continue
      return
      end
C********************************************************************
C SUBROUTINE: get_times
C
C This subroutine calculates the satellite orbit time corrections\
C
C Author: Mark Stollberg (Raytheon STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Mar 01 1999
C              Mar 04 1999 modified for inclusion into OSORAT.f
C                          (Lorraine Breedon)

      subroutine GET_TIMES (iunit,lcend,ra,dec,stime,filename,ounit,
     &                     outfile_full,finishcorr,num,
     &                     errstat)

      Implicit none

      character*(*) filename,outfile_full

      INTEGER counter, i, naxis2
      INTEGER*4 iunit, ounit, errstat, readwrite, blocksize
      INTEGER*4 hdutype, lcend, num
      LOGICAL anyf
      DOUBLE PRECISION lnull, dnull, stime, dumtime, orbtime, delt
      DOUBLE PRECISION dvdt(3), dsatdt(3), dsundt(3), t_corr(1000000)
      DOUBLE PRECISION diff, qtime
      REAL*4 enull1, enull2, enull3, dum_sat(3), dum_vel(3), dum_sun(3)
      REAL*4 posn_sat(3), vel_sat(3), posn_sun(3), raddeg, ra, dec
      REAL*4 s(3), t_sc, t_se, t_tot
      character(6) keyword
      character(80) context, comment
      DATA raddeg/0.0174533/
      LOGICAL finishcorr

      context = ' '
      keyword = 'NAXIS2'
      errstat = 0
      readwrite = 0
      dnull = 0.d0
      lnull = 0.0d0
      enull1 = 0.0
      enull2 = 0.0
      enull3 = 0.0
c      num = 1
        
      call FTGIOU (ounit,errstat)
      call FTOPEN (ounit,filename,readwrite,blocksize,errstat)
      if(errstat.ne.0)then
                WRITE (context,
     & '( '' Error opening orbit file : '', a50)')
     & filename
                call xaerror ( context, 5 )
                goto 9999
        endif

    
C     Get data from the orbit table

      call FTMAHD (ounit,2,hdutype,errstat)
      if(errstat.ne.0)then
                WRITE (context,
     & '( '' Error moving to 1st ext of orbit file : '', a50)')
     & filename
                call xaerror ( context, 5)
                goto 9999
       endif


C     Find preliminary satellite position, velocity, and sun position

      counter = 1
      
      call FTGKYJ (ounit,keyword,naxis2,comment,errstat)
      if(naxis2.eq.0)then
         WRITE (context,
     & '( '' Error orbit file empty :'', a50)')
     & filename
         call xaerror ( context, 5)
         goto 9999
      endif

  100 call FTGCVD (ounit,1,counter,1,1,dnull,dumtime,anyf,errstat) 
      if (dumtime .lt. stime) then
         counter = counter + 1
         if (counter .gt. naxis2) goto 800
         goto 100
      end if
      if (counter .eq. 1) goto 150
      call FTGCVD (ounit,1,counter-1,1,1,dnull,orbtime,anyf,errstat) 
      call FTGCVE (ounit,5,counter-1,1,3,enull1,dum_sat,anyf,errstat)
      call FTGCVE (ounit,6,counter-1,1,3,enull2,dum_vel,anyf,errstat)
      call FTGCVE (ounit,10,counter-1,1,3,enull3,dum_sun,anyf,errstat)
  150 do 200 i=1,3
         posn_sat(i) = dum_sat(i)
         vel_sat(i) = dum_vel(i)
         posn_sun(i) = dum_sun(i)
  200 continue    
      call FTGCVE (ounit,5,counter,1,3,enull1,dum_sat,anyf,errstat)
      call FTGCVE (ounit,6,counter,1,3,enull2,dum_vel,anyf,errstat)
      call FTGCVE (ounit,10,counter,1,3,enull3,dum_sun,anyf,errstat)

C     Obtain starting velocity, position of satellite and position of sun.

      delt = stime - orbtime
      do 300 i=1,3
         dvdt(i) = (dum_vel(i) - vel_sat(i))/(dumtime - orbtime)
         vel_sat(i) = vel_sat(i) + dvdt(i)*delt
         dsatdt(i) = (dum_sat(i) - posn_sat(i))/(dumtime - orbtime)
         posn_sat(i) = posn_sat(i) + dsatdt(i)*delt
         dsundt(i) = (dum_sun(i) - posn_sun(i))/(dumtime - orbtime)
         posn_sun(i) = posn_sun(i) + dsundt(i)*delt
  300 continue 

C     Calculate delay times.

      s(1) = cos(ra*raddeg)*cos(dec*raddeg)
      s(2) = sin(ra*raddeg)*cos(dec*raddeg)
      s(3) = sin(dec*raddeg)

      t_sc = s(1)*posn_sat(1) + s(2)*posn_sat(2) +
     *       s(3)*posn_sat(3)
      t_se = s(1)*posn_sun(1) + s(2)*posn_sun(2) +
     *       s(3)*posn_sun(3)
      t_sc = -t_sc/2.99792458e+05
      t_se = -t_se*4.99004785e+02
      t_tot = t_sc + t_se
      t_corr(num) = stime + DBLE(t_tot)


C write corrected time to given row in LC
       errstat = 0
       call FTPCLD (iunit,1,num,1,1,t_corr(num),errstat)
       if (errstat.ne.0) then
         WRITE (context,
     & '( '' Error writing orbit corrected time in row '', a50)')
     & num
         call xaerror ( context, 5)
         WRITE (context,
     & '( '' .. for light curve :  '', a50)')
     & outfile_full
         call xaerror ( context, 5)
         goto 9999
       endif

C     Generate corrections for other available times in light curve
       num = num+1
  500 continue
      if (num .gt. lcend) then
         finishcorr = .TRUE. 
         goto 9999
      endif
      errstat = 0
      call FTGCVD (iunit,1,num,1,1,lnull,qtime,anyf,errstat)
      if (errstat.ne.0) then
         WRITE (context,
     & '( '' Error getting STIME for '', a50)')
     & outfile_full
         call xaerror ( context, 5)
         goto 9999
      endif

      if (qtime .le. dumtime) then
         diff = qtime - stime
         do 600 i=1,3
            vel_sat(i) = vel_sat(i) + dvdt(i)*diff
            posn_sat(i) = posn_sat(i) + dsatdt(i)*diff
            posn_sun(i) = posn_sun(i) + dsundt(i)*diff
  600    continue 
         t_sc = s(1)*posn_sat(1) + s(2)*posn_sat(2) +
     *          s(3)*posn_sat(3)
         t_se = s(1)*posn_sun(1) + s(2)*posn_sun(2) +
     *          s(3)*posn_sun(3)
         t_sc = -t_sc/2.99792458e+05
         t_se = -t_se*4.99004785e+02
         t_tot = t_sc + t_se
         t_corr(num) = qtime + DBLE(t_tot)
         errstat = 0

         call FTPCLD (iunit,1,num,1,1,t_corr(num),errstat)
         if (errstat.ne.0) then
            WRITE (context,
     & '( '' Error writing orbit corrected time in row '', a50)')
     & num
            call xaerror ( context, 5)
            WRITE (context,
     & '( '' .. for light curve :  '', a50)')
     & outfile_full
            call xaerror ( context, 5)
            goto 9999
         endif         
 
         
         stime = qtime
         num = num + 1
         goto 500
      end if

C     Get new time interval or quit

      if (num .ge. lcend) then
         finishcorr=.TRUE.
         goto 9999
      endif
      stime = qtime 
      counter = counter + 1
      if (counter .gt. naxis2) goto 800
      goto 100

C     Close old orbit file, open new orbit file.

  800 orbtime = dumtime
      call FTCLOS (ounit,errstat)
      call FTFIOU (ounit,errstat)

 
 9999 continue
      end


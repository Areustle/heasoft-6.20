C****************************************************************************
C SELECTOR TASK:  
C      fxbary
C
C FILE:
C      fxbary.f 
C
C DESCRIPTION: 
c     Produce a BARYCENTER corrected file using orbit data of the XTE
c satellite, and the Ephemeris file. The input file should be a LIGHT_CURVE
c produced by either SAEXTRCT or SEEXTRCT. Using files produced by other
c FTOOLs may or may not function properly!
C     
C AUTHOR:  
C      Brian K. Elza 6/95
C
C MODIFICATION HISTORY:
c Beginning March 96 the modification to operate on RAW SA and SE data
c was added. This is a very dangerous option and should be used only
c with great care.
c     4-17-96 Modified the way the JD is calculated +24000001 - 0.5 so
c that the ephemeris is calculated using the next day rather than the
c previous day. 
c     6-01-98 removed unnecessary ftgkys() call in fxbaryopen which
c             caused crashes under oedipus/cfitsio 
c             Also removed ftpcks call on error -- v4.0b
C
C	7/6/98 - by ZG to change the string length of obsdate to 68 in
C		compliance with the new format. 
C
C      17Feb99 - (MJT) added ftflus() calls to insure that reads from the
C                copied-to file (why is this being done anyway??) work 
C                properly (v4.1)
C
C      19Jun00 - (MJT v5.0.2) fixed parsedeg problem with leading '+'
C                      update TIMEREF to 'SOLARSYSTEM' if overwriting TIME column
C                      made sure vers. # was consistently used on screen and kwds
C                Also forced it to loop on all GTI extensions when barytime is 'no'
C                     ie, when operating on a raw data file instead of a lightcurve
C
C NOTES:
C      
C      
C ARGUMENTS:
C      
C
C PRIMARY LOCAL VARIABLES:  
C      in_fil     - input FITS file and extension number
C      ou_fil     - output file
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
C      
C CALLED ROUTINES:
C      subroutine gsacrv - gets parameters from environment
C      subroutine fsacrv - read input FITS file and writes light curve file
C
C***************************************************************************
      subroutine fxbary()
      implicit none

      character(160) in_fil, ou_fil, eph_fil, orb_fil
      character(40) timecol
      double precision starttime, stoptime, ra, dec
      integer status

      character(40) taskname
      character(20) rastr,decstr
      logical abort, lbarytime, lsensecase

c      LOGICAL          MEMB(100)
c      INTEGER*2        MEMS(100)
c      INTEGER*4        MEMI(100)
c      INTEGER*4        MEML(100)
c      REAL             MEMR(100)
c      DOUBLE PRECISION MEMD(100)
c      COMPLEX          MEMX(100)
c      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
c      COMMON /MEM/ MEMD
      
      common /task/ taskname

      taskname = 'fxbary_5.0.2'
      in_fil = ' '
      ou_fil = ' '
      eph_fil = ' '
      orb_fil = ' '
      timecol = ' '
      rastr=' '
      decstr=' '
      abort=.FALSE.
      lbarytime=.TRUE.
      lsensecase=.FALSE.
      starttime=0.0d0
      stoptime=0.0d0
      ra=0.0d0
      dec=0.0d0
      status=0

      call fcecho(' ')
      call fcecho('Running FXBARY version 5.0.2')
      call fcecho('==============================================')
        
C     get the parameters from the par file
      call gbaryparm(in_fil, ou_fil, eph_fil,
     &   orb_fil, timecol, starttime, stoptime, ra,
     &   dec, rastr, decstr, lbarytime, lsensecase,
     &   abort, status)

c      print*,'Input information is ',in_fil, ou_fil, eph_fil,
c     &   orb_fil, timecol, starttime, stoptime, ra,
c     &   dec, status

c      stop
      
      if (status .ne. 0)then
        call fcecho(' ')
        call fcecho('Error reading in information from Parameter file')
        call fcecho('Check parameter file, and fix error')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      endif
      if(abort)then
        call fcecho(' ')
        call fcecho('Aborting error in parameter file!')
        goto 999
      endif
      
      call fcecho(' ')
      call fcecho('###################################################')

      if(.not.lbarytime)then
        call fcecho(' ')
        call fcecho('*************************************************')
        call fcecho('You have selected the option to OVERWRITE')
        call fcecho('the ORIGINAL Time column with BARYCENTER times!')
        call fcecho(' ')
        call fcecho('This is a one-way (non-recoverable) operation')
        call fcecho('and SA files will not be able to be processed')
        call fcecho('by SAEXTRCT properly. Since timestamps will ')
        call fcecho('no longer be linear after this transformation,')
        call fcecho('CDLT can not longer be accurately applied.')
        call fcecho(' ')
        call fcecho('All TIME related keywords (like TSTART and TSTOP)')
        call fcecho('will be updated in the data and GTI extensions.')
        call fcecho('All GTIs will be modified.')
        call fcecho('*************************************************')
      endif
      
      call fcecho(' ')
      call fcecho('Begin processing...')

      call fxbarycorrect(in_fil, ou_fil, eph_fil, orb_fil,
     &   timecol, starttime, stoptime, ra, dec, rastr,
     &   decstr, lbarytime, lsensecase, abort, status)
      if(abort)then
        call fcecho(' ')
        call fcecho('Aborting. Error from FXBARYCORRECT routine!')
        goto 999
      endif
      
c        call udmget(nb,7,itmjds,status)
c        if(status.ne.0)then
c           call fcecho('Error allocating memory for TMJDS')
c           call fcecho('Free memory by killing active processes.')
c           call fcerrm(status)
c           status=0
c           return
c        endif


999   continue
      return
      end


c**********************************************************************
c This subroutine reads in all of the input parameters and does
c some conversions on such things as the RA and DEC so that they
c are converted into meaningful numbers from strings. This conversion
c takes place in the C routine "parsedeg" which is actually in C
c called Pars_deg since on the VMS the FORTRAN and C names must be
c different or an error will result. Don't ask why - it is VMS...
c**********************************************************************
      
      subroutine gbaryparm(in_fil, ou_fil, eph_fil,
     &   orb_fil, timecol, starttime, stoptime, ra,
     &   dec, rastr, decstr, lbarytime, lsensecase,
     &   abort, status)
      
      implicit none
      integer status,raflag,decflag
      character*(*) in_fil,ou_fil,eph_fil,orb_fil
      character*(*) timecol, rastr, decstr
      character(20) cval
      character(40) contxt
      double precision starttime,stoptime,ra,dec
      double precision tempval
      logical abort, lbarytime, lsensecase
      abort=.FALSE.
      lbarytime=.TRUE.
      lsensecase=.FALSE.

C      initialize variables
      ra=0.0d0
      dec=0.0d0
      status=0
      tempval=0.0d0

C      get the name of the input FITS file
      call uclgst('in_file',in_fil,status)
      if (status .ne. 0) then
        call fcerr('could not get IN_FILE parameter')
        goto 999
      endif
      if(in_fil .eq. ' ' .or. in_fil .eq. '-')then
        call fcecho('IN_FILE value NOT SET! Aborting')
        abort=.TRUE.
        goto 999
      endif
      
C  get the name of the output root file
      call uclgst('out_file',ou_fil,status)
      if (status .ne. 0) then
        call fcerr('could not get OUT_FILE prefix parameter')
        goto 999
      endif
      if(ou_fil .eq. ' ' .or. ou_fil .eq. '-')then
        call fcecho('OUT_FILE value NOT SET! Aborting')
        abort=.TRUE.
        goto 999
      endif

C  get the name of the input file containing the ephemeris information. 
      call uclgst('eph_file',eph_fil,status)
      if (status .ne. 0) then
        call fcerr('could not get EPH_FILE parameter')
        goto 999
      endif
      if(eph_fil .eq. ' ' .or. eph_fil .eq. '-')then
        call fcecho(' ')
        call fcecho('EPH_FILE value NOT SET!')
        call fcecho('The ephermis 2000 data file is part of')
        call fcecho('the REFDATA distribution and has the name')
        call fcecho('de200_new.fits in that directory.')
        call fcecho('Cannot continue... Aborting...')
        abort=.TRUE.
        goto 999
      endif
      
C  get the name of the input file containing the XTE orbit information. 
      call uclgst('orbit_file',orb_fil,status)
      if (status .ne. 0) then
        call fcerr('could not get ORB_FIL parameter')
        goto 999
      endif
      if(orb_fil .eq. ' ' .or. orb_fil .eq. '-')then
        call fcecho(' ')
        call fcecho('ORBIT_FILE value NOT SET!')
        call fcecho('You MUST specify an orbit file to')
        call fcecho('perform barycentric corrections on your data.')
        call fcecho('This information is part of the XTE data,')
        call fcecho('and available on certain sites. Aborting!')
        abort=.TRUE.
        goto 999
      endif

C  get the name of the Right Ascension string
      call uclgst('ra_str',rastr,status)
      if (status .ne. 0) then
        call fcecho('could not get RA_STR parameter')
        call fcecho('Setting to blank.')
        status=0
        rastr=' '
      endif

      if(rastr.eq.'INDEF')rastr=' '
      if(rastr.eq.'-')rastr=' '

C  get the name of the Declination string
      call uclgst('dec_str',decstr,status)
      if (status .ne. 0) then
        call fcecho('could not get DEC_STR parameter')
        call fcecho('Setting to blank.')
        status=0
        decstr=' '
      endif

      if(decstr.eq.'INDEF')decstr=' '
      if(decstr.eq.'-')decstr=' '
      
C  get the time column string
      call uclgst('timecol',timecol,status)
      if (status .ne. 0) then
        call fcerr('could not get TIMECOL parameter - aborting')
        abort=.TRUE.
        goto 999
      endif

C  get the start time for inclusion
      call uclgsd('start_time',starttime,status)
      if (status .eq. 3) then
        call uclpst('start_time','INDEF',status)
        status = 0
        starttime=-1.0d0
      else if (status .ne. 0) then
        call fcecho('could not get START_TIME parameter')
        call fcecho('Setting to -1.0d0 - include all')
        starttime=-1.0d0
        status=0
      endif
      
C  get the end time for inclusion
      call uclgsd('end_time',stoptime,status)
      if (status .eq. 3) then
        call uclpst('end_time','INDEF',status)
        status = 0
        stoptime=-1.0d0
      else if (status .ne. 0) then
        call fcecho('could not get END_TIME parameter')
        call fcecho('Setting to -1.0d0 - include all')
        status=0
        stoptime=-1.0d0
      endif


c  get the logical that tells if we are to create a new column
c BARYTIME or overwrite the Time column...
      call uclgsb('barytime',lbarytime,status)
      if (status .ne. 0) then
        call fcecho('could not get BARYTIME parameter')
        call fcecho('Setting to TRUE.')
        status=0
        lbarytime=.TRUE.
      endif

c  get the logical that tells if we are to be case sensitive for
c input values - this if very dangerous to allow since it will make the
c output ambiguous in some instances but such is life...
      call uclgsb('sensecase',lsensecase,status)
      if (status .ne. 0) then
        call fcerr('could not get SENSECASE parameter')
        goto 999
      endif
      
c     RAFLAG is used to tell parsedeg that it is dealing with
c a right ascension so that the string may be expressed as hh mm sec.dec
c as well as deg.dec. This means that the hh mm sec.dec's are multiplied
c by 15 to convert into degrees, and returns the value deg.dec. 
      raflag=1

c     We use tempval to receive the value because if this is set
c to dec as in the declination calculation we get a "bus error"
c which cannot be explained or figured out so this is a work-around.
      if(rastr.ne.' ')then
        call parsedeg(rastr,tempval,raflag,status)
        if(status.ne.0)then
          status=0
          call fcecho(' ')
          call fcecho('Error in parsing RASTR value was noted. ')
          call fcecho('Value read in was ')
          call fcecho(rastr)
          call fcecho('Value returned after parsing was')
          contxt=' '
          call ftd2f(tempval,6,contxt,status)
          if(status.ne.0)then
            call fcecho('Error in converting RA to characters')
            call fcecho('Check RA value in output file...')
            status=0
          else
            call fcecho(contxt)
            call fcecho('Continuing using this value...')
          endif
        endif
        ra = tempval
        tempval = 0.0d0
        cval=' '
        call fcecho(' ')
        call fcecho('Setting RA value to: ')
        status=0
        call ftd2f(ra,7,cval,status)
        if(status.ne.0)then
          call fcecho('Error in RA to characters')
          status=0
        endif
        call fcecho(cval)

        if(ra.gt.360.0d0.or.ra.lt.-360.0d0)then
          call fcecho(' ')
          call fcecho('RA is greater than 360, or less than -360.0')
          call fcecho('Continuing using this value:')
          call fcecho(cval)
        endif

      endif

c     If DECFLAG equals 0, this means that the string may be expressed
c as deg mm sec.dec OR as deg.dec and it will convert and return the
c double precision value expressed as deg.dec.

      decflag=0

c     We use tempval to receive the value because if this is set
c to dec as in the declination calculation we get a "bus error"
c which cannot be explained or figured out so this is a work-around.
      if(decstr.ne.' ')then
        call parsedeg(decstr,tempval,decflag,status)
        if(status.ne.0)then
          status=0
          call fcecho(' ')
          call fcecho('Error in parsing DECSTR value was noted. ')
          call fcecho('Value read in was ')
          call fcecho(decstr)
          call fcecho('Value returned after parsing was')
          contxt=' '
          call ftd2f(tempval,6,contxt,status)
          if(status.ne.0)then
            call fcecho('Error in converting DEC to characters')
            call fcecho('Check DEC value in output file...')
            status=0
          else
            call fcecho(contxt)
            call fcecho('Continuing using this value...')
          endif
        endif
        dec = tempval
        tempval = 0.0d0
        cval=' '
        call fcecho(' ')
        call fcecho('Setting DEC value to: ')
        status=0
        call ftd2f(dec,7,cval,status)
        if(status.ne.0)then
          call fcecho('Error in DEC to characters')
          status=0
        endif
        call fcecho(cval)

        if(dec.gt.360.0d0.or.dec.lt.-360.0d0)then
          call fcecho(' ')
          call fcecho('DEC is greater than 360, or less than -360.0')
          call fcecho('Continuing using this value:')
          call fcecho(cval)
        endif

      endif

c      print*,'Done checking RA value'
c      print*,'OUT of GBARYPARM'
      
999   continue

      return
      end


c**********************************************************************
c
c     This subroutine does ALL of the work for the correction.
c**********************************************************************
      subroutine fxbarycorrect(in_fil, ou_fil, eph_fil, orb_fil,
     &   timecol, starttime, stoptime, ra, dec, rastr,
     &   decstr, lbarytime, lsensecase, abort, status)

      implicit none 
      character*(*) in_fil, ou_fil, eph_fil, orb_fil, timecol,
     &   rastr,decstr
      double precision starttime, stoptime, ra, dec, ra_rad, dec_rad
      integer status,nb,nt,isiz
      parameter (isiz=1000)
      parameter (nb=512)
      parameter (nt=1000)

      character(160) infile,oufile,ephfile,orbfile,
     &   ephfiles(isiz),orbfiles(nt),gfile
      integer extinfile,extoufile,extorbfile,no,extephfile,
     &   ikeywords,ephnofiles,orbnofiles,nogtiextensions
      logical abort,lfxbary, lfxtime, lbarytime, lsensecase,
     &   lstartorb, lstoporb, lendoffile
      character(256) tmpfile(1000)
      character(80) contxt
      character(70) cval

      integer iunit,ounit,ephunit,orbunit,block,xtend,outlen,
     &   fcstln,moreky,i,k,inewcol

      character(40) ttypei(nb),tformi(nb),tuniti(nb),extnam
      integer nrowsi,nfieldi,pcount,itimecol,nultyp
      logical flgval,anynul
      double precision nulvald,timeval
      character(40) timesyso,timesys
      integer nrowso,nfieldo
      character(40) ttypeorb(nb),tformorb(nb),tunitorb(nb),
     &   extnamorb,timesysorb
      integer nrowsorb,nfieldorb

      character(80) timeunit,objects(isiz)
      double precision timezero,conversiono,mjdref,timeorbzero,
     &   timezerof,timeorbzerof,conversionorb,
     &   fmjdref,forbmjdref,int_time,ftstarto,tstarto,
     &   ftstopo,tstopo

      double precision  orbmjdref,orbtmjds(isiz),orbtmjde(isiz),
     &   timeresolution
      integer iorbmjdref,itimezero
C      character(40) obstart,obstop,obsdate
C	by ZG to change the string length of obsdate
	character(68) obsdate
	character(40) obstart,obstop
      
      integer imjdref,itimeorbzero,barynum,
     &   itstarto,itstopo,nrowshold
      logical lstart,lstop,lseparate,lmultiple,lbailout

      common/multiple/lmultiple
      common/bail/lbailout
      common/timeres/timeresolution

      lmultiple=.FALSE.
      lbailout=.FALSE.
      timeresolution=0.0d0

      
c     Note that since we are basically copying the input file to
c the output file and then performing some modifications, we are never
c really working on the INPUT file. So we will actually read all values
c from the output file and perform modifications on it. So you will
c notice that input and output values are used almost interchangeably,
c with the exception of when we are writing final information to the
c output file. So when reading information in, to perform the barycentric
c correction, we will be reading from the OUTPUT file!

      call dinitial(isiz,orbtmjds)
      call dinitial(isiz,orbtmjde)

      ra_rad=0.0d0
      dec_rad=0.0d0
      barynum=0
      orbmjdref=0.0d0
      itstarto=0
      tstarto=0.0d0
      ftstarto=0.0d0
      int_time=0.0d0
      mjdref=0.0d0
      imjdref=0
      iorbmjdref=0
      fmjdref=0
      forbmjdref=0
      nogtiextensions=0
      gfile=' '
      
      obstart=' '
      obstop=' '
      obsdate=' '
      timesys=' '
      lseparate=.TRUE.
      lfxbary=.FALSE.
      lfxtime=.FALSE.
      itimezero=0
      timezerof=0.0d0
      timezero=0.0d0
      
      itimeorbzero=0
      timeorbzerof=0.0d0
      timeorbzero=0.0d0

      conversiono=0.0d0
      conversionorb=0.0d0
      lstart=.FALSE.
      lstop=.FALSE.
      lstartorb=.FALSE.
      lstoporb=.FALSE.
      moreky = 0
      abort = .FALSE.
      lendoffile = .FALSE.
      no=0
      ikeywords=3
      if(ra.ne.0.0d0)ikeywords=ikeywords+1
      if(dec.ne.0.0d0)ikeywords=ikeywords+1      
      
c     Let's assign some logical unit numbers for each file.
c      Assign a unit file number used in inputting file.
      call ftgiou(iunit,status)
      if(status.ne.0)then
        call fcecho('Error getting IN_FIL unit number')
        call fcecho('Setting to logical unit 10')
        status=0
        iunit=10
      endif

c      Assign a unit file number used in outputting file.
      call ftgiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error getting OU_FIL unit number')
        call fcecho('Setting to logical unit 11')
        status=0
        ounit=11
      endif
        
c      Assign a unit file number used in inputting file.
      call ftgiou(ephunit,status)
      if(status.ne.0)then
        call fcecho('Error getting EPH_FIL unit number')
        call fcecho('Setting to logical unit 12')
        status=0
        ephunit=12
      endif

c      Assign a unit file number used in inputting file.
      call ftgiou(orbunit,status)
      if(status.ne.0)then
        call fcecho('Error getting ORB_FIL unit number')
        call fcecho('Setting to logical unit 13')
        status=0
        orbunit=13
      endif

c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     Let's parse the input files and check to be sure that there is
c only one input and outputfile, then let's parse that file
c and get the extension number.
      
      call fcgcls(in_fil,tmpfile,no,abort)
      if(no.ne.1)then
        call fcecho(' ')
        call fcecho('Error!!! More than ONE in_fil specified!')
        call fcecho('Cannot handle more than one input file!!!')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      endif

      call fcpars(tmpfile(1),infile,extinfile,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing IN_FIL parameter!')
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call fcecho(' ')
      call fcecho('Input file is:')
      call fcecho(infile)

c     Let's examine the input file to determine if there are any
c GTI extensions that need to be taken care of.

      if(.not.lbarytime)then

        call fcecho(' ')
        call fcecho('Searching input file for GTI information.')
        call gtisetup(infile,gfile,nogtiextensions,2,abort)
        if(abort)then
          nogtiextensions=0
          abort=.FALSE.
        endif
      endif

c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 2 we
c      force it to 2. 
      if (extinfile.lt.1) extinfile=1
            
c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(iunit,infile,0,block,status)
      if(status.ne.0)then
        call fcerr('Failure to open input file - aborting')
        call fcerrm(status)
        status=0
        call ftclos(iunit,status)
        return
      endif

c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c     Let's get everything set up for the output file.
      no=0
      call fcgcls(ou_fil,tmpfile,no,abort)
      if(no.ne.1)then
        call fcecho(' ')
        call fcecho('Error!!! More than ONE ou_fil specified!')
        call fcecho('Cannot handle more than one file!!!')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      endif

      call fcpars(tmpfile(1),oufile,extoufile,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing OU_FIL parameter!')
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif
      if (extoufile.lt.1) extoufile=1
      outlen=fcstln(oufile)

      call fcecho(' ')
      call fcecho('Output file to be created is:')
      call fcecho(oufile(:outlen))
      call fcecho(' ')
      call fcecho('--------------------------------------------------')
      
      call ffinit(ounit,oufile(:outlen),status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error cannot create OUTPUT_FIL file')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

c     Okay copy the original data file from the INPUT file to
c the output file, up to the point of the data extension that is
c being processed!

      if (extinfile .eq. 1) then
        do 100 i = 1, extinfile+1
          if(i.ne.1)then
            if(status.eq.0)then
              call ftpcks(ounit,status)
              if(status.ne.0)status=0
            endif
            call ftcrhd (ounit, status)
          endif
          
          call ftmahd (iunit, i, xtend, status)
          if(i.eq.extinfile+1)then
            call ftcopy (iunit, ounit, ikeywords, status)
          else
            call ftcopy (iunit, ounit, 0, status)
          endif
          
100     continue
c
c 17Feb99 (MJT):
c need to flush the buffer since subsequent reads are going
c to be done on ounit!
c
        call ftflus(ounit,status)
        if (status .ne. 0) then
          call fcecho(' ')
          call fcecho('ERROR copying extensions')
          call fcecho('Apparently there are no extensions.')
          abort=.TRUE.
          goto 999
        endif
      else
        do 101 i = 1, extinfile
          if(i.ne.1)then
            if(status.eq.0)then
              call ftpcks(ounit,status)
              if(status.ne.0)status=0
            endif
            call ftcrhd (ounit, status)
          endif
          
          call ftmahd (iunit, i, xtend, status)
          if(i.eq.extinfile+1)then
            call ftcopy (iunit, ounit, ikeywords, status)
          else
            call ftcopy (iunit, ounit, 0, status)
          endif
101     continue
c
c 17Feb99 (MJT): see above
c
        call ftflus(ounit,status)
        if (status .ne. 0) then
          call fcecho(' ')
          call fcecho('ERROR copying extensions')
          call fcecho('Apparently there are no extensions.')
          abort=.TRUE.
          goto 999
        endif
      endif

c     Since the output file is identical to the input file except for the
c addition of the barycentric correction, we set the outputfile extension
c equal to the input file extension.
      extoufile=extinfile
      
c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(iunit,extinfile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile')
        call fcerrm(status)
        status=0
      endif

c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(ounit,extoufile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extoufile')
        call fcerrm(status)
        status=0
      endif

      call ftgcno(ounit,.FALSE.,'BARYTIME',barynum,status)
      if(status.eq.0)then
        lfxbary=.TRUE.
      else
        status=0
      endif

c     This will create and add special KEYWORDs that telling what
c has been done so other codes will know what to look for.

      call ftgkyl(iunit,'FXBARY',lfxbary,contxt,status)
      if(status.ne.0)then
        status=0
        call ftpkyl(ounit,'FXBARY',.TRUE.,
     &     'This file has been processed by FXBARY.',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating FXBARY keyword')
          status=0
        endif
        lfxbary=.FALSE.
      endif

      call ftgkyl(iunit,'FXTIME',lfxtime,contxt,status)
      if(status.ne.0)then
        status=0
        if(.not.lbarytime)then
          call ftpkyl(ounit,'FXTIME',.TRUE.,
     &     'Time column was overwritten, GTIs modified.',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error updating FXTIME keyword')
            status=0
          endif
c (MJT) 15Jun00 update TIMEREF kwd
          call ftukys(ounit,'TIMEREF','SOLARSYSTEM',
     &         'barycentric corrections applied to times',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error updating TIMEREF keyword')
            status=0
          endif
          lfxtime=.TRUE.
        else
          lfxbary=.FALSE.
        endif
      endif
      
c     If RA and DEC have been set by the USER than over-ride the
c values that are stored in input file. 
      if(dec.eq.0.0d0)then
        call ftgkyd(ounit,'DEC',dec,contxt,status)        
        if(status.ne.0)then
          status=0
          call ftgkyd(ounit,'DEC_PNT',dec,contxt,status)
          if(status.ne.0)then
            status=0
            call ftgkyd(ounit,'DEC_OBJ',dec,contxt,status)
            if(status.ne.0)then
              status=0
              call ftgkyd(ounit,'DEC_NOM',dec,contxt,status)
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error getting DEC keyword from file!')
                call fcecho('DEC set to 0.0')
                call fcecho('Use DECSTR in PAR file to override')          
                call fcecho('Setting DEC via DECSTR will NOT change')
                call fcecho('DEC in the output file. Use FPARKEY ')
                call fcecho('for that.')
                status=0
              endif
            endif
          endif
        endif
      else
        cval='DEC value used for BARYCENTER correction was:'
        cval(47:68)=decstr
        call ftpcom(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating DEC history')
          status=0
        endif
      endif

      if(ra.eq.0.0d0)then
c        print*,'status'
        call ftgkyd(ounit,'RA',ra,contxt,status)
c        print*,'RA is ',ra,status
        if(status.ne.0)then
          status=0
          call ftgkyd(ounit,'RA_PNT',ra,contxt,status)
c          print*,'RA_PNT is ',ra,status
          if(status.ne.0)then
            status=0
            call ftgkyd(ounit,'RA_OBJ',ra,contxt,status)
c            print*,'RA_OBJ is ',ra,status
            call fcerrm(status)
            if(status.ne.0)then
              status=0
              call ftgkyd(ounit,'RA_NOM',ra,contxt,status)
c              print*,'RA_NOM is ',ra,status
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error getting RA keyword from file!')
                call fcecho('RA set to 0.0')
                call fcecho('Use RASTR in PAR file to override')          
                call fcecho('Setting RA via RASTR does NOT change')
                call fcecho('RA in the output file. Use FPARKEY ')
                call fcecho('for that.')
                status=0
              endif
            endif
          endif
        endif
      else
        cval='RA value used for BARYCENTER correction was:'
        cval(46:67)=rastr
        call ftpcom(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating RA history')
          status=0
        endif
      endif

      call ftghbn(iunit,nb-1,nrowsi,nfieldi,ttypei,tformi,
     &   tuniti,extnam,pcount,status)
      if (status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not get NFIELD information')
        call fcerrm(status)
        abort=.TRUE.
        goto 999
      endif
      if(nrowsi.eq.0)then
        call fcecho(' ')
        call fcecho('This file contains no information under')
        call fcecho('the COLUMNS name or file is empty.')
      endif
      
c     Get TIME information out of the original input file
c since this will have to be taken into account.
      
      call ftgkys(iunit,'TIMEUNIT',timeunit,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMEUNIT'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        timeunit='s'
        status=0
      endif

      call timeconvert(timeunit,conversiono,status)
      if(status.ne.0)then
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        conversiono=1.0d0
        status=0
      endif

      if(conversiono.ne.1.0d0)then
        call fcecho('TIMEUNIT for INPUT file not seconds!')
        call fcecho('SAEXTRCT and SEEXTRCT ONLY USE SECONDS')
        call fcecho('There may be a problem with this input file!')
        call fcecho('KEYWORD value for TIMEUNIT read was')
        call fcecho(timeunit)
      endif
      
      call ftgkyd(ounit,'TIMEZERO',timezero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(ounit,'TIMEZERI',itimezero,contxt,status)
        call ftgkyd(ounit,'TIMEZERF',timezerof,contxt,status)
        if(status.eq.0)then
          timezero = timezerof+dfloat(itimezero)
        endif
      endif

      if(status.ne.0)then
        contxt='Could not find keyword TIMEZERO in INPUT file'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftgkyd(ounit,'MJDREF',mjdref,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(ounit,'MJDREFI',imjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFI')
          call fcecho('Proceeding with MJDREFI set to 0')
          imjdref=0
          status=0
        endif
        call ftgkyd(ounit,'MJDREFF',fmjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFF')
          call fcecho('Proceeding with MJDREFF set to 0.0d0')
          mjdref=0.0d0
          status=0
        endif
        mjdref=fmjdref+dfloat(imjdref)
      endif

      call ftgkyd(ounit,'TSTART',tstarto,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(ounit,'TSTARTI',itstarto,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TSTARTI')
          call fcecho('Proceeding with TSTARTI set to 0')
          itstarto=0
          status=0
        endif
        call ftgkyd(ounit,'TSTARTF',ftstarto,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TSTARTF')
          call fcecho('Proceeding with TSTARTF set to 0.0d0')
          tstarto=0.0d0
          status=0
        endif
          tstarto=ftstarto+dfloat(itstarto)
      endif

      call ftgkyd(ounit,'TSTOP',tstopo,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(ounit,'TSTOPI',itstopo,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TSTOPI')
          call fcecho('Proceeding with TSTOPI set to 0')
          itstopo=0
          status=0
        endif
        call ftgkyd(ounit,'TSTOPF',ftstopo,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for TSTOPF')
          call fcecho('Proceeding with TSTOPF set to 0.0d0')
          tstopo=0.0d0
          status=0
        endif
          tstopo=ftstopo+dfloat(itstopo)
      endif
      
c     Get the integration time from the INPUT file since these
c have to be used in modifying the TSTART and TSTOP values.
c When they are updated...

c      call ftgkyd(ounit,'TIMEDEL',int_time,contxt,status)
c      if(status.ne.0)then
c        call fcecho('Could not find TIMEDEL keyword')
c        call fcecho('This value is necessary...')
c        call fcecho('Will attempt to calculate it...')
c        int_time=0.0d0
c        status=0
c      else
c        int_time=(timezero-tstarto)*2.0d0
c      endif
      
c     Here we will search the input file for a match to timecol
c and insert a new column immediately after that which contains
c the amount of time that must be added to TIME to get back the
c original value in 'LOCAL' co-ordinates.
      
      inewcol=-10
c      do 20 i=1,nfieldi
c        if(ttypei(i).eq.timecol)itimecol=i
c20    continue

      itimecol=0
      call ftgcno(ounit,lsensecase,timecol,itimecol,status)
      if(itimecol.eq.0.or.status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR!!! Cannot find input TIME column!')
        call fcecho('Cannot continue! Aborting...')
        call fcecho(' ')
        call fcecho('The column name being searched for is:')
        call fcecho(timecol)
        call fcecho(' ')
        call fcecho('The columns that exist in this file are:')
        do i=1,nfieldi
          call fcecho(ttypei(i))
        enddo
        abort=.TRUE.
        goto 999
      endif

      call ftgkyl(iunit,'FXBARY',lfxbary,contxt,status)
      if(status.ne.0)then
        status=0
        lfxbary=.FALSE.
      endif

      call ftgkyl(iunit,'FXTIME',lfxtime,contxt,status)
      if(status.ne.0)then
        status=0
        lfxtime=.FALSE.
      endif
      
      if(lfxtime)then
        call fcecho('Error... Code has previously had the Time')
        call fcecho('column overwritten with BARYCENTER times.')
        call fcecho('Cannot unravel BARYCENTER corrected TIME')
        call fcecho('column back to original times. Aborting.')
        abort=.TRUE.
        goto 999
      endif

      call ftgkys(iunit,'TIMESYS',timesys,contxt,status)
      if(status.ne.0)then
        call fcecho('Could not find keyword TIMESYS in INPUT file')
        call fcerrm(status)
        status=0
      else
        if(timesys.ne.'TT')then
          call fcecho('ERROR!!!! ALL XTE files have TIMESYS as TT')
          call fcecho('This file does not! TIMESYS given as')
          call fcecho(timesys)
          if((timesys.eq.'TDB').or.(timesys.eq.'TCB'))then
            call fcecho(' ')
            call fcecho('This file has already been barycenter')
            call fcecho('corrected! You are attempting non-sense!')
            call fcecho('Do you understand what you are doing?')
          endif
          call fcecho('Cannot continue... Aborting...')
          abort=.TRUE.
          goto 999
        endif
      endif
      
      cval='This file was barycenter corrected by FXBARY_5.0.2'
      call ftpcom(ounit,cval,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error creating COMMENT keyword')
        status=0
      endif

c     Let's update the TIMESYS keyword if we are overwriting
c     the TIME column.
      if(.not.lbarytime)then
        contxt=' '
        contxt='Barycenter corrected file'
        call ftukys(ounit,'TIMESYS','TDB',contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating TIMESYS keyword')
          abort=.TRUE.
          goto 999
        endif
c     (MJT) 16Jun00: also update TIMEREF in this case
c           (won't bailout as above; seems too harsh...)
        call ftukys(ounit,'TIMEREF','SOLARSYSTEM',
     &       'barycentric corrections applied to times',status)
        if(status.ne.0)then
           call fcecho(' ')
           call fcecho('Error updating TIMEREF keyword')
           status=0
        endif
      endif
      
      if(lfxbary.and.lbarytime)then
        call fcecho(' ')
        call fcecho('Code has previously had BARYTIME column')
        call fcecho('created with BARYCERTER corrected times.')
        call fcecho('We will overwrite that column with new data.')
        inewcol=barynum
        nfieldo=nfieldi
        
        cval='BARYCENTER time updated in BARYTIME column'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif
        
      elseif((.not.lfxbary).and.lbarytime)then
        call fcecho(' ')
        call fcecho('Creating BARYTIME column')
        inewcol=nfieldi+1
        nfieldo=nfieldi+1
      
c     Now we are going to insert the NEW column information into
c the output file. But first we test to see if the FXBARY keyword
c is set in the original INPUT file since this will tell us if
c this file has ever been through FXBARY before. 
        call fticol(ounit,inewcol,'BARYTIME','1D',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error adding new column.. aborting')
          call fcerrm(status)
          abort=.TRUE.
          goto 999
        endif

        call ftpkns(ounit,'TUNIT',inewcol,1,'s'
     &     ,'physical unit for field',status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error adding new TUNIT value')
        endif
        
        cval='BARYCENTER time given in BARYTIME column'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif

      elseif((.not.lfxbary).and.(.not.lbarytime))then
        inewcol=itimecol
        nfieldo=nfieldi
        
        cval='BARYCENTER time given in TIME column'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif

      elseif(lfxbary.and.(.not.lbarytime))then
        call fcecho(' ')
        call fcecho('BARYTIME column previously created,')
        call fcecho('now you want to overwrite the Time column.')
        call fcecho(' ')
        call fcecho('There are several equally valid things that')
        call fcecho('can be done. I have opted to delete BARYTIME')
        call fcecho('and overwrite the TIME column.')
        call fcecho('If this is not what you wanted, re-examine ')
        call fcecho('the options. If you do NOT want the Time')
        call fcecho('column overwritten - rerun with the option')
        call fcecho('BARYTIME set to yes.')
        call fcecho(' ')
        call fcecho('Deleting previous BARYTIME column.')
        call ftdcol(ounit,barynum,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error deleting BARYTIME column')
          call fcecho('aborting....')
          abort=.TRUE.
          status=0
          goto 999
        endif
        inewcol=itimecol
        nfieldi=nfieldi-1
        
        cval='BARYTIME column removed from this file'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif

        cval='BARYCENTER time now given in TIME column'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif
        
      endif

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      if(starttime.ge.0.0d0)lstart=.TRUE.
      if(stoptime.ge.0.0d0)lstop=.TRUE.

c     Let's scan through the entire input file deleting the rows that
c do not match the starttime, and stoptime criteria if they are set
c otherwise this step is skipped completely thus speeding up the code
c hopefully... 
      if(lstart.or.lstop)then
        nultyp=0
        nulvald=0.0d0
        k=0
        nrowso=nrowsi
        do 30 i=1,nrowsi
          k=k+1

c         call ftgcld(iunit,itimecol,i,1,1,1,nultyp,
c    &       nulvald,timeval,flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(iunit,itimecol,i,1,1,
     &         nulvald,timeval,anynul,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error reading in TIME value from input file')
            call fcecho('Cannot continue.... aborting...')
            call fcerrm(status)
            abort=.TRUE.
            goto 999
          endif
          
          timeval=timeval+timezero

          if(lstart.and.lstop)then
            if(starttime.gt.stoptime)then
              call fcecho(' ')
              call fcecho('STARTTIME greater than STOPTIME!')
              call fcecho('Check your parameter file!')
              call fcecho('Cannot continue... ABORTING!!!')
              abort=.TRUE.
              goto 999
            endif
            if(timeval.lt.starttime.or.timeval.gt.stoptime)then
              call ftdrow(ounit,k,1,status)
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error!!! Could not delete row.')
                call fcecho('Check STARTTIME and STOPTIME inputs')
                call fcecho('Aborting....')
                abort=.TRUE.
                goto 999
              endif
              k=k-1
              nrowso=nrowso-1
            endif
          elseif(lstart)then
            if(timeval.lt.starttime)then
              call ftdrow(ounit,k,1,status)
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error!!! Could not delete row.')
                call fcecho('Check STARTTIME and STOPTIME inputs')
                call fcecho('Aborting....')
                abort=.TRUE.
                goto 999
              endif
              k=k-1
              nrowso=nrowso-1
            endif
          elseif(lstop)then
            if(timeval.gt.stoptime)then
              call ftdrow(ounit,k,1,status)
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Error!!! Could not delete row.')
                call fcecho('Check STARTTIME and STOPTIME inputs')
                call fcecho('Aborting....')
                abort=.TRUE.
                goto 999
              endif
              k=k-1
              nrowso=nrowso-1
            endif
          endif
30      continue

        if((nrowsi-nrowso).ge.nrowsi)then
          call fcecho(' ')
          call fcecho('ERROR you have filtered out ALL rows!!!!')
          call fcecho('Change value of STARTTIME or STOPTIME!')
          call fcecho('Nothing to DO!!! ABORTING...')
          abort=.TRUE.
          goto 999
        endif
      else
        nrowso=nrowsi
      endif

      call ftgkys(ounit,'TIMESYS',timesyso,contxt,status)
      if(status.ne.0)then
        call fcecho('Could not find keyword TIMESYS in INPUT file')
        call fcerrm(status)
        status=0
      else
        if(timesyso.ne.'TT'.and.timesyso.ne.'TDB')then
          call fcecho('ERROR!!!! ALL XTE files have TIMESYS as TT')
          call fcecho('This file does not! TIMESYS given as')
          call fcecho(timesyso)
          if((timesyso.eq.'TDB').or.(timesyso.eq.'TCB'))then
            call fcecho(' ')
            call fcecho('This file has already been barycenter')
            call fcecho('corrected! You are attempting non-sense!')
            call fcecho('Do you understand what you are doing?')
          endif
          call fcecho('Cannot continue... Aborting...')
          abort=.TRUE.
          goto 999
        endif
      endif

c       Finished filtering on input TIMES by the user. Continuing
c with the rest of the work that we have to do... Note that
c eventually we will have to deal with updating some of the keywords
c but since depending on the options selected different levels of
c updating will have to occur, so we will put it off for the moment.
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&        
      
        
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c     Let's handle all of the set-up for the Ephemeris file
      ephnofiles=0
      call fcgcls(eph_fil,ephfiles,ephnofiles,abort)
      if(ephnofiles.ne.1)then
        call fcecho(' ')
        call fcecho('More than ONE eph_fil specified!')
        call fcecho('This is an error! Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call fcpars(ephfiles(1),ephfile,extephfile,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing EPH_FIL parameter!')
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

c      call fcecho(' ')
c      call fcecho('Ephemeris file being used is:')
c      call fcecho(ephfile)

c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 2 we
c      force it to 2. 
      if (extephfile.lt.1) extephfile=1
            
c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(ephunit,ephfile,0,block,status)
      if(status.ne.0)then
        call fcerr('Failure to open Ephemeris file - aborting')
        call fcerrm(status)
        status=0
        call ftclos(ephunit,status)
        return
      endif
      
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c     Let's handle all of the set-up for the Orbit file  
      orbnofiles=0
      call fcgcls(orb_fil,orbfiles,orbnofiles,abort)
      if(orbnofiles.lt.1)then
        call fcecho(' ')
        call fcecho('Less than ONE orb_fil specified!')
        call fcecho('Attempting to sort files by TSTART.')
        call fcecho('Aborting....')
        abort=.TRUE.
        goto 999
      else
        if(orbnofiles.gt.nt)then
          call fcecho(' ')
          call fcecho('Why are you inputting more than 1000 files?')
          call fcecho('If you are unaware of when your observation ')
          call fcecho('occurred. use XDF, or try to narrow it down')
          call fcecho('to less than a 3 year period!!!!')
          call fcecho(' ')
          call fcecho('Aborting... ')
          abort=.TRUE.
          goto 999
        endif
      endif

c     Since we may have more than one ephemeris file that we will
c have to deal with, we have to sort them by times so that we can deal
c with them appropriately!
      call fcecho(' ')
      call fcecho('Sorting ORBIT files:')
      call chktime(orbfiles,objects,obstart,obstop,obsdate,
     &   orbmjdref,iorbmjdref,orbtmjds,orbtmjde,orbnofiles,
     &   status)
      call fcecho('Finished sorting ORBIT files...')
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('An Error sorting ORBFILES by time.')
        call fcecho('Cannot continue... Aborting...')
        abort=.TRUE.
        goto 999
      endif

      nrowshold=1

      if(ra_rad.eq.0.0d0.and.dec_rad.eq.0.0d0)then
        ra_rad = ra * (3.141592653589792d0 / 180.0d0)
        dec_rad = dec * (3.141592653589792d0 / 180.0d0)
      endif
c      print*,'RA and DEC after conversion', ra, dec

      
      do 200 k=1,orbnofiles
        call fxbaryopen(orbfiles(k),extorbfile,orbunit,nrowsorb,
     &     nfieldorb,ttypeorb,tformorb,tunitorb,extnamorb,
     &     orbmjdref,iorbmjdref,forbmjdref,timesysorb,
     &     conversionorb,timeorbzero,timeorbzerof,itimeorbzero,
     &     abort,status)

c        print*,'tstarto,orbtmjds(k)',tstarto,orbtmjds(k)
c        print*,'tstopo,orbtmjde(k)',tstopo,orbtmjde(k)

c     Let's check to be sure that the INPUT file and the IT
c file contain MJDREF values that are the same - they should since
c TIMESYS equals TT for all XTE satellite files, but let's check 
c just to be sure.
        if(orbmjdref.ne.mjdref)then
c          print*,'orbmjdref and mjdref are',
c     &       orbmjdref,mjdref
          call fcecho(' ')
          call fcecho('MJDREF for INPUT and ORBIT files differ!')
          call fcecho('This should NEVER happen with XTE files!')
          call fcecho('All times for INPUT and ORBIT must be in TT')
          call fcecho('Skipping this ORBIT file')
          goto 200
        endif

        if(.not.lstartorb)then
          if((tstarto.ge.orbtmjds(k)).and.
     &       (tstarto.le.orbtmjde(k)))lstartorb=.TRUE.
        endif

        if(.not.lstoporb)then
          if((tstopo.ge.orbtmjds(k)).and.
     &       (tstopo.le.orbtmjde(k)))lstoporb=.TRUE.
        endif
        
        if(lstartorb.or.lstoporb)then

          if(nrowshold.gt.nrowso)then
            call fcecho(' ')
            call fcecho('Have completed generating output file.')
            call fcecho('Terminating...')
            goto 999
          endif

          call fxbaryprocess(iunit,orbunit,ephunit,ounit,nrowsorb,
     &       nrowshold,nrowso,orbtmjds(k),orbtmjde(k),k,orbnofiles,
     &       tstarto,tstopo,
     &       conversiono,conversionorb,orbmjdref,timeorbzero,
     &       mjdref,imjdref,fmjdref,timezero,inewcol,itimecol,
     &       dec_rad,ra_rad,extephfile,nogtiextensions,
     &       lbarytime,lendoffile,abort,status)
          
        else

          call fcecho(' ')
          call fcecho('This orbit file is not within TSTART')
          call fcecho('and TSTOP of the file being processed.')
          call fcecho('Continuing to next orbit file!')
          
          if(tstarto.ge.orbtmjds(k).and.
     &       (tstarto.le.orbtmjds(k)))then
          else
c            call fcecho('This orbit file is not within TSTART')
c            call fcecho('and TSTOP of the file being processed.')
c            call fcecho('Failed TIME test for starting time')
c            call fcecho('(tstarto.ge.orbtmjds(k).and.(tstarto.le.orbtmjd
c     &s(k))')
c            call fcecho('Continuing to next orbit file!')
          endif
          if((tstopo.ge.orbtmjds(k)).and.
     &       (tstopo.le.orbtmjde(k)))then
          else
c            call fcecho('Failed TIME test for stopping time')
c            call fcecho('(tstopo.ge.orbtmjds(k).and.(tstopo.le.orbtmjds(
c     &k))')
c            call fcecho('This orbit file is not within TSTART')
c            call fcecho('and TSTOP of the file being processed.')
c            call fcecho('Continuing to next orbit file!')
          endif
        endif
        
200   continue

      call fcecho(' ')
      call fcecho('Completed generating output file.')
      call fcecho(' ')

      
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
c   This is the section that performs the major part of the operations.
c Only the columns pointed to by itime and inewcol will be modified!!!
c We are ready to actually begin...

c     Okay, now that we are finished generating the output file
c we will have to go back and do some clean-up work on it. The exact
c level of this depends on which options were selected. 

      
c
c
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call ftclos(iunit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not close input file')
        call fcecho(infile)
      endif

      status=0
      call ftpcks(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not update chksum keyword in output file')
      endif

      status=0
      call ftclos(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not close output file')
        call fcecho(oufile)
      endif

      status=0
      call ftclos(ephunit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not close Ephemeris file')
        call fcecho(ephfile)
      endif

      status=0
      call ftclos(orbunit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not close Orbit file')
        call fcecho(orbfile)
      endif
      status=0

999   continue

      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('An error was encountered.')
        call fcecho('Attempting to close all opened files')
        status=0
        call ftclos(iunit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not close input file')
          call fcecho(infile)
        endif
        status=0
c oedipus/cfitsio dies if ounit not open -- just skip this! (MJT 2Jun98)
c       call ftpcks(ounit,status)
c       if(status.ne.0)then
c         call fcecho(' ')
c         call fcecho('Could not update chksum keyword in output file')
c       endif
c       status=0
        call ftclos(ounit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not close output file')
          call fcecho(oufile)
        endif
        status=0
        call ftclos(ephunit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not close Ephemeris file')
          call fcecho(ephfile)
        endif
        status=0
        call ftclos(orbunit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Could not close Orbit file')
          call fcecho(orbfile)
        endif
        status=0
      endif

      call ftfiou(iunit,status)
      if(status.ne.0)then
        call fcecho('Error freeing IN_FIL unit number')
        status=0
      endif

      call ftfiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error freeing OU_FIL unit number')
        status=0
      endif

      call ftfiou(ephunit,status)
      if(status.ne.0)then
        call fcecho('Error freeing EPH_FIL unit number')
        status=0
      endif

      call ftfiou(orbunit,status)
      if(status.ne.0)then
        call fcecho('Error freeing ORB_FIL unit number')
        status=0
      endif

      return
      end


c**********************************************************************
c
c This subroutine will open and scan a list of files returning
c informatoin about MJDREF and TIMESYS and TIMEZERO information from
c them.
c**********************************************************************
      subroutine fxbaryopen(files,extfile,unit,nrows,
     &   nfield,ttype,tform,tunit,extnam,
     &   mjdref,imjdref,fmjdref,timesys,
     &   conversion,timezero,timezerof,itimezero,
     &   abort,status)

      implicit none
      
      integer status,nb,xtend,block,unit
      parameter (nb=512)
      character*(*) ttype(nb),tform(nb),tunit(nb),
     &   timesys,files,extnam
      character(160) file
      character(80) timeunit,contxt
      character(40) cgarf
      integer nrows,nfield,pcount,imjdref,itimezero,extfile
      double precision mjdref,fmjdref,timezero,timezerof,conversion
      logical abort

      cgarf=' '
c      print*,'Into FXBARYOPEN',unit,status,files,extfile
c     Since this routine will be called in cycles and will open
c each file and move to the proper position, we will attempt to close
c each file before opening the next one. If it fails this is a BIG
c problem, due to FITSIO. Thus we have to play stupid games to
c try to test for it. We cannot use "inquire" since IRAF won't work!!!
c     call ftgkys(unit,'CREATOR',cgarf,contxt,status)
c      print*,'Status is ',status
c     if(status.eq.0)then
        call ftclos(unit,status)
c     else
c       status=0
c     endif
c
c MJT 1Jun1998:
c This 'stupid game' crashes things under oedipus/CFITSIO since trying to 
c read a keyword from an unopened file fails rather than happily returning
c a status flag as FITSIO did. Unfortunately, this test never needed to be 
c done at all since closing an already closed (or never opened) file would 
c have worked fine all along... That's the fix I've made above.
      
      call fcpars(files,file,extfile,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error parsing parameter!')
        call fcecho('Check your parameter file!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 2 we
c      force it to 2. 
      if (extfile.lt.1) extfile=1

      call fcecho(' ')
      call fcecho('Orbit file being used is:')
      call fcecho(file)
      
c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(unit,file,0,block,status)
      if(status.ne.0)then
        call fcecho('Failure to open file - aborting')
        call fcerrm(status)
        status=0
        call ftclos(unit,status)
        return
      endif

c      Skip the primary header and go to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(unit,extfile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extfile')
        call fcerrm(status)
        status=0
      endif

      call ftghbn(unit,nb,nrows,nfield,ttype,tform,
     &   tunit,extnam,pcount,status)
      if (status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not get it file NFIELD information')
        call fcerrm(status)
        abort=.TRUE.
        goto 999
      endif
      if(nrows.eq.0)then
        call fcecho(' ')
        call fcecho('This it file contains no information!')
        call fcecho('Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgkys(unit,'TIMESYS',timesys,contxt,status)
      if(status.ne.0)then
        call fcecho('Could not find keyword TIMESYS in it file.')
        call fcerrm(status)
        status=0
      else
        if(timesys.ne.'TT')then
          call fcecho('ERROR!!!! ALL XTE files have TIMESYS as TT')
          call fcecho('or as TDB.')
          call fcecho('This file does not. TIMESYS given as')
          call fcecho(timesys)
          if((timesys.eq.'TDB').or.(timesys.eq.'TCB'))then
            call fcecho(' ')
            call fcecho('This file has already been barycenter')
            call fcecho('corrected! You are attempting non-sense!')
            call fcecho('Do you understand what you are doing?')
          endif          
          call fcecho('Cannot continue... Aborting')
          abort=.TRUE.
          goto 999
        endif
      endif

      call ftgkyd(unit,'TIMEZERO',timezero,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(unit,'TIMEZERI',itimezero,contxt,status)
        call ftgkyd(unit,'TIMEZERF',timezerof,contxt,status)
        if(status.eq.0)then
          timezero = timezerof+dfloat(itimezero)
        endif
      else
        call d2if(timezero,itimezero,timezerof,.FALSE.)
      endif

      if(status.ne.0)then
        contxt='Could not find keyword TIMEZERO in IT file'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif
      
      call ftgkys(unit,'TIMEUNIT',timeunit,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword TIMEUNIT in IT file'
        call fcecho(contxt)
        contxt='Assuming all inputs in SECONDS'
        call fcecho(contxt)
        timeunit='s'
        status=0
      endif

      call timeconvert(timeunit,conversion,status)
      if(status.ne.0)then
        contxt='Could not determine TIMEUNIT conversion'
        call fcecho(contxt)
        contxt='Assuming all inputs are in seconds'
        call fcecho(contxt)
        conversion=1.0d0
        status=0
      endif

      if(conversion.ne.1.0d0)then
        call fcecho('TIMEUNIT for IT file not seconds!')
        call fcecho('continuing with TIMEUNIT from IT file as')
        call fcecho(timeunit)
      endif

      call ftgkyd(unit,'MJDREF',mjdref,contxt,status)
      if(status.ne.0)then
        status=0
        call ftgkyj(unit,'MJDREFI',imjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFI')
          call fcecho('Proceeding with MJDREFI set to 0')
          imjdref=0
          status=0
        endif
        call ftgkyd(unit,'MJDREFF',fmjdref,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find reference for MJDREFF')
          call fcecho('Proceeding with MJDREFF set to 0.0d0')
          mjdref=0.0d0
          status=0
        endif
        mjdref=fmjdref+dfloat(imjdref)
      else
        call d2if(mjdref,imjdref,fmjdref,.FALSE.)
      endif

999   continue
      
      return
      end


c**********************************************************************
c
c This is the main processing routine. We first open up the appropriate
c orbit file and then perform a spline fit on the co-ordinates of the
c satellite position as a function of every 60 seconds. This the position
c at times is then calculated and those co-ordinates are then
c used as input into BARYCEN which calculates the barycentric correction.
c This correction term is then written out to the BARYTIME column. This
c time also has the TIMEZERO value removed from it so that you can
c determine by comparison to the TIME column how much of a correction was
c made.
c**********************************************************************
      
      subroutine fxbaryprocess(iunit,orbunit,ephunit,ounit,nrowsorb,
     &   nrowshold,nrowso,orbtmjds,orbtmjde,orbfilecount,orbfileno,
     &   tstarto,tstopo,
     &   conversiono,conversionorb,orbmjdref,timeorbzero,
     &   mjdref,imjdref,fmjdref,timezero,inewcol,itimecol,
     &   dec,ra,extephfile,nogtiextensions,
     &   lbarytime,lendoffile,abort,status)

      implicit none
      integer nt,inovals,nb,lastext
      
      parameter (inovals = 1000)
      parameter (nt = 15000)
      parameter (nb = 512)
      

      character(40) ttype(nb),tform(nb),tunit(nb),extnam
      integer nrows,nfield,pcount,nultyp,
     &   startpos,stoppos,lrow,nogtiextensions
      logical flgval,anynul
      double precision nulvald
      character(40) timesys,timeunit
      character(70) cval
      
      integer iunit,orbunit,ephunit,ounit,nrowsorb,imjdref,xtend,
     &   nrowshold,nrowso,itimecol,inewcol,extephfile,status,
     &   itstarti,itstopi,itimezero,orbfilecount,orbfileno
      double precision orbtmjds,orbtmjde,tstarto,tstopo,
     &   conversiono,conversionorb,orbmjdref,timeorbzero,
     &   mjdref,timezero,dec,ra,fmjdref,tstart,tstartf,tstop,
     &   tstopf,timezerof

      double precision time(nt),x(nt),c1x(nt),c2x(nt),c3x(nt),
     &   y(nt),c1y(nt),c2y(nt),c3y(nt),z(nt),c1z(nt),c2z(nt),
     &   c3z(nt)
      double precision timeuncorrected,posvec(3),
     &   splint,timereadval,
     &   timeuncorrectedf,timecorrected,barytime,ftimecorrected
c     &   ,dir(3)
      integer jd,jdcor,itimeuncorrected,itimecorrected
      double precision frc,frccor

      double precision timeuncorr(inovals),ftimecorr(inovals),
     &   timecorr(inovals)
c!!!!! MJT 15July96 (g77/linux) novals was declared as logical, but
c!!!!!               seems never to be used as anything but integer
      integer xorb,yorb,zorb,k,i,itimeorb,ierr,
     &   itimecorr(inovals),extfile,novals
      logical abort,lbarytime,
     &   lfxbary, lfxtime, lendoffile
      character(80) contxt


      common/spline_stuff/time,x,c1x,c2x,c3x,
     &   y,c1y,c2y,c3y,z,c1z,c2z,
     &   c3z
      
      tstart=0.0d0
      tstartf=0.0d0
      itstarti=0
      tstop=0.0d0
      tstopf=0.0d0
      itstopi=0
      lfxbary=.FALSE.
      lfxtime=.FALSE.

      if(lendoffile)then
        call fcecho(' ')
        call fcecho('Output file has been completed and closed.')
        call fcecho('Nothing more to do. Returning to calling routine.')
        return
      endif
      
      cval=' '
      
      ierr=0
      
      do i=1,inovals
        itimecorr(i)=0
      enddo
      
      call dinitial(inovals,timeuncorr)
      call dinitial(inovals,ftimecorr)
      call dinitial(inovals,timecorr)
      
c-------------------------------------------------------------------
c Now calculate the direction vector of the source from RA and DEC.
c This value is input to xtebarycen to keep that code from having
c to recalculate this on every iteration. 
c-------------------------------------------------------------------

c     Okay, now that we have the RA and DEC in degrees lets convert
c it into RADIANS so that it is ready to be input to the barycenter
c routines.
      
c      dir(1) = DCOS(dec)*DCOS(ra)
c      dir(2) = DCOS(dec)*DSIN(ra)
c      dir(3) = DSIN(dec)

      if(nt.lt.nrowsorb)then
        call fcecho(' ')
        call fcecho('Too many elements in ORBIT file!')
        call fcecho('Maximum number of elements 864 seconds')
        call fcecho('Increase value of NT in FXBARYPROCESS!')
      endif
      
c----------------------------------------------------------------------
c     At this point we are going to search for which columns in the ORBIT
c file contains the Time information and the information about the X,Y,
c and Z coordinates.
      
      call ftgcno(orbunit,.FALSE.,'Time',itimeorb,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR finding Time column in ORBIT file')
        call fcecho('Cannot continue.... Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgcno(orbunit,.FALSE.,'X',xorb,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR finding X column in ORBIT file')
        call fcecho('Cannot continue.... Aborting...')
        abort=.TRUE.
        goto 999
      endif

      call ftgcno(orbunit,.FALSE.,'Y',yorb,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR finding Y column in ORBIT file')
        call fcecho('Cannot continue.... Aborting...')
        abort=.TRUE.
        goto 999
      endif
      
      call ftgcno(orbunit,.FALSE.,'Z',zorb,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR finding Z column in ORBIT file')
        call fcecho('Cannot continue.... Aborting...')
        abort=.TRUE.
        goto 999
      endif

c     Now that we know the positions of the information we are going
c to read through the entire file and load up the arrays with the
c proper information about the Time and the X, Y, and Z coordinates.
c We are doing this to prepare for calling the spline fitting routines
c splinb which will calculate the necessary coefficients for performing
c a spline fit.
      nultyp=0
      nulvald=0.0d0

      call dinitial(nt,time)
      call dinitial(nt,x)
      call dinitial(nt,y)
      call dinitial(nt,z)

      i=0
      do 10 k = 1, nrowsorb
        i=i+1
c       call ftgcld(orbunit,itimeorb,k,1,1,1,nultyp,
c    &     nulvald,time(k),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(orbunit,itimeorb,k,1,1,
     &       nulvald,time(k),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error reading in TIME value from ORBIT file')
          call fcecho('Cannot continue.... aborting...')
          abort=.TRUE.
          goto 999
        endif
          
        time(k)=time(k)+timeorbzero
        time(k)=time(k)*conversionorb

c       call ftgcld(orbunit,xorb,k,1,1,1,nultyp,
c    &     nulvald,x(k),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(orbunit,xorb,k,1,1,nulvald,x(k),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error reading in X value from ORBIT file')
          call fcecho('Cannot continue.... aborting...')
          abort=.TRUE.
          goto 999
        endif
        
c       call ftgcld(orbunit,yorb,k,1,1,1,nultyp,
c    &     nulvald,y(k),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(orbunit,yorb,k,1,1,nulvald,y(k),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error reading in Y value from ORBIT file')
          call fcecho('Cannot continue.... aborting...')
          abort=.TRUE.
          goto 999
        endif
        
c       call ftgcld(orbunit,zorb,k,1,1,1,nultyp,
c    &     nulvald,z(k),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(orbunit,zorb,k,1,1,nulvald,z(k),anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error reading in Z value from ORBIT file')
          call fcecho('Cannot continue.... aborting...')
          abort=.TRUE.
          goto 999
        endif

10    continue

      call dinitial(nt,c1x)
      call dinitial(nt,c2x)
      call dinitial(nt,c3x)

      call dinitial(nt,c1y)
      call dinitial(nt,c2y)
      call dinitial(nt,c3y)

      call dinitial(nt,c1z)
      call dinitial(nt,c2z)
      call dinitial(nt,c3z)
      
      call fcecho(' ')
      call fcecho('Finished reading in ORBITAL information.')
      call fcecho(' ')
      call fcecho('Calculating spline coefficients for X co-ords.')

      call splinb(time,x,c1x,c2x,c3x,nrowsorb)

      call fcecho('Calculating spline coefficients for Y co-ords.')
      call splinb(time,y,c1y,c2y,c3y,nrowsorb)

      call fcecho('Calculating spline coefficients for Z co-ords.')
      call splinb(time,z,c1z,c2z,c3z,nrowsorb)

      call fcecho('Generated list of all coefficients for spline fit.')

      call fcecho(' ')
      call fcecho('Generating satellite position co-ordinates from')
      call fcecho('ORBIT file at time values in the INPUT file.')

c----------------------------------------------------------------------      
c
c----------------------------------------------------------------------
c     Now we will read in the input file times and generate the X, Y,
c and Z coordinates at those time values.
      
c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(ephunit,extephfile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extephfile')
        call fcerrm(status)
        status=0
      endif

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c This is were we begin our loop of reading all times and converting
c them into barycentric times and writing them out.
      
      do 30 k = nrowshold,nrowso
        
c       call ftgcld(ounit,itimecol,k,1,1,1,nultyp,
c    &     nulvald,timereadval,flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
        call ftgcvd(ounit,itimecol,k,1,1,
     &     nulvald,timereadval,anynul,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error reading in TIME value from INPUT file')
          call fcecho('Cannot continue.... aborting...')
          call fcerrm(status)
          abort=.TRUE.
          goto 999
        endif

c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c       This is the section what we actually calculate TIME information in.
        
        timeuncorrected=timereadval
        timeuncorrected=timeuncorrected+timezero
        timeuncorrected=timeuncorrected*conversiono

c       Test to see if we are outside of the time range of this
c orbit file. If we are than jump out of this look and go on to
c the next part of the code which is designed for modifying the
c GTI at the end of the file. 
        if(timeuncorrected.lt.orbtmjds.or.
     &     timeuncorrected.gt.orbtmjde)then

          if(orbfileno.eq.orbfilecount)then
            if(.not.lendoffile)nrowshold=nrowso
            goto 31
          else
            nrowshold=k
            return
          endif
          
        endif
        
        posvec(1)=splint(timeuncorrected,time,x,c1x,c2x,c3x,nrowsorb)
        posvec(2)=splint(timeuncorrected,time,y,c1y,c2y,c3y,nrowsorb)
        posvec(3)=splint(timeuncorrected,time,z,c1z,c2z,c3z,nrowsorb)    

c      Add the 0.5 d0 that you have to add to julian days to
c break up the integer and real part properly. So we subtract 0.5 from
c the fractional part and add a full day to the integer part. Kind of
c screwy but it turns out to be the best way to do this. 
        
        timeuncorrected=(timeuncorrected/86400.0d0) - 0.5d0
        
        call d2if(timeuncorrected,itimeuncorrected,
     &     timeuncorrectedf,.TRUE.)

        jd=itimeuncorrected+imjdref+2400001
        frc=timeuncorrectedf+fmjdref

c     Since the subroutine to calculate the ephemeris must have a frac-
c     tional time from -0.5 --> 0.5 we have to fix our time to be sure.
        if(frc.gt.0.5)then
          frc = frc - 1.0d0
          jd = jd + 1
        endif
        if(frc.lt.-0.5)then
          frc = frc + 1.0d0
          jd = jd - 1
        endif
        
        call xtebarycen(ephunit,.TRUE.,ra,dec,jd,frc,posvec,
     &     jdcor,frccor,ierr)

c----------------------------------------------------------------------
c        timeuncorrected=(timeuncorrected/86400.0d0) + 0.5d0

c        timeuncorrected=2448799.51839412190d0 +
c     &     (58.1840d0/864000.0d0)
c        call d2if(timeuncorrected,itimeuncorrected,timeuncorrectedf,.TRUE.)
c        jd=itimeuncorrected
c        jd=2448799+1
c        frc=(0.51839412190d0+6.734259259D-4) - 1.0d0
c        ra=164.49545800d0
c        dec=-52.44892800d0
c        print*,'Ra and Dec is ',ra,dec        
c        ra = ra * (3.141592653589792d0 / 180.0d0)
c        dec = dec * (3.141592653589792d0 / 180.0d0)
        
c        print*,'JD time is ',jd
c        print*,'Frac JD time is ',frc
c        print*,'Ra and Dec in radians is ',ra,dec
c        posvec(1)=0.0d0
c        posvec(2)=0.0d0
c        posvec(3)=0.0d0
        
c        call xtebarycen(ephunit,.FALSE.,ra,dec,jd,frc,posvec,
c     &     jdcor,frccor,ierr)

c----------------------------------------------------------------------
        
        if(ierr.ne.0)then
          call fcecho(' ')
          call fcecho('Error in BARYCEN routine!!!')
          call fcecho('Cannot continue... Aborting!')
          abort=.TRUE.
          goto 999
        endif

        jdcor = jdcor - imjdref - 2400001
        frccor = frccor - fmjdref + 0.5d0
        
c       Since there is a 0.5 day offset we have to see that the
c fractional part is large enough to subtract that much, if not
c we have to subtract 1 from the integer part and then just add
c 0.5d0 to the fractional part.

        call d2if(timecorrected,jdcor,frccor,.FALSE.)

        timecorrected = timecorrected * 86400.0d0
        timecorrected = timecorrected/conversiono
        timecorrected = timecorrected-timezero

c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        
        barytime=timecorrected

        call d2if(timecorrected,itimecorrected,
     &     ftimecorrected,.TRUE.)
        
        call ftpcld(ounit,inewcol,k,1,1,barytime,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error writing BARYTIME value')
          call fcecho('into output file... Aborting...')
          abort=.TRUE.
          goto 999
        endif
        
30    continue

      nrowshold=nrowso

c     We have finished our task of writing out the primary extension
c with a barycentric time correction having been performed. Now we will
c have to deal with the other aspects of having performed such a conversion.
c What we have to do not depends upon what the parameter "barytime" was
c set to be - if a barycentric time column was created and written to, then
c we are finished since all TIME related keywords are unchanged, but if
c we have overwritten the TIME column then we will have to change all time
c related keywords throughout this extension AND search for a GTI extension
c and make changes to that extension if it is found.
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c     This is where we start our search for TIME related keywords and
c modify/update those to agree with the new time related stuff...
      
      if(.not.lbarytime)then
c       Get TSTART from the data extension
        
        call ftgkyd(ounit,'TSTART',tstart,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(ounit,'TSTARTI',itstarti,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTARTI')
            call fcecho('Proceeding with TSTARTI set to 0')
            itstarti=0
            status=0
          endif
          call ftgkyd(ounit,'TSTARTF',tstartf,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTARTF')
            call fcecho('Proceeding with TSTARTF set to 0.0d0')
            tstartf=0.0d0
            status=0
          endif
          tstart=tstartf+dfloat(itstarti)
        endif

c       Get TSTOP from the data extension
        
        call ftgkyd(ounit,'TSTOP',tstop,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(ounit,'TSTOPI',itstopi,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTOPI')
            call fcecho('Proceeding with TSTOPI set to 0')
            itstopi=0
            status=0
          endif
          call ftgkyd(ounit,'TSTOPF',tstopf,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTOPF')
            call fcecho('Proceeding with TSTOPF set to 0.0d0')
            tstopf=0.0d0
            status=0
          endif
          tstop=tstopf+dfloat(itstopi)
        endif

        timeuncorr(1)=tstart
        timeuncorr(2)=tstop
        novals=2
        
        call correct_times(timeuncorr,novals,conversiono,
     &   imjdref,fmjdref,timezero,dec,ra,ounit,ephunit,nrowsorb,
     &   itimecorr, ftimecorr, timecorr, lbarytime, abort, status)


        tstart=timecorr(1)
        tstop=timecorr(2)
        
        call ftgkyd(ounit,'TSTART',tstart,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTART',timecorr(1),15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTART value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call d2if(tstart,itimecorrected,
     &     ftimecorrected,.TRUE.)
        
        call ftgkyj(ounit,'TSTARTI',itstarti,contxt,status)
        if(status.eq.0)then
          call ftmkyj(ounit,'TSTARTI',itimecorrected,
     &       contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTARTI value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTARTF',tstartf,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTARTF',ftimecorrected,
     &       15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTARTF value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTOP',tstop,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTOP',timecorr(2),15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOP value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call d2if(tstop,itimecorrected,
     &     ftimecorrected,.TRUE.)
        
        call ftgkyj(ounit,'TSTOPI',itstopi,contxt,status)
        if(status.eq.0)then
          call ftmkyj(ounit,'TSTOPI',itimecorrected,
     &       contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOPI value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTOPF',tstopf,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTOPF',ftimecorrected,
     &       15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOPF value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

      endif
      
      
31    continue

      
      if(((nrowshold.eq.1).or.(nrowshold.eq.nrowso)).and.
     &   (.not.lendoffile))then

        call ftghdn(ounit,extfile)
        
32      call ftmrhd(iunit,1,xtend,status)

        if(status.eq.0)then
          call ftpcks(ounit,status)
          if(status.ne.0)status=0
        endif
        
        call ftcrhd(ounit,status)
        call ftcopy(iunit,ounit,0,status)
        if(status.ne.0)then
          call ftghdn(iunit,lastext)
          goto 33
        endif
        goto 32
        
      endif

33    continue

      if(status.ne.0)status=0

      if((.not.lbarytime).and.(nogtiextensions.ne.0).and.
     &   (.not.lendoffile))then
        call fcecho(' ')
        call fcecho('Now that the main data extension is complete')
        call fcecho('we have to modify the GTI extension, if there are')
        call fcecho('any. Since the TIME column is being modified,')
        call fcecho('all associated KEYWORDS and columns are modified.')

 34     call ftmahd(ounit,extfile+1,xtend,status)
        if(status.ne.0)then
          call fcerr('Error moving to extfile')
          call fcerrm(status)
          status=0
          abort=.TRUE.
          goto 999
        endif

c     This will create and add special KEYWORDs telling what
c has been done so other codes will know what to look for.

        call ftgkyl(ounit,'FXBARY',lfxbary,contxt,status)
        if(status.ne.0)then
          status=0
          call ftpkyl(ounit,'FXBARY',.TRUE.,
     &       'This file has been processed by FXBARY.',status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error updating FXBARY keyword')
            status=0
          endif
          lfxbary=.FALSE.
        endif

        
        call ftgkyl(ounit,'FXTIME',lfxtime,contxt,status)
        if(status.ne.0)then
          status=0
          if(.not.lbarytime)then
            call ftpkyl(ounit,'FXTIME',.TRUE.,
     &         'Time column was overwritten, GTIs modified.',status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error updating FXTIME keyword')
              status=0
            endif
            lfxtime=.TRUE.
          else
            lfxbary=.FALSE.
          endif
        endif
        
        cval='This file was barycenter corrected by FXBARY_5.0.2.'
        call ftpcom(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error creating COMMENT keyword')
          status=0
        endif
        
        cval='ALL GTI information was modified!'
        call ftphis(ounit,cval,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error updating HISTORY keyword')
          status=0
        endif
        
c      Skip the primary header, and data extension and go to 
c      to read all pertinent processing information.
c        print*,'mjt2:moving to extension ',extfile+1,' in ounit'
c        call ftmahd(ounit,extfile+1,xtend,status)
c        if(status.ne.0)then
c          call fcerr('Error moving to extfile')
c          call fcerrm(status)
c          status=0
c          abort=.TRUE.
c          goto 999
c        endif

        call ftgkys(ounit,'TIMESYS',timesys,contxt,status)
        if(status.ne.0)then
          call fcecho('Could not find keyword TIMESYS in this file.')
          call fcerrm(status)
          status=0
        else
          if(timesys.ne.'TT')then
            call fcecho('ERROR!!!! ALL XTE files have TIMESYS as TT')
            call fcecho('This file does not. TIMESYS given as')
            call fcecho(timesys)
            if((timesys.eq.'TDB').or.(timesys.eq.'TCB'))then
              call fcecho(' ')
              call fcecho('This file has already been barycenter')
              call fcecho('corrected! You are attempting nonsense!')
              call fcecho('Do you understand what you are doing?')
            endif
            call fcecho('Cannot continue... Aborting')
            abort=.TRUE.
            goto 999
          endif
        endif

c     Let's update the TIMESYS keyword if we are overwriting
c     the TIME column.
        if(.not.lbarytime)then
           contxt=' '
           contxt='Barycenter corrected file'
           call ftukys(ounit,'TIMESYS','TDB',contxt,status)
           if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error updating TIMESYS keyword in GTI')
              abort=.TRUE.
              goto 999
           endif
c     (MJT) 16Jun00: also update TIMEREF in this case
c           (won't bailout as above; seems too harsh...)
           call ftukys(ounit,'TIMEREF','SOLARSYSTEM',
     &          'barycentric corrections applied to times',status)
           if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Error updating TIMEREF keyword in GTI')
              status=0
           endif
        endif
        
        call ftgkys(ounit,'TIMEUNIT',timeunit,contxt,status)
        if(status.ne.0)then
          call fcecho(' ')
          contxt='Could not find keyword TIMEUNIT'
          call fcecho(contxt)
          contxt='Assuming all inputs in SECONDS'
          call fcecho(contxt)
          timeunit='s'
          status=0
        endif

        call timeconvert(timeunit,conversiono,status)
        if(status.ne.0)then
          call fcecho(' ')
          contxt='Could not determine TIMEUNIT conversion'
          call fcecho(contxt)
          contxt='Assuming all inputs are in seconds'
          call fcecho(contxt)
          conversiono=1.0d0
          status=0
        endif
      
c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
        call ftghbn(ounit,nb,nrows,nfield,ttype,
     &     tform,tunit,extnam,pcount,status)
      
c      Print out any error information about accessing the files
        if (status.ne.0)then
          call fcecho(' ')
          contxt='Could not find TFIELDS from GTI extension'
          call fcecho(contxt)
          call fcerrm(status)
          return
        endif


c       Get the TIMEZERO keyword from this extension.
        call ftgkyd(ounit,'TIMEZERO',timezero,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(ounit,'TIMEZERI',itimezero,contxt,status)
          call ftgkyd(ounit,'TIMEZERF',timezerof,contxt,status)
          if(status.eq.0)then
            timezero = timezerof+dfloat(itimezero)
          endif
        endif

        if(status.ne.0)then
          contxt='Could not find keyword TIMEZERO in GTI extension'
          call fcecho(contxt)
          call fcecho('Setting TIMEZERO value to 0.0d0')
          timezero=0.0d0
          status=0
        endif
        
c       Get TSTART from the data extension
        
        call ftgkyd(ounit,'TSTART',tstart,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(ounit,'TSTARTI',itstarti,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTARTI')
            call fcecho('Proceeding with TSTARTI set to 0')
            itstarti=0
            status=0
          endif
          call ftgkyd(ounit,'TSTARTF',tstartf,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTARTF')
            call fcecho('Proceeding with TSTARTF set to 0.0d0')
            tstartf=0.0d0
            status=0
          endif
          tstart=tstartf+dfloat(itstarti)
        endif

c       Get TSTOP from the data extension
        
        call ftgkyd(ounit,'TSTOP',tstop,contxt,status)
        if(status.ne.0)then
          status=0
          call ftgkyj(ounit,'TSTOPI',itstopi,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTOPI')
            call fcecho('Proceeding with TSTOPI set to 0')
            itstopi=0
            status=0
          endif
          call ftgkyd(ounit,'TSTOPF',tstopf,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find reference for TSTOPF')
            call fcecho('Proceeding with TSTOPF set to 0.0d0')
            tstopf=0.0d0
            status=0
          endif
          tstop=tstopf+dfloat(itstopi)
        endif

        timeuncorr(1)=tstart
        timeuncorr(2)=tstop
        novals=2
        
        call correct_times(timeuncorr,novals,conversiono,
     &   imjdref,fmjdref,timezero,dec,ra,ounit,ephunit,nrowsorb,
     &   itimecorr, ftimecorr, timecorr, lbarytime, abort, status)


        tstart=timecorr(1)
        tstop=timecorr(2)
        
        call ftgkyd(ounit,'TSTART',tstart,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTART',timecorr(1),15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTART value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call d2if(tstart,itimecorrected,
     &     ftimecorrected,.TRUE.)

        call ftgkyj(ounit,'TSTARTI',itstarti,contxt,status)
        if(status.eq.0)then
          call ftmkyj(ounit,'TSTARTI',itimecorrected,
     &       contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTARTI value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTARTF',tstartf,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTARTF',ftimecorrected,
     &       15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTARTF value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTOP',tstop,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTOP',timecorr(2),15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOP value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call d2if(tstop,itimecorrected,
     &     ftimecorrected,.TRUE.)

        call ftgkyj(ounit,'TSTOPI',itstopi,contxt,status)
        if(status.eq.0)then
          call ftmkyj(ounit,'TSTOPI',itimecorrected,
     &       contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOPI value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

        call ftgkyd(ounit,'TSTOPF',tstopf,contxt,status)
        if(status.eq.0)then
          call ftmkyd(ounit,'TSTOPF',ftimecorrected,
     &       15,contxt,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Cannot modify TSTOPF value!')
            call fcecho('Aborting...')
            status=0
            abort=.TRUE.
            goto 999
          endif
        else
          status=0
        endif

c       Now that we have updated the TSTART and TSTOP keywords we have to
c proceed to update the actual data columns themselves.
c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls
        nultyp=0
        nulvald=0.0d0

        
        call ftgcno(ounit,.FALSE.,'START',startpos,status)
        if(status.ne.0)then
          status=0
c          call ftgcno(ounit,exact,gtistart,startpos,status)
        endif
           
        if(status.ne.0)then
          call fcecho(' ')
          contxt='Could not find START column number'
          call fcecho(contxt)
          contxt='aborting... cannot continue'
          call fcecho(contxt)
          call fcerrm(status)
        endif

        call ftgcno(ounit,.FALSE.,'STOP',stoppos,status)
        if(status.ne.0)then
          status=0
c          call ftgcno(ounit,exact,gtistop,stoppos,status)
        endif
        
        if(status.ne.0)then
          call fcecho(' ')
          contxt='Could not find STOP column number'
          call fcecho(contxt)
          contxt='Check input data'
          call fcecho(contxt)
          contxt='aborting... cannot continue'
          call fcecho(contxt)
          call fcerrm(status)
        endif
c      print*,'startpos and stoppos are',startpos,stoppos
        
        do lrow=1,nrows
        
c      Read in a double precision time element stored in
c      the input file into "timearay" 
c         call ftgcld(ounit,startpos,lrow,1,1,1,
c    &       nultyp,nulvald,timeuncorr(1),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(ounit,startpos,lrow,1,1,
     &       nulvald,timeuncorr(1),anynul,status)
         
c         call ftgcld(ounit,stoppos,lrow,1,1,1,
c    &       nultyp,nulvald,timeuncorr(2),flgval,anynul,status)
c     MJT -- 05Jan98:
c     Changing this to ftgcvd since ftgcld should really be using a
c     logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(ounit,stoppos,lrow,1,1,
     &         nulvald,timeuncorr(2),anynul,status)

          novals=2
        
          call correct_times(timeuncorr,novals,conversiono,
     &       imjdref,fmjdref,timezero,dec,ra,ounit,ephunit,nrowsorb,
     &       itimecorr, ftimecorr, timecorr, lbarytime, abort, status)

          call ftpcld(ounit,startpos,lrow,1,1,
     &       timecorr(1),status)
          
          call ftpcld(ounit,stoppos,lrow,1,1,
     &       timecorr(2),status)

        enddo

c        call ftpcks(ounit,status)
        
c        if(status.ne.0)then
c          call fcecho(' ')
c          contxt='Error in reading in GTI values'
c          call fcecho(contxt)
c          call fcerrm(status)
c          status=0
c        endif
           
c        call ftclos(ounit,status)
c        if(status.ne.0)then
c          contxt='Error closing GTI file'
c          call fcecho(contxt)
c          call fcerrm(status)
c          status=0
c        endif

c (MJT) 19Jun00: make it go back and correct the 2nd GTI extension if necessary
        if (lastext .gt. extfile+1) then
           extfile=extfile+1
           goto 34
        endif
        
      endif

      lendoffile=.TRUE.
      status=0

c      timeuncorrected=time(1)
c      do 500 k=1,400
c        timeuncorrected = timeuncorrected+1.0d0
c        posvec(1)=splint(timeuncorrected,time,x,c1x,c2x,c3x,nrowsorb)
c        posvec(2)=splint(timeuncorrected,time,y,c1y,c2y,c3y,nrowsorb)
c        posvec(3)=splint(timeuncorrected,time,z,c1z,c2z,c3z,nrowsorb)        
c        write(13,*) timeuncorrected,posvec(1),posvec(2),posvec(3)
c500   continue
      


999   continue
      
      return
      end


      subroutine correct_times(timeuncorr,novals,conversiono,
     &   imjdref,fmjdref,timezero,dec,ra,ounit,ephunit,nrowsorb,
     &   itimecorr, ftimecorr, timecorr, lbarytime, abort, status)

      implicit none

      integer nt
      parameter (nt = 15000)
      
      integer ounit,imjdref,status,
     &   nrowsorb,ephunit,novals,itimecorr(*)
      double precision timeuncorr(*),timecorr(*),
     &   ftimecorr(*),
     &   conversiono,timezero,dec,ra,fmjdref

      double precision time(nt),x(nt),c1x(nt),c2x(nt),c3x(nt),
     &   y(nt),c1y(nt),c2y(nt),c3y(nt),z(nt),c1z(nt),c2z(nt),
     &   c3z(nt)
      
      double precision posvec(3),
     &   splint,timeuncorrected,ftimeuncorrected,
     &   timecorrected,ftimecorrected
      integer jd,jdcor,itimeuncorrected,itimecorrected
      double precision frc,frccor

      integer i,ierr
      logical abort,lbarytime
      character(80) contxt

      common/spline_stuff/time,x,c1x,c2x,c3x,
     &   y,c1y,c2y,c3y,z,c1z,c2z,
     &   c3z

      contxt=' '
      
      ierr=0


      do i=1,novals
      
        timeuncorrected=timeuncorr(i)
        timeuncorrected=timeuncorrected+timezero
        timeuncorrected=timeuncorrected*conversiono
      
        posvec(1)=splint(timeuncorrected,time,x,c1x,c2x,c3x,nrowsorb)
        posvec(2)=splint(timeuncorrected,time,y,c1y,c2y,c3y,nrowsorb)
        posvec(3)=splint(timeuncorrected,time,z,c1z,c2z,c3z,nrowsorb)    
      
        timeuncorrected=(timeuncorrected/86400.0d0) - 0.5d0
        call d2if(timeuncorrected,itimeuncorrected,
     &     ftimeuncorrected,.TRUE.)
      
        jd=itimeuncorrected+imjdref+2400001
        frc=ftimeuncorrected+fmjdref
        
        call xtebarycen(ephunit,.TRUE.,ra,dec,jd,frc,posvec,
     &     jdcor,frccor,ierr)

        if(ierr.ne.0)then
          call fcecho(' ')
          call fcecho('Error in BARYCEN routine!!!')
          call fcecho('Cannot continue... Aborting!')
          abort=.TRUE.
          goto 999
        endif

        jdcor = jdcor - imjdref - 2400001
        frccor = frccor - fmjdref + 0.5d0

100     continue
        
        if(frccor.lt.0)then
          frccor = frccor + 1.0d0
          jdcor = jdcor - 1
          goto 100
        endif
        
        call d2if(timecorrected,jdcor,frccor,.FALSE.)
        timecorrected = timecorrected * 86400.0d0
        timecorrected=timecorrected/conversiono
        timecorrected=timecorrected-timezero

        call d2if(timecorrected,itimecorrected,
     &     ftimecorrected,.TRUE.)

        timecorr(i)=timecorrected
        ftimecorr(i)=ftimecorrected
        itimecorr(i)=itimecorrected

      enddo

999   continue
      
      return
      end
      

*+
C FTOOLS ROSAT TASK:
C     gevt2rdf
C
C FILE:
C     gevt2rdf.f
C
C DESCRIPTION: 
C
C     Program to read MPE (German) events files and write a new RDF style 
C     file that can be used with HEASARC data analysis programs.  The program 
C     will open two ASCII tables FITS files and write one binary table FITS 
C     file with Accepted and Rejected extensions.  Optionally, the Rejected
C     Events processing may be suppressed.
C AUTHORS/MODIFICATION HISTORY:
C     Ning Gan          (2.0.7: Jun 1998) The data-obs and
C				          date-end  are  writen 
C					  in new format yyyy-mm-dd.
C                                         zerodate is not changed. 
C     Peter D Wilson    (2.0.6: Jun 1998) Updated for new FCPARS behavior
C     Lawrence E. Brown (2.0.5: Jan 1996) Fixed formatted internal read bug
C     Lawrence E. Brown (2.0.4: Sep 1995) Fixed HRI xpix and ypix conversion
C     Lawrence E. Brown (2.0.3: Mar 1995) Fixed HRI DET? column names to RAW?
C     Lawrence E. Brown (2.0.1: Dec 1994) Fixed VMS internal integer read bug
C     Lawrence E. Brown (2.0.0: Oct 1994), First FTOOL version, made it
C        work with HRI files, fixed REVISION keyword bug, Optimized table
C        copying.
C     N. White (1.?) Revisions?
C     P. Tyler (1.0) Original version
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C     evtfile  -      The German Events file name
C     revtfile -      The German Rejected Events file name
C     outfile  -      The Output file
C     irafprefix -    Prefix for PROS related filenames in the keywords
C     chatter -       How much user info to write
C     clobber -       Overwrite output file bye default if true
C     status -        Error condition
C
C COMPILATION:
C     Requires CALLIB, FTOOLS, and FITSIO libraries
C
C CALLED SUBROUTINES:
C     ggevt2rdf    : Gets parameters
C     transg2r     : Does the actual copying and keyword writing 
C     fcerr        : Writes message to STDERR
C     fcecho       : Writes message to STDOUT
*-

      SUBROUTINE GEVT2F()
      IMPLICIT NONE

C     Initialize
      character(160) evtfile, revtfile, outfile, irafprefix
      integer chatter, status
      logical clobber
      character(40) taskname
      character(160) message
      character(7) version
      parameter (version = '2.0.7')
      COMMON/task/taskname
      taskname ='GEVT2RDF'//version


      call ggevt2rdf(evtfile,revtfile,outfile,irafprefix,
     $     chatter,clobber,status)
      if(status.NE.0) goto 987

      call transg2r(evtfile,revtfile,outfile,irafprefix,
     $     chatter,clobber,status)
      if(status.NE.0) goto 987

 987  if(status.NE.0) then
         message = ' ERROR : '//'Incomplete Execution'
         call fcerr(message)
      else
         if(chatter.gt.5) then
            message = taskname//' Finished'
            call fcecho(message)
         endif
      endif
      return
      end

C --------------------------------------------------------------
*+GGEVT2RDF
      subroutine ggevt2rdf(evtfile,revtfile,outfile,irafprefix,
     $     chatter,clobber,status)
      implicit none
      character*(*) evtfile, revtfile, outfile, irafprefix
      integer chatter, status
      logical clobber
C DESCRIPTION
C     Gets the parameters for GEVT2RDF from the parameter file
C
C USER INPUTS and SUBROUTINE ARGUMENTS
C     evtfile  -      The German Events file name
C     revtfile -      The German Rejected Events file name (set to 'NONE' if
C                      reject is FALSE
C     outfile  -      The Output file
C     irafprefix -    Prefix for PROS related filenames in the keywords
C     reject  -       If true, create Rejected Events extension
C     chatter -       How much user info to write
C     clobber -       Overwrite existing output file by default if true
C     status -        Error condition
C
C CALLED SUBROUTINES
C     fcerr           writes to STDERR
C     fcerrm          translates fitsio error number to message and dumps
C                       fitsio error stack
C     fcecho          writes to STDOUT
C     ftcmsg          clear fitsio error stack
C     crmvlbk         removes leading blanks from a string (CALLIB)
C     fcpars          parse a filename[extension] expression
C     uclgs_          get a parameter
C     ck_file         check output filename for legality (CALLIB)
C     clenact         find length of string (CALLIB)
C
C AUTHORS/MODIFICATION HISTORY
C     Lawrece E Brown (1.0.0: 1994 October) Original
C     Peter D Wilson (1.0.1: 1998 June 30)
C           . Strip out INQUIRE calls for FITS files
      character(7) version
      parameter (version = '1.0.1')
*-
      character(40) taskname
      COMMON/task/taskname
C     Local variables
      integer clenact
      character(160) message,filnam,ill_files(2),suffix
      integer extnum,n_ill,usind,dotind,lslashind
      logical valfil,exst,reject


C     Get the chattiness flag
      call uclgsi('chatter',chatter, status)
      if(status.NE.0) then
         message = 'Error getting CHATTER parameter'
         call fcecho(message)
         status = 0
         message = 'Setting CHATTER = 10'
         call fcecho(message)
         chatter = 10
      endif
      if(chatter.ge.10) then
         message='Starting '//taskname
         call fcecho(message)
      endif
      if(chatter.gt.10) then
         message='Getting parameters using GGEVT2RDF'//version
         call fcecho(message)
      endif

C     Get the name of the i/p  events file
      call uclgst('evtfile',evtfile, status)
      if(status.NE.0) then
         message = 'Error Getting Event File (evtfile) parameter'
         call fcerr(message)
         call fcerrm(status)
         return
      endif

C PDW 6/30/98: Don't call INQUIRE. Call FTRTNM to strip off extension
      call crmvlbk(evtfile)
      call ftrtnm( evtfile, filnam, status )
      ill_files(1)=filnam
C      call fcpars(evtfile,filnam,extnum,status)
C      ill_files(1)=filnam
C      inquire(file=filnam,exist=exst)
C
C      if((.not.exst).or.(filnam.eq.' '))then
C         message='Input file does not exist: '//filnam
C         call fcerr(message)
C         status=1
C         return
C      endif
C     since this task doesn't use extension numbers we'll send back
C     the stripped filename
      evtfile=filnam

C     Get the name of the i/p  rejected events file
      call uclgst('revtfile',revtfile, status)
      if(status.NE.0) then
         message=
     $        'Error Getting Rejected Event File (revtfile) parameter'
         call fcerr(message)
         call fcerrm(status)
         return
      endif

      usind=index(evtfile,'_')
      dotind=index(evtfile,'.')
      suffix=' '
      if(dotind.ne.0) suffix=evtfile(dotind:clenact(evtfile))

C     Get the reject parameter
      call uclgsb('reject',reject,status)
      if(status.ne.0) then
         message = 'Error getting REJECT parameter'
         call fcerr(message)
         return
      endif

      if(reject) then

         if(revtfile.eq.'-') then
            if(usind.eq.0) then
               message =
     $      'Couldn''t construct a default rejected events filename.'
               call fcerr(message)
               status=1
               return
            endif
            revtfile=evtfile(1:usind)//'difevents'//suffix
         endif
C PDW 6/30/98: Don't call INQUIRE. Call FTRTNM to strip off extension
         call crmvlbk(revtfile)
         call ftrtnm( revtfile, filnam, status )
         ill_files(2)=filnam
C         call crmvlbk(revtfile)
C         call fcpars(revtfile,filnam,extnum,status)
C         ill_files(2)=filnam
C         inquire(file=filnam,exist=exst)
C         
C         if((.not.exst).or.(filnam.eq.' '))then
C            message='Rejected events file does not exist: '//filnam
C            call fcerr(message)
C            status=1
C            return
C         endif
C     since this task doesn't use extension numbers we'll send back
C     the stripped filename
         revtfile=filnam
      else
         revtfile='NONE'
      endif

C     Get the clobber parameter
      call uclgsb('clobber',clobber,status)
      if(status.ne.0) then
         clobber=.false.
         status=0
         call ftcmsg()
      endif

C     Get the name of the o/p  file
      call uclgst('outfile',outfile, status)
      if(status.NE.0) then
         message = 'Error Getting Output File (outfile) parameter'
         call fcerr(message)
         call fcerrm(status)
         return
      endif


      if(outfile.eq.'-'.or.outfile.eq.'!-') then
         if(usind.eq.0) then
            message =
     $           'Couldn''t construct a default output filename.'
            call fcerr(message)
            status=1
            return
         endif
         if(outfile.eq.'!-') then
            outfile='!'//evtfile(1:usind)//'bas.fits'
         else
            outfile=evtfile(1:usind)//'bas.fits'
         endif
      endif


      valfil=.true.
      n_ill=1
      call ck_file(outfile,ill_files,n_ill,valfil,clobber,chatter)
      if(.not.valfil) then
         message='Output file is invalid'
         call fcerr(message)
         status=1
         return
      endif





C     Get the IRAFPREFIX name
      call uclgst('irafprefix',irafprefix, status)
      if(status.NE.0) then
         if(chatter.ge.5) then
            message = 'Error getting IRAFPREFIX parameter'
            call fcecho(message)
            message = 'Using default name scheme'
            call fcecho(message)
            if(chatter.gt.10) then
               call fcerrm(status)
            endif
         endif
         status=0
         call ftcmsg()
         irafprefix='-'
      endif
      if(irafprefix.eq.'-') then
         do 10 lslashind = clenact(evtfile),1,-1
            if(evtfile(lslashind:lslashind).eq.'/') goto 20
 10      continue
         lslashind=0
 20      continue
         lslashind = lslashind + 1
         if(usind.eq.0.and.dotind.eq.0) then
            irafprefix = evtfile(lslashind:)
         else if(usind.eq.0) then
            irafprefix = evtfile(lslashind:dotind-1)
         else
            irafprefix = evtfile(lslashind:usind-1) 
         endif
      endif

      return
      end

C-------------------------------------------------------------------
*+
      subroutine transg2r(evtfile,revtfile,outfile,irafprefix,
     $     chatter,clobber,status)
      implicit none
      character*(*)   evtfile , revtfile , outfile, irafprefix
      integer chatter, status
      logical clobber
C DESCRIPTION
C     Transforms German events files into RDF format
C
C ARGUMENTS
C     evtfile  -      The German Events file name
C     revtfile -      The German Rejected Events file name (set to 'NONE' if
C                      reject is FALSE
C     outfile  -      The Output file
C     irafprefix -    Prefix for PROS related filenames in the keywords
C     chatter -       How much user info to write
C     clobber -       Overwrite existing output file by default if true
C     status -        Error condition
C
C CALLED SUBROUTINES
C     fcerr           writes to STDERR
C     fcecho          writes to STDOUT
C     fcerrm          translates fitsio error number to message and dumps
C                       fitsio error stack 
C     clenact         find length of string (CALLIB)
C     cgetlun,cfrelun get/free an unused/used lun (CALLIB)
C     opfits          open a fits file for writing (with clobber test) (CALLIB)
C     ft_             FITSIO subroutines
C
C AUTHORS/MODIFICATION HISTORY
C     Lawrence E. Brown (2.0.0: Oct 1994), First FTOOL version, made it
C        work with HRI files, fixed REVISION keyword bug, Optimized table
C        copying.
C     N. White (1.?) Revisions?
C     P. Tyler (1.0) Original version
      character(7) version
      parameter (version = '2.0.0')
*-

C     Local Variables
      INTEGER MAXDIM
      PARAMETER (MAXDIM=99)
      integer xpixadj,ypixadj
C
      integer*2 stat
      integer*2 detx, dety, xpix, ypix
      integer*2 x, y
      integer*2 xdet, ydet
      integer*2 pha, ampl, pi
      integer timecol,amplcol,xpixcol,ypixcol,xdetcol,ydetcol,phacol
      INTEGER revision , rowtotal , hdutype
      INTEGER hours , minutes , degrees , decsec
      INTEGER rowlen, tbcol(10), savestatus
      INTEGER luin , lout , luin2 , block , length , mjdrefi
      INTEGER ii , jj , kk , CLENACT ,  ror , varidat
      integer i, iror, jror, itest
      INTEGER naxes(MAXDIM) , pcount , gcount
      INTEGER bitpix , naxis , tfields , tfields_in, naxis2
      INTEGER talen1 , talen2 , talen3 , talen4
      INTEGER talen6 , talen7 , nrows , scseqbeg , scseqend
      INTEGER tlmin1 , tlmax1 , tlmin2 , tlmax2 , tlmin3
      INTEGER tlmax3 , tlmin4 , tlmax4 , tlmin6 , tlmax6
      INTEGER tlmin7 , tlmax7
C
      DOUBLE PRECISION equinox , ontime
      DOUBLE PRECISION seconds , time
      DOUBLE PRECISION mjdreff , scseqbegr , scseqendr
      DOUBLE PRECISION ra_nom , dec_nom
      DOUBLE PRECISION first , second , third , times(5000)
      DOUBLE PRECISION start(5000) , stop(5000)
      DOUBLE PRECISION tcrpx1 , tcrpx2 , tcrpx3 , tcrpx4
      DOUBLE PRECISION tcrpx6 , tcrpx7 , tcdlt1 , tcdlt2 , tcdlt3
      DOUBLE PRECISION tcdlt4 , tcdlt6 , tcdlt7 , tcrot1
      DOUBLE PRECISION tcrot2 , tcrvl1 , tcrvl2 , tcrvl3 , tcrvl4
C
      CHARACTER sign , char
      character(2) daynum1 , monthnum1 , yearnum1
      character(2) daynum2 , monthnum2 , yearnum2
      character(3) monthstr1 , monthstr2, det_or_raw
      character(5) filter , temp
      character(6) origin , content
      character(7) keyword , setupid
      character(8) telescop , instrume , obs_mode , rdf_version
      character(10) rorstring , xtension
      character(68) time_obs , date_obs , time_end , date_end
      character(16) obs_id
      character(20) object
      character(68) zerodate , zerotime
      character(30) irafname, qpoename
      character(70) comment , history
      character(80) card , context
      character(8) hduclass , hduclas1 , hduclas2 , radecsys
      character(8) tctyp1 , tctyp2
      character(16) ttype(10) , tform(10) , tunit(10) 
      integer*2 ivect(10)
      character(16) extname
      character(80) zwrite
      character(256) zwrite2
      character(256) buffer
C
      LOGICAL simple , extend , notend , pspcfile, amplflag

C      Init
      xpixadj = 0
      ypixadj = 0
      ampl = 0
      rowtotal = 0
      kk = 0
      pspcfile = .false.

C
C     Open the MPE *_events.tfits file for reading
C
      if(chatter.gt.10) then
         context='Translating file with TRANSG2R'//version
         call fcecho(context)
         context='Opening files...'
         call fcecho(context)
      endif
      CALL CGETLUN(luin)
      CALL FTOPEN(luin,evtfile,0,block,status)
      IF ( status.NE.0 ) THEN
         context = ' Unable to open evtfile. '
         call fcerr(context)
         call fcerrm(status)
         GOTO 99000
      ENDIF
C
C     Initialize the new FITS file for writing
C

      status = 0
      ontime = 0.0
      call cgetlun(lout)
      call opfits(lout,outfile,clobber,chatter,status)
      if(status.ne.0) then
         zwrite='Trouble opening'//outfile
         call fcerr(zwrite)
         goto 99000
      endif


      if(chatter.gt.10) then
         context='Initializing primary HDU...'
         call fcecho(context)
      endif
C     PRINT * , 'FTINIT status:' , status
C
C     Get primary header keywords from old ASCII table FITS file
C
      CALL FTGHPR(luin,MAXDIM,simple,bitpix,naxis,naxes,pcount,gcount,
     &     extend,status)
C     PRINT * , 'FTGHPR status:' , status
C
C     Hardwire some keywords into new binary table FITS primary header
C
      simple = .TRUE.
      bitpix = 32
      naxis = 0
      extend = .TRUE.
C
C     Write header keywords to new binary table FITS primary header
C
      CALL FTPHPR(lout,simple,bitpix,naxis,naxes,pcount,gcount,extend,
     &     status)


      call timestamp(lout)

C     PRINT * , 'Write primary header status:' , status
C
C     Hardwire these keywords and write to primary header
C
      content = 'BASIC'
      comment = 'BASIC SCIENCE DATA (EVENTS & GOOD TIMES)'
      CALL FTPKYS(lout,'CONTENT',content,comment,status)
C
      origin = 'USRSDC'
      comment = 'origin of processed data'
      CALL FTPKYS(lout,'ORIGIN',origin,comment,status)
C
C     date = '31/01/94'
C     comment = 'FITS creation date (DD/MM/YY)'
C     CALL FTPKYS(lout,'DATE',date,comment,status)
      call ftpdat(lout,status)

C
      telescop = 'ROSAT'
      comment = 'mission name'
      CALL FTPKYS(lout,'TELESCOP',telescop,comment,status)
C
      irafname = ' '
      irafname = 'null_image'
      comment = 'IRAF file name'
      CALL FTPKYS(lout,'IRAFNAME',irafname,comment,status)
C
      mjdrefi = 48043
      comment = 'MJD integer SC clock start'
      CALL FTPKYJ(lout,'MJDREFI',mjdrefi,comment,status)
C
      mjdreff = 8.79745370370074E-01
      comment = 'MJD fraction SC clock start'
      CALL FTPKYD(lout,'MJDREFF',mjdreff,8,comment,status)
C
      zerodate = '01/06/90'
      comment = 'UT date of SC start (DD/MM/YY)'
      CALL FTPKYS(lout,'ZERODATE',zerodate,comment,status)
C
      rdf_version = ' '
      comment = 'RDF Version'
      CALL FTPKYS(lout,'RDF_VERS',rdf_version,comment,status)
C
      zerotime = '21:06:50'
      comment = 'UT time of SC start (HH:MM:SS)'
      CALL FTPKYS(lout,'ZEROTIME',zerotime,comment,status)
C
      revision = 1
      comment = 'Revision number of processed data'
      CALL FTPKYJ(lout,'REVISION',revision,comment,status)
C
      equinox = 2.000000E3
      CALL FTPKYD(lout,'EQUINOX',equinox,6,
     &     'equinox of celestial coord. system',status)
C
C      comment = 'ROR number'
C      CALL FTPKYJ(lout,'ROR_NUM',ror,comment,status)
C     The previous 2 lines have been copied below to allow the 
C     info to come from the file rather than the filename
C
      setupid = 'NOMINAL'
      comment = 'Instrument setup'
      CALL FTPKYS(lout,'SETUPID',setupid,comment,status)
C
      qpoename= irafprefix(:clenact(irafprefix))//'.qp'
      comment = 'IRAF QPOE file name'
      CALL FTPKYS(lout,'QPOENAME',qpoename,comment,status)


      if (status.ne.0) then
         context = 'Error initializing primary HDU'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Creating GTI extension from HISTORY keywords...'
         call fcecho(context)
      endif

C
C     Move to first extension header in ASCII FITS file to get to
c     HISTORY cards.
C     Also to get the data for the first input file.
C
      CALL FTMAHD(luin,2,hdutype,status)
C
C     Now start reading in HISTORY block.  Get information for
C     other new keywords and write to primary header
C
      keyword = 'HISTORY'
      card = ' '
 88   if (INDEX(card,'ESO-DESCRIPTORS END').ne.0 ) goto 89              

         CALL FTGCRD(luin,keyword,card,status)
C
         IF ( INDEX(card,'SC_MODE').NE.0 ) THEN
            obs_mode = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:18),'(a)') obs_mode
            CALL FTPKYS(lout,'OBS_MODE',obs_mode,
     &           'obs mode: POINTING, SLEW, or SCAN',status)
C     PRINT * , ' OBS_MODE :' , obs_mode, status
C
         ELSEIF ( INDEX(card,'DETECTOR_ID').NE.0 ) THEN
            instrume = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:15),'(a)') instrume
            CALL FTPKYS(lout,'INSTRUME',instrume,
     &           'instrument used for observation',status)
            call ftupch(instrume)
            if(instrume(1:4).eq.'PSPC') then
               pspcfile=.true.
               det_or_raw='DET'
            else
               pspcfile=.false.
               det_or_raw='RAW'
            endif
            if(pspcfile) then
               xpixadj = 7681
               ypixadj = 7680
            else
               xpixadj = 4096
               ypixadj = 4097
            endif

C     PRINT * , ' INSTRUME :' , instrume, status
C
         ELSEIF ( INDEX(card,'FILTER_ID').NE.0 ) THEN
            temp = ' '
            filter = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:15),'(a)') temp
            IF ( temp.EQ.'OFF  ' ) THEN
               filter = 'NONE'
            ELSE
               filter = 'BORON'
            ENDIF
            CALL FTPKYS(lout,'FILTER',filter,
     &           'filter id: NONE OR BORON',status)
C     PRINT * , ' FILTER :' , filter, status
C
         ELSEIF ( INDEX(card,'OBS_TITLE').NE.0 ) THEN
            object = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:26),'(a)') object
            CALL FTPKYS(lout,'OBJECT',object,'name of observed object',
     &           status)
C     PRINT * , ' OBJECT :' , object, status
C
         ELSEIF ( INDEX(card,'POINT_LONG').NE.0 ) THEN
            CALL FTGCRD(luin,keyword,card,status)
            length = CLENACT(card)
            DO 120 ii = 10 , length
               IF ( card(ii:ii).EQ.'H' ) THEN
                  jj = ii - 1
                  READ (card(10:jj),'(BN,I2)') hours
                  kk = ii + 1
               ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                  jj = ii - 1
                  READ (card(kk:jj),'(BN,I2)') minutes
                  kk = ii + 1
               ELSEIF ( card(ii:ii).EQ.'S' .or.
     &                 card(ii:ii).EQ.'.' ) THEN
                  jj = ii - 1
                  READ (card(kk:jj),*) seconds
               ENDIF
 120        CONTINUE
C
C     Turn hours, minutes, seconds into decimal representation
C
            ra_nom = (((hours)+(minutes/60.D0)+(seconds/3600.D0))/24.D0)
     &           *360.D0
            tcrvl1 = ra_nom
C
C     Add new keyword RA_NOM taken from POINT_LONG. TCRVL1 is same value
c     .
C
            comment = 'nominal RA (deg)'
            CALL FTPKYD(lout,'RA_NOM',ra_nom,6,comment,status)
C     PRINT * , ' RA_NOM :' , ra_nom, status
C
         ELSEIF ( INDEX(card,'POINT_LAT').NE.0 ) THEN
            sign = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:10),'(a)') sign
            IF ( sign.EQ.'-' ) THEN
               length = CLENACT(card)
               DO 130 ii = 11 , length
                  IF ( card(ii:ii).EQ.'D' ) THEN
                     jj = ii - 1
                     READ (card(11:jj),'(BN,I2)') degrees
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,I2)') minutes
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'.' ) then
                     jj = ii - 1
                     READ (card(kk:jj),*) seconds
                     decsec = seconds
                     go to 9911
                  ELSEIF ( card(ii:ii).EQ.'S' ) then
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,I2)') decsec
                  ENDIF
 130           CONTINUE
 9911          continue
            ELSEIF ( sign.NE.'-' ) THEN
               sign = '+'
               length = CLENACT(card)
               DO 140 ii = 10 , length
                  IF ( card(ii:ii).EQ.'D' ) THEN
                     jj = ii - 1
                     READ (card(10:jj),'(I2)') degrees
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(I2)') minutes
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'.' ) then
                     jj = ii - 1
                     READ (card(kk:jj),*) seconds
                     decsec = seconds
                     go to 9912
                  ELSEIF ( card(ii:ii).EQ.'S' ) then
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,I2)') decsec
                  ENDIF
 140           CONTINUE
 9912          continue
            ENDIF
C
C     Turn degrees, minutes, seconds into decimal representation
C
C     write (*,*)' card = ', card
C     write (*,*)' degrees = ',degrees, minutes, seconds, decsec
            dec_nom = ABS(degrees) + (minutes+decsec/60.D0)/60.D0
            IF ( sign.EQ.'-' ) dec_nom = dec_nom*(-1.)
            tcrvl2 = dec_nom
C     write(*,*)' dec_nom = ',dec_nom
C
C     Add new keyword DEC_NOM taken from POINT_LAT. TCRVL2 is same value
c     .
C
            comment = 'nominal DEC (deg)'
            CALL FTPKYD(lout,'DEC_NOM',dec_nom,6,comment,status)
C     PRINT * , ' DEC_NOM :' , dec_nom, status
C
         ELSEIF ( INDEX(card,'OBS_ID').NE.0 ) THEN
            obs_id = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:30),'(a)') obs_id
            comment = 'observation ID '
            CALL FTPKYS(lout,'OBS_ID',obs_id,comment,status)
            jror=1
            rorstring=' '
            do 22 iror=1,clenact(obs_id)
               char=obs_id(iror:iror)
C     if the following line fails, it means the character is not an integer
               read(char,'(i1)',err=22) itest
               if(char.eq.'-'.or.char.eq.'+') goto 22
               rorstring(jror:jror)=obs_id(iror:iror)
               jror=jror+1
 22         continue
            read(rorstring,'(BN,I6)') ror
            comment = 'ROR number'
            CALL FTPKYJ(lout,'ROR_NUM',ror,comment,status)
C     PRINT * , ' OBS_ID :' , obs_id, status
C
         ELSEIF ( INDEX(card,'OBS_DATE').NE.0 ) THEN
            daynum1 = ' '
            monthstr1 = ' '
            yearnum1 = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:11),'(a)') daynum1
            READ (card(13:15),'(a)') monthstr1
            IF ( monthstr1.EQ.'JAN' ) THEN
               monthnum1 = '01'
            ELSEIF ( monthstr1.EQ.'FEB' ) THEN
               monthnum1 = '02'
            ELSEIF ( monthstr1.EQ.'MAR' ) THEN
               monthnum1 = '03'
            ELSEIF ( monthstr1.EQ.'APR' ) THEN
               monthnum1 = '04'
            ELSEIF ( monthstr1.EQ.'MAY' ) THEN
               monthnum1 = '05'
            ELSEIF ( monthstr1.EQ.'JUN' ) THEN
               monthnum1 = '06'
            ELSEIF ( monthstr1.EQ.'JUL' ) THEN
               monthnum1 = '07'
            ELSEIF ( monthstr1.EQ.'AUG' ) THEN
               monthnum1 = '08'
            ELSEIF ( monthstr1.EQ.'SEP' ) THEN
               monthnum1 = '09'
            ELSEIF ( monthstr1.EQ.'OCT' ) THEN
               monthnum1 = '10'
            ELSEIF ( monthstr1.EQ.'NOV' ) THEN
               monthnum1 = '11'
            ELSEIF ( monthstr1.EQ.'DEC' ) THEN
               monthnum1 = '12'
            ELSE
               write(context,*) 'Bad month' ,  monthstr1
               call fcerr(context)
               status = 1
               GOTO 99000
            ENDIF
            READ (card(19:20),'(a)') yearnum1
c            date_obs = daynum1 // '/' // monthnum1 // '/' // yearnum1
             date_obs = '19'//yearnum1//'-'//monthnum1 // 
     *                  '-' //daynum1
            daynum2 = ' '
            monthstr2 = ' '
            yearnum2 = ' '
            READ (card(22:23),'(a)') daynum2
            READ (card(25:27),'(a)') monthstr2
            IF ( monthstr2.EQ.'JAN' ) THEN
               monthnum2 = '01'
            ELSEIF ( monthstr2.EQ.'FEB' ) THEN
               monthnum2 = '02'
            ELSEIF ( monthstr2.EQ.'MAR' ) THEN
               monthnum2 = '03'
            ELSEIF ( monthstr2.EQ.'APR' ) THEN
               monthnum2 = '04'
            ELSEIF ( monthstr2.EQ.'MAY' ) THEN
               monthnum2 = '05'
            ELSEIF ( monthstr2.EQ.'JUN' ) THEN
               monthnum2 = '06'
            ELSEIF ( monthstr2.EQ.'JUL' ) THEN
               monthnum2 = '07'
            ELSEIF ( monthstr2.EQ.'AUG' ) THEN
               monthnum2 = '08'
            ELSEIF ( monthstr2.EQ.'SEP' ) THEN
               monthnum2 = '09'
            ELSEIF ( monthstr2.EQ.'OCT' ) THEN
               monthnum2 = '10'
            ELSEIF ( monthstr2.EQ.'NOV' ) THEN
               monthnum2 = '11'
            ELSEIF ( monthstr2.EQ.'DEC' ) THEN
               monthnum2 = '12'
            ENDIF
            READ (card(31:32),'(a)') yearnum2
c           date_end = daynum2 // '/' //monthnum2 // '/' // yearnum2
            date_end = '19'//yearnum2//'-'// monthnum2// 
     *		       '-' //daynum2
            comment = 'UT date of obs start (YYYY-MM-DD)'
c            comment = 'UT date of obs start (DD/MM/YY)'
            CALL FTPKYS(lout,'DATE-OBS',date_obs,comment,status)
C     PRINT * , ' DATE-OBS :' , date_obs, status
            comment = 'UT date of obs start (YYYY-MM-DD)'
c            comment = 'UT date of obs end (DD/MM/YY)'
            CALL FTPKYS(lout,'DATE-END',date_end,comment,status)
C     PRINT * , ' DATE-END :' , date_end, status
C
         ELSEIF ( INDEX(card,'OBS_UT').NE.0 ) THEN
            time_obs = ' '
            time_end = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:22),'(a)') time_obs
            READ (card(26:38),'(a)') time_end
            length = CLENACT(time_obs)
            DO 160 ii = 1 , length
               IF ( time_obs(ii:ii).EQ.' ' ) THEN
                  time_obs(ii:ii) = '0'
               ENDIF
               length = CLENACT(time_end)
 160        CONTINUE
            DO 180 ii = 1 , length
               IF ( time_end(ii:ii).EQ.' ' ) THEN
                  time_end(ii:ii) = '0'
               ENDIF
 180        CONTINUE
            comment = 'UT time of obs start (HH:MM:SS)'
            CALL FTPKYS(lout,'TIME-OBS',time_obs,comment,status)
C     PRINT * , ' TIME-OBS :' , time_obs, status
            comment = 'UT time of obs end (HH:MM:SS)'
            CALL FTPKYS(lout,'TIME-END',time_end,comment,status)
C     PRINT * , ' TIME-END :' , time_end, status
C
         ELSEIF ( INDEX(card,'OBS_CLOCK').NE.0 ) THEN
            scseqbeg = 0.
            scseqend = 0.
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:33),*) scseqbegr
            READ (card(34:56),*) scseqendr
            scseqbeg = INT(scseqbegr)
            scseqend = INT(scseqendr)
            comment = 'SC seq start (sec)'
            CALL FTPKYJ(lout,'SCSEQBEG',scseqbeg,comment,status)
C     PRINT * , ' SCSEQBEG :' , scseqbeg, status
            comment = 'SC seq end (sec)'
            CALL FTPKYJ(lout,'SCSEQEND',scseqend,comment,status)
            comment = 'SC seq start (sec)'
            CALL FTPKYJ(lout,'TSTART',scseqbeg,comment,status)
C
            comment = 'SC seq end (sec)'
            CALL FTPKYJ(lout,'TSTOP',scseqend,comment,status)
C
C     PRINT * , ' SCSEQEND :' , scseqend, status
C
C     Use TIM_SEL to get column data for START and STOP for STDGTI data.
c     Also
C     use TIM_SEL to calculate ONTIME.  Have to loop through the values
c     following
C     the TIM_SEL card, subtract start times from stop times, and sum up
c     those
C     times to get ONTIME.  The TIM_SEL card may have many entries,
c     three to a
C     row.  Every other entry is either a start time or a stop time.
c     The TIM_SEL
C     entries must always be an even number -- one stop for each start.
C
         ELSEIF ( INDEX(card,'TIM_SEL').NE.0 ) THEN
            DO 200 ii = 1 , 100
               times(ii) = 0.0
 200        CONTINUE
C     ontime = 0.0
            CALL FTGCRD(luin,keyword,card,status)
            jj = 1
            kk = 1
            READ (card(11:32),'(g22.0)') first
            READ (card(34:55),'(g22.0)') second
            READ (card(57:78),'(g22.0)') third
 66         if( first.EQ.0 ) goto 67                                    
               times(jj) = first
               IF ( first.NE.0 ) THEN
                  IF ( MOD(jj,2).NE.0 ) THEN
                     start(kk) = first
                  ELSE
                     stop(kk) = first
                     kk = kk + 1
                  ENDIF
               ENDIF
               times(jj+1) = second
               IF ( second.NE.0 ) THEN
                  IF ( MOD(jj+1,2).NE.0 ) THEN
                     start(kk) = second
                  ELSE
                     stop(kk) = second
                     kk = kk + 1
                  ENDIF
               ENDIF
               times(jj+2) = third
               IF ( third.NE.0 ) THEN
                  IF ( MOD(jj+2,2).NE.0 ) THEN
                     start(kk) = third
                  ELSE
                     stop(kk) = third
                     kk = kk + 1
                  ENDIF
               ENDIF
               jj = jj + 3
               CALL FTGCRD(luin,keyword,card,status)
               READ (card(11:32),'(g22.0)') first
               READ (card(34:55),'(g22.0)') second
               READ (card(57:78),'(g22.0)') third
            goto 66                                                     
 67         continue
            kk = kk - 1
            rowtotal = kk
            DO 220 ii = 1 , jj - 1 , 2
               ontime = ontime + ABS(times(ii+1)-times(ii))
 220        CONTINUE
            comment = 'On time '
            CALL FTPKYD(lout,'ONTIME',ontime,6,comment,status)
            if(chatter.ge.10) then
               write(Zwrite,*)'ONTIME :' , ontime
               call fcecho(zwrite)
            endif
C
         ENDIF
      goto 88                                                           
 89   continue
      if (status.ne.0) then
         context = 'Error Creating GTI extension from HISTORY keywords'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Copying HISTORY keywords...'
         call fcecho(context)
      endif

C
C     After all new keywords are taken from the HISTORY block, go back
c     and copy
C     over all of the HISTORY information exactly as in the original
c     file.
C
C     First reset pointer to top of file
C
      CALL FTGKYS(luin,'XTENSION',xtension,comment,status)
C     PRINT * , 'FTGKYS status:' , status
C
      notend = .TRUE.
 77   if ( .not.notend ) goto 78                                        
         CALL FTGCRD(luin,keyword,card,status)
         IF ( card(10:80).EQ.' ' ) THEN
            history = ' '
         ELSE
            length = CLENACT(card)
            history = card(10:length)
         ENDIF
         CALL FTPHIS(lout,history,status)
         IF ( INDEX(card,'ESO-DESCRIPTORS END').NE.0 ) THEN
            notend = .FALSE.
         ENDIF
      goto 77                                                           
 78   continue
      if (status.ne.0) then
         context = 'Error copying HISTORY keywords'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Writing GTI HDU keywords...'
         call fcecho(context)
      endif


C
C     Define the primary array structure with FTPDEF.
C     There is no primary array for the file; naxis=0, naxes=0.
C
      CALL FTPDEF(lout,bitpix,naxis,naxes,pcount,gcount,status)
C     PRINT * , 'Primary FTPDEF status:' , status
C
C******************************************************************************c
C
C     Set up first extension STDGTI as binary extension with columns
c     START and
C     STOP.
C
C     Create the extension header
C
      CALL FTCRHD(lout,status)
C     PRINT * , 'Create extension header STDGTI status:' , status
C
C     Write the required keywords to the new extension header.  Hardwire
C     keyword values.
C
      bitpix = 8
      nrows = rowtotal
      tfields = 2
      ttype(1) = 'START'
      tform(1) = '1D'
      tunit(1) = 's'
      ttype(2) = 'STOP'
      tform(2) = '1D'
      tunit(2) = 's'
      extname = 'STDGTI'
C
C     Write the new extension header as binary
C
      CALL FTPHBN(lout,nrows,tfields,ttype,tform,tunit,extname,pcount,
     &     status)
C     PRINT * , 'STDGTI FTPHBN status:' , status
      if(chatter.ge.10) then
         write(zwrite,*)'Number of rows in STDGTI table: ' , nrows
         call fcecho(zwrite)
      endif
C     Write after-the-fact comments for keywords
C
      CALL FTMKYS(lout,'TTYPE1','START',
     &     'SC clock time of interval start',status)
C     PRINT * , 'TTYPE1 status:' , status
      CALL FTMKYS(lout,'TTYPE2','STOP','SC clock time of interval stop',
     &     status)
C     PRINT * , 'TTYPE2 status:' , status
      CALL FTMKYS(lout,'TFORM1','1D',
     &     'data format of the field : DOUBLE PRECISION',status)
C     PRINT * , 'TFORM1 status:' , status
      CALL FTMKYS(lout,'TFORM2','1D',
     &     'data format of the field : DOUBLE PRECISION',status)
C     PRINT * , 'TFORM2 status:' , status
      CALL FTMKYS(lout,'TUNIT1','s','units: seconds',status)
C     PRINT * , 'TUNIT1 status:' , status
      CALL FTMKYS(lout,'TUNIT2','s','units: seconds',status)
C     PRINT * , 'TUNIT2 status:' , status
      CALL FTMKYS(lout,'EXTNAME','STDGTI',
     &     'Standard Good Time Intervals',status)
C     PRINT * , 'EXTNAME status:' , status
C
C     Write additional keywords to first extension header
C
      content = 'BASIC'
      comment = 'BASIC SCIENCE DATA (EVENTS & GOOD TIMES)'
      CALL FTPKYS(lout,'CONTENT',content,comment,status)
C
      origin = 'USRSDC'
      comment = 'origin of processed data'

      CALL FTPKYS(lout,'ORIGIN',origin,comment,status)
C
C     date = '31/01/94'
C     comment = 'FITS creation date (DD/MM/YY)'
C     CALL FTPKYS(lout,'DATE',date,comment,status)
      call ftpdat(lout,status)
C
      telescop = 'ROSAT'
      comment = 'mission name'
      CALL FTPKYS(lout,'TELESCOP',telescop,comment,status)
C
      zerodate = '01/06/90'
      comment = 'UT date of SC start (DD/MM/YY)'
      CALL FTPKYS(lout,'ZERODATE',zerodate,comment,status)
C
      zerotime = '21:06:50'
      comment = 'UT time of SC start (HH:MM:SS)'
      CALL FTPKYS(lout,'ZEROTIME',zerotime,comment,status)
C
      irafname = ' '
      irafname = irafprefix(:CLENACT(irafprefix)) // '_stdgti.tab'
      comment = 'IRAF file name'
      CALL FTPKYS(lout,'IRAFNAME',irafname,comment,status)
C
      setupid = 'NOMINAL'
      comment = 'Instrument setup'
      CALL FTPKYS(lout,'SETUPID',setupid,comment,status)
C
      mjdrefi = 48043
      comment = 'MJD integer SC clock start'
      CALL FTPKYJ(lout,'MJDREFI',mjdrefi,comment,status)
C
      mjdreff = 8.79745370370074E-01
      comment = 'MJD fraction SC clock start'
      CALL FTPKYD(lout,'MJDREFF',mjdreff,8,comment,status)
C
      revision = 1
      comment = 'Revision number of processed data'
      CALL FTPKYJ(lout,'REVISION',revision,comment,status)
C
      qpoename= irafprefix(:clenact(irafprefix))//'.qp'
      comment = 'IRAF QPOE file name'
      CALL FTPKYS(lout,'QPOENAME',qpoename,comment,status)

      CALL FTPKYS(lout,'OBS_MODE',obs_mode,
     &     'obs mode: POINTING, SLEW, or SCAN',status)
C
      CALL FTPKYS(lout,'INSTRUME',instrume,
     &     'instrument used for observation',status)
C
      CALL FTPKYS(lout,'FILTER',filter,'filter id: NONE OR BORON',
     &     status)
C
      comment = 'observation ID '
      CALL FTPKYS(lout,'OBS_ID',obs_id,comment,status)
C
      comment = 'nominal RA (deg)'
      CALL FTPKYD(lout,'RA_NOM',ra_nom,6,comment,status)
C
      comment = 'nominal DEC (deg)'
      CALL FTPKYD(lout,'DEC_NOM',dec_nom,6,comment,status)
C
      equinox = 2.000000E3
      CALL FTPKYD(lout,'EQUINOX',equinox,6,
     &     'equinox of celestial coord. system',status)
C
      CALL FTPKYS(lout,'OBJECT',object,'name of observed object',status)
C     PRINT * , 'OBJECT status:' , status
C
      comment = ' '
      CALL FTPKYS(lout,'DATE-OBS',date_obs,comment,status)
      CALL FTPKYS(lout,'DATE-END',date_end,comment,status)
C
      comment = ' '
      CALL FTPKYS(lout,'TIME-OBS',time_obs,comment,status)
      CALL FTPKYS(lout,'TIME-END',time_end,comment,status)
C
      comment = 'SC seq start (sec)'
      CALL FTPKYJ(lout,'SCSEQBEG',scseqbeg,comment,status)
C     PRINT * , 'SCSEQBEG status:' , status
C
      comment = 'SC seq end (sec)'
      CALL FTPKYJ(lout,'SCSEQEND',scseqend,comment,status)
C     PRINT * , 'SCSEQEND status:' , status
C
C
      comment = 'SC seq start (sec)'
      CALL FTPKYJ(lout,'TSTART',scseqbeg,comment,status)
C
      comment = 'SC seq end (sec)'
      CALL FTPKYJ(lout,'TSTOP',scseqend,comment,status)
C
      comment = 'On time '
      CALL FTPKYD(lout,'ONTIME',ontime,6,comment,status)
C     PRINT * , 'ONTIME status:' , ontime , status
C
      hduclass = 'OGIP'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLASS',hduclass,comment,status)
C     PRINT * , 'HDUCLASS status:' , status
C
      hduclas1 = 'GTI'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS1',hduclas1,comment,status)
C     PRINT * , 'HDUCLAS1 status:' , status
C
      hduclas2 = 'STANDARD'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS2',hduclas2,comment,status)
C     PRINT * , 'HDUCLAS2 status:' , status
C
C     Define the data array for the first extension
C
      CALL FTBDEF(lout,tfields,tform,varidat,nrows,status)
      if (status.ne.0) then
         context = 'Error writing GTI HDU keywords'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Writing GTI extension table...'
         call fcecho(context)
      endif


C     PRINT * , 'STDGTI FTBDEF status:' , status
C
C     Write the data here.
C
      DO 300 ii = 1 , rowtotal
         CALL FTPCLD(lout,1,ii,1,1,start(ii),status)
         CALL FTPCLD(lout,2,ii,1,1,stop(ii),status)
C     PRINT * , 'FTPCLD status:' , status
 300  CONTINUE
      if (status.ne.0) then
         context = 'Error writing GTI extension table'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Writing STDEVT HDU keywords'
         call fcecho(context)
      endif

C
C******************************************************************************c
C
C     The second extension is STDEVT.
C     Create the extension header
C
      CALL FTCRHD(lout,status)
C     PRINT * , 'Create extension header STDEVT status:' , status
C
C     Get keywords from ASCII header
C
      CALL FTGHTB(luin,MAXDIM,rowlen,nrows,tfields_in,ttype,tbcol,
     $     tform,tunit,extname,status)
C     PRINT * , 'FTGHTB status:' , status
      if(chatter.ge.10) then
         write(zwrite,*)'Number of rows in STDEVT table: ' , nrows
         call fcecho(zwrite)
      endif
C     PRINT * , 'rowlen:' , rowlen
C     PRINT * , 'extname:' , extname
      CALL FTGKYJ(luin,'NAXIS2',naxis2,comment,status)
C     PRINT * , 'NAXIS2 status:' , naxis2 , status
C
C     Write the required keywords to the new extension header.  Hardwire
C     keyword values.  Stat = 0 for STDEVT.
C
      stat = 0
      bitpix = 8
      tfields = 8
      ttype(1) = 'X'
      tform(1) = '1I'
      tunit(1) = 'pixel'
      ttype(2) = 'Y'
      tform(2) = '1I'
      tunit(2) = 'pixel'
      ttype(3) = 'PHA'
      tform(3) = '1I'
      tunit(3) = 'chan'
      ttype(4) = 'PI'
      tform(4) = '1I'
      tunit(4) = 'chan'
      ttype(5) = 'TIME'
      tform(5) = '1D'
      tunit(5) = 's'
      ttype(6) = det_or_raw//'X'
      tform(6) = '1I'
      tunit(6) = 'pixel'
      ttype(7) = det_or_raw//'Y'
      tform(7) = '1I'
      tunit(7) = 'pixel'
      ttype(8) = 'STATUS'
      tform(8) = '1I'
      tunit(8) = 'CODED'
      extname = 'STDEVT'
C
      IF (pspcfile) THEN
         tlmin1 = 1
         tlmax1 = 15360
         tlmin2 = 1
         tlmax2 = 15360
         tlmin3 = 1
         tlmax3 = 256
         tlmin4 = 1
         tlmax4 = 256
         tlmin6 = 1
         tlmax6 = 8192
         tlmin7 = 1
         tlmax7 = 8192
      ELSE
         tlmin1 = 1
         tlmax1 = 8192
         tlmin2 = 1
         tlmax2 = 8192
         tlmin3 = 1
         tlmax3 = 16
         tlmin4 = 1
         tlmax4 = 16
         tlmin6 = 1
         tlmax6 = 4096
         tlmin7 = 1
         tlmax7 = 4096
      ENDIF
C
C     Write the new extension header as binary
C
      CALL FTPHBN(lout,nrows,tfields,ttype,tform,tunit,extname,pcount,
     &     status)
C     PRINT * , 'STDEVT FTPHBN status:' , status
C
C     Write after-the-fact comments for keywords
C
      CALL FTMKYS(lout,'TTYPE1','X',
     &     'Projected X position of photon on sky',status)
      CALL FTMKYS(lout,'TFORM1','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT1','pixel',
     &     'units: 0.5 arcsecond x 0.5 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE2','Y',
     &     'Projected Y position of photon on sky',status)
      CALL FTMKYS(lout,'TFORM2','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT2','pixel',
     &     'units: 0.5 arcsecond x 0.5 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE3','PHA',
     &     'Pulse Height Analyzer bin number',status)
      CALL FTMKYS(lout,'TFORM3','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT3','chan','units: channels',status)
C
      CALL FTMKYS(lout,'TTYPE4','PI','Pulse Invariant bin',status)
      CALL FTMKYS(lout,'TFORM4','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT4','chan','units: channels',status)
C
      CALL FTMKYS(lout,'TTYPE5','TIME','Spacecraft clock time of event',
     &     status)
      CALL FTMKYS(lout,'TFORM5','1D',
     &     'data format of the field : DOUBLE PRECISION',status)
      CALL FTMKYS(lout,'TUNIT5','s','units: seconds',status)
C
      CALL FTMKYS(lout,'TTYPE6',det_or_raw//'X',
     &     'Corrected X coordinate of photon',status)
      CALL FTMKYS(lout,'TFORM6','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT6','pixel',
     &     'units: 0.93 arcsecond x 0.93 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE7',det_or_raw//'Y',
     &     'Corrected Y coordinate of photon',status)
      CALL FTMKYS(lout,'TFORM7','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT7','pixel',
     &     'units: 0.93 arcsecond x 0.93 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE8','STATUS','Status flag for the event',
     &     status)
      CALL FTMKYS(lout,'TFORM8','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT8','CODED','units: none',status)
      CALL FTMKYS(lout,'EXTNAME','STDEVT','Standard Events',status)
C     PRINT * , 'EXTNAME:' , extname , status
C
C     Write additional keywords to new extension header
C
      content = 'BASIC'
      comment = 'BASIC SCIENCE DATA (EVENTS & GOOD TIMES)'
      CALL FTPKYS(lout,'CONTENT',content,comment,status)
C
      origin = 'USRSDC'
      comment = 'origin of processed data'
      CALL FTPKYS(lout,'ORIGIN',origin,comment,status)
C
C     date = '31/01/94'
C     comment = 'FITS creation date (DD/MM/YY)'
C     CALL FTPKYS(lout,'DATE',date,comment,status)
      call ftpdat(lout,status)
C
      telescop = 'ROSAT'
      comment = 'mission name'
      CALL FTPKYS(lout,'TELESCOP',telescop,comment,status)
C
      zerodate = '01/06/90'
      comment = 'UT date of SC start (DD/MM/YY)'
      CALL FTPKYS(lout,'ZERODATE',zerodate,comment,status)
C
      zerotime = '21:06:50'
      comment = 'UT time of SC start (HH:MM:SS)'
      CALL FTPKYS(lout,'ZEROTIME',zerotime,comment,status)
C
      irafname = ' '
      irafname = irafprefix(1:CLENACT(irafprefix)) // '_stdevt.tab'
      comment = 'IRAF file name'
      CALL FTPKYS(lout,'IRAFNAME',irafname,comment,status)
C
      setupid = 'NOMINAL'
      comment = 'Instrument setup'
      CALL FTPKYS(lout,'SETUPID',setupid,comment,status)
C
      mjdrefi = 48043
      comment = 'MJD integer SC clock start'
      CALL FTPKYJ(lout,'MJDREFI',mjdrefi,comment,status)
C
      mjdreff = 8.79745370370074E-01
      comment = 'MJD fraction SC clock start'
      CALL FTPKYD(lout,'MJDREFF',mjdreff,8,comment,status)
C
      revision = 1
      comment = 'Revision number of processed data'
      CALL FTPKYJ(lout,'REVISION',revision,comment,status)
C
      qpoename= irafprefix(:clenact(irafprefix))//'.qp'
      comment = 'IRAF QPOE file name'
      CALL FTPKYS(lout,'QPOENAME',qpoename,comment,status)
C
      CALL FTPKYS(lout,'OBS_MODE',obs_mode,
     &     'obs mode: POINTING, SLEW, or SCAN',status)
C
      CALL FTPKYS(lout,'INSTRUME',instrume,
     &     'instrument used for observation',status)
C
      CALL FTPKYS(lout,'FILTER',filter,'filter id: NONE OR BORON',
     &     status)
C
      comment = 'observation ID '
      CALL FTPKYS(lout,'OBS_ID',obs_id,comment,status)
C
      comment = 'nominal RA (deg)'
      CALL FTPKYD(lout,'RA_NOM',ra_nom,6,comment,status)
C
      comment = 'nominal DEC (deg)'
      CALL FTPKYD(lout,'DEC_NOM',dec_nom,6,comment,status)
C
      equinox = 2.000000E3
      CALL FTPKYD(lout,'EQUINOX',equinox,6,
     &     'equinox of celestial coord. system',status)
C
      CALL FTPKYS(lout,'OBJECT',object,'name of observed object',status)
C
      comment = ' '
      CALL FTPKYS(lout,'DATE-OBS',date_obs,comment,status)
      CALL FTPKYS(lout,'DATE-END',date_end,comment,status)
C
      comment = ' '
      CALL FTPKYS(lout,'TIME-OBS',time_obs,comment,status)
      CALL FTPKYS(lout,'TIME-END',time_end,comment,status)
C
      comment = 'SC seq start (sec)'
      CALL FTPKYJ(lout,'SCSEQBEG',scseqbeg,comment,status)
C
      comment = 'SC seq end (sec)'
      CALL FTPKYJ(lout,'SCSEQEND',scseqend,comment,status)
C
      comment = 'SC seq start (sec)'
      CALL FTPKYJ(lout,'TSTART',scseqbeg,comment,status)
C
      comment = 'SC seq end (sec)'
      CALL FTPKYJ(lout,'TSTOP',scseqend,comment,status)
C
      comment = 'On time '
      CALL FTPKYD(lout,'ONTIME',ontime,6,comment,status)
C     PRINT * , 'ONTIME status: ' , ontime , status
C
      hduclass = 'OGIP'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLASS',hduclass,comment,status)
C
      hduclas1 = 'EVENTS'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS1',hduclas1,comment,status)
C
      hduclas2 = 'ACCEPTED'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS2',hduclas2,comment,status)
C
      tctyp1 = 'RA---TAN'
      comment = 'Coord. type: RA tangent plane projection'
      CALL FTPKYS(lout,'TCTYP1',tctyp1,comment,status)
C
      tctyp2 = 'DEC--TAN'
      comment = 'Coord. type: DEC tangent plane projection'
      CALL FTPKYS(lout,'TCTYP2',tctyp2,comment,status)
C
      tcrpx1 = 7.681000E+03
      comment = 'X axis reference pixel of projected image'
      CALL FTPKYD(lout,'TCRPX1',tcrpx1,6,comment,status)
C
      tcdlt1 = -1.388889E-04
      comment = 'X increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT1',tcdlt1,6,comment,status)
C
      tcrot1 = 0.000000E+00
      comment = 'Rotation angle (degrees)'
      CALL FTPKYD(lout,'TCROT1',tcrot1,6,comment,status)
C
      talen1 = 15360
      comment = 'Dimension of QPOE projected image X axis'
      CALL FTPKYJ(lout,'TALEN1',talen1,comment,status)
C
      equinox = 2.000000E+03
      comment = 'Equinox for sky coordinate system X axis'
      CALL FTPKYD(lout,'EQUINOX',equinox,6,comment,status)
C
      radecsys = 'FK5'
      comment = 'World coord. system for this file'
      CALL FTPKYS(lout,'RADECSYS',radecsys,comment,status)
C
      tcrpx2 = 7.681000E+03
      comment = 'Y axis reference pixel of projected image'
      CALL FTPKYD(lout,'TCRPX2',tcrpx2,6,comment,status)
C
      tcdlt2 = 1.388889E-04
      comment = 'Y increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT2',tcdlt2,6,comment,status)
C
      tcrot2 = 0.000000E+00
      comment = 'Rotation angle (degrees)'
      CALL FTPKYD(lout,'TCROT2',tcrot2,6,comment,status)
C
      talen2 = 15360
      comment = 'Dimension of QPOE projected image Y axis'
      CALL FTPKYJ(lout,'TALEN2',talen2,comment,status)
C
      tcrpx3 = 1.000000E+00
      comment = 'PHA reference channel'
      CALL FTPKYD(lout,'TCRPX3',tcrpx3,6,comment,status)
C
      comment = 'Sky coord. (degrees) at X axis ref. pixel'
      CALL FTPKYD(lout,'TCRVL1',tcrvl1,6,comment,status)
C
      comment = 'Sky coord. (degrees) at Y axis ref. pixel'
      CALL FTPKYD(lout,'TCRVL2',tcrvl2,6,comment,status)
C
      tcrvl3 = 1.000000E+00
      comment = 'Channel value at PHA axis reference'
      CALL FTPKYD(lout,'TCRVL3',tcrvl3,6,comment,status)
C
      tcdlt3 = 1.000000E+00
      comment = 'PHA channel increment'
      CALL FTPKYD(lout,'TCDLT3',tcdlt3,6,comment,status)
C
      talen3 = 256
      comment = 'Dimension of PHA axis'
      CALL FTPKYJ(lout,'TALEN3',talen3,comment,status)
C
      tcrpx4 = 1.000000E+00
      comment = 'PI reference channel'
      CALL FTPKYD(lout,'TCRPX4',tcrpx4,6,comment,status)
C
      tcrvl4 = 1.000000E+00
      comment = 'Channel value at PI axis reference'
      CALL FTPKYD(lout,'TCRVL4',tcrvl4,6,comment,status)
C
      tcdlt4 = 1.000000E+00
      comment = 'PI channel increment'
      CALL FTPKYD(lout,'TCDLT4',tcdlt4,6,comment,status)
C
      talen4 = 256
      comment = 'Dimension of PI axis'
      CALL FTPKYJ(lout,'TALEN4',talen4,comment,status)
C
      tcrpx6 = 4.119000E+03
      comment = 'Detector X axis reference pixel'
      CALL FTPKYD(lout,'TCRPX6',tcrpx6,6,comment,status)
C
      tcdlt6 = 2.595021E-04
      comment = 'X increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT6',tcdlt6,6,comment,status)
C
      talen6 = 8192
      comment = 'Dimension of corrected detector X axis'
      CALL FTPKYJ(lout,'TALEN6',talen6,comment,status)
C
      tcrpx7 = 3.929000E+03
      comment = 'Detector Y axis reference pixel'
      CALL FTPKYD(lout,'TCRPX7',tcrpx7,6,comment,status)
C
      tcdlt7 = -2.595021E-04
      comment = 'Y increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT7',tcdlt7,6,comment,status)
C
      talen7 = 8192
      comment = 'Dimension of corrected detector axis'
      CALL FTPKYJ(lout,'TALEN7',talen7,comment,status)
C
      CALL FTPKYJ(lout,'TLMIN1',tlmin1,
     &     'Minimum legal QPOE projected image X axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX1',tlmax1,
     &     'Maximum legal QPOE projected image X axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN2',tlmin2,
     &     'Minimum legal QPOE projected image Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX2',tlmax2,
     &     'Maximum legal QPOE projected image Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN3',tlmin3,
     &     'Minimum legal PHA axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX3',tlmax3,
     &     'Maximum legal PHA axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN4',tlmin4,
     &     'Mimimun legal PI axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX4',tlmax4,
     &     'Maximum legal PI axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN6',tlmin6,
     &     'Miminum legal raw X axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX6',tlmax6,
     &     'Maximum legal raw X axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN7',tlmin7,
     &     'Minimum legal raw Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX7',tlmax7,
     &     'Maximum legal raw Y axis value',status)
      if (status.ne.0) then
         context = 'Error writing STDEVT HDU keywords'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif

C
C     Define the data array for the second extension
C
      CALL FTBDEF(lout,tfields,tform,varidat,nrows,status)
C     PRINT * , 'STDEVT FTBDEF status:' , status
C



C     Find column numbers

      call ftgcno(luin,.false.,'TIME',timecol,status)
      if(timecol.ne.1) then
         context='TIME is not the first column in the input file'
         call fcerr(context)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Finding STDEVT HDU columns...'
         call fcecho(context)
      endif
      call ftgcno(luin,.false.,'AMPL',amplcol,status)
      if(.not.pspcfile.and.status.ne.0) then
C     HRI files don't appear to have an 'AMPL' column
C     so it's not an error
         status=0
         amplcol=0
         ampl=0
         amplflag=.false.
         call ftcmsg()
      else
         amplflag=.true.
      endif
      call ftgcno(luin,.false.,'XPIX',xpixcol,status)
      call ftgcno(luin,.false.,'YPIX',ypixcol,status)
      call ftgcno(luin,.false.,'XDET',xdetcol,status)
      call ftgcno(luin,.false.,'YDET',ydetcol,status)
      call ftgcno(luin,.false.,'RAW_AMPL',phacol,status)

      if (status.ne.0) then
         context = 'Error finding STDEVT HDU columns'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Writing STDEVT table...'
         call fcecho(context)
      endif



C     Copy and transform data


C     
C
      DO i = 1, nrows
C     
C     get one row from the input file
         call ftgtbs(luin,i,1,rowlen,buffer,status)
C     split it into component variables
         read(buffer,*) time,(ivect(ii),ii=2,tfields_in)
         if(amplflag) ampl=ivect(amplcol)
         xpix=ivect(xpixcol)
         ypix=ivect(ypixcol)
         xdet=ivect(xdetcol)
         ydet=ivect(ydetcol)
         pha=ivect(phacol)
         
         

C     now update the parameters that need it
C     
C     
C     'AMPL' = 'PI'
C     
         pi = ampl
C     
C     'XPIX' = X
C     
         x = xpix + xpixadj
C     
C     'YPIX' = Y
C     
         y = ypixadj - ypix
C     
C     'XDET' to detx
C     
         detx = xdet - 1
c
c     'YDET' to dety
c
          dety = ydet - 1
C
c     write 'TIME' to binary file
c
          CALL FTPCLD(lout,5,i,1,1,time,status)
c
c     write 'PI' to binary file
c
          CALL FTPCLI(lout,4,i,1,1,pi,status)
C
C     write 'X' to binary file
C     
         CALL FTPCLI(lout,1,i,1,1,x,status)
C     
C     write 'Y' to binary file
C     
         CALL FTPCLI(lout,2,i,1,1,y,status)
C     
C     write 'DETX' to binary file
C     
         CALL FTPCLI(lout,6,i,1,1,detx,status)
C     
C     write 'DETY' to binary file
C     
         CALL FTPCLI(lout,7,i,1,1,dety,status)
C     
C     write 'PHA' to binary file
C     
         CALL FTPCLI(lout,3,i,1,1,pha,status)
C     
C     Now write value for 'STATUS' to new binary file.  Stat = 0 for
c     STDEVT.
C
         CALL FTPCLI(lout,8,i,1,1,stat,status)
C     
      ENDDO


      if (status.ne.0) then
         context = 'Error writing STDEVT table'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Opening rejected events file '
         call fcecho(context)
      endif
      if(revtfile.ne.'NONE') then
C
C******************************************************************************c
C
C     Open *_difevents.tfits file to get rejected data information.  Use
c     same
C     other keywords as for STDEVT.  If the *_difevents files does not
c     exist,
C     skip it and close the outfile.  These will be no REJEVT extension.
C
      status = 0
      CALL CGETLUN(luin2)
      CALL FTOPEN(luin2,revtfile,0,block,status)
      IF ( status.NE.0 ) THEN
         context=' Unable to open rejected events file '
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      ENDIF
      if(chatter.gt.10) then
         context='Writing REJEVT HDU keywords...'
         call fcecho(context)
      endif
C     Move ahead to the data extension in the MPE ASCII FITS rejected
c     events
C     table
C
      CALL FTMAHD(luin2,2,hdutype,status)
C     PRINT * , 'REJEVT FTMAHD status:' , status
C
C     Get keywords from ASCII header
C
      CALL FTGHTB(luin2,MAXDIM,rowlen,nrows,tfields_in,ttype,tbcol,
     $     tform,tunit,extname,status)
C
C     Create the extension header
C
      CALL FTCRHD(lout,status)
C     PRINT * , 'Create extension header REJEVT status:' , status
      if(chatter.ge.10) then
         write(zwrite,*)'Number of rows in REJEVT table:' , nrows
         call fcecho(zwrite)
      endif
C
C     Write the required keywords to the new extension header.  Hardwire
C     keyword values. Stat = 1 for REJEVT.
C
      stat = 1
      bitpix = 8
C
      tfields = 8
      ttype(1) = 'X'
      tform(1) = '1I'
      tunit(1) = 'pixel'
      ttype(2) = 'Y'
      tform(2) = '1I'
      tunit(2) = 'pixel'
      ttype(3) = 'PHA'
      tform(3) = '1I'
      tunit(3) = 'chan'
      ttype(4) = 'PI'
      tform(4) = '1I'
      tunit(4) = 'chan'
      ttype(5) = 'TIME'
      tform(5) = '1D'
      tunit(5) = 's'
      ttype(6) = det_or_raw//'X'
      tform(6) = '1I'
      tunit(6) = 'pixel'
      ttype(7) = det_or_raw//'Y'
      tform(7) = '1I'
      tunit(7) = 'pixel'
      ttype(8) = 'STATUS'
      tform(8) = '1I'
      tunit(8) = 'CODED'
      extname = 'REJEVT'
C
C     Write the new extension header as binary
C
      CALL FTPHBN(lout,nrows,tfields,ttype,tform,tunit,extname,pcount,
     &     status)
C     PRINT * , 'REJEVT FTPHBN status:' , status
C
C     Write after-the-fact comments for keywords
C
      CALL FTMKYS(lout,'TTYPE1','X',
     &     'Projected X position of photon on sky',status)
      CALL FTMKYS(lout,'TFORM1','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT1','pixel',
     &     'units: 0.5 arcsecond x 0.5 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE2','Y',
     &     'Projected Y position of photon on sky',status)
      CALL FTMKYS(lout,'TFORM2','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT2','pixel',
     &     'units: 0.5 arcsecond x 0.5 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE3','PHA',
     &     'Pulse Height Analyzer bin number',status)
      CALL FTMKYS(lout,'TFORM3','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT3','chan','units: channels',status)
C
      CALL FTMKYS(lout,'TTYPE4','PI','Pulse Invariant bin',status)
      CALL FTMKYS(lout,'TFORM4','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT4','chan','units: channels',status)
C
      CALL FTMKYS(lout,'TTYPE5','TIME','Spacecraft clock time of event',
     &     status)
      CALL FTMKYS(lout,'TFORM5','1D',
     &     'data format of the field : DOUBLE PRECISION',status)
      CALL FTMKYS(lout,'TUNIT5','s','units: seconds',status)
C
      CALL FTMKYS(lout,'TTYPE6',det_or_raw//'X',
     &     'Corrected X coordinate of photon',status)
      CALL FTMKYS(lout,'TFORM6','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT6','pixel',
     &     'units: 0.93 arcsecond x 0.93 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE7',det_or_raw//'Y',
     &     'Corrected Y coordinate of photon',status)
      CALL FTMKYS(lout,'TFORM7','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT7','pixel',
     &     'units: 0.93 arcsecond x 0.93 arcsecond pixel',status)
C
      CALL FTMKYS(lout,'TTYPE8','STATUS','Status flag for the event',
     &     status)
      CALL FTMKYS(lout,'TFORM8','1I',
     &     'data format of the field : 2-byte INTEGER',status)
      CALL FTMKYS(lout,'TUNIT8','CODED','units: none',status)
      CALL FTMKYS(lout,'EXTNAME','REJEVT','Rejected Events',status)
C
C     Write additional keywords to new extension header -- the same
c     keywords
C     for REJEVT as for STDEVT with one exception
C
      content = 'BASIC'
      comment = 'BASIC SCIENCE DATA (EVENTS & GOOD TIMES)'
      CALL FTPKYS(lout,'CONTENT',content,comment,status)
C
      origin = 'USRSDC'
      comment = 'origin of processed data'
      CALL FTPKYS(lout,'ORIGIN',origin,comment,status)
C
C     date = '31/01/94'
C     comment = 'FITS creation date (DD/MM/YY)'
C     CALL FTPKYS(lout,'DATE',date,comment,status)
      call ftpdat(lout,status)
C
      telescop = 'ROSAT'
      comment = 'mission name'
      CALL FTPKYS(lout,'TELESCOP',telescop,comment,status)
C
      zerodate = '01/06/90'
      comment = 'UT date of SC start (DD/MM/YY)'
      CALL FTPKYS(lout,'ZERODATE',zerodate,comment,status)
C
      zerotime = '21:06:50'
      comment = 'UT time of SC start (HH:MM:SS)'
      CALL FTPKYS(lout,'ZEROTIME',zerotime,comment,status)
C
      irafname = ' '
      irafname = irafprefix(1:CLENACT(irafprefix)) // '_rejevt.tab'
      comment = 'IRAF file name'
      CALL FTPKYS(lout,'IRAFNAME',irafname,comment,status)
C
      setupid = 'NOMINAL'
      comment = 'Instrument setup'
      CALL FTPKYS(lout,'SETUPID',setupid,comment,status)
C
      mjdrefi = 48043
      comment = 'MJD integer SC clock start'
      CALL FTPKYJ(lout,'MJDREFI',mjdrefi,comment,status)
C
      mjdreff = 8.79745370370074E-01
      comment = 'MJD fraction SC clock start'
      CALL FTPKYD(lout,'MJDREFF',mjdreff,8,comment,status)
C
      revision = 1
      comment = 'Revision number of processed data'
      CALL FTPKYJ(lout,'REVISION',revision,comment,status)
C
      qpoename= irafprefix(:clenact(irafprefix))//'.qp'
      comment = 'IRAF QPOE file name'
      CALL FTPKYS(lout,'QPOENAME',qpoename,comment,status)

      CALL FTPKYS(lout,'OBS_MODE',obs_mode,
     &     'obs mode: POINTING, SLEW, or SCAN',status)
C
      CALL FTPKYS(lout,'INSTRUME',instrume,
     &     'instrument used for observation',status)
C
      CALL FTPKYS(lout,'FILTER',filter,'filter id: NONE OR BORON',
     &     status)
C
      comment = 'observation ID '
      CALL FTPKYS(lout,'OBS_ID',obs_id,comment,status)
C
      comment = 'nominal RA (deg)'
      CALL FTPKYD(lout,'RA_NOM',ra_nom,6,comment,status)
C
      comment = 'nominal DEC (deg)'
      CALL FTPKYD(lout,'DEC_NOM',dec_nom,6,comment,status)
C
      equinox = 2.000000E3
      CALL FTPKYD(lout,'EQUINOX',equinox,6,
     &     'equinox of celestial coord. system',status)
C
      CALL FTPKYS(lout,'OBJECT',object,'name of observed object',status)
C     PRINT * , 'OBJECT status:' , status
C
      comment = ' '
      CALL FTPKYS(lout,'DATE-OBS',date_obs,comment,status)
      CALL FTPKYS(lout,'DATE-END',date_end,comment,status)
C
      comment = ' '
      CALL FTPKYS(lout,'TIME-OBS',time_obs,comment,status)
      CALL FTPKYS(lout,'TIME-END',time_end,comment,status)
C
      comment = 'SC seq start (sec)'
      CALL FTPKYJ(lout,'SCSEQBEG',scseqbeg,comment,status)
C     PRINT * , 'SCSEQBEG status:' , status
C
      comment = 'SC seq end (sec)'
      CALL FTPKYJ(lout,'SCSEQEND',scseqend,comment,status)
C     PRINT * , 'SCSEQEND status:' , status
C
      comment = 'On time '
      CALL FTPKYD(lout,'ONTIME',ontime,6,comment,status)
C     PRINT * , 'ONTIME status:' , ontime , status
C
      hduclass = 'OGIP'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLASS',hduclass,comment,status)
C
      hduclas1 = 'EVENTS'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS1',hduclas1,comment,status)
C
C     All other keywords are the same for REJEVT as for STDEVT except
C     for hduclas2 which is 'REJECTED' instead of 'ACCEPTED'
C
      hduclas2 = 'REJECTED'
      comment = ' '
      CALL FTPKYS(lout,'HDUCLAS2',hduclas2,comment,status)
C
      tctyp1 = 'RA---TAN'
      comment = 'Coord. type: RA tangent plane projection'
      CALL FTPKYS(lout,'TCTYP1',tctyp1,comment,status)
C
      tctyp2 = 'DEC--TAN'
      comment = 'Coord. type: DEC tangent plane projection'
      CALL FTPKYS(lout,'TCTYP2',tctyp2,comment,status)
C
      tcrpx1 = 7.681000E+03
      comment = 'X axis reference pixel of projected image'
      CALL FTPKYD(lout,'TCRPX1',tcrpx1,6,comment,status)
C
      tcdlt1 = -1.388889E-04
      comment = 'X increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT1',tcdlt1,6,comment,status)
C
      tcrot1 = 0.000000E+00
      comment = 'Rotation angle (degrees)'
      CALL FTPKYD(lout,'TCROT1',tcrot1,6,comment,status)
C
      talen1 = 15360
      comment = 'Dimension of QPOE projected image X axis'
      CALL FTPKYJ(lout,'TALEN1',talen1,comment,status)
C
      equinox = 2.000000E+03
      comment = 'Equinox for sky coordinate system X axis'
      CALL FTPKYD(lout,'EQUINOX',equinox,6,comment,status)
C
      radecsys = 'FK5'
      comment = 'World coord. system for this file'
      CALL FTPKYS(lout,'RADECSYS',radecsys,comment,status)
C
      tcrpx2 = 7.681000E+03
      comment = 'Y axis reference pixel of projected image'
      CALL FTPKYD(lout,'TCRPX2',tcrpx2,6,comment,status)
C
      tcdlt2 = -1.388889E-04
      comment = 'Y increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT2',tcdlt2,6,comment,status)
C
      tcrot2 = 0.000000E+00
      comment = 'Rotation angle (degrees)'
      CALL FTPKYD(lout,'TCROT2',tcrot2,6,comment,status)
C
      talen2 = 15360
      comment = 'Dimension of QPOE projected image Y axis'
      CALL FTPKYJ(lout,'TALEN2',talen2,comment,status)
C
      tcrpx3 = 1.000000E+00
      comment = 'PHA reference channel'
      CALL FTPKYD(lout,'TCRPX3',tcrpx3,6,comment,status)
C
      comment = 'Sky coord. (degrees) at X axis ref. pixel'
      CALL FTPKYD(lout,'TCRVL1',tcrvl1,6,comment,status)
C
      comment = 'Sky coord. (degrees) at Y axis ref. pixel'
      CALL FTPKYD(lout,'TCRVL2',tcrvl2,6,comment,status)
C
      tcrvl3 = 1.000000E+00
      comment = 'Channel value at PHA axis reference'
      CALL FTPKYD(lout,'TCRVL3',tcrvl3,6,comment,status)
C
      tcdlt3 = 1.000000E+00
      comment = 'PHA channel increment'
      CALL FTPKYD(lout,'TCDLT3',tcdlt3,6,comment,status)
C
      talen3 = 256
      comment = 'Dimension of PHA axis'
      CALL FTPKYJ(lout,'TALEN3',talen3,comment,status)
C
      tcrpx4 = 1.000000E+00
      comment = 'PI reference channel'
      CALL FTPKYD(lout,'TCRPX4',tcrpx4,6,comment,status)
C
      tcrvl4 = 1.000000E+00
      comment = 'Channel value at PI axis reference'
      CALL FTPKYD(lout,'TCRVL4',tcrvl4,6,comment,status)
C
      tcdlt4 = 1.000000E+00
      comment = 'PI channel increment'
      CALL FTPKYD(lout,'TCDLT4',tcdlt4,6,comment,status)
C
      talen4 = 256
      comment = 'Dimension of PI axis'
      CALL FTPKYJ(lout,'TALEN4',talen4,comment,status)
C
      tcrpx6 = 4.119000E+03
      comment = 'Detector X axis reference pixel'
      CALL FTPKYD(lout,'TCRPX6',tcrpx6,6,comment,status)
C
      tcdlt6 = 2.595021E-04
      comment = 'X increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT6',tcdlt6,6,comment,status)
C
      talen6 = 8192
      comment = 'Dimension of corrected detector X axis'
      CALL FTPKYJ(lout,'TALEN6',talen6,comment,status)
C
      tcrpx7 = 3.929000E+03
      comment = 'Detector Y axis reference pixel'
      CALL FTPKYD(lout,'TCRPX7',tcrpx7,6,comment,status)
C
      tcdlt7 = -2.595021E-04
      comment = 'Y increment (degrees per pixel) at ref. pixel'
      CALL FTPKYD(lout,'TCDLT7',tcdlt7,6,comment,status)
C
      talen7 = 8192
      comment = 'Dimension of corrected detector axis'
      CALL FTPKYJ(lout,'TALEN7',talen7,comment,status)
C
      CALL FTPKYJ(lout,'TLMIN1',tlmin1,
     &     'Minimum legal QPOE projected image X axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX1',tlmax1,
     &     'Maximum legal QPOE projected image X axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN2',tlmin2,
     &     'Minimum legal QPOE projected image Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX2',tlmax2,
     &     'Maximum legal QPOE projected image Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN3',tlmin3,
     &     'Minimum legal PHA axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX3',tlmax3,
     &     'Maximum legal PHA axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN4',tlmin4,
     &     'Mimimun legal PI axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX4',tlmax4,
     &     'Maximum legal PI axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN6',tlmin6,
     &     'Miminum legal raw X axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX6',tlmax6,
     &     'Maximum legal raw X axis value',status)
C
      CALL FTPKYJ(lout,'TLMIN7',tlmin7,
     &     'Minimum legal raw Y axis value',status)
C
      CALL FTPKYJ(lout,'TLMAX7',tlmax7,
     &     'Maximum legal raw Y axis value',status)
      if (status.ne.0) then
         context = 'Error writing REJEVT HDU keywords'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
C
C     Define the data array for the REJEVT extension
C
      CALL FTBDEF(lout,tfields,tform,varidat,nrows,status)
C     PRINT * , 'REJEVT FTBDEF status:' , status
C



C     Find column numbers

      call ftgcno(luin2,.false.,'TIME',timecol,status)
      if(timecol.ne.1) then
         context='TIME is not the first column in the input file'
         call fcerr(context)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Finding STDEVT HDU columns...'
         call fcecho(context)
      endif
      call ftgcno(luin2,.false.,'AMPL',amplcol,status)
      if(.not.pspcfile.and.status.ne.0) then
C     HRI files don't appear to have an 'AMPL' column
C     so it's not an error
         status=0
         amplcol=0
         ampl=0
         amplflag=.false.
         call ftcmsg()
      else
         amplflag=.true.
      endif
      call ftgcno(luin2,.false.,'XPIX',xpixcol,status)
      call ftgcno(luin2,.false.,'YPIX',ypixcol,status)
      call ftgcno(luin2,.false.,'XDET',xdetcol,status)
      call ftgcno(luin2,.false.,'YDET',ydetcol,status)
      call ftgcno(luin2,.false.,'RAW_AMPL',phacol,status)

      if (status.ne.0) then
         context = 'Error finding STDEVT HDU columns'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      if(chatter.gt.10) then
         context='Writing REJEVT table'
         call fcecho(context)
      endif



C     Copy and transform data


C     
C
      DO i = 1, nrows
C     
C     get one row from the input file
         call ftgtbs(luin2,i,1,rowlen,buffer,status)
C     split it into component variables
         read(buffer,*) time,(ivect(ii),ii=2,tfields_in)
         if(amplflag) ampl=ivect(amplcol)
         xpix=ivect(xpixcol)
         ypix=ivect(ypixcol)
         xdet=ivect(xdetcol)
         ydet=ivect(ydetcol)
         pha=ivect(phacol)
         
         

C     now update the parameters that need it
C     
C     
C     'AMPL' = 'PI'
C     
         pi = ampl
C     
C     'XPIX' = X
C     
         x = xpix + xpixadj
C     
C     'YPIX' = Y
C     
         y = ypixadj - ypix
C     
C     'XDET' to detx
C     
         detx = xdet - 1
c
c     'YDET' to dety
c
          dety = ydet - 1
C
c     write 'TIME' to binary file
c
         CALL FTPCLD(lout,5,i,1,1,time,status)
c
c     write 'PI' to binary file
c
         CALL FTPCLI(lout,4,i,1,1,pi,status)
C     
C     write 'X' to binary file
C     
         CALL FTPCLI(lout,1,i,1,1,x,status)
C     
C     write 'Y' to binary file
C     
         CALL FTPCLI(lout,2,i,1,1,y,status)
C     
C     write 'DETX' to binary file
C     
         CALL FTPCLI(lout,6,i,1,1,detx,status)
C     
C     write 'DETY' to binary file
C     
         CALL FTPCLI(lout,7,i,1,1,dety,status)
C     
C     write 'PHA' to binary file
C     
         CALL FTPCLI(lout,3,i,1,1,pha,status)
C     
C     Now write value for 'STATUS' to new binary file.  Stat = 1 for
c     REJEVT 
C
         CALL FTPCLI(lout,8,i,1,1,stat,status)
C     
      ENDDO



      if (status.ne.0) then
         context = 'Error writing REJEVT table'
         call fcerr(context)
         call fcerrm(status)
         goto 99000
      endif
      endif                                                             
C
C     Close evtfile, revtfile, and outfile
C
99000 continue
C
C     Print the name of the new file if successful
C
      if(status.eq.0.and.chatter.ge.5) then
            write(zwrite2,*)'New file ' , outfile(:clenact(outfile)),
     $        ' written'
         call fcecho(zwrite2)      
      endif
      savestatus=status
      status = 0

      CALL FTCLOS(luin,status)
      CALL CFRELUN(luin)

      status=0
      if (revtfile.ne.'NONE') then
      CALL FTCLOS(luin2,status)
      CALL CFRELUN(luin2)
      endif
C     

      status=0
      CALL FTCLOS(lout,status)
      CALL CFRELUN(lout)

      status = savestatus
C
99001 FORMAT (a40)
99002 FORMAT (a32,i4)
      return
      END

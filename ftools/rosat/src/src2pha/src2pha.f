*+SRC2PA
C 
C Description:
C  Program extracts a PHA file from a ROSAT RDF SRC file.
C
C Passed Parameters
C  None
C
C User i/ps required (prompted for):
C  None here, isolated in GPSRC2PHA(see below)
C
C Include/Common blocks files
C  common TASK                   : (FTOOLS) standard fatal error message thingy
C 
C Called routines
C  subroutine gpsrc2pha       : (below) Gets parameters from XPI par file
C  subroutine extsrc2pha      : (below) Performs the calculations
C  
C Compilation:
C  subroutines require CALLIB, FTOOLS, FITSIO
C 
C Principal Local Variables:
C  infile - Name of input SRC file
C  specext - Number of spectral extension to be extracted
C  oahext - Number of OAH extension to be extracted
C  outfile - Name of output PHA file
C  chatter - How much to tell the user
C  clobber - Overwrite output file by default if true
C  status - zero if no errors
C  
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Lawrence E Brown (1.0.0:1994 Dec), written to take over non-ftoolized
C                                     functions of extpha
C  Peter D Wilson (1.0.1:1998 Jun), Updated for new FCPARS behavior
*- 
      subroutine src2pa
      implicit none
      
      character(7) version
      parameter (version = '1.0.1')
C     Internals 
      character(40) taskname
      integer chatter, status
      character(80) infile,outfile
      character(160) message
      integer specext,oahext
      logical clobber
C     Initialize
      COMMON/task/taskname
      taskname ='SRC2PHA '//version
      status = 0
      
      
      
C     Get Parameters from the par file
      call gpsrc2pha(infile,outfile,specext,oahext,
     $     clobber,chatter,status)
      if(status.NE.0) goto 999
      
C     extract those suckers
      call extsrc2pha(infile,outfile,specext,oahext,
     $     clobber,chatter,status)


 999  if(status.NE.0) then
         message = '** SRC2PHA '//version//' ERROR : '//
     $        'Incomplete Execution'
         call fcerr(message)
      else 
         if(chatter.gt.5) then
            message = '** SRC2PHA '//version//' Finished'
            call fcecho(message)
         endif
      endif

      return
      end

C -------------------------------------------------------------------------
*+GPSRC2PHA
      subroutine gpsrc2pha(infile,outfile,specext,oahext,
     $     clobber,chatter,status)

      IMPLICIT NONE
      integer chatter, status
      character*(*) infile,outfile
      integer specext,oahext
      logical clobber


C 
C Description:
C  Gets the parameters required by SRC2PHA from the parameter file
C  NOTE - The par file is assumed to have been opened.
C

C Origin:
C  Original
C
C Passed parameters
C  infile - Name of input SRC file
C  specext - Number of spectral extension to be extracted
C  oahext - Number of OAH extension to be extracted
C  outfile - Name of output PHA file
C  chatter - How much to tell the user
C  clobber - Overwrite output file by default if true
C  status - zero if no errors
C
C Called Routines
C  subroutine FCERR        : (FTOOLS) writes to stanard error
C  subroutine FCERRM       : (FTOOLS) Writes standard FITSIO message,
C                            dumps error stack
C  subroutine FCECHO       : (FTOOLS) writes to standard o/p device
C  subroutine FTCMSG       : (FITSIO) clears the FITSIO error stack
C  subroutine FCPARSE      : (FTOOLS) parses an input filename
C  subroutine CK_FILE      : (CALLIB) check output filename for legality
C  subroutine CRMVLBK      : (CALLIB) remove leading blanks
C  subroutine UCLG_        : (XPI) Get parameter values from par file
C
C Compilation:
C  requires XPI/Host interface etc 
C
C Authors/Modification History:
C  Ian M George     (1.0.0: December 1994) Original
C  Peter D Wilson   (1.0.1: June 30, 1998)
C                           Drop INQUIRE test. Replace fcpars with ftrtnm
      character(7) version
      parameter (version = '1.0.1')
*- 
C Internals
      character(30)  errstr, wrnstr
      character(160) message,filnam,ill_files(1)
      integer extnum,n_ill
      logical exst,valfil

      character(40) taskname
      COMMON/task/taskname

C Initialize
      errstr = '** GPSRC2PHA '//version//' ERROR: '
      wrnstr = '** GPSRC2PHA '//version//' WARNING: '
C Get the chattiness flag
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


C Get the name of the input file
      call uclgst('infile',infile, status)
      if(status.NE.0) then
        message = errstr // 'Getting INFILE parameter'
        call fcerr(message)
        call fcerrm(status)
        return
      endif
      
      call crmvlbk(infile)
C PDW 6/30/98: Drop INQUIRE test. Replace fcpars with ftrtnm
C      call fcpars(infile,filnam,extnum,status)
      call ftrtnm( infile, filnam, status )
      ill_files(1)=filnam

C      inquire(file=filnam,exist=exst)
C      if((.not.exst).or.(filnam.eq.' '))then
C         message='Input file does not exist: '//filnam
C         call fcerr(message)
C         status=1
C         return
C      endif

C     since this task doesn't use extension numbers like this we'll send back
C     the stripped filename
      infile=filnam

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

      valfil=.true.
      n_ill=1
      call ck_file(outfile,ill_files,n_ill,valfil,clobber,chatter)
      if(.not.valfil) then
         message='Output file is invalid'
         call fcerr(message)
         status=1
         return
      endif      

C     Get the spectral extension number
      call uclgsi('specext',specext, status)
      if(status.NE.0) then
       message = 'Error Getting spectral extension (specext) parameter'
         call fcerr(message)
         call fcerrm(status)
         return
      endif

C     Get the OAH extension number
      call uclgsi('oahext',oahext, status)
      if(status.NE.0) then
       message = 'Error Getting OAH extension (oahext) parameter'
         call fcerr(message)
         call fcerrm(status)
         return
      endif
      
      if(oahext.eq.specext) then
         message= 
     $'Warning: Spectral and OAH extension numbers are equal.'
         call fcecho(message)
         message= 'This is probably wrong.'
         call fcecho(message)
      endif
      return
      end

C-------------------------------------------------------------------
*+
      subroutine extsrc2pha(infile,outfile,specext,oahext,
     $     clobber,chatter,status)
      implicit none
      integer chatter, status
      character*(*) infile,outfile
      integer specext,oahext
      logical clobber
C DESCRIPTION
C     Takes extensions specext and oahext from input file infile and puts 
C     them in output file oahfile which is in the form of a standard PHA
C     file.
C
C ARGUMENTS
C  infile - Name of input SRC file
C  specext - Number of spectral extension to be extracted
C  oahext - Number of OAH extension to be extracted
C  outfile - Name of output PHA file
C  chatter - How much to tell the user
C  clobber - Overwrite output file by default if true
C  status - zero if no errors
C
C
C CALLED SUBROUTINES
C     fcerr           writes to STDERR
C     fcecho          writes to STDOUT
C     fcerrm          translates fitsio error number to message and dumps
C                       fitsio error stack 
C     cgetlun,cfrelun get/free an unused/used lun (CALLIB)
C     opfits          open a fits file for writing (with clobber test) (CALLIB)
C     ft_             FITSIO subroutines
C
C AUTHORS/MODIFICATION HISTORY
C     Lawrence E. Brown (1.0) Original version, December 1994
      character(7) version
      parameter (version = '1.0.0')
*-
C******************************************************************************
C   the following MEM common block definition is in the system iraf77.inc file
C   and is used for dynamic memory allocation 
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
C     note:
C      datatype      value
C      logical        1
C      integer*2      3
C      Integer        4
C      Long Integer   5
C      Real           6
C      Double         7
C      Complex        8
C******************************************************************************      
C     Local Variables
      character(80) comment,chcomment
      character(160) message
      integer maxdim
      parameter(maxdim=2)
      integer luin, lout, block, htype, nchans, savestatus
      integer theta,sou_frac,b_frac,thetamin,thetamax,ntheta
      integer clenact
      external clenact
      logical anyf
      integer varidat,tfields
      character(80) ttype(maxdim),tform(maxdim),tunit(maxdim)
      character(80) extname
      character(70) hist(1),comm(1)
      character(5) phaversn
      character(16) instrume,filter
      character(16) chantyp,hduclas1
      logical s2pbadkey,GOODDUMMY
      external s2pbadkey,GOODDUMMY
      real exposure,areascal,nullval
      nullval=0.0

      
      if(chatter.gt.10) then
         message='Extracting spectra with EXTSRC2PHA'//version
         call fcecho(message)
         message='Opening files...'
         call fcecho(message)
      endif

      status = 0
      CALL CGETLUN(luin)
      CALL ftopen(luin,infile,0,block,status)
      IF ( status.NE.0 ) THEN
         message = ' Unable to open infile. '
         call fcerr(message)
         call fcerrm(status)
         GOTO 999
      ENDIF
C
C     Initialize the new FITS file for writing
C

      call opnpa(outfile,chatter,lout,clobber,status)
      if(status.ne.0) then
         message='Trouble opening'//outfile
         call fcerr(message)
         goto 999
      endif

C     process spectral extension
      
      call ftmahd(luin,specext+1,htype,status)
      if(status.ne.0) then
         message='Couldn''t move to input spectral extension'
         call fcerr(message)
         goto 999
      endif
      call ftgkys(luin,'HDUCLAS1',hduclas1,comment,status)
      if(hduclas1.ne.'SPECTRUM') then
         write(message,*) 'Extension ',specext,
     $        ' is not a SPECTRUM extension.'
         call fcerr(message)
         goto 999
      endif
      call ftcrhd(lout,status)
      if(status.ne.0) then
         message='Couldn''t create output spectral extension'
         call fcerr(message)
         goto 999
      endif
      call ftcopy(luin,lout,6,status)
      if(status.ne.0) then
         message='Trouble copying spectral extension'
         call fcerr(message)
         goto 999
      endif
C     Since these are binned spectra, the DETCHANS keyword is wrong
      call ftgkyj(luin,'NCHANS',nchans,chcomment,status)
      call ftukyj(lout,'DETCHANS',nchans,chcomment,status)
      comment='no data quality information specified'
      call ftukyj(lout,'QUALITY',0,comment,status)
      comment='no grouping of the data has been defined'
      call ftukyj(lout,'GROUPING',0,comment,status)
      comment='version of format'
      call ftukys(lout,'HDUVERS1','1.1.0',comment,status)
      call ftdkey(lout,'PHAVERSN',status)
      if(status.ne.0) then
         message='Trouble writing keywords to spectral extension'
         call fcerr(message)
         goto 999
      endif
C     We'll need the following later
      call ftgkye(luin,'EXPOSURE',exposure,comment,status)
      call ftgkye(luin,'AREASCAL',areascal,comment,status)
      call ftgkys(luin,'CHANTYPE',chantyp,comment,status)
      if(status.ne.0) then
         message='Trouble reading spectral extension keywords'        
         call fcerr(message)
         goto 999
      endif

      

C     process OAH extension
      call ftmahd(luin,oahext+1,htype,status)
      if(status.ne.0) then
         message='Couldn''t move to input OAH extension'
         call fcerr(message)
         goto 999
      endif
      call ftghbn(luin,maxdim,ntheta,tfields,ttype,tform,tunit,extname,
     $     varidat,status)
      if(status.ne.0) then
         message='Trouble getting header keywords for OAH extension.'
         call fcerr(message)
         goto 999
      endif
      if(extname(1:3).ne.'OAH') then
         write(message,*) 'Extension ',oahext,
     $        ' is not an OAH extension.'
         call fcerr(message)
         goto 999
      endif
      call s2palloc1(theta,sou_frac,b_frac,thetamin,thetamax,ntheta,
     $     status)
      if(status.ne.0) then
         message='Trouble allocating arrays'
         call fcerr(message)
         goto 999
      endif
      call FTGCVE(luin,1,1,1,ntheta,nullval, 
     $     memr(theta),anyf,status)
      call FTGCVE(luin,2,1,1,ntheta,nullval, 
     $     memr(sou_frac),anyf,status)
      if(status.ne.0) then
         message='Couldn''t get data'
         call fcerr(message)
         goto 999
      endif
      call splittheta(memr(theta),memr(thetamin),memr(thetamax),
     $     ntheta)
      hist(1)='Created by src2pha'//version//'from '//
     $     infile
      comm(1)='You know, Beavis. You''re a damn weirdo.'
      phaversn='1.1.0'
      call FTGKYS(luin,'INSTRUME', instrume,comment,status)
      call FTGKYS(luin,'FILTER', filter,comment,status)
      if(status.ne.0) then
         message='Trouble getting keywords from input file.'
         call fcerr(message)
         goto 999
      endif
      call wtrps1(lout,chatter,1,hist,1,comm,phaversn,
     $     instrume,filter,exposure,areascal,nchans,chantyp,
     $     ntheta,memr(thetamin),memr(thetamax),memr(sou_frac),
     $     memr(b_frac),status)
      if(status.ne.0) then
         message='Trouble writing OAH extension.'
         call fcerr(message)
         goto 999
      endif
      call copyhead(luin,lout,.true.,.true.,s2pbadkey,GOODDUMMY,status)
      if(status.ne.0) then
         message='Trouble copying OAH keywords.'
         call fcerr(message)
         goto 999
      endif
C
C     Close files
C
 999  continue
C
C     Print the name of the new file if successful
C
      if(status.eq.0.and.chatter.ge.5) then
         write(message,*)'New file ' , outfile(:clenact(outfile)),
     $        ' written'
         call fcecho(message)      
      endif
      savestatus=status
      status = 0

      CALL FTCLOS(luin,status)
      CALL CFRELUN(luin)

C     

      status=0
      CALL FTCLOS(lout,status)
      CALL CFRELUN(lout)

      status = savestatus
      return
      end

      subroutine splittheta(theta,thetamin,thetamax,ntheta)
      implicit none
      integer ntheta,i
      real theta(ntheta),thetamin(ntheta),thetamax(ntheta)
      real thetamid
      thetamin(1)=theta(1)
      do i = 2,ntheta
         thetamid=(theta(i-1)+theta(i))/2.0
         thetamin(i)=thetamid
         thetamax(i-1)=thetamid
      enddo
      thetamax(ntheta)=theta(ntheta)
      return
      end


      subroutine s2palloc1(theta,sou_frac,b_frac,thetamin,thetamax,
     $     ntheta,status)
      implicit none
      integer theta,b_frac,sou_frac,thetamin,thetamax,ntheta,status
C     Local variables
      integer m
C******************************************************************************
C   the following MEM common block definition is in the system iraf77.inc file
C   and is used for dynamic memory allocation 
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
C     note:
C      datatype      value
C      logical        1
C      integer*2      3
C      Integer        4
C      Long Integer   5
C      Real           6
C      Double         7
C      Complex        8
C*****************************************************************************
      m=max(ntheta,100)
      theta = 0
      sou_frac = 0
      b_frac = 0
      thetamin = 0
      thetamax = 0
      call udmget(m,6,theta,status)
      call s2pconst(memr(theta),m,0.0)
      call udmget(m,6,sou_frac,status)
      call s2pconst(memr(sou_frac),m,0.0)
      call udmget(m,6,b_frac,status)
      call s2pconst(memr(b_frac),m,0.0)
      call udmget(m,6,thetamin,status)
      call s2pconst(memr(thetamin),m,0.0)
      call udmget(m,6,thetamax,status)
      call s2pconst(memr(thetamax),m,0.0)
      return
      end


      subroutine s2pconst(a,n,c)
C     fills array A (size N) with constant C
      implicit none
      integer n
      real a(n),c
      integer i
      do i=1,n
         a(i)=c
      enddo
      return
      end


      
      logical function s2pbadkey(keyrec)
      implicit none
      character(80) keyrec
      logical copy
      copy=.true.
      copy=copy.and.(index(keyrec(1:8),'DETCHANS') .le. 0)
      copy=copy.and.(index(keyrec(1:7),'HDUCLAS') .le. 0)
      copy=copy.and.(index(keyrec(1:7),'HDUVERS') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'TELESCOP') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'INSTRUME') .le. 0)
      copy=copy.and.(index(keyrec(1:6),'FILTER') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'EXPOSURE') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'AREASCAL') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'PHAVERSN') .le. 0)
      copy=copy.and.(index(keyrec(1:8),'CHANTYPE') .le. 0)
      s2pbadkey=.not.copy
      return
      end

      subroutine lcmathmake(in_fil,bg_fil,ou_fil,mi,ai,mb,ab,as,docor,
     &                  iopt,err_mode, ierr)

c do a BacKGround subtraction on binned rate data.

c This routine houses the full processing algorithm for program lcmath.

c It calls routines to open the input files, prepare the output file,
c read from input, do additions or subtractions and write the results
c to the output file.  It also performs several checks on the compatability
c of the two input files, viz:

c    The input and output filenames must be different -- input does not
c    get overwritten.

c    If subtracting, the "background file" must completely contain
c    the input file in time.

c    The number of energy channels in input and background must be the same.

c    If subtracting, the integration time in the input file should be
c    less than or equal to the integration time in the bg file, (although
c    nothing in the lower routines would prevent the program from executing
c    correctly if this were not the case.)

c  I  in_fil    (c)  Name of input file
c  I  bg_fil    (c)  Name of background file
c  I  ou_fil    (c)  name of output file
c  I  mi        (i)  Multiplicative factor for input
c  I  ai        (i)  Additive offset for input
c  I  mb        (i)  Multiplicative factor for background
c  I  ab        (i)  Additive offset for background
c  I  as        (l)  if .true. means add rather than subtract
c  I  docor     (l)  if .true. means apply deadtime/vignet corrections
c  I  iopt      (i)  xronos options array (used only for energy bounds here)
c  I  err_mode  (i)  method to evaluate the error
c  O  ierr      (i)  error status

c Author:  eal   February 1994, NASA/Goddard Space Flight Center

      IMPLICIT NONE

c      INCLUDE 'lcmathdef.inc'

      include '../../include/io.inc'
      integer kmax,cmax,maxdim
      parameter (kmax=40,cmax=100,maxdim=9)

      LOGICAL*4 anynul,finished,as,docor
      character*(*) in_fil,bg_fil,ou_fil
      character(20) keywords(kmax)
      integer nrows(2),lui,luo,lub,ivecti(10),ivecto(10),iopt(*),xtend
     &   ,ierr,ftstat,inext(2),bgext(2),emax(2),trow,frow,i
     &   ,LENACT,ichat,ivectb(10),ctunit(2),crunit(2)
     &   ,naxis(2),naxes(maxdim,2),eaxis(2),taxis(2),
     &   isave1,isave4, err_mode
      REAL mi,mb,ai,ab,ynet,synet
      DOUBLE PRECISION dtsta(2),dtsto(2),dtint(2),dtoffset(2)
     &   ,dtp(2),dtzero(2)
      external LENACT
      DATA keywords /kmax*'                    '/
      parameter (subname = 'lcmathmake:')

      if(ierr.ne.0) return

C Initialize time variables.... or else
      dtsta(1)    = 0.0d0
      dtsta(2)    = 0.0d0
      dtsto(1)    = 0.0d0
      dtsto(2)    = 0.0d0
      dtoffset(1) = 0.0d0
      dtoffset(2) = 0.0d0
      dtp(1)      = 0.0d0
      dtp(2)      = 0.0d0
      dtzero(1)   = 0.0d0
      dtzero(2)   = 0.0d0

c Flag for same input/outpt filename.
      do i=1,10
         ivecti(i)=0
         ivectb(i)=0
      enddo
      inext(1)=0
      inext(2)=0
      bgext(1)=0
      bgext(2)=0
 
      lui=0
      luo=0
      lub=0
      ftstat=0
      xtend=0
      if(in_fil.eq.ou_fil) then
         ierr = -1
         errm = 'Outfile must have a different name from infile'
         GOTO 999
      endif

c------------
c Open files.
c------------

c >>> Still need to define this right.<<<

      ichat = 5

c Preserve iopt(1) and iopt(4)

      isave1 = iopt(1)
      isave4 = iopt(4)
c Open and check input file.

      CALL lcmathopen(in_fil,lui,inext,nrows(1),ivecti,iopt,dtp(1),
     &     dtint(1),dtsta(1),dtsto(1),dtzero(1),dtoffset(1),ctunit(1)
     &            ,crunit(1),naxis(1),naxes(1,1),eaxis(1),taxis(1)
     &            ,emax(1),ierr)

      if (ierr.ne.0) then
         errm='Trouble with file: '//in_fil
         goto 999
      endif

c Open and check background file.

      CALL lcmathopen(bg_fil,lub,bgext,nrows(2),ivectb,iopt,dtp(2),
     &     dtint(2),dtsta(2),dtsto(2),dtzero(2),dtoffset(2),ctunit(2)
     &            ,crunit(2),naxis(2),naxes(1,2),eaxis(2),taxis(2)
     &            ,emax(2),ierr)
      
      if (ierr.ne.0) then
         errm='Trouble with file: '//bg_fil
         goto 999
      endif

      iopt(1) = isave1 
      iopt(4) = isave4

c Check input/background compatibility.

c Start/Stop time.
c      
c      write(*,*)'dtsta(1), dtsta(2), dtsto(1), dtsto(2)',
c     &           dtsta(1), dtsta(2), dtsto(1), dtsto(2)
c
c
c  Do not overlap case  sta(1) > sto(2)   or sto(1)< sta(2)
c 
      IF((dtsta(1).gt.dtsto(2)).or.(dtsto(1).lt.dtsta(2)))THEN
         ierr=-1 
         errm= ' Start/stop times in the input files do not overlap'
         goto 999
      ENDIF
c
c Partial overlap case. The partial overlap is no longer checked  
c           sta(1)                     sto(1)
c            ----------------------------
c
c a)    sta(2)      sto(2)
c        ------------
c b)                            sta(2)        sto(2)
c                                 ---------------
c
c c)     sta(2)                              sto(2)
c        ------------------------------------
c
c
c d)             sta(2)          sto(2)
c                  ----------------
c
c Total overlay
c      IF(dtsta(1).ge.dsta(2)     
c
cc
c      IF((.not.as).and.((dtsta(1).lt.dtsta(2)).or.
c     &                  (dtsto(1).gt.dtsto(2))    )) THEN
c         ierr = -1
c         errm = ' Start/stop times in input and background do not agree'
c         goto 999
c      ENDIF
cc
c Number of energy channels.

      IF(emax(1).ne.emax(2)) THEN
         ierr = -1
         errm = ' Energy channels in input and background do not agree'
         goto 999
      ENDIF

c If dt in infile is not less than dt in background file....

      IF((.not.as).and.(dtint(1) .gt. dtint(2))) THEN
         ierr = -1
         errm = ' Integration time longer in input than in background'
         goto 999
      ENDIF

c ------------------------------------------------------------------------
c Open the output file and copy over all extensions except the rate table.
c Make space for the rate table and return its column numbers in ivecto.
c ------------------------------------------------------------------------

      CALL getlun(luo)

      CALL lcmathcopy(lui,as,inext(1),ivecti,luo,ou_fil,ivecto,ierr)
c      write(*,*)'ierr',ierr
c Return to the rate table in all files.
c      write(*,*)'unit',luo,lui,lub,ftstat  
c      write(*,*)'1 inext(1)',inext(1),bgext(1),xtend  
      CALL ftmahd(lui,inext(1),xtend,ftstat)
      CALL ftmahd(lub,bgext(1),xtend,ftstat)
      CALL ftmahd(luo,inext(1),xtend,ftstat)
c      write(*,*)'4 inext(2)',inext(2),bgext(2),xtend,ftstat  
c ---------------
c Execution part.
c ---------------

c Loop over bins.

      finished = .false.
      DO WHILE ((.not.finished).and.(ierr.eq.0))

c Get rate ynet and error synet from input file - background file.

         CALL lcmathread(lui,lub,ichat,mi,ai,mb,ab,as,iopt,ivecti,
     &          ivectb,nrows,dtp,dtint,dtzero,dtoffset,ctunit,crunit
     &               ,naxis,naxes,eaxis,taxis
     &               ,docor,err_mode,frow,trow,ynet,synet,anynul
     &               ,finished, ierr) 
c Write new subtracted data to output file.

         CALL lcmathwrite(lui,luo,frow,trow,anynul,ivecti,ivecto
     &                ,ynet,synet,ierr)

      ENDDO
      GOTO 1000

c ------------- 
c Closing part. 
c -------------

999   continue
      errm = subname//' '//errm
      CALL xaerror(errm,1)

1000  continue

c Close files
 
      ftstat = 0
      CALL ftclos(lui,ftstat)
c
c test for ckecksum
      IF(luo.ne.0) THEN 
        call ftpcks(luo,ftstat) 
        CALL ftclos(luo,ftstat)
      ENDIF
      CALL ftclos(lub,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         errm = subname//' '//'Closing fits files'
         CALL xaerror(errm,1)
      ENDIF

c Free logical units.
c      write(*,*)'lui,luo,lub',lui,luo,lub  
      CALL frelun(lui)
      IF(luo.ne.0)CALL frelun(luo)
      CALL frelun(lub)
 
      return
      end

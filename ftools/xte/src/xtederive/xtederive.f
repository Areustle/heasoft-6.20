*XTEDERIVE
      subroutine xtedee()
      implicit none
C-----------------------------------------------------------------------
C Description: Takes a file written by FCOLLECT, copies it to another
C    file ( .xfl) and appends some scientifically interesting parameters
C    derived from the collected Orbit and HK AppIds.
C
C Arguments: infile - file produced by FCOLLECT
C
C Authors/Modification History:
C              Mike Tripicco (1995 Nov 22), original version
C
C    11Dec95: v1.0 installed in GOF FTOOLS
C
C    11Dec95: v1.1 uses fticols for speed
C
C    19Dec95: v1.2 includes error checking for missing AppIds
C
C    16Jan96: v1.3 new column added -- McIlwain L-parameter
C
C    25Jan96: v1.4 radecbore2 added so that POINT_RA,POINT_DEC
C                  can still be calculated when AppId 24 is missing
C  
C    05Apr96: v1.5 boresight vector updated to (0.9999998,0.0,0.0007)
C
C    27Jun96: v1.6 adding new derived quantity: POINT_L, POINT_B
C                  (uses libsla: sla_eqgal)
C
C    15Aug96: v1.7 another new derived quantity: OFFSET
C                  (angle between nominal and computed pointing)
C
C    16Aug96: v1.8 cleaned up treatment of logicals
C
C    11Sep96: v1.9 added new quantities: BKGD_THETA, BKGD_PHI
C                  for M. Stark (new subroutine in starksubs.f)
C
C    09Oct96: v1.9.1 minor change in coeffs in EarthCoor()
C
C    24Oct96: v2.0 isolating calculations viz missing columns to maximize
C                  return w/real-time data
C
C    13Nov96: v2.1 added PCUn_ON and NUM_PCU_ON quantities (B-type)
C                  also tightened EarthCoor() in starksubs.f to avoid 
C                  ASIN() FPE when argument exceeds 1.0d0
C
C    06Mar97: v2.2 increased maxcols and decreased row buffer size
C    
C    13Mar97: v2.3 error checking added for exceeding maxcol
C
C    31Oct97: v2.4 fticls (now FITSIO intrisic) replaces fticols
C
C    15Dec97: v2.5 optimization apparently allows compound if
C                  statements to evaluate the second clause even
C                  if the preceeding clause is false -- ugly nesting
C                  near line 1274 results...
C
C    05Feb98: v2.6 handles very long infile names now (was overflowing
C                  variable which holds history keyword text)
C
C    29Apr98: v2.7 ELECTRON[0-4], TIME_SINCE_SAA added
C
C    07May98: v2.8 changed value of TIME_SINCE_SAA outside BKGD_THETA
C                  limits to 99.99999 (> 1 orbit) instead of INDEF
C
C    06Jul98: by ZG to change datstr length to 10 so the format will be
C		dd/mm/yyyy instead of dd/mm/yy.
C
C    27Aug01: v2.9 adds new column "L6CNTPCU0" (changes by C.Markwardt) which
C             requires columns: 'X2LX2RCntPcu0', 'X3LX3RCntPcu0', 'X1LX2LCntPcu0'
C             'X1RX2RCntPcu0', 'X2LX3LCntPcu0' and 'X2RX3RCntPcu0'
C
C    05Jan12: v2.10 now checks for (rare) case of scposx/y/z = 0.0
C-----------------------------------------------------------------------
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
C     datatype	        value
C     logical		1
C     integer*2	        3
C     Integer		4
C     Long Integer	5
C     Real		6
C     Double		7
C     Complex		8
      
      integer errstat,fcstln,nrows,ncols
      integer ilun,olun,blksz,hdutype
      integer i,dumint,j,k
      integer colno(7)
      integer day,month,year
      integer nend,nread,icode
C     The following allows an appidlist of up to 300 entries
      integer maxcol
        parameter(maxcol=301)
      integer rowbuf
        parameter(rowbuf=1000)
      integer nullj
        parameter(nullj=2147483647)
      integer pcuon, numpcuon
      integer nullb
        parameter(nullb=255)

C     Change NUMCOLS when adding a new derived quantity
      integer NUMCOLS
        parameter (NUMCOLS=38)

c     integer*2 nulli
c       parameter(nulli=32767)

      real*8 dbuf(maxcol,rowbuf),dcoori(3),dra,ddec,angl
      real*8 pntra(rowbuf),pntdec(rowbuf),sunra(rowbuf),sundec(rowbuf)
      real*8 moonra(rowbuf),moondec(rowbuf),velra(rowbuf),veldec(rowbuf)
      real*8 dpntl,dpntb,draobj,ddecobj,offset
      real*8 DPI
        parameter(DPI=3.141592653589793d0)
      real*8 nulld
        parameter(nulld=-999999999.d0) 

      real*4 estquat(4),sccoord(3,2),attmat(rowbuf,3,3),tmp(3,3)
      real*4 sunx(3),suni(3),borex(3),borei(3),roll,ra,dec,pntl,pntb
      real*4 boreiarr(rowbuf,3)
      real*4 coorx(3),coori(3),rxte(3),bselimb,bktheta,bkphi,altitude
      real*4 rearth,mcilwainl,b0,scpos(3),earthx
        parameter(rearth=6371.2)
      real*4 nulle, lastlat, saatime
        parameter(nulle=-999999999.) 
      real*4 elctrn0, elctrn1, elctrn2, elctrn3, elctrn4
      real*4 l6cntpcu0

c     character(160) infile,outfile,message
c
c     5Feb98 (MJT):
c     infile can be up to 160 chars -> outfile has ".xfl" appended
c     and histstr (was using dumcom) has 18 chars prepended:
c     "File derived from "
c
c     Also no reason why variable "message" needs to be more than
c     72 chars...
c
      character(160) infile
      character(164) outfile
      character(178) histstr
      character(80)  dumcom, msg, message

      character(40)  colnames(maxcol)
      character(40)  taskname
      character(40)  ttype(NUMCOLS),tform(NUMCOLS)
      character(8)   dumkw
C      character(8)   datstr
      character(10) datstr
      character(2)   dumcol
c     character     nullb
c      parameter(nullb=255)
   
      logical last,clobber,anyf,haveattmat,doit,havesun,doit2,dumlog

      common /task/ taskname
      common /latitude/ lastlat

C     Initialization
      taskname = 'XTEDERIVE v2.10'
      errstat = 0
      clobber = .false.
      anyf = .false.
      lastlat = nulle
C     data borex/1.,0.,0./
C
C     Using boresight vector as per Keith Jahoda
C
      data borex/0.9999998,0.0,0.0007/
C      
      call fcecho(' ')
      message='======Running '//taskname(:fcstln(taskname))//'======'
      call fcecho(message)
C
C     Get the parameters from the .par file and form outfile name
C
      call gpxderiv(infile,clobber,errstat)
      if (errstat .ne. 0) then
        message='error reading parameter file'
        call fcerr(message)
        goto 999
      endif
      outfile=infile(:fcstln(infile))//'.xfl'
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
      if (clobber) outfile='!'//outfile(:fcstln(outfile))
C
C     Assign unit numbers for I/O FITS files
C
      call ftgiou(ilun,errstat)
      if (errstat .ne. 0) then
        message='error assigning unit # for input file'
        call fcerr(message)
        goto 999
      endif
      call ftgiou(olun,errstat)
      if (errstat .ne. 0) then
        message='error assigning unit # for output file'
        call fcerr(message)
        goto 999
      endif
C
C     Open FITS files
C
      call ftopen(ilun,infile(:fcstln(infile)),0,blksz,errstat)
      if (errstat .ne. 0) then
        message='error opening input file'
        call fcerr(message)
        goto 999
      endif
      call ffinit(olun,outfile(:fcstln(outfile)),errstat)
      if (errstat .ne. 0) then
        message='error opening output file'
        call fcerr(message)
        goto 999
      endif
C
C     Copy primary array from infile to outfile
C
      call ftcopy(ilun,olun,0,errstat)
      if (errstat .ne. 0) then
        message='error copying primary array'
        call fcerr(message)
        goto 999
      endif
C
C     Grab the nominal pointing info
C
      call ftgkyd(ilun,'RA_OBJ',draobj,dumcom,errstat)
      call ftgkyd(ilun,'DEC_OBJ',ddecobj,dumcom,errstat)
      if (errstat .ne. 0) then
        draobj=nulld
        ddecobj=nulld
      endif
C
C     Move to the bintable extension (= 2) in infile
C
      call ftmahd(ilun,2,hdutype,errstat)
      if (errstat .ne. 0) then
        message='error moving to bintable extension'
        call fcerr(message)
        goto 999
      endif
C
C     Create a new HDU in outfile and copy in bintable extension
C
      call ftcrhd(olun,errstat)
      if (errstat .ne. 0) then
        message='error creating new bintable extension'
        call fcerr(message)
        goto 999
      endif
      call ftcopy(ilun,olun,0,errstat)
      if (errstat .ne. 0) then
        message='error copying bintable'
        call fcerr(message)
        goto 999
      endif
C
C     Get number of rows in table
C
      call ftgkyj(ilun,'NAXIS2',nrows,dumcom,errstat)
      if (errstat .ne. 0) then
        message='error getting NAXIS2'
        call fcerr(message)
        goto 999
      endif
C
C     Get number of columns in table
C
      call ftgkyj(ilun,'TFIELDS',ncols,dumcom,errstat)
      if (errstat .ne. 0) then
        message='error getting TFIELDS'
        call fcerr(message)
        goto 999
      endif
      if (ncols .gt. maxcol) then
        write(message,'(A18,i3,A24,i2,A1)') 'Too many columns (',
     &  ncols,' ) in input file (max = ',maxcol,')' 
        call fcerr(message)
        message='either decrease the number of AppIds collected or'
        call fcerr(message)
        message='contact xtehelp@athena.gsfc.nasa.gov for assistance'
        call fcerr(message)
        goto 999
      endif
C
C     FILL COLNAMES ARRAY
C
      do i=1,ncols
        if (i .lt. 10) then
          write(dumcol,'(I1)') i
        else
          write(dumcol,'(I2)') i
        endif
        call ftgcnn(ilun,.false.,dumcol(:fcstln(dumcol)),
     &                     colnames(i),dumint,errstat)
        if (errstat .ne. 0) then
          message='error getting column names'
          call fcerr(message)
          goto 999
        endif
      enddo
C
C     Set up new columns in output FITS file
C
      call fcecho('Initializing New Columns....')
      data ttype/'X_RA','X_DEC','Y_RA','Y_DEC','Z_RA','Z_DEC',
     &  'POINT_RA','POINT_DEC','POINT_L','POINT_B','ROLLBIAS',
     &  'VEL_RA','VEL_DEC',
     &  'SUN_RA','SUN_DEC','MOON_RA','MOON_DEC','SUN_ANGLE',
     &  'MOON_ANGLE','RAM_ANGLE','ELV','MCILWAIN_L','OFFSET',
     &  'BKGD_THETA','BKGD_PHI',
     &  'PCU0_ON','PCU1_ON','PCU2_ON','PCU3_ON','PCU4_ON','NUM_PCU_ON',
     &  'TIME_SINCE_SAA',
     &  'ELECTRON0','ELECTRON1','ELECTRON2','ELECTRON3','ELECTRON4',
     &  'L6CNTPCU0'/
C     ***** 20 Aug 2001 by CM: added L6CNTPCU0
C
C     The coefficients in the following should add up to NUMCOLS
C
      data tform/25*'E',6*'B',6*'E',1*'E'/
      call fticls(olun,ncols+1,NUMCOLS,ttype,tform,errstat)
      if (errstat .ne. 0) then
        message='error initializing columns'
        call fcerr(message)
        goto 999
      endif
C
C     Now add comments to TTYPE kws and add TUNITs
C
      call ftkeyn('TTYPE',ncols+1,dumkw,errstat)
      dumcom='Spacecraft X-axis Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+1,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing X_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+2,dumkw,errstat)
      dumcom='Spacecraft X-axis Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+2,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing X_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+3,dumkw,errstat)
      dumcom='Spacecraft Y-axis Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+3,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing Y_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+4,dumkw,errstat)
      dumcom='Spacecraft Y-axis Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+4,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing Y_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+5,dumkw,errstat)
      dumcom='Spacecraft Z-axis Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+5,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing Z_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+6,dumkw,errstat)
      dumcom='Spacecraft Z-axis Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+6,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing Z_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+7,dumkw,errstat)
      dumcom='Pointing Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+7,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing POINT_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+8,dumkw,errstat)
      dumcom='Pointing Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+8,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing POINT_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+9,dumkw,errstat)
      dumcom='Pointing Position (GAL_LON)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+9,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing POINT_L column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+10,dumkw,errstat)
      dumcom='Pointing Position (GAL_LAT)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+10,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing POINT_B column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+11,dumkw,errstat)
      dumcom='Roll Bias Angle'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+11,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ROLLBIAS column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+12,dumkw,errstat)
      dumcom='Velocity Direction (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+12,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing VEL_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+13,dumkw,errstat)
      dumcom='Velocity Direction (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+13,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing VEL_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+14,dumkw,errstat)
      dumcom='Sun Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+14,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing SUN_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+15,dumkw,errstat)
      dumcom='Sun Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+15,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing SUN_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+16,dumkw,errstat)
      dumcom='Moon Position (RA)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+16,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing MOON_RA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+17,dumkw,errstat)
      dumcom='Moon Position (DEC)'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+17,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing MOON_DEC column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+18,dumkw,errstat)
      dumcom='Angle between SUN & POINTING'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+18,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing SUN_ANGLE column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+19,dumkw,errstat)
      dumcom='Angle between MOON & POINTING'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+19,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing MOON_ANGLE column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+20,dumkw,errstat)
      dumcom='Angle between VEL & POINTING'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+20,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing RAM_ANGLE column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+21,dumkw,errstat)
      dumcom='Earth Limb Angle'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+21,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELV column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+22,dumkw,errstat)
      dumcom='McIlwain L parameter'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+22,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'Re',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing MCILWAIN_L column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+23,dumkw,errstat)
      dumcom='Pointing offset angle'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+23,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing OFFSET column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+24,dumkw,errstat)
      dumcom='N-going equatorial crossing longitude'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+24,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing BKGD_THETA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+25,dumkw,errstat)
      dumcom='Phase angle of current orbit'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+25,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'deg',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing BKGD_PHI column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+26,dumkw,errstat)
      dumcom='Status of PCU 0'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+26,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing PCU0_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+27,dumkw,errstat)
      dumcom='Status of PCU 1'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+27,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing PCU1_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+28,dumkw,errstat)
      dumcom='Status of PCU 2'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+28,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing PCU2_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+29,dumkw,errstat)
      dumcom='Status of PCU 3'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+29,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing PCU3_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+30,dumkw,errstat)
      dumcom='Status of PCU 4'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+30,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing PCU4_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+31,dumkw,errstat)
      dumcom='Number of PCUs on'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+31,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing NUM_PCU_ON column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+32,dumkw,errstat)
      dumcom='Approximate time since peak SAA'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+32,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'min',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing TIME_SINCE_SAA column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+33,dumkw,errstat)
      dumcom='Electron rate for PCU0'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+33,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELECTRON0 column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+34,dumkw,errstat)
      dumcom='Electron rate for PCU1'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+34,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELECTRON1 column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+35,dumkw,errstat)
      dumcom='Electron rate for PCU2'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+35,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELECTRON2 column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+36,dumkw,errstat)
      dumcom='Electron rate for PCU3'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+36,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELECTRON3 column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+37,dumkw,errstat)
      dumcom='Electron rate for PCU4'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+37,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,' ',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing ELECTRON4 column'
        call fcerr(message)
        goto 999
      endif
C
      call ftkeyn('TTYPE',ncols+38,dumkw,errstat)
      dumcom='L6 Counts in PCU0'
      call ftmcom(olun,dumkw,dumcom,errstat)
      call ftkeyn('TUNIT',ncols+38,dumkw,errstat)
      dumcom='physical unit of field'
      call ftpkys(olun,dumkw,'count',dumcom,errstat)
      if (errstat .ne. 0) then
        message='error initializing L6CNTPCU0 column'
        call fcerr(message)
        goto 999
      endif

C
C     Fill Data Buffer
C
      nread=0
      last=.false.
      haveattmat=.false.
C
C     EVERYTHING FOLLOWING THIS IS REPEATED FOR EACH DATA BUFFER
C
  111 doit = .true.
      if (rowbuf .lt. nrows-nread) then
        nend=rowbuf
      else
        nend=nrows-nread
        last=.true.
      endif
      write(message,'(A20,i8,A9,i8)') 'Processing rows',
     &  nread+1,' through ',nread+nend 
      call fcecho(message)
      do i=1,nend
        do j=1,ncols
          call ftgcvd(ilun,j,i+nread,1,1,nulld,dbuf(j,i),anyf,errstat)
          if (errstat .ne. 0) then
            message='error filling data buffer'
            call fcerr(message)
            goto 999
          endif
        enddo 
      enddo 
C
C     Construct (rowbuf)x3x3 attitude matrix from estimated quaternions
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols 
        if (colnames(i) .eq. 'ACSESTQ1') colno(1)=i 
        if (colnames(i) .eq. 'ACSESTQ2') colno(2)=i 
        if (colnames(i) .eq. 'ACSESTQ3') colno(3)=i 
        if (colnames(i) .eq. 'ACSESTQ4') colno(4)=i 
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0 .or. colno(4) .eq. 0) then
        msg='Missing Quaternion(s)--Cannot compute Attitude Matrix'
        call fcecho(msg)
      else
        do i=1,nend
          estquat(1)=dbuf(colno(1),i)
          estquat(2)=dbuf(colno(2),i)
          estquat(3)=dbuf(colno(3),i)
          estquat(4)=dbuf(colno(4),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &        dbuf(colno(3),i) .eq. nulld .or. 
     &        dbuf(colno(4),i) .eq. nulld) then
            attmat(i,1,1)=nulle
c           write(message,'(A18,I6)') 
c                 'found NULL in row ',i+nread
c           call fcecho(message)
          else
            call xteamat(estquat,tmp)
            do j=1,3
              do k=1,3
                attmat(i,j,k)=tmp(j,k)
              enddo
            enddo
          endif
        enddo
        haveattmat = .true.
      endif
C
C     Use attitude matrix to compute ra/dec for sc x,y,z axes (6.2)
C
C     Compute the SC X,Y,Z RA/DEC using xtepnt_rd subroutine,
C        ie, fill sccoord (3,2) array
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (haveattmat) then
          do j=1,3
            do k=1,3
              tmp(j,k)=attmat(i,j,k)
            enddo
          enddo
          if (attmat(i,1,1) .ne. nulle) then
            call xtepnt_rd(tmp,sccoord)
          else
c           write(message,'(A34,I6)') 
c    &          'found NULL in ATTITUDE MATRIX row ',i+nread
c           call fcecho(message)
            do j=1,3
              do k=1,2
                sccoord(j,k)=nulle
              enddo
            enddo
          endif
        else
          do j=1,3
            do k=1,2
              sccoord(j,k)=nulle
            enddo
          enddo
        endif
C
C     Write the data into the FITS file
C
c       write (6,*) 'writing col:',ncols+1,' row:',i+nread,
c    &              ' value:',sccoord(1,1)
        call ftpcne(olun,ncols+1,i+nread,1,1,sccoord(1,1),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived X_RA'
          call fcerr(message)
          goto 999
        endif
c       write (6,*) 'writing col:',ncols+2,' row:',i+nread,
c    &              ' value:',sccoord(1,2)
        call ftpcne(olun,ncols+2,i+nread,1,1,sccoord(1,2),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived X_DEC'
          call fcerr(message)
          goto 999
        endif
c       write (6,*) 'writing col:',ncols+3,' row:',i+nread,
c    &              ' value:',sccoord(2,1)
        call ftpcne(olun,ncols+3,i+nread,1,1,sccoord(2,1),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived Y_RA'
          call fcerr(message)
          goto 999
        endif
c       write (6,*) 'writing col:',ncols+4,' row:',i+nread,
c    &              ' value:',sccoord(2,2)
        call ftpcne(olun,ncols+4,i+nread,1,1,sccoord(2,2),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived Y_DEC'
          call fcerr(message)
          goto 999
        endif
c       write (6,*) 'writing col:',ncols+5,' row:',i+nread,
c    &              ' value:',sccoord(3,1)
        call ftpcne(olun,ncols+5,i+nread,1,1,sccoord(3,1),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived Z_RA'
          call fcerr(message)
          goto 999
        endif
c       write (6,*) 'writing col:',ncols+6,' row:',i+nread,
c    &              ' value:',sccoord(3,2)
        call ftpcne(olun,ncols+6,i+nread,1,1,sccoord(3,2),nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing derived Z_DEC'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C
C     Compute pointing position (RA/DEC: 6.1) & Roll Bias Angle (6.4)
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSESTQ1') colno(1)=i
        if (colnames(i) .eq. 'ACSESTQ2') colno(2)=i
        if (colnames(i) .eq. 'ACSESTQ3') colno(3)=i
        if (colnames(i) .eq. 'ACSESTQ4') colno(4)=i
        if (colnames(i) .eq. 'ACSSUNX')  colno(5)=i
        if (colnames(i) .eq. 'ACSSUNY')  colno(6)=i
        if (colnames(i) .eq. 'ACSSUNZ')  colno(7)=i
      enddo
c      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
c     &    colno(3) .eq. 0 .or. colno(4) .eq. 0 .or.
c     &    colno(5) .eq. 0 .or. colno(6) .eq. 0 .or.
c     &    colno(7) .eq. 0) then
c        call fcecho('Column(s) missing--No POINT_RA, DEC or ROLLBIAS')
c        doit = .false.
c      endif
      havesun = .true.
      if (colno(5) .eq. 0 .or. colno(6) .eq. 0 .or.
     &    colno(7) .eq. 0) then
        msg='ACSSUNX,Y,Z column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ROLLBIAS'
        call fcecho(msg)
        havesun = .false.
      endif
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
c        if (doit) then
        if (haveattmat) then
          do j=1,3 
            do k=1,3
              tmp(j,k)=attmat(i,j,k)
            enddo
          enddo
          estquat(1)=dbuf(colno(1),i)
          estquat(2)=dbuf(colno(2),i)
          estquat(3)=dbuf(colno(3),i)
          estquat(4)=dbuf(colno(4),i)
          if (havesun) then
            sunx(1)=dbuf(colno(5),i)
            sunx(2)=dbuf(colno(6),i)
            sunx(3)=dbuf(colno(7),i)
          endif
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        dbuf(colno(4),i) .eq. nulld .or.
     &        attmat(i,1,1) .eq. nulle .or. (.not. havesun)) then
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
            if (haveattmat .and. (tmp(1,1) .ne. nulle)) then
              call radecbore2(tmp,borex,ra,dec,borei)
              roll=nulle
              do j=1,3
                boreiarr(i,j)=borei(j)
              enddo
            else
              ra=nulle
              dec=nulle
              roll=nulle
c mjt 03May99: ELV will now be INDEF instead of fictional...
              do j=1,3
                 boreiarr(i,j)=nulle
              enddo
c             write(message,'(A18,I6)') 'found NULL in row ',i+nread
c             call fcecho(message)
            endif
          else
C
C     Use xtetoirf to transform SUN from sc to inertial coords
C     *then* use radecbore to transform the boresight vector
C     from sc to inertial coords and also compute the roll bias
C
            call xtetoirf(tmp,sunx,suni,-1)
            call radecbore(estquat,borex,suni,ra,dec,borei,roll)
            do j=1,3
              boreiarr(i,j)=borei(j)
            enddo
          endif
        else
          ra=nulle
          dec=nulle
          do j=1,3
            borei(j)=nulle
            boreiarr(i,j)=borei(j)
          enddo
          roll=nulle
        endif
C
C     Compute offset angle
C
        if ((ra .ne. nulle) .and. (dec .ne. nulle) .and.
     &       (draobj .ne. nulld) .and. (ddecobj .ne. nulld)) then
          call angsep(ra*1.d0,dec*1.d0,draobj,ddecobj,offset)
        else
          offset=nulld
        endif
c     write(message,'(A16,D20.9)') 'computed offset ',offset
c     call fcecho(message)
C
C     Write POINT{_RA,_DEC,_L,_B} and ROLLBIAS column data
C
        pntra(i)=ra*1.d0
        pntdec(i)=dec*1.d0
        if (ra .eq. nulle .or. dec .eq. nulle) then
          pntl=nulle
          pntb=nulle
        else
          call sla_eqgal(pntra(i)*DPI/180.d0,pntdec(i)*DPI/180.d0,
     &                 dpntl,dpntb)
          pntl=dpntl*180.d0/DPI
          pntb=dpntb*180.d0/DPI
        endif
C
        call ftpcne(olun,ncols+7,i+nread,1,1,ra,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing POINT_RA'
          call fcerr(message)
          goto 999
        endif
        call ftpcne(olun,ncols+8,i+nread,1,1,dec,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing POINT_DEC'
          call fcerr(message)
          goto 999
        endif
        call ftpcne(olun,ncols+9,i+nread,1,1,pntl,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing POINT_L'
          call fcerr(message)
          goto 999
        endif
        call ftpcne(olun,ncols+10,i+nread,1,1,pntb,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing POINT_B'
          call fcerr(message)
          goto 999
        endif
        call ftpcne(olun,ncols+11,i+nread,1,1,roll,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ROLLBIAS'
          call fcerr(message)
          goto 999
        endif
        call ftpcnd(olun,ncols+23,i+nread,1,1,offset,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing OFFSET'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute Velocity Direction (RA/DEC: 6.3)
C
C     First, call xtetoirf to transform vel.x,y,z (xte --> inertial) 
C      then, use xyz_radec to go from cartesian --> ra/dec
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSSCVELX') colno(1)=i
        if (colnames(i) .eq. 'ACSSCVELY') colno(2)=i
        if (colnames(i) .eq. 'ACSSCVELZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0) then
        msg='ACSSCVELX,Y,Z column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute VEL_RA, DEC or RAM_ANGLE'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (doit .and. haveattmat) then
          do j=1,3
            do k=1,3
              tmp(j,k)=attmat(i,j,k)
            enddo
          enddo
          coorx(1)=dbuf(colno(1),i)
          coorx(2)=dbuf(colno(2),i)
          coorx(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        tmp(1,1) .eq. nulle) then
            dra=nulld
            ddec=nulld
c           write(message,'(A18,I6)') 'VEL: found NULL in row ',i+nread
c           call fcecho(message)
          else
            call xtetoirf(tmp,coorx,coori,-1)
            do j=1,3
              dcoori(j)=coori(j)*1.d0
            enddo
            call xyz_radec(dcoori(1),dcoori(2),dcoori(3),dra,ddec)
          endif
        else
          dra=nulld
          ddec=nulld
        endif
C
C     Write VEL_RA,DEC 
C
        velra(i)=dra
        veldec(i)=ddec
C       write (6,*) 'writing dra=',dra,', ddec=',ddec
        call ftpcnd(olun,ncols+12,i+nread,1,1,dra,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing VEL_RA'
          call fcerr(message)
          goto 999
        endif
        call ftpcnd(olun,ncols+13,i+nread,1,1,ddec,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing VEL_DEC'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute Sun Position (RA/DEC: 6.5)
C
C     First, call xtetoirf to transform sun.x,y,z (xte --> inertial) 
C      then, use xyz_radec to go from cartesian --> ra/dec
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSSUNX') colno(1)=i
        if (colnames(i) .eq. 'ACSSUNY') colno(2)=i
        if (colnames(i) .eq. 'ACSSUNZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0) then
        msg='ACSSUNX,Y,Z column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute SUN_RA, DEC'
        call fcecho(msg)
        call fcecho('-->Will try to use ACSSUNX to get SUN_ANGLE')
        doit = .false.
      endif
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (doit .and. haveattmat) then
          do j=1,3
            do k=1,3
              tmp(j,k)=attmat(i,j,k)
            enddo
          enddo
          coorx(1)=dbuf(colno(1),i)
          coorx(2)=dbuf(colno(2),i)
          coorx(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        tmp(1,1) .eq. nulle) then
            dra=nulld
            ddec=nulld
c           write(message,'(A18,I6)') 'Sun: found NULL in row ',i+nread
c           call fcecho(message)
          else
            call xtetoirf(tmp,coorx,coori,-1)
            do j=1,3
              dcoori(j)=coori(j)*1.d0
            enddo
            call xyz_radec(dcoori(1),dcoori(2),dcoori(3),dra,ddec)
          endif
        else
          dra=nulld 
          ddec=nulld 
        endif
C
C     Write SUN_RA,DEC
C
        sunra(i)=dra
        sundec(i)=ddec
C       write (6,*) 'writing dra=',dra,', ddec=',ddec
        call ftpcnd(olun,ncols+14,i+nread,1,1,dra,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing SUN_RA'
          call fcerr(message)
          goto 999
        endif
        call ftpcnd(olun,ncols+15,i+nread,1,1,ddec,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing SUN_DEC'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute Moon Position (RA/DEC: 6.7)
C
C     First, call xtetoirf to transform sun.x,y,z (xte --> inertial) 
C      then, use xyz_radec to go from cartesian --> ra/dec
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSLUNARX') colno(1)=i
        if (colnames(i) .eq. 'ACSLUNARY') colno(2)=i
        if (colnames(i) .eq. 'ACSLUNARZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0) then
        msg='ACSLUNARX,Y,Z column(s) missing--'
        msg=msg(:fcstln(msg))//
     &      'Cannot compute MOON_RA, DEC'
        call fcecho(msg)
        call fcecho('-->Will try to use ACSLUNARX to get MOON_ANGLE')
        doit = .false.
      endif
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (doit .and. haveattmat) then
          do j=1,3
            do k=1,3
              tmp(j,k)=attmat(i,j,k)
            enddo
          enddo
          coorx(1)=dbuf(colno(1),i)
          coorx(2)=dbuf(colno(2),i)
          coorx(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        tmp(1,1) .eq. nulle) then
            dra=nulld
            ddec=nulld
c           write(message,'(A18,I6)') 'Moon: found NULL in row ',i+nread
c           call fcecho(message)
          else
            call xtetoirf(tmp,coorx,coori,-1)
            do j=1,3
              dcoori(j)=coori(j)*1.d0
            enddo
            call xyz_radec(dcoori(1),dcoori(2),dcoori(3),dra,ddec)
          endif
        else
          dra=nulld
          ddec=nulld
        endif
C
C     Write MOON_RA,DEC
C
        moonra(i)=dra
        moondec(i)=ddec
C       write (6,*) 'writing dra=',dra,', ddec=',ddec
        call ftpcnd(olun,ncols+16,i+nread,1,1,dra,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing MOON_RA'
          call fcerr(message)
          goto 999
        endif
        call ftpcnd(olun,ncols+17,i+nread,1,1,ddec,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing MOON_DEC'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit=.true.
C
C     Compute Sun Angle (6.6) from Point. Pos (RA/DEC), Sun Pos (RA/DEC)
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSSUNX') colno(1)=i
      enddo
      doit2=.true.
      if (colno(1) .eq. 0) then
        msg='ACSSUNX column missing--'
        msg=msg(:fcstln(msg))//'Cannot compute SUN_ANGLE'
        call fcecho(msg)
        doit2=.false.
      endif
      do i=1,nend
        if (sunra(i) .ne. nulld .and. sundec(i) .ne. nulld .and. 
     &      pntra(i) .ne. nulld .and. pntdec(i) .ne. nulld) then
          call angsep(pntra(i),pntdec(i),sunra(i),sundec(i),angl)
        else
          if (doit2 .and. (dbuf(colno(1),i) .ne. nulld)) then
            angl = (180.d0/DPI) * acos(dbuf(colno(1),i))
          else
            angl = nulld
          endif
        endif
        call ftpcnd(olun,ncols+18,i+nread,1,1,angl,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing SUN ANGLE'
          call fcerr(message)
          goto 999
        endif
      enddo
C
C     Compute Moon Angle (6.8) from Point. Pos (RA/DEC), Moon Pos (RA/DEC)
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSLUNARX') colno(1)=i
      enddo
      doit2=.true.
      if (colno(1) .eq. 0) then
        msg='ACSLUNARX column missing--'
        msg=msg(:fcstln(msg))//'Cannot compute MOON_ANGLE'
        call fcecho(msg)
        doit2=.false.
      endif
      do i=1,nend
        if (moonra(i) .ne. nulld .and. moondec(i) .ne. nulld .and. 
     &      pntra(i) .ne. nulld .and. pntdec(i) .ne. nulld) then
          call angsep(pntra(i),pntdec(i),moonra(i),moondec(i),angl)
        else
          if (doit2) then
            if (dbuf(colno(1),i) .ne. nulld) then
              angl = (180.d0/DPI) * acos(dbuf(colno(1),i))
            else
              angl = nulld
            endif
          else
            angl = nulld
          endif
        endif
        call ftpcnd(olun,ncols+19,i+nread,1,1,angl,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing MOON ANGLE'
          call fcerr(message)
          goto 999
        endif
      enddo
C
C     Compute Ram Angle (6.10) from Point. Pos (RA/DEC), Vel Pos (RA/DEC)
C
      do i=1,nend
        if (velra(i) .ne. nulld .and. veldec(i) .ne. nulld .and. 
     &      pntra(i) .ne. nulld .and. pntdec(i) .ne. nulld) then
          call angsep(pntra(i),pntdec(i),velra(i),veldec(i),angl)
        else
          angl = nulld
        endif
        call ftpcnd(olun,ncols+20,i+nread,1,1,angl,nulld,errstat)
        if (errstat .ne. 0) then
          message='error writing RAM ANGLE'
          call fcerr(message)
          goto 999
        endif
      enddo
C
C     Compute Earth Limb Angle (6.9)
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSSCPOSX') colno(1)=i
        if (colnames(i) .eq. 'ACSSCPOSY') colno(2)=i
        if (colnames(i) .eq. 'ACSSCPOSZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0) then
        msg='ACSSCPOSX,Y,Z column(s) missing'
        call fcecho(msg)
        msg='-->Trying alternate calculation for ELV using'
        msg=msg(:fcstln(msg))//' ACSEARTHX and ACSALTITUDE'
        call fcecho(msg)
        doit = .false.
        doit2 = .true.
        do i=1,7
          colno(i)=0
        enddo
        do i=1,ncols
          if (colnames(i) .eq. 'ACSEARTHX') colno(1)=i
          if (colnames(i) .eq. 'ACSALTITUDE') colno(2)=i
        enddo
        if (colno(1) .eq. 0) then
          msg='-->ACSEARTHX column missing--Cannot compute ELV'
          call fcecho(msg)
          doit2=.false.
        endif
        if (colno(2) .ne. 0) then
          altitude=dbuf(colno(2),i)
        else
          altitude=nulle
          if (doit2) then
            call fcecho('-->No ACSALTITUDE--assuming mean value')
          endif
        endif
      endif
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          do j=1,3
            borei(j)=boreiarr(i,j)
          enddo
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        borei(1) .eq. nulle) then
            bselimb=nulle
          else
            call bsearth(rxte,borei,bselimb)
          endif
        else if (doit2) then
          if (dbuf(colno(1),i) .ne. nulld) then
            earthx=dbuf(colno(1),i)
            call elv(earthx,altitude,bselimb)
          else
            bselimb=nulle
          endif
        else 
          bselimb=nulle
        endif
C
C     Write ELV
C
        call ftpcne(olun,ncols+21,i+nread,1,1,bselimb,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELV'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute McIlwain L-parameter (6.11)
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSSCPOSX') colno(1)=i
        if (colnames(i) .eq. 'ACSSCPOSY') colno(2)=i
        if (colnames(i) .eq. 'ACSSCPOSZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0) then
        msg='ACSSCPOSX,Y,Z column(s) missing--Cannot compute MCILWAIN_L'
        call fcecho(msg)
        doit = .false.
      endif
C
C     Initialize Geomagnetic Field Model
C
      call initmagmod
      mcilwainl=0.0
      icode=0
      b0=0.0
C
      do i=1,nend
C
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(1),i) .eq. 0.0d0 .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. 0.0d0 .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. 0.0d0) then
            mcilwainl=nulle
          else
            do j=1,3
              scpos(j)=rxte(j)/rearth
            enddo
            call xteshellc(scpos,mcilwainl,icode,b0)
          endif
        else
          mcilwainl=nulle
        endif
        if (icode .ne. 1) mcilwainl=nulle
c       write(6,*) 'mcilwainl:',mcilwainl,' icode:',icode,' b0:',b0
C
C     Write MCILWAIN_L
C
        call ftpcne(olun,ncols+22,i+nread,1,1,mcilwainl,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing MCILWAIN_L'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute BKGD_THETA, BKGD_PHI, TIME_SINCE_SAA
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'ACSEARTHLAT') colno(1)=i
        if (colnames(i) .eq. 'ACSEARTHLON') colno(2)=i
        if (colnames(i) .eq. 'ACSSCVELZ') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0) then
         msg='ACSEARTHLAT,LON column(s) missing--'
         msg=msg(:fcstln(msg))//'Cannot compute BKGD_THETA/PHI'
         msg=msg(:fcstln(msg))//' or TIME_SINCE_SAA'
         call fcecho(msg)
         doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          if (colno(3) .ne. 0) then
            rxte(3)=dbuf(colno(3),i)
          else
            rxte(3)=nulle
          endif
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld) then
            bktheta=nulle
            bkphi=nulle
            lastlat=nulle
            saatime=nulle
          else
            call EarthCoor(rxte(1), rxte(2), rxte(3), bktheta, bkphi)
            lastlat=rxte(1)
            if (bktheta .gt. -70 .and. bktheta .lt. 150) then
               saatime=((0.272 * bkphi) + (0.214 * bktheta) + 15.7)
            else
               saatime=99.99999
            endif
          endif
        else
          bktheta=nulle
          bkphi=nulle
          saatime=nulle
        endif
C
C     Write BKGD_THETA/PHI, TIME_SINCE_SAA
C
        call ftpcne(olun,ncols+24,i+nread,1,1,bktheta,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing BKGD_THETA'
          call fcerr(message)
          goto 999
        endif
C
        call ftpcne(olun,ncols+25,i+nread,1,1,bkphi,nulle,errstat)
        if (errstat .ne. 0) then
           message='error writing BKGD_PHI'
           call fcerr(message)
           goto 999
        endif
C
        call ftpcne(olun,ncols+32,i+nread,1,1,saatime,nulle,errstat)
        if (errstat .ne. 0) then
           message='error writing TIME_SINCE_SAA'
           call fcerr(message)
           goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute PCU0_ON
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'cmdhvXE_PCU0') colno(1)=i
        if (colnames(i) .eq. 'hvXEon_PCU0') colno(2)=i
        if (colnames(i) .eq. 'hvXErly_PCU0') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 
     &    .or. colno(3) .eq. 0) then
        msg='PCU0 hvXE column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute PCU0_ON'
        call fcecho(msg)
        doit = .false.
      endif
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+26,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+26,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
      do i=1,nend
C
        if (doit) then
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &         dbuf(colno(3),i) .eq. nulld) then
            pcuon=nullb
          else
            if (dbuf(colno(1),i) .ne. 0 .and. dbuf(colno(2),i) .ne. 0
     &          .and. dbuf(colno(3),i) .ne. 0) then
              pcuon = 1
            else
              pcuon = 0
            endif
          endif
        else
          pcuon=nullb
        endif
C
C     Write PCU0_ON
C
        call ftpcnj(olun,ncols+26,i+nread,1,1,pcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing PCU0_ON'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute PCU1_ON
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'cmdhvXE_PCU1') colno(1)=i
        if (colnames(i) .eq. 'hvXEon_PCU1') colno(2)=i
        if (colnames(i) .eq. 'hvXErly_PCU1') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 
     &    .or. colno(3) .eq. 0) then
        msg='PCU1 hvXE column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute PCU1_ON'
        call fcecho(msg)
        doit = .false.
      endif
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+27,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+27,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
      do i=1,nend
C
        if (doit) then
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &         dbuf(colno(3),i) .eq. nulld) then
            pcuon=nullb
          else
            if (dbuf(colno(1),i) .ne. 0 .and. dbuf(colno(2),i) .ne. 0
     &          .and. dbuf(colno(3),i) .ne. 0) then
              pcuon = 1
            else
              pcuon = 0
            endif
          endif
        else
          pcuon=nullb
        endif
C
C     Write PCU1_ON
C
        call ftpcnj(olun,ncols+27,i+nread,1,1,pcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing PCU1_ON'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute PCU2_ON
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'cmdhvXE_PCU2') colno(1)=i
        if (colnames(i) .eq. 'hvXEon_PCU2') colno(2)=i
        if (colnames(i) .eq. 'hvXErly_PCU2') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 
     &    .or. colno(3) .eq. 0) then
        msg='PCU2 hvXE column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute PCU2_ON'
        call fcecho(msg)
        doit = .false.
      endif
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+28,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+28,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
      do i=1,nend
C
        if (doit) then
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &         dbuf(colno(3),i) .eq. nulld) then
            pcuon=nullb
          else
            if (dbuf(colno(1),i) .ne. 0 .and. dbuf(colno(2),i) .ne. 0
     &          .and. dbuf(colno(3),i) .ne. 0) then
              pcuon = 1
            else
              pcuon = 0
            endif
          endif
        else
          pcuon=nullb
        endif
C
C     Write PCU2_ON
C
        call ftpcnj(olun,ncols+28,i+nread,1,1,pcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing PCU2_ON'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute PCU3_ON
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'cmdhvXE_PCU3') colno(1)=i
        if (colnames(i) .eq. 'hvXEon_PCU3') colno(2)=i
        if (colnames(i) .eq. 'hvXErly_PCU3') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 
     &    .or. colno(3) .eq. 0) then
        msg='PCU3 hvXE column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute PCU3_ON'
        call fcecho(msg)
        doit = .false.
      endif
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+29,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+29,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
      do i=1,nend
C
        if (doit) then
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &         dbuf(colno(3),i) .eq. nulld) then
            pcuon=nullb
          else
            if (dbuf(colno(1),i) .ne. 0 .and. dbuf(colno(2),i) .ne. 0
     &          .and. dbuf(colno(3),i) .ne. 0) then
              pcuon = 1
            else
              pcuon = 0
            endif
          endif
        else
          pcuon=nullb
        endif
C
C     Write PCU3_ON
C
        call ftpcnj(olun,ncols+29,i+nread,1,1,pcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing PCU3_ON'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C     Compute PCU4_ON
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'cmdhvXE_PCU4') colno(1)=i
        if (colnames(i) .eq. 'hvXEon_PCU4') colno(2)=i
        if (colnames(i) .eq. 'hvXErly_PCU4') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 
     &    .or. colno(3) .eq. 0) then
        msg='PCU4 hvXE column(s) missing--'
        msg=msg(:fcstln(msg))//'Cannot compute PCU4_ON'
        call fcecho(msg)
        doit = .false.
      endif
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+30,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+30,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
      do i=1,nend
C
        if (doit) then
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or. 
     &         dbuf(colno(3),i) .eq. nulld) then
            pcuon=nullb
          else
            if (dbuf(colno(1),i) .ne. 0 .and. dbuf(colno(2),i) .ne. 0
     &          .and. dbuf(colno(3),i) .ne. 0) then
              pcuon = 1
            else
              pcuon = 0
            endif
          endif
        else
          pcuon=nullb
        endif
C
C     Write PCU4_ON
C
        call ftpcnj(olun,ncols+30,i+nread,1,1,pcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing PCU4_ON'
          call fcerr(message)
          goto 999
        endif
C
      enddo
      doit = .true.
C
C    define null value and write TNULL keyword
C
      call fttnul(olun,ncols+31,nullb,errstat)
      if (errstat .ne. 0) then
        message='error defining NULL value'
        call fcerr(message)
        goto 999
      endif
      call ftkeyn('TNULL',ncols+31,dumkw,errstat)
      if (errstat .ne. 0) then
        message='error forming TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
      call ftpkyj(olun,dumkw,nullb,
     &            'NULL value for B-type columns',errstat)
      if (errstat .ne. 0) then
        message='error writing TNULLn keyword'
        call fcerr(message)
        goto 999
      endif
C
C     Compute NUM_PCU_ON
C
      do i=1,nend
        numpcuon=0
        call ftgcvj(olun, ncols+26, i+nread, 1, 1,
     &              nullb, dumint, dumlog, errstat)
        if (dumlog) then
          numpcuon=nullb
        elseif (numpcuon .ne. nullb) then
          numpcuon=numpcuon+dumint
        endif
        call ftgcvj(olun, ncols+27, i+nread, 1, 1,
     &              nullb, dumint, dumlog, errstat)
        if (dumlog) then
          numpcuon=nullb
        elseif (numpcuon .ne. nullb) then
          numpcuon=numpcuon+dumint
        endif
        call ftgcvj(olun, ncols+28, i+nread, 1, 1,
     &              nullb, dumint, dumlog, errstat)
        if (dumlog) then
          numpcuon=nullb
        elseif (numpcuon .ne. nullb) then
          numpcuon=numpcuon+dumint
        endif
        call ftgcvj(olun, ncols+29, i+nread, 1, 1,
     &              nullb, dumint, dumlog, errstat)
        if (dumlog) then
          numpcuon=nullb
        elseif (numpcuon .ne. nullb) then
          numpcuon=numpcuon+dumint
        endif
        call ftgcvj(olun, ncols+30, i+nread, 1, 1,
     &              nullb, dumint, dumlog, errstat)
        if (dumlog) then
          numpcuon=nullb
        elseif (numpcuon .ne. nullb) then
          numpcuon=numpcuon+dumint
        endif
C
C     Write NUM_PCU_ON
C
        call ftpcnj(olun,ncols+31,i+nread,1,1,numpcuon,nullb,errstat)
        if (errstat .ne. 0) then
          message='error writing NUM_PCU_ON'
          call fcerr(message)
          goto 999
        endif     
C
      enddo
C
      doit = .true.
C
C     Compute ELECTRON0
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'VpX1LCntPcu0') colno(1)=i
        if (colnames(i) .eq. 'VpX1RCntPcu0') colno(2)=i
        if (colnames(i) .eq. 'Q6VxVpXeCntPcu0') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0
     &     .or. colno(3) .eq. 0) then
        msg='necessary column(s) from Std2 file missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ELECTRON0'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        rxte(3) .eq. 0) then
            elctrn0=nulle
          else
             elctrn0=(rxte(1)+rxte(2))/rxte(3)
          endif
        else
          elctrn0=nulle
        endif
C
C     Write ELECTRON0
C
        call ftpcne(olun,ncols+33,i+nread,1,1,elctrn0,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELECTRON0'
          call fcerr(message)
          goto 999
        endif
C
      enddo
c
      doit = .true.
C
C     Compute ELECTRON1
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'VpX1LCntPcu1') colno(1)=i
        if (colnames(i) .eq. 'VpX1RCntPcu1') colno(2)=i
        if (colnames(i) .eq. 'Q6VxVpXeCntPcu1') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0
     &     .or. colno(3) .eq. 0) then
        msg='necessary column(s) from Std2 file missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ELECTRON1'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        rxte(3) .eq. 0) then
            elctrn1=nulle
          else
             elctrn1=(rxte(1)+rxte(2))/rxte(3)
          endif
        else
          elctrn1=nulle
        endif
C
C     Write ELECTRON1
C
        call ftpcne(olun,ncols+34,i+nread,1,1,elctrn1,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELECTRON1'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C
      doit = .true.
C
C     Compute ELECTRON2
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'VpX1LCntPcu2') colno(1)=i
        if (colnames(i) .eq. 'VpX1RCntPcu2') colno(2)=i
        if (colnames(i) .eq. 'Q6VxVpXeCntPcu2') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0
     &     .or. colno(3) .eq. 0) then
        msg='necessary column(s) from Std2 file missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ELECTRON2'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        rxte(3) .eq. 0) then
            elctrn2=nulle
          else
             elctrn2=(rxte(1)+rxte(2))/rxte(3)
          endif
        else
          elctrn2=nulle
        endif
C
C     Write ELECTRON2
C
        call ftpcne(olun,ncols+35,i+nread,1,1,elctrn2,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELECTRON2'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C
      doit = .true.
C
C     Compute ELECTRON3
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'VpX1LCntPcu3') colno(1)=i
        if (colnames(i) .eq. 'VpX1RCntPcu3') colno(2)=i
        if (colnames(i) .eq. 'Q6VxVpXeCntPcu3') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0
     &     .or. colno(3) .eq. 0) then
        msg='necessary column(s) from Std2 file missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ELECTRON3'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        rxte(3) .eq. 0) then
            elctrn3=nulle
          else
             elctrn3=(rxte(1)+rxte(2))/rxte(3)
          endif
        else
          elctrn3=nulle
        endif
C
C     Write ELECTRON3
C
        call ftpcne(olun,ncols+36,i+nread,1,1,elctrn3,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELECTRON3'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C
      doit = .true.
C
C     Compute ELECTRON4
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'VpX1LCntPcu4') colno(1)=i
        if (colnames(i) .eq. 'VpX1RCntPcu4') colno(2)=i
        if (colnames(i) .eq. 'Q6VxVpXeCntPcu4') colno(3)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0
     &     .or. colno(3) .eq. 0) then
        msg='necessary column(s) from Std2 file missing--'
        msg=msg(:fcstln(msg))//'Cannot compute ELECTRON4'
        call fcecho(msg)
        doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
          rxte(1)=dbuf(colno(1),i)
          rxte(2)=dbuf(colno(2),i)
          rxte(3)=dbuf(colno(3),i)
          if (dbuf(colno(1),i) .eq. nulld .or.
     &        dbuf(colno(2),i) .eq. nulld .or.
     &        dbuf(colno(3),i) .eq. nulld .or.
     &        rxte(3) .eq. 0) then
            elctrn4=nulle
          else
             elctrn4=(rxte(1)+rxte(2))/rxte(3)
          endif
        else
          elctrn4=nulle
        endif
C
C     Write ELECTRON4
C
        call ftpcne(olun,ncols+37,i+nread,1,1,elctrn4,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing ELECTRON4'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C
      doit = .true.
C
C     Compute L6CNTPCU0
C
      do i=1,7
        colno(i)=0
      enddo
      do i=1,ncols
        if (colnames(i) .eq. 'X2LX2RCntPcu0') colno(1)=i
        if (colnames(i) .eq. 'X3LX3RCntPcu0') colno(2)=i
        if (colnames(i) .eq. 'X1LX2LCntPcu0') colno(3)=i
        if (colnames(i) .eq. 'X1RX2RCntPcu0') colno(4)=i
        if (colnames(i) .eq. 'X2LX3LCntPcu0') colno(5)=i
        if (colnames(i) .eq. 'X2RX3RCntPcu0') colno(6)=i
      enddo
      if (colno(1) .eq. 0 .or. colno(2) .eq. 0 .or.
     &    colno(3) .eq. 0 .or. colno(4) .eq. 0 .or.
     &    colno(5) .eq. 0 .or. colno(6) .eq. 0) then
         msg = 'XxxXyyCntPcu0 Column(s) missing--'
         msg=msg(:fcstln(msg))//'Cannot compute L6CNTPCU0'
         call fcecho(msg)
         doit = .false.
      endif
C
      do i=1,nend
C
        if (doit) then
           l6cntpcu0=0.0
           do j=1,6
              if (dbuf(colno(j),i) .ne. nulld 
     &             .and. l6cntpcu0 .ne. nulle) then
                 l6cntpcu0=l6cntpcu0+dbuf(colno(j),i)
              else
                 l6cntpcu0=nulle
              endif
           enddo
        else
           l6cntpcu0=nulle
        endif
C
C     Write L6CNTPCU0
C
        call ftpcne(olun,ncols+38,i+nread,1,1,l6cntpcu0,nulle,errstat)
        if (errstat .ne. 0) then
          message='error writing L6CNTPCU0'
          call fcerr(message)
          goto 999
        endif
C
      enddo
C     
C     If necessary, go back and process another buffer
C
      nread=nread+nend
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
      if (.not. last) goto 111

C     Update HISTORY and DATE
      write(histstr,*) 
     &   'File derived from '//infile(:fcstln(infile))
      call ftphis(olun,histstr,errstat)
      call ftgsdt(day,month,year,errstat)
C      datstr='  /  /  '
	datstr='  /  /    '
      write(datstr(1:2),'(i2)')day
      write(datstr(4:5),'(i2)')month
C      write(datstr(7:8),'(i2)')year
	write(datstr(7:10),'(i4)')year
      if (datstr(1:1) .eq. ' ')datstr(1:1)='0'
      if (datstr(4:4) .eq. ' ')datstr(4:4)='0'
C      if (datstr(7:7) .eq. ' ')datstr(7:7)='0'
      if (datstr(7:9) .eq. '   ')datstr(7:9)='000'
      write(dumcom,*) 
     &   'using '//taskname(:fcstln(taskname))//
C     &   ' on '//datstr//' (dd/mm/yy)'
     &   ' on '//datstr//' (dd/mm/yyyy)'
      call ftphis(olun,dumcom,errstat)
      if (errstat .ne. 0) then
        message='error writing HISTORY/DATE keywords'
        call fcerr(message)
        goto 999
      endif
C
C     Close FITS files and notify user of success
C
      call ftclos(ilun,errstat)
      if (errstat .ne. 0) then
        message='error closing input file'
        call fcerr(message)
        goto 999
      endif
      call ftclos(olun,errstat)
      if (errstat .ne. 0) then
        message='error closing output file'
        call fcerr(message)
        goto 999
      endif
      call ftfiou(-1,errstat)
      if (errstat .ne. 0) then
        message='error freeing i/o unit #s'
        call fcerr(message)
        goto 999
      endif
C
      message='=================================='
      call fcecho(message)
      call fcecho('XTEDERIVE finished')
C
 999  continue
      call fcerrm(errstat)
      return
      end

C-----------------------------------------------------------------------

*+GPXDERIV
      subroutine gpxderiv(infil,clobr,status)

      implicit none
      character*(*) infil
      logical clobr
      integer status

C---------------------------------------------------------------------
C Description:  Gets parameters for XTEDERIVE from the parameter file
C
C Arguments:    infil   : The value of the 'infile' param
C               clobr   : Overwrite existing .xfl file?
C               status  : The status of the subroutine
C                              = 0  --> OK 
C                              = 1  --> Problem getting parameters
C
C Authors/Modification History:
C               Mike Tripicco (1995 Nov 22), original version
C---------------------------------------------------------------------
*-Version 1.1

      character(50) contxt
      integer errstat

C     Set Status flag to 'no problem!'
      status = 0
      errstat = 0
C
C     Get infile parameter
C
      call uclgst('infile', infil, errstat)
      if(errstat.ne.0)then
        contxt = 'cannot get infile parameter'
        call fcerr(contxt)
        status = 1
        return
      endif
C
C     Get clobber parameter
C
      call uclgsb('clobber', clobr, errstat)
      if(errstat.ne.0)then
        contxt = 'cannot get clobber parameter'
        call fcerr(contxt)
        status = 1
        return
      endif

      return
      end
C--------------------------------------------------------------------------
      subroutine radecbore2(att,borexte,ra,dec,boreinert)
c
c  Adapted from Tod Strohmayer's radecbore (but without doing the
c   roll bias portion)
c
      implicit real*4 (a-h,o-z)
      dimension boreinert(3),att(3,3),borexte(3)
c
c  Do Transformation of boresight in body to inertial reference (J2000)
c
      do 3 i=1,3
        boreinert(i)=0.0
        do 4 j=1,3
          boreinert(i)=boreinert(i)+att(j,i)*borexte(j)
4       continue
3     continue
      pi=3.141592653589793
      p2=2.0*pi
      crd=180.0/pi
c
c     find dec coordinate of boresight
c
      dec=crd*asin(boreinert(3))
c
c     find ra coordinate of boresight
c

      ay=boreinert(2)
      ax=boreinert(1)
      bx=abs(boreinert(1))
      by=abs(boreinert(2))
      if (ax.eq.0.0) then
        if (ay.gt.0.0) then 
           ra=pi/2.0
        end if
        if (ay.lt.0.0) then
           ra=(3.0*pi)/2.0
        end if
      end if
c         
      if ((ax.gt.0.0).and.(ay.ge.0.0)) then
        ra=atan(by/bx)
      end if
      if ((ax.lt.0.0).and.(ay.ge.0.0)) then
        ra=pi-atan(by/bx)
      end if
      if ((ax.lt.0.0).and.(ay.le.0.0)) then
        ra=pi+atan(by/bx)
      end if
      if ((ax.gt.0.0).and.(ay.le.0.0)) then
        ra=p2-atan(by/bx)
      end if
      ra=ra*crd
   
      return
      end
c
c  Author: M. Tripicco
c  Date:   30 October 1996
c
      subroutine elv(earthx,alt,limbang)
      real*4 earthx, alt, limbang
      real*4 re, pi, crd, meanalt
      real*4 nulle
        parameter(nulle=-999999999.) 

      parameter (re=6378.16)
      parameter (pi=3.141592653589793)
      parameter (meanalt=573.12)

      crd=180.0/pi
c
c    compute half angle subtended by earth
c
      if (alt .eq. nulle) alt=meanalt
      csub=asin(re/(re+alt))
c
c    compute angle between SCX and Earth
c
      cang=acos(earthx)
c
c    output angle
c
      limbang=crd*(cang-csub)
      return
      end

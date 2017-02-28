* WRITEMAT

* Write out the detector response matrix to a FITS format disk file.
* The file format is complex.  It consists of the following main
* elements.
*
* 1. Header information.
*    Who ran the program.  The name of the file.
*    How the "true" energy bins were constructed.
*    Number of measured energy bands.
*    Number of distinct time intervals.
*    Coordinate system used.  Source position.
*    Zenith angle cutoff method.  Type of events accepted.
* 2. A list of the orientation intervals.  Start and stop time,
*    whether TASC was used in the coincidence logic.
* 3. The response matrix itself.
* 4. Properties of the observed energy bands.
*    Energy boundaries.  Size of angular acceptance cones.
*    Zenith cutoffs.  Kniffen factors and uncertainties.
* 5. A list of the true energy values.
* 6. The text of the data file header.  This can be compared with
*    the file read by SPECNLYZ to make sure they are identical.

* The name of the file to produce is contained in the environmental
* variable RESPONSE.

* Inputs:  lots of them

* Outputs: none

* Written for Spectral version 2.7, December 1994
* PLN, Stanford
* Note:  This code used to be in the main routine "matrix.f"
* Version 2.10, Feb 95.  Added blocksize to ftopen calls.  I don't know
*   how it worked without it.

* @(#) writemat.f 1.3@(#)

      subroutine writemat(norns,orntime,nregns,celeq,srclon,srclat,
     >     cutoff,orntasc,resp,eregn,conetheta,cutangle,fudge,
     >     sfudge,einc,pulsar,pstyle,evclass,data_dir,selfil,
     >     outfile)

      implicit none

      include '../SPECMAT_COMMON/spectral.inc'  ! for first,last,spacing,jmax
      include '../SPECMAT_COMMON/lunits.inc'    ! for lumat,lusel

* Arguments
      character*(*)  data_dir,selfil,outfile,pulsar
      integer norns,nregns,evclass
      real*8 orntime(2,nornmax)
      real srclon,srclat,cutoff,resp(first:last,0:jmax)
      real eregn(0:jmax),conetheta(0:jmax),cutangle(0:jmax)
      real fudge(0:jmax),sfudge(0:jmax),einc(first-1:last)
      logical celeq,orntasc(nornmax)
      character(100) pstyle

*     VARIABLES
      integer i,irec,istatus
      real*8 ornstart(nornmax),ornend(nornmax)
      character(1) tunit, extname
      character(130) charline
c      character(30) username,errtext
      character(30) errtext
      character(10) ttype1(5),tunit1(5),tform1(5)
      integer naxes(2)
c      integer naxes(2),blocksize,status

      save

      data ttype1/'eregn','conetheta','cutangle','fudge','sfudge'/
      data tform1/5*'1E'/
      data tunit1/'MeV',2*'degrees',2*' '/


      istatus = 0

*WRITE OUT THE MATRIX:
      write (6,*) 'Opening response file ',outfile
      do i = 1,norns
	 ornstart(i) = orntime(1,i)
         ornend(i) = orntime(2,i)
      enddo
      call ftinit(lumat,outfile,1,istatus)
      naxes(1) = last-first+1
      naxes(2) = nregns+1
      call ftphpr(lumat,.true.,-32,2,naxes,0,1,.true.,istatus)
      call ftpdef(lumat,-32,2,naxes,0,1,istatus)
      call ftpdat(lumat,istatus)
*  Insert useful information into header
csb-02/07	 call getenv('USERNAME',username)

csb-02/07	 call getenv('EVCLASS', evclass)

c	 call ftpkys(lumat,'username',username,'Who did this',istatus)
      call ftpcom(lumat,'SPECTRAL Response Matrix',istatus)
      call ftpkys(lumat,'telescop','EGRET',' ',istatus)
      call ftpkys(lumat,'observer','FICHTEL',' ',istatus)
      call ftpkys(lumat,'equinox','2000',' ',istatus)
      call ftpkys(lumat,'filename',outfile,' ',istatus)
      call ftpkyj(lumat,'first',first,' ',istatus)
      call ftpkyj(lumat,'last',last,' ',istatus)
      call ftpkyj(lumat,'spacing',spacing,' ',istatus)
      call ftpkyj(lumat,'nregns',nregns,'cones',istatus)
      call ftpkyj(lumat,'norns',norns,'time intervals',istatus)
      call ftpkyl(lumat,'celeq',celeq,'coord system',istatus)
      if (celeq) then
	 call ftpkys(lumat,'coords','RA/DEC','coordinate system',
     >               istatus)
      else
	 call ftpkys(lumat,'coords','GALACTIC','coordinate system',
     >               istatus)
      endif
      call ftpkyf(lumat,'srclon',srclon,6,'source longitude',istatus)
      call ftpkyf(lumat,'srclat',srclat,6,'source latitude',istatus)
      call ftpkye(lumat,'cutoff',cutoff,5,'zenith angle',istatus)
      if (evclass.eq.1) then
	 call ftpkys(lumat,'EN-CLAS','A','energy class', istatus)
      else if (evclass.eq.2) then
	 call ftpkys(lumat,'EN-CLAS','A+B+C','energy class',istatus)
      endif
*   Orientation intervals, TASC in/out status
      call ftpknd(lumat,'ornstr',1,norns,ornstart,11,'orn start&',
     >            istatus)
      call ftpknd(lumat,'ornend',1,norns,ornend,11,'orn end&',
     >            istatus)
      call ftpknl(lumat,'ortasc',1,norns,orntasc,'TASC in/out&',
     >            istatus)
*   Write response matrix
      call ftp2de(lumat,0,last-first+1,naxes(1),naxes(2),resp,
     >            istatus)
*   Write energy bands and acceptance cones and zenith cutoffs
*   and empirical fudge factors
      call ftcrhd(lumat,istatus)
      call ftphbn(lumat,nregns+1,5,ttype1,tform1,tunit1,' ',0,
     >            istatus)
      call ftbdef(lumat,5,tform1,0,nregns+1,istatus)
      call ftpcom(lumat,'observed energy bands and acceptance cones',
     >            istatus)
      call ftpcle(lumat,1,1,1,nregns+1,eregn,    istatus)
      call ftpcle(lumat,2,1,1,nregns+1,conetheta,istatus)
      call ftpcle(lumat,3,1,1,nregns+1,cutangle, istatus)
      call ftpcle(lumat,4,1,1,nregns+1,fudge,    istatus)
      call ftpcle(lumat,5,1,1,nregns+1,sfudge,   istatus)
*   Write "true" energy values
      call ftcrhd(lumat,istatus)
      call ftphbn(lumat,last-first+2,1,'true energy','1E','MeV',
     >            ' ',0,istatus)
      call ftbdef(lumat,1,'1E',0,last-first+2,istatus)
      call ftpcom(lumat,'true energy bands',istatus)
      call ftpcle(lumat,1,1,1,last-first+2,einc,istatus)
*   Copy SELECT header
      call ftcrhd(lumat,istatus)
c     call getenv('SELECTFILE',selfil)

c     selfil = data_dir(1:len_trim(data_dir)) // selfil

c     if (pulsar.eq.'PULSAR') then
c            if (pstyle.eq.'OLD') then
c               open (lusel,file=selfil,status='old',access='direct',
c     >              recl=130)
c               call ftphtb(lumat,130,22,1,'SELECT header',1,'A130',
c     >              ' ',' ',istatus)
c               call ftadef(lumat,130,1,1,'A130',22,istatus)
c               call ftpcom(lumat,'SELECT file header lines',istatus)
c               irec = 1
c               read (lusel,rec=irec) charline
c               do while (charline(1:1).eq.'*')
c                  call ftpcls(lumat,1,irec,1,1,charline,istatus)
c                  irec = irec + 1
c                  read (lusel,rec=irec) charline
c               end do
c               call ftmkyj(lumat,'naxis2',irec,'&',istatus)
c               call ftddef(lumat,irec*130,istatus)
c               close (lusel)
c            else if (pstyle.eq.'NEW') then
c               call ftopen(lusel,selfil,0,blocksize,istatus)
c               call ftphtb(lumat,80,30,1,'data file header',1,'A80',
c     >              ' ',' ',istatus)
c               call ftadef(lumat,80,1,1,'A80',30,istatus)
c               call ftpcom(lumat,'data file header lines',istatus)
c               irec = 1
c               call ftgrec(lusel,irec,charline,istatus)
c               do while (charline(1:6).ne.'END   ')
c                  call ftpcls(lumat,1,irec,1,1,charline,istatus)
c                  irec = irec + 1
c                  call ftgrec(lusel,irec,charline,istatus)
c               end do
c               call ftpcls(lumat,1,irec,1,1,charline,istatus)
c               call ftmkyj(lumat,'naxis2',irec,'&',istatus)
c               call ftddef(lumat,irec*80,istatus)
c               call ftclos(lusel,istatus)
c            end if
c     else
  
      open (lusel,file=selfil,status='old')
      tunit = ' '
      extname = ' '
      call ftphtb(lumat,80,30,1,'data file header',1,'A80',
     >            tunit,extname,istatus)
      call ftadef(lumat,80,1,1,'A80',30,istatus)
      call ftpcom(lumat,'data file header lines',istatus)
      irec=1
      read (lusel,'(a)') charline
      do while (charline(1:8).ne.'.......e'.and.
     >          charline(1:8).ne.'End time')
	 call ftpcls(lumat,1,irec,1,1,charline,istatus)
	 irec=irec+1
         read (lusel,'(a)') charline
      enddo
      call ftpcls(lumat,1,irec,1,1,charline,istatus)
      call ftmkyj(lumat,'naxis2',irec,'&',istatus)
      call ftddef(lumat,irec*80,istatus)
      close (lusel)
c     end if

* Close matrix file
      call ftclos(lumat,istatus)
      if (istatus.eq.0) then
	 write (*,*) 'Created response file with no errors.'
      else
	 write (*,*) 'FITS error ',istatus,' occurred when ',
     >               'creating response file:'
	 call ftgerr(istatus,errtext)
	 write (*,*) ' ',errtext
      endif

      return
      end

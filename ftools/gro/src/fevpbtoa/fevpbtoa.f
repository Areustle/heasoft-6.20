C**********************************************************************
C TASK: 
C      fevtbtoa
C
C DESCRIPTION:
C      This utility converts Fits formatted COMPTEL EVP data files 
C      to an ascii formatted file.
C      Application of the scaling factors is optional.
C
C      (The output structure is based on the routine SEVPBTOA written
C      by Rita Freuder at the University of New Hampshire, that
C      uses the native data on a SUN computer.)
C
C
C AUTHOR:
C      Mark Cresitello-Dittmar                        July 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C   The following table indicates the correspondence between the arrays
C   used in this code (first column) and the arrays which are normally
C   referred to in the EVP documentation (and in the COMPASS code).
C
C     pit      = timtag(1)  = TJD
C     pit2     = timtag(2)  = COMPASS tics
C     mep1(1)  = mpar(1)    = D1 energy loss (keV)
C     mep1(2)  = mpar(2)    = D2 energy loss (keV)
C     mep1(3)  = mpar(5)    = phibar (radians)
C     classc   = class      = event classification code
C     mep2(1)  =            = X-location in D1 (scaled)
C     mep2(2)  =            = Y-location in D1 (scaled)
C     mep2(3)  =            = Lambda location in D1 (scaled)
C     mep2(4)  =            = X-location in D2 (scaled)
C     mep2(5)  =            = Y-location in D2 (scaled)
C     mep2(6)  =            = Lambda location in D2 (scaled)
C     mep2(7)  =            = PSD (scaled)
C     mep2(8)  =            = TOF (scaled)
C     rmep2(1) = mpar(7)    = X-location in D1
C     rmep2(2) = mpar(8)    = Y-location in D1
C     rmep2(3) = mpar(9)    = Lambda location in D1
C     rmep2(4) = mpar(10)   = X-location in D2
C     rmep2(5) = mpar(11)   = Y-location in D2
C     rmep2(6) = mpar(12)   = Lambda location in D2
C     rmep2(7) = mpar(13)   = PSD
C     rmep2(8) = mpar(14)   = TOF
C     mep3(1)  = modcom     = Module combination
C     mep3(2)  = reflag     = Rejection flag
C     mep3(3)  = veto       = Veto flag
C     esct(1)  = escat(1)   = Scatter direction galactic longitude (rads)
C     esct(2)  = escat(2)   = Scatter directiongalactic latitude (rads)
C     esct(3)  = escat(3)   = Scatter direction azimuth (rads)
C     esct(4)  = escat(4)   = Scatter direction zenith (rads)
C     esct(5)  = escat(5)   = Earth horizon angle of scatter vector (rads)
C
C
C USAGE:
C      call fevpba
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      see notes
C
C CALLED ROUTINES:
C
C      gevpbtoa  -  retrieves parameters for the run
C      doevpbtoa -  Performs the task
C
C**********************************************************************
       subroutine fevpba

       implicit none

       integer frec,nrec
       integer status
       logical apply
       character*(80) errtxt
       character*(180) fitfile
C
       character*(40) taskname
       common /task/ taskname

       taskname = 'fevpbtoa v1.0'
       status = 0
C
C  get run parameters
       call gevpbtoa(fitfile,frec,nrec,apply,status)
       if(status.ne.0) then
         write(errtxt,'(''Error getting parameters'')')
         call fcecho(errtxt)
         goto 999
       endif
C
C  Meat of the program in this subroutine.
       call doevpbtoa(fitfile,frec,nrec,apply,status)
       if(status.ne.0) then
         write(errtxt,'(''Error from doevp2fits'')')
         call fcecho(errtxt)
       endif
C
C  exit routine
 999   continue

       end


C**********************************************************************
C SUBROUTINE:
C      doevpbtoa
C
C DESCRIPTION:
C      This tool converts Fits formatted COMPTEL EVP data files 
C      to an ascii formatted file.
C      Application of the scaling factors is optional.
C
C      (The output structure is based on the routine SEVPBTOA written
C      by Rita Freuder at the University of New Hampshire, that
C      uses the native data on a SUN computer.)
C
C
C AUTHOR:
C      Mark Cresitello-Dittmar                        July 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C   The following table indicates the correspondence between the arrays
C   used in this code (first column) and the arrays which are normally
C   referred to in the EVP documentation (and in the COMPASS code).
C
C     pit      = timtag(1)  = TJD
C     pit2     = timtag(2)  = COMPASS tics
C     mep1(1)  = mpar(1)    = D1 energy loss (keV)
C     mep1(2)  = mpar(2)    = D2 energy loss (keV)
C     mep1(3)  = mpar(5)    = phibar (radians)
C     classc   = class      = event classification code
C     mep2(1)  =            = X-location in D1 (scaled)
C     mep2(2)  =            = Y-location in D1 (scaled)
C     mep2(3)  =            = Lambda location in D1 (scaled)
C     mep2(4)  =            = X-location in D2 (scaled)
C     mep2(5)  =            = Y-location in D2 (scaled)
C     mep2(6)  =            = Lambda location in D2 (scaled)
C     mep2(7)  =            = PSD (scaled)
C     mep2(8)  =            = TOF (scaled)
C     rmep2(1) = mpar(7)    = X-location in D1
C     rmep2(2) = mpar(8)    = Y-location in D1
C     rmep2(3) = mpar(9)    = Lambda location in D1
C     rmep2(4) = mpar(10)   = X-location in D2
C     rmep2(5) = mpar(11)   = Y-location in D2
C     rmep2(6) = mpar(12)   = Lambda location in D2
C     rmep2(7) = mpar(13)   = PSD
C     rmep2(8) = mpar(14)   = TOF
C     mep3(1)  = modcom     = Module combination
C     mep3(2)  = reflag     = Rejection flag
C     mep3(3)  = veto       = Veto flag
C     esct(1)  = escat(1)   = Scatter direction galactic longitude (rads)
C     esct(2)  = escat(2)   = Scatter directiongalactic latitude (rads)
C     esct(3)  = escat(3)   = Scatter direction azimuth (rads)
C     esct(4)  = escat(4)   = Scatter direction zenith (rads)
C     esct(5)  = escat(5)   = Earth horizon angle of scatter vector (rads)
C
C
C USAGE:
C       call doevpbtoa(fitfile,frec,nrec,apply,status)
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      see notes
C
C CALLED ROUTINES:
C
C      ftopen
C      fcecho
C      fcerrm
C      ftmahd
C      ftgkyj
C      ftgcvd
C      ftgkye
C      ftgcve
C      fttscl
C      ftgcvi
C      ftgcvb
C      ftclos
C
C**********************************************************************
       subroutine doevpbtoa(fitfile,frec,nrec,apply,status)

       implicit none

       integer luin,luout
       integer status
       integer blksiz
       integer hdutype
       integer i,j,k,colno
       integer frec,nrec,recchk
       logical anyf
       logical apply
       character*(8) keywrd
       character*(50) comnt
       character*(80) errtxt

       character*(180) fitfile,outfile

       integer*2     mep2(8)
       integer*2     mep3(3)
       integer       pit,pit2
       real          mep1(3)
       real          esct(5)
       real          rmep2(8)
       character*(8) classc

       integer*2 shtvar
       real      fltvar
       real      scalval
       double precision deg2rad
       parameter (deg2rad = 0.0174532925)

       integer*2 nullval_zero
       parameter (nullval_zero = 0)

       luin = 15
       luout= 17
       blksiz = 1

C  open fits file
       call ftopen(luin, fitfile,0,blksiz,status)
       if(status.ne.0) then
         write(errtxt,'(''Error opening FITS file.'')')
         call fcecho(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  move to binary table extension
       call ftmahd(luin,2,hdutype,status)
       if(status.ne.0) then
         write(errtxt,'(''Error moving to Binary extension'')')
         call fcecho(errtxt)
         goto 998
       endif
C
C  check if (frec+nrec) records exist in the input file
       call ftgkyj(luin, 'NAXIS2  ', recchk, comnt, status)
       if((frec+nrec).gt.recchk) then
         write(errtxt,'(''ERROR, # of records in file:'',i10)') recchk
         call fcecho(errtxt)
         status = 0
       endif

       nrec = nrec + frec - 1
C
C  Open ascii file
       outfile = 'ascfit.evp'
       write(errtxt,'(''Opening '',a11)') outfile
       call fcecho(errtxt)
       call faopen(luout,outfile,3,80,status)
C       open(unit=luout,file=outfile,status='UNKNOWN',iostat=status)
       if(status.ne.0) then
         write(errtxt,'(''Error opening ASCII file.'')')
         call fcecho(errtxt)
         call fcerrm(status)
         goto 998
       endif         
C
C  loop over events
       do 100 i = frec,nrec
C
C    skip column 1 (Time of event in seconds)
C
C    read columns 2 - 9
         do 101 colno = 2,9
           if (apply) then
             write(keywrd,'(''TSCAL'',i1)') colno
             call ftgkye(luin,keywrd,scalval,comnt,status)
             call ftgcve(luin,colno,i,1,1,0,fltvar,anyf,status)
             rmep2(colno-1) = fltvar
           else 
             scalval = 1.0
             call fttscl(luin,colno,dble(scalval),dble(0.0),status)
             call ftgcvi(luin,colno,i,1,1,nullval_zero,shtvar,anyf,
     &                   status)
             mep2(colno-1) = shtvar
           endif
 101     continue
C
C    read columns 10 - 12
         do 102 colno = 10,12
           call ftgcvi(luin,colno,i,1,1,nullval_zero,shtvar,anyf,
     &                 status)
           mep3(colno-9) = shtvar
 102     continue
C
C    read columns 13 and 14 
         colno = 13
          call ftgcvj(luin,colno,i,1,1,0,pit,anyf,status)
         colno = 14
          call ftgcvj(luin,colno,i,1,1,0,pit2,anyf,status)
C
C    read columns 15 and 16
         do 103 colno = 15,16
           call ftgcve(luin,colno,i,1,1,0,fltvar,anyf,status)
           mep1(colno-14) = fltvar
 103     continue
C
C    read columns 17 - 22
         colno = 17
          call ftgcve(luin,colno,i,1,1,0,fltvar,anyf,status)
          fltvar = fltvar * deg2rad
          mep1(3) = fltvar

         do 104 colno = 18,22
           call ftgcve(luin,colno,i,1,1,0,fltvar,anyf,status)
           fltvar = fltvar * deg2rad
           esct(colno-17) = fltvar
 104     continue
C
C     read column 23
           call ftgcvb(luin,colno,i,1,8,0,classc,anyf,status)
C
         if(status.ne.0) then
           if(status.eq.107) then
             write(errtxt,'(''End-of-file encountered'')')
             call fcecho(errtxt)
             status = 0
             goto 105
           else
             write(errtxt,'(''Error getting event '',i10)') i
             call fcecho(errtxt)
             call fcerrm(status)
             goto 998
           endif
         endif
C
C     write entry to ascii file
         write(luout,861,err=998) pit,pit2,classc
         write(luout,867,err=998) (mep1(j),j=1,3)
         if (apply) then
            write(luout,878,err=998) (rmep2(j),j=1,8)
         else
            write(luout,868,err=998) (mep2(j),j=1,8)
         endif
         write(luout,879,err=998) (esct(j),j=1,5),(mep3(k),k=1,3)
C

 100   continue
 105   continue

 998   continue

C  Formats for short EVP  record
 861   format(i6,i9,1x,a8)
 867   format(6e10.3)
 868   format(8i10)
 878   format(8e10.3)
 879   format(5e13.6,i4,i6,i3)

       call ftclos(luin,status)
       close(luout)

 999   continue

       end


C**********************************************************************
C SUBROUTINE:
C      gevpbtoa
C
C DESCRIPTION:
C      Get run parameters from parameter file
C
C AUTHOR:
C      Mark Cresitello-Dittmar                 July 1994
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      uses F77/VOS like calls to read parameters from .par file
C
C USAGE:
C      call gevpbtoa(fitfile, status)
C
C ARGUMENTS:
C     fitfile      Input FITS formatted EVP data file.
C     frec         Record at which to begin translating.
C     nrec         Number of records to translate
C     scal         Flag for applying data scale factors
C     status       Status flag
C     
C PRIMARY LOCAL VARIABLES:
C     errtxt       error message
C
C CALLED ROUTINES:
C     subroutine fcerr     - write error message
C     subroutine uclgs_    - gets parameters from par file
C
C**********************************************************************
       subroutine gevpbtoa(fitfile,frec,nrec,scal,status)
C
       implicit none
C
       character*(*) fitfile
       integer frec,nrec
       logical scal
       integer  status
C
       character*(80) errtxt
C
C
       status = 0
C
C  get the name of the input QVP file.
       call uclgst('fitfile',fitfile,status)
       if (status .ne. 0) then
          errtxt = 'could not get input EVP file parameter'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get record number to start at.
       call uclgsi('frec',frec,status)
       if (status .ne. 0) then
          errtxt = 'could not get starting record parameter'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get record number to start at.
       call uclgsi('nrec',nrec,status)
       if (status .ne. 0) then
          errtxt = 'could not get number of records to translate'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get flag for applying scales if appropriate
       call uclgsb('scal', scal, status)
       if (status .ne. 0) then
          errtxt = 'Error getting scale flag parameter'
          call fcerr(errtxt)
          goto 999
       endif


 999   continue
       return
       end




      subroutine lcmathcopy(lui,as,inext,ivecti,luo,ou_fil,ivecto,ierr)

c COPY the contents of the input file to the output file, except for
c the rate table data, which will be written after BacKGround subtraction.

c This routine makes an exact copy of the input file, except for
c the 1 (one) extension for which the count rate will be modified.
c That extension retains its original place in the file, but its
c structure is altered.  The following points apply:

c   Only columns recognizable to xronos are retained.  These include
c      Time                -- ivect(1)
c      Rate                -- ivect(2)
c      Error               -- ivect(3)
c      Deadtime            -- ivect(4)
c      Integration time    -- ivect(5)
c      Fractional exposure -- ivect(6)

c   Each of these columns are written in 1E (real) format -- except for Time,
c   deadtime, or integration-time columns,
c   which are 1D (double precision) -- regardless of
c   how they are represented in the input file.  This eases any need
c   to reproduce internal scaling from the original file.
c   It would require major modifications for lcmath to
c   process files with TIME packets.

c   The error column is created anew if it is not present in the original
c   file, and its values are calculated internally in subroutine lcmathread.

c Keywords that get changed are:

c     HDUCLAS2 = 'NET' if as = .false., i.e., if subtraction takes place.
c     BACKAPP  =  T    "   " "   "   "   "   "   "   "   "   "   "   "
c     VIGNAPP  =  T  if vignetting corrections are applied 
c     DEADAPP  =  T  if dead time   "    "   "   "   "   " 

c See below for comments regarding subtleties that still need to be worked out
c in copying the header keywords.

c >>> This routine needs to have the docor parameter passed to it as a switch
c     for corrections keywords!<<<

c  I  lui    (i)  Lu of input FITS file
c  I  as     (l)  = true if adding rather than subreacting
c  I  ivecti (i)  List of relevant column numbers in the input file
c  I  inext  (i)  Number of the rate table extension to be modified
c  I  luo    (i)  Lu of output FITS file
c  I  ou_fil (c)  Name of output file
c  O  ivecto (i)  List of relevant column numbers in the output file

c Author:  EAL  NASA Goddard, HSTX   April, 1994

      include '../../include/io.inc'
      integer cmax
      parameter (cmax = 100)
      logical backapp,deadapp,vignapp,as
      character*(*) ou_fil
      character(80) kname,comm,cdum,card
      character(16) extname,tform(cmax),ttype(cmax),tunit(cmax)
     &   ,tform_in(cmax),ttype_in(cmax),tunit_in(cmax)
      integer hdutype,ftstat,pcount,lui,luo,ivecti(*),ivecto(*),inext
     &   ,nkeys,ndum,nrows,nfield,i,block,morekeys,ierr,kystat
      real deadc,vignet,rdum1,rdum2,rdum3
      data block, pcount, morekeys /2880, 0, 0/
      parameter (subname = 'lcmathcopy:')

      if(ierr.ne.0) return

c Open the output file.

      ftstat = 0
      CALL ftinit(luo,ou_fil,block,ftstat)

c If file the already exists, delete it and open a new one.

      if(ftstat.ne.0) then
         ftstat = 0
         CALL xdelfil(ou_fil,ftstat)
         CALL ftinit(luo,ou_fil,block,ftstat)
      endif

c Copy over the primary array.

c      write(*,*)'ftstat,hdutype',ftstat,hdutype
      CALL ftmahd(lui,1,hdutype,ftstat)
      CALL ftcopy(lui,luo,morekeys,ftstat)
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
  
c Copy any extensions previous to the rate table.

      do i = 2, inext - 1
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
         CALL ftmahd(lui,i,hdutype,ftstat)
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
         CALL ftcrhd(luo,ftstat)
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
         CALL ftcopy(lui,luo,morekeys,ftstat)
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
      enddo

c Create the new rate table extension in the output file.

      CALL ftcrhd(luo,ftstat)

c Go to the rate table in the input file, and get header information.

c      write(*,*)'ftstat,hdutype',ftstat,hdutype
      CALL ftmahd(lui,inext,hdutype,ftstat)
c      write(*,*)'ftstat,hdutype',ftstat,hdutype
      CALL ftghsp(lui,nkeys,ndum,ftstat)
      CALL ftghbn(lui,cmax,nrows,nfield,ttype_in,tform_in,tunit_in
     &           ,extname,pcount,ftstat)

c Sift through the columns in the input file to specify columns for 
c the output file.

      nfield = 0
      do i = 1,10
         if(ivecti(i).gt.0) then
            nfield = nfield + 1
            ivecto(i) = nfield

c >>> i = 2 (rate) and i = 3 (error) are special cases in which we know
c     the final form in the output file.<<<
c >>> This part will be updated to include more than one energy channel.<<<

            if(i.eq.2) then
               ttype(nfield) = 'RATE'
               tform(nfield) = 'E'
               tunit(nfield) = 'count/s'
            elseif(i.eq.3) then
               ttype(nfield) = 'ERROR'
               tform(nfield) = 'E'
               tunit(nfield) = 'count/s'
            else
               ttype(nfield) = ttype_in(ivecti(i))
               IF((i.eq.1).or.(i.eq.4).or.(i.eq.5)) THEN
                  tform(nfield) = 'D'
               ELSE
                  tform(nfield) = 'E'
               ENDIF
               tunit(nfield) = tunit_in(ivecti(i))      
               kystat = 0
            endif
         endif
      enddo

c Create an error column if there is none in the input file.

      if(ivecti(3).eq.0) then
         nfield = nfield + 1
         ivecto(3) = nfield
         ttype(nfield) = 'ERROR'
         tform(nfield) = 'E'
         tunit(nfield) = 'count/s'
      endif

c Define the structure of the rate table extension in the output file.

      CALL ftphbn(luo,nrows,nfield,ttype,tform,tunit,extname,pcount
     &           ,ftstat)
      CALL ftbdef(luo,nfield,tform,pcount,nrows,ftstat)

c Copy all keywords from the input file that are not already in the output file.

c >>> We have to be careful about copying mXXXXnnn keywords for
c     multidimensional column arrays.  Not sure yet how to filter
c     these out.<<<

      do i = 1, nkeys
         CALL ftgkyn(lui,i,kname,cdum,comm,ftstat)
         IF((kname(:5).ne.'NAXIS').and.(kname(:5).ne.'TFORM').and.
     &      (kname(:5).ne.'TTYPE').and.(kname(:5).ne.'TUNIT').and.
     &      (kname(:4).ne.'TDIM' ).and.(kname(:5).ne.'TUNIT').and.
     &      (kname(:5).ne.'TSCAL').and.(kname(:5).ne.'TZERO').and.
     &      (kname  .ne.'EXTNAME').and.(kname  .ne.'PCOUNT ').and.
     &      (kname  .ne.'GCOUNT' ).and.(kname  .ne.'TFIELDS').and.
     &      (kname  .ne.'BITPIX' ).and.(kname  .ne.'XTENSION')) THEN
            CALL ftgrec(lui,i,card,ftstat)
            CALL ftprec(luo,card,ftstat)
         ENDIF
      enddo

c Create the HDUCLAS2 = 'NET' keyword

      kystat = 0
      IF(.not.as) THEN
         CALL ftdkey(luo,'HDUCLAS2',kystat)
         comm = 'Data have been background-subtracted'
         CALL ftpkys(luo,'HDUCLAS2','NET',comm,ftstat)
      ENDIF
    
c Corrections keywords: leave unchanged if corrections already applied.

      CALL xrftgbdv(lui,deadapp,deadc,vignapp,vignet,rdum1,rdum2,rdum3)

      IF(.not.deadapp) THEN
         kystat = 0
         CALL ftdkey(luo,'DEADAPP',kystat)
         IF((deadc.lt.1.).or.(ivecti(4).gt.0)) THEN
            deadapp = .true.
            comm = 'Deadtime corrections have been applied.'
         ELSE
            deadapp = .false.
            comm = 'Deadtime corrections have not been applied.'
         ENDIF
         CALL ftpkyl(luo,'DEADAPP',deadapp,comm,ftstat)
      ENDIF

      IF(.not.vignapp) THEN
         kystat = 0
         CALL ftdkey(luo,'VIGNAPP',kystat)
         IF(vignet.lt.1.) THEN
            vignapp = .true.
            comm = 'Collimator corrections have been applied.'
         ELSE
            vignapp = .false.
            comm = 'Collimator corrections have not been applied.'
         ENDIF
         CALL ftpkyl(luo,'VIGNAPP',vignapp,comm,ftstat)
      ENDIF

      IF(.not.as) THEN
         backapp = .true.
         comm = 'Background corrections have been applied'
         CALL ftdkey(luo,'BACKAPP',ftstat)
         ftstat=0
         CALL ftpkyl(luo,'BACKAPP',backapp,comm,ftstat)
      ENDIF

c Error trap.
         
      if(ftstat.ne.0) then
         ierr = ftstat
         write(errm,*) ' lcmathcopy: fitsio error = ',ftstat
         errm = subname//' '//errm
         CALL xaerror(errm,1)
         return
      endif

c Copy over any remaining extensions.

      i = inext
      do while(ftstat.eq.0)
         i = i + 1
         CALL ftmahd(lui,i,hdutype,ftstat)
         CALL ftcrhd(luo,ftstat)
         CALL ftcopy(lui,luo,morekeys,ftstat)
      enddo
c      write(*,*)'ftstat,hdutype',ftstat,hdutype  

      return
      end

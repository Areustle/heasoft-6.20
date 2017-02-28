
      SUBROUTINE wrtarf(arfu, nenerg, instrum, waoaa, lower,
     &                  hiher, middl, arf, hist, nhist,
     &                  rsfil, arfstat)

      INTEGER arfu, nenerg, instrum, arfstat, nhist
      REAL lower(nenerg), hiher(nenerg), middl(nenerg)
      REAL arf(nenerg)
      REAL waoaa
      CHARACTER*(*) hist(nhist), rsfil

c Write the ARF. This routine assumes that the ARF has already
c been created using an FTINIT call.

c Arguments
c    arfu     i           i: I/O unit for ARF
c    rhowbig  i           i: size of energy arrays
c    nenerg   i           i: actual number of energies
c    instrum  i           i: Instrument (0-3)
c    waoaa    r           i: Weighted mean off-axis number
c    lower    r           i: Lower energies
c    hiher    r           i: Upper energies
c    middl    r           i: Mid energies
c    arf      r           i: The calculated areas
c    hist     c           i: History records
c    nhist    i           i: Number of history records
c    rsfil    c           i: The RMF filename used to get energies
c    arfstat  i           r: Status  -   0=OK

      INTEGER arftcol, arfbpp, arfnax, arfpax(2), arfpc, arfgc
      INTEGER i
      LOGICAL foundbadbin, arfsimp, arfxt
      character(64) arfttyp(3),arftfmt(3),arftunt(3)
      CHARACTER context*64, comment*64, instr*4

c Write the primary header and null data

      arfsimp=.true.
      arfbpp=8
      arfnax=0
      arfpax(1)=0
      arfpax(2)=0
      arfpc=0
      arfgc=1
      arfxt=.true.
      call ftphpr(arfu,arfsimp,arfbpp,arfnax,arfpax,
     -            arfpc,arfgc,arfxt,arfstat)
      call ftpdef(arfu,arfbpp,arfnax,arfpax,arfpc,arfgc,arfstat)

* create the ARF extension

      call ftcrhd(arfu,arfstat)

*make the type, format, and unit names for the columns...

      arfttyp(1)='ENERG_LO'
      arfttyp(2)='ENERG_HI'
      arfttyp(3)='SPECRESP'
      arftfmt(1)='1E'
      arftfmt(2)='1E'
      arftfmt(3)='1E'
      arftunt(1)='keV'
      arftunt(2)='keV'
      arftunt(3)='cm**2'
*....and how many columns are there?
      arftcol=3

*write extension table header

      call ftphbn(arfu,nenerg,arftcol,arfttyp,arftfmt,arftunt,
     -            'SPECRESP',0,arfstat)

*write in OGIP standard keywords

      comment='Telescope (mission) name'
      call ftpkys(arfu,'TELESCOP','ASCA',comment,arfstat)

      IF (instrum .EQ. 0) instr = 'SIS0'
      IF (instrum .EQ. 1) instr = 'SIS1'
      IF (instrum .EQ. 2) instr = 'GIS2'
      IF (instrum .EQ. 3) instr = 'GIS3'
      comment='Instrument name'
      call ftpkys(arfu,'INSTRUME',instr,comment,arfstat)

      comment='Instrument filter'
      call ftpkys(arfu,'FILTER','NONE',comment,arfstat)

      comment='Organisation devising file format'
      CALL ftpkys(arfu,'HDUCLASS','OGIP',comment,arfstat)

      comment='File relates to response of instrument'
      CALL ftpkys(arfu,'HDUCLAS1','RESPONSE',comment,arfstat)

      comment='effective area data is stored'
      CALL ftpkys(arfu,'HDUCLAS2','SPECRESP',comment,arfstat)

      comment='version of file format (see OGIP/92-002)'
      CALL ftpkys(arfu,'HDUVERS','1.1.0',comment,arfstat)

      CALL ftpkys(arfu, 'HDUDOC', 'OGIP memos CAL/GEN/92-002 & 92-002a',
     -    'Documents describing the format', arfstat)

      comment='obsolete - included for backward compatibility'
      CALL ftpkys(arfu,'HDUVERS1','1.0.0',comment,arfstat)

      comment='obsolete - included for backward compatibility'
      CALL ftpkys(arfu,'HDUVERS2','1.1.0',comment,arfstat)

      comment='OGIP Format version'
      call ftpkys(arfu,'ARFVERSN','1992a',comment,arfstat)

* write in the RMF file used to get the energies

      comment='RMF file used to get the energies'
      CALL ftpkys(arfu,'RESPFILE',rsfil,comment,arfstat)

* write in weighted off axis angle average

      comment='WMAP-wgtd avg off-axis ang'
      call ftpkye(arfu,'WAOAA',waoaa,5,comment,arfstat)

* write in history keywords

      DO i = 1, nhist
         CALL ftphis(arfu, hist(i), arfstat)
      ENDDO

      if (arfstat.ne.0) then
         call fcerr('Error creating header or keywords for ARF')
         call fcerrm(arfstat)
         goto 999
      endif

* define the structure of the table we're going to write
* it's got the same number of rows as the response file, note.

        call ftbdef(arfu,arftcol,arftfmt,0,nenerg,arfstat)

* write the data into the table, first noting if any of the energy channels
* are outside the allowed range of values, and writing such a comment to
* the ARF file

      i=0
      foundbadbin=.false.
 39   if (.not.foundbadbin.and.i.lt.nenerg) then
         i=i+1
         if (middl(i).gt.-999.001.and.middl(i).lt.-998.999) then
            foundbadbin=.true.
         endif
         goto 39
      endif

      if (foundbadbin) then
         context='0.0 written to SPECRESP column'//
     -           ' for RMF bins not in range 0<E<=12'
         call fcecho(context)
         call ftpcom(arfu,context,arfstat)
      endif
      i=0


      do 19 i=1,nenerg
         call ftpcle(arfu,1,i,1,1,lower(i),arfstat)
         call ftpcle(arfu,2,i,1,1,hiher(i),arfstat)
         call ftpcle(arfu,3,i,1,1,arf(i),arfstat)
 19   continue

      if (arfstat.ne.0) then
         call fcerr('Error writing effective areas to ARF')
         call fcerrm(arfstat)
         goto 999
      endif

* that's all

      call ftclos(arfu,arfstat)
      if (arfstat.ne.0) then
         call fcerr('Error closing ARF file')
         call fcerrm(arfstat)
         goto 999
      endif

 999  CONTINUE
      RETURN
      END

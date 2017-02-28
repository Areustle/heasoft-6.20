*+WT_COPY
c     --------------------------------------------------------
      subroutine wt_copy(infile,outfile,specext,tothd,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This subroutine copys extensions from infile to a new file, 
c excluding the SPECTRUM extension. This is done by calling the
c FTOOL FAPPEND.
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile
      integer chatter,specext,tothd
c       
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c infile       char : Input file
c outfile      char : Output filename
c spectext     int  : Number of SPECTRUM extension
c tothd        int  : Total number of extensions
c chatter      int  : Chattiness flag, >20 verbose
c
c --- CALLED ROUTINES ------------------------------------------------
c
c CFAPPEND          : (FTOOL) Appends extension, standard FTOOL with
c                     minor modifications.
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 March 31)
c Rehana Yusaf (1993 August 6) : (1.0.1) Add chatter to cfappend
c Rehana Yusaf (1995 Dec 14) 1.0.2 ; add wtinfo and friends
c
c Banashree Mitra Seifert (1996 Sept 4)1.1.0: 
c         . ap_in (the infile) was character(70)
c           it is made character(180)
c Peter D Wilson (1998 June 23)1.1.1:
c         . Because extension numbers are now normally *not* stripped
c           from file names, call ftrtnm to do this explicitly
c -------------------------------------------------------------------
      character(5) version 
      parameter (version = '1.1.1')
*-
c --------------------------------------------------------------------
c
c
c --- INTERNALS ------------------------------------------------------
c
      character(7) subname
      parameter (subname='wt_copy')
      character(180) ap_in, rootname
      character(70) subinfo
      character(2) cnum
      integer i,status,befspec,iend
      integer inum
      logical pkey,hist
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)
c
c --- COPY ALL EXTENSIONS EXCEPT SPECTRUM EXTENSION ---
c
c --- COPY EXTENSIONS BEFORE SPECEXT ---
c
c PDW 6/23/98: Add call to ftrtnm
      status = 0
      call ftrtnm(infile,rootname,status)
      befspec = specext - 1
C     iend=index(infile(1:),' ') - 1
      iend=index(rootname(1:),' ') - 1
      pkey=.false.
      hist = .false.
      if (befspec .ge. 1) then
      do i=1,befspec
        IF (i.LT.10) THEN
          inum = 1
          write(cnum(1:inum),100) i
        ELSE
          inum = 2
          write(cnum(1:inum),200) i
        ENDIF
C       ap_in=infile(1:iend)//'['//cnum(1:inum)//']'
        ap_in=rootname(1:iend)//'['//cnum(1:inum)//']'
C       status = 0

        call cfappend(ap_in,outfile,pkey,hist,chatter)
      enddo
      endif
c
c --- COPY EXTENSIONS AFTER SPECEXT ---
c
      do i=specext+1,tothd
        IF (i.LT.10) THEN
          inum = 1
          write(cnum(1:inum),100) i
        ELSE
          inum = 2
          write(cnum(1:inum),200) i
        ENDIF
C       ap_in=infile(1:iend)//'['//cnum(1:inum)//']'
        ap_in=rootname(1:iend)//'['//cnum(1:inum)//']'
C       status = 0
        call cfappend(ap_in,outfile,pkey,hist,chatter)
      enddo
  99  continue
 100  format(I1)
 200  format(I2)
       end
c -------------------------------------------------------------------
c     END OF WT_COPY
c -------------------------------------------------------------------

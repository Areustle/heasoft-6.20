
*+GETKEYS
c     ------------------------------------------------- 
      subroutine getkeys(iunit,nkeys,keys,chatter,ierr)
c     -------------------------------------------------
c --- DESCRIPTION -------------------------------------------------------
c This subroutine reads all the keywords in an extension  and returns
c an array of keywords, this is usefull for other routines as the array
c can be used to ensure no duplication of keywords.
c NOTE !!! HISTORY AND COMMENT keyword names are NOT included
c --- VARIABLES ---------------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,nkeys,chatter,ierr
      character(8) keys(*)
c
c --- VARIABLE DIRECTORY ------------------------------------------------
c
c iunit      int  i : file unit number
c chatter    int  i : Verboseness flag
c ierr       int  i : error flag, 0 is okay
c keys       char o : array of keyword names
c
c --- CALLED ROUTINES ---------------------------------------------------
c
c FTGHSP     : (FTOOLS/FITSIO) Gets numbers of keywords
c FTGREC     : (FTOOLS/FITSIO) Reads record
c WT_FERRMSG : (FTOOLS/CALLIB) Writes approriate error message
c
c --- COMPILATION/LINKING -----------------------------------------------
c
c FTOOLS/FITSIO, FTOOLS/CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------------
c
c Rehana Yusaf (1993 Oct 27) 1.0.0;
c Rehana Yusaf (1995 Dec 14) 1.0.1; add wtinfo and friends
      character(6) version
      parameter (version='1.0.1')
*-
c -----------------------------------------------------------------------
c
c Internals
c 
      character(7) subname
      parameter (subname='getkeys')
      character(70) desc,errinfo
      character(80) rec
      integer nmore,i,tot_keys,status
c
c --- USER INFO ---
c
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,20,2,desc)

c --- Get counter for number of keywords ---

      status = 0
      call ftghsp(iunit,tot_keys,nmore,status)

c --- Read each record and store keyword names ---

      nkeys = 0
      do i=1, tot_keys
        status = 0
        call ftgrec(iunit,i,rec,status)
        IF ((status.NE.0).AND.(chatter.GE.5)) THEN
          errinfo = ' reading Record'
          call wtferr(subname,version,status,errinfo)
          errinfo = 'Rec :'//rec
          call wtinfo(chatter,0,1,errinfo)
        ENDIF
        IF ((rec(1:8).NE.'HISTORY').AND.
     &    (rec(1:8).NE.'COMMENT')) THEN
          nkeys = nkeys + 1
          keys(nkeys) = rec(1:8)
        ENDIF
      enddo
      return
      end
c -----------------------------------------------------------------
c     END OF GETKEYS
c -----------------------------------------------------------------

*+CK_FILE
c     ------------------------------------------------------- 
      subroutine ck_file(ofile,ill_files,n_ill,valfil,
     &                   clobber,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c This subroutine checks the validity of a user defined output filename.
c If the filename is valid, the logical valfil is returned as true.
c There are two forms of filename :
c filename : If it already exists error message is displayed and valfil
c            is returned as false.
c !filename : Allows overwrite, with the exception of illegal filenames, 
c             in this case permission is denied. The file of this name is
c             deleted and then a new file of this name can be written.
c *** UNIX/ULTRIX ***
c Comparison between ill_files (illegal files), and ofile is case sensitive
c *** ----------- ***
c --- VARIABLES ---------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) ofile,ill_files(*)
      integer chatter,n_ill
      logical valfil,clobber
c
c --- VARIABLE DIRECTORY ------------------------------------------------
c
c Arguments ...
c
c ofile     char    : output filename
c infile    char    : input filename
c chatter   int     : chattiness flag, if >20 verbose
c valfil    logical : true if valid output filename
c clobber   logical : true/false, if true then file is clobbered 
c                     (except ill_files)
c
c Called Routines
c  subroutine CRMVLBK		:(CALLIB) removes leading blanks from string
c  subroutine RMVEXSP		:(CALLIB) removes extra spaces from string 
c  subroutine WTERRM		:(CALLIB) writes error message to STDOUT
c  subroutine WTINFO		:(CALLIB) writes info message to STDOUT
c 
c --- AUTHORS/MODIFICATION HISTORY --------------------------------------
c
c Rehana Yusaf (1993 June 21) 
c Rehana Yusaf (1993 August 2) 1.0.1: Routine is made general, an array of
c                              illegal filenames is passed.
c Ian M George (93 Aug 09) 1.0.2: minor cosmetics, comm'd out 1 msg to screen
c Rehana Yusaf (93 Oct 12) 1.0.3; Use clenact to get actual filename length
c Rehana Yusaf (1994 Sept 12) 1.0.4; add clobber argument, and omit delfil
c Rehana Yusaf (1995 April 28) 1.0.5; increase curfile from 80 to 160
c Ian M George (1995 Nov 29) 1.0.6; Added wtinfo & friends
c Ning Gan (2000 Mar 20) 1.0.7; Increase the filename length to 255.  
      character(5) version
      parameter (version = '1.0.7')
*-
c -----------------------------------------------------------------------
c
c
c --- INTERNALS ---------------------------------------------------------
c
      character(7) subname
      parameter (subname = 'ck_file')
      character(255) curfile,desc,checkfile
      integer i,flen,clenact
      logical ext
c
c --- USER INFO ---
c
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,15,2,desc)

c 
c --- Main bit
c

      call crmvlbk(ofile)
      curfile = '  '
      call rmvexsp(ofile,curfile)
      valfil = .true.
      flen = clenact(curfile)
c
c --- !FILENAME CASE ---
c
      IF ((curfile(1:1).EQ.'!').OR.(clobber)) THEN
       IF (curfile(1:1).EQ.'!') THEN
         curfile(1:) = curfile(2:)
       ENDIF
        call crmvlbk(curfile)  
        INQUIRE(FILE=curfile,EXIST=ext)
 
c --- CURFILE = ILL_FILES NOT ALLOWED ---

        IF (ext) THEN
          flen = flen - 1 
          do i=1,n_ill
           checkfile = ill_files(i)
           IF (curfile(1:flen).EQ.checkfile(1:flen)) THEN
            desc='Filename is illegal'
	    call wterrm(subname, version,desc)
            desc = 'offending file : '//checkfile
	    call wtinfo(chatter,0,1,desc)
            valfil = .false.
           ENDIF
          enddo
        ENDIF
c
c --- FILENAME CASE ---
c
      ELSE
        INQUIRE(FILE=curfile,EXIST=ext)
        IF (ext) THEN
          do i=1,n_ill
           checkfile = ill_files(i)
           IF (curfile(1:flen).EQ.checkfile(1:flen)) THEN
            desc='Filename is illegal'
            call wterrm(subname, version, desc)
            desc = 'offending file : '//checkfile
            call wtinfo(chatter,0,1,desc)
            valfil = .false.
           ENDIF
          enddo    
          IF (valfil) THEN
            desc='File already exists, not overwritten'
            call wterrm(subname, version,desc) 
     	    desc = 'Overwrite may be forced by using'//
     &		' syntax !FILENAME '
	    call wtinfo(chatter,0,1, desc)
            valfil = .false.
          ENDIF
        ENDIF
      ENDIF
      return
      end
c ------------------------------------------------------------------------
c     END OF CK_FILE
c ------------------------------------------------------------------------


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
c *** NOTE *** This routine is VMS specific.
c VMS filenames are not case sensitive, thus the comparison between 
c ill_files (illegal filenames) and the output file is not case sensitive.
c *** ---- ***
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
c ill_files char    : illegal filenames
c n_ill     char    : No. of illegal filenames
c chatter   int     : chattiness flag, if >20 verbose
c valfil    logical : true if valid output filename
c clobber   logical : true/false, if true then clobber file, except ill_files
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------------
c
c Rehana Yusaf (1993 June 21) 
c Rehana Yusaf (1993 August 2) 1.0.1: Routine is made general, an array of
c                              illegal filenames is passed.
c Ian M George (1993 August 09) 1.0.2: VMS version does not return error,
c				just warns file exists
c Rehana Yusaf (1993 Oct 12) 1.0.3; Use clenact to get actual file length
c Rehana Yusaf (1994 Sept 12) 1.0.4; add clobber parameter, and omit delfil
c
      character(5) version
      parameter (version = '1.0.4')
*-
c -----------------------------------------------------------------------
c
c
c --- INTERNALS ---------------------------------------------------------
c
      character(80) curfile,desc,checkfile
      character(30) errstr, wrnstr
      integer i,flen,clenact
      logical ext

c --- USER INFO ---
c
      errstr = '** CK_FILE '//version//' ERROR: '
      wrnstr = '** CK_FILE '//version//' WARNING: '

      IF (chatter.GE.15) THEN
        desc = ' ... using CK_FILE Ver '//version
	call fcecho(desc)
      ENDIF
      call crmvlbk(ofile)
      curfile = '   '
      call rmvexsp(ofile,curfile)
      call ftupch(curfile)
      do i=1,n_ill
        call ftupch(ill_files(i))
      enddo
      flen = clenact(curfile)
      valfil = .true.
c
c --- !FILENAME CASE ---
c
      IF ((curfile(1:1).EQ.'!').OR.(clobber)) THEN
        IF (curfile(1:1).EQ.'!') THEN
          curfile(1:) = curfile(2:)
        ENDIF
        flen = flen - 1
        call crmvlbk(curfile)
        INQUIRE(FILE=curfile,EXIST=ext)

c --- CURFILE = ILL_FILES NOT ALLOWED ---

        IF (ext) THEN
          do i=1,n_ill
           checkfile = ill_files(i)
           IF (curfile(1:flen).EQ.checkfile(1:flen)) THEN
            desc = errstr//'Filename is illegal'
            call fcecho(desc)
            desc = '... FILE : '//checkfile
            call fcecho(desc)
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
            desc = errstr//'Filename is illegal'
            call fcecho(desc)
            desc = '... FILE : '//checkfile(1:flen)
            call fcecho(desc)
            valfil = .false.
           ENDIF
          enddo    
          IF (valfil) THEN
           IF (chatter.GE.5) THEN
            desc = wrnstr//'File already exists '
            call fcecho(desc)                   
            desc = ' ... new version maybe written of file : '//curfile
            call fcecho(desc)
           ENDIF
           valfil = .true.
          ENDIF
        ENDIF
      ENDIF
      return
      end
c ------------------------------------------------------------------------
c     END OF CK_FILE
c ------------------------------------------------------------------------


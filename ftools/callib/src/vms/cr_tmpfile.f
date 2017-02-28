*+CR_TMPFILE
c     ---------------------------------------------------------
      subroutine cr_tmpfile(infile,killit,outfile,tfile,tname,
     &                      create,errflg,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This subroutine creates a temporary file called tnameX.tmp where
c X is a number between 1 and 99, ie if tname1.tmp exists then
c tname2.tmp is used. It then copies infile to tfile (tname2.tmp)
c VMS specific, there is a unix version of this code  
c
c --- VARIABLES ---------------------------------------------------
      IMPLICIT NONE
      character*(*) infile,outfile,tfile,tname
      logical killit,create
      integer chatter,errflg
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c Rehana Yusaf (Jan 29 1995) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')
*-
c -----------------------------------------------------------------
c --- INTERNALS
      character(70) desc
      character(28) errstr
      integer i
      logical ext
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        desc = ' ... using CR_TMPFILE Ver '//version//':'
        call fcecho(desc)
      ENDIF
c
c --- CREATE TEMPORARY FILE ---
c
      errstr = ' ERROR: CR_TMPFILE Ver '//version//':'
      ext = .false.
      create = .false.
      errflg = 0
      i = 1
      call crmvlbk(outfile)
        do WHILE ((.NOT.create).AND.(i.LE.99))
          IF (i.lt.10) THEN
            write(tfile,'(a,i1,a)') tname,i,'.tmp'
          ELSE
            write(tfile,'(a,i2,a)') tname,i,'.tmp'
          ENDIF
          call ftupch(tfile)
          INQUIRE(FILE=tfile,EXIST=ext)
          IF (.NOT.ext) THEN
            create = .true.
          ENDIF
          i = i+1
        enddo
      IF (create) THEN
        call copy(infile,tfile,chatter,errflg)
        IF (errflg.NE.0) THEN
          create = .false.
        ENDIF
      ENDIF
      IF (.NOT.create) THEN
        tfile = ' '
        errflg = 1
        desc = errstr//' creating temp copy of infile'
        call fcecho(desc)
      ENDIF
      return
      end
c ----------------------------------------------------------------
c     END OF CR_TMFILE
c ---------------------------------------------------------------- 
        
      

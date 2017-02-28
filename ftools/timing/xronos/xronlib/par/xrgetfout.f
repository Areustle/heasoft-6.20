C***************************************************************
C
C subroutine:  xrgetfout.f
C              get output filename
C
C written by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      3/13/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      many of the ipf and iflags values should be booleans also see
C      notes in xronos.inc
c
C      this routine is to replace xrgetfout calls
C
C      use .qxxx for the default extension where xxx is the name of the
C      ftool (passed in calling sequence).  If FITS file is written, should
C      be .fxxx (or some such thing)
C
C calling sequence:
C      call xrgetfout (cfile, cpf, cpro,  nintv, cfilo, 
C     $    oftype, status)
C
C variables:
C
C      cfile - string array - input - the input filename
C      cpf - string array - input - string parameters
C      cpro - string - input - the name of the program calling xrgetfout
C      ptype - string - input - the "type" of the calling program
C      nintv - integer - input - number of expected intervals
C      cfilo - string - output - the name of the output file
C      status - integer - output - status of operation
C
C***************************************************************
c
      SUBROUTINE xrgetfout(cfile, cpf, cpro, nintv, 
     $     cfilo, oftype, status)
      implicit none
c
      include '../include/io.inc'
      include '../include/xronos.inc'

      character*(*) cpro
      character(160) outfile,tfile
      character(5) cext
      integer istart, istop, i
      integer lenact, oftype_def
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetfout:')
      cext=' '
c
      if (status .ne. 0) return

      outfile = ' '
C get the outfilename
      call uclgst ('outfile', outfile, status)
      if (status .ne. 0) then
         context = ' Error reading requsted outfilename'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif

      if(index(outfile,'/').ne.0.or.index(outfile,'.').ne.0) then
c if the user inputs an output file name  containing a '/' or a '.'
c do not modify
         cfilo = outfile
      else
C check for "default" or blank to use default value given in outfileroot
         cfilo = outfile
         call upc(cfilo)
C     use the value in out file unless file is ' ' (meaning don't write output)
         IF ((outfile.NE.'default').AND.(cfilo(1:7).NE.'DEFAULT')) THEN
            IF(cfilo.NE.' ') cfilo = outfile
         ELSE
C     use the default filename from outfileroot = default
            cfilo = ' '
            IF (cpf(3).EQ.'default' .OR. cpf(3).EQ.'DEFAULT' .OR. cpf(3)
     &           .EQ.'defaultdir' .OR. cpf(3).EQ.'DEFAULTDIR') THEN
               
c     get filename of first infile (used if cpf(3)='default')
               cfilo = cfile(1,1)
c     otherwise use whatever is in outfileroot 
               
            ELSE
               cfilo = cpf(3)
            ENDIF
         ENDIF
c     remove "directory" part of the filename
         IF(cfilo.NE.' ')THEN 
C     find the last slash (Unix) or ] (VMS)
            CALL xrslash (cfilo, istart)
            istart=istart+1
C     find the last .
            DO 200 i = lenact(cfilo), istart, -1
               IF (cfilo(i:i) .EQ. '.') THEN
                  istop = i-1
                  goto 201
               ENDIF
 200        CONTINUE
C     if we are here, there was no . in the filename
            istop = lenact(cfilo)
 201        CONTINUE
c     
c     prepare d/f filename extension
            cext(1:2) = '.f'
            cext(3:4) = cpro(1:2)
c     add the extension to cfilo
            cfilo=cfilo(istart:istop)//cext
         ENDIF
      endif
c high chatter      
      context=' Using output file name: '//cfilo
      call xwrite(context,15)
c      
      IF(cfilo.ne.' ') then
         if(cpro(1:2).eq.'lc'.or.nintv.eq.1) then
            oftype_def = 2
         else
            oftype_def = 1
         endif
 
c  Since outfiletype is currently a hidden parameter, we comment these out
c         call xwrite(' ',10)
c         call xwrite(' Two output file types are available.',10)
c         call xwrite(' Type 1:   1 interval per FITS table row',10)
c         call xwrite(' Type 2:   1 interval per FITS extension',10)

c         call xwrite(' ',5)

c         write(context,'('' Default Output file type is: '',i1)')
c     $        ,oftype_def
c         call xwrite(context,5)
c         call xwrite(' Type INDEF to accept the default value',10)
         
c         call xwrite(' ',5)
         
C     get output file type (1-"columns", 2-"vectors" see manual)
         call uclgsi('outfiletype',oftype,status)
         
         if(status.eq.3) then
            status=0
            oftype = oftype_def
            call uclpsi('outfiletype',oftype,status)
         endif
         
         if(status.ne.0) then
            context= 'Couldn''t get OFTYPE parameter'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif      
      endif
c
c
 999  RETURN
      END
c
c
c

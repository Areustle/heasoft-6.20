
*+COPY
c     -------------------------------------------- 
      subroutine copy(infile,outfile,chatter,ierr)
c     --------------------------------------------
c --- DESCRIPTION ----------------------------------------------
c This system specific routine copys one infile to outfile
c UNIX version spawns 'cp infile outfile'
c VMS version spawns 'copy infile outfile'
c This is the UNIX version
c --------------------------------------------------------------
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (1994 Nov); 1.0.0
      character(5) version
      parameter (version = '1.0.0')
*-
c
c --------------------------------------------------------------
c --- VARIABLES ---

      character*(*) infile,outfile
      integer ierr,chatter
      character(200) str
      character(70) desc
      integer fcstln,errflg,length

c --- USER INFO ---

      IF (chatter.GE.15) THEN
        desc = ' ... using COPY Ver' //version
        call fcecho(desc)
      ENDIF
       
c --- SPAWN TO SYSTEM ---

      str = 'cp '//infile(:fcstln(infile))//' '
     &//outfile(:fcstln(outfile)) 
      length = fcstln(str)
      call cspawn(str,length,errflg)
      return
      end
c ---------------------------------------------------------------------
c     END OF COPY
c ---------------------------------------------------------------------

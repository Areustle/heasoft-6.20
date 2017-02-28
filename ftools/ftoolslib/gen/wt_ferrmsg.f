
*+WT_FERRMSG
      subroutine wt_ferrmsg(status,errstr)
c     ------------------------------------
c --- DESCRIPTION -------------------------------------------------
c
c This routine checks the error flag obtained from FITSIO routines
c and if appropriate uses FTGERR to get the relevant error text.
c Fcecho is used to write to the screen to ensure that the program
c can be used in differant environments.
c
c --- VARIABLES ---------------------------------------------------
c
      character*(*) errstr
      character(80) errmess
      integer status
c
c --- LINKING AND COMPILATION ---
c
c FTOOLS and FITSIO
c 
c --- CALLED ROUTINES ---
c
c subroutine FTGERR     : (FITSIO) Obtains appropriate error text
c subroutine FCECHO     : (FTOOLS) Writes to screen
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
      character(5) version
      parameter (version = '1.0.2')
c
c Rehana Yusaf - original 1.0.0 March 1993;
c Rehana Yusaf (1995 Aug 29) 1.0.2; used character*(*) for errstr instead
c                                   of character(80)
*-  
       IF (status.NE.0) THEN
         call ftgerr(status,errmess)
         call fcecho(errmess)
         call fcecho(errstr)
       ENDIF
       return
       end
c
c      <<<--- END OF SUBROUTINE WT_FERRMSG --->>>
c
                                               
 

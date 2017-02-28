
*+PG_FCECHO
c     ------------------------------------------------ 
      subroutine pg_fcecho(context,page,ln_cnt,status)
c     ------------------------------------------------ 
c --- DESCRIPTION -----------------------------------------------
c
c This routine send a message to the terminal, and pages the
c message if page==yes. Using a pagelength of 23
c
c --- VARIABLES -------------------------------------------------
c
      IMPLICIT NONE
      character*(*) context
      logical page
      integer status,ln_cnt
c
c --- VARIABLE DIRECTORY ---------------------------------------- 
c
c context    char   : message to be sent to screen
c page       logical: If "yes" then output is paged by using more
c ln_cnt     int    : Counter for number of lines displayed to screen
c status     int    : Error status flag, 0 is okay
c
c --- CALLED ROUTINES -------------------------------------------
c
c UCLGSB     : (HOST)   Gets boolean input 
c FCECHO     : (FTOOLS) Screen write
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------
c
c Rehana Yusaf (1994 April 14) 1.0.0; Original
c
      character(5) version
      parameter (version ='1.0.0')
*-
c ---------------------------------------------------------------
c
c --- LOCALS ---
c
      logical more

      call fcecho(context) 
      ln_cnt = ln_cnt + 1
      IF ((page).AND.(ln_cnt.GE.23)) THEN
        status = 0
        call uclgsb('more',more,status)
        IF (.NOT.more) THEN
          call uclpsb('more',.true.,status)
          status = 1
        ENDIF
        ln_cnt = 0
      ENDIF
      return
      end
c ----------------------------------------------------------------
c     END OF PG_FCECHO
c ---------------------------------------------------------------- 

 


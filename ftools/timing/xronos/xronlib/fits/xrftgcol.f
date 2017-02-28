c
      subroutine xrftgcol(nfield,ttype,tunit,ivect)
      implicit none
c
c Get XRonos COLumns to be read, based on FiTs TTYPEnnn keywords.

c This routine searches the column header keywords to find a match
c with various strings.  The columns it searches for are:

c   Quantity               Search string             Ivect index
c----------------------------------------------------------------
c   Time                   'TIME'                         1
c   Rate                   'RATE' or 'COUNT'              2
c   Error                  any substring 'ERR'            3
c   Dead time              'DEADC'                        4
c   Integration time       'TIMEDEL'                      5
c   Fractional exposure    'FRACEXP'                      6
c   GTI start              any substring 'START'          7
c   GTI stop               any substring 'STOP'           8
c   PHA                    'PHA'                          9

c As an added requirement on the (unitless) fractional exposure the routine 
c checks that the value of the TUNIT keyword for that column is blank, = 'none'
c or does not exist.

c In xronos these defaults can be overridden by the options VX (time)
c VY (rate), VS (error), VE (dead time), VC (PHA).  The special option
c VE0 will cause xronos to ignor the GITS.
c
c   I  nfield   (i) = Total number of columns in the FITS extension.
c   I  ttype(n) (c) = The name of column n.
c   I  tunit(n) (c) = The units of column n.
c   O  ivect    (i) = The column numbers for TIME (1), RATE (2), ERROR (3),
c                      DEADTIME (4), DELTA TIME (5), EXPOSURE (6)
c                    , GTI START (7), GTI STOP (8), PHA (9)

c Author: Eric Lufkin, HEASARC/GSFC, September, 1993
c Revised:                           November, 1993 to add column numbers for
c                                    GTI and EXPOSURE extensions.
c
c NOTE do not initialize ivect in this routine but instead should be done 
c in the calling routine. vector position for colum in gti and rate or event 
c table are in different extension. The ivect(n)=0  within this routine 
c zero the column alredy found. 
c

      character(16) ttype(*),comm,tunit(*)
      integer nfield,ivect(*),n
c      do n=1,9
c         ivect(n) = 0
c      enddo

      DO n = nfield, 1, -1

c Make sure keyword strings are in upper case.

         comm = ttype(n)
         CALL rmvlbk(comm)
         CALL upc(comm)

c Look for various column headers.

         IF   (comm(1:  7).eq.'TIME   ')  ivect(1) = n
         IF  ((comm(1:  4).eq.'RATE'   )  
     &    .or.(comm(1:  5).eq.'COUNT'  )) ivect(2) = n
         IF   (comm(1:  5).eq.'DEADC'  )  ivect(4) = n
         IF   (comm(1:  7).eq.'TIMEDEL')  ivect(5) = n
         IF   (comm(1:  7).eq.'FRACEXP')  ivect(6) = n
         IF  ((comm(1:  3).eq.'PHA'    )
     &    .and.(comm(1: 4).ne.'PHAS'))    ivect(9) = n
c Look for any part of the string that says "ERR".

         IF(index(comm,'ERR').gt.0)   ivect(3) = n

c Look for gti column headers.

         IF(index(comm,'START').gt.0) ivect(7) = n
         IF(index(comm,'STOP').gt.0)  ivect(8) = n

      ENDDO

c Make sure any FRACEXP column is truly a fractional exposure.
c This will be overridden by iopt.

      IF(ivect(6).gt.0) THEN

c >>> Might want a call to xwarn here.<<<

         comm = tunit(ivect(6))
         CALL upc(comm)
         CALL rmvlbk(comm)
         IF((comm.ne.'NONE').and.(comm.ne.' ')) ivect(6) = 0
      ENDIF

      return
      end

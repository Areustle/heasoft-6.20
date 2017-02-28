*+CALCRT
        subroutine calcrt(date,time,reftime,status)

        implicit none
        character(10) date
        character(8) time
        double precision reftime
        integer status

C-----------------------------------------------------------------------
C Description: Returns the Modified Julian Day added to the fraction of
C              the day that has elapsed for the time and date given.
C
C Arguments:   date    (i) : the UTC date in dd/mm/yy format
C              time    (i) : the UTC time in hh:mm:ss format
C              reftime (r) : the MJD number plus fraction of day
C              status  (r) : the success value of this routine
C                            OK = zero
C                            Not OK = non-zero
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C              Ron Zellar Sept 29, 1993 -- Original version
*- Version 1.0
C-----------------------------------------------------------------------

C              Lorraine Breedon 23 Jun 1998 - modifications for y2k 
C                                             problem
C version 1.1
C-----------------------------------------------------------------------


        integer iy,imon,id,errstat,ih,im
        double precision mjd,frac,dble,is

C       initialize variables
        status = 0
        iy = 0
        imon = 0
        id = 0
        ih = 0
        im = 0
        is = 0
        errstat=0

C       Parse the date into day month and year
        call fts2dt(date,iy,imon,id,errstat)
        if (errstat .ne. 0) goto 999

C       get the modified julian day
                call ccaldj(iy,imon,id,mjd,errstat)
        if (errstat .ne. 0) goto 999

C       Parse the time into hours, minuets, and seconds
        call fts2tm(time,iy,imon,id,ih,im,is,errstat)
        if (errstat .ne. 0) goto 999

C       Find the fraction of the day that has elapsed
        frac = DBLE((ih*60*60)+(im*60)+is)/86400

C       return the mjd + the fraction of day
        reftime = mjd + frac


        return

999     continue
        status = 1
        return

        end

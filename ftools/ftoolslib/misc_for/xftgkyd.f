
C**************************************************************************
        subroutine xftgkyd(iunit,keywrd,dval,comm,status)

C       read a double precision value and comment string from a header record
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       OUTPUT PARAMETERS:
C       dval    i  output keyword value
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       based on FTGKYD by Wm Pence, HEASARC/GSFC, June 1991
C
C       modified by M. Tripicco to handle keywords which have been
C       split into two (i.e., integer and fractional parts)
C       and renamed XFTGKYD     April 1996

        character*(*) keywrd,comm
        character(80) keywrdi,keywrdf,commi,commf
        integer iunit,status,oldstatus,fcstln
        character value*35,valuei*35,valuef*35
        double precision dval,dvali,dvalf
        integer clen

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)
C       if not found, try finding pair of I/F keywords instead
        if (status .ne. 0) then
          oldstatus=status
          status=0
          clen=fcstln(keywrd)
          if (clen .lt. 8) then
            keywrdi=keywrd(1:clen)//'I'
            keywrdf=keywrd(1:clen)//'F'
          endif
          if (clen .ge. 8) then
            keywrdi=keywrd(1:7)//'I'
            keywrdf=keywrd(1:7)//'F'
          endif
          call ftgkey(iunit,keywrdi,valuei,commi,status)
          call ftgkey(iunit,keywrdf,valuef,commf,status)
          if (status .ne. 0) status=oldstatus
        else
C     convert character string to double precision
C     datatype conversion will be performed if necessary and if possible
          call ftc2d(value,dval,status)
          return
        endif

C       convert strings to doubles (see above) and sum
        call ftc2d(valuei,dvali,status)
        call ftc2d(valuef,dvalf,status)
        dval=dvali+dvalf
C       construct and return an entirely new comment
        comm='Derived from '//keywrdi(1:fcstln(keywrdi))//
     &     ' and '//keywrdf(1:fcstln(keywrdf))

        end


*+PAR_DIM
c     --------------------------------------------
      subroutine par_dim(dimstr,ndims,indexs,ierr)
c     --------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c This subroutine is a general parser for the FITSIO TDIM keyword.
c An example TDIM keyword -
c TDIM1 = (nrad,ntheta,nenerg) for a 3D column, i.e TDIM1 = (20,1,1)
c This is stored as a character string, and this routine extracts all
c the dimension values.
c --------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      integer ndims,indexs(*),ierr
      character*(*) dimstr
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c ndims     int    o : Number of dimensions
c dimstr    char   i : TDIM string
c indexs    int    o : Array of dimension values 
c ierr      int    o : Error flag, 0 is okay
c
c --- CALLED ROUTINES ------------------------------------------------
c
c FCSTLN      : Fortran library routine, gives string length
c INDEX       : Fortran library routine, gives character position 
c               in a string
c CRMVLBK     : Removes leading blanks
c 
c --- AUTHORS/MODIFIACTION HISTORY ----------------------------------
c
c Rehana Yusaf 1.0.0 ; Sept 20 1993
c
       character(5) version
       parameter (version = '1.0.0')
*-
c --------------------------------------------------------------------
c
c --- INTERNALS ---
c
      character(30) errstr
      character(70) errinfo
      integer com_pos, end_str,st_str,st_num,end_num
      integer str_len, fcstln, curnum, charerr
      logical endstr 
     
      errstr = ' ERROR:PAR_DIM Ver'//version//':'
      call crmvlbk(dimstr)
      str_len = fcstln(dimstr)
      end_str = index(dimstr(1:),')')
      st_str = index(dimstr(1:),'(')

c SOME SYNTAX CHECKS ...

      IF (st_str.NE.1) THEN
       ierr = 1
       errinfo = errstr //' Invalid syntax for TDIM string !'
       call fcecho(errinfo)
       return
      ENDIF
      IF (end_str.LT.3) THEN
       ierr = 2
       errinfo = errstr //' Invalid syntax for TDIM string !'
       call fcecho(errinfo)
       return 
      ENDIF

c EXTRACT EACH DIMENSION ...

      endstr = .false.
      st_num = 2
      ndims = 0
      do WHILE(.NOT.endstr)
        com_pos = index(dimstr(st_num:),',')
        IF (com_pos.EQ.0) THEN
          end_num = end_str - 1
          endstr = .true.
        ELSE
          end_num = st_num + com_pos - 2
        ENDIF
        read(dimstr(st_num:end_num),*,IOSTAT=charerr) curnum
        IF (charerr.EQ.0) THEN
          ndims = ndims + 1
          indexs(ndims) = curnum
        ELSE
          errinfo = errstr//' parsing no :'
     &//dimstr(st_num:end_num)
          call fcecho(errinfo)
          ierr = 3
          return
        ENDIF
        st_num = end_num+2
      enddo
      return
      END
c ------------------------------------------------------------------
c      END OF PAR_DIM
c ------------------------------------------------------------------      
       



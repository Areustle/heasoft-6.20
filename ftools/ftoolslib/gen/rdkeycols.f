*+RDKEYCOLS
c     ---------------------------------------------------------------
      subroutine rdkeycols(iunit,ttype,ncol,nrows,name,defval,
     &                     colvals,qcol,errflg,chatter)
c     ---------------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c This subroutine assumes that a FITS file is open, at the desired
c extension. It  determines if  a character column of a given name 
c exists, if it  does then the  values are read into colvals. If a 
c column does not exist then it is determined if a keyword of that 
c name exists.If a keyword exists then it's value is read, and the 
c array colvals is set to this value.
c Error status , errflg = 1, column and  keyword do not exist, set
c                            colvals array to given 
c                            default value (defval).
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) ttype(*),name,colvals(*),defval
      integer iunit,errflg,chatter,nrows,ncol
      logical qcol
c 
c --- VARIABLE DIRECTORY --------------------------------------------
c
c ttype   (i) character array : labels for table columns
c ncol    (i) integer         : Number of columns
c colvals (o) character array : Used to read column (keyword) values
c name    (i) character       : column name
c nrows   (i) integer         : Number of rows in extension
c errflg  (o) integer         : Error status
c                               0 okay,
c                               1 column and keyword not found
c chatter (i) integer         : Flag for screen display
c iunit   (i) integer         : lun
c qcol    (o) logical         : true if column
c
c --- MODIFICATION HISTORY ------------------------------------------
c
c                          Rehana Yusaf (May 16 1995) 1.0.0   
      character(5) version
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c
c INTERNALS
      integer status ,i,colno,fcstln,len
      character(7) snull
      character(30) errstr,comm
      character(70) errinfo
      logical anyflg
c
c --- DETERMINE COLUMN CALLED NAME EXISTS ---
c
      snull = ' '
      errstr = ' ERROR: RDKEYCOLS Ver '//version//':'
      len = fcstln(errstr)
      qcol = .false.
      errflg = 0
      do i=1,ncol
        IF (ttype(i).EQ.name) THEN
          qcol = .true.
        ENDIF
      enddo
c
c --- READ COLUMN/KEYWORD ---
c
      IF (qcol) THEN
        status = 0
        call ftgcno(iunit,.false.,name,colno,status)
        errinfo = errstr(:len)//' finding '//name//'column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          do i=1,nrows
             colvals(i) = defval
          enddo
        ELSE
          status = 0
          call ftgcvs(iunit,colno,1,1,nrows,snull,colvals,
     &                anyflg,status)
          errinfo = errstr(:len)//' reading '//name//' column'
          call wt_ferrmsg(status,errinfo)
        ENDIF
      ELSE
        status = 0
        call ftgkys(iunit,name,colvals(1),comm,status)
        errinfo = errstr(:len)//' reading '//name//' keyword'
        IF (chatter.GE.10) THEN
          call wt_ferrmsg(status,errinfo)
        ENDIF
        IF (status.NE.0) THEN
          errflg = 1
          colvals(1) = defval
        ENDIF
        do i=2,nrows
           colvals(i) = colvals(1)
        enddo
      ENDIF
      return
      end
c ----------------------------------------------------------------
c       END OF RDKEYCOLS
c ----------------------------------------------------------------
          

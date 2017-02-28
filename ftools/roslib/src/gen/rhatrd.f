*+RHATRD
c     -------------------------------------------------------------
      subroutine rhatrd(iunit,n_att,max_att,time,ra_sc,
     &           dec_sc,roan_sc,nom_ra,nom_dec,
     &           nom_roll,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads an HRI FITS RDF format Attitude (ASPECT) extension
c NOTE : Assumes file is already open  at the desired extension.
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME       : Time of corected aspect in full seconds
c ROAN_SC    : Roll angle of SC pointing
c RA_SC      : RA of SC pointing
c DEC_SC     : DEC of SC pointing
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_att,n_att,ierr,chatter
      real*8 time(*),roan_sc(*)
      real*8 ra_sc(*),dec_sc(*)

      real*8 nom_ra,nom_dec,nom_roll
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_att    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_att      int i/r: Counter attitude data (IF NE 0 on entry then that 
c		      many attitude entries already assumed to have been 
c		      read in [from previous extensions])
c time       int    : Array of corrected aspect
c roan_sc    int    : Array of roll angle
c ra_sc      int    : Array of RA of SC pointing
c dec_sc     int    : Array of DEC of SC pointing
c nom_ra     real   : Nominal RA (deg)
c nom_dec    real   : Nominal DEC (deg)
c nom_roll   real   : Nominal ROLL (deg)
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 107/207 error finding extension 
c                                 ierr = 2 Column/keyword number not found
c                                 ierr = 3 Error in reading data
c                                 ierr = 4 Mandatory keyword not found
c
c --- CALLED ROUTINES -----------------------------------------------------
c
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c                          where the j, is for an integer
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c                          where a rootstring is given, thus an array of
c                          keywords can be read
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c --- COMPILATION AND LINKING ---------------------------------------------
c
c Link with FTOOLS - FITSIO, CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (Aug 30 1994) 1.0.0; rdatrd.f used as a basis
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . dimensions declaration is cleaned in various subroutines so 
c        that it carries dimensions from calling routine. So, replaced 
c        by (*) within subroutines instead of specifically mentioning 
c        the dimension
c--------------------------------------------------------------------
       character(5) version
       parameter (version = '1.1.0' )
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,colnum
      integer felem,frow
      real*8 enull
      logical anyflg,foundcol
c
c --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RHATRD Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_att,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
          write(subinfo,'(A,i12)') 
     &		'   ... Number of records found = ', n_att
          call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_att.GT.max_att) THEN
         errinfo = errstr//' MAX_ATT array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING NOMINAL VALUES FOR RA,DEC and ROLL ---
c
      status = 0
      call ftgkyd(iunit,'RA_NOM',nom_ra,comm,status)
      errinfo = errstr//' reading RA_NOM '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = status
        return
      ENDIF

      status = 0
      call ftgkyd(iunit,'DEC_NOM',nom_dec,comm,status)
      errinfo = errstr//' reading DEC_NOM '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = status
        return
      ENDIF

      status = 0
      call ftgkyd(iunit,'ROLL_NOM',nom_roll,comm,status)
      errinfo = errstr//' reading ROLL_NOM '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = status
        return
      ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND TIME COLUMN 

       foundcol=.true.
       status = 0
       colnum=0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TIME column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TIME COLUMN

       frow=1
       felem=1
       enull=0.d0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,time,
     &             anyflg,status)
       errinfo = errstr//' reading TIME column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND ROAN_SC COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'ROAN_SC',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'ROAN_SC column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING ROAN_SC COLUMN

       frow=1
       felem=1
       enull=0.d0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,roan_sc,
     &             anyflg,status)
       errinfo = errstr//' reading roan_sc column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND RA_SC COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RA_SC',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'RA_SC column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF       

c READING RA_SC COLUMN

       frow=1
       felem=1
       enull=0.d0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,ra_sc,
     &             anyflg,status)
       errinfo = errstr//' reading RA_SC column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND DEC_SC COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'DEC_SC',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'DEC_SC column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING DEC_SC COLUMN

       frow=1
       felem=1
       enull=0.d0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,dec_sc,
     &             anyflg,status)
       errinfo = errstr//' reading DEC_SC column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF          
       IF (chatter.GE.20) THEN
         subinfo = '      ... data has been read'
         call fcecho(subinfo)
       ENDIF
       return
       end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RHATRD 
c ------------------------------------------------------------------------


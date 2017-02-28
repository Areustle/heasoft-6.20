
*+RDSTRD
c     ---------------------------------------------------------------
      subroutine rdstrd(iunit,n_sta,max_sta,sta_time,hv_sta,carr_sta,
     &                  gas_sta,det_sta,temp_sta,lv_sta,inst_sta,
     &                  filt_sta,tel_sta,cur_sta,obi_num,chatter,ierr)
c     ---------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS RDF format HKSTA extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME      : Time of status change
c HV_STA    : Status of high voltage
c CARR_STA  : Carrousel status
c GAS_STA   : Gas system status
c DET_STA   : Detector status
c TEMP_STA  : Temperature status
c LV_STA    : Low voltage status
c INST_STA  : Instrument in use
c FILT_STA  : Filter wheel position
c TEL_STA   : Telemetry status
c CURR_STA  : Current status
c OBI_NUM   : OBI number
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_sta,n_sta,ierr,chatter
      real*8 sta_time(max_sta)
      integer hv_sta(max_sta),carr_sta(max_sta),det_sta(max_sta)
      integer temp_sta(max_sta),lv_sta(max_sta),gas_sta(max_sta)
      integer inst_sta(max_sta),filt_sta(max_sta),tel_sta(max_sta)
      integer obi_num(max_sta),cur_sta(max_sta)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_sta    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_sta      int    : Counter for HKSTA data 
c ierr       int    : Error flag, ierr = 0 okay
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
c Rehana Yusaf (May 17 1994) 1.0.0; 
c Peter Wilson (Dec  4 1997) 1.0.1; enull declared as real*8
c
       character(5) version
       parameter (version = '1.0.1' )
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      integer status,colnum
      integer felem,inull,frow
      real*8 enull
      logical anyflg,foundcol

c
c      --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RDSTRD Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c     --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_sta,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(subinfo,'(A,i12)')
     &  '   ... Number of records found = ',n_sta
         call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_sta.GT.max_sta) THEN
         errinfo = errstr//' array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND TIME COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TIME column not present in HKSTA ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF


c READING TIME COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_sta,enull,sta_time,
     &             anyflg,status)
       errinfo = errstr//' reading TIME column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND HV_STA COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'HV_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'HV_STA column not present in HKSTA ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING HV_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,hv_sta,
     &             anyflg,status)
       errinfo = errstr//' reading HV_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND CARR_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'CARR_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'CARR_STA column not present in HKSTA ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING CARR_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,carr_sta,
     &             anyflg,status)
       errinfo = errstr//' reading CARR_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND GAS_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'GAS_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'GAS_STA column not present in HKSTA ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF       

c READING GAS_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,gas_sta,
     &             anyflg,status)
       errinfo = errstr//' reading GAS_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND DET_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'DET_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'DET_STA column not present in HKSTA ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING DET_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,det_sta,
     &             anyflg,status)
       errinfo = errstr//' reading DET_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF          

c CHECK TO FIND TEMP_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEMP_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEMP_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TEMP_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,temp_sta,
     &             anyflg,status)
       errinfo = errstr//' reading TEMP_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                    

c CHECK TO FIND LV_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LV_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LV_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING LV_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,lv_sta,
     &             anyflg,status)
       errinfo = errstr//' reading LV_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND INST_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'INST_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'INST_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING INST_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,inst_sta,
     &             anyflg,status)
       errinfo = errstr//' reading INST_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND FILT_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'FILT_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'FILT_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING FILT_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,filt_sta,
     &             anyflg,status)
       errinfo = errstr//' reading FILT_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND TEL_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEL_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEL_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TEL_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,tel_sta,
     &             anyflg,status)
       errinfo = errstr//' reading TEL_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND CURR_STA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'CURR_STA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'CURR_STA column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF


c READING CURR_STA COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,cur_sta,
     &             anyflg,status)
       errinfo = errstr//' reading CURR_STA column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND OBI_NUM COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'OBI_NUM',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'OBI_NUM column not present in HKSTA ext '
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING OBI_NUM COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_sta,inull,obi_num,
     &             anyflg,status)
       errinfo = errstr//' reading OBI_NUM column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

       IF (chatter.GE.20) THEN
         subinfo = '      ... HKP data has been read'
         call fcecho(subinfo)
       ENDIF
       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDGSRD 
c ------------------------------------------------------------------------


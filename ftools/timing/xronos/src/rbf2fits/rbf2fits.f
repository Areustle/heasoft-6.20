      subroutine rbf2fs()
C
C
      include '../../include/io.inc'
      character(70)  rbf_name, fits_name, new_name
c      character cdate*9, ctim*8
      INTEGER       fits_unit, status, ierr
      LOGICAL       simple, extend
      integer       bitpix, naxis, naxes(2), pcount, gcount
      INTEGER type, exp, nrec, rbf_unit, blocksize, flag
      INTEGER iend, ista, iext, lenact, parse, tchat,lchat
      REAL * 8 timezero
      character(80) istring, context 
      character(255) log_fil
      data istring,parse /' ',0/  
      character(40) taskname
      common /task/ taskname
      include 'xrrbstr.h'
      include 'rbf2fits.inc'
C
      RECORD/rnewbuf_rec_1/newrec1
      RECORD/rnewbuf_rec_2/newrec2
      parameter (subname = 'rbf2fs:')

      call xrversion('rbf2fits', taskname)
c   
c get parameter filename, tchat 
      CALL rbfinit(rbf_name,fits_name,tchat,lchat, ierr)
      IF (ierr.EQ.0) THEN
         call xchaty(tchat , lchat)
         if (lchat.ne.0) then
            log_fil='+'//program(:lenact(program))//'.log'  
            call setlog(istring,parse,log_fil,' ')
         endif
c        
c
c
       Call getlun(rbf_unit)
       call openwr(rbf_unit, rbf_name, 'old','d', ' ',256,1, status)
       if (status.ne.0) then
          write(context,
     &     '(''Error '', i5,'' opening filename '', a51)')status,
     $         rbf_name
         errm = subname//' '//context
         call xaerror (errm, 10) 
         goto 500
       else 
         write(context,'('' Converting'', a69)') rbf_name
         call xwrite (context,10)
       endif
C
c Fits output file:
       status = 0
       blocksize=2880
       IF (fits_name.eq.' ') THEN 
         call dirpos(rbf_name, ista, iend)
         new_name=rbf_name(iend+1:lenact(rbf_name))
       ELSE
         new_name=fits_name
       ENDIF
       iext=index(new_name,'.')
       IF (iext.eq.0)then
            fits_name=new_name(:lenact(new_name))//'.lc'
       ELSE
            fits_name=new_name(1:(iext-1))//'.lc'
       ENDIF
c
c open output file
       call getlun(fits_unit)
       CALL FTINIT(fits_unit, fits_name, blocksize, status)
       if (status.ne.0) then
         write(context,'('' Error opening fits file '', a55)') fits_name
         errm = subname//' '//context
         call xaerror(errm,5)
         write(context,'('' FITS status: '', i4)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       endif
C
C Read the first 2 128 word blocks from rbf_unit:
       read(rbf_unit,REC=1) newrec1
       read(rbf_unit,REC=2) newrec2
       type=newrec1.type
       exp=newrec1.expt
       nrec=newrec1.nrec
C
C     Write the primary header:
       simple=.true.
       bitpix=8
       naxis=0
       pcount=0
       gcount=1
       extend=.true.
       CALL FTPHPR(fits_unit,simple,bitpix,naxis,naxes,pcount,
     &            gcount,extend,status)
       If(status.ne.0) then
         write(context,'('' ftphpr error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       endif
       CALL FTPDEF(fits_unit, bitpix, naxis, naxes, pcount, 
     &            gcount, status)
       If(status.ne.0) then
         write(context,'('' ftpdef error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       endif
c
c     Write descriptive keywords into the primary header:
c     TELESCOP
c     INSTRUME
c     DETNAM
c     FILTER
c     OBJECT
c     RA_OBJ
c     DEC_OBJ    
c     RA_PNT
c     DEC_PNT
c     ROLLANG
c     EQUINOX
c     RADECSYS    
c     DATE-OBS
c     TIME-OBS
c     DATE-END
c     TIME-END
c     ORIGIN
c     DATE
       flag=1
       CALL write_descriptive(fits_unit, newrec1, newrec2,flag,status)
       If(status.ne.0) then
         write(context,'('' write_descriptive error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       endif
       call  xwrite(' Descriptive written', 15)
       write(context,'('' Primary written! Status= '',i4)')status
       call xwrite(context,10)
c
c     Create a new header which is then defined in the def_routines:
       call xwrite (' Writing EXTENSION header...', 10)
       CALL FTCRHD(fits_unit,status)
       write(context, '('' New header created! Status = '', i4)') status
       call xwrite(context,15)
c
c      Define the structure of the binary tables support only type 2
       if (type.eq.2) then
         CALL write_deftype2(fits_unit, status)
         if(status.ne.0) then
            write(context,'('' write_deftype2 error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            goto 500
         else
            call xwrite(' Structure type2 written', 10)
         endif
       elseif (type.eq.1) then
         CALL write_deftype1(fits_unit, status)
         if(status.ne.0) then
            write(context,'('' write_deftype1 error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            goto 500
         else
            call xwrite(' Structure type1 written', 10)
         endif
       elseif (type.eq.0) then
         CALL write_deftype0(fits_unit, status)
         if(status.ne.0) then
            write(context,'('' write_deftype0 error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            goto 500
         else
            call xwrite(' Structure type0 written', 10)
         endif
       else
         call xwrite(' Unknown rates file structure!', 10)
         WRITE(context,'('' the type is '', i4)') type
         GOTO 500
       endif
C
C     Write HDUCLASS keywords into the BINARY table extension:
c
c  HDUCLASS
c  HDUCLAS1
c  HDUCLAS2
c  HDUCLAS3
c  HDUVERS
c  TIMVERSN
c  CREATOR
       CALL write_hdu(fits_unit,newrec1, newrec2,status)
       If(status.ne.0) then
         write(context,'('' write_hdu error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       else
         call xwrite(' HDUCLASS written ', 15)
       endif
C
C     Write decriptive keywords into the BINARY table extension:
       flag=2
       CALL write_descriptive(fits_unit,newrec1, newrec2,flag,status)
       If(status.ne.0) then
         write(context,'('' write_descriptive error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       else
         call xwrite(' Descriptive written in extension', 10)
       endif
C
C     Write timing keywords into secondary header:
c this is a trick timezero is recalculated inside 
c write_timing for file type different from 1
c the tim_point load in timezero for type 1 and 2 garbage
c 
       call tim_point(rbf_unit,newrec1, timezero, status)
       write(context, '( '' timezero'', g22.5)') timezero
       call xwrite(context, 20)
       status=0
       CALL write_timing(fits_unit, newrec1, newrec2, timezero, status)
       If(status.ne.0) then
         write(context,'('' write_timing error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       else
         call xwrite(' Timing written', 10)
       endif
C
C     Write keywords specific to an experiment (ME or GS):
       if (exp.eq.3) then
         CALL write_mekey(fits_unit,newrec1,newrec2,status)
         If(status.ne.0) then
            write(context,'('' write_mekey error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            GOTO 500
         else
            call xwrite(' ME specfic keywords written', 10)
         endif
       elseif(exp.eq.4)then
         CALL write_GSkey(fits_unit,newrec1,newrec2,status)
         If(status.ne.0) then
            write(context,'('' write_gskey error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            goto 500
         else
            call xwrite(' GS specfic keywords written', 10)
         endif
       elseif(exp.eq.1.or.exp.eq.2)then
         CALL write_lekey(fits_unit,newrec1,newrec2,status)
         call xwrite(' LE specific keywords written', 10)
         If(status.ne.0) then
            write(context,'('' write_lekey error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            status=0
         endif
       else
         call xwrite(' Unknown experiment type!', 10)
         WRITE(context,'('' the experiment number is  '', i4)') exp
         call xwrite(context,10) 
         GOTO 500
       endif
c
c     Write keywords comment on light curve LE and background ME
c! this part of the code has been used to insert comment in the 
c! lightcurve from the database       
c!        CALL write_write(fits_unit,newrec1, newrec2, status)
c!         If(status.ne.0) then
c!           write(context,'('' write_write error'', i5)') status
c!           call xaerror(context,5)
c!           goto 500
c!         else
c!           call xwrite(' Write_write written', 10) 
c!         endif
c!      
c
C
C     Write keywords common to all experiments:
       CALL write_comment(fits_unit,newrec1, newrec2, status)
       If(status.ne.0) then
         write(context,'('' write_comment error'', i5)') status
         errm = subname//' '//context
         call xaerror(errm,5)
         goto 500
       else
         call xwrite(' Comments written', 10)
       endif
C
C     Write out the data:
       if (type.eq.2) then
         CALL write_type2_data(rbf_unit,fits_unit,newrec1,newrec2
     &                         , status)
         If(status.ne.0) then
           write(context,'('' write_type2_data error'', i5)') status
           errm = subname//' '//context
           call xaerror(errm,5)
            goto 500
         endif
c       elseif (type.eq.1) then
c         CALL write_type1_data(rbf_unit,fits_unit,newrec1,newrec2
c     &                         , status) 
c         If(status.ne.0) then
c            write(context,'('' write_type1_data error'', i5)') status
c            call xaerror(context,5)
c            status=0
c         endif
       elseif (type.eq.0) then
         CALL write_type0_data(rbf_unit,fits_unit,newrec1,newrec2
     &                         , timezero, status)
         If(status.ne.0) then
            write(context,'('' write_type0_data error'', i5)') status
            errm = subname//' '//context
            call xaerror(errm,5)
            status=0
         endif
       else
         call xwrite(' Unknown file structure !', 10)
         WRITE(context,'('' the type is '', i4)') type
         call xwrite(context,5) 
         call xwrite(' Sorry the Fits file will have no data',5) 
         GOTO 500
       endif
C
C     Close the ratesbuffer and the Fits file:
500    CONTINUE
       CLOSE(rbf_unit)
       CALL FTCLOS(fits_unit, status)
       write(context, '('' Status after closing: '', i4)')status
       call xwrite(context,10)
      else 
        WRITE(context, '('' Status after closing : '',I4)')ierr
        errm = subname//' '//context
        CALL xaerror(errm, 5)
      ENDIF
      return
      END
C

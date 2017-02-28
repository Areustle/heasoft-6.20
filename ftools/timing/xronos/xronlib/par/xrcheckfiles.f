C***************************************************************
C
C subroutine:  xrcheckfiles
C     Checks out the specified infiles.  Strips off and processes file
C     options.
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      3/30/95
C
C modification history:
C
C notes:
C     
C
C calling sequence:
C      call xrcheckfiles(nser,nfil,cfile,dtint,dtsta,dtsto,nobins,ngtis,status)
C
C variables:
C
C     nser         I  Number of series for this task
C     nfil(array)  I  Number of files in each series (array)
C     cfile(array) I  Array of file names (+options)
C     dtint        O  Binning time of infiles (longest if variable)
C     dtsta        O  Start time of first infile
C     dtsto        O  Stop  time of last infile
C     nobins      I/O Running total of number of input bins/events
C     ngtis       I/O Running total of number of input gtis
C     csumm        O  Summary info for plot label
C     
C********************************************************************
      subroutine xrcheckfiles(nser,nfil,cfile,dtint,dtsta,dtsto,
     $     nobins, ngtis, csumm, status)
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      character(5) extopt
      integer m,n,k,iblank,extnum,jopt,isptr,ib,luin
      logical last_blank
      integer lenact
      external lenact
      integer xrsignd
      external xrsignd
      character(40) csuna
      character(15) csuse(8)
      character(160) cfile_in
      include '../include/xronos_init.inc'
      parameter (subname = 'xrcheckfiles:')

      if(status.ne.0) return

      do m=1,nser
         do n=1,nfil(m)
C     check out file cfile(n,m)
            write(context,
     &           '('' Series'',i2,'' file '',i4,'':'')')
     &           m, n
            context=context(:lenact(context))//cfile(n,m)
            call xwrite(context,10)

c     
c     part to decode options and check them
c     
            call xrparseopt(cfile(n,m),cfile_in,copt,iopt,mopt,dopt,
     $           status)
            
            
            
            if(status.ne.0) then
               context='Couldn''t process options for file:'
               errm = subname//' '//context
               call xaerror(errm, 5)
               errm = subname//' '//cfile(n,m)
               call xaerror(errm, 5)
               goto 999
            endif
c     Check whether file is FITS.
            call getlun(luin)
            CALL isitfits(luin,cfile_in,status)
            call frelun(luin)
            if(status.ne.0) then
               context=cfile_in(1:lenact(cfile_in))//
     $              ' is not a FITS file'
               errm = subname//' '//context
               call xaerror(errm, 5)
               goto 999
            endif

C     it's a FITS file
            CALL xrfrdhe( cfile_in, iopt,mopt, dopt,  dtintt,
     &           dtstat, dtstot, nobins(m), ngtis(m), 
     $           csuna, csuse(m), status)
            if(status.ne.0) then
               context='Couldn''t process header of file:'//cfile_in
               errm = subname//' '//context
               call xaerror(errm, 5)
               goto 999
            endif


c     
c     set dtsta and dtsto and check time order of buffers
c     
            IF (n.EQ.1) THEN
               dtstas(m) = dtstat
               dtstos(m) = dtstot
            ELSE
               IF (dtstat.LT.dtstas(m)) THEN
                  status = 1021
                  context='Error: infiles are not time ordered '
                  errm = subname//' '//context
                  call xaerror(errm, 5)
                  goto 999
               ENDIF
               IF (dtstat.LT.dtstos(m)) THEN
                  WRITE (context, 1004) m               
 1004             FORMAT (' **** Warning : Infiles for series ', I1,
     &                 ' overlap in time ')
                  call xwrite(context,5)
                  WRITE (context, 1005) 
 1005             format(15X,
     &                 ' Having > 1 intv or using ',
     &                 'time winds. might cause data loss !')
                  call xwrite(context,5)
               ENDIF
               IF (dtstot.GT.dtstos(m)) dtstos(m) = dtstot
            ENDIF
c     
c     set dtint and check different values
c     
            IF (n.EQ.1 .AND. m.EQ.1) THEN
               dtint = dtintt
            ELSE
c     first condit. is to check that dtint was previously set
               IF (xrsignd(dtint).NE.0.AND.
     &              xrsignd(dtint).NE.xrsignd(dtintt)) THEN
                  status = 1022
                  context='Cannot mix binned data and event arrival'//
     $                 'time files'
                  errm = subname//' '//context
                  call xaerror(errm, 5)
                  GOTO 999
               ENDIF
               IF (dtint.NE.0.d0.AND.dtint.NE.dtintt) THEN
                  context=' **** Warning : Different binning times '//
     &                 'in different infiles !'
                  call xwrite(context,10)
                  context=' New Bin integ. time must be'//
     &                 ' multiple of all bin times !'
                  call xwrite(context,10)
               ENDIF
c     to use longest bin as default
               IF (dtint.LT.dtintt) dtint = dtintt
            ENDIF
            
c     spit out summary info for high chatter
            csumm = ' '
            csumm(1:20) = csuna(1:20)
c
c ignore the following line for now (csuse carries the energy)
c because the summary should be a vector with nser dimension
c            csumm(21+(m-1)*15:35+(m-1)*15) = csuse(m)(1:15)
            call xwrite(csumm,20)
            WRITE (context, 
     $           '('' Tint(s),Start,Stop(d) = '',g12.4,2f15.6)')
     &           dtint, dtstas(m), dtstos(m)
            call xwrite(context,20)
         enddo
      enddo
c     
c     determine dtsta, dtsto
c     
      dtsta = dtstas(1)
      dtsto = dtstos(1)
      IF (nser.GT.1) THEN
         DO m = 2, nser
            IF (dtsta.GT.dtstas(m)) dtsta = dtstas(m)
            IF (dtsto.LT.dtstos(m)) dtsto = dtstos(m)
         ENDDO
      ENDIF
      
      
      
 999  RETURN
      END
c     
c
c

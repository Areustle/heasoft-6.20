C***************************************************************
C
C subroutine:  xrgetres.f
C              get resolution for epoch folding searches
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      4/5/95
C
C modification history:
C
C notes:
C
C calling sequence:
C      call xrgetres(dtnb, dper, nbint, nbin, fperfo, dres, nper, status)
C
C variables:
C     I   dtnb = newbin duration (secs)
c     I   dper = period for folding (s)
c     I   nbint = no. of newbins/intv
C     I   nbin = phase bins/period
C     I   fperfo = period format {1=d / 2=s}
c     O   dres = resolution for period search (s)
c     O   nper = no. of periods for search
C
C***************************************************************
      SUBROUTINE xrgetres(dtnb, dper, nbint, nbin, fperfo,
     $     dres, nper, status)
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      integer iv,nper_def,fperfo
      double precision dres_def
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetres:')
      
 1    if(status.ne.0) return
c     
c
c  Ask for resolution in period search
c
c

c     default resol. is (Fourier res.in interval)/2.
      dres_def = dper*dper/(dtnb*dble(nbint)*2.D0)

      write(context,'('' Default resolution is'',g17.10)') dres_def
      call xwrite(context,5)
      call xwrite(' Type INDEF to accept the default value',10)

      call uclgsd('dres',dres,status)
      if(status.eq.3) then
         status=0
         dres=dres_def
         call uclpsd('dres',dres,status)
      endif



c     convert to s if necessary
      IF (fperfo.EQ.1) dres = dres*86400.D0
      if(status.ne.0) then
         context= 'Couldn''t get DRES parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      

      if(dres.le.0.D0) then
         status=1002
         context = 'Illegal value for DRES (must be > 0)'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif
      
      if(dres.gt.dper) then
         status=1038
         context= 'Illegal value for DRES (must be <= period)'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

      WRITE(context, '('' resolut.(s) = '',g18.10)') dres
      call xwrite(context,15)
c
c
c  Ask for no. of periods in search
c

      
c     set original default
      nper = 128
c     to avoid negative periods. This is now dealt with in the period array preparation
C     routine (xrpper).  The program will give you however many periods you 
C     want and the resolution you want.   If values are odd, the initial period just
C     won't be in the middle of the search space.
c      IF (nper/2.GT.int(dper/dres)) THEN
c         nper = 2*int(dper/dres)
c     reset maximum accordingly
c         iv = nper                      
c      ENDIF
      
      nper_def=nper

      write(context,
     $     '('' Default number of periods is'',i10)') nper_def
      call xwrite(context,5)
      call xwrite(' Type INDEF to accept the default value',10)

      call uclgsi('nper',nper,status)
      if(status.eq.3) then
         status=0
C     use default
         nper = nper_def
         call uclpsi('nper',nper,status)
      endif
      if(status.ne.0) then
         context= 'Couldn''t get NPER parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
      
      
      IF (nper.EQ.0 .OR. nper.EQ.1 .OR. nper.LT.-31) THEN
         status = 1002
         context = 'Illegal value for NPER'
         errm = subname//' '//context
         call xaerror(errm, 5)
         GOTO 999
      ENDIF
      IF (nper.LT.0) nper = 2**(-nper)
C     This screening is no longer necessary.  We don't have maximum sizes anymore,
C     and xrpper shifts the search range to avoid negative periods.
c      IF (nper.GT.iv) then
c         status = 1002
c         context= 'NPER larger than maximum value'
c         call xaerror(context, 5)
c         goto 999
c      endif
c      IF (nper/2.GT.int(dper/dres)) then
c         status = 1039
c         context= 'Too many periods'
c         call xaerror(context, 5)
c         goto 999
c      endif
      
      WRITE (context, '('' Using Number of Periods: '',i10)') nper
      call xwrite(context,15)
      
 999  return
      end


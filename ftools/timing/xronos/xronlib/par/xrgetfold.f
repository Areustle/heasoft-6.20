C***************************************************************
C
C subroutine:  xrgetfold
C              get epoch folding specific binning parameters
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      3/23/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C
C calling sequence:
C      call xrgetfold
C
C variables:
C
c     I   dtint = longest bin duration in input file(s) (in secs)
c                 (=-1. for arrival time files)
c     I   dtsta, dtsto = start and stop times of infiles (days)
c     I   nbdf = default for no. of newbins/intv
c     O   dtnb = newbin duration (secs)
C     O   depoch = epoch (days)
C     O   dper = period (seconds)
C     O   perfo = period format {1=d / 2=s} (passed for use when getting 
C                 period resolution later)
C     O   dpdot = period derivative
C     O   nbin = phase bins/ period
c
C**********************************************************************
      SUBROUTINE xrgetfold(dtint, dtsta, dtsto, nbdf, dtnb,
     $      depoch, dper, perfo, dpdot, nbin, nbint, status)
c
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      character(80) sepoch
      integer epochfo,perfo,iv,iepoch(5),i,nbin_def
      double precision dsepoch,depoch_def
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetfold:')

      if(status.ne.0) return
      do i = 1,5
         iepoch(i)=0
      enddo

c
c  Ask for epoch
c
c     ask for format of epoch
      call uclgsi('epochfo',epochfo,status)
      if(status.NE.0) then
         context= 'Couldn''t get EPOCHFO parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
C the following should be done by XPI
c      IF (epochfo.NE.1 .AND. epochfo.NE.2 .AND. epochfo.NE.3) then
c         status = 1002
c         context = 'EPOCHFO value must be in range 1-3'
c         call xaerror(context, 5)
c         goto 999
c      endif
c
c     ask for epoch

      depoch_def = dble(int(dtsta))
      write(context,'('' Default Epoch is:'',g17.10)') depoch_def
      call xwrite(context,5)
      call xwrite('Type INDEF to accept the default value',10)

      if (epochfo.eq.2) then
         call xwrite(' Epoch format is d s.',1)
      else if (epochfo.eq.3) then
         call xwrite(' Epoch format is d h m s ms.',1)
      else
c     unless it's a 2 or a 3 the epoch is in DAYS
         epochfo=1
         call xwrite(' Epoch format is days.',1)
      endif
         
      call uclgst('sepoch',sepoch,status)
      call upc(sepoch)
      if(sepoch(1:1).eq.'I') then
         depoch = depoch_def
         write(sepoch,'(e17.10)') depoch
         call uclpst('sepoch',sepoch(1:17),status)
         epochfo=1
         call uclpsi('epochfo',epochfo,status)
         status=0
      endif
      if(status.ne.0) then
         context= 'Trouble with SEPOCH  parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      

C     split up epoch string
      call xrparseday(epochfo,sepoch,depoch,dsepoch,iepoch,status)
      if(status.eq.1048)then
         context= 'Couldn''t decode epoch time'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif
      if(status.eq.1005)then
         context= 'Illegal time entered'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif
C     decode output of xrparseday into decimal days
      call xrdhms(depoch,dsepoch,iepoch,epochfo)

      write(context,'('' Epoch used is (days) : '',g17.10)') dres

c
c Ask for period format
c     ask for format of period
      call uclgsi('perfo',perfo,status)
C     the following is handled by XPI
c      IF (perfo.NE.1 .AND. perfo.NE.2) then
c         status = 1002
c         context = 'PERFO value must be either 1 or 2'
c         call xaerror(context, 5)
c         goto 999
c      endif

c
c     ask for period




      if (perfo.eq.1) then
         call xwrite('Period is in days.',1)
      else
c     unless perfo's a 1, the period is in SECONDS
         perfo=2
         call xwrite(' Period format is seconds.',1)
      endif

      call uclgsd('dper',dper,status)
      if(status.ne.0) then
         context= 'Couldn''t get DPER parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
      

      IF (perfo.EQ.1) dper = dper*86400.D0
      IF (dper.LE.0.D0) then
         status = 1002
         context = 'Illegal period'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif
      write(context,'('' period = '',g18.10)') dper
      call xwrite(context,15)
c
c     ask for period derivative pdot
      call uclgsd('dpdot',dpdot,status)
      if(status.ne.0) then
         context= 'Couldn''t get DPDOT parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
c
c  write message
c No. of cycles
      dv = (dtsto-dtsta)*86400.D0/dper
      write(context,280) dv
      call xwrite(context,10)
 280  FORMAT ( ' Expected Cycles .. ', F17.2)
c
c  Get no. of newbins per period (i.e. newbin duration dtnb)
c
c set original default
      nbin = nbdf
      IF (dtint.GT.0.D0) THEN
c maximum no. of newbins/period
         iv = int(dper/dtint)
         IF (nbin.GT.iv) nbin = iv
      ENDIF
      nbin_def = nbin

      write(context,'('' Default phase bins per period are:'',i10)')
     $     nbin_def
      call xwrite(context,5)
      call xwrite(' Type INDEF to accept the default value',10)


      call uclgsi('nphase',nbin,status)
      if(status.eq.3) then
         status=0
         nbin=nbin_def
         call uclpsi('nphase',nbin,status)
      endif

      if(status.ne.0) then
         context= 'Couldn''t get NPHASE parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
      IF (nbin.EQ.0 .OR. nbin.EQ.1 .OR. nbin.LT.-31) THEN
         status= 1002
         context='Illegal number for phasebins/period'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      ENDIF
      IF (nbin.LT.0) nbin = 2**(-nbin)
c
      WRITE (context, '('' nphase = '',i10)') nbin
      call xwrite(context,15)
      
c  define newbin duration
      dtnb = dper/dble(nbin)

C  set default (?) for nbint
      nbint = int((dtsto-dtsta)*86400.D0/dtnb)
c  set d/f to 1 intv
      IF (nbint.LT.(dtsto-dtsta)*86400.D0/dtnb) nbint = nbint + 1

 999  return
      end

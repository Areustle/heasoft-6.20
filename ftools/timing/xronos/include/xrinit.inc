C
C Initialization file for all XRONOS programs
C
C Collected by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      February/March, 1995
C
      data cpf /maxcpf*' '/
      data ipf /maxipf*0/
      data rpf /maxrpf*0./
      data dpf /maxdpf*0.D0/
      data iflags /maxiflags*0/
      data rflags /maxrflags*0./
      data iqdp /maxqdp*0/
      data rqdp /maxqdp*0./
      data cqdp /maxqdp*' '/

c default exposure windows:  We had agreed this would be done with
C a file.  x5getwin will have to be modified to not allow no window file
C if this is removed.  Besides, this file assumes the size of the various
C arrays and must be modified if additional windows are added
c      include 'xrwindabni.h'

c initial values for variables
      integer maxis, maxrs, maxds
      parameter (maxis = maxistat*maxseries)
      parameter (maxrs = maxrstat*maxseries)
      parameter (maxds = maxdstat*maxseries)
      data nfil   /maxseries*0/
      data irtsta /maxis*0/
      data rrtsta /maxrs*0./
      data drtsta /maxds*0.D0/

c values for psd.  some may need to move back to psd
C
c default no. of newbins/intv.
c      DATA nbdf/8/ replaced with a parameter
c =-1 for programs with no intv average and rebinning
      DATA nintfm/0/
c =1 if no. of newbins/intv. must be a power of 2
      DATA ipow2/1/
c =1 to write all bins to terminal (and log)
      DATA ilda/0/
c severity code for infile warnings (2=highest)
      DATA isev/2/
      DATA nobins/maxseries*0/
      DATA ngtis/maxseries*0/
c no. of result columns/frame
      iflags(8) = 3
c no. of indep. variable columns
      iflags(9) = 2
c no. of input series
      iflags(10) = 1
c no. of boxes/plot
      iflags(11) = 1
c as above but for QDP
      iqdp(3) = iflags(11)
c
      do i=1,nfilemax
         do j=1,maxseries
            cfile(i,j)=' '
         enddo
      enddo



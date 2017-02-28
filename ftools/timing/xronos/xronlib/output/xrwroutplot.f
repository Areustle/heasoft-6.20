c
        SUBROUTINE xrwroutplot (cpro, cpf, iqdp, cqdp, cfilo, oftype,
     $     iflags, rflags, irtsta, rrtsta, drtsta, nper, 
     &     csumm, nanal, dtnb, nkount, nbint, nintfm, 
     &     dxsta, dxstep, yr, syr, expr, xr, sxr,
     $     chival, chierr, dpera, kmax, status)
        implicit none
c
c  cpro     I  c  prompt of main (c*4)
c  cpf      I  c  array of global paramater
c  iqdp     I  i  plot flags (es. iqdp(1) 0=no plot 1=plt 2=automatic)
c  cqdp     I  c  array for plot device and pco file name
c  cfilo    I  c  output file name 
c  oftype   I  i  output file type
c  iflags   I  i  integer array flags
c  rflags   I  r  real array flags
c  irtsta   I  i  integer array statistics
c  rrtsta   I  r  real array statistics 
c  drtsta   I  d  double precison array statistics 
c  nper     I  i  size for chival array
c  csumm    I  c  character string for title plot 
c  nanal    I  i  number of points in the analysis (Note this can be diffent
c                 from nbint (es. in psd nanal=nbint/2)
c  dtnb     I  d  new bin size
c  nkount   I  i  size of input array
c  nbint    I  i  number of newbin per interval
c  nintfm   I  i  Number of interval per frame
c  dxsta    I  d  start first bin for the xaxis (frequency, time or phase) 
c  dxstep   I  d  increment for the xaxis (frequency, time or phase)
c  yr       I  r  results array
c  syr      I  r  error on results array
c  expr     I  r  exposure or average number of point/frame array 
c  chival   I  r  chival array results
c  chival   I  r  chival array results
c  dpera    I  d  xaxis other for yr or chival array results dpera
c  kmax     I  i  pointer for dper to get the best period 
c  status   I  i  status flag (0 means "ok")
c
c Input variable
      include '../include/io.inc'
        INTEGER*4 iflags(20), irtsta(20,*)
        INTEGER*4 iqdp(*), oftype
        INTEGER*4 nper, status
        INTEGER*4 nbint, nintfm, nanal, kmax(*), nkount
        REAL*4 rflags(10), rrtsta(20,*)
        REAL*4 yr(nkount, *), syr(nkount, *)
        REAL*4 expr(nkount),chival(nper,*), chierr(nper,*) 
        REAL*8 xr(nkount), sxr(nkount)
        DOUBLE PRECISION dpera(nper)
        DOUBLE PRECISION dtnb, drtsta(20,*), dxsta, dxstep
        character(40)  cpf(*)
        character(160) cqdp(*)
        CHARACTER cfilo*132, cpro*8, csumm*80
c local variable
        INTEGER*4 ita(5), ito(5), itu(5), indx(2), npsize
        DOUBLE PRECISION doo
        INTEGER*4 mxcmd,ncmd
        PARAMETER(mxcmd=50)
        character(132) cmd(mxcmd)
        INTEGER*4 jmax, plot, nv
        integer i, ier

        include '../include/dmcommon.inc'
      parameter (subname = 'xrwroutplot:')
c
        iflags(14) = nanal
c
c calculates variable for plotting and for output
        CALL xrpltvar (cpro, iflags, drtsta, dpera, dxsta, indx,
     &                    ito, ita, itu, doo) 
c
c calculates array of commands for plt
c note kmax is the index for the best period in dper and chival 
c if shouldbe an array 
        IF (iqdp(1).NE.0) THEN
        CALL xrpltcmd (cpro, cqdp, iqdp, ito, ita, itu, dtnb, csumm,
     &                 dxstep, kmax, dpera, iflags, rflags, cmd, ncmd)
        endif
        
        if(cpro(1:2).ne.'es') then
c
c calculates y array
           CALL xryaxis (iflags, rflags, nkount, nanal, 
     &          yr, syr, expr)
c     
c     probably to allocate memory for the xaxis array xr(nanal)
c     Note the phase for for the folding are in xr 
c     calculates x array
           CALL xrxaxis (dxsta, dxstep, doo, iflags, rflags,
     &          xr, sxr, jmax)
        else
c
c  Included for efsearch case where xr and sxr are dummy arrays of
c  size nkount.  Otherwise, jmax could be undefined and fed to 
c  xrwroutf as the size of xr and sxr causing unexpected results
c
           jmax = nkount
        endif
c     
c     run plt only if iqdp(1) not equal 0
        IF (iqdp(1).NE.0) THEN
           if(cpro(1:2).eq.'es') then
              npsize=nper
           else
              npsize=iflags(13)
           endif
           if (cpro(1:2).eq.'ef') npsize=npsize*2
c           nv=npsize*17
           nv=npsize*(iflags(8)+2)
           plot = 0
           call udmget(nv,6,plot,status)
           if(status.ne.0) then
              errm = subname//' '//
     &              'Error allocating memory in plot routine'
              call xaerror(errm, 5)
              if(cfilo.ne.' ')then 
                call xwrite(' The results will be saved in a file', 10)
                status=0
                goto 888 
              else 
                call xwrite(' Try with less points ', 10)
                goto 999
              endif
           endif
c           
           CALL xrplt (cpro, yr, syr, expr, xr, sxr, nkount, npsize,
     &          chival, chierr, kmax, dpera, nper, iflags, cmd,
     &          ncmd, memr(plot), ier) 
           
           call udmfre(plot,6,status)
c
c  non-zero ier signals abort
c
           if ( ier.ne.0 ) then
              status = ier
              goto 999
           endif

        ENDIF
c     
c write output only of requested
c
 888    IF (cfilo.ne.' ') THEN
           CALL xrwroutf (cpro, cfilo, oftype, iflags, rflags, csumm,
     &          rrtsta, drtsta, indx, dtnb, nkount,nbint, nintfm, dxsta, 
     $          dxstep, yr, syr, expr, xr, sxr, kmax,jmax,doo,
     $          chival, chierr, dpera, nper, status)
           if(status.ne.0) then
              errm = subname//' '//'Error writing output file.'
              call xaerror(errm, 5)
              goto 999
           endif
        ENDIF
 999    continue
        RETURN 
        END 

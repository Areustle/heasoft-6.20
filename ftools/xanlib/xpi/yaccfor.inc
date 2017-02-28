      integer maxlinesize
      parameter (maxlinesize=1000)
      integer maxpars
      parameter (maxpars=100)

      character(1000) scom
      character(1000) spars(maxpars)
      character(1000) sval(maxpars)
      character(1000) sline
      character(80) gtprompt
      common /yaccforlc/ scom,spars,sval,sline,gtprompt

      integer npars,cpar
      logical comval
      logical debug

      common /yaccforli/ npars,cpar,comval,debug

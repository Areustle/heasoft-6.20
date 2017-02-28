c
c  Routines that handle null values in fortran
c
c  Usage:
c
c  INTEGER
c
c     integer inull, ivar  
c     logical isinull   
c     ivar = inull()    # Sets ivar to special integer defined as null
c
c     if ( isinull(ivar) ) then   # Checks if ivar is null
c        do something
c     endif
c
c  REAL
c
c     real*4 rnull, rvar  
c     logical isrnull   
c     rvar = rnull()    # Sets rvar to special real value defined as null
c
c     if ( isrnull(rvar) ) then   # Checks if rvar is null
c        do something
c     endif
c
c  DOUBLE
c
c     real*8 dnull, dvar  
c     logical isdnull   
c     dvar = dnull()    # Sets dvar to special real value defined as null
c
c     if ( isdnull(dvar) ) then   # Checks if dvar is null
c        do something
c     endif
c
c---------------------------------------------------------------------
      function inull()
      integer*4 inull
c
c  Returns integer null value
c
      include 'null.inc'

      inull = INUl

      return
      end

c---------------------------------------------------------------------
      function isinull(ival)
      logical isinull
c
c  Takes integer value and returns boolean, which indicates
c  whether it is null or not
c
c  I  ival   (r)  Integer value
c
      integer*4 ival

      include 'null.inc'

      if ( ival.eq.INUl ) then
         isinull = .TRUE.
      else
         isinull = .FALSE.
      endif

      return
      end

c---------------------------------------------------------------------
      function rnull()
      real*4 rnull
c
c  Returns real null value
c
      include 'null.inc'

      rnull = RNUl

      return
      end

c---------------------------------------------------------------------
      function isrnull(rval)
      logical isrnull
c
c  Takes real value and returns boolean, which indicates
c  whether it is null or not
c
c  I  rval   (r)  Real value
c
      real*4 rval

      include 'null.inc'

      if ( rval.eq.RNUl ) then
         isrnull = .TRUE.
      else
         isrnull = .FALSE.
      endif

      return
      end
c---------------------------------------------------------------------
      function dnull()
      real*8 dnull
c
c  Returns double null value
c
      include 'null.inc'

      dnull = DNUl

      return
      end

c---------------------------------------------------------------------
      function isdnull(dval)
      logical isdnull
c
c  Takes double value and returns boolean, which indicates
c  whether it is null or not
c
c  I  dval   (d)  Real value
c
      real*8 dval

      include 'null.inc'

      if ( dval.eq.DNUl ) then
         isdnull = .TRUE.
      else
         isdnull = .FALSE.
      endif

      return
      end

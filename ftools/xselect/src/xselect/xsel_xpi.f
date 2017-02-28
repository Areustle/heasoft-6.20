      SUBROUTINE XSL_UCLGSB(Parname,Buffer,Status)

C     return an boolean value for parname

      CHARACTER*(*) Parname
      LOGICAL Buffer
      INTEGER Status

      call uclgsb(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGSI(Parname,Buffer,Status)

C     return an integer value for parname

      CHARACTER*(*) Parname
      INTEGER Buffer
      INTEGER Status

      call uclgsi(Parname,Buffer,Status)
      return
      end
 

      SUBROUTINE XSL_UCLGSR(Parname,Buffer,Status)

*
* return a REAL value related to parname
*
      CHARACTER*(*) Parname
      REAL Buffer
      INTEGER Status

      call uclgsr(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGSD(Parname,Buffer,Status)
*
* return a DOUBLE PRECISION value related to parname
*
      CHARACTER*(*) Parname
      DOUBLE PRECISION Buffer
      INTEGER Status

      if(status.ne.0) return
      call uclgsd(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGST(Parname,Buffer,Status)

*
* return a string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status

      call uclgst(Parname,Buffer,Status)
      return
      end

      SUBROUTINE XSL_UCLGSTD(Parname,Buffer,Status)

*
* return the default string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status

      call uclgstd(Parname,Buffer,Status)
      return
      end

      SUBROUTINE XSL_UCLGSG(Parname,Buffer,N,Rmin,Rmax,Nr,Status)

*
* parname is the parameter name (c*(*))
* buffer1 is a 2xn real*4 array to hold the resulting ranges
* n is the size of buffer
* rmin and rmax are the min and max for the range
* nr is the number ranges in buffer which are good
* status is the return status
*
* return a range value related to parname
*
*
      CHARACTER*(*) Parname
      INTEGER N
      REAL Buffer(2,N) , Rmin , Rmax
      INTEGER Nr
      INTEGER Status

      call UCLGSG(Parname,Buffer,N,Rmin,Rmax,Nr,Status)
      return
      end 


      SUBROUTINE XSL_UCLGSGPARSE(Buffer,Buffer1,N,Rmin,Rmax,Nr,Status)

* parse a string into a buffer 
 
      IMPLICIT NONE
      INTEGER N
      REAL Buffer1(2,N) , Rmin , Rmax
      CHARACTER*(*) Buffer
      INTEGER Nr
      INTEGER Status

      CALL UCLGSGPARSE(Buffer,Buffer1,N,Rmin,Rmax,Nr,Status)
      RETURN
      END


      SUBROUTINE XSL_UCLGOT(Parname, Status)

* check if parameter has been entered on command line

      IMPLICIT NONE
      CHARACTER*(*) Parname
      INTEGER Status

      CALL uclgot(Parname, Status)
      RETURN
      END

      SUBROUTINE XSL_UCLPSB(Parname,Buffer,Status)

C     put a boolean value into parname

      CHARACTER*(*) Parname
      LOGICAL Buffer
      INTEGER Status

      call uclpsb(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPSI(Parname,Buffer,Status)

C     put an INTEGER value into parname

      CHARACTER*(*) Parname
      INTEGER Buffer
      INTEGER Status

      call uclpsi(Parname,Buffer,Status)
      return
      end
 

      SUBROUTINE XSL_UCLPSR(Parname,Buffer,Status)

*
* put a REAL value into parname
*
      CHARACTER*(*) Parname
      REAL Buffer
      INTEGER Status

      call uclpsr(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPSD(Parname,Buffer,Status)
*
* put a DOUBLE PRECISION value into parname
*
      CHARACTER*(*) Parname
      DOUBLE PRECISION Buffer
      INTEGER Status

      if(status.ne.0) return
      call uclpsd(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPST(Parname,Buffer,Status)

*
* return a string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status

      call uclpst(Parname,Buffer,Status)
      return
      end



c This doesn't really belong in this file but put it here for lack of a
c better place.

c ---------------------------------------------
      subroutine XSL_RUNCF(fname,ECHO,ierr)
c ---------------------------------------------
c Makes a file executable and runs it as a command file
c     Alan Smale, June 1992
      implicit none

      character*(*) fname
      logical ECHO
      integer len1, ierr
      integer LENACT

      ierr = 0
      len1 = LENACT( fname )
c  Cat it back, if ECHO
      IF ( ECHO ) call XSL_MORE(fname)
c Now close the log file, so that the ftools can open it:
      call xpicloselog()
c Now run the command file
      call xsl_runcc(fname,ierr)
c Now reopen the log file
      call xpiopenlog()
      return
      end
c

c
c  Common for text input/output
c  Reading and writing cannot occur simultaneously
c
c  txtqfile = File currently in buffer
c  txtqmode = (1=file to be written, 0=file read)
c  nqrows   = Number of rows defined
c  nqpts    = Number of points defined
c  nqcol    = Number of columns defined
c  qbuf     = Buffer
c  qerr     = Column status flags
c
      include '../include/maxvals.inc'

      character(300) txtqfile
      integer mxqbuf, mxqcom
      integer txtqmode, nqrows, nqpts, nqcol, nqcom
      parameter(mxqbuf=100000, mxqcom=100)
      character(100) qcom
      real*4 qbuf
      integer qerr

      common /txtio/ txtqmode, nqrows, nqpts, nqcol, nqcom,
     &               qerr(MXQCOL), qbuf(mxqbuf)
      common /txtfio/ txtqfile, qcom(mxqcom)

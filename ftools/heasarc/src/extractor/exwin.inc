
C          maxseries = maximum number of time series allowed
C          maxbintype = maximum binning types: nbins, newbins, interval

      integer maxseries, maxbintype
      parameter (maxbintype = 3)
      parameter (maxseries = 4)


C     windows variables
C
C           nwi - number of each type of window:
C           nwito = iflags(5) - total number of all types of windows
C     1 - number of time windows
C     2 - number of phase windows
C     3 - number of intensity windows series 1 original bins
C     4 - number of intensity windows series 1 new bins
C     5 - number of intensity windows series 1 intervals
C     6 - number of intensity windows series 2 original bins
C     7 - number of intensity windows series 2 new bins
C     8 - number of intensity windows series 2 intervals
C     9 - number of intensity windows series 3 original bins
C    10 - number of intensity windows series 3 new bins
C    11 - number of intensity windows series 3 intervals
C    12 - number of intensity windows series 4 original bins
C    13 - number of intensity windows series 4 new bins
C    14 - number of intensity windows series 4 intervals
C    15 - number of exposure windows series 1 original bins
C    16 - number of exposure windows series 1 new bins
C    17 - number of exposure windows series 1 intervals
C    18 - number of exposure windows series 2 original bins
C    19 - number of exposure windows series 2 new bins
C    20 - number of exposure windows series 2 intervals
C    21 - number of exposure windows series 3 original bins
C    22 - number of exposure windows series 3 new bins
C    23 - number of exposure windows series 3 intervals
C    24 - number of exposure windows series 4 original bins
C    25 - number of exposure windows series 4 new bins
C    26 - number of exposure windows series 4 intervals
C
C           pwi(1) = phase epoch
C           pwi(2) = phase period
C           pwia, pwio - phase window start and stop
C
C           fwia, fwio - flux window start and stop
C           ewia, ewio - exposure window start, stop
C
C     maxwintype = time + phase + maxseries*maxbintype (exposure + flux) = 26

      integer maxwintype, maxphwin, maxfluxwin, maxexpwin, maxtimewin
      parameter (maxwintype = 26)
      parameter (maxphwin = 10)
      parameter (maxfluxwin = 10)
      parameter (maxexpwin = 1)
      parameter (maxtimewin = 1000)

      integer nwi(maxwintype), nwito
      real pwia(maxphwin),pwio(maxphwin)
      real fwia(maxseries*maxbintype,maxfluxwin)
      real fwio(maxseries*maxbintype,maxfluxwin)
      real ewia(maxseries*maxbintype),ewio(maxseries*maxbintype)
      double precision twia(maxtimewin),twio(maxtimewin),pwi(2)

      COMMON / ext_xwin / twia, twio ,pwi, pwia, pwio, fwia, fwio, 
     &                    ewia, ewio, nwi, nwito



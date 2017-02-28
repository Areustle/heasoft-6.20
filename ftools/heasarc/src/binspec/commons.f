

C maximum number of GTIs
	integer maxgti
        parameter (maxgti = 100) 

C maximum number of phase specifications
	integer maxphase
	parameter (maxphase = 10)

C maximum number of history records
	integer maxhists
	parameter (maxhists = 10)

C maximum number of columns in fits file
	integer maxcl
	parameter (maxcl = 99)

C maximum number of columns specified
	integer maxcols
	parameter (maxcols = 10)

C maximum size of input array
	integer maxsize
	parameter (maxsize = 1024)

C maximum size of vector to bin
	integer maxvec
	parameter (maxvec = 1024)

C maximum number of ranges allowd
	integer maxrange
	parameter (maxrange = 15)

C maximum number of bins in light curve
	integer maxbins
	parameter (maxbins = 5000)

C values for input file information
	integer iunit, iextnumb, ihtype, tcolno, colno(maxcols), nrows, 
     &       tfields, vecelem(maxcols), rrange, rstart(maxrange), 
     &       rstop(maxrange), ncols

	 logical notimes
	common /incom/ colno, vecelem, rstart, rstop, iunit, iextnumb,
     &           ihtype, tcolno, nrows, tfields, rrange, ncols, notimes

C values for input character information
	character(80) ttype(maxcl), tform(maxcl), tunit(maxcl), extname
	common /inchar/ ttype, tform, tunit, extname

C values for output file information
C NOTE oveclem is used in BINSPEC as the number of vector elements to output
C                      in BINCURVE it is the value of the event
	integer ounit, outcolno(maxcols), onrows, otfields
        double precision ovecelem(maxcols)
	common /outcom/ ovecelem, ounit, outcolno, onrows, otfields

C values for output character
	character(80) ottype(maxcl), otform(maxcl), otunit(maxcl),
     &               outextname
	common /outchar/ ottype, otform, otunit, outextname

C values for GTI file information
	integer gunit, gextnumb, ghtype, gstartno, gstopno, gnrows
	common /gticom/ gunit, gextnumb, ghtype, gstartno, gstopno, 
     &                  gnrows

C GTI data information
	double precision tstart(maxgti), tstop(maxgti), goffset
	common /gtidata/ tstart, tstop, goffset

C values for phase file infomation
	integer punit, pextnumb, phtype, pnrows
	common /phasecom/ punit, pextnumb, phtype, pnrows

C values for phase data
	double precision epoch, period, pstart(maxphase), 
     &           pstop(maxphase), poffset
	common /phdata/ pstart, pstop, epoch, period, poffset

C values for data
	double precision vector(maxvec,maxcols), outvec(maxvec,maxcols)
	integer vrange(maxcols), vstart(maxrange, maxcols),
     &           vstop(maxrange, maxcols)
	common /vecdata/ vector, outvec, vstart, vstop, vrange

C values for light curve information
	integer nbins, counts(maxbins)
	double precision values(maxbins, maxcols), highval, lowval,
     &               binsize
	common /crvdata/ values, highval, lowval, binsize, counts, 
     &                   nbins

C filenames
	character(160) infile, outfile, gtifile, phasefile
	common /filecom/ infile, outfile, gtifile, phasefile

C taskname
	character(40) taskname
	common /task/ taskname


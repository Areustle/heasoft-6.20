C--- NCHAN	I	Number of channels
C--- NDET	I	Number of detectors
C--- NGAS	I	Number of gasses
C--- ISAMP	I	Sample rate (Hz)
C--- ISTRT(2)	I	Start channel
C--- ICOMP(2)	I	Compression (number of input channels per output)
C--- DTP(2)	R*8	Data time per point.
	DOUBLE PRECISION DTP
	COMMON /WSP/NCHAN,NDET,NGAS,ISAMP,ISTRT(2),ICOMP(2),DTP(2)

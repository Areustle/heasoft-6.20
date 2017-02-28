/*****************************************************************************
******************************************************************************
* open an attitude file of any supported format.
* The standard format is the ASCA attitude file format which is a FITS
* bintable with extension name "ATTITUDE" and two columns TIME, and QPARAM.
* QPARAM is an array giving the four components of a quaternion.
* The TELESCOP keyword in the primary header is also read to get the mission
* name, but this is optional and set to "UNKNOWN" if it does not exist.
*
* If an attempt is made to read any other format, then this routine will open
* a new temporary file containing a copy of the file converted to the standard
* format. In general the temporary file will be a memory resident
* "mem://" file.
*
* currently supported formats are the standard format and ASCII RA Dec Roll
* format.
*****************************************************************************/
fitsfile* open_any_attfile(char* filename);

/****************************************************************************
*****************************************************************************
* convert an a file with the following format to a standard 
* quaternion-based attitude file:
* A plane ASCII text file with each row containing four numbers:
* time ra dec roll
* where time is in seconds from some arbitrary reference time,
* and ra, dec are the celestial coordinates of the spacecraft Z axis
* in decimal degrees, and roll is the roll angle, also in degrees.
* any row beginning with "#" will be ignored. In fact any row 
* from which four separate numbers cannot be read will be ignored.
* This routine does not actually close the fitsfile when done
* and returns a pointer to the fitsfile structure. This way it can be
* used to write memory resident files which are deleted on closing.
****************************************************************************/
fitsfile* convert_ascii_ra_dec_roll(char* infile, char* outfile);


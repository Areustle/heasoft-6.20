/* FTOOLS info: $Header: /headas/headas/ftools/futils/src/fcopy/fcopy.c,v 1.2 2003/04/29 18:02:46 pence Exp $ */
/**********************************************************************
 *
 *  fcopy -- Copy an input FITS file to an output one, HDU by HDU.
 *  Expected to be mainly useful with new cfitsio extended-syntax filenames.
 *
 *  Prototype by Bill Pence, NASA/GSFC 664.
 *  Ftools version by Jeff Guerber, Raytheon STX, NASA/GSFC 664, May 1998
 *
 *  $Log: fcopy.c,v $
 *  Revision 1.2  2003/04/29 18:02:46  pence
 *  fcopy was not reporting an End-of-File error if the input file was corrupted
 *  and shorter than expected.
 *
 *  Revision 1.1  1998/05/14 05:36:47  guerber
 *  fcopy: Copy fits files, hdu by hdu.  Initial revision.
 *
 **********************************************************************/

#include <stdio.h>
#include "fitsio.h"
#include "cftools.h"
#include "pfile.h"

static int fcopyparms(char *infile, char *outfile, int *status);

static volatile char rcsid[] = "$Id: fcopy.c,v 1.2 2003/04/29 18:02:46 pence Exp $";


int fcopymain()
{
    fitsfile *infptr, *outfptr;
    int status = 0, ii;
    char infile[FLEN_FILENAME], outfile[FLEN_FILENAME], msg[C_FCERR_MSG];

    c_ptaskn("fcopy 1.0");

    /* get parameters */
    if ( fcopyparms(infile, outfile, &status) != 0 ) return(status);

    /* open the input file */
    if ( fits_open_file(&infptr, infile, READONLY, &status) != 0 )
    {
        fits_get_errstatus(status, msg);
        do  c_fcerr(msg);  while (fits_read_errmsg(msg) != 0);
        return(status);
    }

    /* create the output file */
    if ( fits_create_file(&outfptr, outfile, &status) != 0 )
    {
        fits_close_file(infptr, &status);

        fits_get_errstatus(status, msg);
        do  c_fcerr(msg);  while (fits_read_errmsg(msg) != 0);
        return(status);
    }

    /* copy all the HDUs (previous, current, and following) */
    fits_copy_file(infptr, outfptr, 1, 1, 1, &status);

    fits_close_file(infptr,  &status);
    fits_close_file(outfptr, &status);

    if (status != 0) {
        fits_get_errstatus(status, msg);
        do  c_fcerr(msg);  while (fits_read_errmsg(msg) != 0);
    }

    return(status);
}

/************************************************************************
 *
 *  fcopyparms -- read the parameters
 *
 ************************************************************************/

static int fcopyparms(char *infile, char *outfile, int *status)
{
    char msg[C_FCERR_MSG];
    int BufLen_2 = FLEN_FILENAME - 1;  /* for Uclgst: in/outfile length - 1 */

    Uclgst("infile", infile, status);
    if (*status != 0) {
        sprintf( msg, "could not get infile parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    Uclgst("outfile", outfile, status);
    if (*status != 0) {
        sprintf(msg, "could not get outfile parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
    return(0);
}


/*****************************************************************************
 * The following code is needed so IRAF can call fcopy: no return value (like
 * Fortran subrotuiness) and 5+1 naming convention (not applicable here).
 *****************************************************************************/

#ifdef vms
#define F77CALL fcopy
#endif
#ifdef unix
#define F77CALL fcopy_
#endif

void F77CALL()
{
    int fcopymain();
    int status;

    status = fcopymain();
    return;

}

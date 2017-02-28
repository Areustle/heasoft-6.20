/**********************************************************************
 *
 * arf2arf1 - convert a Type 2 ARF file to a Type 1 ARF file

 * The Ancillary Response File (ARF) is used in combination with the detector
 * Redistribution Matrix File (RMF) by the XSPEC spectral fitting package.  In
 * a Type 2 ARF file multiple ARFs are packed into a single binary table, one
 * per row.  A Type 1 ARF file, on the other hand, only contains a single ARF
 * in each binary table extension.  The process of converting a Type 2 ARF to a
 * Type 1 ARF involves expanding each row of the Type 2 file into a separate
 * binary table extension in the Type 1 file.
 *
 * Initial version written April 2003 by William Pence, NASA/GSFC 662.
 *
 **********************************************************************/

#include <stdio.h>
#include "fitsio.h"
#include "cftools.h"
#include "pfile.h"

#define COL_NAME_LEN 80    /* global variable */
#define MAXRANGES 200      /* allowed maximun number of row ranges */

static int arf2arf1parms(char *infile, char *outfile, char *rows, 
         char *elow_col, char *ehi_col, char *specresp_col, int *status);

int arf2arf1main()
{
    fitsfile *infptr, *outfptr;
    int status = 0, tstatus = 0, keytype;
    int elow = 0, ehi = 0, specresp = 0, arf_num = 0, extver;
    int elow_out, ehi_out, specresp_out, anynull, zero = 0;
    int elow_type, ehi_type, specresp_type;
    long elow_repeat, ehi_repeat, specresp_repeat, vlength;
    long nrows;
    char infile[FLEN_FILENAME], outfile[FLEN_FILENAME], msg[C_FCERR_MSG];
    char newformat[10], elow_tform[40], ehi_tform[40], specresp_tform[40];
    char elow_col[COL_NAME_LEN], ehi_col[COL_NAME_LEN];
    char specresp_col[COL_NAME_LEN], card[81];
    char rows[FLEN_FILENAME];
    char *ttype[3];
    char *tform[3] = {"1E", "1E", "1E"}, *tunit[3] = {"keV", "keV", "cm**2"};
    int numranges, ii, jj, kk, nkeys;
    long minrow[MAXRANGES], maxrow[MAXRANGES];    

    float *values;

    c_ptaskn("arf2arf1 1.0");

    /* get parameters */
    if ( arf2arf1parms(infile, outfile, rows, elow_col, ehi_col, specresp_col,
                       &status) != 0 ) return(status);

    /* define the column names for the output file */
    ttype[0] = elow_col;
    ttype[1] = ehi_col;
    ttype[2] = specresp_col;

    /* open the input ARF2 table */
    if (fits_open_table(&infptr, infile, READONLY, &status) != 0 )
    {
        fits_get_errstatus(status, msg);
        do  c_fcerr(msg);  while (fits_read_errmsg(msg) != 0);
        return(status);
    }

    /* get number of rows (= no. of arfs) */
    fits_get_num_rows(infptr, &nrows, &status);

    /* get positions of the optional energy range columns */
    tstatus = 0;
    fits_get_colnum(infptr, CASEINSEN, elow_col, &elow, &tstatus);
    tstatus = 0;
    fits_get_colnum(infptr, CASEINSEN, ehi_col, &ehi, &tstatus);

    /* get position of the required spectral response column */
    fits_get_colnum(infptr, CASEINSEN, specresp_col, &specresp, &status);

    /* parse list of rows to get no. of ranges, and min and max of each range */
    fits_parse_range(rows, nrows, MAXRANGES, &numranges, minrow, maxrow, 
                     &status);

    /* get ARF_NUM column number, if it exists */
    tstatus = 0;
    fits_get_colnum(infptr, CASEINSEN, "ARF_NUM", &arf_num, &tstatus);

    /* adjust output column numbers if necessary */
    if (arf_num) {
        if (arf_num < elow) elow_out = elow - 1;
        if (arf_num < ehi) ehi_out = ehi - 1;
        if (arf_num < specresp) specresp_out = specresp - 1;
    } else {
        /* output column numbers = input column numbers by default */
        elow_out = elow;
        ehi_out = ehi;
        specresp_out = specresp;
    }

    /* get datatypes and lengths of the  3 vectors */
    if (elow)
    {
       fits_get_coltype(infptr, elow, &elow_type, &elow_repeat, NULL, &status);
       fits_get_coltype(infptr, ehi, &ehi_type, &ehi_repeat, NULL, &status);
    }
    fits_get_coltype(infptr, specresp, &specresp_type, &specresp_repeat,
                     NULL, &status);
    vlength = specresp_repeat;

    /* create the output file */
    if ( fits_create_file(&outfptr, outfile, &status) != 0 )
    {
        fits_close_file(infptr, &status);

        fits_get_errstatus(status, msg);
        do  c_fcerr(msg);  while (fits_read_errmsg(msg) != 0);
        return(status);
    }

    /* do some sanity checks of the input file */
    if (elow)
    {
      if ((elow_repeat != ehi_repeat) || (elow_repeat != specresp_repeat)) 
      {
       c_fcerr(
          "ENERG_LO, ENERG_HI and SPECRESP columns have different lengths");
       status = 1;
      }
    }

    /* copy primary array and any previous extensions to output file */
    fits_copy_file(infptr, outfptr, 1, 0, 0, &status);

    /* allocate temporary array */
    values = (float *) calloc(vlength, sizeof(float)); 

    /* initialize output format string and names of the TFORM keyword */
    strcpy(newformat, "1E");
    sprintf(elow_tform, "TFORM%d", elow);
    sprintf(ehi_tform, "TFORM%d", ehi);
    sprintf(specresp_tform, "TFORM%d", specresp);

    /* now convert each type 2 arf in each row to a type 1 arf */
    for (jj = 0; (jj < numranges) && (!status); jj++)
    {
     for (ii = minrow[jj]; ii <= maxrow[jj]; ii++)
     { 
       fits_create_tbl(outfptr, BINARY_TBL, 0, 3, ttype, tform, tunit, "",
           &status);

       fits_get_hdrspace(infptr, &nkeys, NULL, &status);

       /* copy keywords from input to output file */
       for (kk = 0; kk < nkeys; kk++)
       {
          fits_read_record(infptr, kk+1, card, &status);
          keytype = fits_get_keyclass(card);
          if (keytype > TYP_CMPRS_KEY && keytype != TYP_CKSUM_KEY 
           && keytype != TYP_UNIT_KEY)
              fits_write_record(outfptr, card, &status);
       }

       extver = ii;
       fits_update_key(outfptr, TLONG, "EXTVER", &extver, " ", &status);

       /* read the vectors from the input file and copy to the output file */
       if (elow) {
         fits_read_col(infptr, TFLOAT, elow, ii, 1, vlength, NULL, values, 
                       &anynull, &status);
         fits_write_col(outfptr, TFLOAT, elow_out,1,1, vlength, values,
                        &status);

         fits_read_col(infptr, TFLOAT, ehi, ii, 1, vlength, NULL, values, 
                       &anynull, &status);
         fits_write_col(outfptr, TFLOAT, ehi_out, 1, 1, vlength, values,
                        &status);
       }

       fits_read_col(infptr, TFLOAT, specresp, ii, 1, vlength, NULL, values, 
                     &anynull, &status);
       fits_write_col(outfptr,TFLOAT,specresp_out,1,1,vlength, values, &status);
     }
    }

    free(values);
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
 *  arf2arf1parms -- read the parameters
 *
 ************************************************************************/

static int arf2arf1parms(char *infile, char *outfile, char *rows, char *elow_col,
                         char *ehi_col, char *specresp_col, int *status)
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

    Uclgst("rows", rows, status);
    if (*status != 0) {
        sprintf(msg, "could not get rows parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    BufLen_2 = COL_NAME_LEN - 1;  /* for Uclgst: in/outfile length - 1 */

    Uclgst("energ_lo", elow_col, status);
    if (*status != 0) {
        sprintf(msg, "could not get energ_lo parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    Uclgst("energ_hi", ehi_col, status);
    if (*status != 0) {
        sprintf(msg, "could not get energ_hi parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }

    Uclgst("specresp", specresp_col, status);
    if (*status != 0) {
        sprintf(msg, "could not get specresp parameter, status = %d\n", status);
        c_fcerr(msg);
        return(*status);
    }
    return(0);
}

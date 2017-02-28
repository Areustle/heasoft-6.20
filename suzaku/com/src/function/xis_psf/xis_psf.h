#ifndef _FASTARF_PSF_H_
#define _FASTARF_PSF_H_

#define FASTARF_PSF_NAXIS1    3072
#define FASTARF_PSF_NAXIS2    3072

int xis_psf_init ( char *caldbFile );
int xis_psf ( double offset, double azimuth, double **psf );
int xis_psf_write_fits ( double offset, double azimuth, char *outfile );
void xis_psf_cleanup ( void );

#endif

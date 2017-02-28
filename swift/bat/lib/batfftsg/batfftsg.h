
/* 
 * 
 *  FFT library routines by Tayuka Ooura
 *    ooura@kurims.kyoto-u.ac.jp
 *
 *  Used with permission.
 * 
 *  Single precision version created by C. Markwardt
 *
 *    $Id: batfftsg.h,v 1.1 2002/11/01 17:08:56 craigm Exp $
 *
 */


/* From fftsg.c  - 1D FFT routines */
void cdft(int, int, double *, int *, double *);
void rdft(int, int, double *, int *, double *);
void ddct(int, int, double *, int *, double *);
void ddst(int, int, double *, int *, double *);
void dfct(int, double *, double *, int *, double *);
void dfst(int, double *, double *, int *, double *);

/* From fftsg2d.c   - 2D FFT routines */
void cdft2d(int, int, int, double **, double *, int *, double *);
void rdft2d(int, int, int, double **, double *, int *, double *);
void rdft2dsort(int, int, int, double **);
void ddct2d(int, int, int, double **, double *, int *, double *);
void ddst2d(int, int, int, double **, double *, int *, double *);

/* From sfftsg.c  - float 1D FFT routines */
void scdft(int, int, float *, int *, float *);
void srdft(int, int, float *, int *, float *);
void sddct(int, int, float *, int *, float *);
void sddst(int, int, float *, int *, float *);
void sdfct(int, float *, float *, int *, float *);
void sdfst(int, float *, float *, int *, float *);

/* From sfftsg2d.c  - float 2D FFT routines */
void scdft2d(int, int, int, float **, float *, int *, float *);
void srdft2d(int, int, int, float **, float *, int *, float *);
void srdft2dsort(int, int, int, float **);
void sddct2d(int, int, int, float **, float *, int *, float *);
void sddst2d(int, int, int, float **, float *, int *, float *);

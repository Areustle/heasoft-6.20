/*
*CMZ :  4.21/08 05/01/94  09.03.21  by  Rene Brun
*-- Author :    Fons Rademakers   20/03/91
*-- Modified:   Wojtek Burkot     01/03/92
      INTEGER FUNCTION HMAPM(MFILE, IBASE, IOFFST)
************************************************************************
*                                                                      *
* HMAPM                                                                *
*                                                                      *
* Create a global section. This routine causes the pages at ICOMAD     *
* to be mapped from file MFILE. MFILE must have been created in the    *
* /tmp directory. It returns in IOFFST the offset between ICOMAD and   *
* a base common IBASE (typically the PAWC). On successful completion   *
* this function returns 0. In case of an error -ERRNO is returned.     *
*                                                                      *
* HMAPM is an interface routine that calls the C routine HMAPI.        *
*                                                                      *
* After a global section has been created by HCREATEM, other           *
* processes can use the data in the global section via this function   *
* and HFREEM.                                                          *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) MFILE
cc      INTEGER       ICOMAD(1), IBASE(1), HMAPI
      INTEGER       IBASE(1), HMAPI
      INTEGER * 8   ICOMAD
      SAVE ICOMAD
*
      MFLEN = LENOCC(MFILE)
      HMAPM = HMAPI(MFILE, MFLEN, IBASE, ICOMAD)
      IOFFST = ICOMAD - LOCF(IBASE(1))
cc      Write (*,*) 'IOFFST = ', IOFFST
*
      END
*/

extern int lenocc_(char *mfile, int mfile_len);
extern int hmapi_(char *mfile, int *mflen, int *ibase, unsigned long *icomad);

int
hmapm_(mfile, ibase, ioffst, mfile_len)
  char *mfile;
  int *ibase;
  unsigned *ioffst;
  int mfile_len;
{
	int mflen, istat;
	unsigned long icomad;
	mflen = lenocc_(mfile, mfile_len);
	istat = hmapi_(mfile, &mflen, ibase, &icomad);
	*ioffst = icomad - (((unsigned long)ibase) >> 2);
	return istat;
}

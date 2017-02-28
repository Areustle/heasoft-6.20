/*
*CMZ :  4.21/08 12/01/94  12.49.22  by  Rene Brun
*-- Author :    Fons Rademakers   20/03/91
*-- Modified :  Wojtek Burkot     02/03/91
      INTEGER FUNCTION HCREATEM(MFILE, IBASE, ISIZE, IOFFST)
************************************************************************
*                                                                      *
* HCREATEM                                                             *
*                                                                      *
* Create a global section. This routine causes the pages at ICOMAD     *
* and continuing for at most ISIZE words (1 word = 4 bytes) to be      *
* mapped from file MFILE. MFILE will be created in the /tmp directory. *
* IOFFST is the offset between the address of the common starting at   *
* IBASE and the address of ICOMAD. The space in ICOMAD can then be     *
* addressed like: IBASE(IOFFST+1,...).                                 *
* On successful completion this function returns 0. In case of an      *
* error -ERRNO is returned.                                            *
* HCREATEM is an interface routine that calls the C routine HCREATEI.  *
*                                                                      *
* After a global section has been created by this function, other      *
* processes can use the data in this global section via the functions  *
* HMAPM and HFREEM.                                                    *
* On BSD machines, the shared memory is automatically deleted when     *
* the process who has created it terminates.                           *
* On non-BSD machines, the user has to delete the shared memory via    *
*     CALL HFREEM(0)                                                   *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) MFILE
cc      INTEGER       ICOMAD(1), ISIZE, IBASE(1), HCREATEI
      INTEGER       ISIZE, IBASE(1), HCREATEI
      INTEGER * 8   ICOMAD
      SAVE ICOMAD
*
      MFLEN = LENOCC(MFILE)
      HCREATEM = HCREATEI(MFILE, MFLEN, IBASE, ISIZE, ICOMAD)
      IOFFST = ICOMAD - LOCF(IBASE(1))
cc      Write (*,*) 'IOFFST = ', IOFFST
      END
*/

extern int lenocc_(char *mfile, int mfile_len);
extern int hcreatei_(char *mfile, int *mflen, int *ibase, int *isize, unsigned long *icomad);

int
hcreatem_(mfile, ibase, isize, ioffst, mfile_len)
  char *mfile;
  int *ibase, *isize;
  unsigned *ioffst;
  int mfile_len;
{
	int mflen, istat;
	unsigned long icomad;
	mflen = lenocc_(mfile, mfile_len);
	istat = hcreatei_(mfile, &mflen, ibase, isize, &icomad);
	*ioffst = icomad - (((unsigned long)ibase) >> 2);
	return istat;
}

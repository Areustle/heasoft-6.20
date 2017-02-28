/* ftio.c
 * $Id: ftio.c,v 3.2 2004/06/11 13:25:47 irby Exp $
 *
 * Sun replacements for FORTRAN I/O facilities available on IBM.
 * ---------------------------------------------------------------------------
 * Nand Lal - GSFC - January 91
 * E.S.Panduranga - S.T.X. - 04/18/91
 * ---------------------------------------------------------------------------
 * $Log: ftio.c,v $
 * Revision 3.2  2004/06/11 13:25:47  irby
 * Remove definition of malloc and include stdlib.h instead.  This actually
 * caused a "conflicting types" compilation error for pgplot/pgdispd/proccom.c
 * (with gcc 3.4.0) since stdlib.h was already included through other header
 * files in the code.
 *
 * Revision 3.1  2002/04/16 20:32:08  irby
 * Additions to libgro - previously these codes existed in the following
 * libraries:
 *
 *   libsenstv
 *   libsysutil
 *   libutil
 *   libftio
 *
 * Revision 1.2  1996/09/11  11:24:12  wang
 * Change the 'MAX_UNIT' from 50 to 150.
 *
 * Revision 1.1  1996/03/27  19:15:03  wang
 * Initial revision
 *
 * Revision 1.1  1991/10/08  21:31:57  esp
 * Initial revision
 *
 */

#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

#define	MAX_UNIT 150
#define	IOERROR_Bad_Length 1
#define	IOERROR_Inconsistent_Length 2
#define	BAD_UNIT 3 
#define	OUT_OF_MEMORY 4 
#define	OPEN_FAILURE 5
#define	BAD_BLKSIZE 6
#define	BAD_RECFM 7
#define	OUT_OF_BUFFER_SPACE 8 
#define	WRITE_ERROR 9
#define	END_OF_FILE 10
#define	MAX_BLKSIZE 32760



static char	*rcsid = "$Id: ftio.c,v 3.2 2004/06/11 13:25:47 irby Exp $";

struct FCB {
	char fname[256];
	int infile;
	int tlu;
	int iofunc;
	char recfm[2];
	int lrecl;
	int blksize;
	int mem_acquired;
	char *block_begin;
	char *next_seg;
	char *block_end;
	int  (*readbfun)();
	int  (*readfun)();
	int  (*writebfun)();
} *fcb_ptr[MAX_UNIT+1];

read_vbdisk(fcb) /* Read V/VB block into block_begin and return	length */
struct FCB *fcb;
{
	short bdw[2];
	int toread,nread;

	toread = sizeof(bdw);
	nread = read(fcb->infile,bdw,toread);
	if (nread == 0 ) return(-END_OF_FILE);
	if (nread < 0)	return(nread);
	if (nread < toread) return(IOERROR_Bad_Length);

	toread = bdw[0] - sizeof(bdw);
	nread = read(fcb->infile,fcb->block_begin,toread);
	if (nread == toread)
	{
		fcb->block_end = fcb->block_begin + nread;
		fcb->next_seg =	fcb->block_begin;
		return(nread);
	}
	else
	{
		if (nread <= 1) return(IOERROR_Bad_Length);
		return(-nread);
	}
}
#ifdef FOO
read_vbtape(fcb) /* Read V/VB block from tape into block_begin */
		/* and return length of block */
struct FCB *fcb;
{
	int tlu;
	short bdw;
	int nread;
	tlu = fcb -> tlu;
	nread = tread_(&tlu, fcb->block_begin, fcb -> blksize);
	if (nread <= 0) return(nread);
	if (nread <= 8) return(IOERROR_Bad_Length);
	bdw = *(short *)fcb -> block_begin;

	if (nread == bdw)
	{
		fcb->block_end = fcb->block_begin + nread;
		fcb->next_seg = fcb->block_begin + 4;
		return(nread-4);
	}
	else
	{
		return(IOERROR_Inconsistent_Length);
	}
}
#endif
read_vrec(fcb,area)	/* Read a VB logical record into 'area' and */
			/* return length of the record		    */
struct FCB *fcb;
char **area;
{
	int len, ret_code;

	if (fcb->next_seg == fcb->block_end)
	{
		ret_code = (*(fcb -> readbfun))(fcb);
		if (ret_code <= 0) return(ret_code);
	}

	len = *(short *)fcb->next_seg;
	if ((fcb->next_seg)+len <= fcb->block_end)
	{
		*area = fcb->next_seg+4;
		fcb->next_seg += len;
		return(len-4);
	}
	else
		return(IOERROR_Inconsistent_Length);
	;
}
read_fbdisk(fcb)	/* Read an F/FB block from disk into area pointed by*/
			/* block_begin.  Return 0 if EOF, value returned    */
			/* by read in case of I/O error, length of the 	    */
			/* record if data read is consistent with record    */
			/* attributes, inconsistent length indication  	    */
			/* otherwise.					    */
struct FCB *fcb;
{
	int toread,nread;

	toread = fcb ->	blksize;
	nread = read(fcb -> infile, fcb -> block_begin, toread);
	if (nread <= 0) return(nread);
	if ((nread % fcb -> lrecl) == 0)
	{
		fcb->block_end = fcb->block_begin + nread;
		fcb->next_seg =	fcb->block_begin;
		return(nread);
	}
	else
		return(IOERROR_Inconsistent_Length);
	;
}
#ifdef FOO
read_fbtape(fcb)	/* Read an F/FB block from tape into area pointed by*/
			/* block_begin.  Return 0 if EOF, value returned    */
			/* by read in case of I/O error, length of the 	    */
			/* record if data read is consistent with record    */
			/* attributes, inconsistent length indication  	    */
			/* otherwise.					    */
struct FCB *fcb;
{
	int toread,nread;
	int tlu;

	toread = fcb -> blksize;
	tlu = fcb -> tlu;
	nread = tread_(&tlu, fcb -> block_begin, toread);
	if (nread <= 0) return(nread);

	if ((nread % fcb -> lrecl) == 0)
	{
		fcb->block_end = fcb->block_begin + nread;
		fcb->next_seg =	fcb->block_begin;
		return(nread);
	}
	else
		return(IOERROR_Inconsistent_Length);
	;
}
#endif
read_frec(fcb,area)	/* Returns the address of next record segment in*/
			/* and the length of the segment read.		*/
			/* If EOF or I/O error indication was returned  */
			/* while attempting to read the block, this     */
			/* indication is passed back to the caller.     */
struct FCB *fcb;
char **area;
{
	int len, ret_code;

	if (fcb->next_seg == fcb->block_end)
	{
		ret_code = (*(fcb -> readbfun))(fcb);
		if (ret_code <= 0) return(ret_code);
	}

	len = fcb -> lrecl;
	*area = fcb->next_seg;
	fcb->next_seg += len;
	return(len);
}
read_udisk(fcb,area)
struct FCB *fcb;
char *area;
{
	int toread,nread;

	toread = fcb -> blksize;
	nread = read(fcb -> infile, area, toread);
	return(nread);
}
#ifdef FOO
read_utape(fcb,area)
struct FCB *fcb;
char *area;
{
	int toread,nread;
	int tlu;

	toread = fcb -> blksize;
	tlu = fcb -> tlu;
	nread = tread_(&tlu, area, toread);
	return(nread);
}
#endif
fopen_(func_code,unit,fname,dev,recfm,blksize,lrecl,len_name,len_fm)
int *func_code,*unit;
char fname[],recfm[];
int *dev;
int *blksize,*lrecl;
int len_name,len_fm;
{
	int func;
	struct FCB *fcb;
	char *index();
	char forv;
	int ret_code;
	char *c;
	int labelled;
	int tlu;
	int write_fbudisk();
	int write_fbutape();
	int write_vbdisk();
	int write_vbtape();
	int write_frec();
	int write_vrec();

	switch (*func_code)
	{
		case 1:
			func = O_RDONLY;
			break;
		case 2:
			func = O_WRONLY;
			break;
		case 3:
			func = O_RDWR;
			break;
	}

	if ( abs(*unit) < 1 || abs(*unit) > MAX_UNIT ) return(BAD_UNIT);

	fcb = (struct FCB *)malloc(sizeof(struct FCB));
	if (fcb == NULL) return(OUT_OF_MEMORY);

	fcb -> iofunc = func;

	if (*dev == 0)
	{
		fcb -> tlu = -1;
		strncpy(fcb -> fname,fname,len_name+1);
		c = index(fcb -> fname,' ');
		if (c != NULL) *c = '\0';

		if(func == O_RDONLY)
		  fcb -> infile =	open(fcb -> fname, func,0666);
		else
		  fcb -> infile =	open(fcb -> fname, func|O_CREAT,0666);

		if (fcb -> infile == -1) return(OPEN_FAILURE);
	}
	else
	{
		labelled = (*dev < 0);
		tlu = 0;
		fcb -> tlu = tlu;
/*sb
		ret_code = topen_(&tlu,"/dev/rmt8",&labelled
					      ,strlen("/dev/rmt0"));
*/
		if (ret_code != 0) return(ret_code);
	}

	if (*blksize <= 0) return(BAD_BLKSIZE);
	if (*blksize > MAX_BLKSIZE) return(BAD_BLKSIZE);
	fcb -> blksize = *blksize;

	if (len_fm == 0) return(BAD_RECFM);
	forv = recfm[0];
	if (!isalpha(forv)) return(BAD_RECFM);
	if (islower(forv)) forv	= toupper(forv);

	fcb -> mem_acquired = 0;
	switch (forv)
	{
		case 'F':
			if (*dev == 0)
			{
				if (func == O_RDONLY)
					fcb -> readbfun = read_fbdisk;
			if (func == O_WRONLY)
					fcb -> writebfun = write_fbudisk;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_fbdisk;
					fcb -> writebfun = write_fbudisk;
				}
			}
			if (*dev != 0)
			{
/*sb
				if (func == O_RDONLY)
					fcb -> readbfun = read_fbtape;
				if (func == O_WRONLY)
					fcb -> writebfun = write_fbutape;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_fbtape;
					fcb -> writebfun = write_fbutape;

				}
*/
			}
			if ((len_fm == 1) || (recfm[1] == ' '))
			{
				fcb -> lrecl = *blksize;
			}
			else /* assume blocked */
			{
				fcb -> lrecl = *lrecl;
				fcb -> block_begin = malloc(*blksize);
				if (fcb -> block_begin == NULL)
				{
					return (OUT_OF_BUFFER_SPACE);
				}
				fcb -> mem_acquired = 1;
				fcb -> block_end = fcb -> block_begin;
				fcb -> next_seg = fcb -> block_begin;
				if (func != O_RDONLY)
				{
					fcb -> block_end = fcb -> block_begin+
						fcb -> blksize;
				}
			}
			break;

		case 'V':

			if (*dev == 0)
			{
				if (func == O_RDONLY)
					fcb -> readbfun = read_vbdisk;
				if (func == O_WRONLY)
					fcb -> writebfun = write_vbdisk;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_vbdisk;
					fcb -> writebfun = write_vbdisk;
				}
			}
			if (*dev != 0)
			{
/*sb
				if (func == O_RDONLY)
					fcb -> readbfun = read_vbtape;
				if (func == O_WRONLY)
					fcb -> writebfun = write_vbtape;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_vbtape;
					fcb -> writebfun = write_vbtape;
				}
*/
			}
			fcb -> lrecl = *lrecl;
			fcb -> block_begin = malloc(*blksize);
			if (fcb	-> block_begin == NULL)
			{
				return (OUT_OF_BUFFER_SPACE);
			}
			fcb -> mem_acquired = 1;
			if (func == O_RDONLY)
			{
				fcb -> block_end = fcb -> block_begin;
				fcb -> next_seg = fcb -> block_begin;
			}
			else
			{
				fcb -> block_end = fcb -> block_begin +
						 fcb ->blksize;
				*(int *)fcb -> block_begin = (4 << 16);
				fcb -> next_seg = fcb -> block_begin + 4;
			}
			break;
		default:
			if (*dev == 0)
			{
				if (func == O_RDONLY)
					fcb -> readbfun = read_udisk;
				if (func == O_WRONLY)
					fcb -> writebfun = write_fbudisk;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_udisk;
					fcb -> writebfun = write_fbudisk;
				}
			}
			if (*dev != 0)
			{
/*sb
				if (func == O_RDONLY)
					fcb -> readbfun = read_utape;
				if (func == O_WRONLY)
					fcb -> writebfun = write_fbutape;
				if (func == O_RDWR ) {
					fcb -> readbfun = read_utape;
					fcb -> writebfun = write_fbutape;
				}
*/
			}
	}
	fcb -> recfm[0] = forv;
	fcb -> recfm[1] = ' ';
	if (len_fm > 1)
	{
		if (recfm[1] != ' ') fcb -> recfm[1] = 'B';
	}
	fcb_ptr[abs(*unit)] = fcb;
	return 0;
}
fread_(area,unit,len)
char *area;
int *unit;
int *len;
{
	struct FCB *fcb;
	char forv;
	char *areaptr;
	int nread;

	if ( (abs(*unit) < 1) || (abs(*unit) > MAX_UNIT)) return(BAD_UNIT);
	fcb = fcb_ptr[abs(*unit)];
	forv = fcb -> recfm[0];
	switch (forv)
	{
		case 'F':
			if (fcb -> recfm[1] == ' ')
			{
				fcb -> block_begin = area;
				nread = (*(fcb -> readbfun))(fcb);
			}
			else
			{
				nread = read_frec(fcb,&areaptr);
				if (nread > 0)
				{
					if (*unit < 0)
					{
						area = areaptr;
					}
					else
					{
						bcopy(areaptr,area,nread);
					}
				}
			}
			if (nread > 0)
			{
				*len = nread;
				return(0);
			}
			else
			{
				if (nread == 0) return(1);
				return(2);
			}
			break;
		case 'V':
			nread = read_vrec(fcb,&areaptr);
			if (nread > 0)
			{
				if (*unit < 0)
				{
					area = areaptr;
				}
				else
				{
					bcopy(areaptr,area,nread);
				}
				*len = nread;
				return(0);
			}
			else
			{
				if (nread == 0) return(1);
				if (nread == -END_OF_FILE) return(END_OF_FILE);
				return(2);
			}
			break;
		default:
			nread = (*(fcb -> readbfun))(fcb,area);
			if (nread > 0)
			{
				*len = nread;
				return(0);
			}
			else
			{
				if (nread == 0) return(1);
				return(2);
			}
			break;
	}
}
write_fbudisk(fcb,towrite)
struct FCB *fcb;
int towrite;
{
	int nwritten;

	nwritten = write(fcb -> infile, fcb -> block_begin, towrite);
	if (nwritten != towrite) return(WRITE_ERROR);
	return(0);
}
#ifdef FOO
write_fbutape(fcb,towrite)
struct FCB *fcb;
int towrite;
{
	int nwritten;
	int tlu;

	tlu = fcb -> tlu;
	nwritten = twrite_(&tlu,fcb -> block_begin,towrite);
	if (nwritten != towrite) return(WRITE_ERROR);
	return(0);
}
#endif
write_frec(fcb,area)
struct FCB *fcb;
char *area;
{
	int len, ret_code;

	if (fcb->next_seg == fcb->block_end)
	{
		len = fcb -> blksize;
		ret_code = (*(fcb -> writebfun))(fcb,len);
		if (ret_code != 0) return(ret_code);
		fcb -> next_seg = fcb -> block_begin;
	}
	len = fcb -> lrecl;
	bcopy(area,fcb->next_seg,len);
	fcb->next_seg += len;
	
	return(0);
}
fwrite_(area,unit,len)
char *area;
int *unit;
int *len;
{
	struct FCB *fcb;
	char forv;
	char *areaptr;
	int nwritten;
	int towrite;
	int ret_code;

	if ( (abs(*unit) < 1) || (abs(*unit) > MAX_UNIT)) return(BAD_UNIT);
	fcb = fcb_ptr[abs(*unit)];
	forv = fcb -> recfm[0];
	switch (forv)
	{
		case 'F':
			if (fcb -> recfm[1] == ' ')
			{
				fcb -> block_begin = area;
				towrite = fcb -> blksize;
				ret_code = (*(fcb -> writebfun))(fcb,towrite);
			}
			else
			{
				ret_code = write_frec(fcb,area);
			}
			return(ret_code);
			break;
		case 'V':
			if (*unit < 0)
			{
				bcopy(area,&areaptr,4);
			}
			else
			{
				areaptr = area;
			}
			return(write_vrec(fcb,areaptr,*len));
			break;
		default:
			fcb -> block_begin = area;
			return(*(fcb -> writebfun))(fcb,*len);
			break;
	}
}
write_vbdisk(fcb)
struct FCB *fcb;
{
	int towrite,nwritten;

	towrite = *(short *)fcb -> block_begin;
	nwritten = write(fcb -> infile, fcb -> block_begin, towrite);
	if (nwritten != towrite) return(WRITE_ERROR);
	return(0);
}
#ifdef FOO
write_vbtape(fcb)
struct FCB *fcb;
{
	int towrite,nwritten;
	int tlu;

	tlu = fcb -> tlu;
	towrite = *(short *)fcb -> block_begin;
	nwritten = twrite_(&tlu, fcb -> block_begin, towrite);
	if (nwritten != towrite) return(WRITE_ERROR);
	return(0);
}
#endif
write_vrec(fcb,area,len)
struct FCB *fcb;
char *area;
int len;
{
	int towrite, ret_code;

	towrite = (len + 4);
	if (fcb->next_seg + towrite > fcb->block_end)
	{
		ret_code = (*(fcb -> writebfun))(fcb);
		if (ret_code != 0) return(ret_code);
		fcb -> next_seg = fcb -> block_begin;
		*(int *)fcb -> block_begin = (4 << 16);
		fcb -> next_seg += 4;
	}

	*(int *)fcb -> next_seg = (towrite << 16);
	bcopy(area,(fcb->next_seg)+4,len);
	*(short *)fcb -> block_begin += towrite;
	fcb -> next_seg += towrite;
	return(0);
}

/* REWIND: Positions the read/write pointer to the beginning of file.
	E.S.Panduranga	06/28/91
*****************************************************************************/
rewind_(unit)
int	*unit;
{
	struct FCB	*fcb;
	int		returnCode;

	if ( (abs(*unit) < 1) || (abs(*unit) > MAX_UNIT)) return(BAD_UNIT);
	fcb = fcb_ptr[abs(*unit)];
	returnCode = lseek(fcb->infile, 0l, SEEK_SET);
	if(returnCode < 0)
	  return(2);
	else
	  return(0);
}

fclose_(unit)
int *unit;
{
	struct FCB *fcb;
	char forv;
	int ret_code;
	int len;
	int tlu;

	if ( (abs(*unit) < 1) || (abs(*unit) > MAX_UNIT)) return(BAD_UNIT);
	fcb = fcb_ptr[abs(*unit)];

	if (fcb -> iofunc == O_RDONLY)
	{
		if (fcb -> tlu < 0)
		{
			ret_code = close(fcb->infile);
		}
		else
		{
			tlu = fcb -> tlu;
			ret_code = tclose_(&tlu);
		}
		return(ret_code);
	}

	forv = fcb -> recfm[0];
	ret_code = 0;
	switch (forv)
	{
		case 'F':
			if (fcb -> recfm[1] != ' ')
			{
				len = (fcb -> next_seg - fcb -> block_begin);
				ret_code = (*(fcb -> writebfun))(fcb,len);
			}
			break;
		case 'V':
			if (fcb -> recfm[1] != ' ')
				len = *(short *)fcb -> block_begin;
				if (len > 4)
					ret_code = (*(fcb -> writebfun))(fcb);
				break;
		default:
			break;
			
	}

	if (ret_code == 0)
	{
		if (fcb -> tlu < 0)
		{
			ret_code = close(fcb->infile);
		}
		else
		{
			tlu = fcb -> tlu;
			ret_code = tclose_(&tlu);
		}
	}
	if (ret_code == 0)
	{
		if (fcb -> mem_acquired != 0)
		{
			free(fcb -> block_begin);
		}
		free((char *)fcb);
		fcb_ptr[abs(*unit)] = NULL;
	}
	return(ret_code);
}

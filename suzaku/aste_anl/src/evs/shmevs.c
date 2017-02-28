/* $Id: shmevs.c,v 1.7 2007/04/30 21:25:06 ishisaki Exp $
c **********************************************************************
c *                                                                    *
c *  EVent Selection subroutine PACKAGE by T.TAKAHASHI                 *
c *                      version 1     (1983.4.12)                     *
c *                      version 2     (1983.9  8)                     *
c *                      version 2.2   (1983.10.5)                     *
c *                      version 2.3   (1983.10.11) at LBL delete      *
c *                      version 2.3   (1984.10.11)      ERRTRA        *
c *                      version 2.4   (1984.10.25) add EVScum         *
c *                      version 2.5   (1984.10.31) Call evsiz         *
c *                              internally ( in EVSdef )              *
c *                      version 2.6   (1984.11.1 ) add EVSout_def     *
c *                                     to allow output specified key  *
c *                                     add EVSCLR_ALL                 *
c *                      version 2.7   (1984.12.13) mod EVScum         *
c *         initialize          : EvsIz                                *
c *         flag  definition    : EvsDef(key)                          *
c *         flag  set           : EvsSet(key)                          *
c *                             : EvsVal(key,logic)                    *
c *         flag  clear         : EvsClr(key)                          *
c *         flag  clear all     : EvsClrAll                            *
c *         flag  check         : EVS(key)    .true. if flag is set    *
c *         flag  accumurate    : EvsAcm                               *
c *         output results of event selection : EvsOut(lun)            *
c *         get accumurated EVS number : EvsNum( key ,num)             *
c *         return # of defined : EvsNdf                               *
c *         check if key exists : EvsIsDef(key)                        *
c *         internal routines   : EvsKey : search key and return index *
c *                                                                    *
c **********************************************************************
c *                                                                    *
c * EVS MANUAL ( this manual is wriiten by Taku for KMU analysis )     *
c * To handle event selections.                                        *
c * Each event selection has its own name within 32 characters.        *
c * The value of the selection can be set, reset, or refered by        *
c * its name from any routines.                                        *
c *   1 First, declare the name of event selection.                    *
c *     call EVSDEF( name )                                            *
c *     will define the name ( charcter string within 32 chars ).      *
c *   2 There are three ways to set the value of event selection.      *
c *   2.1 EVSSET( name )                                               *
c *       call EVSSET( name )                                          *
c *       will set the value of the specified event selection to       *
c *       .TRUE. .                                                     *
c *   2.2 EVSCLR( name )                                               *
c *       call EVSCLR( name )                                          *
c *       will set the value of the name to .FALSE. ;                  *
c *       in other word, 'clear'.                                      *
c *   2.3 EVSVAL( name, logic )                                        *
c *       call EVSVAL( name, logic )                                   *
c *       will set the value of the event selection to the value       *
c *       given by 'logic'. 'logic' is a logical expression, as        *
c *       .TRUE., .FALSE., logical variable, or logical                *
c *       combination of logical variables.                            *
c *   3 In order to get the value of the event selection, use          *
c *     logical function EVS.                                          *
c *     EVS( name ) returns the value of the specified event           *
c *     selection. For example, you can use EVS as ...                 *
c *     if( EVS( 'ALL_CHAMBERS_HIT' ) ) then                           *
c *       ......                                                       *
c *     or                                                             *
c *     call EVSVAL( 'PC1V_IS_OK', ( MULT(1).GT.0 ) )                  *
c *     call EVSVAL( 'PC1H_IS_OK', ( MULT(2).GT.0 ) )                  *
c *     call EVSVAL( 'PC1_IS_OK',                                      *
c *    &             ( EVS('PC1V_IS_OK') .and. EVS('PC1H_IS_OK') ) )   *
c *     ...                                                            *
c *     and so on.                                                     *
c *                                                                    *
c *     Do not forget to declare the function EVS as logical ;         *
c *     put the next statement in a declaration part in each routine   *
c *     which uses EVS.                                                *
c *       logical EVS              EVS is a logical function           *
c *                                                                    *
c **********************************************************************

	1997/02/24	Y.ISHISAKI
		rewrite to C for ASTE_ANL

	1997/05/07	Y.ISHISAKI
		add EvsShmClose, EvsEnd

	2003/09/29	Y.ISHISAKI
		check if already initialized in EvsIz() & EvsShmOpen()
		close shared memory in EvsEnd()
		if EVS already initialized, copy it in EvsShmCreate()

	2005/11/28 Y.ISHISAKI	version 1.70 (EVS Ver.3.2)
		use anl_msg_error(), anl_msg_warning(), anl_msg_debug()
		add EvsIsDef(), remove obsolete EvsKID()

	2006/08/10 Y.ISHISAKI	version 1.72 (EVS Ver.3.3)
		null terminate name in EvsOut() when name length reach EVS_MAXNAM

	2007/04/30 Y.ISHISAKI	version 1.80 (EVS Ver.3.4)
		change anl_msg_debug() -> anl_msg_debug3() [actually, not used]
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "anl.h"
#include "evs.h"

#define EVS_MAXKEY	500		/* maximum key number */
#define EVS_MAXNAM	32		/* maximum key length */

static int malloc_flag = 0;

static struct EVS {
	int nalloc;
	int ndef;
	int index_table[EVS_MAXKEY];		/* sorted by name */
	struct {
		char name[EVS_MAXNAM];	/* keyname */
		double flag;			/* 0 : false, 1 : true */
		double num;				/* number of accumurate */
    } record[EVS_MAXKEY];
} *evs;

static int shm_fd = -1;

static void *
mmap_file_open(char *shm_file, int *shm_len)
{
	void *paddr;
	struct stat stat_buf;

	shm_fd = open(shm_file, O_RDWR);
	if ( shm_fd < 0 ) {
		perror("open");
		return NULL;
	}
	stat(shm_file, &stat_buf);
	*shm_len = stat_buf.st_size;
	paddr = mmap(evs, *shm_len,
				 PROT_READ|PROT_WRITE,
				 MAP_SHARED,
				 shm_fd, 0);
	if ( NULL == paddr ) {
		perror("mmap");
		close(shm_fd);
		return NULL;
	}
	return paddr;
}

static void *
mmap_file_create(char *shm_file, int shm_len)
{
	void *paddr;
	char buf[1024];
	int len;

	shm_fd = open(shm_file, O_RDWR|O_CREAT, 0666);
	if ( shm_fd < 0 ) {
		perror("open");
		return NULL;
	}
	/* reserve space in file */
	memset(buf, 0, sizeof(buf));
	len = shm_len;
	while ( len ) {
		int istat, write_size;
		write_size = len;
		if ( sizeof(buf) < len ) {
			write_size = sizeof(buf);
		}
		istat = write(shm_fd, buf, write_size);
		if ( istat != write_size ) {
			perror("write");
			close(shm_fd);
			return NULL;
		}
		len -= write_size;
	}
	paddr = mmap(evs, shm_len,
				 PROT_READ|PROT_WRITE,
				 MAP_SHARED,
				 shm_fd, 0);
	if ( NULL == paddr ) {
		perror("mmap");
		close(shm_fd);
		return NULL;
	}
	return paddr;
}

static int evs_shm_size = 0;
static char *evs_shm_file = NULL;
static char evs_shm_file_buf[1024];

int
EvsShmCreate(char *shm_file)
{
	int alloc_size, i, len;
	struct EVS *old_evs = NULL;

	if ( malloc_flag ) {
		old_evs = evs;
	} else if ( evs_shm_size ) {
		old_evs = malloc(evs_shm_size);
		if ( NULL == old_evs ) {
			return ANL_NG;
		}
		memcpy(old_evs, evs, evs_shm_size);	/* temporary copy to local memory*/
		EvsShmClose();
		malloc_flag = 1;
	}

	alloc_size = sizeof(*evs);
	evs = mmap_file_create(shm_file, sizeof(*evs));
	if ( NULL == evs ) {
		return ANL_NG;
	}
	evs_shm_size = alloc_size;
	len = strlen(shm_file);
	if ( len + 1 < sizeof(evs_shm_file_buf) ) {
		evs_shm_file = evs_shm_file_buf;
	} else {
		evs_shm_file = malloc(strlen(shm_file) + 1);
	}
	if ( NULL == evs_shm_file ) {
		evs_shm_file = shm_file;
		EvsShmClose();
		evs = old_evs;
		return ANL_NG;
	}
	strcpy(evs_shm_file, shm_file);
	evs->nalloc = EVS_MAXKEY;
	evs->ndef = 0;
	for (i = 0; i < evs->nalloc; i++) {
		evs->record[i].num = 0.0;
		evs->record[i].flag = 0.0;
    }

	if ( NULL != old_evs ) {	/* if EVS already initialized, copy it */
		*evs = *old_evs;
		free(old_evs);
		malloc_flag = 0;
	}

	return ANL_OK;
}

int
EvsShmOpen(char *shm_file)
{
	int alloc_size;

	if ( malloc_flag || evs_shm_size ) {
		anl_msg_warning("\
EVS: WARNING: already initialized, clear all EVS keys\n");
		EvsEnd();
	}
	evs = mmap_file_open(shm_file, &alloc_size);
	if ( NULL == evs ) {
		return ANL_NG;
	}
	evs_shm_size = alloc_size;
	return ANL_OK;
}

int
EvsShmClose(void)
{
	if ( 0 < evs_shm_size ) {
		if ( munmap(evs, sizeof(*evs)) < 0 ) {
			perror("munmap");
			return ANL_NG;
		}
		evs = NULL;
		evs_shm_size = 0;
	}

	if ( 0 <= shm_fd ) {
		if ( close(shm_fd) < 0 ) {
			perror("close");
			return ANL_NG;
		}
		shm_fd = -1;
	}

	if ( NULL != evs_shm_file ) {
		if ( unlink(evs_shm_file) < 0 ) {
			perror("unlink");
			return ANL_NG;
		}
		evs_shm_file = NULL;
	}

	return ANL_OK;
}

/*
c EVSIZ    initialize routine
*/
int
EvsIz(void)
{
	int i;

	if ( malloc_flag || evs_shm_size ) {
		anl_msg_warning("\
EVS: WARNING: already initialized, clear all EVS keys\n");
		EvsEnd();
	}

	evs = malloc(sizeof(*evs));
	if ( NULL == evs ) {
		return ANL_NG;
	}
	malloc_flag = 1;
	evs->nalloc = EVS_MAXKEY;
	evs->ndef = 0;
	for (i = 0; i < evs->nalloc; i++) {
		evs->record[i].num = 0.0;
		evs->record[i].flag = 0.0;
    }
	return ANL_OK;
}

int
EvsEnd(void)
{
	if ( malloc_flag ) {
		free(evs);
		malloc_flag = 0;
		evs = NULL;
	} else if ( evs_shm_size ) {
		return EvsShmClose();
	}
	return ANL_OK;
}

/*
c EVSclr_all clear result with all defined key
*/
void
EvsClrAll(void)
{
	int i;

	for (i = 0; i < evs->ndef; i++) {
		evs->record[i].flag = 0.0;
    }
}

/*
c EVSrst_all reset result with all defined key
*/
void
EvsRstAll(void)
{
	int i;

	for (i = 0; i < evs->ndef; i++) {
		evs->record[i].flag = 0.0;
		evs->record[i].num = 0.0;
    }
}

/*
c EVSrst  reset result with specified key
*/
int
EvsRst(char *key)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag = 0.0;
	evs->record[index].num = 0.0;
	return ANL_OK;
}

/*
c EvsAcm  accumulate if flag is set
*/
void
EvsAcm(void)
{
	int i;

	for (i = 0; i < evs->ndef; i++) {
		evs->record[i].num += evs->record[i].flag;
		evs->record[i].flag = 0.0;
    }
}

/*
c EvsDef  EVS definition
*/
int
EvsDef(char *key)
{
	int i, eq, at, bottom;
	int *index_table = evs->index_table;

	bottom = evs->ndef;
	if ( 0 == bottom ) {
		at = 0;
	} else if ( evs->nalloc == bottom ) {
		anl_msg_error("\
EVS: too many definition in EVSdef: %d\n", bottom);
		return ANL_NG;
	} else {
		for (at = 0; at < bottom; at++) {
			eq = strncmp(key, evs->record[index_table[at]].name, EVS_MAXNAM);
			if ( eq < 0 ) {
				break;
			} else if ( 0 == eq ) {
				anl_msg_error("\
EVS: double definition occur in EVSdef: %s\n", key);
				return ANL_NG;
			}
		}
		if ( at < bottom ) {
			for (i = bottom; at < i; i--) {
				index_table[i] = index_table[i-1];
            }
        }
    }
	index_table[at] = bottom;
	for (i = 0; key[i] && i < EVS_MAXNAM; i++) {
		evs->record[bottom].name[i] = key[i];
	}
	if ( i < EVS_MAXNAM ) {
		evs->record[bottom].name[i] = '\0';
	}
	if ( key[i] ) {
		anl_msg_error("\
EVS: keyword length exceed EVS_MAXNAM for %s\n", key);
	}
	evs->record[bottom].num = 0.0;
	evs->record[bottom].flag = 0.0;
	evs->ndef++;
	return ANL_OK;
}

/*
c EvsKey  search key and return index
*/
int
EvsKey(char *key, int *index)
{
	int from, to, at, result;
	int *index_table = evs->index_table;

	from = 0;
	to = evs->ndef - 1;
	while ( from <= to ) {
		at = (from + to) / 2;
		result = strncmp(key, evs->record[index_table[at]].name, EVS_MAXNAM);
		if ( 0 <= result ) {
			from = at + 1;
        }
		if ( result <= 0 ) {
			to = at - 1;
        }
    }
	if ( from == to + 2 ) {
		*index = index_table[(from + to) / 2];
		return ANL_TRUE;
	}
	*index = -1;
	anl_msg_error("\
EVS: illegal keyword detected ---> %s\n", key);
	return ANL_FALSE;
}

/*
c EvsIsDef  check if key exists
*/
int
EvsIsDef(char *key)
{
	int from, to, at, result;
	int *index_table = evs->index_table;

	from = 0;
	to = evs->ndef - 1;
	while ( from <= to ) {
		at = (from + to) / 2;
		result = strncmp(key, evs->record[index_table[at]].name, EVS_MAXNAM);
		if ( 0 <= result ) {
			from = at + 1;
        }
		if ( result <= 0 ) {
			to = at - 1;
        }
    }
	if ( from == to + 2 ) {
		return ANL_TRUE;
	}
	return ANL_FALSE;
}

/*
c EvsNdf  return # of definition
*/
int
EvsNdf(void)
{
	return evs->ndef;
}

/*
c EvsOut  output results of event selection
*/
void
EvsOut(void)
{
	int i;
	char name[EVS_MAXNAM+1];

	name[EVS_MAXNAM] = '\0';	/* required to terminate string */
	anl_flush();
	printf("\
\n\
*** results of Event selection *** < Number of selects : %4d > (EVS Ver.3.4)\n\
\n\
", evs->ndef);
	for (i = 0; i < evs->ndef; i++) {
		strncpy(name, evs->record[i].name, EVS_MAXNAM);
		printf("\
 %10.0f : %s\n", evs->record[i].num, name);
    }
	anl_flush();
}

int
EvsVal(char *key, int logic)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag = logic ? 1.0 : 0.0;
	return ANL_OK;
}

int
EvsSet(char *key)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag = 1.0;
	return ANL_OK;
}

int
EvsClr(char *key)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag = 0.0;
	return ANL_OK;
}

int
Evs(char *key)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	return (0.0 == evs->record[index].flag) ? ANL_FALSE : ANL_TRUE;
}

int
EvsPut(char *key, double val)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag = val;
	return ANL_OK;
}

int
EvsAdd(char *key, double val)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	evs->record[index].flag += val;
	return ANL_OK;
}

int
EvsGet(char *key, double *val)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	*val = evs->record[index].flag;
	return ANL_OK;
}

int
EvsNum(char *key, double *num)
{
	int index;

	if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	}
	*num = evs->record[index].num;
	return ANL_OK;
}

int
EvsfVal(char *key, int *index_ptr, int logic)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	evs->record[index].flag = logic ? 1.0 : 0.0;
	return ANL_OK;
}

int
EvsfSet(char *key, int *index_ptr)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	evs->record[index].flag = 1.0;
	return ANL_OK;
}

int
EvsfClr(char *key, int *index_ptr)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	evs->record[index].flag = 0.0;
	return ANL_OK;
}

int
Evsf(char *key, int *index_ptr)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	return (0.0 == evs->record[index].flag) ? ANL_FALSE : ANL_TRUE;
}

int
EvsfPut(char *key, int *index_ptr, double val)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	evs->record[index].flag = val;
	return ANL_OK;
}

int
EvsfAdd(char *key, int *index_ptr, double val)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	evs->record[index].flag += val;
	return ANL_OK;
}

int
EvsfGet(char *key, int *index_ptr, double *val)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	*val = evs->record[index].flag;
	return ANL_OK;
}

int
EvsfNum(char *key, int *index_ptr, double *num)
{
	int index;

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == EvsKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	*num = evs->record[index].num;
	return ANL_OK;
}

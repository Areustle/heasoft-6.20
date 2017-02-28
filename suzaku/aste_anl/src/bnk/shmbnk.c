/* $Id: shmbnk.c,v 1.9 2007/04/30 21:25:02 ishisaki Exp $
c **********************************************************************
c *                                                                    *
c *  mini Data Bank system     by T.TAKAHASHI                          *
c *                                    (1983.5.27/5.29/6.4)            *
c *         initialize          : BnkIni                               *
c *         BNK key definition  : BnkDef(key,size)                     *
c *         assign key to predefined (key & data)                      *
c *                             : BnkEqv(key1,size,key,start_pointer   *
c *         put data            : BnkPut(key,size,array)               *
c *         get data            : BnkGet(key,size,used_size,array)     *
c *         list key            : BnkLst                               *
c *         return # of defined : BnkNdf                               *
c *         check if key exists : BnkIsDef(key)                        *
c *         internal routines   : BnkKey : search key and return index *
c *                                                                    *
c **********************************************************************

	1997/02/24	Y.ISHISAKI
		rewrite to C for ASTE_ANL

	1997/05/07	Y.ISHISAKI
		add BnkShmClose, BnkEnd

	1998/07/22	Y.ISHISAKI
		count number of put/get

	2003/09/29	Y.ISHISAKI
		check if already initialized in BnkIni() & BnkShmOpen()
		close shared memory in BnkEnd()
		if BNK already initialized, copy it in BnkShmCreate()

	2005/11/28 Y.ISHISAKI	version 1.70 (BNK Ver.3.2)
		use anl_msg_error(), anl_msg_warning(), anl_msg_info(), anl_msg_debug()
		show warning when size mismatch & 1!=start in BNKEQV
		show warning/debug message when BNKGET without BNKPUT
		show debug message when data size mismatch in BNKGET & BNKPUT
		add BnkIsDef(), BnkNdf()

	2006/08/10 Y.ISHISAKI	version 1.72 (BNK Ver.3.3)
		null terminate name in BnkLst() when name length reach BNK_MAXNAM

	2007/04/30 Y.ISHISAKI	version 1.80 (BNK Ver.3.4)
		change anl_msg_debug() -> anl_msg_debug3()
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>		/* defines htonl(), etc */
#include "inet.h"
#include "anl.h"
#include "bnk.h"
#include "sized_io.h"

#define BNK_MAXKEY	5000	/* maximum key number */
#define BNK_MAXNAM	32		/* maximum key length */
#define BNK_ALIGN	16

#define BNK_SINGLE	0
#define BNK_FAMILY	1
#define BNK_EXPORT	2

#define BNK_DEFAULT_PORT	5000

static int malloc_flag = 0;
static int client_fd = -1;

static struct BNK {
	int nalloc;				/* number of allocation (= BNK_MAXKEY) */
	int ndef;				/* number of definition */
	int buffer_size;		/* total buffer size (allocated by user) */
	int buffer_ptr;			/* used buffer area */
	int index_table[BNK_MAXKEY];		/* sorted by name */
	struct {
		char name[BNK_MAXNAM];
		int alloc_size;		/* maximum data size for key */
		int used_size;		/* actually stored data size */
		int buffer_pos;		/* pointer into buffer */
		int attrib;			/* attribute (1:FAMILY 2:EXPORT) */
		int fd;				/* file descripter to connect BnkServer */
		double num_put;		/* number of put */
		double num_get;		/* number of get */
	} record[BNK_MAXKEY];
} *bnk;

static int
align(int size)
{
	return (size + BNK_ALIGN - 1) / BNK_ALIGN * BNK_ALIGN;
}

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
	paddr = mmap(bnk, *shm_len,
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
	paddr = mmap(bnk, shm_len,
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

static int bnk_shm_size = 0;
static char *bnk_shm_file = NULL;
static char bnk_shm_file_buf[1024];

int
BnkShmCreate(int buffer_size, char *shm_file)
{
	int alloc_size, len;
	struct BNK *old_bnk = NULL;

	if ( malloc_flag ) {
		old_bnk = bnk;
	} else if ( bnk_shm_size ) {
		old_bnk = malloc(bnk_shm_size);
		if ( NULL == old_bnk ) {
			return ANL_NG;
		}
		memcpy(old_bnk, bnk, bnk_shm_size);	/* temporary copy to local memory*/
		BnkShmClose();
		malloc_flag = 1;
	}

	alloc_size = align(sizeof(*bnk)) + align(buffer_size);
	bnk = mmap_file_create(shm_file, alloc_size);
	if ( NULL == bnk ) {
		bnk = old_bnk;
		return ANL_NG;
	}
	bnk_shm_size = alloc_size;
	len = strlen(shm_file);
	if ( len + 1 < sizeof(bnk_shm_file_buf) ) {
		bnk_shm_file = bnk_shm_file_buf;
	} else {
		bnk_shm_file = malloc(strlen(shm_file) + 1);
	}
	if ( NULL == bnk_shm_file ) {
		bnk_shm_file = shm_file;
		BnkShmClose();
		bnk = old_bnk;
		return ANL_NG;
	}
	strcpy(bnk_shm_file, shm_file);
	bnk->nalloc = BNK_MAXKEY;
	bnk->ndef = 0;
	bnk->buffer_size = buffer_size;
	bnk->buffer_ptr = 0;

	if ( NULL != old_bnk ) {	/* if BNK already initialized, copy it */
		if ( old_bnk->buffer_ptr <= buffer_size ) {
			char *bufp = (char*)bnk + align(sizeof(*bnk));
			char *old_bufp = (char*)old_bnk + align(sizeof(*old_bnk));
			*bnk = *old_bnk;
			if ( 0 < old_bnk->buffer_ptr ) {
				memcpy(bufp, old_bufp, old_bnk->buffer_ptr);
			}
			bnk->buffer_size = buffer_size;
			free(old_bnk);
			malloc_flag = 0;
		} else {
			anl_msg_error("\
BNK: too small buffer_size (%d)\n", buffer_size);
			BnkShmClose();
			bnk = old_bnk;
			return ANL_NG;
		}
	}

	return ANL_OK;
}

int
BnkShmOpen(char *shm_file)
{
	int alloc_size;

	if ( malloc_flag || bnk_shm_size ) {
		anl_msg_warning("\
BNK: WARNING: already initialized, clear all BNK keys\n");
		BnkEnd();
	}
	bnk = mmap_file_open(shm_file, &alloc_size);
	if ( NULL == bnk ) {
		return ANL_NG;
	}
	bnk_shm_size = alloc_size;
	return ANL_OK;
}

int
BnkShmClose(void)
{
	if ( 0 < bnk_shm_size ) {
		if ( munmap(bnk, bnk_shm_size) < 0 ) {
			perror("munmap");
			return ANL_NG;
		}
		bnk = NULL;
		bnk_shm_size = 0;
	}

	if ( 0 <= shm_fd ) {
		if ( close(shm_fd) < 0 ) {
			perror("close");
			return ANL_NG;
		}
		shm_fd = -1;
	}

	if ( NULL != bnk_shm_file ) {
		if ( unlink(bnk_shm_file) < 0 ) {
			perror("unlink");
			return ANL_NG;
		}
		bnk_shm_file = NULL;
	}

	return ANL_OK;
}

int
BnkIni(int buffer_size)
{
	int alloc_size;

	if ( malloc_flag || bnk_shm_size ) {
		anl_msg_warning("\
BNK: WARNING: already initialized, clear all BNK keys\n");
		BnkEnd();
	}

	alloc_size = align(sizeof(*bnk)) + align(buffer_size);
	bnk = malloc(alloc_size);
	if ( NULL == bnk ) {
		return ANL_NG;
	}
	malloc_flag = 1;
	bnk->nalloc = BNK_MAXKEY;
	bnk->ndef = 0;
	bnk->buffer_size = buffer_size;
	bnk->buffer_ptr = 0;
	return ANL_OK;
}

int
BnkEnd(void)
{
	if ( malloc_flag ) {
		free(bnk);
		malloc_flag = 0;
		bnk = NULL;
	} else if ( bnk_shm_size ) {
		return BnkShmClose();
	}
	return ANL_OK;
}

int
BnkDef(char *key, int size)
{
	int i, eq, at, bottom;
	int *index_table = bnk->index_table;

	bottom = bnk->ndef;
	if ( size <= 0 ) {
		anl_msg_error("\
BNK: data size must be grater than 1: %s\n", key);
		return ANL_NG;
    }
	if ( 0 == bottom ) {
		at = 0;
    } else if ( bnk->nalloc == bottom ) {
		anl_msg_error("\
BNK: too many definition in BNKdef: %d\n", bnk->buffer_size);
		return ANL_NG;
    } else {
		for (at = 0; at < bottom; at++) {
			eq = strncmp(key, bnk->record[index_table[at]].name, BNK_MAXNAM);
			if ( eq < 0 ) {
				break;
			} else if ( 0 == eq ) {
				anl_msg_error("\
BNK: double definition occur in BNKdef: %s\n", key);
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
	for (i = 0; key[i] && i < BNK_MAXNAM; i++) {
		bnk->record[bottom].name[i] = key[i];
	}
	if ( i < BNK_MAXNAM ) {
		bnk->record[bottom].name[i] = '\0';
	}
	if ( key[i] ) {
		anl_msg_error("\
BNK: keyword length exceed BNK_MAXNAM for %s\n", key);
	}
	bnk->record[bottom].alloc_size = size;
	bnk->record[bottom].used_size = 0;
	bnk->record[bottom].buffer_pos = bnk->buffer_ptr;
	bnk->record[bottom].attrib = BNK_SINGLE;
	bnk->record[bottom].fd = -1;
	bnk->record[bottom].num_put = 0.0;
	bnk->record[bottom].num_get = 0.0;
	bnk->buffer_ptr += align(size);
	if ( bnk->buffer_size < bnk->buffer_ptr ) {
		anl_msg_error("\
BNK: you must expand bufsize in BNKINI (now size = %d)\n", bnk->buffer_ptr);
		return ANL_NG;
	}
	bnk->ndef++;
	return ANL_OK;
}

int
BnkKey(char *key, int *index)
{
	int from, to, at, result;
	int *index_table = bnk->index_table;

	from = 0;
	to = bnk->ndef - 1;
	while ( from <= to ) {
		at = (from + to) / 2;
		result = strncmp(key, bnk->record[index_table[at]].name, BNK_MAXNAM);
		if ( 0 <= result ) {
			from = at + 1;
        }
		if ( result <= 0 ) {
			to = at - 1;
        }
    }
	if ( from == to + 2 ) {
		*index = index_table[(from + to) / 2];
		return ANL_OK;
	}
	*index = -1;
	anl_msg_error("\
BNK: illegal keyword detected ---> %s\n", key);
	return ANL_NG;
}

int
BnkIsDef(char *key)
{
	int from, to, at, result;
	int *index_table = bnk->index_table;

	from = 0;
	to = bnk->ndef - 1;
	while ( from <= to ) {
		at = (from + to) / 2;
		result = strncmp(key, bnk->record[index_table[at]].name, BNK_MAXNAM);
		if ( 0 <= result ) {
			from = at + 1;
        }
		if ( result <= 0 ) {
			to = at - 1;
        }
    }
	if ( from == to + 2 ) {
		return ANL_OK;
	}
	return ANL_NG;
}

int
BnkNdf(void)
{
	return bnk->ndef;
}

int
BnkEqv(char *new_key, int size, char *old_key, int start)
{
	int old_index, new_index, alloc_size, old_ptr, cur_ptr;

	if ( ANL_NG == BnkKey(old_key, &old_index) ) {
		return ANL_NG;
    }
	if ( size <= 0 ) {
		anl_msg_error("\
BNK: data size must be grater than 1: %s\n", new_key);
		return ANL_NG;
    }
	alloc_size = bnk->record[old_index].alloc_size;
	if ( alloc_size < start + size - 1 ) {
		anl_msg_error("\
BNK: data size exceeds for %s in BNKEQV: %s\n", old_key, new_key);
		return ANL_NG;
    }
	if ( 1 != start ) {
		anl_msg_warning("\
BNK: WARNING: start=%d is not recommended for BNKEQV: %s\n", start, new_key);
	} else if ( size != alloc_size ) {
		anl_msg_warning("\
BNK: WARNING: DATA size mismatch in BNKEQV: %s\n\
       define -> %d\n\
       now    -> %d\n\
", new_key, alloc_size, size);
	}
	old_ptr = bnk->record[old_index].buffer_pos;
	bnk->record[old_index].attrib |= BNK_FAMILY;
	cur_ptr = bnk->buffer_ptr;		/* store current bottom pointer */
	BnkDef(new_key, size);			/* register new_key in key table */
	bnk->buffer_ptr = cur_ptr;		/* remember pointer */
	if ( ANL_OK == BnkKey(new_key, &new_index) ) {
		bnk->record[new_index].buffer_pos = old_ptr + start - 1;
		bnk->record[new_index].attrib |= BNK_FAMILY;
    }

	if ( (BNK_EXPORT & bnk->record[old_index].attrib) ) {
		int netlong;
		int fd = bnk->record[old_index].fd;

		if ( -1 == sized_write(fd, "eqv", 3) ) goto NG;
		if ( -1 == sized_write(fd, new_key, strlen(new_key)) ) goto NG;
		netlong = htonl(size);
		if ( -1 == sized_write(fd, &netlong, sizeof(netlong)) ) goto NG;
		if ( -1 == sized_write(fd, old_key, strlen(old_key)) ) goto NG;
		netlong = htonl(start);
		if ( -1 == sized_write(fd, &netlong, sizeof(netlong)) ) goto NG;

		bnk->record[new_index].attrib |= BNK_EXPORT;
		bnk->record[new_index].fd = fd;
	}
	return ANL_OK;

 NG:
	bnk->record[old_index].attrib &= ~BNK_EXPORT;
	bnk->record[old_index].fd = -1;
	return ANL_NG;
}

int
BnkPut(char *key, int size, void *ptr)
{
	int index, alloc_size, used_size;
	char *bufp = (char*)bnk + align(sizeof(*bnk));

	if ( ANL_NG == BnkKey(key, &index) ) {
		return ANL_NG;
	}
	alloc_size = bnk->record[index].alloc_size;
	used_size = size;
	if ( alloc_size < size ) {
		anl_msg_warning("\
BNK: WARNIG: DATA Overflow in BNKPUT: %s\n\
       define -> %d\n\
       now    -> %d\n\
", key, alloc_size, size);
		used_size = alloc_size;
	} else if ( alloc_size != size ) {
		anl_msg_debug3("\
BNK: DEBUG: DATA size mismatch in BNKPUT: %s\n\
       define -> %d\n\
       now    -> %d\n\
", key, alloc_size, size);
	}
	bnk->record[index].used_size = used_size;
	memcpy(bufp + bnk->record[index].buffer_pos, ptr, used_size);
	bnk->record[index].num_put = bnk->record[index].num_put + 1.0;

	if ( (BNK_EXPORT & bnk->record[index].attrib) ) {
		int fd = bnk->record[index].fd;

		if ( -1 == sized_write(fd, "put", 3) ) goto NG;
		if ( -1 == sized_write(fd, key, strlen(key)) ) goto NG;
		if ( -1 == sized_write(fd, ptr, size) ) goto NG;
	}

	return ANL_OK;

 NG:
	bnk->record[index].attrib &= ~BNK_EXPORT;
	bnk->record[index].fd = -1;
	return ANL_NG;
}

static int
BnkNetUpdate(int index)
{
	static struct timeval timeout = {0L, 0L};
	char *bufp = (char*)bnk + align(sizeof(*bnk));
	int fd = bnk->record[index].fd;
	int maxfds;
	fd_set rfds;

	maxfds = getdtablesize();
	/* prevent narrow fd_set binaries from blowing up with with */
	/* large getdtablesize() */
#ifdef FD_SET_SIZE
	maxfds = ((FD_SET_SIZE > maxfds)?maxfds:FD_SET_SIZE);
#endif

	FD_ZERO(&rfds);
	FD_SET(fd,&rfds);
	if ( 0 < select(maxfds,&rfds,0,0,&timeout) && FD_ISSET(fd,&rfds) ) {
		if ( -1 == sized_write(fd, "all", 3) ) goto NG;
		if ( -1 == sized_write(fd, "key", 3) ) goto NG;
		if ( -1 == sized_read(fd, NULL, 0) ) goto NG;
		for (;;) {
			char key[BNK_MAXNAM];
			int index, len;

			if (-1 == sized_read_skip(fd,key,sizeof(key),&len)) goto NG;
			if ( len < sizeof(key) ) {
				if ( 0 == len ) break;
				key[len] = '\0';
			}
			if ( ANL_NG == BnkKey(key, &index) ) {
				if (-1 == sized_read_skip(fd,NULL,0,&len)) goto NG;
			} else {
				int alloc, used;
				char *ptr = bufp + bnk->record[index].buffer_pos;

				alloc = bnk->record[index].alloc_size;
				if (-1 == sized_read_skip(fd,ptr,alloc,&used)) goto NG;
				bnk->record[index].used_size = used;
			}
		}
	}
	return ANL_OK;

 NG:
	bnk->record[index].attrib &= ~BNK_EXPORT;
	bnk->record[index].fd = -1;
	return ANL_NG;
}

int
BnkGet(char *key, int size, int *used, void *ptr)
{
	int index, copy_size, used_size;
	char *bufp = (char*)bnk + align(sizeof(*bnk));

	if ( ANL_NG == BnkKey(key, &index) ) {
		return ANL_NG;
	}

	if ( (BNK_EXPORT & bnk->record[index].attrib) ) {
		BnkNetUpdate(index);
	}

	if ( (BNK_FAMILY & bnk->record[index].attrib) ) {
		*used = used_size = bnk->record[index].alloc_size;
	} else {
		*used = used_size = bnk->record[index].used_size;
	}
	copy_size = used_size;
	if ( size < used_size ) {
		copy_size = size;
    }
	if ( 0.0 == bnk->record[index].num_put ) {
		if ( BNK_SINGLE == bnk->record[index].attrib ) {
			anl_msg_warning("\
BNK: WARNING: BNKGET without BNKPUT: %s\n", key);
		} else {
			anl_msg_debug3("\
BNK: DEBUG: BNKGET without BNKPUT: %s\n", key);
		}
	} else if ( size != used_size ) {
		anl_msg_debug3("\
BNK: DEBUG: DATA size mismatch in BNKGET: %s\n\
       used   -> %d\n\
       now    -> %d\n\
", key, used_size, size);
	}
	memcpy(ptr, bufp + bnk->record[index].buffer_pos, copy_size);
	bnk->record[index].num_get = bnk->record[index].num_get + 1.0;

	return ANL_OK;
}

int
BnkfPut(char *key, int *index_ptr, int size, void *ptr)
{
	int index, alloc_size, used_size;
	char *bufp = (char*)bnk + align(sizeof(*bnk));

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == BnkKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}
	alloc_size = bnk->record[index].alloc_size;
	used_size = size;
	if ( alloc_size < size ) {
		anl_msg_warning("\
BNK: WARNIG: DATA Overflow in BNKPUT: %s\n\
       define -> %d\n\
       now    -> %d\n\
", key, alloc_size, size);
		used_size = alloc_size;
	} else if ( alloc_size != size ) {
		anl_msg_debug3("\
BNK: DEBUG: DATA size mismatch in BNKPUT: %s\n\
       define -> %d\n\
       now    -> %d\n\
", key, alloc_size, size);
	}
	bnk->record[index].used_size = used_size;
	memcpy(bufp + bnk->record[index].buffer_pos, ptr, used_size);
	bnk->record[index].num_put = bnk->record[index].num_put + 1.0;

	if ( (BNK_EXPORT & bnk->record[index].attrib) ) {
		int fd = bnk->record[index].fd;

		if ( -1 == sized_write(fd, "put", 3) ) goto NG;
		if ( -1 == sized_write(fd, key, strlen(key)) ) goto NG;
		if ( -1 == sized_write(fd, ptr, size) ) goto NG;
	}

	return ANL_OK;

 NG:
	bnk->record[index].attrib &= ~BNK_EXPORT;
	bnk->record[index].fd = -1;
	return ANL_NG;
}

int
BnkfGet(char *key, int *index_ptr, int size, int *used, void *ptr)
{
	int index, copy_size, used_size;
	char *bufp = (char*)bnk + align(sizeof(*bnk));

	if ( 0 < *index_ptr ) {
		index = *index_ptr - 1;
	} else if ( ANL_NG == BnkKey(key, &index) ) {
		return ANL_NG;
	} else {
		*index_ptr = index + 1;
	}

	if ( (BNK_EXPORT & bnk->record[index].attrib) ) {
		BnkNetUpdate(index);
	}

	if ( (BNK_FAMILY & bnk->record[index].attrib) ) {
		*used = used_size = bnk->record[index].alloc_size;
	} else {
		*used = used_size = bnk->record[index].used_size;
	}
	copy_size = used_size;
	if ( size < used_size ) {
		copy_size = size;
    }
	if ( 0.0 == bnk->record[index].num_put ) {
		if ( BNK_SINGLE == bnk->record[index].attrib ) {
			anl_msg_warning("\
BNK: WARNING: BNKGET without BNKPUT: %s\n", key);
		} else {
			anl_msg_debug3("\
BNK: DEBUG: BNKGET without BNKPUT: %s\n", key);
		}
	} else if ( size != used_size ) {
		anl_msg_debug3("\
BNK: DEBUG: DATA size mismatch in BNKGET: %s\n\
       used   -> %d\n\
       now    -> %d\n\
", key, used_size, size);
	}
	memcpy(ptr, bufp + bnk->record[index].buffer_pos, copy_size);
	bnk->record[index].num_get = bnk->record[index].num_get + 1.0;

	return ANL_OK;
}

void
BnkLst(void)
{
	int i;
	char name[BNK_MAXNAM+1];

	name[BNK_MAXNAM] = '\0';	/* required to terminate string */
	anl_flush();
	printf("\
\n\
BNK: (data storge system) Ver.3.4\n\
    # of key    : %d/%d\n\
    buffer size : %d\n\
    buffer used : %d\n\
------------------------------------------------------------------------------\n\
Name                            Allocated     Used      #Put      #Get  Attrib\n\
------------------------------------------------------------------------------\n\
", bnk->ndef, bnk->nalloc, bnk->buffer_size, bnk->buffer_ptr);
	for (i = 0; i < bnk->ndef; i++) {
		strncpy(name, bnk->record[i].name, BNK_MAXNAM);
		printf("%-32s %8d %8d%10.0f%10.0f  %s%s\n",
			   name,
			   bnk->record[i].alloc_size,
			   bnk->record[i].used_size,
			   bnk->record[i].num_put,
			   bnk->record[i].num_get,
			   (BNK_FAMILY & bnk->record[i].attrib) ? "FAMILY" : "SINGLE",
			   (BNK_EXPORT & bnk->record[i].attrib) ? ",EXPORT" : ""
			   );
	}
	printf("\
------------------------------------------------------------------------------\n\
");
	anl_flush();
}

int
BnkConnect(char *server)
{
	char *host, *p;
	int port;

	host = malloc(strlen(server));
	if ( NULL == host ) {
		host = server;
		port = BNK_DEFAULT_PORT;
	} else {
		strcpy(host, server);
		p = strchr(host, ':');
		if ( NULL == p ) {
			port = BNK_DEFAULT_PORT;
		} else {
			*p = '\0';
			port = atoi(p+1);
			if ( 0 == port ) {
				port = BNK_DEFAULT_PORT;
			}
		}
	}

	client_fd = initport(PORT_NUMBER(port),CLIENT,SOCK_STREAM,host);

	if ( server != host ) free(host);

	if (client_fd < 0) {
		anl_msg_error("\
BNK: initport() = %d in BNKCONNECT\n", client_fd);
		return ANL_NG;
	}

	return ANL_OK;
}

int
BnkExport(char *key)
{
	int index, alloc_size, fd, netlong;

	if ( ANL_NG == BnkKey(key, &index) ) {
		return ANL_NG;
	}

	if ( client_fd < 0 ) {
		anl_msg_error("\
BNK: BnkConnect(server) required before BnkExport(\"%s\")\n", key);
		return ANL_NG;
	}

	alloc_size = bnk->record[index].alloc_size;
	bnk->record[index].attrib |= BNK_EXPORT;
	fd = bnk->record[index].fd = client_fd;

	if ( -1 == sized_write(fd, "def", 3) ) goto NG;
	if ( -1 == sized_write(fd, key, strlen(key)) ) goto NG;
	netlong = htonl(alloc_size);
	if ( -1 == sized_write(fd, &netlong, sizeof(netlong)) ) goto NG;

	return ANL_OK;

 NG:
	bnk->record[index].attrib &= ~BNK_EXPORT;
	bnk->record[index].fd = -1;

	return ANL_NG;
}

int
BnkExportAll(void)
{
	int index;

	if ( client_fd < 0 ) {
		anl_msg_error("\
BNK: BnkConnect(server) required before BnkExportAll\n");
		return ANL_NG;
	}

	for (index = 0; index < bnk->ndef; index++) {
		char *key;
		int alloc_size, fd, netlong, keylen;

		if ( (BNK_EXPORT & bnk->record[index].attrib) ) {
			continue;	/* already exported */
		}
		alloc_size = bnk->record[index].alloc_size;
		bnk->record[index].attrib |= BNK_EXPORT;
		fd = bnk->record[index].fd = client_fd;
		key = bnk->record[index].name;
		for (keylen = 0; keylen < BNK_MAXNAM && key[keylen]; keylen++) {
			;
		}

		if ( -1 == sized_write(fd, "def", 3) ) goto NG;
		if ( -1 == sized_write(fd, key, keylen) ) goto NG;
		netlong = htonl(alloc_size);
		if ( -1 == sized_write(fd, &netlong, sizeof(netlong)) ) goto NG;
	}

	return ANL_OK;

 NG:
	for (index = 0; index < bnk->ndef; index++) {
		bnk->record[index].attrib &= ~BNK_EXPORT;
		bnk->record[index].fd = -1;
	}

	return ANL_NG;
}

int
BnkServer(int port)
{
	int writer;
	int reader;
	fd_set readfds, waitfds;
	int maxfds;

	FD_ZERO(&readfds);
	FD_ZERO(&waitfds);

    maxfds = getdtablesize();	/* for future reference */
    /* prevent narrow fd_set binaries from blowing up with with */
    /* large getdtablesize() */
#ifdef FD_SET_SIZE
	maxfds = ((FD_SET_SIZE > maxfds)?maxfds:FD_SET_SIZE);
#endif

	if ( 0 == port ) {
		port = BNK_DEFAULT_PORT;
	}

	reader = initport(PORT_NUMBER(port),SERVER,SOCK_STREAM,NULL);
	if (reader < 0) {
		anl_msg_error("\
BNK: initport() = %d\n in BNKSERVER", reader);
		return ANL_NG;
	}

	for (;;) {
		int len;
		char cmd[8], key[BNK_MAXNAM+1];

		writer = select_server_stream(reader, &readfds);

		len = sized_read_skip(writer, cmd, sizeof(cmd)-1, &len);
		if (len <= 0) {
			anl_msg_info("%d: EOF\n", writer);
			close(writer);
			continue;
		}
		cmd[len] = '\0';

		len = sized_read_skip(writer, key, sizeof(key)-1, &len);
		if (len <= 0) {
			anl_msg_info("%d: EOF\n", writer);
			close(writer);
			continue;
		}
		key[len] = '\0';

		if ( 0 == strcmp("def", cmd) ) {
			int size;

			len = sized_read_skip(writer, &size, sizeof(size), &len);
			if (len <= 0) {
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}
			size = ntohl(size);

			BnkDef(key, size);

			anl_msg_info("def: key=%s, size=%d\n", key, size);

		} else if ( 0 == strcmp("eqv", cmd) ) {
			int size;
			char key2[BNK_MAXNAM+1];
			int start;

			len = sized_read_skip(writer, &size, sizeof(size), &len);
			if (len <= 0) {
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}
			size = ntohl(size);

			len = sized_read_skip(writer, key2, sizeof(key2)-1, &len);
			if (len <= 0) {
				anl_msg_info("%d: EOF\n", writer);
				close(writer);
				continue;
			}
			key2[len] = '\0';

			len = sized_read_skip(writer, &start, sizeof(start), &len);
			if (len <= 0) {
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}
			start = ntohl(start);

			BnkEqv(key, size, key2, start);

			anl_msg_info("eqv: key=%s, size=%d, key2=%s, start=%d\n",
				   key, size, key2, start);

		} else if ( 0 == strcmp("put", cmd) ) {
			int index, alloc_size, used_size, fd;
			char *bufp = (char*)bnk + align(sizeof(*bnk));

			if ( ANL_NG == BnkKey(key, &index) ) {
				return ANL_NG;
			}
			alloc_size = bnk->record[index].alloc_size;
			len = sized_read_skip(writer,
				bufp + bnk->record[index].buffer_pos, alloc_size, &used_size);
			if ( len < 0 ) {
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}
			if ( alloc_size < used_size ) {
				anl_msg_warning("\
BNK: WARNIG: DATA Overflow in BNKPUT: %s\n\
       define -> %d\n\
       now    -> %d\n\
", key, alloc_size, used_size);
				used_size = alloc_size;
			}
			bnk->record[index].used_size = used_size;

			for (fd = 0; fd < maxfds; fd++) {
				if ( writer == fd || reader == fd ||
					 !FD_ISSET(fd, &readfds) ) continue;
				if ( !FD_ISSET(fd, &waitfds) ) {
					FD_SET(fd, &waitfds);			/* data is ready */
					sized_write(fd, NULL, 0);		/* send dummy data */
					anl_msg_info("write: fd=%d\n", fd);
				}
			}

			anl_msg_info("put: key=%s, size=%d\n", key, used_size);

		} else if ( 0 == strcmp("get", cmd) ) {
			int index, used_size;
			char *bufp = (char*)bnk + align(sizeof(*bnk));

			if ( ANL_NG == BnkKey(key, &index) ) {
				return ANL_NG;
			}
			if ( (BNK_FAMILY & bnk->record[index].attrib) ) {
				used_size = bnk->record[index].alloc_size;
			} else {
				used_size = bnk->record[index].used_size;
			}

			len = sized_write(writer,
				bufp + bnk->record[index].buffer_pos, used_size);
			if ( len < 0 ) {
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}

			anl_msg_info("get: key=%s, used=%d\n", key, used_size);

		} else if ( 0 == strcmp("all", cmd) ) {
			int index, used_size;
			char *bufp = (char*)bnk + align(sizeof(*bnk));

			for (index = 0; index < bnk->ndef; index++) {
				char *ptr;
				char *key = bnk->record[index].name;

				for (len = 0; len < BNK_MAXNAM && key[len]; len++) {
					;
				}
				if ( -1 == sized_write(writer, key, len) ) goto quit;
				used_size = bnk->record[index].used_size;
				ptr = bufp + bnk->record[index].buffer_pos;
				if ( -1 == sized_write(writer, ptr, used_size) ) goto quit;
			}

			if ( -1 == sized_write(writer, NULL, 0) ) {

			quit:
				anl_msg_info("%d: EOF\n",writer);
				close(writer);
				continue;
			}

			FD_CLR(writer, &waitfds);	/* data flushed */

			anl_msg_info("all: key=%s\n", key);

		} else {
				anl_msg_error("\
BNK: unknown command received in BNKSERVER: %s\n", key);

		}
	}
}

/*
  File: unixc.c
  Description: Fortran/C interface routines for CLI
  Author: A.Shirahashi
  Date: 22-Jun-1992

  History:
	 6-Sep-1993, patch for OSF/alpha
     1-Apr-1994, add fsignal_ (by, R.Itoh)
	19-Nov-1994, add fexecp_
	22-Feb-1997 Y.ISHISAKI, add fputenv_
	16-Feb-2005 Y.ISHISAKI, use mkstemp() instead of tempnam() in ftempnam_()
	16-Feb-2005 Y.ISHISAKI, add fsleep_(), fusleep_()
	20-Feb-2005 Y.ISHISAKI, check popen(), malloc(), getcwd() errors
	20-Feb-2005 Y.ISHISAKI, add clexitcode_(), CLexitcode()
	02-Feb-2007 Y.ISHISAKI,
		merge unixio.c, adding fredirect_(), frestore_()
		merge **_unix.c, adding fseek_(), fputc_(), fgetc_()
		add cli__ prefix for f**_() functions
		add clchdir_(), clgetwd_(), cltempnam_()

  Public: cli__opendir_, cli__readdir_, cli__closedir_, cli__fisatty_,
		  cli__fdelete_, cli__fsystem_, cli__fflush_,
		  cli__fexecp_, cli__fputenv_,
		  cli__fsleep_, cli__fusleep_,
		  cli__fredirect_, cli__frestore_,
		  cli_fseek_, cli_fputc_, cli_fgetc_,
		  clchdir_, clgetwd_, cltempnam_, clexitcode_
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "cli.h"

#define MAXFP 8

static FILE *filep[MAXFP] = { 0,0,0,0,0,0,0,0 };
static int fpp;

static int shell_exit_code = -1;

int
cli__opendir_(filename, length)
  char *filename;
  int length;
{
	char ls[] = "/bin/ls ";
	char *p;
	FILE *fp;

	p = malloc(length + sizeof(ls));
	if ( NULL == p ) {
		return 0;
    }
	strcpy(p, ls);
	strncpy(p + sizeof(ls) - 1, filename, length);
	p[length + sizeof(ls) - 1] = '\0';
	/*printf("popen(%s)\n", p);*/
	fp = popen(p, "r");
	free(p);
	if ( NULL == fp ) {
		return 0;
    }
	for (fpp = 1; fpp < MAXFP; fpp++) {
		if ( 0 == filep[fpp] ) {
			filep[fpp] = fp;
			return fpp;
		}
	}
	return 0;
}

int
cli__readdir_(dirp, filename, length)
  int *dirp;
  char *filename;
  int length;
{
	char *status;
	char *p;

	if ( 0 == *dirp || 0 == filep[*dirp] ) {
		return 0;
    }
	status = fgets(filename,length,filep[*dirp]);
	if ( NULL == status ) {
		return 0;
    }

	if ( NULL != (p = strchr(filename,'\n')) ) {
		while ( p < filename+length ) {
			*p++ = ' ';
		}
	}

	return !0;
}

void
cli__closedir_(dirp)
  int *dirp;
{
	if ( 0 == *dirp || 0 == filep[*dirp] ) {
		return;
    }
	pclose(filep[*dirp]);
	filep[*dirp] = 0;
}

int
cli__fisatty_(dp)
  int *dp;
{
	int status;
	status = isatty( *dp );
	return status;
}

void
cli__fdelete_(file, length)
  char *file;
  int length;
{
	char *p = malloc(length+1);

	if ( NULL == p ) {
		return;
    }
	strncpy(p, file, length);
	p[length] = '\0';
#ifdef DEBUG
	fprintf(stderr,"fdelete_:file=[%s]\n",p);
#endif
	unlink(p);
	free(p);
}

void
cli__fsystem_(comand, lcmd)
  char *comand;
  int lcmd;
{
	char *p;

	if( lcmd > 0 ) {
		if ( NULL == (p = malloc(lcmd+1)) ) {
			return;
        }
		strncpy(p, comand, lcmd);
		p[lcmd] = '\0';
		shell_exit_code = system(p);
		free(p);
	} else {
		if( NULL != (p = getenv("SHELL")) ) {
			shell_exit_code = system(p);
		} else {
			shell_exit_code = system("/bin/csh");
		}
	}
}

void
cli__fflush_(fh)
  int *fh;
{
	if ( 1 == *fh ) {
		fflush(stdout);
	}
}

void
cli__fexecp_(inbuf, result, reslen, inlen, outlen)
  char *inbuf;
  char *result;
  int *reslen;
  int inlen;
  int outlen;
{
	char *p;
	FILE *fp;
	int n;

	p = malloc(inlen+1);
	if ( NULL == p ) {
		shell_exit_code = -1;
		return;
    }

	strncpy(p, inbuf, inlen);
	p[inlen] = '\0';
	/*printf("inbuf=%s\n",p);*/
	/*printf("inlen=%d\n",inlen);*/
	fp = popen(p,"r");
	free(p);
	n = outlen;
	if ( NULL == fp ) {
		n = 0;
		shell_exit_code = -1;
	} else {
		if ( NULL == fgets(result,n,fp) ) {
			n = 0;
		} else {
			n = strlen(result);
		}
		shell_exit_code = pclose(fp);
    }
	if ( 0 < n && '\n' == result[n-1] ) {
		n--;
    }
	memset(result+n, ' ', outlen - n);
	*reslen = n;
}

void
cli__fputenv_(string, length)
  char *string;
  int length;
{
	char *p = malloc(length+1);

	if ( NULL == p ) {
		return;
	}
	strncpy(p,string,length);
	p[length] = '\0';
	putenv(p);
	/*free(p);*/	/* should not free allocated memory in putenv() */
}

void
cli__fsleep_(sec)
  unsigned int *sec;
{
	sleep(*sec);
}

void
cli__fusleep_(usec)
  unsigned int *usec;
{
	usleep(*usec);
}

int
cli__fredirect_(fh, file, mode, length)
  int *fh;
  char *file;
  char *mode;
  int length;
{
	int fs, ff;
	char *path;
/*	FILE *fp;*/
/*
	fprintf(stderr,"redirect: *fh=%d, file=<%.*s>, length=%d\n",
		*fh,length,file,length);
*/
	path = (char *)malloc(length+1);
	if( path == (char *)NULL ) {
		fprintf(stderr,"path = NULL\n");
		return(-1);
	}
	strncpy(path,file,length);
	*(path+length) = '\0';
/*
   if( (fp = fopen(path,"w")) == (FILE *)NULL ) {
		free(path);
		return(-1);
	}
	ff = fileno(fp);
*/
	if( *mode == 'w' ) {
/*		fprintf(stderr,"try to open %s (create)\n",path);*/
		ff = open(path,O_WRONLY|O_CREAT|O_TRUNC,0666);
	} else if( *mode == 'a' ) {
/*		fprintf(stderr,"try to open %s (append)\n",path);*/
		ff = open(path,O_WRONLY|O_APPEND,0);
	} else {
		fprintf(stderr,"unknown mode %c\n",*mode);
	}
	if( ff < 0 ) {
		fprintf(stderr,"can't open file %s\n",path);
		free(path);
		path = (char *)NULL;
		return(-1);
	}
	fs = dup(*fh);
	dup2(ff,*fh);
	close(ff);
	free(path);
	return(fs);
}

void
cli__frestore_(fh, fs)
  int *fh;
  int *fs;
{
/*	fprintf(stderr,"frestore: *fh=%d,*fs=%d\n",*fh,*fs);*/
	if( *fs > 0 ) {
		close(*fh);
		dup2(*fs,*fh);
		close(*fs);
	}
}

int
cli__fseek_(int *lun, int *offset, int *whence)
{
	int fd, istat;

	CLflush(*lun);
	fd = CLfnum(*lun);
/*	printf("unixc: fseek_(): fd=%d, lun=%d, offset=%d, whence=%d\n",
		fd, *lun, *offset, *whence);*/
	if ( fd < 0 ) {
		return -1;
	}
	istat = lseek(fd, *offset, *whence);
	CLflush(*lun);
	return istat;
}

int
cli__fputc_(int *lun, char *c)
{
	int fd, siz;

	CLflush(*lun);
	fd = CLfnum(*lun);
	if ( fd < 0 ) {
		return -1;
	}
	siz = write(fd, c, 1);
	CLflush(*lun);
	if ( siz != 1 ) {
		return -1;
	}
	return 0;
}

int
cli__fgetc_(int *lun, char *c)
{
	int fd, siz;

	CLflush(*lun);
	fd = CLfnum(*lun);
	if ( fd < 0 ) {
		return -1;
	}
	siz = read(fd, c, 1);
	CLflush(*lun);
	if ( siz != 1 ) {
		return -1;
	}
	return 0;
}

void
clchdir_(path, length)
  char *path;
  int length;
{
	char *p = malloc(length+1);

	if ( NULL == p ) {
		return;
    }
	strncpy(p, path, length);
	p[length] = '\0';
	chdir(p);
	free(p);
}

void
clgetwd_(path, length)
  char *path;
  int length;
{
	char *p;

	if ( NULL == getcwd(path, length) ) {
		return;
    }
	for (p = path + strlen(path); p < path + length; p++) {
		*p = ' ';
	}
}

void
cltempnam_(directory, prefix, output, dlen, plen, olen)
  char *directory;
  char *prefix;
  char *output;
  int dlen;
  int plen;
  int olen;
{
	int fd, len;

	strncpy(output, directory, dlen);
	if ( 0 < dlen && '/' != output[dlen-1] ) {
		output[dlen] = '/';
		dlen++;
	}
	strncpy(output+dlen, prefix, plen);
	strcpy(output+dlen+plen, "XXXXXX");
	fd = mkstemp(output);
	if ( -1 == fd ) {
		memset(output, ' ', olen);
		return;
	}
	close(fd);
	len = strlen(output);
	memset(output+len, ' ', olen-len);
}

int
clexitcode_(void)
{
	return shell_exit_code;
}

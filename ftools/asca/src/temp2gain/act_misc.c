/****************************************************************/
/* ascatool.c	ascatool misc functions
/****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <atFunctions.h>
#include "ascatool.h"

#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define null(p)		((p)==NULL)

int
ascatool_cmp_frf_files(const void *p1, const void *p2)
{
	struct ascatool_frf_files *f1, *f2;
	f1 = (struct ascatool_frf_files*)p1;
	f2 = (struct ascatool_frf_files*)p2;
	if ( f1->starttime < f2->starttime ) return -1;
	if ( f1->starttime == f2->starttime ) {
		if ( f1->size < f2->size ) return -1;	/* priority for big size FRF */
		if ( f1->size == f2->size ) return 0;
	}
	return 1;
}

double
ascatool_frf2ascatime(char *fn)
{
	AtTime t;
	double mjd;
	t.sc = 0;
	t.ms = 0.0;
	sscanf(fn+2, "%02u%02u%02u_%02u%02u", &t.yr, &t.mo, &t.dy, &t.hr, &t.mn);
	atMJulian(&t, &mjd);
	return mjd2asca(mjd);
}

int
ascatool_frf_file_name(char *p, char ft_or_fa)
{
	if ('f' == *p++ &&
		ft_or_fa == *p++ &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		'_' == *p++ &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		'.' == *p++ &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		isdigit(*p++) &&
		(('\0' == *p) ||
		 ('m' == *p && '\0' == *++p) ||
		 ('g' == *p && '\0' == *++p) ||
		 ('G' == *p && '\0' == *++p) ||
		 ('p' == *p && '\0' == *++p)) ) {
		return 1;
	}
	return 0;
}

struct ascatool_frf_files *
ascatool_read_frf_dir(char *frfpath, char ft_or_fa)
{
	DIR *dp;
	struct ascatool_frf_files *files;
	struct dirent *dir;
	struct stat sbuf;
	char *path, *p;
	int i, n, len, nalloc;
	double ascatime;
	nalloc = 100;
	files = malloc(sizeof(*files)*nalloc);
	if ( null(files) ) return NULL;
	p = frfpath;
	len = strlen(p);
	path = malloc(len+2);
	if ( null(path) ) {
		p = "\0";
	} else {
		strcpy(path, p);
		path[len+1] = '\0';
		for (p = path; *p; p++) {
			if ( ':' == *p ) *p = '\0';
		}
		p = path;
	}
	n = 0;
	for (len = 0; *p; p += len+1) {
		len = strlen(p);
		dp = opendir(p);
		if ( null(dp) ) continue;
		for (;;) {
			dir = readdir(dp);
			if ( null(dir) ) break;
			/*if ( 0 == dir->d_ino ) continue;*/
			/* 06 Apr 06 Not portable since not POSIX-required */
			if ( ! ascatool_frf_file_name(dir->d_name, ft_or_fa) ) continue;
			ascatime = ascatool_frf2ascatime(dir->d_name);
			if ( nalloc <= n ) {
				nalloc += 100;
				files = realloc(files, sizeof(*files)*nalloc);
				if ( null(files) ) {
					n = 0;
					break;
				}
			}
			files[n].fn = malloc(len+strlen(dir->d_name)+2);
			if ( null(files[n].fn) ) break;
			strcpy(files[n].fn, p);
			if ( '/' == p[len-1] ) {
				strcpy(files[n].fn+len, dir->d_name);
			} else {
				files[n].fn[len] = '/';
				strcpy(files[n].fn+len+1, dir->d_name);
			}
			files[n].starttime = ascatime;
			files[n].endtime = 0.0;
			stat(files[n].fn, &sbuf);
			files[n].size = sbuf.st_size;
			/*fprintf(stderr, "%s\n", files[n].fn);*/
			n++;
		}
	}
	free(path);
	files = realloc(files, sizeof(*files)*(n+1));
	files[n].fn = NULL;
	qsort(files, n, sizeof(*files), ascatool_cmp_frf_files);
	return files;
}

char*
ascatool_read_region_file(char *filename)
{
	FILE *fp;
	char *p, *fn;
	int nalloc, n;
	fn = ascatool_expand_file_name(filename);
	if ( NULL == fn ) return NULL;
	fp = fopen(fn, "r");
	free(fn);
	if ( NULL == fp ) return NULL;
	n = 0;
	nalloc = 1024;
	p = malloc(nalloc);
	if ( NULL == p ) return NULL;
	for (;;) {
		int c;
		unless ( n < nalloc ) {
			nalloc += 1024;
			p = realloc(p, nalloc);
			if ( NULL == p ) return NULL;
		}
		c = fgetc(fp);
		if ( '#' == c ) {
			for (;;) {
				c = fgetc(fp);
				if ( EOF == c || '\n' == c ) break;
			}
			continue;
		}
		if ( '@' == c ) {
			if ( 0 == n || ( n && ( '\0' == p[n-1] || ',' == p[n-1] ) ) ) {
				static int i;
				static char *q;
				i = 0;
				q = p + n;
				for (;;) {
					c = fgetc(fp);
					if ( EOF == c || c <= ' ' ) break;
					q[i++] = c;
					unless ( n + i < nalloc ) {
						p = realloc(p, nalloc);
						if ( NULL == p ) return NULL;
					}
				}
				q[i] = '\0';
				q = ascatool_read_region_file(q);
				unless ( NULL == q ) {
					while ( q[0] || q[1] ) {
						unless ( n < nalloc ) {
							nalloc += 1024;
							p = realloc(p, nalloc);
						if ( NULL == p ) return NULL;
						}
						p[n++] = *q++;
					}
					p[n++] = '\0';
				}
				continue;
			}
		}
		if ( EOF == c ) {
			if ( n && ( '\0' == p[n-1] || ',' == p[n-1] ) ) n--;
			p[n++] = '\0';
			break;
		}
		if ( '\t' == c || ' ' == c ) continue;
		if ( '\n' == c ) c = '\0';
		if ( '\0' == c && n && '\0' == p[n-1] ) continue;
		p[n++] = c;
	}
	fclose(fp);
	p = realloc(p, n+1);
	if ( NULL == p ) return NULL;
	p[n] = '\0';
	return p;
}

char*
ascatool_scanYYMMDDHHMMSS(char *p, AtTime *ji)
{
	if ( 6 == sscanf(p, "%02u%02u%02u%02u%02u%02u",
					 &ji->yr, &ji->mo, &ji->dy, &ji->hr, &ji->mn, &ji->sc) ) {
		p += 12;
		if ( '.' == *p && 1 == sscanf(p+1, "%03f", &ji->ms) ) {
			p += 4;
		} else {
			ji->ms = 0;
		}
	}
	return p;
}

char*
ascatool_scanTimeOffset(char *p, double *sec)
{
	unsigned long v;
	*sec = 0;
	for (;;) {
		if ( 1 != sscanf(p, "%lu", &v) ) break;
		while ( '0' <= *p && *p <= '9' ) p++;
		switch (*p++) {
		case 'm': *sec += 60*v; break;
		case 'h': *sec += 60*60*v; break;
		default: *sec += v;
		}
	}
	return p;
}

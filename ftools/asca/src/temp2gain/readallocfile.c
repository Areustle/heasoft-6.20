#include <stdio.h>
#include <stdlib.h>

#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define null(p)		((p)==NULL)

char*
read_alloc_file(FILE *fp)
{
	char *p;
	int c, n, nalloc;
	p = NULL;
	n = nalloc = 0;
	for (;;) {
		unless ( n < nalloc ) {
			nalloc += 1024;
			if ( null(p) ) {
				p = (char*)malloc(sizeof(*p)*nalloc);
			} else {
				p = (char*)realloc(p, nalloc);
			}
			if ( null(p) ) return NULL;
		}
		c = fgetc(fp);
		if ( EOF == c ) break;
		p[n++] = c;
	}
	if ( 0 == n && EOF == c ) return NULL;
	p = (char*)realloc(p, n+1);
	if ( null(p) ) return NULL;
	p[n] = '\0';
	return p;
}

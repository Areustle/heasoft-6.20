/* return file name position */

char*
splitpath(char *p)
{
	char *pp;
	for (pp = p; *p; p++) {
		if ( *p == '/' ) {
			pp = p + 1;
		}
	}
	return(pp);
}

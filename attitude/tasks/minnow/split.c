
#include <stdlib.h>
#include <string.h>


char **string_split(const char *in, char delimiter, int * pcount)
{
	int i, count;
	char *raw, *current;
	char **aref;
	int length, oi;

	*pcount = -1;

	length = strlen(in);

	count = 1;
	for (i = 0; i < length; ++i)
		if (in[i] == delimiter)
			++count;

	raw = (char *) malloc(count * sizeof(char*) + length + count);
	aref = (char **) raw;
	current = raw + count * sizeof(char*);

	oi = 0;
	aref[0] = current;

	for (i = 0; i < length; ++i)
	{
		char c = in[i];

		if (c == delimiter) {
			*current = 0;
			++oi;
			aref[oi] = current + 1;
		}
		else
			*current = c;

		++current;
	}

	*current = 0;

	*pcount = count;

	return aref;
}



#ifdef SPLIT_TEST

#include <stdio.h>

void parse(const char *in)
{
	int i, count;
	char **tmp;
	tmp = split(in, ',', &count);
	printf("parse(%s) => count=%d\n", in, count);
	for (i = 0; i < count; ++i)
		printf("\t%s\n", tmp[i]);
	free(tmp);
}


int main(int argc, char **argv)
{
	int i;
	for (i = 1; i < argc; ++i)
		parse(argv[i]);
	return 0;
}

#endif


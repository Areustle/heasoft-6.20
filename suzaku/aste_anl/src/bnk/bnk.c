#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"

static void
f2c_string(char *f_string, char *c_string, int len)
{
	strncpy(c_string, f_string, len);
	while ( 0 < len && ' ' == c_string[len-1] ) {
		len--;
	}
	c_string[len] = '\0';
}

int
bnkini_(int *buffer_size)
{
	return BnkIni(*buffer_size);
}

int
bnkdef_(char *key, int *size, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkDef(buf, *size);
}

int
bnkkey_(char *key, int *index, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkKey(buf, index);
}

int
bnkisdef_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkIsDef(buf);
}

int
bnkndf_(void)
{
	return BnkNdf();
}

int
bnkeqv_(char *new_key, int *size, char *old_key, int *start, int new_len, int old_len)
{
	char new_buf[80], old_buf[80];
	f2c_string(new_key, new_buf, new_len);
	f2c_string(old_key, old_buf, old_len);
	return BnkEqv(new_buf, *size, old_buf, *start);
}

int
bnkput_(char *key, int *size, void *ptr, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkPut(buf, *size, ptr);
}

int
bnkget_(char *key, int *size, int *used, void *ptr, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkGet(buf, *size, used, ptr);
}

int
bnkfput_(char *key, int *index, int *size, void *ptr, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkfPut(buf, index, *size, ptr);
}

int
bnkfget_(char *key, int *index, int *size, int *used, void *ptr, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return BnkfGet(buf, index, *size, used, ptr);
}

/* $Id: evs.c,v 1.4 2005/11/29 20:00:00 ishisaki Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "evs.h"

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
evsiz_(void)
{
	return EvsIz();
}

void
evsclr_all_(void)
{
	EvsClrAll();
}

void
evsrst_all_(void)
{
	EvsRstAll();
}

void
evsacm_(void)
{
	EvsAcm();
}

int
evsdef_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsDef(buf);
}

int
evskey_(char *key, int *index, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsKey(buf, index);
}

int
evsisdef_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsIsDef(buf);
}

int
evsndf_(void)
{
	return EvsNdf();
}

void
evsout_(int *lun)
{
	EvsOut();
}

int
evsval_(char *key, int *logic, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsVal(buf, *logic);
}

int
evsset_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsSet(buf);
}

int
evsclr_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsClr(buf);
}

int
evs_(char *key, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return Evs(buf);
}

int
evsfset_(char *key, int *index, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsfSet(buf, index);
}

int
evsfclr_(char *key, int *index, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return EvsfClr(buf, index);
}

int
evsf_(char *key, int *index, int key_len)
{
	char buf[80];
	f2c_string(key, buf, key_len);
	return Evsf(buf, index);
}

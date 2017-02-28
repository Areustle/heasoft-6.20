/*
C File: clitestc.c
C Description: Test program for CLI
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"

int
main(void)
{
	static char string[80];
	static char text[16];
	static char words[8][16], word[16], key[16];
	static char *table[5] = {
		"aa", "ab", "c", "d", "e"
	};
	int status;
	int lstat;
	int ival = 123;
	float rval = 1.23e0;
	double dval = 1.23e0;
	int hval = 255;
	int lval = 1;
	int i, nw, ierr, it;
	int lun = 1;
	int length;

/* ...test for basic input routines */
	CLintrd("Input INTEGER", &ival);
	printf("ival = %d\n", ival);
	CLintrdL("Input INTEGER (IntrdL)", &ival, 1, 10);
	printf("ival = %d\n", ival);
	CLintrdX("Input INTEGER (IntrdX)", &ival, text, sizeof(text));
	printf("ival = %d, text = %s\n", ival, text);

	CLfltrd("Input REAL*4", &rval);
	printf("rval = %f\n", rval);
	CLfltrdL("Input REAL*4 (FltrdL)", &rval, 1.0, 10.0);
	printf("rval = %f\n", rval);
	CLfltrdX("Input REAL*4 (FltrdX)", &rval, text, sizeof(text));
	printf("dval = %f, text = %s\n", rval, text);

	CLfdprd("Input REAL*8", &dval);
	printf("dval = %f\n", dval);
	CLfdprdL("Input REAL*8 (FdprdL)", &dval, 1.0, 10.0);
	printf("dval = %f\n", dval);
	CLfdprdX("Input REAL*8 (FdprdX)", &dval, text, sizeof(text));
	printf("dval = %f, text = %s\n", dval, text);

	CLhexrd("Input HEX", &hval);
	printf("hval = %d\n", hval);
	CLhexrdL("Input HEX (HexrdL)", &hval, 1, 10);
	printf("hval = %d\n", hval);

	CLlogrd("Input LOGICAL", &lval);
	printf("lval = %d\n", lval);

/* ...test for conversion routines */
	CLtxtrd("?Input Real", text, sizeof(text));
	CLatof(text, &rval);
	printf("text = %s, rval = %f\n", text, rval);
	CLtxtrd("?Input Integer", text, sizeof(text));
	CLatoi(text, &ival);
	printf("text = %s, ival = %d\n", text, ival);
	CLfltrd("?Input Real", &rval);
	CLftoa(rval, text, sizeof(text));
	printf("rval = %f, text = %s\n", rval, text);
	CLfdprd("?Input Double", &dval);
	CLdtoa(dval, text, sizeof(text));
	printf("dval = %f, text = %s\n", dval, text);

/* ...test for string maipulation routines */
	nw = sizeof(words)/sizeof(*words);
	CLword("word1, word2, word3", " ,", &nw, words[0], sizeof(words[0]));
	for (i = 0; i < nw; i++) {
		printf("word[%d] = %.*s\n", i, (int)sizeof(words[0]), words[i]);
	}
	CLpart("word1 word2 word3", 2, word, sizeof(word));
	printf("2nd part is: %s\n", word);

/* ...test for OPNRD */
	CLopnrd("clitest.com");
	CLtxtrd("OPNRD>", string, sizeof(string));
	CLtxtrd("OPNRD>", string, sizeof(string));
	CLtxtrd("OPNRD>", string, sizeof(string));
	CLtxtrd("OPNRD>", string, sizeof(string));

/* ...test for LUNRD */
	strcpy(string, "clitest.com");
	CLxopen(lun, string, "READ", "FORMATTED", "OLD", sizeof(string));
	CLlunrd(lun);
	CLtxtrd("LUNRD>", string, sizeof(string));
	CLtxtrd("LUNRD>", string, sizeof(string));
	CLtxtrd("LUNRD>", string, sizeof(string));
	CLtxtrd("LUNRD>", string, sizeof(string));
	CLclos(lun, &ierr);

/* ... test for IOPEN */
	unlink("clitest.dat");
	status = CLiopen(lun, "clitest", ".dat", " ");
	printf("Iopen (W) status = %d\n", status);
	printf("THIS IS A TEST (1)\n");
	CLclos(lun, &ierr);
	status = CLiopen(lun, "clitest", ".dat", "R");
	printf("Iopen (R) status = %d\n", status);
	/*
      read(1,'(A)') string
      write(6,*) string
	*/
	CLclos(lun, &ierr);

/* ... test for QOPEN */
	strcpy(string, "*.com");
	lstat = CLqopen("filename>", lun, string, "READ", "FORMATTED", "OLD",
			sizeof(string));
	if ( lstat ) {
		printf("%s\n", string);
        length = sizeof(string) - 1;
		ierr = CLgets(lun, string, sizeof(string), &length);
		if ( 0 == ierr ) {
			printf("line=%s\n", string);
        }
		CLclos(1, &ierr);
    } else {
		printf("failed\n", string);
    }
	strcpy(string, "clitest.dat");
	lstat = CLqopen("filename>", 1, string, "WRITE", "FORMATTED", "NEW",
			sizeof(string));
	CLputl(1, "THIS IS A TEST (2)");
	CLclos(1, &ierr);

/* ... test for KEYRD */
	CLkeyrd(-3, "?KEY>", key, table, 5, &it, sizeof(key));
	printf("%d %s\n", it, key);

	return 0;
}

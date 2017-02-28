/************************************************************************/
/*		aeFilenameGet	ver. 1.2	(1992-Nov.-01)		*/
/*									*/
/*	history								*/
/*		0.0: first version					*/
/*		0.1: bug fix of 0.0					*/
/*		1.0: Three keies					*/
/*		1.1: _ is avalable for KEY Word				*/
/*		1.2: The maximum character length per one line 		*/
/*			is changed into 1024.				*/
/*		     The maximum character length per one keyword 	*/
/*			is changed into 256.				*/
/*		coded by T.G.Tsuru (tsuru@cr.scphys.kyoto-u.ac.jp)	*/
/************************************************************************/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#define		MAX_LEN		1024
#define		MAX_KEY_LEN	256
#define		ENV_ASTEFILNAM	"ASTEFIL"

static int	opWordCheck(w)
char	w;
{
	int	ret_val;

	if(w<0x09)			/*	CNTL CODE	*/
		ret_val = -1;
	else if(w==0x09)		/*	TAB		*/
		ret_val=1;
	else if(w>=0x0a && w<=0x1f)	/*	CNTL CODE	*/
		ret_val = -1;
	else if(w>=0x20 && w<=0x22)	/*	(spcae)!"	*/
		ret_val = 1;
	else if(w==0x23)		/*	#		*/
		ret_val = -1;
	else if(w>=0x24 && w<=0x2f)	/*	$%&'()*+,-./	*/
		ret_val = 1;
	else if(w>=0x30 && w<=0x39)	/*	0123456789	*/
		ret_val = 0;
	else if(w>=0x3a && w<=0x40)	/*	:;<=>?@		*/
		ret_val = 1;
	else if(w>=0x41 && w<=0x5a)	/*	A-Z		*/
		ret_val = 0;
	else if(w>=0x5b && w<=0x5e)	/*	[\]^		*/
		ret_val = 1;
	else if(w==0x5f)		/*	_		*/
		ret_val = 0;
	else if(w==0x60)		/*	`		*/
		ret_val = 1;
	else if(w>=0x61 && w<=0x7a)	/*	a-z		*/
		ret_val = 0;
	else if(w>=0x7b && w<=0x7e)	/*	{|}~		*/
		ret_val = 1;
	else if(w>=0x7f)		/*	Hennano		*/
		ret_val = -1;

	return(ret_val);

}

int	aeFilenameGet(key1, key2, key3, filnam)
char	*key1, *key2, *key3, *filnam;
/*	ret_val == -1 : Environment Value of ASTEFIL is not set.	*/
/*	ret_val == -2 : The file defined with ASTEFIL is not found	*/
/*	ret_val == -3 : KEY WORDs are not found or filnam is not defined*/
/*	ret_val == 0  : No problem					*/
/*	KEY WORD are case insensitive.					*/
/*	Strings after # untill the end of the line are regarded as a comment*/
{
	char	*astefil;
	FILE	*fp_astefil;
	char	one_line[MAX_LEN];
	char	*file_part, *key_part;
	int	n, keynum;
	char	tmp_key1[MAX_KEY_LEN];
	char	tmp_key2[MAX_KEY_LEN];
	char	tmp_key3[MAX_KEY_LEN];

	if ( (astefil=getenv(ENV_ASTEFILNAM)) == NULL )
		return(-1);
/*	printf("%s\n", astefil);        */

	if ((fp_astefil=fopen(astefil, "r"))==NULL)
		return(-2);	

	while(fgets(one_line, MAX_LEN, fp_astefil)!=NULL)
		{
/*		printf("!%s!", one_line);	*/
		n=0;
		do	{
			if(opWordCheck(one_line[n])==-1)
				one_line[n]=NULL;
			}
		while(one_line[n++]!=NULL && n<MAX_LEN);
/*		printf("\n%s\n", one_line);	*/

		if ( (file_part=strchr(one_line, '=')) != NULL )
			{
			file_part[0]=NULL;
			file_part++;
/*			printf("file_part=%s\n", file_part);	*/
			if( sscanf(file_part, "%s", filnam) == 1 )
				{
				key_part=one_line;
/*				printf("key_part=%s\n", key_part);	*/
				for(n=0;key_part[n]!=NULL; n++)
					{
					if(opWordCheck(key_part[n])==1)
						key_part[n]=' ';
					}
				tmp_key1[0]=tmp_key2[0]=tmp_key3[0]=NULL;
				if((keynum=sscanf(key_part,"%s %s %s", tmp_key1, tmp_key2, tmp_key3))>0)
					{
/*					printf("%d %s %s %s %s\n", keynum, tmp_key1, tmp_key2, tmp_key3, filnam);*/
					if(strcasecmp(tmp_key1, key1)==0 
						&& strcasecmp(tmp_key2, key2)==0 && strcasecmp(tmp_key3, key3)==0)
						{
						fclose(fp_astefil);
						return(0);
						}
					}
				}
			}
		}
	fclose(fp_astefil);
	return(-3);
}

/*  int jsetenv_(char *string, int *mkmk, int jstrlen) 			*/
/*					 				*/
/*					 				*/
/*  $Id: jsetenv_.c,v 1.1 2002/04/16 20:27:35 irby Exp $				 				*/
/* 					 				*/
/*  Purpose: Interface for FORTRAN to interogate the environment       	*/
/* 	     where environment variable number mkmk is returned to	*/
/* 	     to the calling program rather than selected by NAME	*/
/* 	     as in C function getenv(char*)				*/
/* 					 				*/
/*  Effect:  Adds or replaces contents of string in the environment	*/
/*  Programmer: J.A.Esposito delivered 29 AUG 1995			*/
/* 					 				*/
/* 					 				*/
/*  $Log: jsetenv_.c,v $
/*  Revision 1.1  2002/04/16 20:27:35  irby
/*  New GRO tool 'like'.  Submitted by S.Bansal.
/*
 * Revision 5.1  1996/02/29  20:54:20  jae
 * *** empty log message ***
 *
 * Revision 5.0  1996/02/13  21:57:06  jae
 * Subroutine Module for like V5.00
 *                					 	*/
/* 					 				*/
/* 					 				*/


static char *Id;
extern char **environ;

int jsetenv_(char *string,  int *mkmk, int jstrlen)
{
   int i=0,j=0,k=0;
   char *p;

   Id = "$Id: jsetenv_.c,v 1.1 2002/04/16 20:27:35 irby Exp $";


   k = *mkmk-1;

   if (k < 0) return (-2);
   if (environ[k] == '\0') return(-1);

   p=environ[k];

   while(p[i] != '\0'){string[i]=p[i]; i++; if(i >= jstrlen)return(-i);}

   return(i);
}

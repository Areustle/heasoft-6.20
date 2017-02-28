#include <stdio.h>
#include <math.h>
#include "fitsio.h"
#include "ftsort.h"

int get_tblitem(fitsfile* fptr, int row, int repeat,int dtype, int colnum, tblItem * item)
{
    int status=0;
    int anynull;
    item->dtype=dtype;
    switch(dtype) 
    {
         case TBIT:
         case TBYTE:
         case TSHORT:
         case TINT:
               fits_read_colnull(fptr,TLONG,colnum,row,1,1,
                       &(item->data.i),&(item->undef_flag),&anynull,&status);
         break;
         case TSTRING:
               item->data.s =(char*) malloc((repeat+1)*sizeof(char));
               fits_read_colnull(fptr,TSTRING,colnum,row,1,1,
                       &(item->data.s),&(item->undef_flag),&anynull,&status);
         break;
         case TLONG: 
         case TFLOAT:
         case TDOUBLE:
               fits_read_colnull(fptr,TDOUBLE,colnum,row,1,1,
                       &(item->data.d),&(item->undef_flag),&anynull,&status);
         break;
         case TCOMPLEX:
         case TDBLCOMPLEX:
               fits_read_colnull(fptr,TDBLCOMPLEX,colnum,row,1,1,
                       &(item->data.c),&(item->undef_flag),&anynull,&status);
         break;
     }
     return status;
}

int cmp_tblitem(tblItem item1, tblItem item2)
{
   int greater_than=0;

   if( item1.dtype != item2.dtype ) 
   {
       fprintf(stderr,"CMP_TBLITEM:Error, the two item has different dtype\n");
       return greater_than;
   }

   switch(item1.dtype)
   {
         case TBIT:
         case TBYTE:
         case TSHORT:
         case TINT:
            if(item1.data.i > item2.data.i) greater_than =1;
            else if( item1.data.i < item2.data.i) greater_than=-1;
         break;
         case TSTRING:
            greater_than =strcmp(item1.data.s,item2.data.s);
         break;
         case TLONG: 
         case TFLOAT:
         case TDOUBLE:
            if(item1.data.d > item2.data.d) greater_than =1;
            else if( item1.data.d < item2.data.d) greater_than=-1;
         break;
         case TCOMPLEX:
         case TDBLCOMPLEX:
            if( item1.data.c.real > item2.data.c.real ) greater_than =1;
            else if( item1.data.c.real < item2.data.c.real) greater_than=-1;
            else if( item1.data.c.imag > item2.data.c.imag ) greater_than =1;
            else if( item1.data.c.imag < item2.data.c.imag ) greater_than =-1;
         break;
   }
   return greater_than;
}


int col_cmp1(fitsfile* fptr, int row1, int row2, int repeat,int dtype, int colnum)
{
    tblItem item1,item2;

    get_tblitem(fptr, row1, repeat, dtype, colnum, &item1);
    get_tblitem(fptr, row2, repeat, dtype, colnum, &item2);
    

    return cmp_tblitem(item1,item2);
}


void adjust_heap1(fitsfile* fptr,int start, int root, int n, int ascend,int repeat, int dtype,int colnum)
{
   unsigned char * buffer0;
   unsigned char * buffer1;
   tblItem item0, item1, item2;
   long width;
   int j,status=0;


   fits_read_key_lng(fptr,"NAXIS1",&width,NULL,&status);
   buffer0 = (unsigned char*) malloc(width*sizeof(unsigned char));
   buffer1 = (unsigned char*) malloc(width*sizeof(unsigned char));

   get_tblitem(fptr, start+root, repeat, dtype, colnum, &item0);
   fits_read_tblbytes(fptr,start+root,1,width,buffer0,&status);

   for (j=2*(root+1)-1; j<n; j=(j+1)*2-1)
   {
         if( j < n-1)
         {
              get_tblitem(fptr, start+j, repeat, dtype, colnum, &item1);
              get_tblitem(fptr, start+j+1, repeat, dtype, colnum, &item2);
              if(ascend)
              {
                  if(cmp_tblitem(item1,item2) < 0) j++;
              }
              else 
              {
                 if(cmp_tblitem(item1,item2) > 0 ) j++; 
              }
         }
         get_tblitem(fptr, start+j, repeat, dtype, colnum, &item1);
         if(ascend)
         {
              if(cmp_tblitem(item1,item0) <= 0) break;
         }
         else
         {
              if(cmp_tblitem(item1,item0) >= 0) break;
         }
 
         fits_read_tblbytes(fptr,start+j,1,width,buffer1,&status);
         fits_write_tblbytes(fptr,start+(j+1)/2-1,1,width,buffer1,&status);
   }
   fits_write_tblbytes(fptr,start+(j+1)/2-1,1,width,buffer0,&status);
   
   free(buffer0);
   free(buffer1);
}
   
void heap_sort1(fitsfile* fptr, int start, int end, int ascend,int repeat,int dtype,int colnum)
{
    int i;
    int n;
    long width;
    int status=0;
    unsigned char * buffer;
    unsigned char * buffer1;

    fits_read_key_lng(fptr,"NAXIS1",&width,NULL,&status);
    buffer = (unsigned char*) malloc(width*sizeof(unsigned char));
    buffer1 = (unsigned char*) malloc(width*sizeof(unsigned char));

    n = end - start;


    for (i = n/2-1; i>=0; i--)
    {
         adjust_heap1(fptr,start,i,n,ascend,repeat,dtype,colnum);
    }
   
    for (i = n-2; i>=0; i--)
    {
         fits_read_tblbytes(fptr,start,1,width,buffer,&status);
         fits_read_tblbytes(fptr,start+i+1,1,width,buffer1,&status);
         fits_write_tblbytes(fptr,start,1,width,buffer1,&status);
         fits_write_tblbytes(fptr,start+i+1,1,width,buffer,&status);
         adjust_heap1(fptr,start,0,i+1,ascend,repeat,dtype,colnum);
    }

    free(buffer);
    free(buffer1);
}

    
void insert_sort1(fitsfile* fptr, int start, int end, int ascend,int repeat,int dtype,int colnum)
{
     int i,j,n;
     long width;
     int status=0;
     tblItem item0;
     tblItem item1;
     unsigned char * buffer0;
     unsigned char * buffer1;

     fits_read_key_lng(fptr,"NAXIS1",&width,NULL,&status);
     buffer0 = (unsigned char*) malloc(width*sizeof(unsigned char));
     buffer1 = (unsigned char*) malloc(width*sizeof(unsigned char));

     n =end -start;

     for (j=1; j < n; j++)
     {
        fits_read_tblbytes(fptr,start+j,1,width,buffer0,&status);
        get_tblitem(fptr,start+j,repeat, dtype,colnum, &item0);
        i =j-1;
        while (i >=0) 
        {
            get_tblitem(fptr,start+i,repeat, dtype,colnum, &item1);
            if(ascend) 
            {
                if(cmp_tblitem(item1,item0) <= 0) break;
            }
            else 
            {
                if(cmp_tblitem(item1,item0) >= 0) break;
            }
            fits_read_tblbytes(fptr,start+i,1,width,buffer1,&status);
            fits_write_tblbytes(fptr,start+i+1,1,width,buffer1,&status);
            i--;
        }
        fits_write_tblbytes(fptr,start+i+1,1,width,buffer0,&status);
           
     }
}

void shell_sort1(fitsfile* fptr, int start, int end, int ascend,int repeat,int dtype,int colnum)
{
     double aln2i = 1.442695022;
     double tiny = 1.0e-5;
     int n,nn,m,i,j,lognb2;
     long width;
     tblItem item0, item1;
     unsigned char * buffer0;
     unsigned char * buffer1;
     int greater_than;
     int status=0;

     fits_read_key_lng(fptr,"NAXIS1",&width,NULL,&status);
     buffer0 = (unsigned char*) malloc(width*sizeof(unsigned char));
     buffer1 = (unsigned char*) malloc(width*sizeof(unsigned char));
     
     n =end-start;
     lognb2 = (int)(log((double)n) * aln2i + tiny);

 
     m = n;
     for (nn = 0; nn < lognb2; nn++)
     {
         m >>=1;
         for (j=m; j<n; j++)
         {
             i = j-m;
             get_tblitem(fptr,start+j,repeat, dtype,colnum, &item0);
             fits_read_tblbytes(fptr,start+j,1,width,buffer0,&status);
             while (i >= 0)
             {
                  get_tblitem(fptr,start+i,repeat, dtype,colnum, &item1);
                  if(ascend) 
                       greater_than = cmp_tblitem(item1,item0);
                  else
                       greater_than = cmp_tblitem(item0,item1);
 
                  if( greater_than >0 )
                  {
                      fits_read_tblbytes(fptr,start+i,1,width,buffer1,&status);
                      fits_write_tblbytes(fptr,start+i+m,1,width,buffer1,
                                      &status);
                      i -= m;
                  }
                  else break;
              }
              fits_write_tblbytes(fptr,start+i+m,1,width,buffer0,&status);
          }
      }
}
              

int do_filesort(int start, int end, fitsfile* outfptr,int* ascend, long* repeat,
int* dtype, int* colnum, int ncols, char* method,int unique,int *status)
{

      int i, j, delrows, tdelrows;

      tdelrows =0;


      if(strcmp(method,"heap")==0)
      {
            heap_sort1(outfptr,start,end,*ascend,*repeat,*dtype,*colnum);
      }
      else if(strcmp(method,"insert")==0)
      {
            insert_sort1(outfptr,start,end,*ascend, *repeat,*dtype,*colnum);
      }
      else if(strcmp(method,"shell")==0 )
      {
            shell_sort1(outfptr,start,end,*ascend,*repeat,*dtype,*colnum);
      }

      for ( i = start; i <end; i= j)
      {
          j=i+1;

          while ( j < end && col_cmp1(outfptr,i,j,*repeat,*dtype,*colnum)==0) j++;

          delrows =0;

          if( j>i+1 )
          {
                  if(ncols > 1) 
                  {
                       delrows =do_filesort(i,j,outfptr,ascend+1,repeat+1,dtype+1,
                                  colnum+1,ncols-1,method,unique,status);
                  }
	          else if(unique) 
        	  {
                      ffdrow(outfptr,i+1,j-i-1,status);
                      delrows =j-i-1;
                      /*
                      end =end -delrows;
                      j=i+1;
                      */
                   
          	  }
          }
          end -=delrows;
          tdelrows +=delrows;
          j -=delrows;
          
       }

       return tdelrows;

}


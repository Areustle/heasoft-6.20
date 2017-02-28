#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <cfortran.h>


void ldpfil(int itype, float** tval, int* nmtval, float** efil, int* nin,
                float** tabs, int* ncomp, int* status);

void pflbol(int itype, const float* abun, float* bolo, int* status);
FCALLSCSUB4(pflbol,PFLBOL,pflbol,INT,FLOATV,FLOATV,PINT)

void pflbol(int itype, const float* abun, float* bolo, int* status)
{

   // Subroutine to return the bolometric luminosities for the tabulated 
   // temperatures from one of the plasma emission table files.

   // Arguments :
   //    itype       I        i: type of plasma emission file
   //                               1 = R-S
   //                               2 = Mekal
   //    abun        R        i: elemental abundances
   //    bolo        R        r: bolometric luminosities
   //    status      I        r: 0==OK

   // Translated to C++ Feb. 2009 (C.Gordon)

   const int MKEL = 14;
   const int RSEL = 12;

   float nabun[MKEL];
   float andgrv[MKEL]={9.77e-2, 3.63e-4, 1.12e-4, 8.51e-4, 1.23e-4,
                       2.14e-6, 3.80e-5, 2.95e-6, 3.55e-5, 1.62e-5, 
                       3.63e-6, 2.29e-6, 4.68e-5, 1.78e-6};
   int rselt[RSEL]={0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13};
   string celt[MKEL] = {"He",    "C",    "N",    "O",    "Ne",
			"Na",    "Mg",    "Al",    "Si",    "S",
			"Ar",    "Ca",    "Fe",    "Ni"};

   // set abundances - the we use the Anders & Grevesse abundance
   // scale internally so shift from the Solar abundance table in use.

   if (itype == 1)
   {
      for (int i=0; i<RSEL; ++i)
         nabun[i] = abun[i]*FunctionUtility::getAbundance(celt[rselt[i]])
                           / andgrv[rselt[i]];
   }
   else
   {
      for (int i=0; i<MKEL; ++i)
         nabun[i] = abun[i]*FunctionUtility::getAbundance(celt[i])/andgrv[i];
   }

   // Call ldpfil to either set up the table arrays or just get the pointers
   // if they are already set up.

   float *tval=0, *efil=0, *tabs=0;
   int nmtval=0, nin=0, ncomp=0;
   ldpfil(itype, &tval, &nmtval, &efil, &nin, &tabs, &ncomp, status);
   if (*status)
      return;

   for (int j=0; j<nmtval; ++j)
   {
      bolo[j] = .0;

      // Sum over the H contribution

      int ioff = nin*ncomp*j;
      for (int i=0; i<nin; ++i)
         bolo[j] += tabs[ioff+i]*0.5*(efil[i] + efil[i+1]);

      // Sum over the other elements

      for (int k=0; k<ncomp-1; ++k)
      {
         ioff += nin;
         for (int i=0; i<nin; ++i)
            bolo[j] += nabun[k]*tabs[ioff+i]*0.5*(efil[i] + efil[i+1]);
      }
   }
}

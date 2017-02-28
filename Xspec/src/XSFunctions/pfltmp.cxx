void ldpfil(int itype, float** tval, int* nmtval, float** efil, int* nin,
                float** tabs, int* ncomp, int* status);


void pfltmp(int itype, float** tval, int* nmtval, int* status)
{

   // Subroutine to return the tabulated temperatures from one of the
   // plasma emission table files.

   // Arguments :
   //    itype       I        i: type of plasma emission file
   //                               1 = R-S
   //                               2 = Mekal
   //    itval       I        r: pointer to the array of temperatures
   //    nmtval      I        r: number of temperatures
   //    status      I        r: 0==OK

   // Call ldpfil to either set up the table arrays or just get the pointers
   // if they are already set up.

   float* efil=0, *tabs=0;
   int nin=0, ncomp=0;
   ldpfil(itype, tval, nmtval, &efil, &nin, &tabs, &ncomp, status);
}

#include <string.h>

/* test whether the keywords's comment is a common fits comment: 
   "FITS (Flexible Image Transport System) format defined in Astronomy and
    Astrophysics Supplement Series v44/p363, v44/p371, v73/p359, v73/p365.
    Contact the NASA Science Office of Standards and Technology for the
    FITS Definition document #100 and other FITS information." 

    return 1 if the comment contains one of the above lines. 
*/
    int fit_comment(char *comment) 	/* comment string */
{

    if( strstr(comment,
      "FITS (Flexible Image Transport System) format defined in Astronomy and")
       != NULL) return 1;
     
       
    if( strstr(comment,
      "Astrophysics Supplement Series v44/p363, v44/p371, v73/p359, v73/p365.")
       != NULL) return 1;
    
    if( strstr(comment,
      "Contact the NASA Science Office of Standards and Technology for the")
       != NULL) return 1;
    
    if( strstr(comment,
      "FITS Definition document #100 and other FITS information.") 
       != NULL) return 1;

    return 0;
}

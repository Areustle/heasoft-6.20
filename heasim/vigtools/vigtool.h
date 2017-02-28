/**
 * \file vigtool.h
 * \brief Header file for vigtools
 * \author David Riethmiller
 * \date $Date: 2014/04/04 15:33:03 $
 */

#define STR_MAX_LEN 1500

/**
 * \brief Structure to hold vig data
 * \param[in,out] ncol          number of columns in table 
 * \param[in,out] nrow          number of rows in table 
 * \param[in,out] n_asciilines  number of lines in ascii file 
 * \param[in,out] extname       extention name 
 * \param[in,out] vigtype       vignette file type 
 * \param[in,out] type_desc     character type description 
 * \param[in,out] vig_matrix    the naxis1 x nrow matrix of vignette data 
 * \param[in,out] energy_vec    array of energies 
 * \param[in,out] angle_vec     arrray of angle offsets 
 * \param[in,out] key           array of string keywords 
 * \param[in,out] val           array of string values 
 * \param[in,out] comm          array of string comments 
 * \param[in,out] col_form      array of string column formats 
 * \param[in,out] col_type      array of string column names 
 * \param[in,out] col_unit      array of string column units */
typedef struct{
    /* This structure holds the vignette data */
    int ncol;                     /* number of columns in table */
    int nrow;                     /* number of rows in table */
    int n_asciilines;             /* number of lines in ascii file */
    char extname[STR_MAX_LEN];    /* extention name */
    int vigtype;                  /* vignette file type */
    char type_desc[STR_MAX_LEN];  /* character type description */
    double ** vig_matrix;         /* the naxis1 x nrow matrix of vignette data */
    double * energy_vec;          /* array of energies */
    double * angle_vec;           /* arrray of angle offsets */
    char ** key;                  /* array of string keywords */
    char ** val;                  /* array of string values */
    char ** comm;                 /* array of string comments */
    char ** col_form;             /* array of string column formats */
    char ** col_type;             /* array of string column names */
    char ** col_unit;             /* array of string column units */
} Vig_struct;

/**
 * \brief Remove a substring from a primary string
 * \param[in,out] s     the primary string
 * \param[in] toremove  string to remove */
void remove_substring(char *s, const char *toremove);

/**
 * \brief Create type 1 FITS file
 * \param[in] s_vig          structure containing vig data
 * \param[in] file_vig_fits  name of file to create  */
void write_fits1(Vig_struct * s_vig, char * file_vig_fits);

/**
 * \brief Create type 2 FITS file
 * \param[in] s_vig          structure containing vig data
 * \param[in] file_vig_fits  name of file to create  */
void write_fits2(Vig_struct * s_vig, char * file_vig_fits);

/**
 * \brief Free vig data from memory
 * \param[in] s_vig  structure containing vig data  */
void deallocate_vig(Vig_struct * s_vig);

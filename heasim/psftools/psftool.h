/**
 * \file psftool.h
 * \brief Header file for psftools
 * \author David Riethmiller
 * \date $Date: 2014/04/04 15:41:15 $
 */

#define STR_MAX_LEN 1500


/**
 * \brief Structure to hold psf data
 * \param[in,out] ncol          number of columns in table 
 * \param[in,out] nrow          number of rows in table 
 * \param[in,out] n_asciilines  number of lines in ascii file 
 * \param[in,out] extname       extention name  
 * \param[in,out] psftype       psf file type 
 * \param[in,out] type_desc]    character type description 
 * \param[in,out] energy_vec    array of energies 
 * \param[in,out] angle_vec     array of angle offsets 
 * \param[in,out] radii         array of radii 
 * \param[in,out] eef_matrix    psf matrix 
 * \param[in,out] key           array of string keywords 
 * \param[in,out] val           array of string values 
 * \param[in,out] comm          array of string comments 
 * \param[in,out] col_form      array of string column formats 
 * \param[in,out] col_type      array of string column names  
 * \param[in,out] col_unit      array of string column units 
 * \param[in,out] n_eef         number of energies 
 * \param[in,out] n_radii       number of radii */
typedef struct{
    /* This structure holds the vignette data */
    int ncol;                    /* number of columns in table */
    int nrow;                    /* number of rows in table */
    int n_asciilines;            /* number of lines in ascii file */
    char extname[STR_MAX_LEN];   /* extention name */
    int psftype;                 /* psf file type */
    char type_desc[STR_MAX_LEN]; /* character type description */
    double * energy_vec;         /* array of energies */
    double * angle_vec;          /* array of angle offsets */
    double * radii;              /* array of radii */
    double ** eef_matrix;        /* psf matrix */
    char ** key;                 /* array of string keywords */
    char ** val;                 /* array of string values */
    char ** comm;                /* array of string comments */
    char ** col_form;            /* array of string column formats */
    char ** col_type;            /* array of string column names */
    char ** col_unit;            /* array of string column units */
    int n_eef;                   /* number of energies */
    int n_radii;                 /* number of radii */
} PSF_struct;



/**
 * \brief Remove a substring from a primary string
 * \param[in,out] s     the primary string
 * \param[in] toremove  string to remove */
void remove_substring(char *s, const char *toremove);

/**
 * \brief Create type 1 FITS file
 * \param[in] s_psf          structure containing psf data
 * \param[in] file_psf_fits  name of file to create  */
void write_fits1(PSF_struct * s_psf, char * file_psf_fits);

/**
 * \brief Create type 2 FITS file
 * \param[in] s_psf          structure containing psf data
 * \param[in] file_psf_fits  name of file to create  */
void write_fits2(PSF_struct * s_psf, char * file_psf_fits);

/**
 * \brief Free psf data from memory
 * \param[in] s_psf  structure containing psf data  */
void deallocate_psf(PSF_struct * s_psf);

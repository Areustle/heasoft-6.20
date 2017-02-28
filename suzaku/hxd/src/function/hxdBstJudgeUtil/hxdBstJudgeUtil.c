#include "hxdBstJudgeUtil.h"

/*
** hxdBstJudge_LC_new -
**
**      allocates a new light curve data structure
*/
int
hxdBstJudge_LC_new( hxdBstJudge_LC* lc, int num_row ) {
    lc->time     = ( double* )calloc( num_row, sizeof( double ) );
    lc->rate     = ( double* )calloc( num_row, sizeof( double ) );
    lc->error    = ( double* )calloc( num_row, sizeof( double ) );
    lc->fracexp  = ( double* )calloc( num_row, sizeof( double ) );
    lc->deadc    = ( double* )calloc( num_row, sizeof( double ) );
    lc->timedel  = 0.0;
    lc->tzero    = 0.0;
    lc->tpixr    = 0.0;
    lc->mean     = 0.0;
    lc->variance = 0.0;
    lc->npts     = num_row;
    lc->has_deadc_col = 1;
    if ( lc->time == NULL || lc->rate == NULL ||
         lc->error == NULL || lc->fracexp == NULL ||
         lc->deadc == NULL ) {
        if ( lc->time )    { free( lc->time ); }
        if ( lc->rate )    { free( lc->rate ); }
        if ( lc->error )   { free( lc->error ); }
        if ( lc->fracexp ) { free( lc->fracexp ); }
        if ( lc->deadc )   { free( lc->deadc ); }
        return ANL_NG;
    }
    return ANL_OK;
}


/*
** hxdBstJudge_LC_free -
**
**      frees memory allocated for a light curve
*/
void
hxdBstJudge_LC_free( hxdBstJudge_LC* lc ) {
    if ( !lc ) {
        return;
    }
    if ( lc->time )    { free( lc->time ); }
    if ( lc->rate )    { free( lc->rate ); }
    if ( lc->error )   { free( lc->error ); }
    if ( lc->fracexp ) { free( lc->fracexp ); }
    if ( lc->deadc )   { free( lc->deadc ); }
    lc->time     = ( double* )0;
    lc->rate     = ( double* )0;
    lc->error    = ( double* )0;
    lc->fracexp  = ( double* )0;
    lc->deadc    = ( double* )0;
    lc->timedel  = 0.0;
    lc->tzero    = 0.0;
    lc->tpixr    = 0.0;
    lc->mean     = 0.0;
    lc->variance = 0.0;
    lc->npts     = 0;
    lc->has_deadc_col = 0;
}

/*
** columns to read from light curve
*/
#define LCFITS_NUM_COL 5
enum {
    TIME = 0,
    RATE,
    ERROR,
    FRACEXP,
    DEADC
};

static char* lcfits_colname[ LCFITS_NUM_COL ] = {
    "TIME",
    "RATE*",
    "ERROR*",
    "FRACEXP",
    "DEADC"
};

/*
** hxdBstJudge_LC_read -
**
**      reads a fits light curve
**      must have TIME, RATE, ERROR and FRACEXP columns
*/
int
hxdBstJudge_LC_read( char filename[ ], hxdBstJudge_LC* lc ) {

    char comment[ 80 ];
    char pname[ ] = "hxdBstJudge_LC_read()";

    char* k;

    int i;
    int istat         = 0;
    int anynul        = 0;
    int caseinsen     = TRUE;
    int has_deadc_col = 1;

    int colnum[ LCFITS_NUM_COL ];

    long numrow    = 0;
    long firstrow  = 1;
    long firstelem = 1;

    double nulval  = 0.0;

    fitsfile* fp = ( fitsfile* )0;

    anl_msg_info( "%s: reading light curve in %s\n",
                  pname, filename );


    /*
    ** open and go to 1st data extension (hopefully a light curve)
    */
    if ( fits_open_data( &fp, filename, READONLY, &istat ) ) {
        anl_msg_error( "%s: fits_open_data('%s') failed (%d)\n",
                       pname, filename, istat );
        return ANL_NG;
    }

    /*
    ** get column numbers
    */
    has_deadc_col = 1;
    for( i = 0; i < LCFITS_NUM_COL; i++ ) {
        if ( fits_get_colnum( fp, caseinsen, lcfits_colname[ i ],
                              &colnum[ i ], &istat ) ) {
            if ( i != DEADC && istat != COL_NOT_UNIQUE ) {
                anl_msg_error( "%s: fits_get_colnum('%s') failed on %s (%d)\n",
                               pname, lcfits_colname[ i ], filename, istat );
                return ANL_NG;
            } else if ( i == DEADC ) {
                has_deadc_col = 0;
                istat = 0;
            } else {
                istat = 0;
            }
        }
    }

    /*
    ** get number of rows
    */
    if ( fits_get_num_rows( fp, &numrow, &istat) ) {
        anl_msg_error( "%s: fits_get_num_rows() failed on %s (%d)\n",
                       pname, filename, istat );
        return ANL_NG;
    }
    if ( numrow <= 0 ) {
        anl_msg_error( "%s: no rows in %s\n", pname, filename );
        return ANL_NG;
    }

    /*
    ** allocate a new hxdBstJudge_LC
    */
    if ( hxdBstJudge_LC_new( lc, numrow ) ) {
        anl_msg_error( "%s: failed to allocate memory for light curve\n",
                       pname );
    }
    lc->has_deadc_col = has_deadc_col;

    /*
    ** read some keys
    */
    if ( fits_read_key_dbl( fp, k = "TIMEDEL", &lc->timedel, comment, &istat ) ) {
        anl_msg_error( "%s: fits_read_key_dbl('%s') failed on %s (%d)\n",
                       pname, k, filename, istat );
        return ANL_NG;
    }
    if ( fits_read_key_dbl( fp, k = "TIMEZERO",  &lc->tzero, comment, &istat ) ) {
        istat = 0;
        lc->tzero = 0.0;
        fits_read_errmsg( comment );
    }
    if ( fits_read_key_dbl( fp, k = "TIMEPIXR",  &lc->tpixr, comment, &istat ) ) {
        istat = 0;
        lc->tpixr = 0.5;
        fits_read_errmsg( comment );
    }

    /*
    ** read TIME, RATE, ERROR, and FRACEXP columns
    */
    if ( fits_read_col_dbl( fp, colnum[ TIME ], firstrow, firstelem, numrow,
                            nulval, lc->time, &anynul, &istat ) ) {
        anl_msg_error( "%s: fits_read_col_dbl('%s') failed on %s (%d)\n",
                       pname, lcfits_colname[ TIME ], filename, istat );
        return ANL_NG;
    }
    if ( fits_read_col_dbl( fp, colnum[ RATE ], firstrow, firstelem, numrow,
                            nulval, lc->rate, &anynul, &istat ) ) {
        anl_msg_error( "%s: fits_read_col_dbl('%s') failed on %s (%d)\n",
                       pname, lcfits_colname[ RATE ], filename, istat );
        return ANL_NG;
    }
    if ( fits_read_col_dbl( fp, colnum[ ERROR ], firstrow, firstelem, numrow,
                            nulval, lc->error, &anynul, &istat ) ) {
        anl_msg_error( "%s: fits_read_col_dbl('%s') failed on %s (%d)\n",
                       pname, lcfits_colname[ ERROR ], filename, istat );
        return ANL_NG;
    }
    if ( fits_read_col_dbl( fp, colnum[ FRACEXP ], firstrow, firstelem, numrow,
                            nulval, lc->fracexp, &anynul, &istat ) ) {
        anl_msg_error( "%s: fits_read_col_dbl('%s') failed on %s (%d)\n",
                       pname, lcfits_colname[ FRACEXP ], filename, istat );
        return ANL_NG;
    }
    if ( lc->has_deadc_col &&
         fits_read_col_dbl( fp, colnum[ DEADC ], firstrow, firstelem, numrow,
                            nulval, lc->deadc, &anynul, &istat ) ) {
        anl_msg_error( "%s: fits_read_col_dbl('%s') failed on %s (%d)\n",
                       pname, lcfits_colname[ DEADC ], filename, istat );
        return ANL_NG;
    } else {
        for ( i = 0; i < lc->npts; i++ ) {
            lc->deadc[ i ] = 1.0;
        }
    }

    /*
    ** done
    */
    if ( fits_close_file( fp, &istat ) ) {
        anl_msg_error( "%s: fits_close_file('%s') failed (%d)\n",
                       pname, filename, istat );
        return ANL_NG;
    }
    return ANL_OK;
}

/*
** hxdBstJudge_LC_stats_basic -
**
**      calculates basic light curve statistics and stores them
**      in the input light curve data structure
*/
void
hxdBstJudge_LC_stats_basic( hxdBstJudge_LC* lc, double minthresh,
                            double maxthresh ) {
    int i;

    double dd1 = 0.0;
    double dd2 = 0.0;
    double total_time = 0.0;
    double total_cnt = 0.0;
    double total_cnt_square = 0.0;

    for ( i = 0; i < lc->npts; i++ ) {
        if ( lc->rate[ i ] > minthresh && lc->rate[ i ] < maxthresh ) {
            dd1 = lc->timedel * lc->fracexp[ i ] * lc->deadc[ i ];
            dd2 = lc->rate[ i ] * dd1;
            total_cnt += dd2;
            total_cnt_square += dd2 * dd2;
            total_time += dd1;
        }
    }
    lc->mean = total_cnt / total_time;

    dd1 = 0.0;
    for ( i = 0; i < lc->npts; i++ ) {
        if ( lc->rate[ i ] > minthresh && lc->rate[ i ] < maxthresh ) {
            lc->variance += ( lc->rate[ i ] - lc->mean ) * ( lc->rate[ i ] - lc->mean );
            dd1 += 1.0;
        }
    }
    if ( dd1 > 0.0 ) {
        lc->variance /= dd1;
    } else {
        lc->variance = 0.0;
    }
    lc->stddev = sqrt( lc->variance );
}

/*
** hxdBstJudge_LC_bincenter -
**
**      calculates the center of a light curve time bin
*/
double hxdBstJudge_LC_bincenter( hxdBstJudge_LC* lc, long i ) {
    double time;
    time  = lc->time[ i ];
    time -= lc->timedel * ( lc->tpixr - 0.5 );
    time += lc->tzero;
    return time;
}

/*
** hxdBstJudge_LC_bst_judge -
**
**      detects bursts
*/
void
hxdBstJudge_LC_bst_judge( hxdBstJudge_LC* lc, hxdBstJudge_Trigger** intrigs,
                          int nintrigs, hxdBstJudge_Trigger*** outtrigs,
                          int* ntrigs, hxdBstJudge_DetAlg det_alg,
                          double gaptol, double overlaptol, double maxdur,
                          int durest, int* status ) {

    int i, j, k;
    int burst_det;
    int in_burst;
    int zeroflag = 0;

    double winsize;
    double last_win_start;
    double judge_time       = 0.0;
    double bgd_integ_time   = 0.0;
    double sum_bgd          = 0.0;
    double bgd_mean         = 0.0;
    double bgd_tot_time     = 0.0;
    double cnt_src          = 0.0;
    double src_rate         = 0.0;
    double src_tot_time     = 0.0;
    double poisson          = 0.0;
    double cnt_level        = 0.0;
    double sigma_level      = 0.0;
    double pre_sigma_level  = 0.0;
    double pre_cnt_src      = 0.0;
    double pre_cnt_src_rate = 0.0;
    double dummy;

    hxdBstJudge_Trigger* best    = ( hxdBstJudge_Trigger* ) 0;
    hxdBstJudge_Trigger* newbest = ( hxdBstJudge_Trigger* ) 0;


    judge_time     = det_alg == HETE2 ? 1.0 : intrigs[0]->fstop - intrigs[0]->fstart;
    bgd_integ_time = det_alg == HETE2 ? 8.0 : intrigs[0]->b1stop - intrigs[0]->b1start;

    /* calculate the window size for STEP detection and check */
    last_win_start = lc->time[ 0 ];
    winsize = intrigs[0]->b1stop - intrigs[0]->b1start;
    if ( det_alg == STEP && winsize < 60.0 ) {
        fprintf( stderr, "STEP detection window must be > 60 seconds\n" );
        *status = ANL_NG;
        return;
    }

    *ntrigs = 0;


    /*
    ** loop over light curve
    */
    for ( i = ( int )( bgd_integ_time / lc->timedel ); i < lc->npts; i++ ) {



        /* check if we are still in the last detected burst */
        if ( *ntrigs > 0 ) {
            in_burst = ( lc->time[ i ] <= ( *outtrigs )[ *ntrigs - 1 ]->fstop );
        } else {
            in_burst = 0;
        }


        /**********************************************
        ** This section is for the GIGNA method and for
        ** detecting and skipping over WAM "scans"
        **********************************************/
        /*
        ** get "background" counts and mean background rate
        */
        sum_bgd      = 0.0;
        bgd_tot_time = 0.0;
        j = i - ( int )( bgd_integ_time / lc->timedel );
        j = j < 0 ? 0 : j;
        for( ; j < i; j++ ) {
            sum_bgd += lc->rate[ j ] * lc->timedel * lc->fracexp[ j ] * lc->deadc[ j ];
            bgd_tot_time += lc->timedel * lc->fracexp[ j ] * lc->deadc[ j ];
            if ( lc->rate[ j ] <= 1.0 || ( j > 0 && lc->rate[ j - 1 ] <= 1.0 ) ||
                 lc->rate[ j + 1 ] <= 1.0 ) {
                zeroflag = 1;
            }
        }
        if ( bgd_tot_time != 0.0 ) {
            bgd_mean = sum_bgd / bgd_tot_time;
        } else {
            bgd_mean = 0.0;
        }


        /*
        ** calculation for source times
        */
        cnt_src = 0.0;
        src_tot_time = 0.0;
        for ( k = i; k < i + ( int )( judge_time / lc->timedel ) && k < lc->npts; k++ ) {
            cnt_src += lc->rate[ k ] * lc->timedel * lc->fracexp[ k ] * lc->deadc[ k ];
            src_tot_time += lc->timedel * lc->fracexp[ k ] * lc->deadc[ k ];
            if ( lc->rate[ k ] <= 1.0 || lc->rate[ k - 1 ] <= 1.0 ||
                 ( k + 1 < lc->npts && lc->rate[ k + 1 ] <= 1.0 ) ) {
                zeroflag = 1;
            }
        }
        if ( src_tot_time != 0.0 ) {
            src_rate = cnt_src / src_tot_time;
        } else {
            src_rate = 0.0;
        }



        /*
        ** calculate source significance
        */
        poisson     = sqrt( bgd_mean * src_tot_time );
        cnt_level   = cnt_src - bgd_mean * src_tot_time;
        sigma_level = cnt_level / poisson;



        /*
        ** probably during the WAM scan disable for 10 minutes
        */
        if ( pre_sigma_level < -10.0 &&
             src_rate < ( lc->mean - lc->stddev ) &&
             src_rate > 1.0 &&
             pre_cnt_src_rate < ( lc->mean - lc->stddev ) &&
             pre_cnt_src_rate > 1.0 ) {
            j = i;
            while ( i < lc->npts &&
                    lc->time[ i ] - bgd_integ_time / lc->timedel < lc->time[ j ] + 600.0 ) {
                lc->rate[ i ] = 0.0;
                lc->error[ i ] = 0.0;
                i++;
            }
            /*printf( "Skipping WAM scan from T=%f to T=%f\n", lc->time[ j ],
                    lc->time[ i ] - bgd_integ_time / lc->timedel );*/

            /*
            ** if we're inside a detected burst, delete it since it probably is just
            ** a detection of the "scan".
            */
            if ( *ntrigs > 0 ) {
                best = ( *outtrigs )[ *ntrigs - 1 ];
                if ( ( best->b2start <= best->b2stop &&
                       lc->time[ j ] <= best->b2stop + 2.0 ) ||
                     ( best->b2start > best->b2stop &&
                       lc->time[ j ] <= best->fstop + 2.0 ) ) {
                    if ( best ) {
                        hxdBstJudge_Trigger_free( &best );
                    }
                    ( *ntrigs )--;
                }
                best = ( hxdBstJudge_Trigger* ) 0;
            }
            continue;
        }


        /*
        ** update WAM scan check vars
        */
        pre_cnt_src     = cnt_src;
        pre_sigma_level = sigma_level;
        if ( src_tot_time != 0.0 ) {
            pre_cnt_src_rate = pre_cnt_src / src_tot_time;
        } else {
            pre_cnt_src_rate = 0.0;
        }

        /* next if any zero rates encountered */
        if ( zeroflag ) {
            zeroflag = 0;
            continue;
        }

        /* next if in burst */
        if ( in_burst ) {
            continue;
        }


        /*
        ** do the trigger calc if using HETE2 algorithm
        ** and check if there's a burst detected (either algorithm)
        */
        burst_det = 0;
        if ( det_alg == HETE2 ) {
            best = hxdBstJudge_Trigger_get_best( intrigs, nintrigs, lc,
                                                 i, 1, gaptol, status );
            if ( *status != ANL_OK ) {
                return;
            }
            burst_det = ( best && best->sigma >= intrigs[0]->sigma ) ? 1 : 0;
        } else if ( det_alg == GINGA ) {
            burst_det = sigma_level >= intrigs[0]->sigma ? 1 : 0;
        } else if ( det_alg == STEP ) {
            if ( lc->time[ i ] - last_win_start >= winsize / 2.0 ) {
                best = hxdBstJudge_Step_calc( lc, i, winsize,
                                              intrigs[0]->sigma, 1.3, 0.99,
                                              intrigs[0]->fore_rateerr, status );
                burst_det = ( best && fabs(best->sigma) >= intrigs[0]->sigma )
                                ? 1 : 0;
                last_win_start = lc->time[ i ];

            }
        }

        /*
        ** if so, add it to the list to refine later
        */
        if ( burst_det ) {

            if ( det_alg == HETE2 ) {
                if ( durest ) {
                    hxdBstJudge_Trigger_zero( best );
                    newbest = hxdBstJudge_Trigger_calcbest( best, lc, j, 1, gaptol,
                                                            overlaptol, maxdur, status );
                    hxdBstJudge_Trigger_free( &best );
                } else {
                    newbest = best;
                }
                best = ( hxdBstJudge_Trigger* ) 0;
            } else if ( det_alg == GINGA ) {
                newbest = hxdBstJudge_Trigger_calc( intrigs[0], lc, i, 0, gaptol, status );
                if ( *status != ANL_OK ) {
                    return;
                }
                /* re-calculate sigma, since the real GINGA method
                ** defines sigma slightly differently than "we" do:
                **
                **      sig = net_counts / sqrt( estimated bkg_counts )
                */
                if ( newbest ) {
                    newbest->sigma = newbest->net_cnts / sqrt( newbest->back_cnts );
                }
            } else {
                newbest = best;
                best = ( hxdBstJudge_Trigger* ) 0;
            }
            if ( !newbest ) {
                continue;
            }

            ( *ntrigs )++;

            /*
            ** save in output list
            */
            *outtrigs = ( hxdBstJudge_Trigger** )
                        realloc( *outtrigs,
                                 *ntrigs * sizeof( hxdBstJudge_Trigger* ) );
            if ( !*outtrigs ) {
                *status = ANL_NG;
                return;
            }
            ( *outtrigs )[ *ntrigs - 1 ] = newbest;
        } else {
            if ( best ) {
                hxdBstJudge_Trigger_free( &best );
                best = ( hxdBstJudge_Trigger* ) 0;
            }
        }
    }

    /*
    ** for HETE2 there may still be overlapping or "touching" detections
    ** so merge them (within overlaptol)
    */
    if ( det_alg == HETE2 ) {
        for ( i = 1; i < *ntrigs; i++ ) {
            best = ( *outtrigs )[ i ];
            newbest = ( *outtrigs )[ i - 1 ];
            if ( best && newbest &&
                 newbest->fstop >= best->fstart - overlaptol ) {

                /* re-calculate the trigger */
                newbest->b2start += ( best->fstop - newbest->fstop );
                newbest->b2stop  += ( best->fstop - newbest->fstop );
                newbest->fstop = best->fstop;
                j = newbest->ifstart;

                hxdBstJudge_Trigger_zero( newbest );
                hxdBstJudge_Trigger_free( &best );
                if ( durest ) {
                    best = hxdBstJudge_Trigger_calcbest( newbest, lc, j, 1, gaptol,
                                                         overlaptol, maxdur, status );
                } else {
                    best = hxdBstJudge_Trigger_calc( newbest, lc, j, 1, gaptol, status );
                }
                ( *outtrigs )[ i - 1 ] = best;
                best = ( hxdBstJudge_Trigger* ) 0;
                hxdBstJudge_Trigger_free( &newbest );
                newbest = ( hxdBstJudge_Trigger* ) 0;
                for ( j = i + 1; j < *ntrigs; j++ ) {
                    ( *outtrigs )[ j - 1 ] = ( *outtrigs )[ j ];
                }
                ( *ntrigs )--;
                i--;
            }
        }
    }

    /*
    ** loop over detected bursts and characterize them
    */
    k = 0;
    for ( i = 0; i < *ntrigs; i++ ) {

        best = ( *outtrigs )[ i ];

        /*
        ** check if we've got a detection
        */
        burst_det = 0;
        if ( det_alg == HETE2 ) {
            burst_det = best && best->sigma >= intrigs[0]->sigma;
        } else if ( det_alg == GINGA ) {
            burst_det = best && best->sigma >= intrigs[0]->sigma;
        } else {
            /*burst_det = best && best->sigma >= intrigs[0]->sigma;*/
            burst_det = 1;
        }

        if ( !best || !burst_det ) {
            for ( j = i + 1; j < *ntrigs; j++ ) {
                ( *outtrigs )[ j - 1 ] = ( *outtrigs )[ j ];
            }
            ( *ntrigs )--;
            i--;
            continue;
        }

        /* if STEP mode, check for overlapping steps */
        /* pick the one with the best sigma */
        if ( i < *ntrigs - 1 && det_alg == STEP && ( *outtrigs )[ i + 1 ] ) {
            newbest = ( *outtrigs )[ i + 1 ];
            if ( ( newbest->fstart >= best->fstart && newbest->fstart <= best->fstop ) ||
                 ( newbest->fstop  >= best->fstart && newbest->fstop  <= best->fstop ) ||
                 ( newbest->fstart <= best->fstart && newbest->fstop  >= best->fstop ) ) {
                if ( newbest->sigma > best->sigma ) {
                    for ( j = i + 1; j < *ntrigs; j++ ) {
                        ( *outtrigs )[ j - 1 ] = ( *outtrigs )[ j ];
                    }
                } else {
                    for ( j = i + 2; j < *ntrigs; j++ ) {
                        ( *outtrigs )[ j - 1 ] = ( *outtrigs )[ j ];
                    }
                }
                ( *ntrigs )--;
                i--;
                continue;
            }
        }

        /* calculate T50/T90 */
        if ( det_alg != STEP &&
                hxdBstJudge_Trigger_burstdur( best, lc ) ) {
            *status = ANL_NG;
            return;
        }
        ( *outtrigs )[ k ] = best;
        k++;
    }


}

/*
** hxdBstJudge_Trigger_new -
**
**      allocates a new trigger data structure and initializes it
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_new( double fstart, double fdur, double b1dur,
                         double b1offset, double b2dur, double b2offset,
                         double sigma, int bgorder ) {
    hxdBstJudge_Trigger* trig;

    trig = ( hxdBstJudge_Trigger* ) malloc( sizeof( hxdBstJudge_Trigger ) );
    if ( !trig ) {
        return ( hxdBstJudge_Trigger* ) 0;
    }
    trig->ifstart      = 0;
    trig->fstart       = fstart;
    trig->fstop        = fstart + fdur;
    trig->b1stop       = fstart - b1offset;
    trig->b1start      = trig->b1stop - b1dur;
    trig->b2start      = trig->fstop + b2offset;
    trig->b2stop       = trig->b2start + b2dur;
    trig->fore_cnts    = 0.0;
    trig->fore_rate    = 0.0;
    trig->fore_rateerr = 0.0;
    trig->back_cnts    = 0.0;
    trig->back_rate    = 0.0;
    trig->back_rateerr = 0.0;
    trig->net_cnts     = 0.0;
    trig->net_rate     = 0.0;
    trig->net_rateerr  = 0.0;
    trig->sigma        = sigma;
    trig->bgorder      = bgorder;
    trig->t50          = 0.0;
    trig->t50err       = 0.0;
    trig->t50start     = 0.0;
    trig->t50stop      = 0.0;
    trig->t90          = 0.0;
    trig->t90err       = 0.0;
    trig->t90start     = 0.0;
    trig->t90stop      = 0.0;
    if ( bgorder >= 0 ) {
        trig->bgcoeffs = ( double* ) malloc( ( bgorder + 1 ) *
                                             sizeof( double ) );
        if ( !trig->bgcoeffs ) {
            free( trig );
            trig = ( hxdBstJudge_Trigger* ) 0;
        }
    } else {
        trig->bgcoeffs = ( double* ) 0;
    }
    return trig;
}

/*
** hxdBstJudge_Trigger_free -
**
**      frees memory allocated to a trigger
*/
void hxdBstJudge_Trigger_free( hxdBstJudge_Trigger** trig ) {

    if ( !*trig ) {
        return;
    }
    if ( ( *trig )->bgcoeffs ) {
        free( ( *trig )->bgcoeffs );
        ( *trig )->bgcoeffs = ( double* ) 0;
    }
    free( *trig );
    *trig = ( hxdBstJudge_Trigger* ) 0;
}

/*
** hxdBstJudge_Trigger_zero -
**
**      zeroes a trigger by shifting the foreground start time to zero, and
**      all other times relative to it. Does not touch any of the non-time
**      related members.
*/
void hxdBstJudge_Trigger_zero( hxdBstJudge_Trigger* trig ) {

    if ( !trig ) {
        return;
    }
    trig->fstop   -= trig->fstart;
    trig->b1stop  -= trig->fstart;
    trig->b1start -= trig->fstart;
    trig->b2start -= trig->fstart;
    trig->b2stop  -= trig->fstart;
    trig->fstart  = 0.0;
    trig->ifstart = 0;
}

/*
** hxdBstJudge_Trigger_deepcopy -
**
**      performs a deep copy of 1 trigger data structure to another
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_deepcopy( hxdBstJudge_Trigger* trig ) {

    int i;

    hxdBstJudge_Trigger* outtrig;
    
    outtrig = ( hxdBstJudge_Trigger* ) malloc( sizeof( hxdBstJudge_Trigger ) );
    if ( !outtrig ) {
        return ( hxdBstJudge_Trigger* ) 0;
    }
    outtrig->ifstart      = trig->ifstart;
    outtrig->fstart       = trig->fstart;
    outtrig->fstop        = trig->fstop;
    outtrig->b1stop       = trig->b1stop;
    outtrig->b1start      = trig->b1start;
    outtrig->b2start      = trig->b2start;
    outtrig->b2stop       = trig->b2stop;
    outtrig->fore_cnts    = trig->fore_cnts;
    outtrig->fore_rate    = trig->fore_rate;
    outtrig->fore_rateerr = trig->fore_rateerr;
    outtrig->back_cnts    = trig->back_cnts;
    outtrig->back_rate    = trig->back_rate;
    outtrig->back_rateerr = trig->back_rateerr;
    outtrig->net_cnts     = trig->net_cnts;
    outtrig->net_rate     = trig->net_rate;
    outtrig->net_rateerr  = trig->net_rateerr;
    outtrig->sigma        = trig->sigma;
    outtrig->bgorder      = trig->bgorder;
    outtrig->t50          = trig->t50;
    outtrig->t50err       = trig->t50err;
    outtrig->t50start     = trig->t50start;
    outtrig->t50stop      = trig->t50stop;
    outtrig->t90          = trig->t90;
    outtrig->t90err       = trig->t90err;
    outtrig->t90start     = trig->t90start;
    outtrig->t90stop      = trig->t90stop;
    if ( trig->bgorder >= 0 && trig->bgcoeffs ) {
        outtrig->bgcoeffs = ( double* ) malloc( ( trig->bgorder + 1 ) *
                                                sizeof( double ) );
        if ( !outtrig->bgcoeffs ) {
            free( outtrig );
            outtrig = ( hxdBstJudge_Trigger* ) 0;
        } else {
            for ( i = 0; i <= trig->bgorder; i++ ) {
                if ( trig->bgcoeffs ) {
                    outtrig->bgcoeffs[ i ] = trig->bgcoeffs[ i ];
                } else {
                    outtrig->bgcoeffs[ i ] = 0.0;
                }
            }
        }
    } else {
        outtrig->bgcoeffs = ( double* ) 0;
    }
    return outtrig;
}

/*
** HXDbstJudge_Trigger_get_best -
**
**      calculates the best trigger for a light curve at a given time
**      choosing among all input triggers
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_get_best( hxdBstJudge_Trigger** intrigs, long ntrigs,
                              hxdBstJudge_LC* lc, long itrig, int bgorder,
                              double gaptol, int* status ) {
    long i;

    hxdBstJudge_Trigger* besttrig = ( hxdBstJudge_Trigger* ) 0;
    hxdBstJudge_Trigger* calctrig = ( hxdBstJudge_Trigger* ) 0;

    if ( *status != ANL_OK ) {
        return besttrig;
    }

    /*
    ** check if we are off the end of the light curve
    */
    if ( itrig >= lc->npts ) {
        return besttrig;
    }

    /*
    ** check each input trigger
    */
    for ( i = 0; i < ntrigs; i++ ) {

        /*
        ** calculate this trigger
        */
        calctrig = hxdBstJudge_Trigger_calc( intrigs[ i ], lc, itrig,
                                             bgorder, gaptol, status );
        if ( *status != ANL_OK ) {
            return besttrig;
        }

        /*
        ** see if it's the best
        */
        if ( calctrig && besttrig ) {
            if ( calctrig->sigma > besttrig->sigma ) {
                hxdBstJudge_Trigger_free( &besttrig );
                besttrig = calctrig;
            } else {
                hxdBstJudge_Trigger_free( &calctrig );
                calctrig = ( hxdBstJudge_Trigger* ) 0;
            }
        } else if ( calctrig ) {
            besttrig = calctrig;
        }
    }

    /*
    ** return the best trigger found, regardless if it is NULL
    */
    return besttrig;
}

/*
** hxdBstJudge_Trigger_calcbest -
**
**      calculates the best trigger for a light curve at a given time
**      given an input trigger. It does so by adjusting the fore and 
**      background intervals until the S/N is maximized.
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_calcbest( hxdBstJudge_Trigger* intrig,
                              hxdBstJudge_LC* lc, long itrig,
                              long bgorder, double gaptol, double overlaptol,
                              double maxdur, int* status ) {

    long i = itrig;
    long j = 0;
    int drops;

    hxdBstJudge_Trigger* trig    = ( hxdBstJudge_Trigger* ) 0;
    hxdBstJudge_Trigger* best    = ( hxdBstJudge_Trigger* ) 0;
    hxdBstJudge_Trigger* newbest = ( hxdBstJudge_Trigger* ) 0;

    if ( *status != ANL_OK ) {
        return best;
    }

    best = hxdBstJudge_Trigger_calc( intrig, lc, i, bgorder, gaptol, status );
    if ( !best || *status != ANL_OK ) {
        return best;
    }

    /*
    ** calculate the best trigger by extending the "late" trigger background
    ** and the foreground interval
    */
    trig = hxdBstJudge_Trigger_deepcopy( intrig );
    if ( !trig ) {
        *status = ANL_NG;
        return best;
    }
    hxdBstJudge_Trigger_zero( trig );
    drops = 0;
/*
    printf( "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
            trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
    do {
        trig->fstop   += lc->timedel;
        trig->b2start += lc->timedel;
        trig->b2stop  += lc->timedel;
        newbest = hxdBstJudge_Trigger_calc( trig, lc, i, bgorder, gaptol, status );
        if ( *status != ANL_OK ) {
            return best;
        }
        if ( newbest && newbest->sigma > best->sigma ) {
            hxdBstJudge_Trigger_free( &best );
            best = newbest;
            drops = 0;
/*
            printf( "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
                    trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
        } else {
            hxdBstJudge_Trigger_free( &newbest );
            newbest = ( hxdBstJudge_Trigger* ) 0;
            drops++;
        }
        /* quit if sigma drops more than overlaptol */
        if ( drops * lc->timedel > overlaptol ) {
            break;
        }
    } while ( trig->fstop - trig->fstart <= maxdur );

    /*
    ** try also shrinking the foreground and late background intervals
    */
    hxdBstJudge_Trigger_free( &trig );
    trig = hxdBstJudge_Trigger_deepcopy( best );
    if ( !trig ) {
        *status = ANL_NG;
        return best;
    }
    hxdBstJudge_Trigger_zero( trig );
/*
    printf( "\n%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
            trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
    while ( trig->fstop - trig->fstart > intrig->fstop - intrig->fstart ) {
        trig->fstop   -= lc->timedel;
        trig->b2start -= lc->timedel;
        trig->b2stop  -= lc->timedel;
        if ( trig->fstop - trig->fstart < lc->timedel ) {
            break;
        }
    }
    while ( trig->fstop - trig->fstart >= lc->timedel ) {
        trig->fstop   -= lc->timedel;
        trig->b2start -= lc->timedel;
        trig->b2stop  -= lc->timedel;
        if ( trig->fstop - trig->fstart < lc->timedel ) {
            break;
        }
        newbest = hxdBstJudge_Trigger_calc( trig, lc, i, bgorder, gaptol, status );
        if ( *status != ANL_OK ) {
            return best;
        }
        if ( newbest && newbest->sigma > best->sigma ) {
            hxdBstJudge_Trigger_free( &best );
            best = newbest;
/*
            printf( "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
                    trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
        } else {
            hxdBstJudge_Trigger_free( &newbest );
            newbest = ( hxdBstJudge_Trigger* ) 0;
        }
    }

    /*
    ** also try extending the "early" trigger background
    ** and the foreground interval (in the other direction)
    */
    hxdBstJudge_Trigger_free( &trig );
    trig = hxdBstJudge_Trigger_deepcopy( best );
    if ( !trig ) {
        *status = ANL_NG;
        return best;
    }
    hxdBstJudge_Trigger_zero( trig );
    drops = 0;
/*
    printf( "\n%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
            trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
    j = i;
    do {
        i--;
        trig->fstart  -= lc->timedel;
        trig->b1start -= lc->timedel;
        trig->b1stop  -= lc->timedel;
        newbest = hxdBstJudge_Trigger_calc( trig, lc, i, bgorder, gaptol, status );
        if ( *status != ANL_OK ) {
            return best;
        }
        if ( newbest && newbest->sigma > best->sigma ) {
            hxdBstJudge_Trigger_free( &best );
            best = newbest;
            drops = 0;
            j = i;
/*
            printf( "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
                    trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
        } else {
            hxdBstJudge_Trigger_free( &newbest );
            newbest = ( hxdBstJudge_Trigger* ) 0;
            drops++;
        }
        /* quit if sigma drops more than overlaptol */
        if ( drops * lc->timedel > overlaptol ) {
            break;
        }
    } while ( trig->fstop - trig->fstart <= maxdur );
    i = j;

    /*
    ** last, try shrinking the early background and forground intervals
    ** from the early side
    */
    hxdBstJudge_Trigger_free( &trig );
    trig = hxdBstJudge_Trigger_deepcopy( best );
    if ( !trig ) {
        *status = ANL_NG;
        return best;
    }
    hxdBstJudge_Trigger_zero( trig );
/*
    printf( "\n%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
            trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
    i = best->ifstart;
    while ( i < itrig ) {
        i++;
        trig->fstart  += lc->timedel;
        trig->b1start += lc->timedel;
        trig->b1stop  += lc->timedel;
        if ( trig->fstop - trig->fstart < lc->timedel ) {
            break;
        }
    }
    while ( trig->fstart < trig->fstop ) {
        i++;
        trig->fstart  += lc->timedel;
        trig->b1start += lc->timedel;
        trig->b1stop  += lc->timedel;
        if ( trig->fstop - trig->fstart < lc->timedel ) {
            break;
        }
        newbest = hxdBstJudge_Trigger_calc( trig, lc, i, bgorder, gaptol, status );
        if ( *status != ANL_OK ) {
            return best;
        }
        if ( trig->fstart <= trig->fstop && newbest &&
             newbest->sigma > best->sigma ) {
            hxdBstJudge_Trigger_free( &best );
            best = newbest;
/*
            printf( "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f\n", trig->fstart, trig->fstop,
                    trig->b1start, trig->b1stop, trig->b2start, trig->b2stop );
*/
        } else {
            hxdBstJudge_Trigger_free( &newbest );
        }
    }
    hxdBstJudge_Trigger_free( &trig );
    trig = ( hxdBstJudge_Trigger* ) 0;
    return best;
}

/*
** hxdBstJudge_Trigger_calc -
**
**      calculates a trigger on a light curve
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_calc( hxdBstJudge_Trigger* intrig,
                          hxdBstJudge_LC* lc, long itrig,
                          int bgorder, double gaptol, int* status ) {
    long i, j;

    double dummy;

    hxdBstJudge_Trigger* calctrig = ( hxdBstJudge_Trigger* ) 0;

    if ( *status != ANL_OK ) {
        return calctrig;
    }

    if ( itrig < 0 || itrig >= lc->npts ) {
        return calctrig;
    }

    /*
    ** look ahead and back to see if we're at a gap and check for any zero rates
    */
    i = itrig;
    if ( intrig->b2stop >= intrig->b2start ) {
        dummy = intrig->b2stop - intrig->fstart;
        j = i + ( int )( 0.5 + dummy / lc->timedel );
        if ( j <= i || j >= lc->npts || lc->time[ j ] - lc->time[ i ] > dummy + 2.0 * lc->timedel ) {
            /*printf( "fore gap T[%d]=%f to T[%d]=%f\n", i, lc->time[ i ], j, lc->time[ j ] );*/
            return calctrig;
        }
        for ( i = itrig; i <= j; i++ ) {
            if ( lc->rate[ i ] < 1.0 ) {
                return calctrig;
            }
        }
    } else {
        dummy = intrig->fstop - intrig->fstart;
        j = i + ( int )( 0.5 + dummy / lc->timedel );
        if ( j <= i || j >= lc->npts || lc->time[ j ] - lc->time[ i ] > dummy + 2.0 * lc->timedel ) {
            /*printf( "fore gap T[%d]=%f to T[%d]=%f\n", i, lc->time[ i ], j, lc->time[ j ] );*/
            return calctrig;
        }
        for ( i = itrig; i <= j; i++ ) {
            if ( lc->rate[ i ] < 1.0 ) {
                return calctrig;
            }
        }
    }
    i = itrig;
    dummy = intrig->fstart - intrig->b1start;
    j = i - ( int )( 0.5 + dummy / lc->timedel );
    if ( j <= 0 || lc->time[ i ] - lc->time[ j ] > dummy + 2.0 * lc->timedel ) {
        /*printf( "back gap T[%d]=%f to T[%d]=%f\n", i, lc->time[ i ], j, lc->time[ j ] );*/
        return calctrig;
    }
    for ( i = itrig; i >= j; i-- ) {
        if ( lc->rate[ i ] < 1.0 ) {
            return calctrig;
        }
    }

    /*
    ** allocate trigger
    */
    calctrig = hxdBstJudge_Trigger_deepcopy( intrig );
    if ( !calctrig ) {
        *status = ANL_NG;
        return calctrig;
    }

    /*
    ** estimate the sigma detection level in the foreground interval
    */
    hxdBstJudge_Trigger_sigma( lc, itrig, calctrig, bgorder, status );

    /*
    ** done
    */
    return calctrig;
}

/*
** hxdBstJudge_Trigger_sigma -
**
**      estimates the gaussian sigma detection level for a given
**      trigger and an input light curve
*/
void hxdBstJudge_Trigger_sigma( hxdBstJudge_LC* lc, long itrig,
                                hxdBstJudge_Trigger* intrig, int bgorder,
                                int* status ) {
    long ib1start;
    long ib1stop;
    long ifstart;
    long ifstop;
    long ib2start;
    long ib2stop;
    long i, j, k;
    long nbkg;

    double fore_tot_time = 0.0;

    double ddummy1   = 0.0;
    double ddummy2   = 0.0;
    double ddummy3   = 0.0;
    
    double* bgtime = ( double* ) 0;
    double* bgrate = ( double* ) 0;
    double* bgerr  = ( double* ) 0;

    if ( *status != ANL_OK ) {
        return;
    }

    intrig->sigma = 0.0;

    /*
    ** calculate the background and foreground start/stop times
    ** and related indices into the light curve
    */
    ifstart  = intrig->ifstart = itrig;
    ifstop   = itrig + ( long )( ( intrig->fstop   - intrig->fstart ) / lc->timedel + 0.5 );
    ifstop   = ifstop < ifstart ? ifstart : ifstop;

    ib1stop  = itrig + ( long )( ( intrig->b1stop  - intrig->fstart ) / lc->timedel + 0.5 );
    ib1stop  = ib1stop >= ifstart ? ifstart - 1 : ib1stop;
    ib1start = ib1stop - ( long )( ( intrig->b1stop - intrig->b1start ) / lc->timedel + 0.5 );

    ib2start = itrig + ( long )( ( intrig->b2start - intrig->fstart ) / lc->timedel + 0.5 );
    ib2start = ib2start <= ifstop ? ifstop + 1 : ib2start;
    ib2stop  = ib2start + ( long )( ( intrig->b2stop - intrig->b2start ) / lc->timedel + 0.5 );

    intrig->b1start = hxdBstJudge_LC_bincenter( lc, ib1start );
    intrig->b1stop  = hxdBstJudge_LC_bincenter( lc, ib1stop );
    intrig->b2start = hxdBstJudge_LC_bincenter( lc, ib2start );
    intrig->b2stop  = hxdBstJudge_LC_bincenter( lc, ib2stop );
    intrig->fstart  = hxdBstJudge_LC_bincenter( lc, ifstart );
    intrig->fstop   = hxdBstJudge_LC_bincenter( lc, ifstop );

    /* quick sanity check */
    if ( ib1stop  < 0 || ib1stop  >= lc->npts ||
         ib2stop  < 0 || ib2stop  >= lc->npts ||
         ifstop   < 0 || ifstop   >= lc->npts ||
         ib1start < 0 || ib1start >= lc->npts ||
         ib2start < 0 || ib2start >= lc->npts ||
         ifstart  < 0 || ifstart  >= lc->npts ) {
        return;
    }

    /*
    ** allocate mem for partial background light curve
    */
    j = 0;
    nbkg = 0;
    for ( i = ib1start; i <= ib1stop; i++, j++ ) {
        if ( lc->rate[ i ] > 1.0 ) {
            nbkg++;
        }
    }
    for ( i = ib2start; i <= ib2stop; i++, j++ ) {
        if ( lc->rate[ i ] > 1.0 ) {
            nbkg++;
        }
    }
    if ( nbkg <= 0 ) {
        return;
    }
    bgtime = ( double* ) malloc( nbkg * sizeof( double ) );
    bgrate = ( double* ) malloc( nbkg * sizeof( double ) );
    bgerr  = ( double* ) malloc( nbkg * sizeof( double ) );
    if ( !bgtime || !bgrate || !bgerr ) {
        fprintf( stderr, "hxdBstJudge_Trigger_sigma(): Could not allocate memory!\n" );
        *status = ANL_NG;
        return;
    }

    /*
    ** allocate mem for fit coefficients
    */
    if ( intrig->bgcoeffs ) {
        free( intrig->bgcoeffs );
    }
    intrig->bgcoeffs = ( double* ) calloc( ( bgorder + 1 ), sizeof( double ) );
    if ( !intrig->bgcoeffs ) {
        fprintf( stderr, "hxdBstJudge_Trigger_sigma(): Could not allocate memory!\n" );
        *status = ANL_NG;
        return;
    }
    intrig->bgorder = bgorder;

    /*
    ** setup a partial background light curve to fit
    */
    j = 0;
    for ( i = ib1start; i <= ib1stop; i++ ) {
        if ( lc->rate[ i ] > 1.0 ) {
            bgtime[ j ] = lc->time[ i ] - lc->time[ ib1start ];
            bgrate[ j ] = lc->rate[ i ];
            bgerr[ j ]  = lc->error[ i ];
            j++;
        }
    }
    for ( i = ib2start; i <= ib2stop; i++ ) {
        if ( lc->rate[ i ] > 1.0 ) {
            bgtime[ j ] = lc->time[ i ] - lc->time[ ib1start ];
            bgrate[ j ] = lc->rate[ i ];
            bgerr[ j ]  = lc->error[ i ];
            j++;
        }
    }

    /*
    ** get a polynomial fit to the background
    */
    hxdBstJudge_Trigger_polyfit( bgtime, bgrate, bgerr,
                                 intrig->bgcoeffs, nbkg, bgorder );

    /*
    ** calculate foreground counts and
    ** expected background value in foreground interval
    */
    intrig->fore_cnts = 0.0;
    intrig->back_cnts = 0.0;
    for ( i = ifstart; i <= ifstop; i++ ) {
        ddummy3 = lc->timedel * lc->fracexp[ i ] * lc->deadc[ i ];
        fore_tot_time += ddummy3;
        intrig->fore_cnts += lc->rate[ i ] * ddummy3;
        ddummy1 = 0.0;
        for ( j = 0; j <= bgorder; j++ ) {
            ddummy2 = 1.0;
            for ( k = 0; k < j; k++ ) {
                ddummy2 *= lc->time[ i ] - lc->time[ ib1start ];
            }
            ddummy1 += intrig->bgcoeffs[ j ] * ddummy2;
        }
        intrig->back_cnts += ddummy1 * ddummy3;
    }

    /* calculate rates */
    intrig->fore_rate    = intrig->fore_cnts / fore_tot_time;
    intrig->back_rate    = intrig->back_cnts / fore_tot_time;
    intrig->net_rate     = intrig->fore_rate - intrig->back_rate;
    intrig->net_cnts     = intrig->fore_cnts - intrig->back_cnts;
    intrig->back_rateerr = sqrt( intrig->back_cnts ) / fore_tot_time;
    intrig->fore_rateerr = sqrt( intrig->fore_cnts ) / fore_tot_time;
    intrig->net_rateerr  = sqrt( intrig->back_rateerr * intrig->back_rateerr +
                                 intrig->fore_rateerr * intrig->fore_rateerr );

    /*
    ** calculate sigma using counts
    */
    if ( intrig->back_cnts > 0.0 && intrig->back_cnts + intrig->fore_cnts > 0.0 ) {
        intrig->sigma = ( intrig->net_cnts ) /
                        sqrt( intrig->back_cnts + intrig->fore_cnts );
    } else {
        intrig->sigma = 0.0;
    }
    
    /*
    ** free memory
    */
    if ( bgtime ) { free( bgtime ); }
    if ( bgrate ) { free( bgrate ); }
    if ( bgerr )  { free( bgerr );  }
}

/*
** hxdBstJudge_Trigger_polyfit_func -
** 
**      polynomial fit function for SV decomp
*/
void hxdBstJudge_Trigger_polyfit_func( double x, double* afunc, unsigned int m ) {
    unsigned int i;
    afunc[ 0 ] = 1.0;
    for ( i = 1; i < m; i++ ) {
        afunc[ i ] = afunc[ i - 1 ] * x;
    }
}

/*
** hxdBstJudge_Trigger_polyfit -
**
**      fits a degree=degree polynomial to x/y/yerr
*/
void hxdBstJudge_Trigger_polyfit( double* x, double* y, double* yerr,
                                  double* a, int n, int degree ) {
    int i;
    int ma;

    double   wt    = 0.0;
    double   chisq = 0.0;
    double*  w = ( double* ) 0;
    double** u = ( double** ) 0;
    double** v = ( double** ) 0;

    /*
    ** depending on the polynomial degree we want, use different methods
    */

    /*
    ** 0th order poly - just get mean
    */
    if ( degree < 1 ) {
        a[ 0 ] = 0.0;
        for ( i = 0; i < n; i++ ) {
            /*if ( yerr[ i ] != 0.0 ) {
                wt = 1.0 / yerr[ i ] / yerr[ i ];
                a[ 0 ] += y[ i ] * wt;
                chisq  += wt;
            }*/
            a[ 0 ] += y[ i ];
            chisq  += 1.0;
        }
        if ( chisq != 0.0 ) {
            a[ 0 ] /= chisq;
        }

    /*
    ** linear least squares
    */
    } else if ( degree < 2 ) {

        hxdBstJudge_Trigger_linleast( x, y, yerr, a, &chisq, n );

    /*
    ** higher orders - use svd decomp to do least-squares
    */
    } else {

        ma = degree + 1;

        /*
        ** allocate memory
        */
        w = ( double* ) malloc( n * sizeof( double ) );
        u = ( double** ) malloc( n * sizeof( double* ) );
        v = ( double** ) malloc( n * sizeof( double* ) );
        for ( i = 0; i < n; i++ ) {
            u[ i ] = ( double* ) malloc( n * sizeof( double ) );
            v[ i ] = ( double* ) malloc( n * sizeof( double ) );
        }

        /*
        ** do the fit
        */
        HDsvdfit( x, y, yerr, n, a, ma, u, v, w,
                  n, n, &chisq, hxdBstJudge_Trigger_polyfit_func );
        /*
        ** free memory
        */
        for ( i = 0; i < n; i++ ) {
            if ( u[ i ] ) { free( u[ i ] ); }
            if ( v[ i ] ) { free( v[ i ] ); }
        }
        if ( u ) { free( u ); }
        if ( v ) { free( v ); }
        if ( w ) { free( w ); }
    }
}

/*
** hxdBstJudge_Trigger_linleast -
**
**      does linear least squares fit
*/
void hxdBstJudge_Trigger_linleast( double* x, double* y, double* yerr,
                                   double* a, double* chisq, int n ) {
    int i;

    double wt, sx, sy, ss, s2, so;

    ss = sx = sy = s2 = 0.0;
    for ( i = 0; i < n; i++ ) {
        if ( yerr[ i ] > 0.0 ) {
            wt = 1.0 / ( yerr[ i ] * yerr[ i ] );
            ss += wt;
            sx += x[ i ] * wt;
            sy += y[ i ] * wt;
        }
    }
    so = ss == 0.0 ? 0.0 : sx / ss;
    for ( i = 0; i < n; i++ ) {
        if ( yerr[ i ] > 0.0 ) {
            wt = ( x[ i ] - so ) / yerr[ i ];
            s2 += wt * wt;
            a[ 1 ] += wt * y[ i ] / yerr[ i ];
        }
    }
    a[ 1 ] = s2 == 0.0 ? 0.0 : a[ 1 ] / s2;
    a[ 0 ] = ss == 0.0 ? 0.0 : ( sy - sx * a[ 1 ] ) / ss;

    *chisq = 0.0;
    for ( i = 0; i < n; i++ ) {
        if ( yerr[ i ] > 0.0 ) {
            so = ( y[ i ] - a[ 0 ] - a[ 1 ] * x[ i ] ) / yerr[ i ];
            *chisq += so * so;
        }
    }
}

/*
** hxdBstJudge_Trigger_icrosstime -
*/
int
hxdBstJudge_Trigger_icrosstime( double *cumsum, double cummin, double maxcts,
                                double thresh, int istart, int istop,
                                int durerrmeth, double rms, int *ifirst0,
                                int *ilast0 ) {
    int i, j;
    int ifirst, ilast, nstep;
    int outside, idelt = 1;
    double deffsign, deffsign0 = 1.0;

    if ( istop > istart ) {
        /* From low -> high */
        idelt = 1;
        deffsign0 = +1.0;
        nstep = istop - istart + 1;
    } else {
        /* From high -> low */
        idelt = -1;
        deffsign0 = -1.0;
        nstep = istart - istop + 1;
    }

    deffsign = deffsign0;

    ifirst = -1; ilast = -1; 
    outside = 1;
    deffsign = deffsign0;
    for ( i = istart, j = 0; j < nstep; j++, i += idelt ) {
        double eff = ( cumsum[ i ] - cummin ) / ( maxcts - cummin );
        double deff;
        int pass;

        if ( eff < 0 ) eff = 0;
        if ( eff > 1 ) eff = 1;
        if ( durerrmeth == 0 ) {
            deff = 0;
        } else if ( durerrmeth == 1 ) {
            /* Total variance */
            deff = deffsign * rms;
        } else {
            /* Fractional variance */
            deff = deffsign * rms * sqrt( eff * ( 1.0 - eff ) );
        }

        /* Threshold passing criteria */
        if ( idelt == 1 ) {
            pass = ( eff + deff ) >= thresh;  /* Passing low -> high */
        } else {
            pass = ( eff + deff ) <= thresh;  /* Passing high -> low */
        }

        if ( outside && pass ) {
            outside = 0;
            ilast = i;
            if ( ifirst == -1 ) ifirst = i;
            deffsign = -deffsign0;
        } else if ( !outside && !pass ) {
            outside = 1;
        }
    }

    *ifirst0 = ifirst; *ilast0 = ilast;
    return 0;
}

/*
** hxdBstJudge_Trigger_burstdur -
**
**      Compute burst duration measures - the method is to scan the
**      "cumulative" counts array, starting from the left and right hand side
**      until X% of the counts have been scanned over.  The algorithm accounts
**      for the possibility that the threshold may be crossed several times,
**      and uses an estimate of the light curve uncertainties to establish a
**      threshold "band" rather than a hard threshold number.
**
**      This code is adapted from the Swift task battblocks. Thanks to
**      Craig M. for the original code.
*/
int
hxdBstJudge_Trigger_burstdur( hxdBstJudge_Trigger* trig,
                              hxdBstJudge_LC* lc ) {
    int ntxx = 2;
    double  txxlist[ 2 ] = { 50.0, 90.0 };
    double* txx_uncertainty[ 2 ];
    double* txxdur[ 2 ];
    double* txxtstart[ 2 ];
    double* txxtstop[ 2 ];

    double maxcts, cummin;
    int istart, istop;

    /* No confidence bands */
    int ilow0first = 0, ilow0last = 0;  /* First and last crossings low TXX */
    int ihigh0first = 0, ihigh0last = 0;/* First and last crossings high TXX */

    /* With confidence bands */
    int ilowfirst = 0, ilowlast = 0;    /* First and last crossings low TXX */
    int ihighfirst = 0, ihighlast = 0;  /* First and last crossings high TXX */

    int i, j;

    long jstart;
    long jstop;
    long nrows;

    double dum;
    double rms;
    double dtr;
    double* cumsum;
    double* cumerr;

    /* setup pointers to t50/t90 values for looping */
    txx_uncertainty[ 0 ] = &( trig->t50err );
    txxdur[ 0 ]          = &( trig->t50 );
    txxtstart[ 0 ]       = &( trig->t50start );
    txxtstop[ 0 ]        = &( trig->t50stop );
    txx_uncertainty[ 1 ] = &( trig->t90err );
    txxdur[ 1 ]          = &( trig->t90 );
    txxtstart[ 1 ]       = &( trig->t90start );
    txxtstop[ 1 ]        = &( trig->t90stop );

    /* calculate the foreground indices into the light curve */
    istart  = trig->ifstart - 1;
    istop   = trig->ifstart + ( long )( ( trig->fstop - trig->fstart ) /
                                        lc->timedel + 0.5 ) + 1;
    istop   = istop < istart ? istart : istop;
    istop   = istop >= lc->npts ? lc->npts - 1 : istop;

    /* quick sanity check */
    if ( istop  < 0 || istop  >= lc->npts ||
         istart < 0 || istart >= lc->npts ) {
        return 0;
    }

    nrows  = istop - istart + 1;
    jstart = 0;
    jstop  = nrows - 1;

    /* get the accumulated counts/error arrays and the gaussian rms */
    cumsum = ( double* ) calloc( nrows, sizeof( double ) );
    cumerr = ( double* ) calloc( nrows, sizeof( double ) );
    if ( !cumsum || !cumerr ) {
        fprintf( stderr, "Failed to allocate memory!\n" );
        return -1;
    }

    dum = lc->timedel * lc->fracexp[ istart ] * lc->deadc[ istart ];
    cumsum[ 0 ] = lc->rate[ istart ] * dum;
    cumerr[ 0 ] = lc->error[ istart ] * lc->error[ istart ] * dum * dum;
    for ( i = istart + 1; i <= istop; i++ ) {
        j = i - istart;
        dum = lc->timedel * lc->fracexp[ i ] * lc->deadc[ i ];
        cumsum[ j ] = cumsum[ j - 1 ] + lc->rate[ i ] * dum;
        cumerr[ j ] = cumerr[ j - 1 ] + lc->error[ i ] * dum *
                                        lc->error[ i ] * dum;
    }
    rms = cumerr[ jstop ] - cumerr[ jstart ];
    rms = sqrt( rms ) / ( cumsum[ jstop ] - cumsum[ jstart ] );

    /*
    ** Get total counts and min counts
    ** NOTE: it is assumed that the first bin is a background bin,
    ** and the last bin is a background bin.
    */
    maxcts = cumsum[ jstop ];
    cummin = cumsum[ jstart ];

    /* Search for these percentage points */
    for ( j = 0; j < ntxx; j++ ) {

        double txx = txxlist[j];          /* Percent of flux to search for */
        double txxstart = ( 100.0 - txx ) / 2.0; /* Start percentage point */
        double txxstop  = txx + txxstart;        /* Stop  percentage point */
        double tlowavg = 0, thighavg = 0;
        double lowdiff, highdiff;

        /*
        ** Look for 5% crossing point; record the first and last crossings
        ** Here "first" means first when approaching from the outside, so
        ** "last" is the innermost crossing.
        */
        txxstart /= 100.0;
        txxstop  /= 100.0;

        /* Without any error band, TXX alone */
        hxdBstJudge_Trigger_icrosstime( cumsum, cummin, maxcts, txxstart,
                                        jstart, jstop, 0, rms, &ilow0first,
                                        &ilow0last );

        /* With error band to determine confidence limits */
        hxdBstJudge_Trigger_icrosstime( cumsum, cummin, maxcts, txxstart,
                                        jstart, jstop, 1, rms, &ilowfirst,
                                        &ilowlast );

        /*
        ** Same for the 95% crossing points, but this time we approach
        ** from the outside on the right.  "Last" is still the innermost
        ** crossing.
        */
        hxdBstJudge_Trigger_icrosstime( cumsum, cummin, maxcts, txxstop,
                                        jstop, 0, 0, rms, &ihigh0first,
                                        &ihigh0last );
        hxdBstJudge_Trigger_icrosstime( cumsum, cummin, maxcts, txxstop,
                                        jstop, 0, 1, rms, &ihighfirst,
                                        &ihighlast );

        /* convert into light curve indices */
        ilow0first  += istart;
        ilow0last   += istart;
        ilowfirst   += istart;
        ilowlast    += istart;
        ihigh0first += istart;
        ihigh0last  += istart;
        ihighfirst  += istart;
        ihighlast   += istart;

        /* TXX start/stop times, based on the runs with no confidence bands */
        tlowavg  = 0.5 * ( lc->time[ ilow0last ] + lc->time[ ilow0first ] );
        thighavg = 0.5 * ( lc->time[ ihigh0last ] + lc->time[ ihigh0first ] );

        /*
        ** If things are really screwed up, then high will be less than
        ** low. Bad. This bit of code will rever to the "outer" bins
        ** only, which should be robustly different.
        */
        if ( tlowavg >= thighavg ) {
            tlowavg = lc->time[ ilow0first ];
            thighavg = lc->time[ ihigh0first ];
        }

        /*
        ** Compute the uncertainty based on the times only, assuming no
        ** bin-width information
        */
        lowdiff = tlowavg - lc->time[ ilowlast ];
        if ( fabs( tlowavg - lc->time[ ilowfirst ] ) > fabs( lowdiff ) ) {
            lowdiff = tlowavg - lc->time[ ilowfirst ];
        }
        highdiff = thighavg - lc->time[ ihighlast ];
        if ( fabs( thighavg - lc->time[ ihighfirst ] ) > fabs( highdiff ) ) {
            highdiff = thighavg - lc->time[ ihighfirst ];
        }

        *( txx_uncertainty[ j ] ) = sqrt( lowdiff * lowdiff +
                                          highdiff * highdiff );

        /* Scan around the T5/T95 times to find the corresponding bin.
        ** (We took the average above, so tlowavg and thighavg no longer
        ** correspond to a specific bin.)
        */
        dtr = 0.0;

        /* Outward in */
        for ( i = ilowfirst; i <= ilowlast; i++ ) {
            if ( tlowavg >= ( lc->time[ i ] - lc->timedel / 2.0 ) &&
                 tlowavg < ( lc->time[ i ] + lc->timedel / 2.0 ) ) {
                break;
            }
        }

        /* Never the first or last bin, which are background */
        if ( i == istart ) {
            i++;
        }
        if ( i == istop ) {
            i--;
        }
        *( txxtstart[ j ] ) = lc->time[ i ] - lc->timedel / 2.0;
        dtr += ( lc->timedel * lc->timedel ) / 4.0;

        /* Outward in */
        for ( i = ihighfirst; i >= ihighlast; i-- ) {
            if ( thighavg >= ( lc->time[ i ] - lc->timedel / 2.0 ) &&
                 thighavg < ( lc->time[ i ] + lc->timedel / 2.0 ) ) {
                break;
            }
        }

        /* Never the first or last bin, which are background */
        if ( i == istart ) {
            i++;
        }
        if ( i == istop ) {
            i--;
        }
        *( txxtstop[ j ] )  = lc->time[ i ] + lc->timedel / 2.0;
        dtr += ( lc->timedel*lc->timedel ) / 4.0;

        dtr = sqrt( 2.0 * dtr );
        if ( *( txx_uncertainty[ j ] ) < dtr ) {
            *( txx_uncertainty[ j ] ) = dtr;
        }

        /* Fall-back uncertainty */
        if ( *( txx_uncertainty[ j ] ) == 0 ) {
            dtr = 0.0;
            dtr += ( lc->time[ ilowlast ] - lc->time[ ilowlast - 1 ] ) *
                   ( lc->time[ ilowlast ] - lc->time[ ilowlast - 1 ] ) / 4.0;
            dtr += ( lc->time[ ihighlast ] - lc->time[ ihighlast - 1 ] ) *
                   ( lc->time[ ihighlast ] - lc->time[ ihighlast - 1 ] ) / 4.0;

            *( txx_uncertainty[ j ] ) = sqrt( 2.0 * dtr );
        }
        *( txxdur[ j ] ) = *( txxtstop[ j ] ) - *( txxtstart[ j ] );
    }
    return 0;
}

/*
** hxdBstJudge_Step_calc -
** 
**      calculates a step
*/
hxdBstJudge_Trigger*
hxdBstJudge_Step_calc( hxdBstJudge_LC* lc, long istep, double winsz, double sigma,
                       double maxchi, double width_frac, double delchi, int* status ) {

    int fitstat;
    int fititer;

    long i, jstep, npts, niter, maxiter, tries, maxtries, dof_lin, dof_step;

    double pinit[ 5 ], pbest[ 5 ];
    double height, heightlo, heighthi;
    double parstep  = 0.05;
    double chisqlin = 0.0;
    double chisqstep, chisqstep2;
    double fstat, fprob, sigprob, snr;

    mp_result fitresults, fitresults2;

    mp_par constraints[ 5 ];

    hxdBstJudge_LC lcsub;

    hxdBstJudge_Trigger* outstep = ( hxdBstJudge_Trigger* )0;


    maxiter  = 500;
    maxtries = 10;

    if ( width_frac <= 0 || width_frac >= 1 ) {
        *status = ANL_NG;
        return outstep;
    }



    /* setup the data to fit */
    lcsub.time    = &( lc->time[ istep ] );
    lcsub.rate    = &( lc->rate[ istep ] );
    lcsub.error   = &( lc->error[ istep ] );
    lcsub.fracexp = ( double* )0;
    lcsub.deadc   = ( double* )0;

    jstep = istep;
    while ( jstep < lc->npts && lc->time[ jstep ] - lc->time[ istep ] < winsz ) {
        jstep++;
    }
    if ( lc->time[ jstep ] - lc->time[ istep ] < winsz ) {
        return outstep;
    }
    jstep--;
    npts = jstep - istep + 1;
    if ( npts < 10 ) {
        return outstep;
    }

    /* do a linear fit first */
    hxdBstJudge_Trigger_linleast( lcsub.time, lcsub.rate, lcsub.error,
                                  &( pinit[ 0 ] ), &chisqlin, npts );




    dof_lin = npts - 2;

    /* initial guesses for parameters and parameter constraints */
    memset( &( constraints[ 0 ] ), 0, sizeof( constraints ) );

    /* constant term */
    pinit[ 0 ] = lc->rate[ istep ];

    /* linear term */
    pinit[ 1 ] = ( lc->rate[ jstep ] - lc->rate[ istep ] ) /
                 ( lc->time[ jstep ] - lc->time[ istep ] );
    constraints[ 1 ].limited[ 0 ] = 1;
    constraints[ 1 ].limited[ 1 ] = 1;
    constraints[ 1 ].limits[ 0 ]  = -30;
    constraints[ 1 ].limits[ 1 ]  = 30;

    /* occultation time */
    pinit[ 2 ] = lc->time[ istep ] + winsz / 2.0;
    constraints[ 2 ].limited[ 0 ] = 1;
    constraints[ 2 ].limited[ 1 ] = 1;
    constraints[ 2 ].limits[ 0 ]  = lc->time[ istep ];
    constraints[ 2 ].limits[ 1 ]  = lc->time[ jstep ];

    /* "height" of step */
    pinit[ 3 ] = 10.0;
    constraints[ 3 ].limited[ 0 ] = 1;
    constraints[ 3 ].limited[ 1 ] = 1;
    constraints[ 3 ].limits[ 0 ]  = -500;
    constraints[ 3 ].limits[ 1 ]  = 500;

    /* "width" of step */
    pinit[ 4 ] = 2 * atanh( width_frac ) / 10.0;
    constraints[ 4 ].limited[ 0 ] = 1;
    constraints[ 4 ].limited[ 1 ] = 1;
    /* ~ 120 seconds max */ 
    constraints[ 4 ].limits[ 0 ]  = 2 * atanh( width_frac ) / 120.0;
    /* ~ 1 second minimum */
    constraints[ 4 ].limits[ 1 ]  = 2 * atanh( width_frac );





    /* do the fit */
    memset( &fitresults, 0, sizeof( fitresults ) );

    fititer = 0;
    fitresults.niter = 100;
    while(fititer < 25 && fitresults.niter > 1)
      {
	fitstat = mpfit( hxdBstJudge_Step_fit_func, npts, 5, &( pinit[ 0 ] ),
			 &( constraints[ 0 ] ), ( mp_config* ) 0, ( void* ) &lcsub,
			 &fitresults );
	fititer++;
      }

    if ( fitstat < 1 ) {
        return outstep;
    }



    /* fit sanity checks */
    /* check the chisq vs linear fit*/
    /* if better do an f-test, otherwise return */
    dof_step = npts - fitresults.nfree;
    chisqstep = fitresults.bestnorm;
    if ( pinit[ 3 ] == 0.0 || pinit[ 4 ] == 0.0 ||
         pinit[ 2 ] - lc->time[ istep ] < 0.1 * winsz ||
         lc->time[ jstep ] - pinit[ 2 ] < 0.1 * winsz ||
         chisqstep / dof_step > maxchi ||
         chisqstep >= chisqlin ) {
        return outstep;
    }

    /* save best fit */
    for ( i = 0; i < 5; i++ ) { pbest[ i ] = pinit[ i ]; }
    memcpy( &fitresults2, &fitresults, sizeof( fitresults ) );

    /* fit again using "down" step for initial condition */
    pinit[ 0 ] = lc->rate[ istep ];
    pinit[ 1 ] = ( lc->rate[ jstep ] - lc->rate[ istep ] ) /
                 ( lc->time[ jstep ] - lc->time[ istep ] );
    pinit[ 2 ] = lc->time[ istep ] + winsz / 2.0;
    pinit[ 3 ] = -10.0;
    pinit[ 4 ] = 2 * atanh( width_frac ) / 10.0;
    memset( &fitresults, 0, sizeof( fitresults ) );

    fititer = 0;
    fitresults.niter = 100;
    while(fititer < 25 && fitresults.niter > 1)
      {
	fitstat = mpfit( hxdBstJudge_Step_fit_func, npts, 5, &( pinit[ 0 ] ),
			 &( constraints[ 0 ] ), ( mp_config* ) 0, ( void* ) &lcsub,
			 &fitresults );
	fititer++;
      }


    if ( fitstat < 1 ) {
        return outstep;    }
    chisqstep2 = fitresults.bestnorm;
    if ( chisqstep2 < chisqstep ) {
        /*printf( "negative fit better (%f vs %f)\n",chisqstep2, chisqstep );*/
        for ( i = 0; i < 5; i++ ) { pbest[ i ] = pinit[ i ]; }
        chisqstep = chisqstep2;
    } else {
        memcpy( &fitresults, &fitresults2, sizeof( fitresults ) );
        for ( i = 0; i < 5; i++ ) { pinit[ i ] = pbest[ i ]; }
        /*printf( "positive fit better (%f vs %f)\n", chisqstep, chisqstep2 );*/
    }

    /* do f-test (not quite correct, but...) */
    hxdBstJudge_Step_ftest( chisqlin, dof_lin, chisqstep, 
                            dof_step, &fstat, &fprob );
    if ( fprob == -1.0 * HUGE_VAL ) {
        return outstep;
    }
    sigprob = erfc( sigma / sqrt( 2.0 ) );
    if ( sigprob < fprob ) {
        return outstep;
    }
    /*printf( "step better than line @ T = %1.15E\n", pinit[ 2 ] );*/

    /* determine confidence interval */
    tries = 0;
    while ( tries < maxtries ) {
        tries++;
        height = pinit[ 3 ];
        constraints[ 3 ].fixed = 1;
        niter = 0;
        chisqstep = fitresults.bestnorm;
        while ( niter < maxiter &&
                fitresults.bestnorm < chisqstep + delchi &&
                fitresults.bestnorm > chisqstep - delchi ) {
            niter++;
            pinit[ 3 ] += parstep;
            memset( &fitresults, 0, sizeof( fitresults ) );

	    fititer = 0;
	    fitresults.niter = 100;
	    while(fititer < 25 && fitresults.niter > 1)
	      {
		fitstat = mpfit( hxdBstJudge_Step_fit_func, npts, 5, &( pinit[ 0 ] ),
                             &( constraints[ 0 ] ), ( mp_config* ) 0, ( void* ) &lcsub,
                             &fitresults );
	      }
            if ( fitstat < 1 ) {
                continue;
            }

            /* check for new best fit */
            if ( fitresults.bestnorm / ( dof_step - 1 ) < chisqstep / dof_step ) {
                break;
            }
        }
        /* new best found */
        if ( fitresults.bestnorm / ( dof_step - 1 ) < chisqstep / dof_step ) {
            chisqstep = fitresults.bestnorm;
            for ( i = 0; i < 5; i++ ) { pbest[ i ] = pinit[ i ]; }
            continue;
        }
        if ( niter == maxiter ) {
            return outstep;
        }
        heighthi = pinit[ 3 ];

        for ( i = 0; i < 5; i++ ) { pinit[ i ] = pbest[ i ]; }
        while ( niter < maxiter &&
                fitresults.bestnorm < chisqstep + delchi &&
                fitresults.bestnorm > chisqstep - delchi ) {
            niter++;
            pinit[ 3 ] -= parstep;
            memset( &fitresults, 0, sizeof( fitresults ) );

	    fititer = 0;
	    fitresults.niter = 100;
	    while(fititer < 25 && fitresults.niter > 1)
	      {
		fitstat = mpfit( hxdBstJudge_Step_fit_func, npts, 5, &( pinit[ 0 ] ),
				 &( constraints[ 0 ] ), ( mp_config* ) 0, ( void* ) &lcsub,
				 &fitresults );
	      }
            if ( fitstat < 1 ) {
                continue;
            }

            /* check for new best fit */
            if ( fitresults.bestnorm / ( dof_step - 1 ) < chisqstep / dof_step ) {
                break;
            }
        }
        /* new best found */
        if ( fitresults.bestnorm / ( dof_step - 1 ) < chisqstep / dof_step ) {
            chisqstep = fitresults.bestnorm;
            for ( i = 0; i < 5; i++ ) { pbest[ i ] = pinit[ i ]; }
            continue;
        }
        if ( niter == maxiter ) {
            return outstep;
        }
        heightlo = pinit[ 3 ];

        /* if we made it here, we're done */
        break;
    }
    if ( tries == maxtries ) {
        return outstep;
    }

    /*
    ** Calculate the signal to noise of the height parameter as the height over
    ** the confidence interval. This is a funny definition, especially since
    ** the caller has control over the confidence interval.
    ** Create output "trigger" if good enough.
    */
    snr = fabs( height / ( heighthi - heightlo ) );
    if ( snr >= sigma ) { 
        outstep = hxdBstJudge_Trigger_new( 0, 0, 0, 0, 0, 0, 0, 5 );
        if ( !outstep ) {
            *status = ANL_NG;
            return outstep;
        }
        /* start and stop of step - user controlled fraction of step */
        outstep->fstart  = pinit[ 2 ] - atanh( width_frac ) / pinit[ 4 ];
        outstep->fstop   = pinit[ 2 ] + atanh( width_frac ) / pinit[ 4 ];

        /* background start and stop */
        outstep->b1start = lc->time[ istep ];
        outstep->b1stop  = outstep->fstart;
        outstep->b2start = outstep->fstop;
        outstep->b2stop  = lc->time[ jstep ];

        /* step fit params */
        for ( i = 0; i < 5; i++ ) {
            outstep->bgcoeffs[ i ] = pbest[ i ];
        }

        outstep->ifstart = istep;
        while ( lc->time[ outstep->ifstart ] < pinit[ 2 ] ) {
            outstep->ifstart++;
        }
        outstep->sigma = snr;
    }


    return outstep;
}

/*
** hxdBstJudge_Step_fit_func -
** 
**      step fit function for mpfit fit, basically constant plus linear
**      plus hyperbolic tangent.
*/
int hxdBstJudge_Step_fit_func( int m, int n, double *p, double *deviates,
                               double **derivs, void *private ) {
    int i;
    double modval;

    hxdBstJudge_LC* lc;

    lc = ( hxdBstJudge_LC* ) private;

    for ( i = 0; i < m; i++ ) {
      modval = hxdBstJudge_Step_stepfunc( lc->time[ i ], p );
        if ( lc->error[ i ] != 0 ) {
            deviates[ i ] = ( lc->rate[ i ] - modval ) / lc->error[ i ];
        } else {
            return -1;
        }
    }
    return 0;
}

/*
** hxdBstJudge_Step_stepfunc -
** 
**      returns a step function that is a constant + linear + hyperboloc tangent
**
*/
double hxdBstJudge_Step_stepfunc( double x, double* p ) {
    return ( p[ 0 ] + p[ 1 ] * ( x - p[ 2 ] ) +
             p[ 3 ] * tanh( ( x - p[ 2 ] ) * p[ 4 ] ) );
}

/*
** hxdBstJudge_Step_ftest -
**
**      computes f-statistic and related probability given an old
**      chi-squared value and its related dof, and a new chi-squared
**      and related dof.
*/
void hxdBstJudge_Step_ftest( double chisqold, int dof_old, double chisqnew, 
                             int dof_new, double* fstat, double* fprob ) {
    double d1, d2, d3;
    d1 = chisqold - chisqnew;
    d2 = ( double )( dof_old - dof_new );
    d3 = chisqnew / dof_new;
    *fstat = d1 / d2 / d3;

    d1 = dof_new / 2.0;
    d2 = ( dof_old - dof_new ) / 2.0;
    d3 = dof_new / ( dof_new + ( dof_old - dof_new ) * *fstat );
    *fprob = hxdBstJudge_Step_incbeta( d1, d2, d3 );
}

/*
** hxdBstJudge_Step_incbeta -
**
**      Incomplete beta function I_x(a, b). based on NR routine.
*/
double hxdBstJudge_Step_incbeta( double a, double b, double x ) {
    double incbeta, bt;
    bt = incbeta = 0.0;
    if ( x < 0 || x > 1 ) {
        return -1.0 * HUGE_VAL;
    }
    if ( x == 0.0 || x == 1.0 ) {
        bt = 0.0;
    } else {
        bt = exp( lgamma( a + b ) - lgamma( a ) - lgamma( b ) +
                  a * log( x ) + b * log( 1.0 - x ) );
    }
    if ( x < ( a + 1.0 ) / ( a + b + 2.0 ) ) {
        incbeta = hxdBstJudge_Step_cfbeta( a, b, x );
        if ( incbeta == -1.0 * HUGE_VAL ) {
            return incbeta;
        }
        incbeta *= bt / a;
    } else {
        incbeta = hxdBstJudge_Step_cfbeta( b, a, 1.0 - x );
        if ( incbeta == -1.0 * HUGE_VAL ) {
            return incbeta;
        }
        incbeta = 1.0 - bt * incbeta / b;
    }
    return incbeta;
}

/*
** hxdBstJudge_Step_cfbeta -
**
**      Incomplete Beta function, computed using continuous fraction.
**      Based on NR routine.
*/
double hxdBstJudge_Step_cfbeta( double a, double b, double x ) {
    int itmax = 200;
    int m;
    double eps, am, bm, az, qab, qap, qam, bz, em, ap, bp, d, app, aold, tem, bpp;
    eps = 3.0e-7;
    am = bm = az = 1.0;
    qab = a + b;
    qap = a + 1.0;
    qam = a - 1.0;
    bz  = 1.0 - qab * x / qap;
    for( m = 1; m <= itmax; m++ ) {
        em   = m;
        tem  = em + em;
        d    = em * ( b - m ) * x / ( ( qam + tem ) * ( a + tem ) );
        ap   = az + d * am;
        bp   = bz + d * bm;
        d    = -( a + em ) * ( qab + em ) * x / ( ( a + tem ) * ( qap + tem ) );
        app  = ap + d * az;
        bpp  = bp + d * bz;
        aold = az;
        am   = ap / bpp;
        bm   = bp / bpp;
        az   = app / bpp;
        bz   = 1.0;
        if ( fabs( az - aold ) < eps *fabs( az ) ) {
            return az;
        }
    }
    return ( -1.0 * HUGE_VAL );
}


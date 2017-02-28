/// \file sgdevtidlib.cxx
/// \brief functions to act on the sequence probability file for SGD
/// \author Robert S. Hill
/// \date $Date: 2016/12/05 19:49:37 $

#define AHLABEL tool_sgdevtid_sgdevtidlib
#define AHCVSID "$Id: sgdevtidlib.cxx,v 1.56 2016/12/05 19:49:37 rshill Exp $"

#include "hxisgdevtid/fluor.h"
#include "sgdevtidlib.h"
#include "ahgen/ahgen.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <algorithm>
#include <sstream>
#include <set>
#include <iomanip>

namespace sgdevtidlib {

/** \addtogroup tool_sgdevtid
 *  @{
 */

void reInitSignals(RowSignals& signals) {
  signals.m_nsignal = 0;
  for (ahfits::IndexType i=0; i<MAX_NUMSIGNAL; ++i) {
    signals.m_readout_id_rmap[i] = 0;
    signals.m_rawx[i] = 0;
    signals.m_rawy[i] = 0;
    signals.m_sensor_id[i] = 0;
    signals.m_camerax[i] = 0.0;
    signals.m_cameray[i] = 0.0;
    signals.m_cameraz[i] = 0.0;
    signals.m_epi[i] = 0.0;
    signals.m_layer[i] = 0;
    signals.m_layer_type[i] = -1;
    signals.m_flag[i] = false;

    signals.m_cluster[i] = 0;

    signals.m_sigarray_0[i] = 0;
    signals.m_sigarray_1_0[i] = 0;
    signals.m_sigarray_1a_1a[i] = 0;
    signals.m_sigarray_1a_1b[i] = 0;
    signals.m_sigarray_1a_2[i] = 0;
    signals.m_sigarray_1a_3[i] = 0;

    signals.m_epi_current[i] = 0.0;
    signals.m_var_current[i] = 0.0;

    signals.m_epi_merge_1_0[i] = 0.0;
    signals.m_epi_merge_1a_1a[i] = 0.0;
    signals.m_epi_merge_1a_1b[i] = 0.0;
    signals.m_epi_merge_1a_2[i] = 0.0;
    signals.m_epi_merge_1a_3[i] = 0.0;

    signals.m_var_merge_1_0[i] = 0.0;
    signals.m_var_merge_1a_1a[i] = 0.0;
    signals.m_var_merge_1a_1b[i] = 0.0;
    signals.m_var_merge_1a_2[i] = 0.0;
    signals.m_var_merge_1a_3[i] = 0.0;

    signals.m_merge_survivor[i] = false;
  }
}

// ****************************************************************************

void reInitHits(Hits& hits) {
  hits.m_m=0;
  hits.m_best_k=-1;
  for (int i=0; i<MAX_NUMHITS; ++i) {
    hits.m_hitarray[i] = -1; 
    hits.m_epiarray[i] = 0.0; 
    hits.m_deltaepiarray[i] = 0.0; 
  }
}

// ****************************************************************************

void partialReInitExtraOutputData(ExtraOutputRowData& extraoutrow) {

  for (ahfits::IndexType ii=0; ii < MAX_PERM*MAX_NUMHITS; ii++) {
    extraoutrow.m_e[ii] = 0.0;
    extraoutrow.m_delta_e[ii] = 0.0;
    extraoutrow.m_f[ii] = 0.0;
    extraoutrow.m_delta_f[ii] = 0.0;
    extraoutrow.m_check_f[ii] = 0;
    extraoutrow.m_cos_theta_g[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_g[ii] = 0.0;
    extraoutrow.m_cos_theta_k[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_k[ii] = 0.0;
    extraoutrow.m_g[ii] = 0.0;
    extraoutrow.m_delta_g[ii] = 0.0;
    extraoutrow.m_check_g[ii] = 0;
  }
  extraoutrow.num_e = 1;
  extraoutrow.num_delta_e = 1;
  extraoutrow.num_f = 1;
  extraoutrow.num_delta_f = 1;
  extraoutrow.num_cos_theta_g = 1;
  extraoutrow.num_delta_cos_theta_g = 1;
  extraoutrow.num_cos_theta_k = 1;
  extraoutrow.num_delta_cos_theta_k = 1;
  extraoutrow.num_g = 1;
  extraoutrow.num_delta_g = 1;
  for (ahfits::IndexType ii=0; ii < MAX_PERM; ii++) {
    extraoutrow.m_test_f[ii] = 0;
    extraoutrow.m_test_g[ii] = 0;
    extraoutrow.m_prob[ii] = 0.0;
    extraoutrow.m_test_prob[ii] = 0;
    extraoutrow.m_fom[ii] = 0.0;
    extraoutrow.m_cos_theta_g_0[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_g_0[ii] = 0.0;
    extraoutrow.m_cos_theta_k_0[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_k_0[ii] = 0.0;
    extraoutrow.m_g_0[ii] = 0.0;
    extraoutrow.m_delta_g_0[ii] = 0.0;
  }
  extraoutrow.num_prob = 1;
  extraoutrow.num_test_prob = 1;
  extraoutrow.num_fom = 1;
  extraoutrow.num_cos_theta_g_0 = 1;
  extraoutrow.num_delta_cos_theta_g_0 = 1;
  extraoutrow.num_cos_theta_k_0 = 1;
  extraoutrow.num_delta_cos_theta_k_0 = 1;
  extraoutrow.num_g_0 = 1;
  extraoutrow.num_delta_g_0 = 1;
}

// ****************************************************************************

void mergeAdjacentSignals (Par & par, hxisgdevtid::fluor::DataType& fluor_table,
  RowSignals& signals,
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete) {
  //  Step 1-0:  merge signals within each layer:  there is a given set of valid
  //  patterns:
  //       xx       xx        xx
  //                 x        xx
  //  and all rotations and reflections.
  //
  //    current_cluster = 0
  //    while at least one signal is unassigned
  //      start current cluster using next unassigned signal
  //      repeat until cluster_has_changed == false
  //        cluster_has_changed = false
  //        loop through signals j in current_cluster
  //          loop through signals k not in current_cluster
  //            is signal k (non-diagonally) adjacent to signal j?
  //               yes:  add to cluster i
  //                     cluster_has_changed = true
  //               no:  no-op
  //          end loop k
  //        end loop j
  //      end repeat
  //      ++current_cluster
  //    end while
  //
  //    loop through clusters i
  //      if cluster i has 5 or more members,
  //        then occurrence is BAD --> go to next occurrence
  //      check configuration of cluster:
  //        if max(CAMERAY) > min(CAMERAY) + d1_0 
  //        or max(CAMERAX) > min(CAMERAX) + d1_0
  //        or max(CAMERAZ) > min(CAMERAZ) + d1_0,
  //          then occurrence is BAD --> go to next occurrence
  //        cluster fits into 2x2 square if this point is reached: GOOD
  //        merge all pixels
  //      note that if cluster i has 2 members, then it is automatically
  //        good by this algorithm, because the diagonally adjacent case
  //        is skipped over and the two pixels are in separate one-pixel
  //        clusters
  //    end loop
  //
  //
  //  NOTE:  Good and bad individual signals are decided by EPI threshold,
  //  which is one value for each readout.

  //  Loop through layers, finding the clusters of edge-adjacent pixels
  //  in each layer.  
  //

  long total_nclust = 0;
  IntVectorIndex row_layers;   // Signals as a function of layer number

  if (reco_complete) {
    AH_THROW_LOGIC("reco_complete is true at start of mergeAdjacentSignals");
  }

  //  Find out what layers are present.
  //
  for (int i=0; i<signals.m_nsignal; ++i) {
    row_layers[signals.m_layer[i]].push_back(i);  // Add signal subscript to list per layer
  }
  
  //  Propagate the previous sigarray to the one for this substep.
  //
  for (int i=0; i<signals.m_nsignal; ++i) {
    signals.m_sigarray_1_0[i] = signals.m_sigarray_0[i];
  }

  //  Iterate through individual layers, and treat all signals in each layer.
  //
  for (IntVectorIndex::iterator it=row_layers.begin(); it!=row_layers.end(); ++it) {

    int iclust=0;         //  Current cluster number
    int nclust=0;         //  Number of clusters
    int next_l=0;         //  Next signal within a layer
    int n_assigned=0;     //  Number of signals in layer currently assigned to a cluster
    int n_badepi=0;       //  Number of signals in layer with EPI below threshold
    int n_goodepi=0;      //  Number of signals in layer with EPI above threshold
    bool cluster_in_progress=false;  //  Current cluster is still being updated
    int csize=0;          //  Number of signals in current cluster

    IntVectorIndex clusters;   // Cluster membership intermediate results

    //  Get the list of signals in this layer 
    //
    std::vector<int>& sig_layer = it->second;
     
    //  Get the number of signals in this layer.
    //
    int nsig_layer = sig_layer.size();

    clusters.clear();     //  Initialize the cluster accounting for this layer

    //  n_goodepi is number of signals available for assignment
    //  to clusters in this layer.
    //
    for (int jl=0; jl<nsig_layer; ++jl) {
      int jr=sig_layer[jl];    // jl: select signal from layer; jr: select signal from row
      if (signals.m_flag[jr]) {
        ++n_goodepi;
      } else {
        ++n_badepi;
        signals.m_sigarray_1_0[jr] = -1;  // Subscripts of bad signals set to null
      }
      signals.m_cluster[jr] = -1;  // Initialize cluster membership.
    }

    AH_DEBUG << "Processing layer " << it->first << " with " << n_goodepi << " good signals" << std::endl;

    //  Find starting signal for first cluster in the layer.
    //
    for (next_l=0; next_l < nsig_layer; ++next_l) {
      int ir=sig_layer[next_l];    // ir: select signal from row
      if (signals.m_flag[ir]) {
        if (signals.m_cluster[ir] < 0) {
          AH_DEBUG << "First signal is " << next_l << " in layer; " << ir << " in row" << std::endl;
          break;
        }
      }
    }

    if (next_l >= nsig_layer) continue;  // All signals in layer bad; next layer.

    //  A test-in-the-middle loop that executes until all of the
    //  good signals in the layer have been assigned to clusters.
    //
    while (true) {
      int ir=sig_layer[next_l];    // next_l: select from layer; ir: select from row
      signals.m_cluster[ir] = iclust;
      (clusters[iclust]).push_back(ir);
      AH_DEBUG << "Number of elements for cluster " << iclust << " = " << clusters[iclust].size() << std::endl;
      ++n_assigned;
      cluster_in_progress=true;
      while (cluster_in_progress) {
        csize = clusters[iclust].size();
        cluster_in_progress = false;  // Changed to true if cluster grows.
        
        //  Loop through signals that are currently in the cluster.
        //
        for (int jc=0; jc<csize; ++jc) {    // jc: select signal from cluster
          int jr=(clusters[iclust])[jc];    // jr: select signal from row
          AH_DEBUG << "Testing other signals for adjacency to signal " << jr << " (" << jc << " within cluster)" << std::endl;
          AH_DEBUG << "readout_id_rmap = " << signals.m_readout_id_rmap[jr] << std::endl;

          //  Loop through all the signals in the layer to find ones to
          //  add to the cluster.
          //
          for (int kl=0; kl<nsig_layer; ++kl) {   // kl: select signal from layer
            int kr=sig_layer[kl];                 // kr: select signal from row
            bool adj = false;
            if (!(signals.m_flag[kr])) continue;  // Bad EPI
            if (signals.m_cluster[kr] == iclust) continue;   // Already assigned to this cluster
            adj = pixelsAdjacent(signals,jr,kr,par.d10);    // Belongs in cluster
            AH_DEBUG << "Adjacency test result " << adj << std::endl;
            if (adj) {
              signals.m_cluster[kr] = iclust;
              (clusters[iclust]).push_back(kr);
              cluster_in_progress = true;
              ++n_assigned;
              AH_DEBUG << "Added signal " << kr << " (" << kl << " within layer)"  << " to cluster " << iclust << std::endl;
              AH_DEBUG << "readout_id_rmap = " << signals.m_readout_id_rmap[kr] << std::endl;
              AH_DEBUG << "Number of elements for cluster " << iclust << " = " << clusters[iclust].size() << std::endl;
            }
          }
        }
      }

      if (n_assigned >= nsig_layer) break;  // Exit if all signals in layer assigned.

      ++iclust;   // Next cluster number

      //  Find starting signal for next cluster in the layer.
      //
      for (next_l=0; next_l < nsig_layer; ++next_l) {
        int ir=sig_layer[next_l];    // ir: select signal from row
        if (signals.m_flag[ir]) {
          if (signals.m_cluster[ir] < 0) {
            AH_DEBUG << "First signal is " << next_l << " in layer; " << ir << " in row" << std::endl;
            break;
          }
        }
      }
      if (next_l >= nsig_layer) break;    // no more signals to check
//        AH_DEBUG << "FAILURE: next_l, nsig_layer: " << next_l << ", " << nsig_layer << std::endl;
//        AH_THROW_LOGIC ("Bug in finding next signal unassigned to a cluster");
//      }
    }  // end of cluster assignment loop
    AH_DEBUG << "End of cluster assignment loop" << std::endl;

    //  At this point, all cluster assignments within the current
    //  layer have been made.

    //  Find the maximum energy in each cluster and merge
    //  the clusters into one pixel apiece.  Also, check whether
    //  cluster pixels are in an allowed configuration
    //  (pixel-by-pixel adjacency is not a sufficient condition).
    //
    nclust = clusters.size();
    AH_DEBUG << "Number of clusters found = " << nclust << std::endl;
    AH_DEBUG << "Number of signals in first cluster = " << (clusters[0]).size() << std::endl;
    total_nclust += nclust;
    for (int i=0; i < nclust; ++i) {
//      double minx=0.0, miny=0.0, minz=0.0;  // Working variables for computing min coords   *** old algorithm
//      double maxx=0.0, maxy=0.0, maxz=0.0;  // Working variables for computing max coords   *** old algorithm
      double max_epi=0.0;                   // Biggest EPI found in cluster
      double tot_epi=0.0, variance=0.0;     // Total EPI and variance of merged energy
//      int max_epi_element=0;                // Subscript of signal with biggest EPI         *** old algorithm

      csize = (clusters[i]).size();         // Number of signals in current cluster

      AH_DEBUG << "Processing cluster " << i << " with " << csize << " elements" << std::endl;

      if (csize > 5) {
        //  Cluster is BAD, therefore occurrence is BAD
        //
        AH_DEBUG << "Finished: cluster with more than 5 signals (RECO_CLUSTER_TOO_MANY_SIGNALS)" << std::endl;
        setRecoStatusBit(outrow, RECO_CLUSTER_TOO_MANY_SIGNALS);
        sgdevtidlib::setOutputEventNull(outrow);
        reco_complete = true;
        break;
      }

      // find largest EPI in cluster; also get summed EPI
      for (int j=0; j < csize; ++j) {
        int ir=clusters[i][j];             // Select signal index from row
        double sig_epi=signals.m_epi[ir];
        if (sig_epi > max_epi) max_epi=sig_epi;
        tot_epi+=sig_epi;
      }
      AH_DEBUG << "Cluster has total EPI of " << tot_epi << std::endl;

      // determine if cluster has valid shape
      //
      // starting with each signal which has the max EPI (there could be more
      // than one), check that all other signals are adjacent (not diagonal).
      // If so, then the shape is valid.  If not, check if cluster shape is
      // a square.
      // Note: this method assumes all signals have unique coordinates
      int shape=e_NOSHAPE;
      int jcluster=-1;                                   // index of central signal in cluster
      for (int j=0; j < csize; ++j) {
        int ir=clusters[i][j];                           // Select signal index from row
        if (signals.m_epi[ir] < max_epi) continue;       // only max signals with largest EPI
        jcluster=j;
        int rawx0=signals.m_rawx[ir];
        int rawy0=signals.m_rawy[ir];

        // check that all other signals are adjacent to selected signal
        int xsum=0;          // sums help to determine shape
        int ysum=0;
        bool okay=true;     // true if valid shape
        for (int jo=0; jo < csize; ++jo) {
          int ir=clusters[i][jo];                          // Select signal index from row
          int rawx=signals.m_rawx[ir];
          int rawy=signals.m_rawy[ir];
          xsum+=rawx;
          ysum+=rawy;
          if (jo == j) continue;    // no need to check selected signal
          int dx=std::abs(rawx-rawx0);
          int dy=std::abs(rawy-rawy0);
          if (dx+dy == 1) continue;                        // point is adjacent
          okay=false;
          break;
        }

        // if okay, determine shape
        if (okay) {
          if (csize == 1)
            shape=e_SINGLE;
          else if (csize == 2)
            shape=e_DOUBLE;
          else if (csize == 4)
            shape=e_TEE;
          else if (csize == 5)
            shape=e_CROSS;
          else {      // size is 3
            if (xsum == 3*rawx0 || ysum == 3*rawy0)
              shape=e_LINE;
            else
              shape=e_ELBOW;
          }
        }

        // if not okay and size=4; check for square shape
        if (!okay && csize == 4) {
          // get smallest rawx/rawy
          int xmin=rawx0;
          int ymin=rawy0;
          for (int jo=0; jo < csize; ++jo) {
            int ir=clusters[i][jo];                          // Select signal index from row
            int rawx=signals.m_rawx[ir];
            int rawy=signals.m_rawy[ir];
            if (rawx < xmin) xmin=rawx;
            if (rawy < ymin) ymin=rawy;
          }

          // check that all signals are with (+1,+1) of xmin,ymax
          okay=true;
          for (int jo=0; jo < csize; ++jo) {
            int ir=clusters[i][jo];                          // Select signal index from row
            int rawx=signals.m_rawx[ir];
            int rawy=signals.m_rawy[ir];
            if (rawx-xmin <= 1 && rawy-ymin <= 1) continue;    // okay
            okay=false;
            break;
          }
          if (okay) shape=e_SQUARE;
        }

        // if have valid shape, no need to continue to check other signals
        if (shape != e_NOSHAPE) break;

      }   // end loop over signals in cluster

      // if shape has 2 signals or is square, there may be more than one valid
      // central signal with the max EPI; if this is the case, randomly choose 
      // the central signal
      if (shape == e_DOUBLE || shape == e_SQUARE) {
        std::vector<int> jvals;
        for (int j=0; j < csize; ++j) {
          int ir=clusters[i][j];             // Select signal index from row
          if (signals.m_epi[ir] == max_epi) jvals.push_back(j);
        }
        int jidx=ahgen::getRandomInt(0,jvals.size()-1);
        jcluster=jvals[jidx];
        if (jvals.size() > 1) {
          extraoutrow.m_rand1_0=1;
          if (shape == e_DOUBLE)
            AH_DEBUG << "Randomly choosing central signal of DOUBLE signal" << std::endl;
          else
            AH_DEBUG << "Randomly choosing central signal of SQUARE signal" << std::endl;
          for (int jjj=0; jjj < (int)jvals.size(); jjj++)
            AH_DEBUG << "Possible central signal: readout_id_rmap = " << signals.m_readout_id_rmap[clusters[i][jvals[jjj]]] << std::endl;
          AH_DEBUG << "Selected signal: readout_id_rmap = " << signals.m_readout_id_rmap[clusters[i][jcluster]] << std::endl;
        }
      }

      // store shape index in extraout and flag that some merging was done
      extraoutrow.m_clstrshape=shape;
      if (shape != e_SINGLE) extraoutrow.m_merge1_0=1;

      // if invalid shape, done with occurrence
      if (shape == e_NOSHAPE) {
        setRecoStatusBit(outrow, RECO_CLUSTER_WRONG_SHAPE);
        sgdevtidlib::setOutputEventNull(outrow);
        reco_complete = true;
        AH_DEBUG << "Finished: Cluster shape is bad (RECO_CLUSTER_WRONG_SHAPE)" << std::endl;
        break;
      } else {
        AH_DEBUG << "Cluster has valid shape: shape = " << cluster_shape_string[shape] << std::endl;
        AH_DEBUG << "Cluster signal index: jcluster = " << jcluster << std::endl;
        AH_DEBUG << "Cluster central signal has readout_id_rmap = " << signals.m_readout_id_rmap[clusters[i][jcluster]] << std::endl;
      }

      // sanity check: jcluster must not be negative
      if (jcluster < 0) AH_THROW_LOGIC("Bug!  Value of jcluster not assigned for valid cluster");

      // assign variance
      if (hxisgdevtid::SGDLayerType(it->first) == hxisgdevtid::SGD_SI_STACK) {
        variance=fluor_table.m_sgd_errsi_a_sqr+
                 fluor_table.m_sgd_errsi_b_sqr*tot_epi+
                 fluor_table.m_sgd_errsi_c_sqr*tot_epi*tot_epi;
        AH_DEBUG << "Si variance (res^2) = " << variance << std::endl;
      } else if (hxisgdevtid::SGDLayerType(it->first) == hxisgdevtid::SGD_CDTE_BOTTOM) {
        variance=fluor_table.m_sgd_errcdbtm_a_sqr+
                 fluor_table.m_sgd_errcdbtm_b_sqr*tot_epi+
                 fluor_table.m_sgd_errcdbtm_c_sqr*tot_epi*tot_epi;
        AH_DEBUG << "CdTe bottom variance (res^2) = " << variance << std::endl;
      } else {
        variance=fluor_table.m_sgd_errcdsid_a_sqr+
                 fluor_table.m_sgd_errcdsid_b_sqr*tot_epi+
                 fluor_table.m_sgd_errcdsid_c_sqr*tot_epi*tot_epi;
        AH_DEBUG << "CdTe side variance (res^2) = " << variance << std::endl;
      }

      //  Cluster is good, so merge all signals onto signal indicated by jcluster
      //    (1) generate sigarray mapping; 
      //    (2) store total energy in max element of cluster 
      //        and set the others to zero
      for (int j=0; j < csize; ++j) {
        int jr = clusters[i][j]; // i = current cluster, j = current signal
        AH_DEBUG << "Merging signal number " << jr << " of row..." << std::endl;
        if (j == jcluster) {
          signals.m_epi_current[jr] = tot_epi;
          signals.m_epi_merge_1_0[jr] = tot_epi;
          signals.m_var_merge_1_0[jr] = variance; // Not a total, just one
          signals.m_merge_survivor[jr]  = true;   // This should be redundant.
          AH_DEBUG << "... " << jr << " is the max element" << std::endl;
        } else {
          signals.m_epi_current[jr] = 0.0;
          signals.m_epi_merge_1_0[jr] = 0.0;
          signals.m_var_merge_1_0[jr] = 0.0;
          signals.m_merge_survivor[jr] = false;
          AH_DEBUG << "... " << jr << " is not the max element" << std::endl;
        }
        signals.m_sigarray_1_0[jr] = clusters[i][jcluster];
      }
      AH_DEBUG << "Done merging cluster; final EPI = " << signals.m_epi_current[clusters[i][jcluster]] << std::endl;

    }  // end loop on clusters
    if (reco_complete) break;
  } // end loop on layers

  //  Build extra output row.
  //
  if (par.extrainfo) {
    for (int i=0; i < signals.m_nsignal; ++i) {
      extraoutrow.m_sigarray1_0[i] = signals.m_sigarray_1_0[i];
    }
    extraoutrow.num_sigarray1_0 = signals.m_nsignal;
  }

  //  If only one signal left, we are done.
  //
  if (!reco_complete && total_nclust == 1) {
    setRecoStatusBit(outrow, RECO_ONE_HIT_REMAINING_1_0);
    getSequenceOneHitNoEscape(signals, sequence_type);

    //  Build primary output row.
    //
    buildOutputRowOneHit(signals, outrow);
    AH_DEBUG << "Finished: one hit remaining after step 1_0 (RECO_ONE_HIT_REMAINING_1_0)" << std::endl;
    reco_complete = true;
  }

  return;
}

// ****************************************************************************

void  mergeFluorAndScatter(Par & par, RowSignals& signals,
  hxisgdevtid::fluor::DataType& fluor_table, 
  hxisgdevtid::remap::GeomKeywords geom, 
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete) {

  // Vector sizes needed for basic allocations, immediately following.
  int nsignal = signals.m_nsignal;
  int npair = ((nsignal-1)*nsignal)/2;

  // Incremented when signal becomes candidate to be merged into a hit
  std::vector<int> cand(nsignal);

  // For tracking sorted priority: signal already used in current step.
  std::vector<bool> flag_used(nsignal);

  // First and second members of pairs. 
  std::vector<int> j_pair(npair);
  std::vector<int> k_pair(npair);

  // Distance between members of pairs in CAMERA coordinates.
  std::vector<double> distance(npair);

  // Flag for good pairs in the sense that both signals are valid.
  std::vector<bool> pair_flag(npair);

  // Flag for good pairs in the sense that they should be merged.
  std::vector<bool> good(npair);

  // What should be merged into what
  std::vector<int> src(npair);
  std::vector<int> dest(npair);

  // This is a utility struct set up to enable use of the std::sort routine.
  std::vector<IntDouble> pair_distance(npair);
  std::vector<int> ss(npair);    // Sorted subscripts

  // Initialize vectors.
  //
  for (int i=0; i<npair; ++i) {
    j_pair[i] = -1;
    k_pair[i] = -1;
    distance[i] = -1.0;
    pair_flag[i] = false;
  }

  // Number of hits.
  int m=0;
  
  //  Initialize m here to the number of possible hits after step 1_0.
  //  m will be decremented as signals disappear by being merged into
  //  other signals.
  //
  for (int i=0; i < nsignal; ++i) {
    if (signals.m_merge_survivor[i]) ++m;
  }

  //  Build list of pairs.  The lesser number signal is alway put
  //  into the first member of the pair, to avoid ambiguity.

  //  Loop on signals j and k without duplicating pairs.
  //
  int jkpair = 0, n_goodpair = 0;
  for (int j=0; j<nsignal; ++j) {

    double cxj = signals.m_camerax[j];
    double cyj = signals.m_cameray[j];
    double czj = signals.m_cameraz[j];

    for (int k=j+1; k<nsignal; ++k) {   // k > j

      double cxk = signals.m_camerax[k];
      double cyk = signals.m_cameray[k];
      double czk = signals.m_cameraz[k];

      //  Each SignalPair includes:  j, k, flag for good or bad pair, and distance
      //
      if (signals.m_merge_survivor[j] && signals.m_merge_survivor[k]) {
        pair_flag[jkpair] = true;
        j_pair[jkpair] = j;
        k_pair[jkpair] = k;
        ++n_goodpair;
      } else {
        pair_flag[jkpair] = false;
        j_pair[jkpair] = -1;
        k_pair[jkpair] = -1;
      }
      distance[jkpair] = 
        std::sqrt((cxk-cxj)*(cxk-cxj) + (cyk-cyj)*(cyk-cyj) + (czk-czj)*(czk-czj));

      //  This is a trick to set up the sort by distance, below.
      //
      pair_distance[jkpair].m_int = jkpair;
      pair_distance[jkpair].m_double = distance[jkpair];
      ++jkpair;
    }
  }
  if (npair != jkpair) {
    AH_THROW_LOGIC("Bad number of pairs");
  }

  //  Step 1a: fluorescence and scattering
  //  There is a specific order for checking the possibilities:
  //  
  //  Substeps:
  //    1a-1a:  CdTe-CdTe, same layer
  //    1a-1b:  CdTe-CdTe, different layers
  //    1a-2:   Si-CdTe
  //    1a-3:   Si-Si, different layers
  //
  //  NOTE:  There are subtle differences among these
  //  substeps.  Each substep is structured similarly to the others, 
  //  but is not the same.

  //  Substep 1a-1a:  CdTe-CdTe, same layer

  AH_DEBUG << "Starting substep 1a-1a - Fluorescence CdTe-CdTe (same layer)" << std::endl;

  //  Copy tracking (including sigarray) information from previous substep.
  //
  for (int i=0; i < nsignal; ++i) {
    signals.m_epi_merge_1a_1a[i] = signals.m_epi_merge_1_0[i];
    signals.m_var_merge_1a_1a[i] = signals.m_var_merge_1_0[i];
    signals.m_sigarray_1a_1a[i] = signals.m_sigarray_1_0[i];
    AH_DEBUG << "Step 1a-1a: Setting m_epi_current to " << signals.m_epi_merge_1_0[i] << " for index " << i << std::endl;
    signals.m_epi_current[i] = signals.m_epi_merge_1_0[i];
    signals.m_var_current[i] = signals.m_var_merge_1_0[i];
  }

  // Initialize vectors.
  //
  for (int i=0; i<npair; ++i) {
    good[i] = false;
    src[i] = -1;
    dest[i] = -1;
  }
  for (int j=0; j<nsignal; ++j) {
    flag_used[j] = false;
    cand[j] = 0;
  }

  for (int i=0; i < npair; ++i) {
    if (pair_flag[i]) {  // Is it a good pair?
      int j = j_pair[i]; // Fetch signal indexes
      int k = k_pair[i];
      int lj = signals.m_layer[j];  // Layer index
      int lk = signals.m_layer[k];

      if (lj == lk && hxisgdevtid::SGDLayerIsCdTe(lj) && hxisgdevtid::SGDLayerIsCdTe(lk)) {

        AH_DEBUG << "Pair loop 1a-1a i=" << i << "; signals " << j_pair[i] << " & " << k_pair[i] << std::endl;
        AH_DEBUG << "layer[" << j << "] = " << lj << std::endl;
        AH_DEBUG << "layer[" << k << "] = " << lk << std::endl;
        AH_DEBUG << "distance = " << distance[i] << std::endl;
        AH_DEBUG << "distance check (par.d1a1a) = " << par.d1a1a << std::endl;

        if (distance[i] <= par.d1a1a) {
          
          //  Same layer, CdTe:  retrieve info
          bool fluorj = energyIsCdTeFluor(signals.m_epi_current[j], fluor_table);
          bool fluork = energyIsCdTeFluor(signals.m_epi_current[k], fluor_table);
          AH_DEBUG << "1a-1a: fluorj = " << fluorj << "; energy = " << signals.m_epi_current[j] << std::endl;
          AH_DEBUG << "1a-1a: fluork = " << fluork << "; energy = " << signals.m_epi_current[k] << std::endl;
  
          if (fluorj || fluork) {
            double epij = signals.m_epi_current[j];               // Signal energy
            double epik = signals.m_epi_current[k];
            ++cand[j];
            ++cand[k];
            good[i] = true;
  
            if (fluorj && fluork) {
              //  Both could be fluorescence; pick higher energy
              if (epij > epik) {
                AH_DEBUG << "Both signals could be fluorescence; merge into higher energy signal: " << j << std::endl;
                src[i] = k;
                dest[i] = j;
              } else if (epij < epik) {
                AH_DEBUG << "Both signals could be fluorescence; merge into higher energy signal: " << k << std::endl;
                src[i] = j;
                dest[i] = k;
              } else {
                AH_DEBUG << "Both signals could be fluorescence and have same energy" << std::endl;
                if (ahgen::getRandomInt(0,1) == 0) {
                  AH_DEBUG << "  randomly choose to merge into signal " << j << std::endl;
                  src[i] = k;
                  dest[i] = j;
                } else {
                  AH_DEBUG << "  randomly choose to merge into signal " << k << std::endl;
                  src[i] = j;
                  dest[i] = k;
                }
              }
            } else if (fluorj) {
              //  Signal j is fluorescence, so merge into signal k
              AH_DEBUG << "Only signal " << j << " is fluorescent; merge into signal " << k << std::endl;
              src[i] = j;
              dest[i] = k;
            } else if (fluork) {
              //  Signal k is fluorescence, so merge into signal j
              AH_DEBUG << "Only signal " << k << " is fluorescent; merge into signal " << j << std::endl;
              src[i] = k;
              dest[i] = j;
            }
          }
  
        } else {     //  If bad, just continue
          AH_DEBUG << "Neither signal is fluorescent" << std::endl;
        }
      }   // end if (both layers are the same and are CdTe)
    }     // end if (valid pair)
  }

  // Check for signals with more than 1 merging candidate.  For such signals,
  // randomly choose which partner to merge with.
//  for (int i=0; i < nsignal; ++i) {
//    if (cand[i] > 1) {
//      AH_DEBUG << "Signal " << i << " could merge with multiple signals:" << std::endl;
//      std::vector<int> poss;            // possibilities of pair index to use
//      for (int jk=0; jk < npair; ++jk) {
//        if (!good[jk]) continue;
//        if (src[jk] == i || dest[jk] == i) poss.push_back(jk);
//        if (src[jk] == i)
//          AH_DEBUG << "  for pair " << jk << " could merge with destination signal " << dest[jk] << std::endl;
//        else
//          AH_DEBUG << "  for pair " << jk << " could merge with source signal " << src[jk] << std::endl;
//      }
//      int jkkeep=poss[ahgen::getRandomInt(0,poss.size()-1)];
//      AH_DEBUG << "Randomly selected pair " << jkkeep << " as merging pair" << std::endl;
//      extraoutrow.m_rand1a_1a=1;          // flag that random selection made
//      for (std::vector<int>::iterator ijk=poss.begin(); ijk != poss.end(); ijk++) {
//        if (*ijk != jkkeep) {
//          cand[j_pair[*ijk]]--;
//          cand[k_pair[*ijk]]--;
//          good[*ijk]=false;
//        }
//      }
//    }
//  }
  for (int i=0; i < nsignal; ++i) {
    if (cand[i] > 1) {
      AH_DEBUG << "Signal " << i << " could merge with multiple signals:" << std::endl;
      for (int jk=0; jk < npair; ++jk) {
        if (!good[jk]) continue;
        if (src[jk] == i || dest[jk] == i) {
          cand[j_pair[jk]]--;
          cand[k_pair[jk]]--;
          good[jk]=false;
          if (src[jk] == i)
            AH_DEBUG << "  for pair " << jk << " skip merging with destination signal " << dest[jk] << std::endl;
          else
            AH_DEBUG << "  for pair " << jk << " skip merging with source signal " << src[jk] << std::endl;
        }
      }
    }
  }

  //  Merge signals.
  //
  for (int i=0; i<npair; ++i) {
    if (good[i]) {
      --m;
      merge_signals_1a_1a(signals, src[i], dest[i]);
      extraoutrow.m_merge1a_1a=1;     // flag that merging was done in this step
    }
  }
  assertNumHitsCorrect(signals, m);

  //  Build extra output row.
  //
  if (par.extrainfo) {
    for (int i=0; i < nsignal; ++i) {
      extraoutrow.m_sigarray1a_1a[i] = signals.m_sigarray_1a_1a[i];
    }
      extraoutrow.num_sigarray1a_1a = nsignal;
  }

  // If one signal left, done.  Write out the event and go to next
  // occurrence.

  //  Get the description of the hit.
  //
  if (m == 1) {
    setRecoStatusBit(outrow, RECO_ONE_HIT_REMAINING_1A_1A);
    AH_DEBUG << "Finished: one hit remaining after 1a-1a (RECO_ONE_HIT_REMAINING_1A_1A)" << std::endl;
    reco_complete = true;

    //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
    //
    getSequenceOneHitNoEscape(signals, sequence_type);

    //  Fill in output row.
    //
    buildOutputRowOneHit(signals, outrow);
    return;
  }
        
  // Substep 1a-1b:  CdTe-CdTe, different layers

  AH_DEBUG << "Starting substep 1a-1b - Fluorescence CdTe-CdTe (different layers)" << std::endl;

  //  Merge-tracking data for this step initialized.
  //
  for (int i=0; i < nsignal; ++i) {
    signals.m_epi_merge_1a_1b[i] = signals.m_epi_merge_1a_1a[i];
    signals.m_var_merge_1a_1b[i] = signals.m_var_merge_1a_1a[i];
    signals.m_sigarray_1a_1b[i] = signals.m_sigarray_1a_1a[i];
    AH_DEBUG << "Step 1a-1b: Setting m_epi_current to " << signals.m_epi_merge_1a_1a[i] << " for index " << i << std::endl;
    signals.m_epi_current[i] = signals.m_epi_merge_1a_1a[i];
    signals.m_var_current[i] = signals.m_var_merge_1a_1a[i];
  }

  //  Sort pairs by distance.  Setup for this was done in pair
  //  forming loop before step 1a-1a.  Here we need the data so here
  //  we do the sort.
  //
  std::sort(pair_distance.begin(), pair_distance.end(), IntDoubleCompare);
  for (int i=0; i<npair; ++i) {
    ss[i] = pair_distance[i].m_int;   // Subscripts sorted by pair distance
  }

  // Initialize vectors.
  //
  for (int i=0; i<npair; ++i) {
    good[i] = false;
    src[i] = -1;
    dest[i] = -1;
  }
  for (int j=0; j<nsignal; ++j) {
    flag_used[j] = false;
    cand[j] = 0;
  }

  for (int i=0; i < npair; ++i) {

    int j = j_pair[i];
    int k = k_pair[i];

    //  This tracks whether the signal is still in existence
    //  (hasn't been merged into another one and thus disappeared).
    //
    bool ok_j = signals.m_merge_survivor[j];
    bool ok_k = signals.m_merge_survivor[k];
    
    //  Make sure both signals are above threshold
    //  and have not been used already.

    if (pair_flag[i] && ok_k && ok_j) {

      int lj = signals.m_layer[j];  // Layers
      int lk = signals.m_layer[k];

      //  Make sure both signals are CdTe and from different layers.
      //
      if (lj != lk && hxisgdevtid::SGDLayerIsCdTe(lj) && hxisgdevtid::SGDLayerIsCdTe(lk)) {

        AH_DEBUG << "1a-1b:  pair loop, i = " << i << "  signals " << j << " & " << k << std::endl;
        AH_DEBUG << "layer[" << j << "] = " << lj << std::endl;
        AH_DEBUG << "layer[" << k << "] = " << lk << std::endl;
        AH_DEBUG << "distance = " << distance[i] << std::endl;
        AH_DEBUG << "distance check (par.d1a1b) = " << par.d1a1b << std::endl;
  
        //  Use distance criterion.
        //
        if (distance[i] <= par.d1a1b) {
  
          bool fluorj = energyIsCdTeFluor(signals.m_epi_current[j], fluor_table);
          bool fluork = energyIsCdTeFluor(signals.m_epi_current[k], fluor_table);
          AH_DEBUG << "1a-1b: fluorj = " << fluorj << "; energy = " << signals.m_epi_current[j] << std::endl;
          AH_DEBUG << "1a-1b: fluork = " << fluork << "; energy = " << signals.m_epi_current[k] << std::endl;
  
          if (fluorj || fluork) {
            double epij = signals.m_epi_current[j];
            double epik = signals.m_epi_current[k];
            good[i] = true;
            if (fluorj && fluork) {
              //  Both could be fluorescence; pick higher energy
              if (epij > epik) {
                AH_DEBUG << "Both signals could be fluorescence; merge into higher energy signal: " << j << std::endl;
                src[i] = k;
                dest[i] = j;
              } else if (epik > epij) {
                AH_DEBUG << "Both signals could be fluorescence; merge into higher energy signal: " << k << std::endl;
                src[i] = j;
                dest[i] = k;
              } else {
                AH_DEBUG << "Both signals could be fluorescence and have same energy" << std::endl;
                if (ahgen::getRandomInt(0,1) == 0) {
                  AH_DEBUG << "  randomly choose to merge into signal " << j << std::endl;
                  src[i] = k;
                  dest[i] = j;
                } else {
                  AH_DEBUG << "  randomly choose to merge into signal " << k << std::endl;
                  src[i] = j;
                  dest[i] = k;
                }
              }
            } else if (fluorj) {
              //  Signal j is fluorescence, so merge into signal k
              AH_DEBUG << "Only signal " << j << " is fluorescent; merge into signal " << k << std::endl;
              src[i] = j;
              dest[i] = k;
            } else if (fluork) {
              //  Signal k is fluorescence, so merge into signal j
              AH_DEBUG << "Only signal " << k << " is fluorescent; merge into signal " << j << std::endl;
              src[i] = k;
              dest[i] = j;
            }
          }
        }   // end if (distance within parameter)
      }     // end if (both layers CdTe and adjacent)
    }       // end if (valid pair)
  }

  // Look for pairs involving the same signal where the pair distances are the
  // same; in such cases, do not merge with any possibility.
  // Note: using sorted indices, given by ss[].
  for (int i=0; i < nsignal; ++i) {
    std::vector<int> poss;            // Possibilities of pair index to use.
    double mindist=-1.;
    for (int jk=0; jk < npair; ++jk) {
      if (!good[ss[jk]]) continue;
      if (src[ss[jk]] != i && dest[ss[jk]] != i) continue;
      if (mindist < 0.0) mindist=pair_distance[jk].m_double;    // pair_distance is already sorted, so don't use sorted indices.
      if (pair_distance[jk].m_double == mindist) poss.push_back(jk);
    }
    if (poss.size() > 1) {
      AH_DEBUG << "Signal " << i << " can merge with multiple signals: " << std::endl;
      for (std::vector<int>::iterator ijk=poss.begin(); ijk != poss.end(); ijk++) {
        good[ss[*ijk]]=false;
        if (src[ss[*ijk]] == i)
          AH_DEBUG << "  ... remove merging with destination signal " << dest[ss[*ijk]] << std::endl;
        else
          AH_DEBUG << "  ... remove merging source signal " << src[ss[*ijk]] << std::endl;
      }
    }
  }

  //  Merge signals.
  //
  //  Here the pairs are treated in distance-sorted order, so
  //  we use the sorted subscripts array.
  //
  for (int i=0; i<npair; ++i) {
    int is=ss[i];     // Sorted subscript.
    if (good[is] && !(flag_used[src[is]]) && !(flag_used[dest[is]])) {
      flag_used[src[is]] = true;
      flag_used[dest[is]] = true;
      --m;
      merge_signals_1a_1b(signals, src[is], dest[is]);
      extraoutrow.m_merge1a_1b=1;     // Flag that merging was done in this step.
    }
  }
  assertNumHitsCorrect(signals, m);

  //  Build extra output row.
  //
  if (par.extrainfo) {
    for (int i=0; i < nsignal; ++i) {
      extraoutrow.m_sigarray1a_1b[i] = signals.m_sigarray_1a_1b[i];
    }
    extraoutrow.num_sigarray1a_1b = nsignal;
  }

  // If one signal left, done.  Write out the event and go to next
  // occurrence.
  //
  if (m == 1) {
    setRecoStatusBit(outrow, RECO_ONE_HIT_REMAINING_1A_1B);
    AH_DEBUG << "Finished: one hit remaining after 1a-1b (RECO_ONE_HIT_REMAINING_1A_1B)" << std::endl;
    reco_complete = true;

    //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
    //
    getSequenceOneHitNoEscape(signals, sequence_type);

    //  Fill in CAMERAX, CAMERAY, CAMERAZ.
    //
    buildOutputRowOneHit(signals, outrow);
    return;
  }

  //  Substep 1a-2:  Si-CdTe Combination.
  //
  AH_DEBUG << "Starting substep 1a-2 - Fluorescence CdTe-Si" << std::endl;

  //  Copy merge tracking data from previous step to this step and
  //  initialize flag_used.
  //
  for (int i=0; i < nsignal; ++i) {
    signals.m_epi_merge_1a_2[i] = signals.m_epi_merge_1a_1b[i];
    signals.m_var_merge_1a_2[i] = signals.m_var_merge_1a_1b[i];
    signals.m_sigarray_1a_2[i] = signals.m_sigarray_1a_1b[i];
    signals.m_epi_current[i] = signals.m_epi_merge_1a_1b[i];
    signals.m_var_current[i] = signals.m_var_merge_1a_1b[i];
  }

  // Initialize vectors.
  //
  for (int i=0; i<npair; ++i) {
    good[i] = false;
    src[i] = -1;
    dest[i] = -1;
  }
  for (int j=0; j<nsignal; ++j) {
    flag_used[j] = false;
    cand[j] = 0;
  }

  //  Go through pairs.
  //
  for (int i=0; i < npair; ++i) {
     
    int j = j_pair[i];
    int k = k_pair[i];

    //  This tracks whether the signal is still in existence
    //  (hasn't been merged into another one and thus disappeared).
    //
    bool ok_j = signals.m_merge_survivor[j];
    bool ok_k = signals.m_merge_survivor[k];
    
    //  Make sure both signals are above threshold as well.
    //
    if (pair_flag[i] && ok_k && ok_j) {
      
      //  Signals used in this step cannot be used again.
      //
      int lj = signals.m_layer[j];   // Layers
      int lk = signals.m_layer[k];

      if (((layerIsCdTeNearSi(lj, geom) && hxisgdevtid::SGDLayerIsSi(lk)) || (hxisgdevtid::SGDLayerIsSi(lj) && layerIsCdTeNearSi(lk, geom)))) {

        AH_DEBUG << "1a-2:  pair loop, i = " << i << "  signals " << j << " & " << k << std::endl;
        AH_DEBUG << "layer[" << j << "] = " << lj << std::endl;
        AH_DEBUG << "layer[" << k << "] = " << lk << std::endl;
        AH_DEBUG << "distance = " << distance[i] << std::endl;
        AH_DEBUG << "distance check (par.d1a2) = " << par.d1a2 << std::endl;
  
        //  Make sure both signals are in the right layers and within the distance
        //  criterion.
        //
        if (distance[i] <= par.d1a2) {
  
          bool fluorj = hxisgdevtid::SGDLayerIsSi(lj) 
            && energyIsSiFluor(signals.m_epi_current[j], fluor_table);
          bool fluork = hxisgdevtid::SGDLayerIsSi(lk) 
            && energyIsSiFluor(signals.m_epi_current[k], fluor_table);
          AH_DEBUG << "1a-2: fluorj = " << fluorj << "; energy = " << signals.m_epi_current[j] << std::endl;
          AH_DEBUG << "1a-2: fluork = " << fluork << "; energy = " << signals.m_epi_current[k] << std::endl;
  
          if (fluorj || fluork) {
            good[i] = true;
  
            if (fluorj) {

              //  Signal j is fluorescence, so merge into signal k.
              //
              AH_DEBUG << "Signal " << j << " is fluorescent; merge into signal " << k << std::endl;
              src[i] = j;
              dest[i] = k;
            } else if (fluork) {

              //  Signal k is fluorescence, so merge into signal j.
              //
              AH_DEBUG << "Only signal " << k << " is fluorescent; merge into signal " << j << std::endl;
              src[i] = k;
              dest[i] = j;
            }
          }
        }    // end if (distance within parameter)
      }      // end if (have CdTe and Si layers near to each other)
    }        // end if (valid pair)
  }

  // Look for pairs involving the same signal where the pair distances are the
  // same; in such cases, do not merge with any possibility
  // Note: using sorted indices, given by ss[].
  //
  for (int i=0; i < nsignal; ++i) {
    std::vector<int> poss;            // Possibilities of pair index to use.
    double mindist=-1.;
    for (int jk=0; jk < npair; ++jk) {
      if (!good[ss[jk]]) continue;
      if (src[ss[jk]] != i && dest[ss[jk]] != i) continue;
      if (mindist < 0.0) mindist=pair_distance[jk].m_double;    // pair_distance is already sorted, so don't use sorted indices.
      if (pair_distance[jk].m_double == mindist) poss.push_back(jk);
    }
    if (poss.size() > 1) {
      AH_DEBUG << "Signal " << i << " can merge with multiple signals: " << std::endl;
      for (std::vector<int>::iterator ijk=poss.begin(); ijk != poss.end(); ijk++) {
        good[ss[*ijk]]=false;
        if (src[ss[*ijk]] == i)
          AH_DEBUG << "  ... remove merging with destination signal " << dest[ss[*ijk]] << std::endl;
        else
          AH_DEBUG << "  ... remove merging with source signal " << src[ss[*ijk]] << std::endl;
      }
    }
  }

  //
  //  Here the pairs are treated in distance-sorted order, so
  //  we use the sorted subscripts array.
  //
  for (int i=0; i<npair; ++i) {
    int is=ss[i];     // Sorted subscript.
    if (good[is] && !(flag_used[src[is]]) && !(flag_used[dest[is]])) {
      flag_used[src[is]] = true;
      flag_used[dest[is]] = true;
      --m;
      merge_signals_1a_2 (signals, src[is], dest[is]);
      extraoutrow.m_merge1a_2=1;     // Flag that merging was done in this step.
    }
  }
  assertNumHitsCorrect(signals, m);

  //  Build extra output row.
  //
  if (par.extrainfo) {
    for (int i=0; i < nsignal; ++i) {
      extraoutrow.m_sigarray1a_2[i] = signals.m_sigarray_1a_2[i];
    }
    extraoutrow.num_sigarray1a_2 = nsignal;
  }

  if (m == 1) {
    setRecoStatusBit(outrow, RECO_ONE_HIT_REMAINING_1A_2);
    AH_DEBUG << "Finished: one hit remaining after 1a-2 (RECO_ONE_HIT_REMAINING_1A_2)" << std::endl;
    reco_complete = true;

    //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
    //
    getSequenceOneHitNoEscape(signals, sequence_type);


    //  Fill in CAMERAX, CAMERAY, CAMERAZ.
    //
    buildOutputRowOneHit(signals, outrow);
    return;
  }

  //  Substep 1a-3:  Si-Si Combination.
  //
  AH_DEBUG << "Starting substep 1a-3 - electron scattering Si-Si" << std::endl;

  //  Propagate the signal merging information from the previous step.
  //
  for (int i=0; i < nsignal; ++i) {
    signals.m_epi_merge_1a_3[i] = signals.m_epi_merge_1a_2[i];
    signals.m_var_merge_1a_3[i] = signals.m_var_merge_1a_2[i];
    signals.m_sigarray_1a_3[i] = signals.m_sigarray_1a_2[i];
    signals.m_epi_current[i] = signals.m_epi_merge_1a_2[i];
    signals.m_var_current[i] = signals.m_var_merge_1a_2[i];
  }

  // Get list of signals to check (from Si layer).
  //
  std::set<int> candidates;
  for (int i=0; i < nsignal; ++i) {
    if (!hxisgdevtid::SGDLayerIsSi(signals.m_layer[i])) continue;     // Only check signals in Si layers.
    if (!signals.m_merge_survivor[i]) continue;       // Skip signals which are already merged.
    candidates.insert(i);
  }
  AH_DEBUG << "Number of Si-layer signals to check: " << candidates.size() << std::endl;

  while (candidates.size() > 0) {
    int isig=*candidates.begin();     // Get first element from set.
    candidates.erase(isig);           // Remove element from set.
    AH_DEBUG << "Form group starting with signal " << isig << std::endl;

    std::vector<int> group;        // To hold indices of group.
    group.push_back(isig);
    int igroup=-1;                 // Current index of group.
    while (1) {
      igroup++;
      if (igroup > (int)group.size()-1) break;     // Done forming group.
      int jsig=group[igroup];
      std::set<int> toremove;        // candidates to remove if added to a group
      for (std::set<int>::iterator kit=candidates.begin(); kit != candidates.end(); kit++) {
        double distance=calcDistance(signals,jsig,*kit);
        if (signals.m_layer[jsig] == signals.m_layer[*kit]) {    // skip signals pairs on same layer
          AH_DEBUG << "Skipping signals on same layer: " << jsig << ", " << *kit << std::endl;
          continue;
        }
        AH_DEBUG << "Distance between signals " << jsig << " and " << *kit << " is " << distance << " (check = " << par.d1a3 << ")" << std::endl;
        if (calcDistance(signals,jsig,*kit) <= par.d1a3) {
          AH_DEBUG << "Adding signal to group: " << *kit << std::endl;
          group.push_back(*kit);
          toremove.insert(*kit);
        }
      }
      // remove candidates that have been added to a group
      for (std::set<int>::iterator kit=toremove.begin(); kit != toremove.end(); kit++) {
        candidates.erase(*kit);
      }
      toremove.clear();
      
    }

    // If group has only one point, nothing to do.
    //
    if (group.size() == 1) {
      AH_DEBUG << "Only one point in group; nothing to do" << std::endl;
      continue;
    }
    AH_DEBUG << "Group has nsignal = " << group.size() << std::endl;

    // If group is too large, set RECO_STATUS and go to next occurrence.
    //
    if (group.size() > 3) {
      setRecoStatusBit(outrow, RECO_SI_SI_GROUP_TOO_LARGE);
      sgdevtidlib::setOutputEventNull(outrow);
      AH_DEBUG << "Si-Si group has more than 3 signals; nsignals: " << group.size() << std::endl;
      AH_DEBUG << "Setting RECO_STATUS: RECO_SI_SI_GROUP_TOO_LARGE" << std::endl;
      reco_complete = true;
      return;
    }

    // Get value of largest EPI.
    //
    double max_epi=0.0;
    for (std::vector<int>::iterator iit=group.begin(); iit != group.end(); iit++) {
      double epi=signals.m_epi_current[*iit];
      if (epi > max_epi) max_epi=epi;
    }
    AH_DEBUG << "Max EPI in group: " << max_epi << std::endl;

    // Check if more than one signal has the max EPI; if so select one of those
    // signals at random to merge into.
    //
    std::vector<int> poss;
    for (std::vector<int>::iterator iit=group.begin(); iit != group.end(); iit++) {
      if (signals.m_epi_current[*iit] == max_epi) poss.push_back(*iit);
    }
    if (poss.size() > 1) {
      extraoutrow.m_rand1a_3=1;
      AH_DEBUG << "More than one signal with max(EPI); selecting random destination" << std::endl;
    }
    int idest=poss[ahgen::getRandomInt(0,poss.size()-1)];
    AH_DEBUG << "Merge group into signal: " << idest << std::endl;

    // Merge group together.
    //
    m-=group.size()-1;     // Decrease number of hits by extra signals in group.
    merge_signals_1a_3(signals,group,idest);
    extraoutrow.m_merge1a_3=1;     // Flag that merging was done in this step.

  } // end while signals left to check


  //  Build extra output row.
  //
  if (par.extrainfo) {
    for (int i=0; i < nsignal; ++i) {
      extraoutrow.m_sigarray1a_3[i] = signals.m_sigarray_1a_3[i];
    }
    extraoutrow.num_sigarray1a_3 = nsignal;
    extraoutrow.m_m = m;
  }

  // If one signal left, done.  Write out the event and go to next
  // occurrence.
  //
  if (m == 1) {

    setRecoStatusBit(outrow, RECO_ONE_HIT_REMAINING_1A_3);
    AH_DEBUG << "Finished: one hit remaining after 1a-3 (RECO_ONE_HIT_REMAINING_1A_3)" << std::endl;
    reco_complete = true;

    //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
    //
    getSequenceOneHitNoEscape(signals, sequence_type);

    //  Fill in CAMERAX, CAMERAY, CAMERAZ.
    //
    buildOutputRowOneHit(signals, outrow);
    return;
  }

  //  Patterns with more than 4 hits are too complicated to analyze.
  
  if (m > MAX_NUMHITS) {
    setRecoStatusBit(outrow, RECO_TOO_MANY_HITS);
    sgdevtidlib::setOutputEventNull(outrow);
    AH_DEBUG << "Finished: more than 4 hits after merging (RECO_TOO_MANY_HITS)" << std::endl;
    reco_complete = true;
    return;
  }

  return;

}

// ****************************************************************************
//
// Utility routines for merging signals.
//
              
void merge_signals_1a_1a (RowSignals& signals, int src, int dest) {
  signals.m_epi_merge_1a_1a[dest] += signals.m_epi_merge_1a_1a[src];
  signals.m_epi_current[dest] = signals.m_epi_merge_1a_1a[dest]; 

  signals.m_var_merge_1a_1a[dest] += signals.m_var_merge_1a_1a[src];
  signals.m_var_current[dest] = signals.m_var_merge_1a_1a[dest];

  signals.m_epi_merge_1a_1a[src] = 0;
  signals.m_var_merge_1a_1a[src] = 0;
  signals.m_epi_current[src] = 0;
  signals.m_var_current[src] = 0;
  signals.m_merge_survivor[src] = false;
  signals.m_sigarray_1a_1a[src] = signals.m_sigarray_1a_1a[dest]; 
}

// ****************************************************************************
              
void merge_signals_1a_1b (RowSignals& signals, int src, int dest) {
  signals.m_epi_merge_1a_1b[dest] += signals.m_epi_merge_1a_1b[src];
  signals.m_epi_current[dest] = signals.m_epi_merge_1a_1b[dest]; 

  signals.m_var_merge_1a_1b[dest] += signals.m_var_merge_1a_1b[src];
  signals.m_var_current[dest] = signals.m_var_merge_1a_1b[dest];

  signals.m_epi_merge_1a_1b[src] = 0;
  signals.m_var_merge_1a_1b[src] = 0;
  signals.m_epi_current[src] = 0;
  signals.m_var_current[src] = 0;
  signals.m_merge_survivor[src] = false;
  signals.m_sigarray_1a_1b[src] = signals.m_sigarray_1a_1b[dest]; 
}

// ****************************************************************************
              
void merge_signals_1a_2 (RowSignals& signals, int src, int dest) {
  signals.m_epi_merge_1a_2[dest] += signals.m_epi_merge_1a_2[src];
  signals.m_epi_current[dest] = signals.m_epi_merge_1a_2[dest]; 

  signals.m_var_merge_1a_2[dest] += signals.m_var_merge_1a_2[src];
  signals.m_var_current[dest] = signals.m_var_merge_1a_2[dest];

  signals.m_epi_merge_1a_2[src] = 0;
  signals.m_var_merge_1a_2[src] = 0;
  signals.m_epi_current[src] = 0;
  signals.m_var_current[src] = 0;
  signals.m_merge_survivor[src] = false;
  signals.m_sigarray_1a_2[src] = signals.m_sigarray_1a_2[dest]; 
}

// ****************************************************************************
              
void merge_signals_1a_3 (RowSignals& signals, std::vector<int> group, int dest) {

  for (std::vector<int>::iterator iit=group.begin(); iit != group.end(); iit++) {
    if (*iit == dest) continue;
    signals.m_epi_merge_1a_3[dest]+=signals.m_epi_merge_1a_3[*iit];
    signals.m_epi_current[dest]=signals.m_epi_merge_1a_3[dest];
    signals.m_var_merge_1a_3[dest]+=signals.m_var_merge_1a_3[*iit];
    signals.m_var_current[dest]=signals.m_var_merge_1a_3[dest];
    
    signals.m_epi_merge_1a_3[*iit] = 0;
    signals.m_var_merge_1a_3[*iit] = 0;
    signals.m_epi_current[*iit] = 0;
    signals.m_var_current[*iit] = 0;
    signals.m_merge_survivor[*iit] = false;
    signals.m_sigarray_1a_3[*iit] = signals.m_sigarray_1a_3[dest];
  }
}

// ****************************************************************************

bool pixelsAdjacent(RowSignals& signals, int j, int k, double d1_0) {
  // This implementation requires the pixels to be in the same
  // sensor.  Alternative implementations in the comments require
  // the same layer (a weaker requirement), or don't care about
  // the layer and sensor, as noted.

  // if (sig1->sensor_id != sig2->sensor_id) return false;  // if only in same layer
  // return abs(sig1->m_rawx - sig2->m_rawx) <= 1 && abs(sig1->m_rawy - sig2->m_rawy) <= 1;
  
  // Method using d1_0, if no requirement about layer or sensor:
  double dx = signals.m_camerax[k] - signals.m_camerax[j];
  double dy = signals.m_cameray[k] - signals.m_cameray[j];
  double dz = signals.m_cameraz[k] - signals.m_cameraz[j];
  double dsq = dx*dx + dy*dy + dz*dz; 
  double d1sq = d1_0*d1_0; 
  AH_DEBUG << "j = " << j << "; k = " << k << "; dsq = " << dsq << "; d1sq = " << d1sq << std::endl;
  return dsq <= d1sq + FLOAT_EPSILON;
}

// ****************************************************************************

bool IntDoubleCompare (const IntDouble& item1, const IntDouble& item2) {
  return item1.m_double < item2.m_double;
}

// ****************************************************************************

void performFGProbTests (Par& par, RowSignals& signals, Hits& hits,
  hxisgdevtid::fluor::DataType& fluor_table, sgdprobseq::DataType& probseq_table,
  hxisgdevtid::remap::GeomKeywords& geom, double prob[],
  bool test_f[], bool test_g[], double g[][MAX_NUMHITS], bool & escape_flag, double escape_en[MAX_PERM],
  OutputRowData& outrow, ahfits::FilePtr fpextra, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete) {

  //  Number of hits.
  //
  int m = hits.m_m;

  //  Variables used in F test
  //
  double f[MAX_PERM][MAX_NUMHITS];        // Quantity F to test Compton constraint
  double delta_f[MAX_PERM][MAX_NUMHITS];  // Uncertainty in F
  bool check_f[MAX_PERM][MAX_NUMHITS];    // F test result for each scattering case

  //  Variables used in G test
  //
  double delta_g[MAX_PERM][MAX_NUMHITS];           // Uncertainty in G
  bool check_g[MAX_PERM][MAX_NUMHITS];             // G test result for each scattering case
  double g_test_limit=0.0;                         // Bound of G for passing the test
  double r[MAX_PERM][MAX_NUMHITS][3];              // Pixel position vectors
  double delta_r[MAX_PERM][MAX_NUMHITS][3];        // Uncertainties in pixel position vectors
  double cos_tht_g_m_minus_2[MAX_PERM];            // Saved values of cos_theta_g
  double cos_theta_g[MAX_PERM][MAX_NUMHITS];       // Cosine of geometric scattering angle
  double cos_theta_k[MAX_PERM][MAX_NUMHITS];       // Cosine of kinematic scattering angle
  double delta_cos_theta_g[MAX_PERM][MAX_NUMHITS]; // Uncertainty in cos_theta_g
  double delta_cos_theta_k[MAX_PERM][MAX_NUMHITS]; // Uncertainty in cos_theta_k
  //
  //  ee is variable E in the Ichinohe presentation
  //    = the array of stepwise total energies for each permutation
  //  var_ee and delta_ee are the corresponding variances
  //    and uncertainties
  //  mat is the "material matrix" with codes for Si, CdTe bottom,
  //    and CdTe side, to be used in evaluating sequence probabilities
  //
  // *** Note: the procedure for constructing 'mat' is also performed
  //     in getHitSequence() where the result is stored in sequence_type.
  //     At this time, we have not factored out this algorithm.
  double ee[MAX_PERM][MAX_NUMHITS], var_ee[MAX_PERM][MAX_NUMHITS], delta_ee[MAX_PERM][MAX_NUMHITS];
  int mat[MAX_PERM][MAX_NUMHITS];
  
  //  Variables used in test for low-probability sequences
  //
  bool test_prob[MAX_PERM];   //  Flag indicating probability above threshold
  int num_test_prob = 0;      //  Number of test_prob that are true

  //  Access the correct permutation table for m.
  //
  int n_perm = 0;
  const int* pr = selectPermArray(m, n_perm);

  //  Initialize all those arrays.
  //
  for (int k=0; k<MAX_PERM; ++k) {
    for (int j=0; j<MAX_NUMHITS; ++j) {
      for (int n=0; n<3; ++n) {
        r[k][j][n] = 0.0;
      }
      f[k][j] = 0.0;
      delta_f[k][j] = 0.0;
      check_f[k][j] = true;
      g[k][j] = 0.0;
      delta_g[k][j] = 0.0;
      check_g[k][j] = true;
      cos_theta_g[k][j] = 0.0;
      cos_theta_k[k][j] = 0.0;
      delta_cos_theta_g[k][j] = 0.0;
      delta_cos_theta_k[k][j] = 0.0;
      ee[k][j] = 0.0;
      var_ee[k][j] = 0.0;
      delta_ee[k][j] = 0.0;
      mat[k][j] = -1;
    }
    prob[k] = 0.0;
    cos_tht_g_m_minus_2[k] = 0.0;
    test_f[k] = false;
    test_g[k] = false;
    test_prob[k] = false;
    escape_en[k]=0.0;        // declared in doWork of tool
  }

  //  Set up arrays of hit attributes for F and G calculations,
  //  corresponding to every permutation.
  //
  //  k steps through the permutations for the given m.
  //  j steps through the elements of each permutation.
  //
  for (int k=0; k<n_perm; ++k) {    // k: Step through permutations
    for (int j=0; j<m; ++j) {       // j: Step through members of each permutation

      int p = pr[m*k + j];          // Permuted index for j
      int hp = hits.m_hitarray[p];    // Permuted index in signals

      //  Set up hit coordinates for later use.  Done here because
      //  used in cos_tht_g_m_minus_2 calc below, in case the G test is skipped.
      //
      r[k][j][0] = signals.m_camerax[hp];
      r[k][j][1] = signals.m_cameray[hp];
      r[k][j][2] = signals.m_cameraz[hp];

      //  Set up material matrix for probability calculation.
      //  "Layer type" and "material" (Si, CdTe bottom, CdTe side)
      //  are the same thing.
      //
      mat[k][j] = signals.m_layer_type[hp];

      //  Set up CAMERAXYZ errors to match the material matrix.
      //
      camCoordErrors(signals.m_layer[hp], signals.m_layer_type[hp], geom,
                       delta_r[k][j]);
 
      //  Set up stepwise summed energy.
      //
      ee[k][j] = 0.0;
      var_ee[k][j] = 0.0;
     
      for (int j1=j; j1<m; ++j1) {  // j1: Step through hits to be summed
        int p1 = pr[m*k + j1];      // Permuted index for j1
        ee[k][j] += hits.m_epiarray[p1];
        var_ee[k][j] += std::pow(hits.m_deltaepiarray[p1], 2);
        AH_DEBUG << "Accum ee[" << k << "][" << j << "] = " << ee[k][j] << ";  " <<   "var_ee[" << k << "][" << j << "] = " << var_ee[k][j] << "; p1 = " << p1 << std::endl;
      }
      delta_ee[k][j] = std::sqrt(var_ee[k][j]);
    }

    //  Might be needed for escape energy calculation.  This needs to be done
    //  here in case the G test is skipped.
    //
    if (m >= 3) {
      cos_tht_g_m_minus_2[k] = computeCosThetaG(r[k][m-3], r[k][m-2], r[k][m-1]);
      AH_DEBUG << "k = " << k << "; cos_tht_g_m_minus_2[k] = " << cos_tht_g_m_minus_2[k] << std::endl;
    }
  }

  // Loop for narrowing down the list of possible sequences.  
  // This loop has at most two iterations for any given input row:
  //
  //  (1) without escape energy
  //  (2) with escape energy, only if necessary
  // 
  // This operation has been structured using a
  // for loop with a hardcoded maximum of 2 iterations.
  // The escape energy calculation is coded at the 
  // beginning of the loop but is only executed for the second
  // iteration.  escape_flag is set to false throughout the first
  // iteration and to true throughout the second iteration.
  //
  for (int iter=0; iter<2; ++iter) {

    //  HEAD_OF_ESCAPE_LOOP in TRF

    int num_f_good = 0;             // Number of permutations passing F test
    int num_g_good = 0;             // Number of permutations passing F & G tests
    int save_k = 0;                 // Save the index of a permutation

    escape_flag = iter > 0;         // Flag for second time through loop

    if (escape_flag) {
      
      if (m < 3) {
        std::stringstream ss;
        ss.str("");
        for (int i=0; i < hxisgdevtid::SGD_SIZE_RECO_STATUS; ++i) {
          ss << (int)outrow.m_reco_status[i] << " ";
        }
        AH_ERR << "OCCURRENCE_ID = " << outrow.m_occurrence_id << std::endl;
        AH_ERR << "RECO_STATUS   = " << ss.str() << std::endl;
        AH_THROW_LOGIC("Escape energy calc attempted with fewer than 3 hits");
      }

      AH_DEBUG << "Second round" << std::endl;
      bool calc_ok = computeEscapeEnergy (ee, var_ee, delta_ee, 
        cos_tht_g_m_minus_2, n_perm, m, mat, fluor_table, escape_en);
      if (!calc_ok) {
        setRecoStatusBit(outrow, RECO_SINGULARITY_IN_ESCAPE_CALC);
        sgdevtidlib::setOutputEventNull(outrow);
        AH_DEBUG << "Finished: singularity in escape energy calculation (RECO_SINGULARITY_IN_ESCAPE_CALC)" << std::endl;
        reco_complete = true;
        return;
      }
      //
      //  Specific escape energy flags in RECO_STATUS are set below, depending on
      //  the step that fails.  This general one is set here.
      setRecoStatusBit(outrow, RECO_PERFORM_ESCAPE_CALC);
    }

    //  Compute quantities to test for unphysical sequences.
    //  1)  |cos theta_Compton| should be less than or equal to 1.

    //  Quantity F in Ichinohe presentation and TRF
    
    //  m is the correct loop limit; one less
    //  than number of hits is computed
    //  me = mass of electron
    //  c = speed of light  
    //  MEC2 = me*c*c  should be computed as const up above
    //  Watch units!  We are in Kev.

    //  k steps through the permutations for the given m.
    //
    for (int k=0; k<n_perm; ++k) {

      //  Compute the F values, if there are 2, 3, or 4 hits.
      //  j steps through the hits.
      //
      for (int j=0; j<m-1; ++j) {

        //  Temporary variables to clarify the formulae.
        //
        int p = pr[m*k + j];               // Permuted index for j.
        double ej = hits.m_epiarray[p];      // Individual hit energy (not changed by escape).
        double dj = hits.m_deltaepiarray[p]; // Uncertainty in hit energy.
        double e0 = ee[k][j];              // E value.
        double e1 = ee[k][j+1];            // E value.
        double d1 = delta_ee[k][j+1];      // E uncertainty.
        double cos_theta_k_j=0.0;           // Cos(theta_k) for current J value.

        AH_DEBUG << "e0 = " << e0 << std::endl;
        AH_DEBUG << "e1 = " << e1 << std::endl;
        AH_DEBUG << "ej = " << ej << std::endl;
        AH_DEBUG << "d1 = " << d1 << std::endl;
        AH_DEBUG << "dj = " << dj << std::endl;
       
        // calculate cos(theta k); the value is used in the G test, but range-checked here in the F test
        cos_theta_k_j = 1.0 - MEC2/e1 + MEC2/e0;
    
        //  F and propagated uncertainty in F
        //
        f[k][j] = 2.0*e1*e1 + 2.0*e1*ej - ej*MEC2;
        delta_f[k][j]=std::sqrt(std::pow((4.*e1+2.*ej)*d1,2)+std::pow((2.*e1-MEC2)*dj,2));

        //  Does test pass for each element?
        //
        //  If cos(theta_k) < -1 then Compton scattering is unphysical => F test fails
        //  If escape energy < 0.0 then Compton scattering is unphysical => F test fails
        check_f[k][j] = (escape_en[k] >= 0.0) && (f[k][j]+par.a*delta_f[k][j] >= 0.0);
        AH_DEBUG << "                k   j     cos(theta_k)           F                 delta-F" << std::endl;
        std::stringstream msg;
        if (escape_flag)
          msg << "--FTEST_ESC: ";
        else
          msg << "--FTEST:     ";
        msg << std::setw(4) << k
            << std::setw(4) << j
            << std::setw(20) << std::setprecision(12) << cos_theta_k_j
            << std::setw(20) << std::setprecision(12) << f[k][j]
            << std::setw(20) << std::setprecision(12) << delta_f[k][j];
        if (check_f[k][j])
          msg << "  PASSES" << std::endl;
        else
          msg << "  FAILS" << std::endl;
        AH_DEBUG << msg.str();
      }
      
      //  Does test pass for every element of the permutation?
      //
      test_f[k] = true;   // A logical-and combination, so init to true
      for (int j=0; j<m-1; ++j) {
        test_f[k] = test_f[k] && check_f[k][j];
      } 
      if (test_f[k])
        AH_DEBUG << "F test with k = " << k << " PASSES" << std::endl;
      else
        AH_DEBUG << "F test with k = " << k << " FAILS" << std::endl;
    }

    //  Count permutations passing the F test.
    //
    num_f_good = 0;
    for (int k=0; k<n_perm; ++k) {
      if (test_f[k]) {
        ++num_f_good;
        save_k = k;
      }
    }

    //  At this point, the F test has been run on all the permutations.
    //
    //  The following if-block contains the G test.
    //
    //  For the first time through the "iter" loop (iter=0, escape_flag=false), 
    //  the following cases are possible:
    //
    //    num_f_good       m     action
    //
    //        >0           >2    do the G test
    //        >1            2    G test not possible; use probability table
    //         1            2    accept the event
    //         0            2    skip occurrence, go to next one
    //         0           >2    do escape energy calculation, redo F test
    //
    //  For the second time through the "iter" loop (iter=1, escape_flag=true), 
    //  the escape energy has already been used and cannot be used again, so 
    //  that the following cases are possible:
    //
    //    num_f_good       m     action
    //
    //        >0           >2    do the G test
    //         0          any    skip occurrence, go to next one
    //    These cases won't occur but for now still get tested:
    //        >1            2    G test not possible; use probability table
    //         1            2    accept the event
    //

    //  Build extra output row.
    //
    AH_DEBUG << "After F test: m = " << m << "; n_perm = " << n_perm << std::endl;
    if (par.extrainfo) {
      //AH_DEBUG << "Filling extraoutrow" << std::endl;
      extraoutrow.m_numsignal=signals.m_nsignal;
      extraoutrow.m_numperm = n_perm;
      extraoutrow.m_escape_flag = (escape_flag?1:0);
      //AH_DEBUG << "Filling extraoutrow: n_perm = " << n_perm << std::endl;
      for (int i=0; i < n_perm; ++i) {
        for (int j=0; j < m; ++j) {
          //AH_DEBUG << "Filling extraoutrow arrays: i = " << i << ";  j = " << j << std::endl;
          extraoutrow.m_e[i*m+j] = ee[i][j];
          extraoutrow.m_delta_e[i*m+j] = delta_ee[i][j];
          extraoutrow.m_f[i*m+j] = f[i][j];
          extraoutrow.m_delta_f[i*m+j] = delta_f[i][j];
          extraoutrow.m_check_f[i*m+j] = (check_f[i][j]?1:0);
        }
        //AH_DEBUG << "Filling extraoutrow arrays: i = " << i << std::endl;
        extraoutrow.m_test_f[i] = (test_f[i]?1:0);
      }
      //AH_DEBUG << "Setting extraoutrow lengths" << std::endl;
      extraoutrow.num_e = n_perm*m;
      extraoutrow.num_delta_e = n_perm*m;
      extraoutrow.num_f = n_perm*m;
      extraoutrow.num_delta_f = n_perm*m;
    }

    if (num_f_good == 0) {
      AH_DEBUG << "F test failed for all permutations..." << std::endl;
      if (m == 2) {   //  If escape_flag, m is already greater than 2
        AH_DEBUG << "...m=2" << std::endl;
        setRecoStatusBit(outrow, RECO_ALL_BAD_F_M_EQ_2);
        sgdevtidlib::setOutputEventNull(outrow);
        AH_DEBUG << "Finished: all F tests failed with m=2 (RECO_ALL_BAD_F_M_EQ_2)" << std::endl;
        reco_complete = true;
        return;
        //  in case of num_f_good > 1, G test will be automatically skipped below
        //  because of m>2 condition
      } else {  //  m==3 || m==4
        AH_DEBUG << "...m=3 or 4; escape_flag = " << escape_flag << std::endl;
        if (escape_flag) {
          setRecoStatusBit(outrow, RECO_ALL_BAD_F_M_GT_2);
          sgdevtidlib::setOutputEventNull(outrow);
          AH_DEBUG << "Finished: all tests failed in escape iteration (RECO_ALL_BAD_F_M_GT_2)" << std::endl;
          reco_complete = true;
          return;
        } else {
          //  The calculation is about to be redone, so write the existing
          //  extra output row, and partially reinitialize.
          //
          AH_DEBUG << "Perform escape iteration from F" << std::endl;
          if (par.extrainfo) {
            AH_DEBUG << "Writing extra output row" << std::endl;
            ahfits::writeRow(fpextra);
            ahfits::nextRow(fpextra);
            extraoutrow.m_row_count++;
            partialReInitExtraOutputData(extraoutrow);
          }
          setRecoStatusBit(outrow, RECO_GOTO_ESCAPE_CALC_VIA_F_FAILURE);
          continue;   // to the redo of the F and G tests
        }
      }
    }
    if (num_f_good == 1 && m == 2) {
      AH_DEBUG << "F test succeeded for only 1 permutation with two hits" << std::endl;

      // Save selected permutation index.
      //
      hits.m_best_k=save_k;

      //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
      //
      getHitSequence(signals, hits, pr, save_k, escape_flag, sequence_type);
      
      //  Fill in CAMERAX, CAMERAY, CAMERAZ.
      //
      buildOutputRowFirstHit(signals, hits, pr, save_k, escape_en[save_k],outrow);

      setRecoStatusBit(outrow, RECO_ONE_SEQUENCE_REMAINING_F);
      AH_DEBUG << "Finished: one two-hit sequence remaining after F tests (RECO_ONE_SEQUENCE_REMAINING_F)" << std::endl;
      reco_complete = true;
      return;
    }

    // This point is reached if num_f_good > 1.
    //
    // Initialize G test results with F test results (no need to perform
    // G test when F test has failed)
    for (int k=0; k<n_perm; ++k) test_g[k] = test_f[k];

    AH_DEBUG << "At least one F test succeeded..." << std::endl;

    // For m == 2 this calculation is impossible.
    //
    if (m == 2) {  
      AH_DEBUG << "m=2; skipping G test" << std::endl;
    } else {
      AH_DEBUG << "...m=3 or 4" << std::endl;

      //  Test for unphysical sequences.
      //
      //  2)  Are geometric and kinematic scattering angles consistent?
      //      Geometric angle is from coordinates of hits.
      //      Kinematic angle is from Compton formula.
      //
      //  Quantity G in Ichinohe presentation.
      //
      //  This can't be done unless there are at least 3 hits (because
      //  you need an angle).
      //
      for (int k=0; k<n_perm; ++k) {

        // Skip G test if F test has failed.
        //
        if (!test_f[k]) {
          AH_DEBUG << "G test with k = " << k << " FAILS  (since F test failed)" << std::endl;
          if (escape_flag)
            AH_DEBUG << "--GTEST_ESC_SKIP: " << k << std::endl;
          else
            AH_DEBUG << "--GTEST_SKIP: " << k << std::endl;
          continue;
        }

        //  For m=3, j is only 0.
        //  For m=4, j is either 0 or 1.
        //
        for (int j=0; j<m-2; ++j) {

          //  Temporary variable used below to store the variance of one hit.
          //
          double var_epi = 0.0;

          //  Stored in middle point of the three points, per TRF.
          //
          cos_theta_g[k][j+1] = computeCosThetaG(r[k][j], r[k][j+1], r[k][j+2]);
          if (par.delgmethod == sgdevtidlib::e_ANALYTIC) {
            delta_cos_theta_g[k][j+1]=computeDeltaCosThetaG_analytic(r[k][j], r[k][j+1], r[k][j+2], 
                                        delta_r[k][j], delta_r[k][j+1], delta_r[k][j+2]); 
          } else {
            delta_cos_theta_g[k][j+1]=computeDeltaCosThetaG_corner(r[k][j], r[k][j+1], r[k][j+2], 
                                        delta_r[k][j], delta_r[k][j+1], delta_r[k][j+2]);
          }

          cos_theta_k[k][j+1] = 1.0 - MEC2/ee[k][j+2] + MEC2/ee[k][j+1];

          var_epi = var_ee[k][j+1] - var_ee[k][j+2];    // Variance of hit [j+1] in permutation [k].
          delta_cos_theta_k[k][j+1] = MEC2*
            sqrt( std::pow( delta_ee[k][j+2]*std::pow(ee[k][j+2], -2) - std::pow(ee[k][j+1], -2), 2 )
                + var_epi*std::pow( std::pow(ee[k][j+1], -2), 2) );

          g[k][j+1] = cos_theta_g[k][j+1] - cos_theta_k[k][j+1];
          delta_g[k][j+1] = 
            std::sqrt( std::pow(delta_cos_theta_g[k][j+1], 2)
                     + std::pow(delta_cos_theta_k[k][j+1], 2) );
          g_test_limit = par.b*delta_g[k][j+1];

          // If Cos(theta_k) < -1: then Compton process cannot happen for these energies => G test fails.
          //
          if ((-g_test_limit < g[k][j+1]) && (g[k][j+1] < g_test_limit)) {
            check_g[k][j+1] = true;
          } else {
            check_g[k][j+1] = false;
          }

          AH_DEBUG << "                k  j+1    cos(theta_k)        cos(theta_g)               G                 delta-G" << std::endl;
          std::stringstream msg;
          if (escape_flag)
            msg << "--GTEST_ESC: ";
          else
            msg << "--GTEST:     ";
          msg << std::setw(4) << k
              << std::setw(4) << j+1
              << std::setw(20) << std::setprecision(12) << cos_theta_k[k][j+1]
              << std::setw(20) << std::setprecision(12) << cos_theta_g[k][j+1]
              << std::setw(20) << std::setprecision(12) << g[k][j+1]
              << std::setw(20) << std::setprecision(12) << delta_g[k][j+1];
          if (check_g[k][j+1])
            msg << "  PASSES" << std::endl;
          else
            msg << "  FAILS" << std::endl;
          AH_DEBUG << msg.str();
        }  

        for (int j=0; j<m-2; ++j) {
          test_g[k] = test_g[k] && check_g[k][j+1];
        }
        if (test_g[k])
          AH_DEBUG << "G test with k = " << k << " PASSES" << std::endl;
        else
          AH_DEBUG << "G test with k = " << k << " FAILS" << std::endl;
      }  // end loop k (permutations)

      //  Build extra output row.
      //
      if (par.extrainfo) {

        for (int i=0; i < n_perm; ++i) {
          for (int j=0; j < m; ++j) {
            extraoutrow.m_g[i*m+j] = g[i][j];
            extraoutrow.m_delta_g[i*m+j] = delta_g[i][j];
            extraoutrow.m_cos_theta_g[i*m+j] = cos_theta_g[i][j];
            extraoutrow.m_delta_cos_theta_g[i*m+j] = delta_cos_theta_g[i][j];
            extraoutrow.m_cos_theta_k[i*m+j] = cos_theta_k[i][j];
            extraoutrow.m_delta_cos_theta_k[i*m+j] = delta_cos_theta_k[i][j];
            extraoutrow.m_g[i*m+j] = g[i][j];
            extraoutrow.m_delta_g[i*m+j] = delta_g[i][j];
            extraoutrow.m_check_g[i*m+j] = (check_g[i][j]?1:0);
          }
          extraoutrow.m_test_g[i] = (test_g[i]?1:0);
        }
        extraoutrow.num_g = n_perm*m;
        extraoutrow.num_delta_g = n_perm*m;
        extraoutrow.num_cos_theta_g = n_perm*m;
        extraoutrow.num_delta_cos_theta_g = n_perm*m;
        extraoutrow.num_cos_theta_k = n_perm*m;
        extraoutrow.num_delta_cos_theta_k = n_perm*m;
        extraoutrow.num_g = n_perm*m;
        extraoutrow.num_delta_g = n_perm*m;
      }

      //  Count the permutations that pass
      //
      num_g_good = 0;
      for (int k=0; k<n_perm; ++k) {
        if (test_g[k]) {
          ++num_g_good;
          save_k = k;
        }
      }

      if (num_g_good == 0) {
        AH_DEBUG << "G test failed for all permutations..." << std::endl;
        if (escape_flag) {
          setRecoStatusBit(outrow, RECO_ALL_BAD_G);
          sgdevtidlib::setOutputEventNull(outrow);
          AH_DEBUG << "Finished: all G tests failed after escape calc (RECO_ALL_BAD_G)" << std::endl;
          reco_complete = true;
          return;
        } else {
          AH_DEBUG << "Perform escape iteration from G" << std::endl;
          //  The calculation is about to be redone, so write the existing
          //  extra output row, and partially reinitialize.
          //
          if (par.extrainfo) {
            AH_DEBUG << "Writing extra output row" << std::endl;
            ahfits::writeRow(fpextra);
            ahfits::nextRow(fpextra);
            extraoutrow.m_row_count++;
            partialReInitExtraOutputData(extraoutrow);
          }
          setRecoStatusBit(outrow, RECO_GOTO_ESCAPE_CALC_VIA_G_FAILURE);
          continue;   //  to calculation with escape energy included
        }
      }
      if (num_g_good == 1) {
        AH_DEBUG << "G test succeeded for 1 permutation" << std::endl;

        // Save selected permutation index
        hits.m_best_k=save_k;

        //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
        //
        getHitSequence(signals, hits, pr, save_k, escape_flag, sequence_type);

        //  Fill in CAMERAX, CAMERAY, CAMERAZ.
        //
        buildOutputRowFirstHit(signals, hits, pr, save_k, escape_en[save_k],outrow);

        setRecoStatusBit(outrow, RECO_ONE_SEQUENCE_REMAINING_G);
        AH_DEBUG << "Finished: one sequence remaining after G tests (RECO_ONE_SEQUENCE_REMAINING_G)" << std::endl;
        reco_complete = true;
        return;
      }
      if (num_g_good > 1) AH_DEBUG << "Multiple G tests succeeded..." << std::endl;
    }
    
    //
    //  Step 3.  Abandon low-probability sequences.
    //
    for (int k=0; k<n_perm; ++k) {
      
      //  m determines which elements of mat[k][*] are used.
      //
      prob[k] = sgdprobseq::lookupProbability(mat[k], escape_flag, probseq_table);
      if (m == 2 && test_f[k]) {
        // prob[k] = probability (mat[k,0],mat[k,1]) from CALDB
        test_prob[k] = prob[k] > par.probaccept2;
      } else if (m == 3 && test_g[k]) {    //  test_g includes test_f
        // prob[k] = probability (mat[k,0],mat[k,1],mat[k,3]) from CALDB
        test_prob[k] = prob[k] > par.probaccept3;
      } else if (m == 4 && test_g[k]) {    //  test_g includes test_f
        // prob[k] = probability (mat[k,0],mat[k,1],mat[k,3],mat[k,4]) from CALDB
        test_prob[k] = prob[k] > par.probaccept4;
      }
      if (test_f[k] && (m==2 || test_g[k])) {
        std::stringstream msg;
        msg << "--PROBSEQ: " << std::setw(4) << k << "   " << std::setw(20) << std::setprecision(15) << prob[k];
        if (test_prob[k])
          msg << "  ACCEPT" << std::endl;
        else
          msg << "  REJECT" << std::endl;
        AH_DEBUG << msg.str();
      }
    }

    //  Build extra output row.
    //
    if (par.extrainfo) {

      for (int i=0; i < n_perm; ++i) {
        extraoutrow.m_test_prob[i] = (test_prob[i]?1:0);
        extraoutrow.m_prob[i] = prob[i];
      }
      extraoutrow.num_test_prob = n_perm*m;
      extraoutrow.num_prob = n_perm;
    }

    num_test_prob = 0;
    for (int k=0; k<n_perm; ++k) {
      if (test_prob[k]) {
        ++num_test_prob;
        save_k = k;
      }
    }

    if (num_test_prob == 1) {
      //  One sequence left; done with this occurrence.
      //

      // Save selected permutation index
      hits.m_best_k=save_k;

      setRecoStatusBit(outrow, RECO_ONE_SEQUENCE_REMAINING_PROBSEQ);
      AH_DEBUG << "Finished: one sequence remaining (k=" << save_k << ") after rejecting for low probability (RECO_ONE_SEQUENCE_REMAINING_PROBSEQ)" << std::endl;
      reco_complete = true;

      //  Fill in hit sequence description (SEQUENCE and MECHANISM1).
      //
      getHitSequence(signals, hits, pr, save_k, escape_flag, sequence_type);

      //  Fill in CAMERAX, CAMERAY, CAMERAZ.
      //
      buildOutputRowFirstHit(signals, hits, pr, save_k, escape_en[save_k],outrow);
      return;
    }

    if (num_test_prob > 1) {
      //  Multiple sequences have passed the F, G, and PROBSEQ tests.  This
      //  means that escape energy calc is unneeded and the program
      //  can go on to the tie break.
      return;
    }

    if (num_test_prob == 0) {
      if (m == 2) {
        setRecoStatusBit(outrow, RECO_ALL_LOW_PROB_M_EQ_2);
        sgdevtidlib::setOutputEventNull(outrow);
        AH_DEBUG << "Finished: all sequences have low probability with m=2 (RECO_ALL_LOW_PROB_M_EQ_2)" << std::endl;
        reco_complete = true;
        return;
      }
      if (escape_flag) {
        setRecoStatusBit(outrow, RECO_ALL_LOW_PROB_M_GT_2);
        sgdevtidlib::setOutputEventNull(outrow);
        AH_DEBUG << "Finished: all sequences have low probability with m=3 in escape iteration (RECO_ALL_LOW_PROB_M_GT_2)" << std::endl;
        reco_complete = true;
        return;
      } else {
        //  The calculation is about to be redone, so write the existing
        //  extra output row, and partially reinitialize.
        //
        if (par.extrainfo) {
          AH_DEBUG << "Writing extra output row" << std::endl;
          ahfits::writeRow(fpextra);
          ahfits::nextRow(fpextra);
          extraoutrow.m_row_count++;
          partialReInitExtraOutputData(extraoutrow);
        }
        setRecoStatusBit(outrow, RECO_GOTO_ESCAPE_CALC_VIA_PROBSEQ_FAILURE);
        continue;   //  to calculation using escape energy
      }
    }
  } // end loop iter
}

// ****************************************************************************

void performTieBreaking (Par& par, RowSignals& signals, Hits& hits,
  hxisgdevtid::remap::GeomKeywords& geom, 
  double prob[], bool test_f[], bool test_g[], double g[][MAX_NUMHITS],
  bool escape_flag, double escape_en[MAX_PERM], OutputRowData& outrow,
  ExtraOutputRowData& extraoutrow, SeqInfo& sequence_type, bool& reco_complete) {

  double BADFOM=-999.;       // very large negative FOM to indicate bad value

  double fom[MAX_PERM];
  double best_fom = 0.0;
  double cos_theta_g[MAX_PERM], delta_cos_theta_g[MAX_PERM]; 
  double cos_theta_k[MAX_PERM], delta_cos_theta_k[MAX_PERM];
  double g0[MAX_PERM], delta_g0[MAX_PERM]; 
  double offaxis=0.0;                // |theta_k-theta_g|
  double ee[MAX_PERM][MAX_NUMHITS], var_ee[MAX_PERM][MAX_NUMHITS], delta_ee[MAX_PERM][MAX_NUMHITS];     // E_i  for each permutation

  int m = hits.m_m;         // Number of hits

  //  Access the correct permutation table for m.
  //
  int n_perm = 0;
  const int* pr = selectPermArray(m, n_perm);

  // initialize arrays
  for (int k=0; k<MAX_PERM; ++k) {
    for (int j=0; j<MAX_NUMHITS; ++j) {
      ee[k][j] = 0.0;
      var_ee[k][j] = 0.0;
      delta_ee[k][j] = 0.0;
    }
    cos_theta_g[k]=0.;
    delta_cos_theta_g[k]=0.;
    cos_theta_k[k]=0.;
    delta_cos_theta_k[k]=0.;
    g0[k]=0.;
    delta_g0[k]=0.;
  }

  // initialize FOM values
  // this is necessary since some k permutations are skipped if the F or G test failed
  for (int k=0; k<n_perm; ++k) {
    fom[k]=BADFOM;
  }

  // calculate E_i = e_i + e_{i+1} + ...
  for (int k=0; k<n_perm; ++k) {    // k: Step through permutations
    for (int j=0; j<m; ++j) {       // j: Step through members of each permutation
      ee[k][j] = escape_en[k];      // escape_en == 0.0 if no escape
      for (int j1=j; j1<m; ++j1) {  // j1: Step through hits to be summed
        int p1 = pr[m*k + j1];      // Permuted index for j1
        ee[k][j] += hits.m_epiarray[p1];
        var_ee[k][j] += std::pow(hits.m_deltaepiarray[p1], 2);
      }
      delta_ee[k][j] = std::sqrt(var_ee[k][j]);
    }
  }

  //
  //  Step 4.  Tie-breaking to choose only one sequence.
  //
  AH_DEBUG << "Calculate FOM for each sequence; nperm = " << n_perm << std::endl;
  for (int k=0; k < n_perm; ++k) {
    AH_DEBUG << "k, m, test_f, test_g = " << k << ", " << m << ", " << test_f[k] << ", " << test_g[k] << std::endl;
    if ((k == hits.m_best_k) || (m == 2 && test_f[k]) || (m > 2 && test_g[k]) ) {   // test_g includes test_f.
      double probk=0.0;             // Probability of each permutation.
      double dx1, dy1, dz1;         // Difference between hits 0 and 1 in CAMERA coordinates.
      double dx2, dy2, dz2;         // Difference between hits 1 and 2 in CAMERA coordinates.
      double norm1;                 // Distance between hits 0 and 1 in CAMERA coordinates.
      double norm2;                 // Distance between hits 1 and 2 in CAMERA coordinates.
      double r0[3];                 // r0 = boresight = "hit 0" for this purpose.
      double r1[3], r2[3];          // r1, r2 = coords of hits 1 and 2 in CAMERA coordinates.
      double delta_r0[3], delta_r1[3], delta_r2[3];  // Errors in r0, r1, r2.
      int p1 = pr[m*k];             // Subscript of hit 1 in hits arrays.
      int p2 = pr[m*k + 1];         // Subscript of hit 2 in hits arrays.
      int hp1 = hits.m_hitarray[p1];  // Subscript of hit 1 in signals arrays.
      int hp2 = hits.m_hitarray[p2];  // Subscript of hit 2 in signals arrays.
      double delta_epi1 = hits.m_deltaepiarray[p1];  // Error in epi for hit 1.
      r0[0] = 0.0;
      r0[1] = 0.0;
      r0[2] = par.distz;
      r1[0] = signals.m_camerax[hp1];
      r1[1] = signals.m_cameray[hp1];
      r1[2] = signals.m_cameraz[hp1];
      r2[0] = signals.m_camerax[hp2];
      r2[1] = signals.m_cameray[hp2];
      r2[2] = signals.m_cameraz[hp2];
      delta_r0[0] = 0.0;
      delta_r0[1] = 0.0;
      delta_r0[2] = 0.0;

      // Get the correct CAMERA[XYZ] errors for the layer that each
      // hit is assigned to.
      //
      camCoordErrors(signals.m_layer[hp1], signals.m_layer_type[hp1], geom,
                       delta_r1);
      camCoordErrors(signals.m_layer[hp2], signals.m_layer_type[hp2], geom,
                       delta_r2);

      // Compute cos theta_g with a dot product.
      //
      dx1 = r1[0] - r0[0];
      dy1 = r1[1] - r0[1];
      dz1 = r1[2] - r0[2];
      dx2 = r2[0] - r1[0];
      dy2 = r2[1] - r1[1];
      dz2 = r2[2] - r1[2];
      norm1 = sqrt(dx1*dx1 + dy1*dy1 + dz1*dz1);
      norm2 = sqrt(dx2*dx2 + dy2*dy2 + dz2*dz2);
      cos_theta_g[k] = (dx1*dx2 + dy1*dy2 + dz1*dz2)/(norm1*norm2);

      // Compute error in cos theta_g.
      //
      if (par.delgmethod == sgdevtidlib::e_ANALYTIC) {
        delta_cos_theta_g[k]=computeDeltaCosThetaG_analytic(r0, r1, r2, delta_r0, delta_r1, delta_r2); 
      } else {
        delta_cos_theta_g[k]=computeDeltaCosThetaG_corner(r0, r1, r2,  delta_r0, delta_r1, delta_r2);
      }
      
      // Compute cos theta_k using the Compton formula.
      //
      cos_theta_k[k] = 1.0 - MEC2/ee[k][1] + MEC2/ee[k][0];

      // Compute error in cos theta_k using the formula resulting from propagation
      // of error.
      //
      delta_cos_theta_k[k] = MEC2*
        sqrt( std::pow( delta_ee[k][1]*std::pow(ee[k][1], -2) - std::pow(ee[k][0], -2), 2 )
            + std::pow( delta_epi1*std::pow(ee[k][0], -2), 2) );

      g0[k] = cos_theta_g[k] - cos_theta_k[k];
      double tmp_thetak=std::acos(std::max(-1.,cos_theta_k[k]));      
      offaxis=tmp_thetak-std::acos(cos_theta_g[k]);
      AH_DEBUG << "***  k = " << k << "  ***" << std::endl;
      AH_DEBUG << "k,1,ee[k][1]: " << k << ", " << 1 << ", " << ee[k][1] << std::endl;
      AH_DEBUG << "k,0,ee[k][0]: " << k << ", " << 0 << ", " << ee[k][0] << std::endl;
      AH_DEBUG << "cos_theta_g, cos_theta_k: " << cos_theta_g[k] << ", " << cos_theta_k[k] << std::endl;
      AH_DEBUG << "theta_g, theta_k: " << std::acos(cos_theta_g[k]) << ", " << tmp_thetak << std::endl;
      AH_DEBUG << "delta-theta: " << offaxis << std::endl;
      delta_g0[k] = delta_cos_theta_g[k]*std::sqrt(1.0-cos_theta_g[k]*cos_theta_g[k])
                  - delta_cos_theta_k[k]*std::sqrt(1.0-cos_theta_k[k]*cos_theta_k[k]);  // sine identity

      // Sequence probability
      probk = prob[k];

      // note: from Ichinohe-san: 0-term uses delta-theta and other terms use delta-cos-theta
      switch (m) {
        case 2:
          fom[k] = (1. - std::abs(offaxis)/par.paraoffset0)*par.weight0 + probk*par.weight3;
          break;
        case 3:
          fom[k] = (1. - std::abs(offaxis)/par.paraoffset0)*par.weight0 +
                   (1. - std::abs(g[k][1])/par.paraoffset1)*par.weight1 +
                   probk*par.weight3;
          break;
        case 4:
          fom[k] = (1. - std::abs(offaxis)/par.paraoffset0)*par.weight0 +
                   (1. - std::abs(g[k][1])/par.paraoffset1)*par.weight1 +
                   (1. - std::abs(g[k][2])/par.paraoffset2)*par.weight2 +
                   probk*par.weight3;
          break;
      }
      AH_DEBUG << "--FOM: " << k << "    " << fom[k] << std::endl;
    }
  }

  // choose best sequence (best_k), if not already filled)
  if (hits.m_best_k < 0) {
    // get best FOM value
    best_fom = BADFOM;
    for (int k=0; k<n_perm; ++k) {
      if (fom[k] > best_fom) best_fom = fom[k];
    }
    if (best_fom == BADFOM) AH_THROW_LOGIC("all FOM values are invalid; something wrong....");
  
    // choose sequence with best FOM; if more than one sequence has the FOM, 
    // randomly select sequence to use
    std::vector<int> poss;
    for (int k=0; k<n_perm; ++k) {
      if (fom[k] == best_fom) poss.push_back(k);
    }
    if (poss.size() == 1) {
      hits.m_best_k=poss[0];
    } else {
      hits.m_best_k=poss[ahgen::getRandomInt(0,poss.size()-1)];
      extraoutrow.m_rand_fom=1;
      AH_DEBUG << "More than one sequence has the best FOM value" << std::endl;
      AH_DEBUG << "Randomly selected sequence " << hits.m_best_k << " from list of " << poss.size() << " sequences" << std::endl;
    }
    AH_DEBUG << "Tie-breaking selected sequence k = " << hits.m_best_k << " with FOM = " << best_fom << std::endl;
    AH_DEBUG << "--TIEBREAK_SEQ: " << hits.m_best_k << std::endl;
  
    // set RECO_STATUS bit and set up output row, ONLY if reco_complete is false coming into this function
    setRecoStatusBit(outrow, RECO_ONE_SEQUENCE_REMAINING_TIE_BREAK);
    buildOutputRowFirstHit(signals, hits, pr, hits.m_best_k, escape_en[hits.m_best_k],outrow);
    AH_DEBUG << "Finished: one sequence remaining after tie break (RECO_ONE_SEQUENCE_REMAINING_TIE_BREAK)" << std::endl;
    reco_complete = true;

    //  Build extra output row.
    //
    if (par.extrainfo) {
  
      for (int i=0; i < n_perm; ++i) {
        extraoutrow.m_cos_theta_g_0[i] = cos_theta_g[i];
        extraoutrow.m_delta_cos_theta_g_0[i] = delta_cos_theta_g[i];
        extraoutrow.m_cos_theta_k_0[i] = cos_theta_k[i];
        extraoutrow.m_delta_cos_theta_k_0[i] = delta_cos_theta_k[i];
        extraoutrow.m_g_0[i] = g0[i];
        extraoutrow.m_delta_g_0[i] = delta_g0[i];
        extraoutrow.m_fom[i] = fom[i];
      }
      extraoutrow.num_cos_theta_g_0 = n_perm;
      extraoutrow.num_delta_cos_theta_g_0 = n_perm;
      extraoutrow.num_cos_theta_k_0 = n_perm;
      extraoutrow.num_delta_cos_theta_k_0 = n_perm;
      extraoutrow.num_g_0 = n_perm;
      extraoutrow.num_delta_g_0 = n_perm;
      extraoutrow.num_fom = n_perm;
    }
  }

  //  Fill in sequence description.
  //
  getHitSequence(signals, hits, pr, hits.m_best_k, escape_flag, sequence_type);

  //  Fill in primary output row.
  //
  double tmp_thetak=std::acos(std::max(-1.,cos_theta_k[hits.m_best_k]));
  outrow.m_offaxis_null=0;
  outrow.m_offaxis = RADTODEG*(tmp_thetak-std::acos(cos_theta_g[hits.m_best_k]));
  outrow.m_compton_th_null=0;
  outrow.m_compton_th=RADTODEG*tmp_thetak;
  outrow.m_delcompton[0]=g[hits.m_best_k][1];
  outrow.m_delcompton[1]=g[hits.m_best_k][2];
}

// ****************************************************************************

bool computeEscapeEnergy (double ee[MAX_PERM][MAX_NUMHITS], 
  double var_ee[MAX_PERM][MAX_NUMHITS], double delta_ee[MAX_PERM][MAX_NUMHITS], 
  double cos_tht_g_m_minus_2[MAX_PERM], 
  int n_perm, int m, int mat[MAX_PERM][MAX_NUMHITS], 
  hxisgdevtid::fluor::DataType& fluor_table, double escape_en[MAX_PERM]) {
  //
  //  Equation for escape energy, from Ichinohe:
  //
  //  Let x = e[m-1] + e_esc
  //  Let A = ( 1 - cos(theta[m-2]) )/(m_e*c^2)
  //     (A has dimensions of 1/energy)
  //
  //  Then
  //                e[m-2] + x
  //     x =     ------------------
  //             1 + (e[m-2] + x)*A
  //
  //  This is a quadratic equation:
  //
  //     x + x*(e[m-2] + x)*A = e[m-2] + x
  //         x*(e[m-2] + x)*A = e[m-2]
  //     x*e[m-2]*A + x^2*A - e[m-2] = 0
  //     A*x^2 + A*e[m-2]*x - e[m-2] = 0
  //     x^2 + e[m-2]*x - e[m-2]/A = 0
  //
  //     x = 0.5*(-e[m-2] + sqrt(e[m-2]^2 + 4*e[m-2]/A))
  //
  //     NOTE 1: 1-cos(theta) >= 0
  //     therefore A >= 0
  //     So that the sqrt term >= e[m-2] and the plus sign is
  //     taken for the sqrt in the quadratic formula.
  //
  //     NOTE 2: x is undefined if cos(theta[m-2]) == 1.
  //     This corresponds to zero angle, which
  //     can't be handled.
  //

  AH_DEBUG << "             k             e_esc                  e_2                  aa                       x" << std::endl;
  for (int k=0; k < n_perm; ++k) {
    // double e_2 = epiarray[k][m-2];
    double e_2 = ee[k][m-2] - ee[k][m-1];
    double aa = ( 1.0 - cos_tht_g_m_minus_2[k] )/MEC2;
    double x = 0.5*(-e_2 + sqrt(e_2*e_2 + 4.0*e_2/aa));   // new energy of last signal
    double e_esc = x - ee[k][m-1];

    std::stringstream msg;
    msg << "--ESCAPE: "
        << std::setw(4) << k
        << std::setw(24) << std::setprecision(15) << e_esc
        << std::setw(24) << std::setprecision(15) << e_2
        << std::setw(24) << std::setprecision(15) << aa
        << std::setw(24) << std::setprecision(15) << x;

    // The coeff A above will blow up.
    if (-FLOAT_EPSILON < aa && aa < FLOAT_EPSILON) {
      msg << "    A=0 SKIP" << std::endl;
      AH_DEBUG << msg.str();
      return false;
    }
    msg << std::endl;
    AH_DEBUG << msg.str();

    //  Update E and delta(E).
    //
    double var_last_old=var_ee[k][m-1];   // original variance of last hit
    double var_last_new=0.0;              // new variance of last hit (computed below)
    escape_en[k]=e_esc;
    for (int j=0; j<m; ++j) ee[k][j] += e_esc;
    if (mat[k][m-1] == hxisgdevtid::SGD_SI_STACK) {
      var_last_new=fluor_table.m_sgd_errsi_a_sqr+
                   fluor_table.m_sgd_errsi_b_sqr*x+
                   fluor_table.m_sgd_errsi_c_sqr*x*x;
    } else if (mat[k][m-1] == hxisgdevtid::SGD_CDTE_BOTTOM) {
      var_last_new=fluor_table.m_sgd_errcdbtm_a_sqr+
                   fluor_table.m_sgd_errcdbtm_b_sqr*x+
                   fluor_table.m_sgd_errcdbtm_c_sqr*x*x;
    } else {
      var_last_new=fluor_table.m_sgd_errcdsid_a_sqr+
                   fluor_table.m_sgd_errcdsid_b_sqr*x+
                   fluor_table.m_sgd_errcdsid_c_sqr*x*x;
    }
    for (int j=0; j<m; ++j) {
      var_ee[k][j] += var_last_new-var_last_old;
      delta_ee[k][j] = std::sqrt(var_ee[k][j]);
    }

  }
  return true;
}

// ****************************************************************************

void getProbFOV(sgdprobfov::DataType& probfov_table,
  RowSignals& signals, Hits& hits, double escape_en[MAX_PERM],
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow) {

  // if reconstruction failed, set FOV probability to NULL
  if (outrow.m_pi_null == 1) {
    outrow.m_probability_null=1;
    return;
  }

  double ee_tot = 0.0;     // Total energy = E_0 (including escape energy if any)
  double prob_fov = 0.0;   // Probability that a photon is really in the FOV
  int m = hits.m_m;

  // map of parameter values for FOV lookup
  sgdprobfov::ParValues parvals;

  //  Access the correct permutation table for m.
  //
  int n_perm = 0;
  const int* pr = selectPermArray(m, n_perm);

  int p0 = pr[m*hits.m_best_k];            // Subscript of 1st hit in best_k sequence
  int p1 = pr[m*hits.m_best_k + 1];        // Subscript of 2nd hit in best_k sequence
  int hp0 = hits.m_hitarray[p0];              // Permuted signal index
  int hp1 = hits.m_hitarray[p1];              // Permuted signal index
  double dx1 = signals.m_camerax[hp1] - signals.m_camerax[hp0];  // CAMERAX diff between 1st two hits
  double dy1 = signals.m_cameray[hp1] - signals.m_cameray[hp0];  // CAMERAY diff between 1st two hits
  double dz1 = signals.m_cameraz[hp1] - signals.m_cameraz[hp0];  // CAMERAZ diff between 1st two hits
  double dist0 = std::sqrt(dx1*dx1 + dy1*dy1 + dz1*dz1);  // Distance (mm) between first two hit points
  double phi = std::atan2(dy1, dx1);                      // Azimuthal angle (radians)

  ee_tot = 0.0;
  for (int j=0; j < m; ++j) {
    ee_tot += hits.m_epiarray[j];         // Total energy
  }
  ee_tot+=escape_en[hits.m_best_k];       // add in escape energy

  outrow.m_distance0_null=0;
  outrow.m_distance0 = dist0;
  outrow.m_compton_ph_null=0;
  outrow.m_compton_ph = RADTODEG*phi;

  // Look up FOV probability
  parvals["ENE_TOTAL"]=ee_tot;
  parvals["COMPTON_TH"]=outrow.m_compton_th;
  parvals["COMPTON_PH"]=outrow.m_compton_ph;
  parvals["CAMERAX"]=signals.m_camerax[hp1];
  parvals["CAMERAY"]=signals.m_cameray[hp1];
  parvals["CAMERAZ"]=signals.m_cameraz[hp1];
  parvals["DISTANCE0"]=dist0;
  parvals["OFFAXIS"]=outrow.m_offaxis;
  prob_fov=sgdprobfov::lookupFOVProbability(parvals,probfov_table);

  outrow.m_probability_null=0;
  outrow.m_probability=prob_fov;

  AH_DEBUG << "                   ENE_TOTAL         COMPTON_TH        COMPTON_PH        CAMERAX           CAMERAY           CAMERAZ           DISTANCE0         OFFAXIS           PROBFOV" << std::endl;
  std::stringstream msg;
  msg << "--PROBFOV: "
      << std::setw(18) << std::setprecision(10) << ee_tot
      << std::setw(18) << std::setprecision(10) << outrow.m_compton_th
      << std::setw(18) << std::setprecision(10) << outrow.m_compton_ph
      << std::setw(18) << std::setprecision(10) << signals.m_camerax[hp1]
      << std::setw(18) << std::setprecision(10) << signals.m_cameray[hp1]
      << std::setw(18) << std::setprecision(10) << signals.m_cameraz[hp1]
      << std::setw(18) << std::setprecision(10) << dist0
      << std::setw(18) << std::setprecision(10) << outrow.m_offaxis
      << std::setw(18) << std::setprecision(10) << prob_fov
      << std::endl;
  AH_DEBUG << msg.str();

  return;
}

// ****************************************************************************

void assertNumHitsCorrect(RowSignals& signals, int m) {
  long chk_numhits=0;
  for (int i=0; i<signals.m_nsignal; ++i) {
    if (signals.m_merge_survivor[i]) ++chk_numhits;
  }
  if (chk_numhits != m) {
    std::stringstream msg;
    msg << "Inconsistency in counting hits; chk != m: " << chk_numhits << " != " << m;
    AH_THROW_LOGIC(msg.str());
  }
}

// ****************************************************************************

void getHitSequence(RowSignals& signals, Hits& hits,
  const int* pr, int ksel, bool escape_flag, SeqInfo& sequence_type) {

  //  sequence_type.sequence is a 4-element array.  Each element contains
  //  a code for the type of layer in which the hit occurs:  
  //  0=Si, 1=CdTe bottom, 2=CdTe side.  Unused elements contain -1.
  //
  //  sequence_type.mechanism1 is a std::string with a one-letter code for the
  //  physical mechanism of each hit:  C for Compton interaction, and P for
  //  photoabsorption.  These codes are followed by E is the escape
  //  energy calculation was done.

  // *** Note: the procedure for constructing 'sequence' is also performed
  //     in performFGProbTests() where the result is stored in mat[] for
  //     each possible sequence.  At this time, we have not factored out
  //     this algorithm.
  
  int m = hits.m_m;

  for (int i=0; i < MAX_NUMHITS; ++i) sequence_type.m_sequence[i] = -1;
  sequence_type.m_mechanism1 = "";

  for (int ihit=0; ihit < m ; ++ihit) {
    int isig = hits.m_hitarray[pr[m*ksel + ihit]];
    sequence_type.m_sequence[ihit] = signals.m_layer[isig]/100;
    if (ihit <= (m - 2)) {
      sequence_type.m_mechanism1 += "C";
    } else {           // ihit == (m - 1)
      sequence_type.m_mechanism1 += "P";
    }
  }
  if (escape_flag) {
    sequence_type.m_mechanism1 += "E";
  }
}

// ****************************************************************************

void getSequenceOneHit(RowSignals& signals, bool escape_flag, SeqInfo& sequence_type) {
  Hits hits;
  int nsignal = signals.m_nsignal;
  int pr[1] = {0};
  int ksel = 0;

  int hit=0, count=0;
  for (int i=0; i< nsignal; ++i) {
    if (signals.m_merge_survivor[i]) {
      hit = i;
      ++count;
    }
  }
  if (count > 1) AH_THROW_LOGIC ("getSequenceOneHit invoked for multiple hits");

  hits.m_m = 1;
  hits.m_hitarray[0] = hit;
  getHitSequence(signals, hits, pr, ksel, escape_flag, sequence_type);
}

// ****************************************************************************

void getSequenceOneHitNoEscape(RowSignals& signals, SeqInfo& sequence_type) {
  const bool local_escape_flag=false;
  getSequenceOneHit(signals, local_escape_flag, sequence_type);
}


// ****************************************************************************

void buildOutputRowFirstHit(RowSignals& signals, Hits& hits, const int* pr, 
     int ksel, double escape_en, OutputRowData& outrow) {

  // get total number of hits and location of first hit
  int m = hits.m_m;
  int isig = hits.m_hitarray[pr[m*ksel]];

  // set number of hits in output row
  setNumHits(hits,escape_en,outrow);

  // set the material type; 1 - Si, 2 - CdTe, 3 - multiple layers
  outrow.m_mattype_null=0;
  if (m == 1) {
    int layer=signals.m_layer[hits.m_hitarray[0]];
    if (hxisgdevtid::SGDLayerIsSi(layer))
      outrow.m_mattype=1;
    else  // CdTe
      outrow.m_mattype=2;
  } else {
    outrow.m_mattype=3;
  }

  // get totalepi
  double epitot=0.0;
  for (int i=0; i < signals.m_nsignal; i++) {
    if (!signals.m_flag[i]) continue;      // skip signals below threshold or NULL
    double toadd=signals.m_epi_current[i];
    if (toadd > 0.0) AH_DEBUG << "signal index i: " << i << "; add to EPITOT: " << toadd << std::endl;
    epitot+=toadd;
  }
    AH_DEBUG << "add to EPITOT (escape): " << escape_en << std::endl;
  epitot+=escape_en;

  //  Use permutation to subscript hitarray, which furnishes
  //  subscript for signals.
  //
  hxisgdevtid::convertEPIToPI("SGD",epitot,outrow.m_pi,outrow.m_pi_null);
  //convertEpiToSGDPI(epitot,outrow.m_pi,outrow.m_pi_null); 
  AH_DEBUG << "EPI -> PI  : " << epitot << " -> " << outrow.m_pi << std::endl;
  outrow.m_ene_total_null=0;
  outrow.m_ene_total=epitot;
  outrow.m_camerax = signals.m_camerax[isig];
  outrow.m_cameray = signals.m_cameray[isig];
  outrow.m_cameraz = signals.m_cameraz[isig];
}

// ****************************************************************************

void buildOutputRowOneHit(RowSignals& signals, OutputRowData& outrow) {
  Hits hits;
  int nsignal = signals.m_nsignal;
  int pr[1] = {0};
  int ksel = 0;

  int hit=0, count=0;
  for (int i=0; i< nsignal; ++i) {
    if (signals.m_merge_survivor[i]) {
      hit = i;
      ++count;
    }
  }
  if (count > 1) AH_THROW_LOGIC ("getCoordsOneHit invoked for multiple hits");
  hits.m_m=1;
  hits.m_hitarray[0] = hit;

  // note: 2nd to last argument gives an escape energy of zero 
  //       (escape energy is N/A for this function)
  buildOutputRowFirstHit(signals, hits, pr, ksel, 0.0, outrow);
}

// ****************************************************************************

void setOutputEventNull(OutputRowData& outrow) {
  outrow.m_pi_null = 1;
  outrow.m_ene_total_null=1;
  outrow.m_mattype_null=1;
  outrow.m_compton_th_null = 1;
  outrow.m_compton_ph_null = 1;
  outrow.m_distance0_null = 1;
  outrow.m_offaxis_null = 1;
  outrow.m_probability_null = 1;
  outrow.m_camerax_null = 1;
  outrow.m_cameray_null = 1;
  outrow.m_cameraz_null = 1;
  outrow.m_seq_hits_null=1;
}

// ****************************************************************************

const int* selectPermArray(int m, int& n_perm) {
  //  Select permutation table based on the number of
  //  hits (m).
  //
  const int* pr;
  switch (m) {
    case 2:
      n_perm = (sizeof(PERM_2)/sizeof(PERM_2[0]))/m;
      pr = PERM_2;
      break;
    case 3:
      n_perm = (sizeof(PERM_3)/sizeof(PERM_3[0]))/m;
      pr = PERM_3;
      break;
    case 4:
      n_perm = (sizeof(PERM_4)/sizeof(PERM_4[0]))/m;
      pr = PERM_4;
      break;
    default: AH_THROW_LOGIC("Number of hits (m) > 4 or < 2");
  }
return pr;
}

// ****************************************************************************

double computeCosThetaG(double ra[], double rb[], double rc[]) {

  //  Computes cosine of angle between vector rc-rb and vector ra-rb
  //
  double dx1 = rb[0] - ra[0];
  double dy1 = rb[1] - ra[1];
  double dz1 = rb[2] - ra[2];
  double dx2 = rc[0] - rb[0];
  double dy2 = rc[1] - rb[1];
  double dz2 = rc[2] - rb[2];
  double norm1 = std::sqrt(dx1*dx1 + dy1*dy1 + dz1*dz1);
  double norm2 = std::sqrt(dx2*dx2 + dy2*dy2 + dz2*dz2);
  return (dx1*dx2 + dy1*dy2 + dz1*dz2)/(norm1*norm2);
}

// ****************************************************************************

double computeDeltaCosThetaG_corner
  (double ra[], double rb[], double rc[], double epsa[], double epsb[], double epsc[]) {

  AH_DEBUG << "using CORNER method to calculate Delta-cos(ThetaG)" << std::endl;

  //  Estimates uncertainty in cosine theta_G
  //
  double orig = computeCosThetaG(ra, rb, rc);
  double max_deviation = -999.9e9;
  double ra_t[3], rb_t[3], rc_t[3];
  for (int i=-1; i<2; i+=2) {
    ra_t[0] = ra[0] + i*epsa[0];
    for (int j=-1; j<2; j+=2) {
      ra_t[1] = ra[1] + j*epsa[1];
      for (int k=-1; k<2; k+=2) {
        ra_t[2] = ra[2] + k*epsa[2];
        for (int p=-1; p<2; p+=2) {
          rb_t[0] = rb[0] + p*epsb[0];
          for (int q=-1; q<2; q+=2) {
            rb_t[1] = rb[1] + q*epsb[1];
            for (int r=-1; r<2; r+=2) {
              rb_t[2] = rb[2] + r*epsb[2];
              for (int e=-1; e<2; e+=2) {
                rc_t[0] = rc[0] + e*epsc[0];
                for (int f=-1; f<2; f+=2) {
                  rc_t[1] = rc[1] + f*epsc[1];
                  for (int g=-1; g<2; g+=2) {
                    rc_t[2] = rc[2] + g*epsc[2];
                    double t = computeCosThetaG(ra_t, rb_t, rc_t);
                    double t_dev = t - orig;
                    if (t_dev < 0) t_dev = -t_dev;
                    if (t_dev > max_deviation) {
                      max_deviation = t_dev;
                    }
  } } } } } } } } } 
  return max_deviation;
}

// ****************************************************************************

double computeDeltaCosThetaG_analytic
  (double ra[], double rb[], double rc[], double epsa[], double epsb[], double epsc[]) {

  // This function is taken from ComptonSoft: source/tools/src/ComptonEvent2.cc;
  // provided to Mike Witthoeft by Ichinohe-san on Feb 23, 2014.

  AH_DEBUG << "using ANALYTIC method to calculate Delta-cos(ThetaG)" << std::endl;

  double vec12[3];       // rb-ra
  double vec23[3];       // rc-rb
  double aa=0.0;          // dot_product(vec12,vec23)
  double bb=0.0;          // magnitude vec12
  double cc=0.0;          // magnitude vec23
  double bb2=0.0;         // magnitude vec12 squared
  double cc2=0.0;         // magnitude vec23 squared
  double bcinverse=0.0;   // 1/bb/cc
  double deltasquared=0.0;

  for (int i=0; i < 3; i++) { // iterate through coords x,y,z
    vec12[i]=rb[i]-ra[i];
    vec23[i]=rc[i]-rb[i];
    aa+=vec12[i]*vec23[i];
    bb2+=vec12[i]*vec12[i];
    cc2+=vec23[i]*vec23[i];
  }
  bb=std::sqrt(bb2);
  cc=std::sqrt(cc2);
  bcinverse=1./bb/cc;

  for (int i=0; i < 3; i++) { // iterate through coords x,y,z
    double deltaa=(rb[i]-rc[i])-aa*(ra[i]-rb[i])/bb2;
    double deltab=(rc[i]+ra[i]-2.*rb[i])-aa*(rb[i]-ra[i])/bb2-aa*(rb[i]-rc[i])/cc2;
    double deltac=(rb[i]-ra[i])-aa*(rc[i]-rb[i])/cc2;
    deltasquared+=deltaa*deltaa*epsa[i]*epsa[i]+
                  deltab*deltab*epsb[i]*epsb[i]+
                  deltac*deltac*epsc[i]*epsc[i];
  }

  return bcinverse*std::sqrt(deltasquared);
}

// ****************************************************************************

//int layerType(int layer_num) {
//  if (layer_num <= SI_STACK_MAX) {
//    return SI_STACK;
//  } else if (layer_num <= CDTE_BOTTOM_MAX) {
//  return CDTE_BOTTOM;
//  } else {
//  return CDTE_SIDE;
//  }
//}
//
//// ****************************************************************************
//
//bool layerIsCdTe(int layer_num) {
//  int lt = layerType(layer_num);
//  return lt == CDTE_BOTTOM || lt == CDTE_SIDE;
//}
//
//// ****************************************************************************
//
//bool layerIsSi(int layer_num) {
//  int lt = layerType(layer_num);
//  return lt == SI_STACK;
//}

// ****************************************************************************

bool energyIsCdTeFluor(double epi, hxisgdevtid::fluor::DataType& fluor_table) {
  std::string fluorescence;
  bool bottom = false, side = false;
  bottom = hxisgdevtid::fluor::hasMatch("CdTe_btm", epi,
    fluor_table, fluorescence);
  side = hxisgdevtid::fluor::hasMatch("CdTe_side", epi,
    fluor_table, fluorescence);
  return bottom || side;
}

// ****************************************************************************

bool energyIsSiFluor(double epi, hxisgdevtid::fluor::DataType& fluor_table) {
  std::string fluorescence;
  return hxisgdevtid::fluor::hasMatch("Si", epi,fluor_table, fluorescence);
}

// ****************************************************************************

bool layerIsCdTeNearSi(int layer, 
  hxisgdevtid::remap::GeomKeywords& geom) {
  bool retval = false;
  if (layer == geom.m_surside1 ||
      layer == geom.m_surside2 ||
      layer == geom.m_surside3 ||
      layer == geom.m_surside4 ||
      layer == geom.m_surbtm) {
    retval = true;
  }
  return retval;

  // for (int i=0; i < NUM_CDTE_NEAR_SI; ++i) {
  //   if (layer == CDTE_NEAR_SI[i]) {
  //     retval = true;
  //     break;
  //   }
  // }
  // return retval;

}

// ****************************************************************************

bool layerIsCdTeNorthSouth(int layer, 
  hxisgdevtid::remap::GeomKeywords geom) {

  bool retval = false;
  if (layer == geom.m_surside2 || layer == geom.m_surside2+1) {    // SURSIDE2 corresponds to NORTH
    retval = true;
  } else if (layer == geom.m_surside4 || layer == geom.m_surside4+1) {    // SURSIDE4 corresponds to SOUTH
    retval = true;
  }

  return retval;
}

// ****************************************************************************

bool layerIsCdTeEastWest(int layer, 
  hxisgdevtid::remap::GeomKeywords geom) {

  bool retval = false;
  if (layer == geom.m_surside1 || layer == geom.m_surside1+1) {    // SURSIDE1 corresponds to EAST
    retval = true;
  } else if (layer == geom.m_surside3 || layer == geom.m_surside3+1) {    // SURSIDE3 corresponds to WEST
    retval = true;
  }

  return retval;
}

// ****************************************************************************

void camCoordErrors(int layer, int layer_type, 
  hxisgdevtid::remap::GeomKeywords geom, double delta_r[]) {

  switch (layer_type) {
    case hxisgdevtid::SGD_SI_STACK:
      delta_r[0] = geom.m_delxyany;
      delta_r[1] = geom.m_delxyany;
      delta_r[2] = geom.m_delzsi;
      break;
    case hxisgdevtid::SGD_CDTE_BOTTOM:
      delta_r[0] = geom.m_delxyany;
      delta_r[1] = geom.m_delxyany;
      delta_r[2] = geom.m_delzcdte;
      break;
    case hxisgdevtid::SGD_CDTE_SIDE:
      if (layerIsCdTeNorthSouth(layer, geom)) {
        delta_r[0] = geom.m_delxyany;
        delta_r[1] = geom.m_delzcdte;
        delta_r[2] = geom.m_delxyany;
      } else if (layerIsCdTeEastWest(layer, geom)) {
        delta_r[0] = geom.m_delzcdte;
        delta_r[1] = geom.m_delxyany;
        delta_r[2] = geom.m_delxyany;
      } else {
        AH_THROW_RUNTIME("Bad layer number while arranging CAMERAXYZ errors.");
      }
      break;
    default:
      AH_THROW_RUNTIME("Bad layer type while arranging CAMERAXYZ errors.");
  }
}


// ****************************************************************************

void setRecoStatusBit(sgdevtidlib::OutputRowData& outrow, int bit) {
  outrow.m_reco_status[bit]=1;
  ++outrow.m_reco_hist[bit];
  AH_DEBUG << "Set bit " << bit << " in RECO_STATUS" << std::endl;
}

// ****************************************************************************

void clearRecoStatusHist(sgdevtidlib::OutputRowData& outrow) {
  for (int i=0; i < hxisgdevtid::SGD_SIZE_RECO_STATUS; ++i) {
    outrow.m_reco_hist[i]=0;
  }
}

// ****************************************************************************

void clearRecoStatusBit(sgdevtidlib::OutputRowData& outrow, int bit) {
  outrow.m_reco_status[bit]=0;
}

// ****************************************************************************

void clearRecoStatus(sgdevtidlib::OutputRowData& outrow) {
  for (int i=0; i < hxisgdevtid::SGD_SIZE_RECO_STATUS; ++i) clearRecoStatusBit(outrow, i);
}

// ****************************************************************************

double calcDistance(RowSignals& signals, int isig, int jsig) {
  double dx=signals.m_camerax[isig]-signals.m_camerax[jsig];
  double dy=signals.m_cameray[isig]-signals.m_cameray[jsig];
  double dz=signals.m_cameraz[isig]-signals.m_cameraz[jsig];
  return std::sqrt(dx*dx+dy*dy+dz*dz);
}

// ****************************************************************************

void setNumHits(Hits& hits, double escape_en, OutputRowData& outrow) {
  for (int i=0; i < 5; i++) outrow.m_numhits[i]=0;
  if (escape_en > 0.0) outrow.m_numhits[4]=1;
  switch(hits.m_m) {
    case 1:
      outrow.m_numhits[0]=1;
      break;
    case 2:
      outrow.m_numhits[1]=1;
      break;
    case 3:
      outrow.m_numhits[2]=1;
      break;
    case 4:
      outrow.m_numhits[3]=1;
      break;
    default:
      std::stringstream msg;
      msg << "Invalid number of hits (" << hits.m_m << ") sent to setNumHits()";
      AH_THROW_LOGIC(msg.str());
  }
}

// ****************************************************************************
  
/** @} */

}  // namespace sgdevtidlib

/* Revision Log
 $Log: sgdevtidlib.cxx,v $
 Revision 1.56  2016/12/05 19:49:37  rshill
 Substitude underscore for blank in material names that key
 fluorescence data:  'CdTe btm' --> 'CdTe_btm' and 'CdTe side' --> 'CdTe_side'

 Revision 1.55  2015/04/28 20:25:47  mwitthoe
 sgdevtid: fix bug where escape energy was not included in event energy in FOV lookup routine; remove obsolete +++ comments

 Revision 1.54  2015/03/17 21:49:37  mwitthoe
 sgdevtid: remove obsolete error on magnitude of cos(thetak), which is checked later

 Revision 1.53  2015/03/03 18:37:33  mwitthoe
 sgdevtid: update after Feb 2015 Japan meeting: store delta-F in F test; remove randomization in fluorescence merging; remove expand mode; add energy-dependent energy uncertainties

 Revision 1.52  2015/01/23 17:51:08  mwitthoe
 sgdevtid: use sequence probabilities instead of FOV probabilities in the figure-of-merit calculation

 Revision 1.51  2015/01/23 17:06:17  mwitthoe
 sgdevtid: 1) disable buffering for the FOV probability file since file is not read sequentially; 2) remove useless check on number of hits in FOV lookup function

 Revision 1.50  2015/01/22 21:55:24  mwitthoe
 sgdevit: implement FOV probability; see issue 482

 Revision 1.49  2014/12/30 22:06:20  rshill
 Corrected treatment of null EPI and PI.

 Revision 1.48  2014/12/24 20:53:40  rshill
 Modified for parameter standards update

 Revision 1.47  2014/12/24 18:31:33  rshill
 Updated for parameter standards (Redmine issue #472)

 Revision 1.46  2014/12/04 18:40:58  mwitthoe
 sgdevtid: add include statement for ahgen to sgdevtidlib.cxx

 Revision 1.45  2014/09/15 17:19:06  mwitthoe
 sgdevtid: add MATTYPE column to output, to be used in gain fitting; see issue 432

 Revision 1.44  2014/08/18 16:50:25  mwitthoe
 sgdevtid: there was a bug that showed up in ut03 on Mac OS 10.7.5 & 10.9.4 (Hans) where the tool would stop (no seg fault or other message); the bug was caused by deleting a member of a std::set in the middle of an iteration over that set; the solution was to delete members of the set outside of the iteration; see issue 425

 Revision 1.43  2014/08/08 20:02:25  mwitthoe
 sgdevtib: bugfixes - 1) in performFGProbTests(), cos(theta_g) should only be calculated of m>=3 instead of m>=2; 2) add array initializations in performTieBreaking

 Revision 1.42  2014/08/05 13:03:15  mwitthoe
 sgdevtid: fix implicit type conversion in convertEpiToSGDPI to abavoid compiler warning

 Revision 1.41  2014/07/24 17:40:14  mwitthoe
 sgdevtid: update probm CALDB file format; bug-fix: there were two expand variables being used; bug-fix: there was a mix-up between layer and sensor index when determining location of a signal; bug-fix: extrainfo was not being checked before trying to write the extra output file in sgdevtidlib

 Revision 1.40  2014/06/13 00:39:45  rshill
 Geometry in remap header.  expand parameter and
 outfileextra=NONE.  Corrected error computations.

 Revision 1.39  2014/04/14 20:45:55  rshill
 Fixed bug initializing the material matrix and layer type.

 Revision 1.38  2014/03/27 19:48:49  rshill
 Code was correctly skipping the G test for m=2, but setting
 RECO_STATUS as if this were the same case as the G test failing.
 Now it skips the G test and proceeds to the test for low probability
 sequences.

 Revision 1.37  2014/03/21 18:14:33  mwitthoe
 sgdevtid: in probm CALDB file 1) change type of probhit from int to double, 2) rename columns to match test file sent by Hiro on Mar 20; add best_k to hits structure; print hit sequence to log file (with AH_DEBUG) when occurrence_id is used; initialize test_f/g/prob to false instead of true

 Revision 1.36  2014/03/19 20:29:35  mwitthoe
 sgdevtid: change F test from F>0 to F>=0

 Revision 1.35  2014/03/19 15:33:28  mwitthoe
 sgdevtid: fix sign error in Delta-cos(theta_k); change F-test to exclude Delta-F

 Revision 1.34  2014/03/18 20:02:57  mwitthoe
 sgdevtid: in the extra output file, the column D_C_THETA_K_0 was not being filled in... it is now

 Revision 1.33  2014/03/14 21:55:04  mwitthoe
 sgdevtid: ensure that all output columns are written to; update RECO_STATUS values

 Revision 1.32  2014/03/13 21:03:43  mwitthoe
 sgdevtid: add random selection for Step 1a-2; change FOM expression; include extension name in CALDB lookup function for the badpix file; if single F test passes with M>2, continue to G test; add lookup function for ProbM file to return the MECHANISM index which gets output in the SEQHITS column; print number of occurrences which have PI=NULL to end of log file; remove some old, commented-out code

 Revision 1.31  2014/03/11 21:53:09  mwitthoe
 sgdevtid: only include signals above threshold in the PI calculation

 Revision 1.30  2014/03/11 21:06:58  mwitthoe
 sgdevtid: fix items 10,13,14,15 in issue #360; this involved removing an unused constant, changing the name of a variable, and updating default parameter values; a new function was added to check if a given energy was consistent with the Si fluorescence energy (the previous version incorrectly used the CdTe energies in Step 1a-2)

 Revision 1.29  2014/03/11 20:24:37  mwitthoe
 sgdevtid: completed SGD items 3 & 4 listed in issue #360; these items involved adding random selection in steps 1-0, 1a-1a, 1a-1b, and tie-breaking; now take absolute value of delta-g in the G test; fixed bug in FOM calculation which was omitting the escape energy

 Revision 1.28  2014/03/06 09:39:52  mwitthoe
 sgdevtid: fix bug in fluorescence lookup; fix bug in hit count in step 1a-2; fix bug in FOM

 Revision 1.27  2014/03/05 14:03:50  mwitthoe
 sgdevtid: add a couple more columns to the extra output file to keep track of when merging occurred and the cluster shape (step 1-0); fix bug where total EPI was not calculated properly; in escape loop, automatically fail F test if escape energy is negative; if the calculated PI is out-of-range, set it to the max value: 2048

 Revision 1.26  2014/03/04 14:10:19  mwitthoe
 sgdevtid: change algorithm for Steps 1-0 and 1a-3; in Step 1-0, certain clusters up to size 5 are allowed; in Step 1a-3, up to 3 signals can now be merged (instead of only 2)

 Revision 1.25  2014/03/01 17:22:55  mwitthoe
 sgdevtid: update tool based on testing results on 2/28 between Hiro, Ichinohe, and MCW; the updates include bug fixes and algorithm changes; details are in the Japan notes under Build 5/Sprint 1 on the redmine wiki

 Revision 1.24  2014/02/22 00:08:24  rshill
 Added debug output for sin theta; deleted it for filling extraoutrow fields.

 Revision 1.23  2014/02/21 20:51:42  rshill
 Fixed failure to propagate variances through signal merges;
 added occurrence_id parameter to process one selected row.

 Revision 1.22  2014/02/20 22:58:40  rshill
 Bug fixes in writing output rows.

 Revision 1.21  2014/02/15 00:35:41  rshill
 Bugfixes: G test subscripting; for-loop initialization.

 Revision 1.20  2014/02/12 23:31:07  rshill
 Made some progress debugging.

 Revision 1.19  2014/02/12 01:09:44  rshill
 Debugging in progress.

 Revision 1.18  2014/02/07 01:06:50  rshill
 Debugged several FITS in output and extra output files.

 Revision 1.17  2014/02/06 00:42:15  rshill
 Fixed extra output handling and delta E.

 Revision 1.16  2014/02/05 00:45:15  rshill
 Updated RECO_STATUS handling.

 Revision 1.15  2014/02/04 00:57:52  rshill
 Edited for TRF conformance.

 Revision 1.14  2014/02/03 22:15:25  rshill
 Greatly simplified flow on return from functions;
 rationalized RECO_STATUS bit assignments.

 Revision 1.13  2014/02/03 19:14:04  rshill
 Partway through reconciliation with TRF.  Still builds.

 Revision 1.12  2014/01/31 00:15:54  rshill
 Added access to probm and probd files and deuggued via library unit tests.

 Revision 1.11  2014/01/30 20:42:13  rshill
 Added in actually using the probm CALDB file.

 Revision 1.10  2014/01/30 18:30:10  rshill
 Filled material matrix for probm CALDB lookup.  Still needs that lookup.

 Revision 1.9  2014/01/30 03:51:52  rshill
 Revised for closer TRF conformance.

 Revision 1.8  2014/01/29 22:12:53  rshill
 Interim version with callouts from TRF meeting.

 Revision 1.7  2014/01/29 15:15:02  rshill
 Deleted an unused utility routine for signal variance.

 Revision 1.6  2014/01/29 01:41:49  rshill
 Completed some version of all routines

 Revision 1.5  2014/01/28 23:39:53  rshill
 Revsions following walkthrough with MW.

 Revision 1.4  2014/01/28 20:31:46  rshill
 Building output row data; improved set of values for condition.

 Revision 1.3  2014/01/27 23:58:32  rshill
 Added extra output file management.  Improved permutation
 management.  Moved many declarations from sgdevtid to sgdevtidlib.

 Revision 1.2  2014/01/25 01:37:07  rshill
 Brought closer to TRF with differences called out.

 Revision 1.1  2014/01/24 19:44:15  rshill
 First commit of SGD event reconstruction functions.

 
*/

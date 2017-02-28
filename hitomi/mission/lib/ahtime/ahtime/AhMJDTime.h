/// \file AhMJDTime.h
/// \brief represent time using the Modified Julian Date (MJD) format.
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:18 $

#ifndef AHTIME_AHMJDTIME_H
#define AHTIME_AHMJDTIME_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_AHMJDTIME,"$Id: AhMJDTime.h,v 1.2 2014/09/10 02:41:18 mwitthoe Exp $")

#include "ahtime/ahtime_base.h"

/// \ingroup mod_ahtime
namespace ahtime {

/** \addtogroup mod_ahtime
 *  @{
 */

// forward declaration
class AhMJDTime;

/// \brief container storing date and time in MJD format
///
/// The integral and fractional portion of the date are stored separately;
/// the former represents the date while the latter is the time.
class AhMJDTime {
  public:

    /// \brief initialize with zero
    AhMJDTime();

    /// \brief initialize with integral and fractional part separately
    /// \param[in] mjdi integral part of MJD
    /// \param[in] mjdf fractional part of MJD
    AhMJDTime(long mjdi, double mjdf);

    /// \brief initialize using single MJD value
    /// \param[in] mjd MJD value
    AhMJDTime(double mjd);

    /// \brief set date using integral and fractional part separately
    /// \param[in] mjdi integral part of MJD
    /// \param[in] mjdf fractional part of MJD
    void set(long mjdi, double mjdf);

    /// \brief set date using single MJD value
    /// \param[in] mjd MJD value
    void set(double mjd);

    /// \brief return MJD value
    /// \return MJD
    double mjd() const;

    /// \brief return integral part of MJD
    /// \return whole part of MJD
    long mjdi() const;

    /// \brief return fractional part of MJD
    /// \return fractional part of MJD
    double mjdf() const;

    /// \brief compare with another instance; +1 means this > dt
    /// \param[in] dt AhMJDTime instance to compare to
    /// \return +1,0,-1 based on order
    int compare(const AhMJDTime & dt) const;

    /// \brief add the given number of seconsd to time
    /// \param[in] number of seconds to add
    void addSeconds(double seconds);

    /// \brief subtract the given number of seconsd to time
    /// \param[in] number of seconds to remove
    void subtractSeconds(double seconds);

  private:
    long m_mjdi;              ///< integral part of MJD date
    double m_mjdf;            ///< fractional part of MJD [0.0:1.0)

};


/** @} */

}  // namespace ahtime

#endif /* AHTIME_AHMJDTIME_H */

/* Revision Log
 $Log: AhMJDTime.h,v $
 Revision 1.2  2014/09/10 02:41:18  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.1  2013/09/18 14:09:13  mwitthoe
 ahtime library: add classes to store times in the DateTime and MJD formats


*/

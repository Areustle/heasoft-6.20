//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
// for abs(int) function
#include <cstdlib>

// RandomLux
#include <XSUtil/Numerics/RandomLux.h>

namespace Numerics {
    const int RandomLux::s_MAXLEV = 4;
//                               default
//   Luxury Level   0     1     2   *3*    4
// Corresponds to p=24    48    97   223   389
//      time factor 1     2     3     6    10   on slow workstation
//                  1    1.5    2     3     5   on fast mainframe
//
    const int RandomLux::s_ndskip[RandomLux::s_MAXLEV+1] = {0, 24, 73, 199, 365};

    // Class Numerics::RandomLux 
    const int RandomLux::s_LXDFLT = 3;
    const int RandomLux::s_IGIGA = 1000000000;
    const int RandomLux::s_JSDFLT = 314159265;
    const int RandomLux::s_ITWO24 = 16777216;
    const int RandomLux::s_ICONS = 2147483563;
    float RandomLux::s_TWOP12 = 4096.;
    float RandomLux::s_twom24 = -1.;
    float RandomLux::s_twom12 = -1.;
    int RandomLux::s_next[24];

    RandomLux::RandomLux()
      : m_notyet(true),
        m_i24(23),
        m_j24(9),
        m_carry(.0),
        m_luxlev(3),
        m_nskip(0),
        m_in24(0),
        m_kount(0),
        m_mkount(0),
        m_inseed(0)
    {
       for (int i=0; i<24; ++i)
          m_seeds[i] = .0;
       // Initialize s_twom24, s_twom12, and s_next once for all objects.
       if (s_twom24 < .0)
       {
          s_twom24 = 1.0;
          for (int i=0; i<24; ++i)
          {
             s_twom24 /= 2.0;
             s_next[i] = i-1;
          }
          s_next[0] = 23;
          s_twom12 = s_twom24*4096.;
       }
    }


    RandomLux::~RandomLux()
    {
    }


    void RandomLux::rluxgo (int lux, int ins, int k1, int k2)
    {
       // Function to initialize from one or three integers
       int iseeds[24];
       if (lux < 0)
          m_luxlev = s_LXDFLT;
       else if (lux <= s_MAXLEV)
          m_luxlev = lux;
       else if (lux < 24 || lux > 2000)
          m_luxlev = s_MAXLEV;
       else
       {
          m_luxlev = lux;
          for (int ilx=0; ilx<=s_MAXLEV; ++ilx)
             if (lux == s_ndskip[ilx] + 24)
                m_luxlev = ilx;
       }

       if (m_luxlev <= s_MAXLEV)
          m_nskip = s_ndskip[m_luxlev];
       else
          m_nskip = m_luxlev - 24;
       m_in24 = 0;
       int jseed = 0;
       if (ins > 0)
          jseed = ins;
       else 
          jseed = s_JSDFLT;
       m_inseed = jseed;
       m_notyet = false;
       for (int i=0; i<24; ++i)
       {
          int k = jseed/53668;
          jseed = 40014*(jseed-k*53668) - k*12211;
          if (jseed < 0)
             jseed += s_ICONS;
          iseeds[i] = jseed % s_ITWO24;
       }
       for (int i=0; i<24; ++i)
       {
          m_seeds[i] = iseeds[i]*s_twom24;
       }
       m_i24 = 23;
       m_j24 = 9;
       m_carry = .0;
       if (m_seeds[23] == .0)
          m_carry = s_twom24;
       // If restarting at a break point, skip K1 + IGIGA*K2
       // Note that this is the number of numbers delivered to
       // the user PLUS the number skipped (if luxury .GT. 0).
       m_kount = k1;
       m_mkount = k2;
       if (k1+k2 != .0)
       {
          for (int iouter=0; iouter <= k2; ++iouter)
          {
             int inner = s_IGIGA;
             if (iouter == k2)
                inner = k1;
             for (int isk=0; isk<inner; ++isk)
             {
                float uni = m_seeds[m_j24] - m_seeds[m_i24] - m_carry;
                if (uni < .0)
                {
                   uni += 1.0;
                   m_carry = s_twom24;
                }
                else
                   m_carry = .0;
                m_seeds[m_i24] = uni;
                m_i24 = s_next[m_i24];
                m_j24 = s_next[m_j24];
             }
          }
          // Get the right value of IN24 by direct calculation
          m_in24 = m_kount % (m_nskip+24);
          if (m_mkount > 0)
          {
             int izip = s_IGIGA % (m_nskip+24);
             int izip2 = m_mkount*izip + m_in24;
             m_in24 = izip2 % (m_nskip+24);
          }
          // Now IN24 had better be between zero and 23 inclusive
          if (m_in24 > 23)
             m_in24 = 0;
       }
    }

    void RandomLux::ranlux (float* rvec, int lenv)
    {
       // s_notyet is true if no initialization has been performed yet.
       // Default Initialization by Multiplicative Congruential
       int iseeds[24];
       if (m_notyet)
       {
          m_notyet = false;
          int jseed = s_JSDFLT;
          m_inseed = jseed;
          m_luxlev = s_LXDFLT;
          m_nskip = s_ndskip[m_luxlev];
          m_in24 = 0;
          m_kount = 0;
          m_mkount = 0;
          for (int i=0; i<24; ++i)
          {
             int k = jseed/53668;
             jseed = 40014*(jseed-k*53668) - k*12211;
             if (jseed < 0)
                jseed += s_ICONS;
             iseeds[i] = jseed % s_ITWO24;
          }
          for (int i=0; i<24; ++i)
          {
             m_seeds[i] = iseeds[i]*s_twom24;
          }
          m_i24 = 23;
          m_j24 = 9;
          m_carry = .0;
          if (m_seeds[23] == .0)
                m_carry = s_twom24;
       }

       //  The Generator proper: "Subtract-with-borrow",
       //  as proposed by Marsaglia and Zaman,
       //  Florida State University, March, 1989

       for (int ivec=0; ivec<lenv; ++ivec)
       {
          float uni = m_seeds[m_j24] - m_seeds[m_i24] - m_carry;
          if (uni < .0)
          {
             uni += 1.0;
             m_carry = s_twom24;
          }
          else
             m_carry = .0;
          m_seeds[m_i24] = uni;
          m_i24 = s_next[m_i24];
          m_j24 = s_next[m_j24];
          rvec[ivec] = uni;
          // small numbers (with less than 12 "significant" bits)
          // are "padded".
          if (uni < s_twom12)
          {
             rvec[ivec] += s_twom24*m_seeds[m_j24];
             // and zero is forbidden in case someone takes a 
             // logarithm
             if (rvec[ivec] == .0)
                rvec[ivec] = s_twom24*s_twom24;
          }
          // Skipping to luxury.  As proposed by Martin Luscher.
          m_in24 += 1;
          if (m_in24 == 24)
          {
             m_in24 = 0;
             m_kount += m_nskip;
             for (int isk=0; isk<m_nskip; ++isk)
             {
                uni = m_seeds[m_j24] - m_seeds[m_i24] - m_carry;
                if (uni < .0)
                {
                   uni += 1.0;
                   m_carry = s_twom24;
                }
                else
                   m_carry = .0;
                m_seeds[m_i24] = uni;
                m_i24 = s_next[m_i24];
                m_j24 = s_next[m_j24];
             }             
          }
       } // end ivec loop
       m_kount += lenv;
       if (m_kount >= s_IGIGA)
       {
          m_mkount += 1;
          m_kount -= s_IGIGA;
       }
    }

    void RandomLux::rluxin (const int* isdext)
    {
       // Function to input and float integer seeds from previous run
       for (int i=0; i<24; ++i)
          m_seeds[i] = isdext[i]*s_twom24;
       m_carry = .0;
       if (isdext[24] < .0)
          m_carry = s_twom24;
       int isd = std::abs(isdext[24]);
       m_i24 = isd % 100 - 1;
       isd /= 100;
       m_j24 = isd % 100 - 1;
       isd /= 100;
       m_in24 = isd % 100;
       isd /= 100;
       m_luxlev = isd;

       if (m_luxlev <= s_MAXLEV)
          m_nskip = s_ndskip[m_luxlev];
       else if (m_luxlev >= 24)
          m_nskip = m_luxlev - 24;
       else
       {
          m_nskip = s_ndskip[s_MAXLEV];
          m_luxlev = s_MAXLEV;
       }
       m_inseed = -1;
    }

    void RandomLux::rluxut (int* isdext) const
    {
       // Function to output seeds as integers
       for (int i=0; i<24; ++i)
          isdext[i] = static_cast<int>(m_seeds[i]*s_TWOP12*s_TWOP12);
       isdext[24] = m_i24+1 + 100*(m_j24+1) + 10000*m_in24 + 1000000*m_luxlev;
       if (m_carry > .0)
          isdext[24] *= -1;
    }

    void RandomLux::rluxat (int& lout, int& inout, int& k1, int& k2) const
    {
       lout = m_luxlev;
       inout = m_inseed;
       k1 = m_kount;
       k2 = m_mkount;
    }

    // Additional Declarations

} // namespace Numerics


#include "Beta.h"
#include "Gamma.h"
#include <iostream>

namespace Numerics {

        const int BetaCf::ITMAX = 100;
        const Real BetaCf::epsilon = 3.E-7;


        Real BetaCf::operator() (Real a, Real b, Real x)
        {
                // continued fraction expansion
                Real am (1); 
                Real bm (1);
                Real az (1);
                Real qab ( a + b );
                Real qap ( a + 1 );
                Real qam ( a - 1 );
                Real bz ( 1 - qab*x/qap );
                Real d (0);
                Real tem(0);
                Real em(0);
                Real ap(0);
                Real app(0);
                Real aold(az);
                Real bp(0);
                Real bpp (0);
                int m(1);                
                for (; m <= ITMAX; ++m)
                {
                        em      = m;
                        tem     = em + em;
                        d       = em*(b - m)*x/((qam + tem)*(a + tem));
                        ap      = az + d*am;
                        bp      = bz + d*bm;
                        d       = -(a + em)*(qab + em)*x/((a + tem)*(qap + tem));
                        app     = ap + d*az;
                        bpp     = bp + d*bz;
                        aold    = az;
                        am      = ap/bpp;
                        bm      = bp/bpp;
                        az      = app/bpp;
                        bz      = 1;
                        if ( std::abs(az - aold) < epsilon*std::abs(az)) break;
                }
                if ( m > ITMAX )
                {
                        std::cerr << "A or B too big, or ITMAX too small"<<std::endl;
                        return -999;
                }
                return az;
        }

        Real betaI( Real a, Real b, Real x)
        {
                Real bt (0);
                Real betai (0);


                if ( x < 0 || x > 1)
                {
                        std::cerr << " Bad argument in betaI " << x << "( should be in (0,1) )"
                                <<std::endl;
                        return -999; 
                }

                if ( a == 0 || b == 0)
                {
                        string which;
                        a == 0 ? which = "a is " : which = "b is ";
                        if ( a == 0 && b == 0) which = "a and b are";
                        std::cerr << " Bad argument in betaI: " << which << "zero" <<std::endl;
                        return -999; 

                }

                else 
                {
                        if ( x != 0 && x != 1)
                        {
                                GammaLN lnGamma;
                                bt = exp(lnGamma(a+b) - lnGamma(a) - lnGamma(b) 
                                                + a*log(x) + b*log(1.-x));

                        }
                        BetaCf betacf;
                        if ( x < ( a + 1.)/(a + b + 2.))
                        {
                                betai = bt*betacf(a,b,x)/a;
                        }
                        else
                        {
                                betai = 1. - bt*betacf(b,a,1.-x)/b;
                        }
                }
                return betai;

        }
}

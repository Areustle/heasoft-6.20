
#include <cmath>
#include <iostream>
#include <iomanip>
#include <Gamma.h>
#include <XSUtil/Utils/IosHolder.h>

namespace Numerics
{
        const Real STP = 2.50662827465;
        const Real FPF = 5.5;
        const Real EPS = 3.E-7;
        const size_t ITMAX = 10000;
        bool GammaLN::s_init = false;
        std::vector<Real> C;

        GammaLN::GammaLN()
        {
                if (!s_init)
                {
                     C.resize(6);
                     C[0] =    76.18009173;
                     C[1] =   -86.50532033;
                     C[2] =    24.01409822;
                     C[3] =   -1.231739516;
                     C[4] =   .120858003E-2;
                     C[5] =  -.536382E-05;
                     s_init = true;
                }       
        }

        Real GammaLN::operator()(Real xx) const
        {
                static const Real PI (4.*std::atan(1.));
                Real lngamma(0);
                using namespace std;
                bool smallx ( xx < 1.);
                Real xxx(xx);
                if (smallx) xxx = 2. - xx;
                Real x (xxx - 1.);                
                Real tt (x + FPF);
                if (tt <= 0) 
                {
                        *IosHolder::errHolder() << "GammLN Error: argument of log <= 0"
                                <<std::endl;
                        return -99.;       
                }
                tt = (x + 0.5)*log(tt) - (tt);
                Real ser(1.);
                for (size_t j = 0; j <= 5; ++j)
                {
                        x += 1;
                        ser += C[j]/x;
                }
                lngamma = tt + log(STP*ser);

                if ( smallx )
                {
                   lngamma   = -lngamma;
                   lngamma  += log(PI*(1.- xx)/sin(PI*(1.- xx)));     
                }

                return lngamma;
        }

        Real GammaP::operator() (Real a,Real x) const 
        {
                GammaQ G;
                return 1. - G(a,x);
        }

        Real Erf::operator() (Real x) const 
        {
                GammaP G;
                return  x < 0 ? -G(0.5,x*x) : G(0.5,x*x) ;
        }

        Real Erfc::operator() (Real x) const
        {
                GammaQ GQ;
                GammaP GP;
                return x < 0 ? 1. + GP(0.5,x*x) : GQ(0.5,x*x);

        }

        Real GammaQ::operator() (Real a,Real x) const 
        {
                Real gq (0);
                Real gln(0);
                if ( x < 0 ) 
                {
                        *IosHolder::errHolder() << "GammQ: X < 0" <<std::endl;
                        return -99;       
                }       
                if ( a <= 0 ) 
                {
                        *IosHolder::errHolder() << "GammQ: A <=  0"<<std::endl;
                        return -99;       
                }       
                Real gamSer(0);
                if ( x < a + 1) 
                {

                        Gser(gamSer,a,x,gln);
                        gq = 1. - gamSer;
                }
                else
                {
                        Gcf(gamSer,a,x,gln);
                        gq = gamSer;
                }
                return gq;
        }


        void Gcf(Real& gamSer, Real a, Real x, Real& gln)
        {
                GammaLN G;
                Real gOld(0), g(0), a0(1.), a1(x), b0(0), b1(1.), fac(1.);

                gln  = G(a);

                size_t j (1);

                while ( j < ITMAX )
                {
                        Real an (j);
                        Real ana (an - a );
                        a0 = (a1 + a0*ana)*fac;
                        b0 = (b1 + b0*ana)*fac;
                        Real anf = an*fac;
                        a1 = x*a0 + anf*a1;
                        b1 = x*b0 + anf*b1;
                        if (a1 != 0)
                        {
                                fac = 1./a1;
                                g   = b1 * fac;
                                if ( std::abs(1. - gOld/g) < EPS )  
                                {
                                        gamSer = exp( -x + a*log(x) - gln )*g;
                                        break;
                                }     
                                gOld = g;
                        }
                        ++j;  
                        if (j == ITMAX)
                        {
                                *IosHolder::errHolder()  
                                 << " GCF: too many iterations before required accuracy achieved"
                                 << std::endl;
                        }      
               }
        }

        void Gser(Real& gamSer, Real a, Real x, Real& gln)
        {
                GammaLN G;

                gln = G(a);

                if ( x < 0 )
                {
                        *IosHolder::errHolder() << "Error: GSER: X <  0"<<std::endl; 
                        gamSer = 0;

                }
                else if (x == .0)
                {
                   gamSer = .0;
                }
                else
                {
                        Real ap = a;       
                        Real sum( 1./a );
                        Real del (sum);
                        size_t j (1);
                        while ( j < ITMAX)
                        {
                                ap += 1;
                                del *= (x/ap);
                                sum += del;
                                if ( std::abs(del) < std::abs(sum)*EPS)
                                {
                                     gamSer = sum*exp(-x + a*log(x) - gln);  
                                     break;
                                }
                                ++j;       
                                if (j == ITMAX)
                                {
                                        *IosHolder::errHolder()  
                                          << " GSER: too many iterations before required accuracy achieved"
                                          << std::endl;
                                }      
                        }
                }



        }        
}

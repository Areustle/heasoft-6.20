
#include "CosmologyFunction.h"

namespace Numerics
{


        const Real Eta::c1 = -0.1540;
        const Real Eta::c2 =  0.4304;
        const Real Eta::c3 =  0.19097;
        const Real Eta::c4 =  0.066941;
        const Real Eta::m8th = -0.125;
        const Real Eta::thrd = 1./3.;

        Real Eta::operator() (Real A, Real Omega) const
        {
                Real eta (0);

                Real S3 (0);
                Omega != 0 ? S3 = ( 1 - Omega )/Omega : S3 = -1 ;
                Real S = pow(S3,thrd);
                Real A2 = A*A;

                eta = 2.*sqrt(S3 + 1)*pow((1./A2 + c1*S/A  + c2*S*S)/A2 + (c3/A + c4*S)*S3,m8th);

                return eta;
        }

        Real FZSQ::operator() (Real z, Real q0, Real lambda) const
        {
                Eta ETA;

                // cosmological constant in flat universe
                if ( lambda != 0 )
                {
                        Real Omega0( 1. - lambda );
                        Real fz = (( 1. + z )*(ETA(1.,Omega0) - ETA(1./(1.+z),Omega0)));
                        return fz*fz;
                }       
                else
                {
                        // no cosmological constant.
                        Real x (2.*z*q0);
                        if ( x > 0.0001)
                        {
                                Real fz = q0*z+ (q0-1.)*(sqrt(1. + x) - 1.);
                                fz /= (q0*q0);
                                return fz*fz;
                        }
                        else
                        {
                                Real zsq (z*z);
                                Real zcu (zsq*z);
                                Real fz = z + 0.5*zsq - 0.5*q0*(zsq + zcu) + 0.5*q0*q0*zcu;
                                return fz*fz;
                        }
                }
        }

}

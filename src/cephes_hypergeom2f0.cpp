//
//  cephes_hypergeom2f1.cpp
//
//
//  Created by Patrik on 23/05/15.
//
//


/*
 Cephes Math Library Release 2.8:  June, 2000
 Copyright 1984, 1987, 1988, 2000 by Stephen L. Moshier
 */


/*Add standard c math implementation for functions
 as specified in the cephes documentation
 */
#include <math.h>
#include <stdexcept>
#include <string>

#include "cephes_hypergeom2f0.h"


/*
 constants
 from cephes consts.c
 */

const double MAXNUM =  1.7976931348623158E308;
const double MACHEP =  1.11022302462515654042E-16;

namespace cephes {
    

    /*							hyp2f0()	*/
    /* type determines what converging factor to use */
    double hypergeom2F0( double a, double b, double x, int type, double * err )
    {
    double a0, alast, t, tlast, maxt;
    double n, an, bn, u, sum, temp;

    an = a;
    bn = b;
    a0 = 1.0e0;
    alast = 1.0e0;
    sum = 0.0;
    n = 1.0e0;
    t = 1.0e0;
    tlast = 1.0e9;
    maxt = 0.0;

    do
        {
        if( an == 0 )
            goto pdone;
        if( bn == 0 )
            goto pdone;

        u = an * (bn * x / n);

        /* check for blowup */
        temp = fabs(u);
        if( (temp > 1.0 ) && (maxt > (MAXNUM/temp)) )
            goto error;

        a0 *= u;
        t = fabs(a0);

        /* terminating condition for asymptotic series */
        if( t > tlast )
            goto ndone;

        tlast = t;
        sum += alast;	/* the sum is one term behind */
        alast = a0;

        if( n > 200 )
            goto ndone;

        an += 1.0e0;
        bn += 1.0e0;
        n += 1.0e0;
        if( t > maxt )
            maxt = t;
        }
    while( t > MACHEP );


    pdone:	/* series converged! */

    /* estimate error due to roundoff and cancellation */
    *err = fabs(  MACHEP * (n + maxt)  );

    alast = a0;
    goto done;

    ndone:	/* series did not converge */

    /* The following "Converging factors" are supposed to improve accuracy,
     * but do not actually seem to accomplish very much. */

    n -= 1.0;
    x = 1.0/x;

    switch( type )	/* "type" given as subroutine argument */
    {
    case 1:
        alast *= ( 0.5 + (0.125 + 0.25*b - 0.5*a + 0.25*x - 0.25*n)/x );
        break;

    case 2:
        alast *= 2.0/3.0 - b + 2.0*a + x - n;
        break;

    default:
        ;
    }

    /* estimate error due to roundoff, cancellation, and nonconvergence */
    *err = MACHEP * (n + maxt)  +  fabs ( a0 );


    done:
    sum += alast;
    return( sum );

    /* series blew up: */
    error:
    *err = MAXNUM;
        throw std::runtime_error(std::string("cephes::hypergeom2F0: Total loss of precission"));
    //mtherr( "hyperg", TLOSS );
    return( sum );
    }
        
    
}

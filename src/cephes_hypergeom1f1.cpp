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



/*							hyperg.c
 *
 *	Confluent hypergeometric function
 *
 *
 *
 * SYNOPSIS:
 *
 * double a, b, x, y, hyperg();
 *
 * y = hyperg( a, b, x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Computes the confluent hypergeometric function
 *
 *                          1           2
 *                       a x    a(a+1) x
 *   F ( a,b;x )  =  1 + ---- + --------- + ...
 *  1 1                  b 1!   b(b+1) 2!
 *
 * Many higher transcendental functions are special cases of
 * this power series.
 *
 * As is evident from the formula, b must not be a negative
 * integer or zero unless a is an integer with 0 >= a > b.
 *
 * The routine attempts both a direct summation of the series
 * and an asymptotic expansion.  In each case error due to
 * roundoff, cancellation, and nonconvergence is estimated.
 * The result with smaller estimated error is returned.
 *
 *
 *
 * ACCURACY:
 *
 * Tested at random points (a, b, x), all three variables
 * ranging from 0 to 30.
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC       0,30         2000       1.2e-15     1.3e-16
 qtst1:
 21800   max =  1.4200E-14   rms =  1.0841E-15  ave = -5.3640E-17
 ltstd:
 25500   max = 1.2759e-14   rms = 3.7155e-16  ave = 1.5384e-18
 *    IEEE      0,30        30000       1.8e-14     1.1e-15
 *
 * Larger errors can be observed when b is near a negative
 * integer or zero.  Certain combinations of arguments yield
 * serious cancellation error in the power series summation
 * and also are not in the region of near convergence of the
 * asymptotic series.  An error message is printed if the
 * self-estimated relative error is greater than 1.0e-12.
 *
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

namespace cephes{

    //unnamed namespace to hide from externals
    namespace internal {
        double hy1f1p(double, double, double, double *);
        double hy1f1a(double, double, double, double *);
    }
}

namespace cephes {
    
    double hypergeom1F1( double a, double b, double x)
    {
    double asum, psum, acanc, pcanc, temp;

    /* See if a Kummer transformation will help */
    temp = b - a;
    if( fabs(temp) < 0.001 * fabs(a) )
        return( exp(x) * cephes::hypergeom1F1( temp, b, -x )  );


    psum = internal::hy1f1p( a, b, x, &pcanc );
    if( pcanc < 1.0e-15 )
        goto done;


    /* try asymptotic series */

    asum = internal::hy1f1a( a, b, x, &acanc );


    /* Pick the result with less estimated error */

    if( acanc < pcanc )
        {
        pcanc = acanc;
        psum = asum;
        }

    done:
    if( pcanc > 1.0e-12 )
    //	mtherr( "hyperg", PLOSS );
        throw std::runtime_error(std::string("cephes::hypergeom1F1: Partial loss of precission"));

    return( psum );
    }


    //unnamed namespace to hide for externs
    namespace internal {




        /* Power series summation for confluent hypergeometric function		*/


        double hy1f1p( double a, double b, double x, double * err )
        {
        double n, a0, sum, t, u, temp;
        double an, bn, maxt, pcanc;


        /* set up for power series summation */
        an = a;
        bn = b;
        a0 = 1.0;
        sum = 1.0;
        n = 1.0;
        t = 1.0;
        maxt = 0.0;


        while( t > MACHEP )
            {
            if( bn == 0 )			/* check bn first since if both	*/
                {
                    throw std::runtime_error(std::string("cephes::hypergeom2F1::psi: function singularity"));
        //		mtherr( "hyperg", SING );
                return( MAXNUM );	/* an and bn are zero it is	*/
                }
            if( an == 0 )			/* a singularity		*/
                return( sum );
            if( n > 200 )
                goto pdone;
            u = x * ( an / (bn * n) );

            /* check for blowup */
            temp = fabs(u);
            if( (temp > 1.0 ) && (maxt > (MAXNUM/temp)) )
                {
                pcanc = 1.0;	/* estimate 100% error */
                goto blowup;
                }

            a0 *= u;
            sum += a0;
            t = fabs(a0);
            if( t > maxt )
                maxt = t;
        /*
            if( (maxt/fabs(sum)) > 1.0e17 )
                {
                pcanc = 1.0;
                goto blowup;
                }
        */
            an += 1.0;
            bn += 1.0;
            n += 1.0;
            }

        pdone:

        /* estimate error due to roundoff and cancellation */
        if( sum != 0.0 )
            maxt /= fabs(sum);
        maxt *= MACHEP; 	/* this way avoids multiply overflow */
        pcanc = fabs( MACHEP * n  +  maxt );

        blowup:

        *err = pcanc;

        return( sum );
        }
        
        
        
        


        /*							hy1f1a()	*/
        /* asymptotic formula for hypergeometric function:
         *
         *        (    -a                         
         *  --    ( |z|                           
         * |  (b) ( -------- 2f0( a, 1+a-b, -1/x )
         *        (  --                           
         *        ( |  (b-a)                      
         *
         *
         *                                x    a-b                     )
         *                               e  |x|                        )
         *                             + -------- 2f0( b-a, 1-a, 1/x ) )
         *                                --                           )
         *                               |  (a)                        )
         */

        double hy1f1a( double a, double b, double x,double * err )
        {
        double h1, h2, t, u, temp, acanc, asum, err1, err2;

        if( x == 0 )
            {
            acanc = 1.0;
            asum = MAXNUM;
            goto adone;
            }
        temp = log( fabs(x) );
        t = x + temp * (a-b);
        u = -temp * a;

        if( b > 0 )
            {
            temp = lgamma(b);
            t += temp;
            u += temp;
            }

        h1 = cephes::hypergeom2F0( a, a-b+1, -1.0/x, 1, &err1 );

        temp = exp(u) / tgamma(b-a);
        h1 *= temp;
        err1 *= temp;

        h2 = cephes::hypergeom2F0( b-a, 1.0-a, 1.0/x, 2, &err2 );

        if( a < 0 )
            temp = exp(t) / tgamma(a);
        else
            temp = exp( t - lgamma(a) );

        h2 *= temp;
        err2 *= temp;

        if( x < 0.0 )
            asum = h1;
        else
            asum = h2;

        acanc = fabs(err1) + fabs(err2);


        if( b < 0 )
            {
            temp = tgamma(b);
            asum *= temp;
            acanc *= fabs(temp);
            }


        if( asum != 0.0 )
            acanc /= fabs(asum);

        acanc *= 30.0;	/* fudge factor, since error of asymptotic formula
                 * often seems this much larger than advertised */

        adone:


        *err = acanc;
        return( asum );
        }

    }
    
}


//
//  cephes_hypergeom2f1.cpp
//
//
//  Created by Patrik on 23/05/15.
//
//

/*Add standard c math implementation for functions
 as specified in the cephes documentation
*/
#include <math.h>
#include <stdexcept>
#include <string>

#include "cephes_hypergeom2f1.h"


/*
 constants
 from cephes consts.c
 */

const double MAXNUM =  1.7976931348623158E308;
const double MACHEP =  1.11022302462515654042E-16;

//double PI     =  3.14159265358979323846; //cephes

/*
 prototypes
 */

namespace cephes{
    namespace internal{
        //hyp2f1.c
        double hyt2f1(double, double, double, double, double *);
        double hys2f1(double, double, double, double, double *);
    }
}


/*
 defs
 */
//hyp2f1.c
#define EPS 1.0e-13
#define EPS2 1.0e-10
#define ETHRESH 1.0e-12

//psi.c
#define EUL 0.57721566490153286061




/*							hyp2f1.c
 *
 *	Gauss hypergeometric function   F
 *	                               2 1
 *
 *
 * SYNOPSIS:
 *
 * double a, b, c, x, y, hyp2f1();
 *
 * y = hyp2f1( a, b, c, x );
 *
 *
 * DESCRIPTION:
 *
 *
 *  hyp2f1( a, b, c, x )  =   F ( a, b; c; x )
 *                           2 1
 *
 *           inf.
 *            -   a(a+1)...(a+k) b(b+1)...(b+k)   k+1
 *   =  1 +   >   -----------------------------  x   .
 *            -         c(c+1)...(c+k) (k+1)!
 *          k = 0
 *
 *  Cases addressed are
 *	Tests and escapes for negative integer a, b, or c
 *	Linear transformation if c - a or c - b negative integer
 *	Special case c = a or c = b
 *	Linear transformation for  x near +1
 *	Transformation for x < -0.5
 *	Psi function expansion if x > 0.5 and c - a - b integer
 *      Conditionally, a recurrence on c to make c-a-b > 0
 *
 * |x| > 1 is rejected.
 *
 * The parameters a, b, c are considered to be integer
 * valued if they are within 1.0e-14 of the nearest integer
 * (1.0e-13 for IEEE arithmetic).
 *
 * ACCURACY:
 *
 *
 *               Relative error (-1 < x < 1):
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      -1,7        230000      1.2e-11     5.2e-14
 *
 * Several special cases also tested with a, b, c in
 * the range -7 to 7.
 *
 * ERROR MESSAGES:
 *
 * A "partial loss of precision" message is printed if
 * the internally estimated relative error exceeds 1^-12.
 * A "singularity" message is printed on overflow or
 * in cases not addressed (such as x < -1).
 */

/*							hyp2f1	*/


/*
 Cephes Math Library Release 2.8:  June, 2000
 Copyright 1984, 1987, 1992, 2000 by Stephen L. Moshier
 */




////////////////////////////*********************************////////////////////////////////
//
// hyp2f1.c
//
////////////////////////////*********************************////////////////////////////////
// [[Rcpp::export]]
double hypWrap(double a, double b, double c, double x)
{
    return cephes::hypergeom2F1(a, b, c, x);
}


namespace cephes{

    double hypergeom2F1(double a, double b, double c, double x)
    {
        double d, d1, d2, e;
        double p, q, r, s, y, ax;
        double ia, ib, ic, id, err;
        int flag, i, aid;

        err = 0.0;
        ax = fabs(x);
        s = 1.0 - x;
        flag = 0;
        ia = round(a); /* nearest integer to a */
        ib = round(b);

        if( a <= 0 )
        {
            if( fabs(a-ia) < EPS )		/* a is a negative integer */
                flag |= 1;
                }

        if( b <= 0 )
        {
            if( fabs(b-ib) < EPS )		/* b is a negative integer */
                flag |= 2;
                }

        if( ax < 1.0 )
        {
            if( fabs(b-c) < EPS )		/* b = c */
            {
                y = pow( s, -a );	/* s to the -a power */
                goto hypdon;
            }
            if( fabs(a-c) < EPS )		/* a = c */
            {
                y = pow( s, -b );	/* s to the -b power */
                goto hypdon;
            }
        }



        if( c <= 0.0 )
        {
            ic = round(c); 	/* nearest integer to c */
            if( fabs(c-ic) < EPS )		/* c is a negative integer */
            {
                /* check if termination before explosion */
                if( (flag & 1) && (ia > ic) )
                    goto hypok;
                if( (flag & 2) && (ib > ic) )
                    goto hypok;
                goto hypdiv;
            }
        }

        if( flag )			/* function is a polynomial */
            goto hypok;

        if( ax > 1.0 )			/* series diverges	*/
            goto hypdiv;

        p = c - a;
        ia = round(p); /* nearest integer to c-a */
        if( (ia <= 0.0) && (fabs(p-ia) < EPS) )	/* negative int c - a */
            flag |= 4;

            r = c - b;
            ib = round(r); /* nearest integer to c-b */
            if( (ib <= 0.0) && (fabs(r-ib) < EPS) )	/* negative int c - b */
                flag |= 8;

                d = c - a - b;
                id = round(d); /* nearest integer to d */
                q = fabs(d-id);

            /* Thanks to Christian Burger <BURGER@DMRHRZ11.HRZ.Uni-Marburg.DE>
             * for reporting a bug here.  */
                if( fabs(ax-1.0) < EPS )			/* |x| == 1.0	*/
                {
                    if( x > 0.0 )
                    {
                        if( flag & 12 ) /* negative int c-a or c-b */
                        {
                            if( d >= 0.0 )
                                goto hypf;
                            else
                                goto hypdiv;
                        }
                        if( d <= 0.0 )
                            goto hypdiv;
                        y = tgamma(c)*tgamma(d)/(tgamma(p)*tgamma(r));
                        goto hypdon;
                    }

                    if( d <= -1.0 )
                        goto hypdiv;

                }

        /* Conditionally make d > 0 by recurrence on c
         * AMS55 #15.2.27
         */
        if( d < 0.0 )
        {
            /* Try the power series first */
            y = internal::hyt2f1( a, b, c, x, &err );
            if( err < ETHRESH )
                goto hypdon;
            /* Apply the recurrence if power series fails */
            err = 0.0;
            aid = 2 - id;
            e = c + aid;
            d2 = cephes::hypergeom2F1(a,b,e,x);
            d1 = cephes::hypergeom2F1(a,b,e+1.0,x);
            q = a + b + 1.0;
            for( i=0; i<aid; i++ )
            {
                r = e - 1.0;
                y = (e*(r-(2.0*e-q)*x)*d2 + (e-a)*(e-b)*x*d1)/(e*r*s);
                e = r;
                d1 = d2;
                d2 = y;
            }
            goto hypdon;
        }


        if( flag & 12 )
            goto hypf; /* negative integer c-a or c-b */

    hypok:
        y = internal::hyt2f1( a, b, c, x, &err );


    hypdon:
        if( err > ETHRESH )
        {
            ;
            //mtherr( "hyp2f1", PLOSS );
            /*	printf( "Estimated err = %.2e\n", err ); */
        }
        return(y);

        /* The transformation for c-a or c-b negative integer
         * AMS55 #15.3.3
         */
    hypf:
        y = pow( s, d ) * internal::hys2f1( c-a, c-b, c, x, &err );
        goto hypdon;

        /* The alarm exit */
    hypdiv:
        throw std::overflow_error(std::string("cephes::hypergeom2F1 overflow"));
        //mtherr( "hyp2f1", OVERFLOW );
        return( MAXNUM );
    }



    //use hyt2f1 and hys22f1 only internally
    namespace internal{

        /* Apply transformations for |x| near 1
         * then call the power series
         */
        double hyt2f1(double a,double b,double c,double x,double * loss )
        {
            double p, q, r, s, t, y, d, err, err1;
            double ax, id, d1, d2, e, y1;
            int i, aid;

            err = 0.0;
            s = 1.0 - x;
            if( x < -0.5 )
            {
                if( b > a )
                    y = pow( s, -a ) * hys2f1( a, c-b, c, -x/s, &err );

                    else
                        y = pow( s, -b ) * hys2f1( c-a, b, c, -x/s, &err );

                        goto done;
            }

            d = c - a - b;
            id = round(d);	/* nearest integer to d */

            if( x > 0.9 )
            {
                if( fabs(d-id) > EPS ) /* test for integer c-a-b */
                {
                    /* Try the power series first */
                    y = hys2f1( a, b, c, x, &err );
                    if( err < ETHRESH )
                        goto done;
                    /* If power series fails, then apply AMS55 #15.3.6 */
                    q = hys2f1( a, b, 1.0-d, s, &err );
                    q *= tgamma(d) /(tgamma(c-a) * tgamma(c-b));
                    r = pow(s,d) * internal::hys2f1( c-a, c-b, d+1.0, s, &err1 );
                    r *= tgamma(-d)/(tgamma(a) * tgamma(b));
                    y = q + r;

                    q = fabs(q); /* estimate cancellation error */
                    r = fabs(r);
                    if( q > r )
                        r = q;
                        err += err1 + (MACHEP*r)/y;

                        y *= tgamma(c);
                        goto done;
                }
                else
                {
                    /* Psi function expansion, AMS55 #15.3.10, #15.3.11, #15.3.12 */
                    if( id >= 0.0 )
                    {
                        e = d;
                        d1 = d;
                        d2 = 0.0;
                        aid = id;
                    }
                    else
                    {
                        e = -d;
                        d1 = 0.0;
                        d2 = d;
                        aid = -id;
                    }

                    ax = log(s);

                    /* sum for t = 0 */
                    y = psi(1.0) + psi(1.0+e) - psi(a+d1) - psi(b+d1) - ax;
                    y /= tgamma(e+1.0);

                    p = (a+d1) * (b+d1) * s / tgamma(e+2.0);	/* Poch for t=1 */
                    t = 1.0;
                    do
                    {
                        r = psi(1.0+t) + psi(1.0+t+e) - psi(a+t+d1)
                        - psi(b+t+d1) - ax;
                        q = p * r;
                        y += q;
                        p *= s * (a+t+d1) / (t+1.0);
                        p *= (b+t+d1) / (t+1.0+e);
                        t += 1.0;
                    }
                    while( fabs(q/y) > EPS );


                    if( id == 0.0 )
                    {
                        y *= tgamma(c)/(tgamma(a)*tgamma(b));
                        goto psidon;
                    }

                    y1 = 1.0;

                    if( aid == 1 )
                        goto nosum;

                    t = 0.0;
                    p = 1.0;
                    for( i=1; i<aid; i++ )
                    {
                        r = 1.0-e+t;
                        p *= s * (a+t+d2) * (b+t+d2) / r;
                        t += 1.0;
                        p /= t;
                        y1 += p;
                    }
                nosum:
                    p = tgamma(c);
                    y1 *= tgamma(e) * p / (tgamma(a+d1) * tgamma(b+d1));

                    y *= p / (tgamma(a+d2) * tgamma(b+d2));
                    if( (aid & 1) != 0 )
                        y = -y;

                        q = pow( s, id );	/* s to the id power */
                        if( id > 0.0 )
                            y *= q;
                            else
                                y1 *= q;

                                y += y1;
                                psidon:
                                goto done;
                }

            }

            /* Use defining power series if no special cases */
            y = hys2f1( a, b, c, x, &err );

        done:
            *loss = err;
            return(y);
        }





        /* Defining power series expansion of Gauss hypergeometric function */

        double hys2f1(double a, double b, double c, double x, double * loss )
        /* loss: estimates loss of significance */
        {
            double f, g, h, k, m, s, u, umax;
            int i;

            i = 0;
            umax = 0.0;
            f = a;
            g = b;
            h = c;
            s = 1.0;
            u = 1.0;
            k = 0.0;
            do
            {
                if( fabs(h) < EPS )
                {
                    *loss = 1.0;
                    return( MAXNUM );
                }
                m = k + 1.0;
                u = u * ((f+k) * (g+k) * x / ((h+k) * m));
                s += u;
                k = fabs(u);  /* remember largest term summed */
                if( k > umax )
                    umax = k;
                    k = m;
                    if( ++i > 10000 ) /* should never happen */
                    {
                        *loss = 1.0;
                        return(s);
                    }
            }
            while( fabs(u/s) > MACHEP );

            /* return estimated relative error */
            *loss = (MACHEP*umax)/fabs(s) + (MACHEP*i);

            return(s);
        }
    }


    ////////////////////////////*********************************////////////////////////////////
    //
    // psi.c
    //
    ////////////////////////////*********************************////////////////////////////////

    /*
     Cephes Math Library Release 2.8:  June, 2000
     Copyright 1984, 1987, 1992, 2000 by Stephen L. Moshier
     */

    static double A[] = {
        8.33333333333333333333E-2,
        -2.10927960927960927961E-2,
        7.57575757575757575758E-3,
        -4.16666666666666666667E-3,
        3.96825396825396825397E-3,
        -8.33333333333333333333E-3,
        8.33333333333333333333E-2
    };



    double psi(double x)
    {
        double p, q, nz, s, w, y, z;
        int i, n, negative;

        negative = 0;
        nz = 0.0;

        if( x <= 0.0 )
        {
            negative = 1;
            q = x;
            p = floor(q);
            if( p == q )
            {
                throw std::runtime_error(std::string("cephes::hypergeom2F1::psi: function singularity"));
                // mtherr( "psi", SING );
                return( MAXNUM );
            }
            /* Remove the zeros of tan(PI x)
             * by subtracting the nearest integer from x
             */
            nz = q - p;
            if( nz != 0.5 )
            {
                if( nz > 0.5 )
                {
                    p += 1.0;
                    nz = q - p;
                }
                nz = M_PI/tan(M_PI*nz);
            }
            else
            {
                nz = 0.0;
            }
            x = 1.0 - x;
        }

        /* check for positive integer up to 10 */
        if( (x <= 10.0) && (x == floor(x)) )
        {
            y = 0.0;
            n = x;
            for( i=1; i<n; i++ )
            {
                w = i;
                y += 1.0/w;
            }
            y -= EUL;
            goto done;
        }

        s = x;
        w = 0.0;
        while( s < 10.0 )
        {
            w += 1.0/s;
            s += 1.0;
        }

        if( s < 1.0e17 )
        {
            z = 1.0/(s * s);
            y = z * polevl( z, A, 6 );
        }
        else
            y = 0.0;

            y = log(s)  -  (0.5/s)  -  y  -  w;

            done:

            if( negative )
            {
                y -= nz;
            }

        return(y);
    }




    ////////////////////////////*********************************////////////////////////////////
    //
    // polevl.c
    //
    ////////////////////////////*********************************////////////////////////////////

    /*
     Cephes Math Library Release 2.1:  December, 1988
     Copyright 1984, 1987, 1988 by Stephen L. Moshier
     Direct inquiries to 30 Frost Street, Cambridge, MA 02140
     */


    double polevl( double x, double coef[], int N )
    {
        double ans;
        int i;
        double *p;

        p = coef;
        ans = *p++;
        i = N;

        do
            ans = ans * x  +  *p++;
            while( --i );

        return( ans );
    }

    /*							p1evl()	*/
    /*                                          N
     * Evaluate polynomial when coefficient of x  is 1.0.
     * Otherwise same as polevl.
     */

    double p1evl( double x, double coef[], int N )
    {
        double ans;
        double *p;
        int i;

        p = coef;
        ans = x + *p++;
        i = N-1;

        do
            ans = ans * x  + *p++;
            while( --i );

        return( ans );
    }
}

